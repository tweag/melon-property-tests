{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Model.State where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Hedgehog (HTraversable (..), Var)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)


data ModelState (v :: * -> *) = ModelState
  { _msDecimals :: UIntN 256
    -- ^ Quote asset decimals
  , _msPriceUpdateId :: UIntN 256
    -- ^ Price-feed update Id. (Incremented at every update.)
    -- Investment execution requires that two updates happened since the
    -- request.
    --
    -- Note, the model's update Id and the fund's update Id won't necessarily
    -- match. If the shrinking procedure removes price-feed updates, then the
    -- model will be affected, but the fund will not.
  , _msPrices :: HashMap Address (UIntN 256)
    -- ^ Current asset prices as of latest price-feed update.
  , _msPriceFeed :: [HashMap Address (UIntN 256)]
    -- ^ Infinite list of prices for price-feed update.
  , _msTotalShares :: UIntN 256
    -- ^ Total number of shares in existence.
  , _msAssetBalances :: HashMap Address (UIntN 256)
    -- ^ Fund's holdings of assets.
  , _msIsShutdown :: !Bool
    -- ^ Whether the fund contract is shut-down.
  , _msInvestments :: [InvestmentRequest v]
    -- ^ Open investment requests.
    --
    -- Note, that the actual fund might hold more open requests than the model.
    -- If a request operation is shrunk away, then the model will lose that
    -- request, but the request won't be undone on the fund.
  } deriving (Eq, Ord, Show)


data InvestmentRequest (v :: * -> *) = InvestmentRequest
  { _irId :: Var (UIntN 256) v
  , _irInvestor :: Address
  , _irAsset :: Address
  , _irGive :: UIntN 256
  , _irShare :: UIntN 256
  , _irPriceUpdateId :: UIntN 256
  } deriving (Eq, Ord, Show)
instance HTraversable InvestmentRequest where
  htraverse f req = InvestmentRequest
    <$> htraverse f (_irId req)
    <*> pure (_irInvestor req)
    <*> pure (_irAsset req)
    <*> pure (_irGive req)
    <*> pure (_irShare req)
    <*> pure (_irPriceUpdateId req)


makeClassy ''ModelState
makeClassy ''InvestmentRequest


msAssetPrice :: Address -> Lens' (ModelState v) (UIntN 256)
msAssetPrice asset = msPrices.at asset.non 0

msAssetBalance :: Address -> Lens' (ModelState v) (UIntN 256)
msAssetBalance asset = msAssetBalances.at asset.non 0

msSharePrice :: Getter (ModelState v) (UIntN 256)
msSharePrice = to $ \s ->
  if s^.msTotalShares > 0 then
    sum
      [ (s^.msAssetPrice asset) * (s^.msAssetBalance asset)
      | asset <- s^.msAssetBalances.to HashMap.keys
      ]
    `div`
    (s^.msTotalShares)
  else
    10^(s^.msDecimals)
