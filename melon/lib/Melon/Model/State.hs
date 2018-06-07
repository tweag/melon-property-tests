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


newtype SimpleModelState (v :: * -> *) = SimpleModelState
  { _smsPriceFeed :: [HashMap Address (UIntN 256)]
  } deriving (Eq, Ord, Show)
instance HTraversable SimpleModelState where
  htraverse _ (SimpleModelState priceFeed) = pure $
    SimpleModelState priceFeed


data ModelState (v :: * -> *) = ModelState
  { _msQuoteAsset :: !Address
    -- ^ Quote asset
  , _msQuoteDecimals :: !(UIntN 256)
    -- ^ Quote asset decimals
  , _msPriceUpdateId :: !(UIntN 256)
    -- ^ Price-feed update Id. (Incremented at every update.)
    -- Investment execution requires that two updates happened since the
    -- request.
    --
    -- Note, the model's update Id and the fund's update Id won't necessarily
    -- match. If the shrinking procedure removes price-feed updates, then the
    -- model will be affected, but the fund will not.
  , _msPrices :: !(HashMap Address (UIntN 256))
    -- ^ Current asset prices as of latest price-feed update.
  , _msDecimals :: !(HashMap Address (UIntN 256))
    -- ^ Asset decimal places.
  , _msPriceFeed :: ![HashMap Address (UIntN 256)]
    -- ^ Infinite list of prices for price-feed update.
  , _msTotalShares :: !(UIntN 256)
    -- ^ Total number of shares in existence.
  , _msAssetBalances :: !(HashMap Address (UIntN 256))
    -- ^ Fund's holdings of assets.
  , _msIsShutdown :: !Bool
    -- ^ Whether the fund contract is shut-down.
  , _msInvestments :: ![InvestmentRequest v]
    -- ^ Open investment requests.
    --
    -- Note, that the actual fund might hold more open requests than the model.
    -- If a request operation is shrunk away, then the model will lose that
    -- request, but the request won't be undone on the fund.
  , _msInvestorAssets :: !(HashMap Address (HashMap Address (UIntN 256)))
    -- ^ Mapping @investor -> asset -> balance@.
  } deriving (Eq, Ord, Show)


data InvestmentRequest (v :: * -> *) = InvestmentRequest
  { _irId :: !(Var (UIntN 256) v)
  , _irInvestor :: !Address
  , _irAsset :: !Address
  , _irGive :: !(UIntN 256)
  , _irShare :: !(UIntN 256)
  , _irPriceUpdateId :: !(UIntN 256)
  } deriving (Eq, Ord, Show)
instance HTraversable InvestmentRequest where
  htraverse f req = InvestmentRequest
    <$> htraverse f (_irId req)
    <*> pure (_irInvestor req)
    <*> pure (_irAsset req)
    <*> pure (_irGive req)
    <*> pure (_irShare req)
    <*> pure (_irPriceUpdateId req)


makeClassy ''SimpleModelState
makeClassy ''ModelState
makeClassy ''InvestmentRequest


msAssetPrice :: Address -> Lens' (ModelState v) (UIntN 256)
msAssetPrice asset = msPrices.at asset.non 0

msAssetDecimals :: Address -> Lens' (ModelState v) (UIntN 256)
msAssetDecimals asset = msDecimals.at asset.non 0

msInvertedAssetPrice :: Address -> Getter (ModelState v) (UIntN 256)
msInvertedAssetPrice asset = to $ \s ->
  let
    assetPrice = s^.msAssetPrice asset
    assetDecimals = s^.msAssetDecimals asset
    quoteDecimals = s^.msQuoteDecimals
  in
  ( (10^quoteDecimals) * (10^assetDecimals) )
  `div` assetPrice

msInvertedPrices :: Getter (ModelState v) (HashMap Address (UIntN 256))
msInvertedPrices = to $ \s ->
  imap (\asset _ -> s^.msInvertedAssetPrice asset) (s^.msPrices)

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
    10^(s^.msQuoteDecimals)

msShareCost
  :: UIntN 256
    -- ^ Share quantity
  -> Address
    -- ^ Asset to buy shares in
  -> UIntN 256
    -- ^ Asset decimals
  -> Getter (ModelState v) (UIntN 256)
msShareCost shareQuantity asset assetDecimals = to $ \s ->
  let
    quoteDecimals = s^.msQuoteDecimals
    sharePrice = s^.msSharePrice
    -- Share cost in quote asset
    shareQuoteCost = (shareQuantity * sharePrice)
      `div` (10^quoteDecimals)
    -- Share cost in investment asset
    shareAssetCost = shareQuoteCost * (s^.msInvertedAssetPrice asset)
      `div` (10^assetDecimals)
  in
  if asset == s^.msQuoteAsset then
    -- If the asset is the quote asset, take the price in quote asset.
    shareQuoteCost
  else
    -- Otherwise, take the price in give asset.
    shareAssetCost

msInvestorAssetBalance :: Address -> Address -> Lens' (ModelState v) (UIntN 256)
msInvestorAssetBalance investor asset =
  msInvestorAssets.at investor.non mempty.at asset.non 0
