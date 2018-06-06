{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Model.State where

import Control.Lens.TH (makeClassy)
import Hedgehog (Var)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.ABI.Prim.Int (UIntN)


data ModelState (v :: * -> *) = ModelState
  { _msPriceUpdateId :: UIntN 256
    -- ^ Price-feed update Id. (Incremented at every update.)
    -- Investment execution requires that two updates happened since the
    -- request.
    --
    -- Note, the model's update Id and the fund's update Id won't necessarily
    -- match. If the shrinking procedure removes price-feed updates, then the
    -- model will be affected, but the fund will not.
  , _msPrices :: [UIntN 256]
    -- ^ Current asset prices as of latest price-feed update.
  , _msPriceFeed :: [[UIntN 256]]
    -- ^ Infinite list of prices for price-feed update.
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
  } deriving (Eq, Ord, Show)


makeClassy ''ModelState
makeClassy ''InvestmentRequest
