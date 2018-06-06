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


initialModelState :: ModelState v
initialModelState = ModelState
  { _msIsShutdown = False
  , _msInvestments = mempty
  }


data ModelState (v :: * -> *) = ModelState
  { _msIsShutdown :: !Bool
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