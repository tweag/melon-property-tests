module Melon.Contract.PreminedAsset
  ( deploy
  ) where

import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.Assets.PreminedAsset as PreminedAsset
import Melon.Context
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


-- | @deploy owner@
-- Deploy a pre-mined asset that will hold a predefined amount in
-- @owner@'s name.
deploy
  :: MonadMelon m
  => Address
    -- ^ Owner
  -> m Address
    -- ^ Returns the contract address
deploy owner = do
  defaultCall <- getCall
  let ownerCall = defaultCall { callFrom = Just owner }
  tx <- liftWeb3 $ PreminedAsset.constructor ownerCall
  liftWeb3 $ getContractAddress tx
