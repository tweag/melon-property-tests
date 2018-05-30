module Melon.Contract.PreminedAsset
  ( deploy
  ) where

import Control.Lens ((^.))
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Web3.Types (Call (..))
import Network.Ethereum.Web3.Provider (Web3)

import qualified Melon.ABI.Assets.PreminedAsset as PreminedAsset
import Melon.Context
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


-- | @deploy owner@
-- Deploy a pre-mined asset that will hold a predefined amount in
-- @owner@'s name.
deploy
  :: Address
    -- ^ Owner
  -> MelonT Web3 Address
    -- ^ Returns the contract address
deploy owner = withContext $ \ctx -> do
  let ownerCall = (ctx^.ctxCall) { callFrom = Just owner }
  tx <- PreminedAsset.constructor ownerCall
  getContractAddress tx
