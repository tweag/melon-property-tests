{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Melon.Deploy where

import Control.Lens ((^.), to)
import Control.Monad (forM_, unless, void)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Web3.Eth
import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Types
import System.IO

import qualified Melon.ABI.Assets.PreminedAsset as PreminedAsset
import qualified Melon.ABI.Fund as Fund
import Melon.Context
import qualified Melon.Contract.Fund as Fund
import Melon.Contract.PriceFeed (updateCanonicalPriceFeed)
import qualified Melon.Contract.Version as Version
import Melon.Model
import Melon.ThirdParty.Network.Ethereum.Web3.Eth


mockBytes :: Text
mockBytes = "0x86b5eed81db5f691c36cc83eb58cb5205bd2090bf3763a19f0c5bf2f074dd84b"


mockAddress :: Address
mockAddress = "0x083c41ea13af6c2d5aaddf6e73142eb9a7b00183"


deploy :: IO
  ( VersionDeployment -- ^ Version contract and modules
  , Address -- ^ Melon fund
  , Address -- ^ Price feed
  , MelonT Web3 () -- ^ Update the price feed
  , Address -- ^ Fund manager
  , [Address] -- ^ Registered assets
  )
deploy = do
  hSetBuffering stdout LineBuffering
  r <- runWeb3 $ runMelonT $ do

    ------------------------------------------------------------
    -- Accounts
    owner:manager:investorA:_ <- liftWeb3 accounts

    defaultCall <- withContext $ \ctx -> pure (ctx^.ctxCall)
    let ownerCall = defaultCall { callFrom = Just owner }
        managerCall = defaultCall { callFrom = Just manager }
        investorACall = defaultCall { callFrom = Just investorA }

    version <- Version.deploy owner
    let mlnToken = version^.vdMlnToken
        ownerCallMln = ownerCall { callTo = Just mlnToken }
        nonMlnAssets = filter (/=mlnToken) $ HashMap.keys $ version^.vdAssets
        canonicalPriceFeed = version^.vdCanonicalPriceFeed

    ------------------------------------------------------------
    -- Setup a fund instance
    -- XXX: Separate out the following fund setup.
    fund <- Fund.deploy version manager

    let managerCallFund = managerCall { callTo = Just $ fund^.fdAddress }
        callFund = defaultCall { callTo = Just $ fund^.fdAddress }

    ------------------------------------------------------------
    -- Enable investment and redemption
    -- XXX: Move this into tests and make it part of the model.
    void $ liftWeb3 $ Fund.enableInvestment managerCallFund
      nonMlnAssets
    void $ liftWeb3 $ Fund.enableRedemption managerCallFund
      nonMlnAssets
    let investTokens =
          [ (spec^.asTokenName, asset)
          | (asset, spec) <- HashMap.toList $ version^.vdAssets
          ]

    ------------------------------------------------------------
    -- Verify enabled investment and redemption
    -- XXX: Move this into tests and make it part of the model.
    liftWeb3 $ forM_ investTokens $ \(sym, token) -> do
      investAllowed <- Fund.isInvestAllowed callFund token
      unless investAllowed $ error $ "Investment not allowed for " ++ show sym
      redeemAllowed <- Fund.isRedeemAllowed callFund token
      unless redeemAllowed $ error $ "Redemption not allowed for " ++ show sym

    ------------------------------------------------------------
    -- Initial price feed update
    -- XXX: Should this just happen every 30 seconds or so?
    -- XXX: Replace by mock price feed update.
    let updatePriceFeed =
          updateCanonicalPriceFeed
            canonicalPriceFeed
            (version^.vdStakingPriceFeed)
            owner
            "MLN"
            18
            [ (spec^.asCryptoCompareName, address, spec^.asDecimals)
            | (address, spec) <- HashMap.toList (version^.vdAssets)
            ]
    updatePriceFeed

    ------------------------------------------------------------
    -- Initial calculate share price
    liftWeb3 $
      Fund.calcGav callFund
      >>= getTransactionEvents >>= \case
        [Fund.PortfolioContent {}] -> pure ()
        _ -> error "Expected one PortfolioContent event"
    void $ liftWeb3 $ Fund.calcSharePrice callFund

    ------------------------------------------------------------
    -- Three initial trades
    -- XXX: Move this into tests and the model
    let initialTokenAmount = 10^(23::Int)
    void $ liftWeb3 $ PreminedAsset.transfer ownerCallMln
      investorA initialTokenAmount
      >>= getTransactionEvents >>= \case
        [PreminedAsset.Transfer {}] -> pure ()
        _ -> error "Initial token transfer failed"

    let tradeToken = mlnToken
        investorACallFund = investorACall { callTo = Just $ fund^.fdAddress }
        investorACallMln = investorACall { callTo = Just mlnToken }
        trades = [ (2000, 2000), (10^(18::Int), 10^(18::Int)), ((10^(18::Int)) `div` 2, 10^(18::Int)) ]
    forM_ trades $ \(shareQuantity, giveQuantity) -> do

      -- Approve asset
      liftWeb3 $
        PreminedAsset.approve investorACallMln
          (fund^.fdAddress) giveQuantity
        >>= getTransactionEvents >>= \case
          [PreminedAsset.Approval {}] -> pure ()
          _ -> error "investor token allowance failed."

      -- Request investment
      reqId <- liftWeb3 $ Fund.requestInvestment investorACallFund
        giveQuantity -- give quantity (MLN-T)
        shareQuantity -- ask quantity (shares)
        tradeToken -- asset to invest in
        >>= getTransactionEvents >>= \case
          [Fund.RequestUpdated reqId] -> pure reqId
          _ -> error "Expected one RequestUpdated event"

      -- Update price-feed twice
      updatePriceFeed >> updatePriceFeed

      -- Execute investment
      void $ liftWeb3 $ Fund.executeRequest investorACallFund reqId
    updatePriceFeed
    liftWeb3 $
      Fund.calcGav callFund
      >>= getTransactionEvents >>= \case
        [Fund.PortfolioContent {}] -> pure ()
        _ -> error "Expected one PortfolioContent event"
    void $ liftWeb3 $ Fund.calcSharePrice callFund

    updatePriceFeed

    return
      ( version, fund^.fdAddress, canonicalPriceFeed, updatePriceFeed
      , manager, version^.vdAssets.to HashMap.keys)

  case r of
    Left err -> error $ "deploy failed: " ++ show err
    Right x -> return x
