{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Test.Commands where

import Control.Exception.Safe (MonadThrow)
import Control.Lens (view)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Ethereum.ABI.Prim.Address (Address)
import Network.Ethereum.Web3.Types (Call (..))

import qualified Melon.ABI.Assets.Asset as Asset
import qualified Melon.ABI.Fund as Fund
import qualified Melon.ABI.PriceFeeds.CanonicalPriceFeed as CanonicalPriceFeed
import Melon.Context
import Melon.Deploy (deploy)


tests :: IO Bool
tests = checkParallel $$(discover)


prop_melonport :: Property
prop_melonport = property $ do
  (fund, priceFeed, assets) <- liftIO deploy
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialModelState $
      [ checkSharePrice fund priceFeed assets
      ]
  runMelonT $ executeSequential initialModelState actions


data ModelState (v :: * -> *) = ModelState
  deriving (Eq, Ord, Show)

initialModelState :: ModelState v
initialModelState = ModelState


data CheckSharePrice (v :: * -> *) = CheckSharePrice
  deriving (Eq, Show)
instance HTraversable CheckSharePrice where
  htraverse _ CheckSharePrice = pure CheckSharePrice


checkSharePrice
  :: (Monad n, Monad m, MonadIO m, MonadThrow m)
  => Address -- ^ Melon fund
  -> Address -- ^ Price-feed
  -> [Address] -- ^ Owned assets
  -> Command n (MelonT m) ModelState
checkSharePrice fund priceFeed assets =
  let
    gen _ = Just $ pure CheckSharePrice
    execute CheckSharePrice = hoistWeb3 $ do
      defaultCall <- view ctxCall

      let callPriceFeed = defaultCall { callTo = Just priceFeed }
      (prices, _timestamps) <- liftWeb3 $
        CanonicalPriceFeed.getPrices callPriceFeed assets

      balances <- liftWeb3 $ forM assets $ \asset -> do
        let callAsset = defaultCall { callTo = Just asset }
        Asset.balanceOf callAsset fund

      let callFund = defaultCall { callTo = Just fund }
      totalShares <- liftWeb3 $ Fund.totalSupply callFund
      sharePrice <- liftWeb3 $ Fund.calcSharePrice callFund

      pure (prices, balances, totalShares, sharePrice)
  in
  Command gen execute
    [ Ensure $ \_prior _after CheckSharePrice (prices, balances, totalShares, sharePrice) -> do
        footnote $ "prices " ++ show prices
        footnote $ "balances " ++ show balances
        footnote $ "totalShares " ++ show totalShares
        length prices === length balances
        unless (totalShares == 0) $ do
          let calculatedSharePrice =
                sum (zipWith (*) balances prices)
                `div`
                totalShares
          sharePrice === calculatedSharePrice
    ]
