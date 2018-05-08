module Melon.Config where

import Data.Default
import Network.Ethereum.Web3.Types


-- | Default Gas settings.
defGas :: Call
defGas = def { callGas = Just 6900000, callGasPrice = Just 100000000000 }
