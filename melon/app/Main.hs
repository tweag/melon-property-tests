module Main
  ( main
  ) where

import Control.Monad (void)

import Melon.Test.Commands


main :: IO ()
main = void tests
