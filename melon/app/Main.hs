module Main
  ( main
  ) where

import Control.Monad (void)

import Melon.Test


main :: IO ()
main = void $ tests 20 200
