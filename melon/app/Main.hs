{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.Semigroup ((<>))
import Hedgehog (Seed (..), Size)
import Options.Applicative
import qualified Options.Applicative.Help.Chunk as PP
import qualified Options.Applicative.Help.Pretty as PP
import Text.Read (readEither)

import Melon.Test


main :: IO ()
main = do
  (cfg, cmd) <- parseArgs
  case cmd of
    CheckAll ->
      void $ tests cfg
    RecheckSimple size seed ->
      recheck_prop_melonport cfg size seed
    RecheckModel size seed ->
      recheck_prop_melonport_model cfg size seed


parseArgs :: IO (TestConfig, PropTestCommand)
parseArgs =
  execParser
  $ info ((,) <$> parseConfig <*> parseCommand <**> helper)
    $  fullDesc
    <> progDescDoc propTestsDescription
    <> header propTestsHeader

  where

    propTestsHeader = "Melon fund property-tests"
    propTestsDescription = PP.unChunk $ PP.vsepChunks
      [ PP.paragraph
        "Generates random sequences of commands against the Melon fund\
        \ smart-contract and executes these testing for specified invariants\
        \ in between."
      , PP.paragraph
        "Two different test-cases are implemented, the simple and the model\
        \ tests. The simple tests do not keep a detailed model of the fund.\
        \ Instead, they query the fund for required information at every\
        \ command invocation.\
        \ The model tests carry a model of the fund and can have more detailed\
        \ expectations towards the fund's behaviour. They are more limited in\
        \ that not all aspects of the fund are modelled, and those that are\
        \ not cannot be tested by these tests."
      ]

    parseConfig = TestConfig
      <$> parseTestLimit
      <*> parseNumCommands
      <*> parseShrinkLimit

    parseTestLimit = option (eitherReader $ first ("LIMIT: " ++ ) . readPositive)
      $  metavar "LIMIT"
      <> value (defaultTestConfig^.tcTestLimit)
      <> long "limit"
      <> short 'l'
      <> help "Positive number specifying the number of test-repetitions."
      <> showDefaultWith (show @Int . fromIntegral)
    parseNumCommands = option (eitherReader $ first ("COMMANDS: " ++) . readPositive)
      $  metavar "COMMANDS"
      <> value (defaultTestConfig^.tcNumCommands)
      <> long "commands"
      <> short 'c'
      <> help "Positive number specifying the number commands to generate."
      <> showDefault
    parseShrinkLimit = option (eitherReader $ first ("SHRINKS: " ++) . readPositive)
      $  metavar "SHRINKS"
      <> value (defaultTestConfig^.tcShrinkLimit)
      <> long "shrinks"
      <> short 's'
      <> help
        "Positive number specifying the number of shrink attempts to be made.\
        \ Shrinking tries to find a shorter, still valid, sequence of commands\
        \ and invariants and continuously verifies these shorter sequences\
        \ until a minimal counter example is found. This can help make the\
        \ test report more understandable. However, it takes longer than\
        \ displaying the result immediately, and it can emphasize differences\
        \ between the model and the actual contract and point to differences\
        \ in the wrong place."
      <> showDefault

    readPositive s = do
      (n :: Int) <- readEither s
      if n <= 0 then
        Left "Must be a positive number."
      else
        pure (fromIntegral n)

    parseCommand = subparser
      $  command "check" (info
          (pure CheckAll <**> helper)
          (progDesc checkAllDesc))
      <> command "recheck-simple" (info
          (RecheckSimple <$> parseSize <*> parseSeed <**> helper)
          (progDescDoc recheckSimpleDesc))
      <> command "recheck-model" (info
          (RecheckModel <$> parseSize <*> parseSeed <**> helper)
          (progDescDoc recheckModelDesc))

    checkAllDesc =
      "Run all property-tests, the simple and the model test-cases, with the\
      \ specified test-limit and number of commands."
    recheckSimpleDesc = PP.unChunk $ PP.vcatChunks
      [ PP.paragraph
        "Re-check a particular failed simple test.\
        \ E.g. if you encounter a test-case failure which mentions the\
        \ following re-check command"
      , PP.indent 4 . PP.hang 4 <$> PP.paragraph
        "> recheck (Size 8) (Seed 9082926469563838346 (-5629536107532025561))\
        \ prop_melonport"
      , PP.paragraph
        "then you can re-check that test-case by issuing the following command"
      , PP.indent 4 . PP.hang 4 <$> PP.paragraph
        "melon-property-tests recheck-simple --\
        \ 8 9082926469563838346 -5629536107532025561"
      , PP.paragraph
        "Note the double-dash to escape the minus sign."
      ]
    recheckModelDesc = PP.unChunk $ PP.vcatChunks
      [ PP.paragraph
        "Re-check a particular failed model test.\
        \ E.g. if you encounter a test-case failure which mentions the\
        \ following re-check command"
      , PP.indent 4 . PP.hang 4 <$> PP.paragraph
        "> recheck (Size 8) (Seed 9082926469563838346 (-5629536107532025561))\
        \ prop_melonport_model"
      , PP.paragraph
        "then you can re-check that test-case by issuing the following command"
      , PP.indent 4 . PP.hang 4 <$> PP.paragraph
        "melon-property-tests recheck-model --\
        \ 8 9082926469563838346 -5629536107532025561"
      , PP.paragraph
        "Note the double-dash to escape the minus sign."
      ]

    parseSize = argument (eitherReader $ first ("SIZE: " ++ ) . readPositive)
      $  metavar "SIZE"
      <> help "Size of the randomly generated data."
    parseSeed = Seed <$> parseSeedValue <*> parseSeedGamma
    parseSeedValue = argument (eitherReader $ first ("SEED1: " ++ ) . readInt64)
      $  metavar "SEED1"
      <> help "The first component of the random seed."
    parseSeedGamma = argument (eitherReader $ first ("SEED2: " ++) . readOddInt64)
      $  metavar "SEED2"
      <> help "The second component of the random seed, must be an odd number."

    readInt64 s = do
      (n :: Int64) <- readEither s
      pure n
    readOddInt64 s = do
      n <- readInt64 s
      if even n then
        Left "Must be an odd number."
      else
        pure (fromIntegral n)


-- | Command-line commands to the property-tests executable.
data PropTestCommand
    -- | Check all test-cases with given test-limit and number of commands.
  = CheckAll
    -- | Recheck a particular simple test-case failure.
  | RecheckSimple Size Seed
    -- | Recheck a particular model test-case failure.
  | RecheckModel Size Seed
  deriving Show
