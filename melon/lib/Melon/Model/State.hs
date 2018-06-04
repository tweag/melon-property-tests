{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Melon.Model.State where

import Control.Lens.TH (makeClassy)


initialModelState :: ModelState v
initialModelState = ModelState
  { _msIsShutdown = False
  }


data ModelState (v :: * -> *) = ModelState
  { _msIsShutdown :: !Bool
    -- ^ Whether the fund contract is shut-down.
  }
  deriving (Eq, Ord, Show)


makeClassy ''ModelState
