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


data ModelState (v :: * -> *) = ModelState
  deriving (Eq, Ord, Show)


makeClassy ''ModelState
