{-# LANGUAGE DeriveGeneric #-}

module Text.PTB.RNNG (
  Action(..),
  ) where

import GHC.Generics --base
import Text.PTB (PTBTree(..)) --nlp-tools

data Action = T
  deriving (Eq, Show, Read, Generic)

ptbTree2Actions :: PTBTree -> [Action]
ptbTree2Actions tree = case tree of
  Word t1 t2 ->
  Phrase s trees ->
  Err s t -> 
