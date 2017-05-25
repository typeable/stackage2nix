{-# LANGUAGE TemplateHaskell #-}

module Stack.Types where

import Control.Lens
import Data.Text as T

newtype CabalFlag = CabalFlag { fromCabalFlag :: Text }
  deriving (Ord, Eq, Show)

makePrisms ''CabalFlag

unCabalFlag :: CabalFlag -> String
unCabalFlag = T.unpack . fromCabalFlag
