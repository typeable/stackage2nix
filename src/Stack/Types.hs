{-# LANGUAGE TemplateHaskell #-}

module Stack.Types where

import Control.Lens
import Data.Text as T
import System.FilePath


data StackYaml = StackYaml
  { _syDirName  :: !FilePath
  , _syFileName :: !FilePath }
  deriving (Ord, Eq, Show)

makeLenses ''StackYaml

syFilePath :: Getter StackYaml FilePath
syFilePath = to stackYamlPath
  where
    stackYamlPath sy = sy ^. syDirName </> sy ^. syFileName

newtype CabalFlag = CabalFlag { fromCabalFlag :: Text }
  deriving (Ord, Eq, Show)

makePrisms ''CabalFlag

unCabalFlag :: CabalFlag -> String
unCabalFlag = T.unpack . fromCabalFlag
