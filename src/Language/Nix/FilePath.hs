{-# LANGUAGE DeriveGeneric #-}

module Language.Nix.FilePath where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import           Data.String
import           Distribution.Compat.ReadP as ReadP
import           Distribution.Text
import           GHC.Generics (Generic)
import           Prelude hiding (FilePath)
import qualified System.FilePath.Posix as Posix
import           Test.QuickCheck as QC
import           Text.PrettyPrint as PP


newtype FilePath = FilePath { unFilePath :: String }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''FilePath

instance NFData FilePath where
  rnf = rnf . unFilePath

instance Arbitrary FilePath where
  arbitrary = FilePath <$> QC.suchThat arbitrary Posix.isValid
  shrink = fmap FilePath . shrink . unFilePath

instance Text FilePath where
  disp = text . renderFilePath
  parse = parseFilePath

instance IsString FilePath where
  fromString s = fromMaybe (error $ "invalid Nix file path:" ++ s) (simpleParse s)

parseFilePath :: ReadP r FilePath
parseFilePath = do
  path <- ReadP.munch (const True)
  if Posix.isValid path
    then pure (FilePath path)
    else pfail

renderFilePath :: FilePath -> String
renderFilePath = Posix.combine "."
               . Posix.normalise
               . Posix.dropTrailingPathSeparator
               . unFilePath
