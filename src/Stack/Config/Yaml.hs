module Stack.Config.Yaml where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text as T
import Stack.Config.TH

data Location a = Location
  { _lLocation :: a
  , _lExtraDep :: Maybe Bool
  , _lSubdirs  :: Maybe [FilePath]
  } deriving (Eq, Show)

makeLenses ''Location
deriveJSON jsonOpts ''Location

data Git = Git
  { _gGit    :: Text
  , _gCommit :: Text
  } deriving (Eq, Show)

makeLenses ''Git
deriveJSON jsonOpts ''Git

data Hg = Hg
  { _hHg     :: Text
  , _hCommit :: Text
  } deriving (Eq, Show)

makeLenses ''Hg
deriveJSON jsonOpts ''Hg

-- New syntax

data NewGit = NewGit
  { _ngGit     :: Text
  , _ngCommit  :: Text
  , _ngSubdirs :: Maybe [FilePath]
  } deriving (Eq, Show)

makeLenses ''NewGit
deriveJSON jsonOpts ''NewGit

data NewHg = NewHg
  { _nhHg      :: Text
  , _nhCommit  :: Text
  , _nhSubdirs :: Maybe [FilePath]
  } deriving (Eq, Show)

makeLenses ''NewHg
deriveJSON jsonOpts ''NewHg

data Package
  = Simple Text
  | LocationSimple (Location Text)
  | LocationGit (Location Git)
  | LocationHg (Location Hg)
  | PNewGit NewGit
  | PNewHg NewHg
  deriving (Eq, Show)

makePrisms ''Package

instance FromJSON Package where
  parseJSON v =
    (Simple <$> parseJSON v) <|>
    (LocationSimple <$> parseJSON v) <|>
    (LocationGit <$> parseJSON v) <|>
    (LocationHg <$> parseJSON v) <|>
    (PNewGit <$> parseJSON v) <|>
    (PNewHg <$> parseJSON v)

instance ToJSON Package where
  toJSON = \case
    Simple t         -> toJSON t
    LocationSimple t -> toJSON t
    LocationGit t    -> toJSON t
    LocationHg t     -> toJSON t
    PNewGit t        -> toJSON t
    PNewHg t         -> toJSON t

-- To support support both old and new syntax, we allow all varieties of package
-- definitions in both "packages" and "extra-deps" sections
data Config = Config
  { _cResolver  :: Text
  , _cPackages  :: Maybe [Package]
  , _cExtraDeps :: Maybe [Package]
  } deriving (Eq, Show)

makeLenses ''Config

deriveJSON jsonOpts ''Config
