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

data Archive = Archive
  { _aArchive :: Text
  , _aSha256  :: Maybe Text
  , _aSubdirs :: Maybe [FilePath]
  } deriving (Eq, Show)

makeLenses ''Archive
deriveJSON jsonOpts ''Archive

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
  = PSimple Text
  | PLocationSimple (Location Text)
  | PLocationGit (Location Git)
  | PLocationHg (Location Hg)
  | PArchive Archive
  | PNewGit NewGit
  | PNewHg NewHg
  deriving (Eq, Show)

makePrisms ''Package

instance FromJSON Package where
  parseJSON v =
    (PSimple <$> parseJSON v) <|>
    (PLocationSimple <$> parseJSON v) <|>
    (PLocationGit <$> parseJSON v) <|>
    (PLocationHg <$> parseJSON v) <|>
    (PArchive <$> parseJSON v) <|>
    (PNewGit <$> parseJSON v) <|>
    (PNewHg <$> parseJSON v)

instance ToJSON Package where
  toJSON = \case
    PSimple t         -> toJSON t
    PLocationSimple t -> toJSON t
    PLocationGit t    -> toJSON t
    PLocationHg t     -> toJSON t
    PArchive t        -> toJSON t
    PNewGit t         -> toJSON t
    PNewHg t          -> toJSON t

-- To support both old and new syntax, we allow all variants of package
-- definitions in "packages" and "extra-deps" sections
data Config = Config
  { _cResolver  :: Text
  , _cPackages  :: Maybe [Package]
  , _cExtraDeps :: Maybe [Package]
  } deriving (Eq, Show)

makeLenses ''Config

deriveJSON jsonOpts ''Config
