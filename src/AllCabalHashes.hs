module AllCabalHashes where

import Control.Lens hiding ((<.>))
import Control.Monad (when)
import Data.Aeson as A
import Data.ByteString as BS
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding as T
import Distribution.Nixpkgs.Hashes as NH
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text ( display )
import OpenSSL.Digest as SSL ( digest, digestByName )
import System.Directory
import System.FilePath


type SHA1Hash = T.Text
type SHA256Hash = String

data Meta = Meta
  { _mHashes    :: !(Map String String)
  , _mLocations :: ![String]
  , _mPkgsize   :: !Int
  } deriving (Show)

makeLenses ''Meta

instance FromJSON Meta where
  parseJSON (Object v) = Meta
    <$> v .: "package-hashes"
    <*> v .: "package-locations"
    <*> v .: "package-size"
  parseJSON o          = fail ("invalid Cabal metadata: " ++ show o)

readPackageByHash :: FilePath -> SHA1Hash -> IO (GenericPackageDescription, SHA256Hash)
readPackageByHash repoDir sha1Hash = do
  let (l1, rest) = T.splitAt 2 sha1Hash
      (l2, _) = T.splitAt 2 rest
      fullPath = repoDir </> "_hash-lookup" </> T.unpack l1 </> T.unpack l2 </> T.unpack sha1Hash
  exists <- doesFileExist fullPath
  when (not exists) $ fail (fullPath ++ " doesn't exists")
  buf <- BS.readFile fullPath
  cabal <- case parseGenericPackageDescription (T.unpack $ T.decodeUtf8 buf) of
    ParseOk _ a     -> return a
    ParseFailed err -> fail ("Git SHA1 " ++ show sha1Hash ++ ": " ++ show err)
  let
    hash = printSHA256 (digest (digestByName "sha256") buf)
    pkg  = setCabalFileHash hash cabal
  return (pkg, hash)

readPackageByName :: FilePath -> PackageIdentifier -> IO (GenericPackageDescription, SHA256Hash)
readPackageByName repoDir (PackageIdentifier name version) = do
  let cabalFile = repoDir </> unPackageName name </> display version </> unPackageName name <.> "cabal"
  buf <- BS.readFile cabalFile
  cabal <- case parseGenericPackageDescription (T.unpack $ T.decodeUtf8 buf) of
             ParseOk _ a  -> return a
             ParseFailed err -> fail (cabalFile ++ ": " ++ show err)
  let
    hash = NH.printSHA256 (SSL.digest (SSL.digestByName "sha256") buf)
    pkg  = setCabalFileHash hash cabal
  return (pkg, hash)

setCabalFileHash :: SHA256Hash -> GenericPackageDescription -> GenericPackageDescription
setCabalFileHash hash cabal = cabal
  { packageDescription = (packageDescription cabal)
    { customFieldsPD = ("X-Cabal-File-Hash", hash) : customFieldsPD (packageDescription cabal)
    }
  }

readPackageMeta :: FilePath -> PackageIdentifier -> IO Meta
readPackageMeta dirPrefix (PackageIdentifier name version) = do
  let metaFile = dirPrefix </> unPackageName name </> display version </> unPackageName name <.> "json"
  buf <- BS.readFile metaFile
  case eitherDecodeStrict buf of
    Left msg -> fail (metaFile ++ ": " ++ msg)
    Right x  -> return $ over (mHashes . ix "SHA256") (printSHA256 . packHex) x
