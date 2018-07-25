module AllCabalHashes where

import Control.Lens hiding ((<.>))
import Data.Aeson as A
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Map as M
import Data.Text as T
import Distribution.Nixpkgs.Hashes as NH
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Text ( display )
import Git
import Git.Libgit2 as Libgit2
import OpenSSL.Digest as SSL ( digest, digestByName )
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
  let repoOpts = defaultRepositoryOptions { repoPath = repoDir }
  repo <- openLgRepository repoOpts
  buf <- runLgRepository repo $ do
    BlobObj (Blob _ contents) <- lookupObject =<< parseOid sha1Hash
    case contents of
      BlobString bs -> return bs
      BlobStringLazy bsl -> return $ BSL.toStrict bsl
      _ -> fail $ "Git SHA1 " ++ show sha1Hash ++ ": expected single Blob"
  cabal <- let (_, res) = runParseResult (parseGenericPackageDescription buf)
           in case res of Right a       -> return a
                          Left (_, err) -> fail ("Git SHA1 " ++ show sha1Hash ++ ": " ++ show err)
  let
    hash = printSHA256 (digest (digestByName "sha256") buf)
    pkg  = setCabalFileHash hash cabal
  return (pkg, hash)

readPackageByName :: FilePath -> PackageIdentifier -> IO (GenericPackageDescription, SHA256Hash)
readPackageByName repoDir (PackageIdentifier name version) = do
  let cabalFile = repoDir </> unPackageName name </> display version </> unPackageName name <.> "cabal"
  buf <- BS.readFile cabalFile
  cabal <- let (_, res) = runParseResult (parseGenericPackageDescription buf)
           in case res of Right a       -> return a
                          Left (_, err) -> fail (cabalFile ++ ": " ++ show err)
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
