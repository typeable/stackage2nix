module Stack.Config.TH where

import Data.Aeson.TH
import Data.Char as C
import Data.List as L
import Data.Text as T
import Text.Inflections as TI

toDashedField :: String -> String
toDashedField = toDashed' . dropPrefix

toDashed' :: String -> String
toDashed' = either (const $ error "toDashed failed") T.unpack
  . TI.toDashed
  . T.pack

dropPrefix :: String -> String
dropPrefix = L.dropWhile (not . C.isUpper)

jsonOpts :: Options
jsonOpts = defaultOptions
  { fieldLabelModifier = toDashedField
  , omitNothingFields  = True }
