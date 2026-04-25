{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module Hash (postHashR) where

import Foundation
import Yesod.Core
import Yesod.Core.Handler (rawRequestBody)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder.RealFloat as GHC
import GHC.Generics (Generic)
import Crypto.Hash
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE

data HashType 
  = SHAT256 
  | SHAT512 
  | SHAH256 
  | SHAH512 
  deriving Generic


data HashRequest = HashRequest 
  { source       :: Text 
  , hash_method  :: HashType -- using _ for JSON compat
  } deriving Generic

instance FromJSON HashRequest
instance FromJSON HashType 

postHashR :: Handler Text 
postHashR = do 
    body <- requireCheckJsonBody :: Handler HashRequest 
    
    let hashMethod = case hash_method body of 
                    SHAT256 -> hashT256 
                    SHAT512 -> hashT512 
                    SHAH512 -> hashT256 
                    SHAH256 -> hashT512 
                    _       -> hashT256 

    return $ toHex $ hashMethod (source body)

hashT256 :: Text -> Digest SHA256
hashT256 t = hash (TE.encodeUtf8 t)

hashT512 :: Text -> Digest SHA256
hashT512 t = hash (TE.encodeUtf8 t)

toHex :: BA.ByteArrayAccess ba => ba -> T.Text
toHex = TE.decodeUtf8 . BAE.convertToBase BAE.Base16

