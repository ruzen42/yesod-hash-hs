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
import qualified Data.ByteArray.Encoding as BAE

data HashType 
  = SHA2_256 
  | SHA2_512 
  | SHA3_256 
  | SHA3_512 
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
                    SHA2_256 -> hashT256 
                    SHA2_512 -> hashT256 
                    _        -> hashT256 

    return $ toHex $ hashMethod (source body)

hashT256 :: Text -> Digest SHA256
hashT256 t = hash (TE.encodeUtf8 t)

toHex :: Digest SHA256 -> T.Text
toHex = TE.decodeUtf8 . BAE.convertToBase BAE.Base16

