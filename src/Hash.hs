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

data HashRequest = HashRequest 
  { source :: Text 
  } deriving Generic

instance FromJSON HashRequest

postHashR :: Handler Text 
postHashR = do 
    body <- requireCheckJsonBody :: Handler HashRequest 

    return $ toHex $ hashText (source body)

hashText :: Text -> Digest SHA256
hashText t = hash (TE.encodeUtf8 t)

toHex :: Digest SHA256 -> T.Text
toHex = TE.decodeUtf8 . BAE.convertToBase BAE.Base16

