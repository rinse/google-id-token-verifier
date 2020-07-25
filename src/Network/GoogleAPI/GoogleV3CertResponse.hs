{-# LANGUAGE DeriveGeneric #-}
module Network.GoogleAPI.GoogleV3CertResponse where

import           Crypto.JOSE.JWK (JWK)
import           Data.Aeson      (FromJSON, ToJSON)
import           GHC.Generics    (Generic)


-- |Response JSON type of https://www.googleapis.com/oauth2/v3/certs
newtype GoogleV3CertResponse = GoogleV3CertResponse
    { keys :: [JWK]
    } deriving (Generic, Show)

instance FromJSON GoogleV3CertResponse
instance ToJSON GoogleV3CertResponse
