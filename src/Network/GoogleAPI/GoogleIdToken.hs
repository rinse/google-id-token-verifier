{-# LANGUAGE DeriveGeneric     #-}
module Network.GoogleAPI.GoogleIdToken where

import           Data.Aeson
import qualified Data.Text                       as T
import           Data.Time.Clock.POSIX
import           GHC.Generics                    (Generic)


data GoogleIdToken = GoogleIdToken
    { iss :: T.Text
    , aud :: T.Text
    , azp :: T.Text
    , iat :: POSIXTime
    , exp :: POSIXTime
    , sub :: T.Text
    } deriving (Generic, Show)

instance ToJSON GoogleIdToken
instance FromJSON GoogleIdToken
