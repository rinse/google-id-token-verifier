{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.GoogleAPI.GoogleIdTokenVerifier
    ( GoogleIdTokenString
    , GoogleIdTokenJWT
    , ClientId
    , decodeGoogleIdTokenString
    , verifyGoogleIdTokenJWT
    , verifyGoogleIdTokenJWTIO
    ) where

import           Prelude                                hiding (exp)

import           Control.Arrow                          ((|||))
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.JOSE.JWS                        (JWKSet (..),
                                                         verifyJWS')
import           Crypto.JWT                             (SignedJWT,
                                                         decodeCompact)
import           Data.Aeson                             (FromJSON, eitherDecode)
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Text                              as T
import           Data.Time.Clock.POSIX                  (POSIXTime,
                                                         getPOSIXTime)
import           Network.HTTP.Req

import           Network.GoogleAPI.GoogleIdToken
import           Network.GoogleAPI.GoogleV3CertResponse
import           Network.GoogleAPI.JoseException


type GoogleIdTokenString = BSL.ByteString
type GoogleIdTokenJWT = SignedJWT

-- |Decodes GoogleIdTokenString.
-- |GoogleIdTokenString is just a lazy bytestring which represents GoogleIdToken in JWT.
decodeGoogleIdTokenString :: MonadThrow m => GoogleIdTokenString -> m GoogleIdTokenJWT
decodeGoogleIdTokenString = throwJoseError . decodeCompact

verifyJWTInternal :: MonadThrow m => JWKSet -> GoogleIdTokenJWT -> m BS.ByteString
verifyJWTInternal jwkset signedJwt = throwJoseError $ verifyJWS' jwkset signedJwt

type ClientId = T.Text

googleIss :: [T.Text]
googleIss = ["accounts.google.com", "https://accounts.google.com"]

-- |Verifies GoogleIdToken following the guidelines
-- |https://developers.google.com/identity/sign-in/web/backend-auth#verify-the-integrity-of-the-id-token
verifyGoogleIdToken :: MonadThrow m => POSIXTime -> ClientId -> GoogleIdToken -> m ()
verifyGoogleIdToken posixTime clientId GoogleIdToken{..} = do
    when (aud /= clientId) $ throwString "AudMismatched"
    when (iss `notElem` googleIss) $ throwString "IssMimatched"
    when (posixTime > exp) $ throwString "TokenExpired"
    when (posixTime < iat) $ throwString "InvalidIat"

liftStringError :: MonadThrow m => Either String a -> m a
liftStringError = throwString ||| return

decodeVerifiedJWT :: (MonadThrow m, FromJSON a) => BS.ByteString -> m a
decodeVerifiedJWT = fmap liftStringError eitherDecode . BSL.fromStrict

-- |Verifies JWT and GoogleIdToken and returns payloads.
verifyGoogleIdTokenJWT :: (FromJSON a, MonadThrow m) => POSIXTime -> JWKSet -> ClientId -> GoogleIdTokenJWT -> m a
verifyGoogleIdTokenJWT posixTime jwkset clientId signedJwt = do
    verifiedBS <- verifyJWTInternal jwkset signedJwt
    googleIdToken <- decodeVerifiedJWT verifiedBS
    verifyGoogleIdToken posixTime clientId googleIdToken
    decodeVerifiedJWT verifiedBS

-- |Verifies JWT and GoogleIdToken and returns payloads.
verifyGoogleIdTokenJWTIO :: (FromJSON a, MonadThrow m, MonadIO m) => ClientId -> GoogleIdTokenJWT -> m a
verifyGoogleIdTokenJWTIO clientId signedJwt = do
    jwtSet <- requestGoogleAPIJwkSet
    posixTime <- liftIO getPOSIXTime
    verifyGoogleIdTokenJWT posixTime jwtSet clientId signedJwt

requestGoogleAPIJwkSet :: MonadIO m => m JWKSet
requestGoogleAPIJwkSet = JWKSet . keys . responseBody <$> runReq defaultHttpConfig jwkRequest
    where
    jwkRequest = req GET url NoReqBody jsonResponse mempty
    url = https "www.googleapis.com" /: "oauth2" /: "v3" /: "certs"
