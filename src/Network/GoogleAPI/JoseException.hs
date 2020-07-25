module Network.GoogleAPI.JoseException where

import           Control.Arrow          ((|||))
import           Control.Exception.Safe (Exception, MonadThrow, throw)
import           Crypto.JOSE.Error      (Error)


newtype JoseException = JoseException Error deriving (Show)

instance Exception JoseException

throwJoseError :: MonadThrow m => Either Error a -> m a
throwJoseError = (throw . JoseException) ||| return
