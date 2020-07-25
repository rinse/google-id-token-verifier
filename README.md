# The GoogleIdToken verifier for Haskell.

## What is GoogleIdToken?

See: https://developers.google.com/identity/sign-in/web/backend-auth#verify-the-integrity-of-the-id-token

## How to use it?

You should have your ClientId of your Google App and GoogleIdToken from an end user.

```Haskell
import Data.Aeson   (FromJSON)
import GHC.Generics (Generic)
import Network.GoogleAPI.GoogleIdTokenVerifier


yourClientId :: ClientId
yourClientId = "yourClientId"

yourIdTokenString :: GoogleIdTokenString
yourIdTokenString = "yourIdTokenString"

data YourPayload = YourPayload deriving (Generics, Show)

instance FromJSON YourPayload

sample :: IO ()
sample = do
    yourPayload <- verifyGoogleIdTokenStringIO yourClientId yourIdTokenString
    print yourPayload
```
