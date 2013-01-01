module Owl.Helpers.Auth.HashDB
    ( HashDBUser(..)
    , Unique (..)
    , setPassword
      -- * Authentification
    , validateUser
    , authHashDB
    , getAuthIdHashDB
      -- * Predefined data type
    , User
    , UserGeneric (..)
    , UserId
    , migrateUsers
    ) where

import Prelude
import Yesod.Persist
import Yesod.Handler
import Yesod.Form
import Yesod.Auth
import Yesod.Widget (toWidget)
import Text.Hamlet (hamlet)

import Control.Applicative         ((<$>), (<*>))
import Control.Monad               (replicateM,liftM)
import Control.Monad.IO.Class      (MonadIO, liftIO)

import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)
import Data.Text                   (Text, pack, unpack, append)
import Data.Maybe                  (fromMaybe)
import System.Random               (randomRIO)

-- | Interface for data type which holds user info. It's just a
--   collection of getters and setters
class HashDBUser user where
  -- | Retrieve password hash from user data
  userPasswordHash :: user -> Maybe Text
  -- | Retrieve salt for password
  userPasswordSalt :: user -> Maybe Text

  -- | Deprecated for the better named setSaltAndPasswordHash 
  setUserHashAndSalt :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setUserHashAndSalt = setSaltAndPasswordHash

  -- | a callback for setPassword
  setSaltAndPasswordHash :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setSaltAndPasswordHash = setUserHashAndSalt

-- | Generate random salt. Length of 8 is chosen arbitrarily
randomSalt :: MonadIO m => m Text
randomSalt = pack `liftM` liftIO (replicateM 8 (randomRIO ('0','z')))

-- | Calculate salted hash using SHA1.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt = 
  pack . showDigest . sha1 . BS.pack . unpack . append salt

-- | Set password for user. This function should be used for setting
--   passwords. It generates random salt and calculates proper hashes.
setPassword :: (MonadIO m, HashDBUser user) => Text -> user -> m user
setPassword pwd u = do salt <- randomSalt
                       return $ setSaltAndPasswordHash salt (saltedHash salt pwd) u


----------------------------------------------------------------
-- Authentification
----------------------------------------------------------------

-- | Given a user ID and password in plaintext, validate them against
--   the database values.
validateUser :: ( YesodPersist yesod
                , b ~ YesodPersistBackend yesod
                , b ~ PersistEntityBackend user
                , PersistStore b (GHandler sub yesod)
                , PersistUnique b (GHandler sub yesod)
                , PersistEntity user
                , HashDBUser    user
                ) => 
                Unique user b   -- ^ User unique identifier
             -> Text            -- ^ Password in plaint-text
             -> GHandler sub yesod Bool
validateUser userID passwd = do
  -- Checks that hash and password match
  let validate u = do hash <- userPasswordHash u
                      salt <- userPasswordSalt u
                      return $ hash == saltedHash salt passwd
  -- Get user data
  user <- runDB $ getBy userID
  return $ fromMaybe False $ validate . entityVal =<< user


login :: AuthRoute
login = PluginR "hashdb" ["login"]


-- | Handle the login form. First parameter is function which maps
--   username (whatever it might be) to unique user ID.
postLoginR :: ( YesodAuth y, YesodPersist y
              , HashDBUser user, PersistEntity user
              , b ~ YesodPersistBackend y
              , b ~ PersistEntityBackend user
              , PersistStore b (GHandler Auth y)
              , PersistUnique b (GHandler Auth y)
              )
           => (Text -> Maybe (Unique user b))
           -> GHandler Auth y ()
postLoginR uniq = do
    (mu,mp) <- runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    isValid <- fromMaybe (return False) 
                 (validateUser <$> (uniq =<< mu) <*> mp)
    if isValid 
       then do setCreds True $ Creds "hashdb" (fromMaybe "" mu) []
               y <- getYesod
               redirectUltDest $ loginDest y
       else do setMessage "Invalid username/password"
               toMaster <- getRouteToMaster
               redirect $ toMaster LoginR


-- | A drop in for the getAuthId method of your YesodAuth instance which
--   can be used if authHashDB is the only plugin in use.
getAuthIdHashDB :: ( YesodAuth master, YesodPersist master
                   , HashDBUser user, PersistEntity user
                   , Key b user ~ AuthId master
                   , b ~ YesodPersistBackend master
                   , b ~ PersistEntityBackend user
                   , PersistUnique b (GHandler sub master)
                   , PersistStore b (GHandler sub master)
                   )
                => (AuthRoute -> Route master)   -- ^ your site's Auth Route
                -> (Text -> Maybe (Unique user b)) -- ^ gets user ID
                -> Creds master                  -- ^ the creds argument
                -> GHandler sub master (Maybe (AuthId master))
getAuthIdHashDB authR uniq creds = do
    muid <- maybeAuthId
    case muid of
        -- user already authenticated
        Just uid -> return $ Just uid
        Nothing       -> do
            x <- case uniq (credsIdent creds) of
                   Nothing -> return Nothing
                   Just u  -> runDB (getBy u)
            case x of
                -- user exists
                Just (Entity uid _) -> return $ Just uid
                Nothing       -> do
                    setMessage "User not found"
                    redirect $ authR LoginR

-- | Prompt for username and password, validate that against a database
--   which holds the username and a hash of the password
authHashDB :: ( YesodAuth m, YesodPersist m
              , HashDBUser user
              , PersistEntity user
              , b ~ YesodPersistBackend m
              , b ~ PersistEntityBackend user
              , PersistStore b (GHandler Auth m)
              , PersistUnique b (GHandler Auth m))
           => (Text -> Maybe (Unique user b)) -> AuthPlugin m
authHashDB uniq = AuthPlugin "hashdb" dispatch $ \tm -> do 
  name <- lift newIdent
  pwd  <- lift newIdent
  toWidget [hamlet|
<div .page-header>
  <h3>Login
<form method="post" action="@{tm login}" .form-horizontal>
  <div .control-group.info>
    <label .control-label for="##{name}">Account ID
    <div .controls>
      <input type="text" ##{name} name="username" .span3 autofocus="" required>
  <div .control-group.info>
    <label .control-label for="##{pwd}">Password
    <div .controls>
      <input type="password" ##{pwd} name="password" .span3 required>
  <div .control-group>
    <div .controls.btn-group>
    <input type="submit" .btn.btn-primary value="Login">

  <script>
    if (!("autofocus" in document.createElement("input"))) {
      document.getElementById("#{name}").focus();
    }
|]
    where
        dispatch "POST" ["login"] = postLoginR uniq >>= sendResponse
        dispatch _ _              = notFound


----------------------------------------------------------------
-- Predefined datatype
----------------------------------------------------------------

-- | Generate data base instances for a valid user
share [mkPersist sqlSettings, mkMigrate "migrateUsers"]
         [persistUpperCase|
User
    username Text Eq
    password Text
    salt     Text
    UniqueUser username
|]

instance HashDBUser (UserGeneric backend) where
  userPasswordHash = Just . userPassword
  userPasswordSalt = Just . userSalt
  setSaltAndPasswordHash s h u = u { userSalt     = s
                               , userPassword = h
                               }