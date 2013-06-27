module Model ( module Model
             , module Model.Fields
             ) where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist.Quasi
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Owl.Helpers.Util (toGravatarHash)
import Model.Fields

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

userFullname :: User -> Text
userFullname u = if T.null $ userFamilyname u <> userGivenname u
                 then "(no name)"
                 else userFamilyname u <> " " <> userGivenname u

userMd5hash' :: User -> Text
userMd5hash' u = case userMd5hash u of
  Just x -> x
  Nothing -> toGravatarHash $ userUsername u

newUser :: IO User
newUser = do
  now <- liftIO getCurrentTime
  return $ User { userUsername = ""
                , userPassword = ""
                , userSalt = ""
                , userRole = None
                , userFamilyname = ""
                , userGivenname = ""
                , userComment = Nothing
                , userEmail = Nothing
                , userVerkey = Nothing
                , userVerstatus = Nothing
                , userMd5hash = Nothing
                , userCreated = now
                , userUpdated = now
                }
