module Model where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Quasi
import Data.Monoid ((<>))
import Owl.Helpers.Util (toGravatarHash)

data Role = None | Admin
          deriving (Read, Show, Eq, Ord, Enum, Bounded)
derivePersistField "Role"

data VerStatus = Unverified | Verified
            deriving (Read, Show, Eq, Ord, Enum, Bounded)
derivePersistField "VerStatus"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

userFullname :: User -> Text
userFullname u = if T.null $ userFamilyname u <> userGivenname u
                 then "(no name)"
                 else userFamilyname u <> " " <> userGivenname u

userMd5hash' :: User -> Text
userMd5hash' u = case userMd5hash u of
  Just x -> x
  Nothing -> toGravatarHash $ userUsername u

defUser :: User
defUser = User { userUsername = ""
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
               }
