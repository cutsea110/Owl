module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Monoid ((<>))
import Owl.Helpers.Util (toGravatarHash)

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
userFullname u = userFamilyname u <> " " <> userGivenname u

userMd5hash' :: User -> Text
userMd5hash' u = case userMd5hash u of
  Just x -> x
  Nothing -> toGravatarHash $ userUsername u
