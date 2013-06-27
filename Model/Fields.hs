module Model.Fields where

import Prelude
import Yesod

data Role = None | Admin
          deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "Role"

data VerStatus = Unverified | Verified
            deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "VerStatus"
