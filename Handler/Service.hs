module Handler.Service
       ( postAuthenticateR
       ) where

import Import
import Data.Maybe (fromJust)
import Owl.Helpers.Auth.HashDB (validateUser)

postAuthenticateR :: Handler RepJson
postAuthenticateR = do
  mauth <- (,) <$> lookupPostParam "ident" <*> lookupPostParam "pass"
  case mauth of
    (Just ident, Just pass) -> do
      checked <- validateUser (UniqueUser ident) pass
      if checked then authenticated ident else don'tAuthenticated (ident, pass)
    _ -> invalidArgs ["Not found required query parameter."]
  where
    authenticated ident = do
      u <- runDB $ getBy404 (UniqueUser ident)
      jsonToRepJson $ object [ "authenticate" .= True
                             , "user" .= object [ "ident" .= ident
                                                , "email" .= userEmail (entityVal u)
                                                ]
                             ]
    don'tAuthenticated (ident, pass) = do 
      jsonToRepJson $ object [ "authenticate" .= False
                             , "rejected" .= object [ "ident" .= ident
                                                    , "pass" .= pass
                                                    ]
                             ]
