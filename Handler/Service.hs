module Handler.Service
       ( postAuthenticateR
       ) where

import Import
import Data.Maybe (fromJust)

postAuthenticateR :: Handler RepJson
postAuthenticateR = do
  mk <- lookupPostParam "auth"
  liftIO $ putStrLn $ show mk
  let (passP, uname) = (maybe False (const True)  mk, mk)
  jsonToRepJson $ object [ "authenticate" .= passP
                         , "user" .= object [ "ident" .= uname
                                            , "email" .= ("cutsea110@gmail.com"::Text)
                                            ]
                         ]
