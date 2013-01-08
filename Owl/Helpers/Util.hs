module Owl.Helpers.Util 
       ( newIdent2
       , newIdent3
       , newIdent4
       , getCurrentRoute'
       ) where

import Prelude (($), return, fmap, Maybe)
import Yesod
import Control.Applicative ((<$>),(<*>))
import Data.Text (Text)


newIdent2 :: Yesod m => GHandler s m (Text, Text)
newIdent2 = (,) <$> newIdent <*> newIdent

newIdent3 :: Yesod m => GHandler s m (Text, Text, Text)
newIdent3 = (,,) <$> newIdent <*> newIdent <*> newIdent

newIdent4 :: Yesod m => GHandler s m (Text, Text, Text, Text)
newIdent4 = (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent


getCurrentRoute' :: Yesod m => GHandler s m (Maybe (Route m))
getCurrentRoute' = do
  mcr' <- getCurrentRoute
  toMaster <- getRouteToMaster
  return $ fmap toMaster mcr'
