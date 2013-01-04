module Owl.Helpers.Util 
       ( newIdent2
       , newIdent3
       , newIdent4
       ) where

import Import

newIdent2 :: Handler (Text, Text)
newIdent2 = (,) <$> newIdent <*> newIdent

newIdent3 :: Handler (Text, Text, Text)
newIdent3 = (,,) <$> newIdent <*> newIdent <*> newIdent

newIdent4 :: Handler (Text, Text, Text, Text)
newIdent4 = (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent

