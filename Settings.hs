-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
       ( PersistConfig
       , staticDir
       , staticRoot
       , widgetFileSettings
       , widgetFile
       , Extra(..)
       , parseExtra
       , userNumPerPage
       , fillGapWidth
       , pagenateWidth
       , owlEmailAddress
       , owl_pub
       , owl_priv
       , clientPublicKeys
       ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import qualified Data.ByteString as B
import Text.Hamlet
import Network.Mail.Mime (Address(..))
import Crypto.PubKey.RSA

-- | Which Persistent backend this site is using.
type PersistConfig = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"

userNumPerPage :: Int
userNumPerPage = 10
fillGapWidth :: Int
fillGapWidth = 3
pagenateWidth :: Int
pagenateWidth = 3

owlEmailAddress :: Address
owlEmailAddress = Address { addressName = Just "Owl system"
                          , addressEmail = "noreply"
                          }

-- |
-- Owl RSA keys
--
owl_pub :: PublicKey
owl_pub = PublicKey { public_size = 128
                    , public_n = 133978865898371049756915690541771190310631304559640804303990893481160872232160722925370093358396509346866547508177130752551249861802825991982314077620630462699557927940806588373415331051489847062718976316744747135498419296507215040001805779727816051742538971179969585665983463554641712741262022247106195741053
                    , public_e = 65537
                    }

owl_priv :: PrivateKey
owl_priv = PrivateKey { private_pub =
                           PublicKey { public_size = 128
                                     , public_n = 133978865898371049756915690541771190310631304559640804303990893481160872232160722925370093358396509346866547508177130752551249861802825991982314077620630462699557927940806588373415331051489847062718976316744747135498419296507215040001805779727816051742538971179969585665983463554641712741262022247106195741053
                                     , public_e = 65537
                                     }
                      , private_d = 19212557512746862468765638642470141241425652688580561802476561590184931828399933992288755014453063076458364183313985608320134369916580842465321691586106853692871897012390839282681616460844036213721385349961942497415273654854238296042505585104980989086801928365757789852599372134414803954265273385830072361025
                      , private_p = 0
                      , private_q = 0
                      , private_dP = 0
                      , private_dQ = 0
                      , private_qinv = 0
                      }
-- |
-- Kestrel RSA keys
--
kestrel_pub :: PublicKey
kestrel_pub = PublicKey
               { public_size = 128
               , public_n = 120332330775436211464295534571972888673387300679598566062508608458655719305789686349395492378128543547366411787689981100009887893124585139574893280404868432860950478019280547842280406034811001082141945533376685014426870049436530317211335145209888151263557448528262054842217958692127741265705082667427881950147
               , public_e = 65537
               }
-- |
-- BISocie RSA keys
--
bisocie_pub :: PublicKey
bisocie_pub = PublicKey
                { public_size = 128
                , public_n = 142887632286261757537094637659623324734697953632479544023914951183445364758392871651394986748021326605711095552963080510340887443639041675225698836993818697214651802692561889331648696803649628007583123319923152270864869553112419238211869186697095157615128026430308563152416661072674744763116497602227470107301
                , public_e = 65537
                }

clientPublicKeys :: [(B.ByteString, PublicKey)]
clientPublicKeys = [ ("Kestrel", kestrel_pub)
                   , ("BISocie", bisocie_pub)
                   ]
