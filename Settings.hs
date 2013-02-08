-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

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
