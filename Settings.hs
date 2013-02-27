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
       , Client(..)
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
userNumPerPage = 30
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
owl_pub = PublicKey { public_size = 256
                    , public_n = 23935502722801508291122222398018117881284958223263854065673689606867055652122077115632498984650750679970467900697728966520426415008444072251453446123881488809248692462117519335720631061157343736650249371835293662619945999329307142886808914215692490190245599500864907497806854772652186075160282343362861100625964817657470875052275949634580109631117392627776939182328215081842240646543745078419135398375800047086393491931547537516953037019818981085723402984601825491050312705896863144307436654552505557222743591857763940190952404403348742192979262305085887506928609325609473826220183742944601830381993567783603917096371
                    , public_e = 65537
                    }

owl_priv :: PrivateKey
owl_priv = PrivateKey { private_pub =
                           PublicKey { public_size = 256
                                     , public_n = 23935502722801508291122222398018117881284958223263854065673689606867055652122077115632498984650750679970467900697728966520426415008444072251453446123881488809248692462117519335720631061157343736650249371835293662619945999329307142886808914215692490190245599500864907497806854772652186075160282343362861100625964817657470875052275949634580109631117392627776939182328215081842240646543745078419135398375800047086393491931547537516953037019818981085723402984601825491050312705896863144307436654552505557222743591857763940190952404403348742192979262305085887506928609325609473826220183742944601830381993567783603917096371
                                     , public_e = 65537
                                     }
                      , private_d = 12571644639276645534537881492661849851375418953586346409640657089088267538296628446134273464602409934936653281014041327579323405975565891863215138356596252617482620381326720029512726587838291134824219660303561018275230498633037985755370804967156978459933076369360389488220509864416032913913777542811486714462612131970371715082830263182084577895598321361919322815869813189435189210824686617452762312871684195761327186259768348164478401930840390156610664951164933186582895066392106786716843002355768729204751023998610282892428336748125415554581186490341353020312987484911314490221636618677220440014796137038456228337473
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
kestrel_pub = PublicKey { public_size = 256
                        , public_n = 21228709312220288517503495190383087008620565254339967134095466068365647031226729669265671664798784928948846353620011623578414876424990762980943729885946652556572428902689997212044632493925435778543200901607282978155864935426522893231195315858378754230462066295557643431595879074834502607097823427913242585518657629041668450361956795113827730227717621313907040381667287023411039162112335885551742896477496864860480202946704420444852614551591081039685989010619581744204539847080564272228683094296009683827755542701376148992554748322533792248770505527486231377342851844922810933440422738171377892597476265198722904705743
                        , public_e = 65537
                        }
-- |
-- BISocie RSA keys
--
bisocie_pub :: PublicKey
bisocie_pub = PublicKey { public_size = 256
                        , public_n = 26870894169656268922545466741244168653544649756656829956589570478322104073464464636099601680532399473063611528659467583653152563017053866283270489292347012891333147755789584030406827683655185500190631673233269615494706763605175355492877596147900145933645660705964615619338455015241164432176334486419428384384974050406087406010204416327844796922320664721237134256567167659312781788762669925188944476890454877966341502697521958464868440718211676830032809781819146851835132213216793606593877127871190638938341522988710426877815716099448539090381927083488689515969622028201590093791836441861188546136053472609977446229559
                        , public_e = 65537
                        }

data Client = Client { clientId :: B.ByteString
                     , clientName :: Text
                     , pubkey :: PublicKey
                     }

clientPublicKeys :: [Client]
clientPublicKeys = [ Client "Kestrel" "Kestrel" kestrel_pub
                   , Client "BISocie" "BISocie" bisocie_pub
                   ]
