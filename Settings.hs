-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import Database.Persist.Postgresql (PostgresConf)
import qualified Data.ByteString as B
import Network.Mail.Mime (Address(..))
import Crypto.PubKey.RSA

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .: "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

-- The rest of this file contains owl's settings

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
