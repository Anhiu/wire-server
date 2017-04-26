{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Brig.App
    ( schemaVersion

      -- * App Environment
    , Env
    , newEnv
    , closeEnv
    , awsEnv
    , awsConfig
    , galley
    , gundeck
    , userTemplates
    , providerTemplates
    , teamTemplates
    , requestId
    , httpManager
    , extGetManager
    , settings
    , currentTime
    , geoDb
    , zauthEnv
    , digestSHA256
    , digestMD5
    , metrics
    , applog
    , turnEnv

      -- * App Monad
    , AppT
    , AppIO
    , runAppT
    , runAppResourceT
    , forkAppIO

    , locationOf
    ) where

import Bilge (MonadHttp, Manager, newManager, RequestId (..))
import Bilge.RPC (HasRequestId (..))
import Brig.Options (Opts (..), Settings (..))
import Brig.Template (Localised, forLocale)
import Brig.Provider.Template
import Brig.Team.Template
import Brig.User.Search.Index (runIndexIO, IndexEnv (..), MonadIndexIO (..))
import Brig.User.Template
import Brig.Types (Locale (..))
import Brig.ZAuth (MonadZAuth (..), runZAuth)
import Cassandra (MonadClient (..), Keyspace (..), runClient)
import Cassandra.Schema (versionCheck)
import Control.AutoUpdate
import Control.Concurrent (forkIO)
import Control.Error
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding ((.=))
import Control.Monad (liftM2, void, (>=>))
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, transResourceT)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Id (UserId)
import Data.IP
import Data.List1 (list1, List1)
import Data.Metrics (Metrics)
import Data.Misc
import Data.Int (Int32)
import Data.IORef
import Data.Time.Clock
import Data.Word
import Network.HTTP.Client (ManagerSettings (..), responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import OpenSSL.EVP.Digest (getDigestByName, Digest)
import OpenSSL.Session (SSLOption (..))
import System.Directory (canonicalizePath)
import System.Logger.Class hiding (Settings, settings)

import qualified Bilge                    as RPC
import qualified Brig.Aws.Types           as Aws
import qualified Brig.TURN                as TURN
import qualified Brig.ZAuth               as ZAuth
import qualified Cassandra                as Cas
import qualified Cassandra.Settings       as Cas
import qualified Data.GeoIP2              as GeoIp
import qualified Data.List.NonEmpty       as NE
import qualified Data.Metrics.Middleware  as Metrics
import qualified Database.V5.Bloodhound   as ES
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.IO             as Text
import qualified OpenSSL.Session          as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import qualified Ropes.Aws                as Aws
import qualified System.FilePath          as Path
import qualified System.FSNotify          as FS
import qualified System.Logger            as Log
import qualified System.Logger.Class      as LC

schemaVersion :: Int32
schemaVersion = 43

-------------------------------------------------------------------------------
-- Environment

data Env = Env
    { _galley        :: RPC.Request
    , _gundeck       :: RPC.Request
    , _casClient     :: Cas.ClientState
    , _awsEnv        :: Aws.Env
    , _awsConfig     :: Aws.Config
    , _metrics       :: Metrics
    , _applog        :: Logger
    , _requestId     :: RequestId
    , _usrTemplates  :: Localised UserTemplates
    , _provTemplates :: Localised ProviderTemplates
    , _tmTemplates   :: Localised TeamTemplates
    , _httpManager   :: Manager
    , _extGetManager :: [Fingerprint Rsa] -> Manager
    , _settings      :: Settings
    , _geoDb         :: Maybe (IORef GeoIp.GeoDB)
    , _fsWatcher     :: FS.WatchManager
    , _turnEnv       :: IORef TURN.Env
    , _currentTime   :: IO UTCTime
    , _zauthEnv      :: ZAuth.Env
    , _digestSHA256  :: Digest
    , _digestMD5     :: Digest
    , _indexEnv      :: IndexEnv
    }

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv o = do
    Just md5 <- getDigestByName "MD5"
    Just sha256 <- getDigestByName "SHA256"
    Just sha512 <- getDigestByName "SHA512"
    mtr <- Metrics.metrics
    lgr <- Log.new $ defSettings & setOutput StdOut . setFormat Nothing
    cas <- initCassandra o lgr
    mgr <- initHttpManager
    ext <- initExtGetManager
    utp <- loadUserTemplates o
    ptp <- loadProviderTemplates o
    ttp <- loadTeamTemplates o
    aws <- initAws o lgr mgr
    zau <- initZAuth o
    clock <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    w   <- FS.startManagerConf
         $ FS.defaultConfig { FS.confDebounce = FS.Debounce 5, FS.confPollInterval = 10000000 }
    g   <- geoSetup lgr w (optGeoDb o)
    t   <- turnSetup lgr w sha512 (optTurnServers o) (optTurnLifetime o)
            =<< (Text.encodeUtf8 . Text.strip <$> Text.readFile (optTurnSecret o))
    return $! Env
        { _galley        = endpoint (optGalleyHost  o) (optGalleyPort  o)
        , _gundeck       = endpoint (optGundeckHost o) (optGundeckPort o)
        , _casClient     = cas
        , _awsEnv        = fst aws
        , _awsConfig     = snd aws
        , _metrics       = mtr
        , _applog        = lgr
        , _requestId     = mempty
        , _usrTemplates  = utp
        , _provTemplates = ptp
        , _tmTemplates   = ttp
        , _httpManager   = mgr
        , _extGetManager = ext
        , _settings      = optSettings o
        , _geoDb         = g
        , _turnEnv       = t
        , _fsWatcher     = w
        , _currentTime   = clock
        , _zauthEnv      = zau
        , _digestMD5     = md5
        , _digestSHA256  = sha256
        , _indexEnv      = mkIndexEnv o lgr mgr mtr
        }
  where
    endpoint h p = RPC.host h . RPC.port p $ RPC.empty

mkIndexEnv :: Opts -> Logger -> Manager -> Metrics -> IndexEnv
mkIndexEnv o lgr mgr mtr =
    let bhe  = ES.mkBHEnv (ES.Server (optElasticsearchUrl o)) mgr
        lgr' = Log.clone (Just "index.brig") lgr
    in IndexEnv mtr lgr' bhe Nothing (optUserIndex o)

geoSetup :: Logger -> FS.WatchManager -> Maybe FilePath -> IO (Maybe (IORef GeoIp.GeoDB))
geoSetup _   _ Nothing   = return Nothing
geoSetup lgr w (Just db) = do
    path  <- canonicalizePath db
    geodb <- newIORef =<< GeoIp.openGeoDB path
    startWatching w path (replaceGeoDb lgr geodb)
    return $ Just geodb

turnSetup :: Logger -> FS.WatchManager -> Digest -> FilePath -> Word32 -> ByteString -> IO (IORef TURN.Env)
turnSetup lgr w dig f ttl secret = do
    path    <- canonicalizePath f
    servers <- fromMaybe (error "Empty TURN list, check turn file!") <$> readTurnList path
    te      <- newIORef =<< TURN.newEnv dig servers ttl secret
    startWatching w path (replaceTurnServers lgr te)
    return te

startWatching :: FS.WatchManager -> FilePath -> FS.Action -> IO ()
startWatching w p = void . FS.watchDir w (Path.dropFileName p) predicate
  where
    predicate (FS.Added f _)    = Path.equalFilePath f p
    predicate (FS.Modified f _) = Path.equalFilePath f p
    predicate (FS.Removed _ _)  = False

replaceGeoDb :: Logger -> IORef GeoIp.GeoDB -> FS.Event -> IO ()
replaceGeoDb g ref e = do
    let logErr x = Log.err g (msg $ val "Error loading GeoIP database: " +++ show x)
    handleAny logErr $ do
        GeoIp.openGeoDB (FS.eventPath e) >>= atomicWriteIORef ref
        Log.info g (msg $ val "New GeoIP database loaded.")

replaceTurnServers :: Logger -> IORef TURN.Env -> FS.Event -> IO ()
replaceTurnServers g ref e = do
    let logErr x = Log.err g (msg $ val "Error loading turn servers: " +++ show x)
    handleAny logErr $ readTurnList (FS.eventPath e) >>= \case
        Just servers -> readIORef ref >>= \old -> do
            atomicWriteIORef ref (old & TURN.turnServers .~ servers)
            Log.info g (msg $ val "New turn servers loaded.")
        Nothing -> Log.warn g (msg $ val "Empty or malformed turn servers list, ignoring!")

initAws :: Opts -> Logger -> Manager -> IO (Aws.Env, Aws.Config)
initAws o l m = do
    e <- Aws.newEnv l m (liftM2 (,) (optAwsKeyId o) (optAwsSecretKey o))
    let c = Aws.config (optAwsAccount o) (optAwsSesQueue o) (optAwsInternalQueue o)
                       (optAwsBlacklistTable o) (optAwsPreKeyTable o)
    return (e, c)

initZAuth :: Opts -> IO ZAuth.Env
initZAuth o = do
    sk <- ZAuth.readKeys (optZAuthPrivateKeys o)
    pk <- ZAuth.readKeys (optZAuthPublicKeys o)
    case (sk, pk) of
        (Nothing,     _) -> error ("No private key in: " ++ optZAuthPrivateKeys o)
        (_,     Nothing) -> error ("No public key in: " ++ optZAuthPublicKeys o)
        (Just s, Just p) -> ZAuth.mkEnv s p (optZAuthSettings o)

initHttpManager :: IO Manager
initHttpManager = do
    ctx <- SSL.context
    SSL.contextAddOption ctx SSL_OP_NO_SSLv2
    SSL.contextAddOption ctx SSL_OP_NO_SSLv3
    SSL.contextSetCiphers ctx "HIGH"
    SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
    SSL.contextLoadSystemCerts ctx
    -- Unfortunately, there are quite some AWS services we talk to
    -- (e.g. SES, Dynamo) that still only support TLSv1.
    -- Ideally: SSL.contextAddOption ctx SSL_OP_NO_TLSv1
    newManager (opensslManagerSettings ctx)
        { managerConnCount           = 1024
        , managerIdleConnectionCount = 4096
        , managerResponseTimeout     = responseTimeoutMicro 10000000
        }

initExtGetManager :: IO ([Fingerprint Rsa] -> Manager)
initExtGetManager = do
    ctx <- SSL.context
    SSL.contextAddOption ctx SSL_OP_NO_SSLv2
    SSL.contextAddOption ctx SSL_OP_NO_SSLv3
    SSL.contextSetCiphers ctx rsaCiphers
    -- We use public key pinning with service providers and want to
    -- support self-signed certificates as well, hence 'VerifyNone'.
    SSL.contextSetVerificationMode ctx SSL.VerifyNone
    SSL.contextLoadSystemCerts ctx
    mgr <- newManager (opensslManagerSettings ctx)
        { managerConnCount           = 100
        , managerIdleConnectionCount = 512
        , managerResponseTimeout     = responseTimeoutMicro 10000000
        }
    Just sha <- getDigestByName "SHA256"
    return (mkManager ctx sha mgr)
  where
    mkManager ctx sha mgr fprs =
        let pinset = map toByteString' fprs
        in setOnConnection ctx (verifyRsaFingerprint sha pinset) mgr

initCassandra :: Opts -> Logger -> IO Cas.ClientState
initCassandra o g = do
    c <- maybe (return $ NE.fromList [optCassHost o])
               (Cas.initialContacts "cassandra_brig")
               (optDiscoUrl o)
    p <- Cas.init (Log.clone (Just "cassandra.brig") g)
            $ Cas.setContacts (NE.head c) (NE.tail c)
            . Cas.setPortNumber (fromIntegral (optCassPort o))
            . Cas.setKeyspace (Keyspace (optCassKeyspace o))
            . Cas.setMaxConnections 4
            . Cas.setPoolStripes 4
            . Cas.setSendTimeout 3
            . Cas.setResponseTimeout 10
            . Cas.setProtocolVersion Cas.V3
            $ Cas.defSettings
    runClient p $ versionCheck schemaVersion
    return p

userTemplates :: Monad m => Maybe Locale -> AppT m (Locale, UserTemplates)
userTemplates l = forLocale l <$> view usrTemplates

providerTemplates :: Monad m => Maybe Locale -> AppT m (Locale, ProviderTemplates)
providerTemplates l = forLocale l <$> view provTemplates

teamTemplates :: Monad m => Maybe Locale -> AppT m (Locale, TeamTemplates)
teamTemplates l = forLocale l <$> view tmTemplates

closeEnv :: Env -> IO ()
closeEnv e = do
    Cas.shutdown $ e^.casClient
    FS.stopManager $ e^.fsWatcher
    Log.flush    $ e^.applog
    Log.close    $ e^.applog

-------------------------------------------------------------------------------
-- App Monad

newtype AppT m a = AppT (ReaderT Env m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadReader Env
             )

type AppIO = AppT IO

instance MonadIO m => MonadLogger (AppT m) where
    log l m = do
        g <- view applog
        r <- view requestId
        Log.log g l $ field "request" (unRequestId r) ~~ m

instance MonadIO m => MonadLogger (ExceptT err (AppT m)) where
    log l m = lift (LC.log l m)

instance Monad m => MonadHttp (AppT m) where
    getManager = view httpManager

instance MonadIO m => MonadZAuth (AppT m) where
    liftZAuth za = view zauthEnv >>= \e -> runZAuth e za

instance MonadIO m => MonadZAuth (ExceptT err (AppT m)) where
    liftZAuth = lift . liftZAuth

instance (MonadThrow m, MonadCatch m, MonadIO m) => MonadClient (AppT m) where
   liftClient m = view casClient >>= \c -> runClient c m
   localState f = local (over casClient f)

instance MonadIndexIO AppIO where
    liftIndexIO m = view indexEnv >>= \e -> runIndexIO e m

instance Monad m => HasRequestId (AppT m) where
    getRequestId = view requestId

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

runAppResourceT :: ResourceT AppIO a -> AppIO a
runAppResourceT ma = do
    e <- ask
    liftIO . runResourceT $ transResourceT (runAppT e) ma

forkAppIO :: Maybe UserId -> AppIO a -> AppIO ()
forkAppIO u ma = do
    a <- ask
    g <- view applog
    r <- view requestId
    let logErr e = Log.err g $ request r ~~ user u ~~ msg (show e)
    void . liftIO . forkIO $
        either logErr (const $ return ()) =<<
            runExceptT (syncIO $ runAppT a ma)
  where
    request = field "request" . unRequestId
    user    = maybe id (field "user" . toByteString)

locationOf :: (MonadIO m, MonadReader Env m) => IP -> m (Maybe Location)
locationOf ip = view geoDb >>= \case
    Just g -> do
        database <- liftIO $ readIORef g
        return $! do
            (lat, lon) <- GeoIp.geoLocation =<< hush (GeoIp.findGeoData database "en" ip)
            return (location (Latitude lat) (Longitude lon))
    Nothing -> return Nothing

readTurnList :: FilePath -> IO (Maybe (List1 IpAddr))
readTurnList = readFile >=> return . fn . mapMaybe readMay . lines
  where
    fn []     = Nothing
    fn (x:xs) = Just (list1 x xs)
