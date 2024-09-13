{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.Acid where

import AccessControl.Acid          ()
import AccessControl.Relation      (object, rels)
import AccessControl.Schema        (schema)
import AccessControl.Check         (RelationState, mkRelationState)
import Clckwrks.NavBar.Acid        (NavBarState       , initialNavBarState)
import Clckwrks.ProfileData.Acid   (ProfileDataState, initialProfileDataState)
import Clckwrks.Types              (UUID)
import Clckwrks.URL                (ClckURL)
import Control.Applicative         ((<$>))
import Control.Exception           (bracket, catch, throw)
import Control.Lens                ((?=), (.=), (^.), (.~), makeLenses, view, set)
import Control.Lens.At             (IxValue(..), Ixed(..), Index(..), At(at))
import Control.Concurrent          (killThread, forkIO)
import Control.Monad.Reader        (ask)
import Control.Monad.State         (modify, put)
import Data.Acid                   (AcidState, Query, Update, createArchive, makeAcidic)
import Data.Acid.Local             (openLocalStateFrom, createCheckpointAndClose)
import Data.Acid.Memory            (openMemoryState)
#if MIN_VERSION_acid_state (0,16,0)
import Data.Acid.Remote            (acidServerSockAddr, skipAuthenticationCheck)
import Data.Int                    (Int64)
import Network.Socket              (SockAddr(SockAddrUnix))
#else
import Data.Acid.Remote            (acidServer, skipAuthenticationCheck)
import Network                     (PortID(UnixSocket))
#endif
import Data.Data                   (Data, Typeable)
import Data.Maybe                  (fromMaybe)
import Data.SafeCopy               (Migrate(..), base, deriveSafeCopy, extension)
import Data.Text                   (Text)
import qualified Data.Text         as Text
import Happstack.Authenticate.Core (SimpleAddress(..))
import Happstack.Authenticate.Handlers (AuthenticateState)
import Happstack.Authenticate.Password.Handlers (PasswordState)
import Prelude                     hiding (catch)
import System.Directory            (removeFile)
import System.FilePath             ((</>))
import System.IO.Error             (isDoesNotExistError)
import HSP.Google.Analytics        (UACCT)

-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState_v0 = CoreState_v0
    { coreUACCT_v0        :: Maybe UACCT  -- ^ Google Account UAACT
    , coreRootRedirect_v0 :: Maybe Text
    }
    deriving (Eq, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''CoreState_v0)

-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState_1 = CoreState_1
    { coreSiteName_1      :: Maybe Text
    , coreUACCT_1         :: Maybe UACCT  -- ^ Google Account UAACT
    , coreRootRedirect_1  :: Maybe Text
    , coreLoginRedirect_1 :: Maybe Text
    }
    deriving (Eq, Data, Typeable, Show)

instance Migrate CoreState_1 where
    type MigrateFrom CoreState_1 = CoreState_v0
    migrate (CoreState_v0 ua rr) = CoreState_1 Nothing ua rr Nothing

$(deriveSafeCopy 1 'extension ''CoreState_1)



-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState_2 = CoreState_2
    { _coreSiteName_2       :: Maybe Text
    , _coreUACCT_2          :: Maybe UACCT  -- ^ Google Account UAACT
    , _coreRootRedirect_2   :: Maybe Text
    , _coreLoginRedirect_2  :: Maybe Text
    , _coreFromAddress_2    :: Maybe SimpleAddress
    , _coreReplyToAddress_2 :: Maybe SimpleAddress
    , _coreSendmailPath_2   :: Maybe FilePath
    , _coreEnableOpenId_2   :: Bool -- ^ allow OpenId authentication
    }
    deriving (Eq, Data, Typeable, Show)
$(deriveSafeCopy 2 'extension ''CoreState_2)

instance Migrate CoreState_2 where
    type MigrateFrom CoreState_2 = CoreState_1
    migrate (CoreState_1 sn ua rr lr) = CoreState_2 sn ua rr lr Nothing Nothing Nothing True

-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState_3 = CoreState_3
    { _coreSiteName_3       :: Maybe Text
    , _coreUACCT_3          :: Maybe UACCT  -- ^ Google Account UAACT
    , _coreRootRedirect_3   :: Maybe Text
    , _coreLoginRedirect_3  :: Maybe Text
    , _coreFromAddress_3    :: Maybe SimpleAddress
    , _coreReplyToAddress_3 :: Maybe SimpleAddress
    , _coreSendmailPath_3   :: Maybe FilePath
    , _coreEnableOpenId_3   :: Bool -- ^ allow OpenId authentication
    , _coreBodyPolicy_3     :: (FilePath, Int64, Int64, Int64) -- ^ (temp directory for uploads, maxDisk, maxRAM, maxHeader)
    }
    deriving (Eq, Data, Typeable, Show)
$(deriveSafeCopy 3 'extension ''CoreState_3)

instance Migrate CoreState_3 where
    type MigrateFrom CoreState_3 = CoreState_2
    migrate (CoreState_2 sn ua rr lr fa rta smp eo) = CoreState_3 sn ua rr lr fa rta smp eo ("/tmp/", (10 * 10^6), (1 * 10^6), (1 * 10^6))

-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState_4 = CoreState_4
    { _coreSiteName_4       :: Maybe Text
    , _coreUACCT_4          :: Maybe UACCT  -- ^ Google Account UAACT
    , _coreRootRedirect_4   :: Maybe Text
    , _coreLoginRedirect_4  :: Maybe Text
    , _coreBackToSiteRedirect_4 :: Text -- ^ where 'Back To <sitename>' link in settings panel should go
    , _coreFromAddress_4    :: Maybe SimpleAddress
    , _coreReplyToAddress_4 :: Maybe SimpleAddress
    , _coreSendmailPath_4   :: Maybe FilePath
    , _coreEnableOpenId_4   :: Bool -- ^ allow OpenId authentication
    , _coreBodyPolicy_4     :: (FilePath, Int64, Int64, Int64) -- ^ (temp directory for uploads, maxDisk, maxRAM, maxHeader)
    }
    deriving (Eq, Data, Typeable, Show)

instance Migrate CoreState_4 where
    type MigrateFrom CoreState_4 = CoreState_3
    migrate (CoreState_3 sn ua rr lr fa rta smp eo bp) = CoreState_4 sn ua rr lr (Text.pack "/") fa rta smp eo bp

$(deriveSafeCopy 4 'extension ''CoreState_4)
makeLenses ''CoreState_4

-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState = CoreState
    { _coreSiteName       :: Maybe Text
    , _coreUACCT          :: Maybe UACCT  -- ^ Google Account UAACT
    , _coreRootRedirect   :: Maybe Text
    , _coreLoginRedirect  :: Maybe Text
    , _coreSignupRedirect  :: Maybe Text -- ^ were to redirect to after a new account is created
    , _coreBackToSiteRedirect :: Text -- ^ where 'Back To <sitename>' link in settings panel should go
    , _coreFromAddress    :: Maybe SimpleAddress
    , _coreReplyToAddress :: Maybe SimpleAddress
    , _coreSendmailPath   :: Maybe FilePath
    , _coreEnableOpenId   :: Bool -- ^ allow OpenId authentication
    , _coreBodyPolicy     :: (FilePath, Int64, Int64, Int64) -- ^ (temp directory for uploads, maxDisk, maxRAM, maxHeader)
    }
    deriving (Eq, Data, Typeable, Show)

instance Migrate CoreState where
    type MigrateFrom CoreState = CoreState_4
    migrate (CoreState_4 sn ua rr lr bts fa rta smp eo bp) = CoreState sn ua rr lr lr bts fa rta smp eo bp

$(deriveSafeCopy 5 'extension ''CoreState)
makeLenses ''CoreState


initialCoreState :: CoreState
initialCoreState = CoreState
    { _coreSiteName       = Nothing
    , _coreUACCT          = Nothing
    , _coreRootRedirect   = Nothing
    , _coreLoginRedirect  = Nothing
    , _coreSignupRedirect = Nothing
    , _coreBackToSiteRedirect = (Text.pack "/")
    , _coreFromAddress    = Nothing
    , _coreReplyToAddress = Nothing
    , _coreSendmailPath   = Nothing
    , _coreEnableOpenId   = True
    , _coreBodyPolicy     = ("/tmp/", (10 * 10^6), (1 * 10^6), (1 * 10^6))
    }

-- | get the site name
getSiteName :: Query CoreState (Maybe Text)
getSiteName = view coreSiteName

-- | set the site name
setSiteName :: Maybe Text -> Update CoreState ()
setSiteName name = coreSiteName .= name

-- | get the 'UACCT' for Google Analytics
getUACCT :: Query CoreState (Maybe UACCT)
getUACCT = view coreUACCT

-- | set the 'UACCT' for Google Analytics
setUACCT :: Maybe UACCT -> Update CoreState ()
setUACCT mua = coreUACCT .= mua

-- | get the path that @/@ should redirect to
getRootRedirect :: Query CoreState (Maybe Text)
getRootRedirect = view coreRootRedirect

-- | get the path that 'Back To Site' should go to
getBackToSiteRedirect :: Query CoreState Text
getBackToSiteRedirect = view coreBackToSiteRedirect

-- | set the path that 'Back To Site' should go to
setBackToSiteRedirect :: Text -> Update CoreState ()
setBackToSiteRedirect path = coreBackToSiteRedirect .= path

-- | set the path that @/@ should redirect to
setRootRedirect :: Maybe Text -> Update CoreState ()
setRootRedirect path = coreRootRedirect .= path

-- | get the 'BodyPolicy' data for requests which can have bodies
getBodyPolicy :: Query CoreState (FilePath, Int64, Int64, Int64)
getBodyPolicy = view coreBodyPolicy

-- | set the 'BodyPolicy' data for requests which can have bodies
setBodyPolicy :: (FilePath, Int64, Int64, Int64) -> Update CoreState ()
setBodyPolicy bp = coreBodyPolicy .= bp

-- | get the path that we should redirect to after login
getLoginRedirect :: Query CoreState (Maybe Text)
getLoginRedirect = view coreLoginRedirect

-- | set the path that we should redirect to after login
setLoginRedirect :: Maybe Text -> Update CoreState ()
setLoginRedirect path = coreLoginRedirect .= path

-- | get the path that we should redirect to after signup
getSignupRedirect :: Query CoreState (Maybe Text)
getSignupRedirect = view coreSignupRedirect

-- | set the path that we should redirect to after signup
setSignupRedirect :: Maybe Text -> Update CoreState ()
setSignupRedirect path = coreSignupRedirect .= path

-- | get the From: address for system emails
getFromAddress :: Query CoreState (Maybe SimpleAddress)
getFromAddress = view coreFromAddress

-- | get the From: address for system emails
setFromAddress :: Maybe SimpleAddress -> Update CoreState ()
setFromAddress addr = coreFromAddress .= addr

-- | get the Reply-To: address for system emails
getReplyToAddress :: Query CoreState (Maybe SimpleAddress)
getReplyToAddress = view coreReplyToAddress

-- | get the Reply-To: address for system emails
setReplyToAddress :: Maybe SimpleAddress -> Update CoreState ()
setReplyToAddress addr = coreReplyToAddress .= addr

-- | get the path to the sendmail executable
getSendmailPath :: Query CoreState (Maybe FilePath)
getSendmailPath = view coreSendmailPath

-- | set the path to the sendmail executable
setSendmailPath :: Maybe FilePath -> Update CoreState ()
setSendmailPath path = coreSendmailPath .= path

-- | get the status of enabling OpenId
getEnableOpenId :: Query CoreState Bool
getEnableOpenId = view coreEnableOpenId

-- | set the status of enabling OpenId
setEnableOpenId :: Bool -> Update CoreState ()
setEnableOpenId b = coreEnableOpenId .= b

-- | get the entire 'CoreState'
getCoreState :: Query CoreState CoreState
getCoreState = ask

-- | set the entire 'CoreState'
setCoreState :: CoreState -> Update CoreState ()
setCoreState = put

$(makeAcidic ''CoreState
  [ 'getUACCT
  , 'setUACCT
  , 'getRootRedirect
  , 'setRootRedirect
  , 'getBackToSiteRedirect
  , 'setBackToSiteRedirect
  , 'getLoginRedirect
  , 'setLoginRedirect
  , 'getSignupRedirect
  , 'setSignupRedirect
  , 'getBodyPolicy
  , 'setBodyPolicy
  , 'getSiteName
  , 'setSiteName
  , 'getFromAddress
  , 'setFromAddress
  , 'getReplyToAddress
  , 'setReplyToAddress
  , 'getSendmailPath
  , 'setSendmailPath
  , 'setEnableOpenId
  , 'getEnableOpenId
  , 'getCoreState
  , 'setCoreState
  ])

data Acid = Acid
    { -- acidAuthenticate :: AcidState AuthenticateState
      acidProfileData  :: AcidState ProfileDataState
    , acidCore         :: AcidState CoreState
    , acidNavBar       :: AcidState NavBarState
    , acidRebac        :: AcidState RelationState
    }

class GetAcidState m st where
    getAcidState :: m (AcidState st)

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    -- open acid-state databases
    bracket (openLocalStateFrom (basePath </> "core")        initialCoreState)        (createArchiveCheckpointAndClose) $ \core ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createArchiveCheckpointAndClose) $ \profileData ->
    bracket (openLocalStateFrom (basePath </> "navBar")      initialNavBarState)      (createArchiveCheckpointAndClose) $ \navBar ->
    bracket (openMemoryState (mkRelationState clckwrksSchema clckwrksRels)) (const $ pure ()) $ \rebac ->
    -- create sockets to allow `clckwrks-cli` to talk to the databases
#if MIN_VERSION_acid_state (0,16,0)
    bracket (forkIO (tryRemoveFile (basePath </> "core_socket") >> acidServerSockAddr skipAuthenticationCheck (SockAddrUnix $ basePath </> "core_socket") profileData))
            (\tid -> killThread tid >> tryRemoveFile (basePath </> "core_socket")) $ const $

#else
    bracket (forkIO (tryRemoveFile (basePath </> "core_socket") >> acidServer skipAuthenticationCheck (UnixSocket $ basePath </> "core_socket") profileData))
            (\tid -> killThread tid >> tryRemoveFile (basePath </> "core_socket")) $ const $
#endif
#if MIN_VERSION_acid_state (0,16,0)
    bracket (forkIO (tryRemoveFile (basePath </> "profileData_socket") >> acidServerSockAddr skipAuthenticationCheck (SockAddrUnix $ basePath </> "profileData_socket") profileData))
            (\tid -> killThread tid >> tryRemoveFile (basePath </> "profileData_socket"))
#else
    bracket (forkIO (tryRemoveFile (basePath </> "profileData_socket") >> acidServer skipAuthenticationCheck (UnixSocket $ basePath </> "profileData_socket") profileData))
            (\tid -> killThread tid >> tryRemoveFile (basePath </> "profileData_socket"))
#endif
            (const $ f (Acid profileData core navBar rebac))

    where
      tryRemoveFile fp = removeFile fp `catch` (\e -> if isDoesNotExistError e then return () else throw e)
      createArchiveCheckpointAndClose acid =
          do createArchive acid
             createCheckpointAndClose acid

-- * Initial REBAC schema and relations for clckwrks
--
-- It should perhaps be possible to pull in the initial schema in from
-- a static file? Or perhaps always get it from the file and never
-- from the database so that the schema updates can be tracked in a
-- revision control system?

clckwrksSchema =
  [schema|
         definition user {}


         definition platform {
           relation administrator: user

           permission super_admin = administrator
         }

         /* controls access to the REBAC API

            For example, can a user query the underlying schema and relations database.
         */
         definition rebac_api {
           relation controller: platform

           permission get    = controller->super_admin
           permission modify = controller->super_admin
         }

         /*
         controls access to various pages in the REBAC admin panel.

         While the rebac_api might prevent the user from seeing any
         data in the database should they visit the schema or relations page,
         we can create a better user experience if we don't even let them see
         the page.
         */
         definition rebac_url {
           relation controller: platform

           permission rebac_view = controller->super_admin
         }

         definition usergroup {
           relation direct_member: user | usergroup#member

           permission membership = direct_member
         }

         /* schema for clckwrks-plugin-page
         */
         definition page {
           relation admin: user
           relation viewer: user | usergroup#member

           permission view = viewer + admin
           permission edit = admin
         }

 |]
-- #         page:1#admin@user:1
--          usergroup:subscribers#direct_member@user:1
clckwrksRels =
  [rels|
         platform:clckwrks#administrator@user:1
         page:admin#admin@user:1
         page:1#viewer@usergroup:subscribers#membership
         page:1#admin@user:1
         usergroup:subscribers#direct_member@user:1
         rebac_api:schema#controller@platform:clckwrks
         rebac_api:relations#controller@platform:clckwrks
         rebac_url:schema_panel#controller@platform:clckwrks
         rebac_url:relations_panel#controller@platform:clckwrks
  |]


