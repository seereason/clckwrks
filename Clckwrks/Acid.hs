{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Acid where

import Clckwrks.NavBar.Acid          (NavBarState       , initialNavBarState)
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
import Data.Acid.Remote            (acidServer, skipAuthenticationCheck)
import Data.Data                   (Data, Typeable)
import Data.Maybe                  (fromMaybe)
import Data.SafeCopy               (Migrate(..), base, deriveSafeCopy, extension)
import Data.Text                   (Text)
import Happstack.Authenticate.Core          (AuthenticateState, SimpleAddress(..))
import Happstack.Authenticate.Password.Core (PasswordState)
import Network                     (PortID(UnixSocket))
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
$(deriveSafeCopy 1 'extension ''CoreState_1)

instance Migrate CoreState_1 where
    type MigrateFrom CoreState_1 = CoreState_v0
    migrate (CoreState_v0 ua rr) = CoreState_1 Nothing ua rr Nothing

-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState = CoreState
    { _coreSiteName       :: Maybe Text
    , _coreUACCT          :: Maybe UACCT  -- ^ Google Account UAACT
    , _coreRootRedirect   :: Maybe Text
    , _coreLoginRedirect  :: Maybe Text
    , _coreFromAddress    :: Maybe SimpleAddress
    , _coreReplyToAddress :: Maybe SimpleAddress
    , _coreSendmailPath   :: Maybe FilePath
    }
    deriving (Eq, Data, Typeable, Show)
$(deriveSafeCopy 2 'extension ''CoreState)

makeLenses ''CoreState
instance Migrate CoreState where
    type MigrateFrom CoreState = CoreState_1
    migrate (CoreState_1 sn ua rr lr) = CoreState sn ua rr lr Nothing Nothing Nothing

initialCoreState :: CoreState
initialCoreState = CoreState
    { _coreSiteName       = Nothing
    , _coreUACCT          = Nothing
    , _coreRootRedirect   = Nothing
    , _coreLoginRedirect  = Nothing
    , _coreFromAddress    = Nothing
    , _coreReplyToAddress = Nothing
    , _coreSendmailPath   = Nothing
    }

-- | get the 'UACCT' for Google Analytics
getUACCT :: Query CoreState (Maybe UACCT)
getUACCT = view coreUACCT

-- | set the 'UACCT' for Google Analytics
setUACCT :: Maybe UACCT -> Update CoreState ()
setUACCT mua = coreUACCT .= mua

-- | get the path that @/@ should redirect to
getRootRedirect :: Query CoreState (Maybe Text)
getRootRedirect = view coreRootRedirect

-- | set the path that @/@ should redirect to
setRootRedirect :: Maybe Text -> Update CoreState ()
setRootRedirect path = coreRootRedirect .= path

-- | get the path that we should redirect to after login
getLoginRedirect :: Query CoreState (Maybe Text)
getLoginRedirect = view coreLoginRedirect

-- | set the path that we should redirect to after login
setLoginRedirect :: Maybe Text -> Update CoreState ()
setLoginRedirect path = coreLoginRedirect .= path

-- | get the site name
getSiteName :: Query CoreState (Maybe Text)
getSiteName = view coreSiteName

-- | set the site name
setSiteName :: Maybe Text -> Update CoreState ()
setSiteName name = coreSiteName .= name

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
  , 'getLoginRedirect
  , 'setLoginRedirect
  , 'getSiteName
  , 'setSiteName
  , 'getCoreState
  , 'setCoreState
  ])

data Acid = Acid
    { -- acidAuthenticate :: AcidState AuthenticateState
      acidProfileData  :: AcidState ProfileDataState
    , acidCore         :: AcidState CoreState
    , acidNavBar       :: AcidState NavBarState
    }

class GetAcidState m st where
    getAcidState :: m (AcidState st)

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createArchiveCheckpointAndClose) $ \profileData ->
    bracket (openLocalStateFrom (basePath </> "core")        initialCoreState)        (createArchiveCheckpointAndClose) $ \core ->
    bracket (openLocalStateFrom (basePath </> "navBar")      initialNavBarState)      (createArchiveCheckpointAndClose) $ \navBar ->
    bracket (forkIO (tryRemoveFile (basePath </> "profileData_socket") >> acidServer skipAuthenticationCheck (UnixSocket $ basePath </> "profileData_socket") profileData))
            (\tid -> killThread tid >> tryRemoveFile (basePath </> "profileData_socket"))
            (const $ f (Acid profileData core navBar))
    where
      tryRemoveFile fp = removeFile fp `catch` (\e -> if isDoesNotExistError e then return () else throw e)
      createArchiveCheckpointAndClose acid =
          do createArchive acid
             createCheckpointAndClose acid

