{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}
module Clckwrks.Rebac.API
       ( getRelationTuples
       , getSchema
--       , putSchema
       , addRelationTuple
--       , removeRelationTuple
       , RebacApi(..)
       ) where

import AccessControl.Acid            (AddRelationTuple(..), Check(..), PutSchema(..), GetSchema(..), GetRelationTuples(..))
import AccessControl.Check           (RelationState(..), Access(..), RelationState(..))
import AccessControl.Schema          (Permission(..), Relation(..), Schema(..), ToPermission(..), ToRelation(..), ObjectType(..))
import AccessControl.Relation        (KnownPermission, RelationTuple(..), ToObject(..), Object(..), ObjectId(..))

import Clckwrks.AccessControl       (checkAccess)
import Clckwrks.Authenticate.Plugin (authenticatePlugin, authenticatePluginLoader)
import Clckwrks.Authenticate.Monad  (AuthenticatePluginState(..))
import Clckwrks.Monad               (Clck, ClckPlugins, plugins, query, update)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TVar  (modifyTVar')
import Control.Monad                (join)
import Control.Monad.State          (get)
import Control.Monad.Trans          (liftIO)
-- import Data.Acid as Acid            (AcidState, query, update)
import Data.Data                    (Data)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import Data.Maybe                   (maybe)
import Data.Monoid                  (mempty)
import Data.Text                    (Text)
import qualified Data.Text          as Text
import Data.Typeable                (Typeable)
import Data.UserId                  (UserId)
import GHC.Generics                 (Generic)
import Web.Plugins.Core             (Plugin(..), When(Always), addCleanup, addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, getPluginsSt, initPlugin, modifyPluginState')

data RebacApi
  = SchemaR
  | RelationsR
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToObject RebacApi where
  toObject SchemaR =
    Object (ObjectType "rebac_api") (ObjectId "schema")
  toObject RelationsR =
    Object (ObjectType "rebac_api") (ObjectId "relations")

instance KnownPermission RebacApi Permission UserId

getSchema :: Clck url (Either Text Schema)
getSchema =
  do a <- checkAccess SchemaR (Permission "get")
     case a of
       Allowed ->
         do s <- query GetSchema
            pure $ Right $ s
       NotAllowed reasons ->
         pure $ Left $ Text.pack $ show reasons

getRelationTuples :: Clck url (Either Text [ RelationTuple ])
getRelationTuples =
  do a <- checkAccess RelationsR (Permission "get")
     case a of
       Allowed ->
         do ts <- query GetRelationTuples
            pure $ Right $ ts
       NotAllowed reasons ->
         pure $ Left $ Text.pack $ show reasons

addRelationTuple :: RelationTuple -> Clck url (Either Text ())
addRelationTuple rt =
  do a <- checkAccess RelationsR (Permission "modify")
     case a of
       Allowed ->
         do update (AddRelationTuple rt)
            pure (Right ())
       NotAllowed reasons ->
         pure $ Left $ Text.pack $ show reasons

{-

getUser :: UserId -> Clck url (Maybe User)
getUser uid =
  do p <- plugins <$> get
     ~(Just aps) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ Acid.query (acidStateAuthenticate aps) (GetUserByUserId uid)

-- | Update an existing 'User'. Must already have a valid 'UserId'.
--
-- no security checks are performed to ensure that the caller is
-- authorized to change data for the 'User'.
insecureUpdateUser :: User -> Clck url ()
insecureUpdateUser user =
  do p <- plugins <$> get
     ~(Just aps) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ Acid.update (acidStateAuthenticate aps) (UpdateUser user)

getUsername :: UserId -> Clck url (Maybe Username)
getUsername uid =
  do mUser <- getUser uid
     pure $ _username <$> mUser

getEmail :: UserId -> Clck url (Maybe Email)
getEmail uid =
  do mUser <- getUser uid
     pure $ join $ _email <$> mUser

setCreateUserCallback :: ClckPlugins -> Maybe (User -> IO ()) -> IO ()
setCreateUserCallback p mcb =
  do ~(Just aps) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ atomically $ modifyTVar' (apsAuthenticateConfigTV aps) $ (\ac -> ac { _createUserCallback = mcb })
     pure ()

setSignupPluginURL :: ClckPlugins
                   -> Text
                   -> Text
                   -> IO ()
setSignupPluginURL plugins pn pu =
  do modifyPluginState' plugins (pluginName authenticatePlugin) $ \aps ->
       aps { apsSignupPluginURLs = Map.insert pn pu (apsSignupPluginURLs aps) }
     authenticatePluginLoader plugins
     pure ()

-}
