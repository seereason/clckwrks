{-# LANGUAGE CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RankNTypes, RecordWildCards, ScopedTypeVariables, UndecidableInstances, OverloadedStrings, TemplateHaskell #-}
module Clckwrks.AccessControl where

import AccessControl.Acid            (Check(..))
import AccessControl.Check           (RelationState(..), Access(..), RelationState(..))
import AccessControl.Schema          (Permission(..), Relation(..), ToPermission(..), ToRelation(..), ObjectType(..), ppPermission)
import AccessControl.Relation        (KnownPermission, ToObject(..), Object(..), ObjectId(..), ppObject)
import Clckwrks.Authenticate.Plugin  (getUserId)
import Clckwrks.Monad
import Control.Monad.Trans           (MonadIO(..))
import Clckwrks.Types
import Clckwrks.Unauthorized         (unauthorizedPage)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy      as TL
import Data.UserId                   (UserId(..))
import Happstack.Server              (Happstack, askRq, escape, rqUri, rqQuery)

instance KnownPermission Object Permission UserId
instance KnownPermission Object Permission (Maybe UserId)

-- | find out if the current user has permession to access a resource
checkAccess :: (KnownPermission resource permission (Maybe UserId), Happstack m, MonadIO m) => resource -> permission -> ClckT url m Access
checkAccess res perm =
  do mu <- getUserId
     query (Check (toObject res) (toPermission perm) (toObject mu))
{-
     case mu of
       Nothing ->
             query (Check (toObject res) (toPermission perm) (Object (ObjectType "anonymous") (ObjectId "anonymous")))
       (Just uid) ->
-}

-- | assert that a user has permission to access a resource. If this assertion is wrong, show an 'unauthorized access' page
assertAccess ::(KnownPermission resource permission (Maybe UserId), Happstack m, MonadIO m) => resource -> permission -> ClckT url m ()
assertAccess res perm =
  do a <- checkAccess res perm
     case a of
       Allowed -> pure ()
       NotAllowed reasons ->
         do rq <- askRq
            mu <- getUserId
            escape $ do setRedirectCookie (rqUri rq ++ rqQuery rq)
                        unauthorizedPage  ("You do not have permission to access this resource. " <>
                                           "resource = " <> (TL.pack (show $ ppObject (toObject res))) <>
                                           ", permission = " <> (TL.pack (show $ ppPermission (toPermission perm))) <>
                                           ", subject = " <> (TL.pack (show $ ppObject (toObject mu))) <>
                                           ", reasons = " <> (TL.pack $ show reasons) :: TL.Text)
