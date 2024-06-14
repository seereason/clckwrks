{-# LANGUAGE CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RankNTypes, RecordWildCards, ScopedTypeVariables, UndecidableInstances, OverloadedStrings, TemplateHaskell #-}
module Clckwrks.AccessControl where

import AccessControl.Acid            (Check(..))
import AccessControl.Check           (RelationState(..), Access(..), RelationState(..))
import AccessControl.Schema          (Permission(..), Relation(..), ToPermission(..), ToRelation(..), ObjectType(..))
import AccessControl.Relation        (KnownPermission, ToObject(..), Object(..), ObjectId(..))
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

checkAccess :: (KnownPermission resource permission UserId, Happstack m, MonadIO m) => resource -> permission -> ClckT url m Access
checkAccess res perm =
  do mu <- getUserId
     case mu of
       Nothing ->
             query (Check (toObject res) (toPermission perm) (Object (ObjectType "anonymous") (ObjectId "anonymous")))
       (Just uid) ->
             query (Check (toObject res) (toPermission perm) (toObject uid))


assertAccess ::(KnownPermission resource permission UserId, Happstack m, MonadIO m) => resource -> permission -> ClckT url m ()
assertAccess res perm =
  do a <- checkAccess res perm
     case a of
       Allowed -> pure ()
       NotAllowed reasons ->
         do rq <- askRq
            escape $ do setRedirectCookie (rqUri rq ++ rqQuery rq)
                        unauthorizedPage  ("You do not have permission to access this resource. " <> (TL.pack $ show reasons) :: TL.Text)

