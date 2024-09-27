{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.Rebac.Types where

import AccessControl.Check    (RelationState(..), Access(..), RelationState(..), )
import AccessControl.Schema   (KnownPermission, Permission(..), ToPermission(..))
import AccessControl.Relation (ToObject(..), Object(..), ObjectId(..), ObjectType(..),Relation(..), ToRelation(..) )
import Data.Data              (Data, Typeable)
import GHC.Generics           (Generic)

data RebacRelation
  = RebacAdmin
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToRelation RebacRelation where
  toRelation r =
    Relation $ case r of
                 RebacAdmin -> "admin"

data RebacPermission
  = RebacView
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToPermission RebacPermission where
  toPermission p =
    Permission $ case p of
                   RebacView -> "rebac_view"

