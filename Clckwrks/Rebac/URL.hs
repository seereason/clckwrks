{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Rebac.URL
       ( RebacURL(..)
       )
       where

-- import Clckwrks.AccessControl      () -- ToObject UserId
import AccessControl.Relation      (KnownPermission(..), ToObject(..), Object(..), ObjectId(..))
import AccessControl.Schema        (ObjectType(..))
import Clckwrks.Rebac.Types        (RebacPermission(..))
import Clckwrks.Types              ()
import Data.Data                   (Data, Typeable)
import Data.UserId                 (UserId(..))
import GHC.Generics                (Generic)
import Web.Routes.TH               (derivePathInfo)

data RebacURL
  = SchemaPanel
  | RelationsPanel
  deriving (Eq, Ord, Data, Typeable, Generic, Read, Show)

derivePathInfo ''RebacURL

instance ToObject RebacURL where
  toObject u =
    let objId = ObjectId $ case u of
          SchemaPanel    -> "schema_panel"
          RelationsPanel -> "relations_panel"
    in Object (ObjectType "rebac_url") objId

instance KnownPermission RebacURL RebacPermission UserId

