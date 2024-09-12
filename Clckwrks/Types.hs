{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Clckwrks.Types
    ( UUID
    , Prefix(..)
    , Trust(..)
    , NamedLink(..)
    ) where

import AccessControl.Schema          (Permission(..), Relation(..), ToPermission(..), ToRelation(..), ObjectType(..))
import AccessControl.Relation        (KnownPermission, ToObject(..), Object(..), ObjectId(..))
import Control.Applicative ((<$>))
import Data.Aeson    (ToJSON(..), (.=), object)
import Data.Data     (Data, Typeable)
import Data.SafeCopy (SafeCopy(..), base, deriveSafeCopy, safeGet, safePut, contain)
import qualified Data.Text as T
import Data.Text     (Text)
import qualified Data.Text.Encoding as T
import Data.UUID.Types (UUID)
import Data.UUID.Orphans ()
import Data.UserId (UserId(..))
import HSP.Google.Analytics (UACCT)

instance ToObject UserId where
  toObject (UserId n) = Object (ObjectType "user") (ObjectId $ T.pack $ show n)

instance ToObject (Maybe UserId) where
  toObject (Just (UserId n)) = Object (ObjectType "user") (ObjectId $ T.pack $ show n)
  toObject Nothing           = Object (ObjectType "user") (ObjectId $ "anonymous")

-- | 'SafeCopy' instances for some 3rd party types
$(deriveSafeCopy 0 'base ''UACCT)

-- | at present this is only used by the menu editor
newtype Prefix = Prefix { prefixText :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance SafeCopy Prefix where
    kind = base
    getCopy = contain $ (Prefix . T.decodeUtf8) <$> safeGet
    putCopy = contain . safePut . T.encodeUtf8 . prefixText
    errorTypeName _ = "Prefix"

data Trust
    = Trusted   -- ^ used when the author can be trusted     (sanitization is not performed)
    | Untrusted -- ^ used when the author can not be trusted (sanitization is performed)
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Trust)

data NamedLink = NamedLink
    { namedLinkTitle :: Text
    , namedLinkURL   :: Text
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''NamedLink)

instance ToJSON NamedLink where
    toJSON (NamedLink{..}) =
        object [ "navBarItemName" .= namedLinkTitle
               , "navBarItemLink" .= namedLinkURL
               ]

