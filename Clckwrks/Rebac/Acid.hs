{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.Rebac.Acid where

import AccessControl.Relation      (ObjectType(..), Relation(..), RelationTuple(..), Tag(..), hasTag, object, rels)
import AccessControl.Schema        (Schema, schema)
import Control.Monad.Reader        (ask)
import Control.Monad.State         (put, get)
import Data.Acid                   (Query, Update, makeAcidic)
import Data.Data                   (Data)
import Data.List                   (partition, union)
import Data.SafeCopy               (SafeCopy(..), base, contain, safeGet, safePut)
import Data.Time.Clock             (UTCTime)
import Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import Data.Text                   (Text)
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), ($$), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

newtype RelationTxId = RelationTxId { unRelationTxId :: Integer }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance SafeCopy RelationTxId where version = 1 ; kind = base

succRelationIxId :: RelationTxId -> RelationTxId
succRelationIxId (RelationTxId rid) = (RelationTxId (succ rid))

ppRelationTxId :: RelationTxId -> Doc
ppRelationTxId (RelationTxId n) = PP.text "RelationTxId" <+> PP.text (show n)

data RLEAction
  = RLEAdd
  | RLERemove
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance SafeCopy RLEAction where version = 1 ; kind = base

{-
It may seem like the Timestamp and Transaction Id are nearly duplicates. But fast transactions could result in identical time stamps and there are sitautions where the clock could appear to jump back in time.

-}
data RelationLogEntry = RelationLogEntry
  { rleTimestamp     :: UTCTime
  , rleRelationTuple :: RelationTuple
  , rleAction        :: RLEAction
  , rleComment       :: Text
  , rleTxId          :: RelationTxId
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance SafeCopy RelationLogEntry where version = 1 ; kind = base

{-

Tuple logging:

 - tuples can be added and removed
 - does it make sense for a tuple to be modified? A user might originally only have read access to a resource but then be upgrade to read+write. Or perhaps the way an object id is encoded changes and there is a migration?
 - with only add/remove the history chain will be boring. A tuple is added, possibly removed, and perhaps added again/removed more times.
 - let's assume that modifications are going to be important since they have better potential to retain a history

 - the owner of an object (such as a document) wants to be able to view the history all the permissions for that document. Even if some subject can no longer access the object, knowing they could for a while could be important information.

 - one view we might want to see is a purely chronologic view of all relation tuples
 - one view we want to see is the chronologic history of a specific resource
 - one view we want to see is the chonological history of a specific resource and subject

 - most of the time we only want to access the currently active relation tuples

Should a relation tuple have unique id?
Should each operation have a unique, sequential id?

The current rebac backend wants things in a nice simple form -- would it actually be slower if it just looked directly at a the log?

The most common operation -- `check` starts by looking up a Resource.

If we store the log and the current state separately, what happens if they get out of sync?

We don't want to replay the whole log at startup -- but maybe we don't need to? The head of each resource will be the current state -- so we just slurp out that info.

storing the log separately could mean that it could be stored on disk or other slower storage media if it grows too large without affect the speed of the check operation.

It is not clear that modifying a relation is a sensible operation.

Let's stick with read/write/delete.

-}

data RebacState = RebacState
  { rsTuples :: [ RelationTuple ]    -- currently important tuples
  , rsLog    :: [ RelationLogEntry ] -- how did we get here
  , rsTxId   :: RelationTxId         -- next unassigned TxId
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance SafeCopy RebacState where version = 1 ; kind = base

-- | add a 'RelationTuple' to the database
--
-- If the 'RelationTuple' already exists in the 'rsTuples' the 'rsTuple' cache will not be modified, but the
-- 'RelationTuple' will still be added to the 'rsLog'
--
-- FIXME: add a precondition check. If the precondition fails, no modifications are performed.
addRelationTuple :: RelationTuple -- ^ 'RelationTuple' to add
                 -> UTCTime       -- ^ approximate wall time this relation tuple was added
                 -> Text          -- ^ a human readable comment explaining how/why this relation got added
                 -> Update RebacState RelationLogEntry
addRelationTuple rt now comment =
  do rs <- get
     let rle = (RelationLogEntry { rleTimestamp     = now
                                 , rleRelationTuple = rt
                                 , rleAction        = RLEAdd
                                 , rleComment       = comment
                                 , rleTxId          = rsTxId rs
                                 })
     put $ rs { rsTuples = if rt `elem` (rsTuples rs) then (rsTuples rs) else (rt : (rsTuples rs))  -- don't insert into rsTuples cache if it already exists
              , rsLog = rle : (rsLog rs)
              , rsTxId = succRelationIxId (rsTxId rs)
              }
     pure rle



addRelationTuples :: [RelationTuple] -- ^ 'RelationTuple's to add
                  -> UTCTime       -- ^ approximate wall time this relation tuple was added
                  -> Text          -- ^ a human readable comment explaining how/why this relation got added
                  -> Update RebacState [RelationLogEntry]
addRelationTuples [] now comment = pure []
addRelationTuples rts now comment =
  do rs <- get
     let rles = [ RelationLogEntry { rleTimestamp     = now
                                   , rleRelationTuple = rt
                                   , rleAction        = RLEAdd
                                   , rleComment       = comment
                                   , rleTxId          = rsTxId rs
                                   } | rt <- rts ]
     put $ rs { rsTuples = union rts (rsTuples rs)
              , rsLog = rles ++ (rsLog rs)
              , rsTxId = succRelationIxId (rsTxId rs)
              }
     pure rles

-- | remove a 'RelationTuple' from the database
--
-- This only removes tuples that are an exact match.
--
-- If the 'RelationTuple' does not exist in the 'rsTuples' the 'rsTuple' cache will not be modified and no error will be returned.
-- But the 'RelationTuple' removal will still be added to the 'rsLog'.
--
removeRelationTuple :: RelationTuple -- ^ 'RelationTuple' to remove
                    -> UTCTime       -- ^ approximate wall time this relation tuple was removed
                    -> Text          -- ^ a human readable comment explaining how/why this relation got added
                    -> Update RebacState RelationLogEntry
removeRelationTuple rt now comment =
  do rs <- get
     let rle = (RelationLogEntry { rleTimestamp     = now
                                 , rleRelationTuple = rt
                                 , rleAction        = RLERemove
                                 , rleComment       = comment
                                 , rleTxId          = rsTxId rs
                                 })
     put $ rs { rsTuples = filter ((/=) rt) (rsTuples rs)
              , rsLog    = rle : (rsLog rs)
              , rsTxId   = succRelationIxId (rsTxId rs)
              }
     pure rle

removeRelationTuplesByTag :: Tag      -- ^ remove *any* tuple with this tag
                          -> UTCTime  -- ^ approximate wall time this relation tuple was removed
                          -> Text     -- ^ a human readable comment explaining how/why these relations got removed
                          -> Update RebacState [RelationLogEntry]
removeRelationTuplesByTag tag now comment =
  do rs <- get
     let (remove, keep) = partition (hasTag tag) (rsTuples rs)
     let rles = [ RelationLogEntry { rleTimestamp     = now
                                   , rleRelationTuple = rt
                                   , rleAction        = RLERemove
                                   , rleComment       = comment
                                   , rleTxId          = rsTxId rs
                                   } | rt <- remove ]
     put $ rs { rsTuples = keep
              , rsLog    = rles ++ (rsLog rs)
              , rsTxId   = succRelationIxId (rsTxId rs)
              }
     pure rles

-- | this provides an atomic way to remove all the tuples associated with a 'tag' and insert new tuples for that 'tag'.
--
-- NOTE: it is up to the caller to ensure that the new tuples have the same tag
replaceRelationTuplesByTag :: Tag -- ^ remove *any* tuples with this tag
                           -> [RelationTuple] -- ^ add these new tuples. They *should* have the same tag, but that is not enforced
                           -> UTCTime -- ^ approximate wall time this operation was performed
                           -> Text -- ^ humman readable comment on why this happened
                           -> Update RebacState [RelationLogEntry]
replaceRelationTuplesByTag tag rts now comment =
  do removedRLEs <- removeRelationTuplesByTag tag now comment
     addedRLEs   <- addRelationTuples rts now comment
     pure (removedRLEs ++  addedRLEs)

-- | get all the 'RelationTuple' from the database
getRelationTuples :: Query RebacState [ RelationTuple ]
getRelationTuples =
  do rs <- ask
     pure $ rsTuples rs

-- | get all the 'RelationTupleEntry' from the database
getRelationLog :: Query RebacState [ RelationLogEntry ]
getRelationLog =
  do rs <- ask
     pure $ rsLog rs

makeAcidic ''RebacState
 [ 'addRelationTuple
 , 'addRelationTuples
 , 'removeRelationTuple
 , 'removeRelationTuplesByTag
 , 'replaceRelationTuplesByTag
 , 'getRelationTuples
 , 'getRelationLog
 ]



-- * Initial REBAC schema and relations for clckwrks
--
-- It should perhaps be possible to pull in the initial schema in from
-- a static file? Or perhaps always get it from the file and never
-- from the database so that the schema updates can be tracked in a
-- revision control system?

clckwrksSchema :: Schema
clckwrksSchema =
  [schema|
         definition user {}

         definition platform {
           relation administrator: user

           permission super_admin = administrator
         }

         /* the core clckwrks plugins */
         definition clck {
           relation controller: platform
           permission admin = controller->super_admin
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

         /* schema for clckwrks-plugin-page */
         definition page {
           relation admin: user
           relation viewer: user | usergroup#member

           permission view = viewer + admin
           permission edit = admin
         }

         /* schema for clckwrk-plugin-stripe */
         definition stripe {
         }

 |]

    {-
         page:1#viewer@usergroup:subscribers#membership
         page:1#admin@user:1
         page:1#viewer@user:2
         usergroup:subscribers#direct_member@user:1
      -}

clckwrksRels :: [ RelationTuple ]
clckwrksRels =
  [rels|
         platform:clckwrks#administrator@user:1                      # security warning - the first account created automatically gets admin access
         clck:admin#controller@platform:clckwrks                     # platform admins are also clck admins
         page:admin#controller@platform:clckwrks                     # platform admins are also page admins
         page:1#viewer@user:*
         rebac_api:schema#controller@platform:clckwrks
         rebac_api:relations#controller@platform:clckwrks
         rebac_api:relation_log#controller@platform:clckwrks
         rebac_url:schema_panel#controller@platform:clckwrks
         rebac_url:relations_panel#controller@platform:clckwrks
         rebac_url:relation_log_panel#controller@platform:clckwrks
  |]


-- | initialRebacState
--
-- we want to populate the relation log to include the entries that
-- were originally added by `initialRebacState`. Those entries will
-- have a timestamp of epoch time.
initialRebacState :: RebacState
initialRebacState = RebacState
  { rsTuples = clckwrksRels
  , rsLog    = map fakeLog clckwrksRels
  , rsTxId   = RelationTxId 1
  }
  where
    fakeLog :: RelationTuple -> RelationLogEntry
    fakeLog rt = RelationLogEntry (posixSecondsToUTCTime 0) rt RLEAdd "initial rebac state" (RelationTxId 0)
