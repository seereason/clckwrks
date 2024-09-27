{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.Rebac.Acid where

import AccessControl.Relation      (ObjectType(..), Relation(..), RelationTuple(..), object, rels)
import AccessControl.Schema        (Schema, schema)
import Control.Monad.Reader        (ask)
import Control.Monad.State         (put, get)
import Data.Acid                   (Query, Update, makeAcidic)
import GHC.Generics                (Generic)

data RebacState = RebacState
  { rsTuples :: [ RelationTuple ] }
  deriving (Eq, Ord, Read, Show, Generic)


addRelationTuple :: RelationTuple -> Update RebacState ()
addRelationTuple rt =
  do rs <- get
     if rt `elem` (rsTuples rs)
       then pure ()
       else put $ rs { rsTuples = rt : (rsTuples rs) }

removeRelationTuple :: RelationTuple -> Update RebacState ()
removeRelationTuple rt =
  do rs <- get
     put $ rs { rsTuples = filter ((/=) rt) (rsTuples rs) }

getRelationTuples :: Query RebacState [ RelationTuple ]
getRelationTuples =
  do rs <- ask
     pure $ rsTuples rs

makeAcidic ''RebacState
 [ 'addRelationTuple
 , 'removeRelationTuple
 , 'getRelationTuples
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

         /* controls access to the REBAC API

            For example, can a user query the underlying schema and relations database.
         */
         definition rebac_api {
           relation controller: platform

           permission get    = controller->super_admin
           permission modify = controller->super_admin
         }

         definition clck {
           relation controller: platform
           permission admin = controller->super_admin
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

         /* schema for clckwrk-plugin-stripe
         */
         definition stripe {
         }

 |]
-- #         page:1#admin@user:1
--          usergroup:subscribers#direct_member@user:1
--         usergroup:price_0O8U5HMFVgr8nx2JswmPcAxw#direct_member@user:1

clckwrksRels :: [ RelationTuple ]
clckwrksRels =
  [rels|
         page:2#viewer@usergroup:price_0O8U5HMFVgr8nx2JswmPcAxw#membership
         platform:clckwrks#administrator@user:1
         clck:admin#controller@platform:clckwrks
         page:admin#admin@user:1
         page:1#viewer@usergroup:subscribers#membership
         page:1#admin@user:1
         page:1#viewer@user:2
         usergroup:subscribers#direct_member@user:1
         rebac_api:schema#controller@platform:clckwrks
         rebac_api:relations#controller@platform:clckwrks
         rebac_url:schema_panel#controller@platform:clckwrks
         rebac_url:relations_panel#controller@platform:clckwrks
  |]


direct_member :: Relation
direct_member = Relation "direct_member"

usergroup :: ObjectType
usergroup = ObjectType "usergroup"

initialRebacState :: RebacState
initialRebacState = RebacState
  { rsTuples = clckwrksRels }
