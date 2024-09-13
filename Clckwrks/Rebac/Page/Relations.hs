{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes, FlexibleContexts, GADTs #-}
module Clckwrks.Rebac.Page.Relations where

import AccessControl.Acid            (Check(..), AddRelationTuple(..), RemoveRelationTuple(..))
import AccessControl.Check           (RelationState(..), Access(..), RelationState(..))
import AccessControl.Schema          (Permission(..), Relation(..), ToPermission(..), ToRelation(..), ObjectType(..), Schema(..), ppSchema, knownObjectTypes, knownRelations)
import AccessControl.Relation        (KnownPermission, Object(..), ObjectId(..), RelationTuple(..), ToObject(..), ppRelationTuple, ppRelationTuples)
import Clckwrks
import Clckwrks.AccessControl      (checkAccess)
import Clckwrks.Monad              (plugins)
import Clckwrks.Admin.Template     (template)
import Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Authenticate.Monad (AuthenticatePluginState(..))
import Clckwrks.ProfileData.Acid   (GetProfileData(..), SetProfileData(..))
import Clckwrks.Rebac.API          (RebacApi(..), getRelationTuples, getSchema)
import Clckwrks.Rebac.URL          (RebacURL(..))
import Control.Monad.State         (get)
import Control.Monad.Trans         (liftIO)
import qualified Data.Acid         as Acid
import Data.Text                   (pack)
import qualified Data.Text         as Text
import Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy    as TL
import Data.Maybe                  (fromMaybe)
import Data.UserId                 (UserId)
import Happstack.Authenticate.Core (Email(..), User(..))
import Happstack.Authenticate.Handlers (GetUserByUserId(..), UpdateUser(..))
import Language.Haskell.HSX.QQ     (hsx)
import Text.Reform                 ((++>), mapView, transformEitherM)
import Text.Reform.HSP.Text        (form, inputText, inputSubmit, labelText, fieldset, ol, li, errorList, select, setAttrs)
import Text.Reform.Happstack       (reform)
import HSP.XMLGenerator
import HSP.XML
import Web.Plugins.Core            (Plugin(..), getPluginState)

-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

relationsPanel :: RebacURL -> Clck RebacURL Response
relationsPanel here =
  do ets    <- getRelationTuples
     (knownObjectTys, knownRels) <-
       do eSchema <- getSchema
          case eSchema of
            (Left e)  -> pure ([], [])
            (Right s) -> pure (knownObjectTypes s, knownRelations s)
     canMod <- checkAccess RelationsR (Permission "modify")
     action <- showURL here
     case ets of
       (Right tuples) ->
         do template "REBAC Relations"  () $ [hsx|
              <%>
               <% relationsTable action knownObjectTys knownRels tuples %>
               <% show canMod %>
              </%>
                                              |]
       (Left e) ->
         ok $ toResponse $ show e

--                <pre><code><% show $ ppRelationTuples tuples %></code></pre>

inputText' txt = inputText txt `setAttrs` [("size" := "15") :: Attr Text Text]

objectFormlet :: [ ObjectType ] -> ClckForm RebacURL Object
objectFormlet objectTypes =
--  Object <$> (td $ ObjectType <$> inputText' "") <*> (td $ ObjectId <$> inputText' "")
  Object <$> (td $ select ((ObjectType "","") : (map (\ot@(ObjectType ott) -> (ot, ott)) objectTypes)) ((==) (ObjectType ""))) <*> (td $ ObjectId <$> inputText' "")
  where
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])

emptyRelationIsNothing :: Relation -> Maybe Relation
emptyRelationIsNothing r@(Relation txt)
  | Text.null txt = Nothing
  | otherwise  = Just r

data FormAction
  = AddRT RelationTuple
  | RemoveRT RelationTuple

relationTupleFormlet :: [ ObjectType ] -> [ Relation ] -> ClckForm RebacURL FormAction
relationTupleFormlet knownObjectTys knownRels =
  tr ((AddRT <$> (RelationTuple 
                     <$> (objectFormlet knownObjectTys)
                     <*> (td $ select ((Relation "","") : (map (\r@(Relation rTxt) -> (r,rTxt)) knownRels)) ((==) (Relation "")))
                     <*> (objectFormlet knownObjectTys)
                     <*> (td $ (fmap emptyRelationIsNothing $ select ((Relation "","") : (map (\r@(Relation rTxt) -> (r,rTxt)) knownRels)) ((==) (Relation ""))))
                     <* (td $ inputSubmit "+"))))
  where
    tr = mapView (\xml -> [[hsx|<tr><% xml %></tr>|]])
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])
  {-
    (fieldset $
      (divControlGroup $
        (divControls (label' "new password" ++> inputPassword))
        )
      <* (divControlGroup $ divControls $ inputSubmit "Change Password"  `setAttrs` [("class" := "btn") :: Attr Text Text])
      ) `transformEitherM` updatePassword
    where
      label' :: Text -> ClckForm ProfileDataURL ()
      label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])

      divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
      divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])
-}
relationsTable action knownObjectTys knownRels tuples =
  [hsx|
     <div>
      <table class="rebac-relations">
       <caption>Relations</caption>
       <thead>
        <tr>
         <th colspan="2">Resource</th>
         <th>Relation</th>
         <th colspan="3">Subject</th>
         <th>Action</th>
        </tr>
        <tr>
         <th>Object Type</th>
         <th>Object ID</th>
         <th></th>
         <th>Object Type</th>
         <th>Object ID</th>
         <th>Subject Relation</th>
         <th></th>
        </tr>
       </thead>
       <tbody>
        <% mapM mkRow (zip [0..] tuples) %>
        <% reform (form ("" :: String)) "rebac" updated Nothing (relationTupleFormlet knownObjectTys knownRels) %>
       </tbody>
      </table>

     </div>
      |]
    where
      updated :: FormAction -> Clck RebacURL Response
      updated (AddRT rt) =
        do update (AddRelationTuple rt)
           seeOtherURL RelationsPanel
      updated (RemoveRT rt) =
        do -- liftIO $ putStrLn $ "RemoveRT - " ++ show (ppRelationTuple rt)
           update (RemoveRelationTuple rt)
           seeOtherURL RelationsPanel
{-
      mkRow :: ( StringType (ServerPartT IO) ~ Text
               , XMLType (ServerPartT IO) ~ XML 
               ) => RelationTuple -> Clck RebacURL [XMLGenT (ServerPartT IO) XML]
-}
      mkRow (i, r) =
        reform (form ("" :: String)) (TL.pack $ "rebac-" ++ show i) updated Nothing (fmap (\_ -> (RemoveRT r)) ((mapView (\x -> [mkRow'' r x]) ((inputSubmit "X") :: ClckForm RebacURL (Maybe Text.Text)))))
      mkRow'' :: RelationTuple
              -> [XMLGenT (ClckT RebacURL (ServerPartT IO)) XML]
              -> XMLGenT (ClckT RebacURL (ServerPartT IO)) (XMLType (ClckT RebacURL (ServerPartT IO)))
      mkRow'' (RelationTuple (Object (ObjectType rt) (ObjectId ri)) (Relation r) (Object (ObjectType st) (ObjectId si)) msr ) delButton =
         let sr = case msr of
                    Nothing -> ""
                    (Just (Relation r)) -> r
         in
             [hsx|
                    <tr><td><% rt  %></td>
                        <td><% ri  %></td>
                        <td><% r   %></td>
                        <td><% st  %></td>
                        <td><% si  %></td>
                        <td><% sr %></td>
                        <td><% delButton %></td>
                    </tr>
                    |]

      mkRow' (RelationTuple (Object (ObjectType rt) (ObjectId ri)) (Relation r) (Object (ObjectType st) (ObjectId si)) msr )  =
         let sr = case msr of
                    Nothing -> ""
                    (Just (Relation r)) -> r
         in
             [hsx|
                    <tr><td><% rt  %></td>
                        <td><% ri  %></td>
                        <td><% r   %></td>
                        <td><% st  %></td>
                        <td><% si  %></td>
                        <td><% sr %></td>
                        <td><button>X</button></td>
                    </tr>
                    |]
