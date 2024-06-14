{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes, FlexibleContexts, GADTs #-}
module Clckwrks.Rebac.Page.Relations where

import AccessControl.Acid            (Check(..))
import AccessControl.Check           (RelationState(..), Access(..), RelationState(..))
import AccessControl.Schema          (Permission(..), Relation(..), ToPermission(..), ToRelation(..), ObjectType(..), ppSchema)
import AccessControl.Relation        (KnownPermission, Object(..), ObjectId(..), RelationTuple(..), ToObject(..), ppRelationTuples)
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
import Data.Maybe                  (fromMaybe)
import Data.UserId                 (UserId)
import Happstack.Authenticate.Core (Email(..), User(..))
import Happstack.Authenticate.Handlers (GetUserByUserId(..), UpdateUser(..))
import Language.Haskell.HSX.QQ     (hsx)
import Text.Reform                 ((++>), mapView, transformEitherM)
import Text.Reform.HSP.Text        (form, inputText, inputSubmit, labelText, fieldset, ol, li, errorList, setAttrs)
import Text.Reform.Happstack       (reform)
import HSP.XMLGenerator
import HSP.XML
import Web.Plugins.Core            (Plugin(..), getPluginState)

-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

relationsPanel :: RebacURL -> Clck RebacURL Response
relationsPanel here =
  do ets <- getRelationTuples
     canMod <- checkAccess RelationsR (Permission "modify")
     action <- showURL here
     case ets of
       (Right tuples) ->
         do template "REBAC Relations"  () $ [hsx|
              <%>
               <% relationsTable action tuples %>
               <% show canMod %>
              </%>
                                              |]
       (Left e) ->
         ok $ toResponse $ show e

--                <pre><code><% show $ ppRelationTuples tuples %></code></pre>

inputText' txt = inputText txt `setAttrs` [("size" := "15") :: Attr Text Text]

objectFormlet :: ClckForm RebacURL Object
objectFormlet =
  Object <$> (td $ ObjectType <$> inputText' "") <*> (td $ ObjectId <$> inputText' "")
  where
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])

relationTupleFormlet :: ClckForm RebacURL RelationTuple
relationTupleFormlet =
  tr ((RelationTuple <$> objectFormlet <*> (td $ Relation <$> inputText' "") <*> objectFormlet <* (td $ inputSubmit "+")))
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
relationsTable action tuples =
  [hsx|
     <div>
      <table class="rebac-relations">
       <caption>Relations</caption>
       <thead>
        <tr>
         <th colspan="2">Resource</th>
         <th>Relation</th>
         <th colspan="2">Subject</th>
         <th>Action</th>
        </tr>
        <tr>
         <th>Object Type</th>
         <th>Object ID</th>
         <th></th>
         <th>Object Type</th>
         <th>Object ID</th>
         <th></th>
        </tr>
       </thead>
       <tbody>
        <% mapM mkRow tuples %>
        <% reform (form ("" :: String)) "rebac" updated Nothing relationTupleFormlet %>
       </tbody>
      </table>

     </div>
      |]
    where
      updated :: RelationTuple -> Clck RebacURL Response
      updated rt =
        ok $ toResponse $ show rt
      mkRow (RelationTuple (Object (ObjectType rt) (ObjectId ri)) (Relation r) (Object (ObjectType st) (ObjectId si)))  =
             [hsx|
                    <tr><td><% rt %></td>
                        <td><% rt %></td>
                        <td><% r  %></td>
                        <td><% st %></td>
                        <td><% si %></td>
                        <td><button>X</button></td>
                    </tr>
                    |]
