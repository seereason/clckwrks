{-# language OverloadedStrings #-}
module Clckwrks.Mail where

import Clckwrks.Acid                   (CoreState(..), GetFromAddress(..), GetReplyToAddress(..), GetSendmailPath(..), getAcidState)
import Clckwrks.Monad                  (ClckT(..), query)
import Control.Monad.Trans             (MonadIO(liftIO))
import Data.Text                       (Text)
import Happstack.Authenticate.Core     (Email(..), SimpleAddress(..))
import Network.Mail.Mime               (Address(..), Mail(..), emptyMail, renderMail', renderAddress, renderSendMail, renderSendMailCustom, sendmail)


-- | produce an empty 'Mail' message with the 'from' address in 'CoreState'
--
-- The `reply-to` address is not automatically set. See 'addSystemReplyTo'
emptySystemEmail :: (MonadIO m) => ClckT url m (Either Text Mail)
emptySystemEmail =
  do mFrom <- query GetFromAddress
     case mFrom of
       Nothing   -> pure $ Left  $ "Default from address not configured"
       (Just (SimpleAddress fromNm (Email fromEm)))  -> pure $ Right $ emptyMail (Address fromNm fromEm)

addSystemReplyTo :: (MonadIO m) => Mail -> ClckT url m Mail
addSystemReplyTo m =
  do mReplyTo <- query GetReplyToAddress
     case mReplyTo of
       Nothing -> pure m
       (Just (SimpleAddress replyNm (Email replyEm))) ->
         pure $ m { mailHeaders = mailHeaders m ++ [("reply-to", renderAddress (Address replyNm replyEm))] }


-- | send an email using the sendmail configured in 'CoreState'
--
-- Use 'emptySystemEmail' to ensure the 'From' address is set correctly.
--
-- If the sendmail path is not configured -- it will try system default of /usr/sbin/sendmail.
--
-- There is no error reporting if things fail.
sendMail :: (MonadIO m) => Mail
         -> ClckT url m ()
sendMail mail =
  do mSendmailPath <- query GetSendmailPath
     case mSendmailPath of
       Nothing             -> liftIO $ renderSendMail mail
       (Just sendmailPath) -> liftIO  $ renderSendMailCustom sendmailPath ["-t"] mail
