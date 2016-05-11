{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module Blockchain.BlockNotify (
  createBlockTrigger,
  blockNotificationSource
  ) where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Database.Persist as SQL
import qualified Database.Persist.Postgresql as SQL
import qualified Database.PostgreSQL.Simple as PS
import           Database.PostgreSQL.Simple.Notification

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs
import Blockchain.Data.NewBlk
import Blockchain.ExtWord
import Blockchain.SHA
import Blockchain.EthConf

createBlockTrigger::(MonadIO m, MonadLogger m)=>
                    m ()
createBlockTrigger = do
  conn <- liftIO $ PS.connect PS.defaultConnectInfo {
    PS.connectUser = user . sqlConfig $ ethConf,
    PS.connectPassword = password . sqlConfig $ ethConf,
    PS.connectDatabase = database . sqlConfig $ ethConf
    }
  res2 <- liftIO $ PS.execute_ conn "DROP TRIGGER IF EXISTS client_block_notify ON new_blk;\n\
\CREATE OR REPLACE FUNCTION client_block_notify() RETURNS TRIGGER AS $client_block_notify$ \n\ 
    \ BEGIN \n\
    \     PERFORM pg_notify('new_block', NEW.hash::text ); \n\
    \     RETURN NULL; \n\
    \ END; \n\
\ $client_block_notify$ LANGUAGE plpgsql; \n\
\ CREATE TRIGGER client_block_notify AFTER INSERT OR DELETE ON new_blk FOR EACH ROW EXECUTE PROCEDURE client_block_notify();"

  logInfoN $ T.pack $ "created trigger with result: " ++ show res2

byteStringToSHA::B.ByteString->SHA
byteStringToSHA s =
  case B16.decode s of
   (s', "") -> SHA $ bytesToWord256 $ B.unpack s'
   _ -> error "byteString in wrong format"



blockNotificationSource::(MonadIO m, MonadBaseControl IO m, MonadResource m, MonadLogger m)=>
                         Source m Block
blockNotificationSource = do
  conn <- liftIO $ PS.connect PS.defaultConnectInfo {
    PS.connectUser = user . sqlConfig $ ethConf,
    PS.connectPassword = password . sqlConfig $ ethConf,
    PS.connectDatabase = database . sqlConfig $ ethConf
    }

  _ <- register $ PS.close conn

  pool <- liftIO $ runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  createBlockTrigger

  forever $ do
    _ <- liftIO $ PS.execute_ conn "LISTEN new_block;"
    logInfoN "about to listen for block notifications"
    rowId <- liftIO $ fmap (byteStringToSHA . notificationData) $ getNotification conn
    logInfoN $ T.pack $ "########### block has been added: rowId=" ++ show rowId
    maybeBlock <- lift $ getNewBlk' pool rowId
    case maybeBlock of
     Nothing -> error "wow, item was removed in notificationSource before I could get it....  This didn't seem like a likely occurence when I was programming, you should probably deal with this possibility now"
     Just b -> yield $ newBlkToBlock b
     
getNewBlk'::(MonadIO m, MonadBaseControl IO m)=>
            SQLDB->SHA->m (Maybe NewBlk)
getNewBlk' pool h = do
  res <- runResourceT $
    SQL.runSqlPool (SQL.getBy $ TheHash h) pool

  return $ fmap SQL.entityVal res

