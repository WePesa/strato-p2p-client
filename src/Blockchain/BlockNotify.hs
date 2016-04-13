{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module Blockchain.BlockNotify (
  createBlockTrigger,
  blockNotificationSource
  ) where

import Control.Monad.Logger
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Database.Persist as SQL
import qualified Database.Persist.Postgresql as SQL
--import qualified Database.Persist.Sql as SQL
import qualified Database.PostgreSQL.Simple as PS
import           Database.PostgreSQL.Simple.Notification
import           Conduit
--import           Data.List.Split
import           Control.Monad

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs
import Blockchain.Data.NewBlk
import Blockchain.ExtWord
import Blockchain.SHA

createBlockTrigger :: PS.Connection -> IO ()
createBlockTrigger conn = do
     res2 <- PS.execute_ conn "DROP TRIGGER IF EXISTS client_block_notify ON new_blk;\n\
\CREATE OR REPLACE FUNCTION client_block_notify() RETURNS TRIGGER AS $client_block_notify$ \n\ 
    \ BEGIN \n\
    \     PERFORM pg_notify('new_block', NEW.hash::text ); \n\
    \     RETURN NULL; \n\
    \ END; \n\
\ $client_block_notify$ LANGUAGE plpgsql; \n\
\ CREATE TRIGGER client_block_notify AFTER INSERT OR DELETE ON new_blk FOR EACH ROW EXECUTE PROCEDURE client_block_notify();"

     putStrLn $ "created trigger with result: " ++ (show res2)

byteStringToSHA::B.ByteString->SHA
byteStringToSHA s =
  case B16.decode s of
   (s', "") -> SHA $ bytesToWord256 $ B.unpack s'
   _ -> error "byteString in wrong format"


--notificationSource::(MonadIO m)=>SQLDB->PS.Connection->Source m Block
blockNotificationSource'::SQLDB->PS.Connection->Source IO Block
blockNotificationSource' pool conn = forever $ do
    _ <- liftIO $ PS.execute_ conn "LISTEN new_block;"
    --liftIO $ putStrLn $ "about to listen for block notifications"
    rowId <- liftIO $ fmap (byteStringToSHA . notificationData) $ getNotification conn
    liftIO $ putStrLn $ "########### block has been added: rowId=" ++ show rowId
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

connStr'::SQL.ConnectionString
connStr' = BC.pack $ "host=localhost dbname=eth user=postgres password=api port=5432"

blockNotificationSource::Source IO Block
blockNotificationSource = do

  conn <- liftIO $ PS.connect PS.defaultConnectInfo {
            PS.connectPassword = "api",
            PS.connectDatabase = "eth"
           }

  pool <- liftIO $ runNoLoggingT $ SQL.createPostgresqlPool connStr' 20

  liftIO $ createBlockTrigger conn

  blockNotificationSource' pool conn
  
getBlock::SQLDB->SQL.Key NewBlk->IO (Maybe Block)
getBlock pool row = do
    --pool <- getSQLDB      
    fmap (fmap newBlkToBlock) $ SQL.runSqlPool (SQL.get row) pool
