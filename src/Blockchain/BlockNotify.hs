{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module Blockchain.BlockNotify (
  createBlockTrigger,
  blockNotificationSource
  ) where

import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BC
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

createBlockTrigger :: PS.Connection -> IO ()
createBlockTrigger conn = do
     res2 <- PS.execute_ conn "DROP TRIGGER IF EXISTS block_notify ON block;\n\
\CREATE OR REPLACE FUNCTION block_notify() RETURNS TRIGGER AS $block_notify$ \n\ 
    \ BEGIN \n\
    \     PERFORM pg_notify('new_block', NEW.id::text ); \n\
    \     RETURN NULL; \n\
    \ END; \n\
\ $block_notify$ LANGUAGE plpgsql; \n\
\ CREATE TRIGGER block_notify AFTER INSERT OR DELETE ON raw_block FOR EACH ROW EXECUTE PROCEDURE block_notify();"

     putStrLn $ "created trigger with result: " ++ (show res2)


--notificationSource::(MonadIO m)=>SQLDB->PS.Connection->Source m Block
blockNotificationSource'::SQLDB->PS.Connection->Source IO Block
blockNotificationSource' pool conn = forever $ do
    _ <- liftIO $ PS.execute_ conn "LISTEN new_block;"
    --liftIO $ putStrLn $ "about to listen for block notifications"
    rowId <- liftIO $ fmap (SQL.toSqlKey . read . BC.unpack . notificationData) $ getNotification conn
    liftIO $ putStrLn $ "########### block has been added: rowId=" ++ show rowId
    maybeBlock <- lift $ getBlock pool rowId
    case maybeBlock of
     Nothing -> error "wow, item was removed in notificationSource before I could get it....  This didn't seem like a likely occurence when I was programming, you should probably deal with this possibility now"
     Just b -> yield b

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
  
getBlock::SQLDB->SQL.Key Block->IO (Maybe Block)
getBlock pool row = do
    --pool <- getSQLDB      
    SQL.runSqlPool (SQL.get row) pool
