{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module Blockchain.RawTXNotify (
  createTXTrigger,
  txNotificationSource
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

import Blockchain.Data.RawTransaction
import Blockchain.DB.SQLDB
import Blockchain.EthConf

createTXTrigger :: PS.Connection -> IO ()
createTXTrigger conn = do
     res2 <- PS.execute_ conn "DROP TRIGGER IF EXISTS client_tx_notify ON raw_transaction;\n\
\CREATE OR REPLACE FUNCTION client_tx_notify() RETURNS TRIGGER AS $client_tx_notify$ \n\ 
    \ BEGIN \n\
    \     PERFORM pg_notify('new_transaction', NEW.id::text ); \n\
    \     RETURN NULL; \n\
    \ END; \n\
\ $client_tx_notify$ LANGUAGE plpgsql; \n\
\ CREATE TRIGGER client_tx_notify AFTER INSERT OR DELETE ON raw_transaction FOR EACH ROW EXECUTE PROCEDURE client_tx_notify();"

     putStrLn $ "created trigger with result: " ++ (show res2)


--notificationSource::(MonadIO m)=>SQLDB->PS.Connection->Source m RawTransaction
txNotificationSource'::SQLDB->PS.Connection->Source IO RawTransaction
txNotificationSource' pool conn = forever $ do
    _ <- liftIO $ PS.execute_ conn "LISTEN new_transaction;"
    --liftIO $ putStrLn $ "about to listen for raw transaction notifications"
    rowId <- liftIO $ fmap (SQL.toSqlKey . read . BC.unpack . notificationData) $ getNotification conn
    --liftIO $ putStrLn $ "########### raw transaction has been added: rowId=" ++ show rowId
    maybeTx <- lift $ getTransaction pool rowId
    case maybeTx of
     Nothing -> error "wow, item was removed in notificationSource before I could get it....  This didn't seem like a likely occurence when I was programming, you should probably deal with this possibility now"
     Just tx -> yield tx

txNotificationSource::Source IO RawTransaction
txNotificationSource = do
  conn <- liftIO $ PS.connectPostgreSQL connStr
  pool <- liftIO $ runNoLoggingT $ SQL.createPostgresqlPool connStr' 20

  liftIO $ createTXTrigger conn

  txNotificationSource' pool conn
  
getTransaction::SQLDB->SQL.Key RawTransaction->IO (Maybe RawTransaction)
getTransaction pool row = do
    --pool <- getSQLDB      
    SQL.runSqlPool (SQL.get row) pool
