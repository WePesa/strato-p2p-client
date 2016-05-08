{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module Blockchain.RawTXNotify (
  createTXTrigger,
  txNotificationSource
  ) where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Database.Persist as SQL
import qualified Database.Persist.Postgresql as SQL
--import qualified Database.Persist.Sql as SQL
import qualified Database.PostgreSQL.Simple as PS
import           Database.PostgreSQL.Simple.Notification

import Blockchain.Data.RawTransaction
import Blockchain.DB.SQLDB

createTXTrigger::(MonadIO m, MonadLogger m)=>
                 m ()
createTXTrigger = do
  conn <- liftIO $ PS.connect PS.defaultConnectInfo { --TODO add to configuration file
    PS.connectPassword = "api",
    PS.connectDatabase = "eth"
    }

  res2 <- liftIO $ PS.execute_ conn "DROP TRIGGER IF EXISTS client_tx_notify ON raw_transaction;\n\
\CREATE OR REPLACE FUNCTION client_tx_notify() RETURNS TRIGGER AS $client_tx_notify$ \n\ 
    \ BEGIN \n\
    \     PERFORM pg_notify('new_transaction', NEW.id::text ); \n\
    \     RETURN NULL; \n\
    \ END; \n\
\ $client_tx_notify$ LANGUAGE plpgsql; \n\
\ CREATE TRIGGER client_tx_notify AFTER INSERT OR DELETE ON raw_transaction FOR EACH ROW EXECUTE PROCEDURE client_tx_notify();"

  liftIO $ PS.close conn

  logInfoN $ T.pack $ "created trigger with result: " ++ (show res2)



txNotificationSource::(MonadIO m, MonadResource m, MonadLogger m)=>
                      Source m RawTransaction
txNotificationSource = do
  conn <- liftIO $ PS.connect PS.defaultConnectInfo {
            PS.connectPassword = "api",
            PS.connectDatabase = "eth"
           }

  _ <- register $ PS.close conn

  pool <- liftIO $ runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  lift $ createTXTrigger

  forever $ do
    _ <- liftIO $ PS.execute_ conn "LISTEN new_transaction;"
    logInfoN $ T.pack $ "about to listen for raw transaction notifications"
    rowId <- liftIO $ fmap (SQL.toSqlKey . read . BC.unpack . notificationData) $ getNotification conn
    logInfoN $ T.pack $ "########### raw transaction has been added: rowId=" ++ show rowId
    maybeTx <- liftIO $ getTransaction pool rowId
    case maybeTx of
     Nothing -> error "wow, item was removed in notificationSource before I could get it....  This didn't seem like a likely occurence when I was programming, you should probably deal with this possibility now"
     Just tx -> yield tx

connStr'::SQL.ConnectionString
connStr' = BC.pack $ "host=localhost dbname=eth user=postgres password=api port=5432"

getTransaction::SQLDB->SQL.Key RawTransaction->IO (Maybe RawTransaction)
getTransaction pool row = do
    --pool <- getSQLDB      
    SQL.runSqlPool (SQL.get row) pool
