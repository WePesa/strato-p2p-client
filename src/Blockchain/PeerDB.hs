{-# LANGUAGE FlexibleInstances,  ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.PeerDB where

import Control.Monad
import qualified Data.Text as T
import Network
import System.IO.Unsafe

import Blockchain.Data.DataDefs
    
import qualified Data.ByteString.Char8 as BC

import           Control.Monad.Logger    (runNoLoggingT)
import qualified Database.Persist.Postgresql as SQL

connStr'::SQL.ConnectionString
connStr' = BC.pack $ "host=localhost dbname=eth user=postgres password=api port=5432"
                   
ipAddressesDB::[(String, PortNumber)]
ipAddressesDB = unsafePerformIO $ do
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  peers <- 
      flip SQL.runSqlPool sqldb $ do
        (x::[SQL.Entity PPeer]) <- SQL.selectList [] []
        return x
                               
  forM peers $ \peer -> return (T.unpack $ pPeerIp $ SQL.entityVal peer, fromIntegral $ pPeerPort $ SQL.entityVal peer)
