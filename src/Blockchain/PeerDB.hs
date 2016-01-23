{-# LANGUAGE FlexibleInstances,  ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.PeerDB where

import System.IO.Unsafe

import Blockchain.Data.DataDefs
    
import qualified Data.ByteString.Char8 as BC

import           Control.Monad.Logger    (runNoLoggingT)
import qualified Database.Persist.Postgresql as SQL

connStr'::SQL.ConnectionString
connStr' = BC.pack $ "host=localhost dbname=eth user=postgres password=api port=5432"
                   
ipAddressesDB::[PPeer]
ipAddressesDB = unsafePerformIO $ do
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20

  fmap (map SQL.entityVal) $ flip SQL.runSqlPool sqldb $ SQL.selectList [] []
                               
