{-# LANGUAGE FlexibleInstances,  ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.PeerDB where

import System.IO.Unsafe

import Blockchain.Data.DataDefs
    
import           Control.Monad.Logger    (runNoLoggingT)
import qualified Database.Persist.Postgresql as SQL
import Blockchain.EthConf

ipAddressesDB::[PPeer]
ipAddressesDB = unsafePerformIO $ do
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20

  fmap (map SQL.entityVal) $ flip SQL.runSqlPool sqldb $ SQL.selectList [] []
                               
