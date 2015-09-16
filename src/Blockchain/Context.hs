{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getDebugMsg,
  addDebugMsg,
  clearDebugMsg,
  addNeededBlockHashes,
  clearNeededBlockHashes
  ) where


import Control.Monad.Trans.Resource
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Database.Persist.Postgresql as SQL

import Blockchain.Data.DataDefs
import Blockchain.Data.Peer
import Blockchain.DB.HashDB
import Blockchain.DB.SQLDB
import Blockchain.SHA

--import Debug.Trace

data Context =
  Context {
    contextSQLDB::SQLDB,
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer],
    miningDataset::B.ByteString,
    vmTrace::[String]
    }

type ContextM = StateT Context (ResourceT IO)

instance HasSQLDB ContextM where
  getSQLDB = fmap contextSQLDB get

{-
initContext::String->IO Context
initContext theType = do
  liftIO $ putStr "Loading mining cache.... "
  hFlush stdout
  dataset <- return "" -- mmapFileByteString "dataset0" Nothing
  liftIO $ putStrLn "Finished"
  homeDir <- getHomeDirectory                     
  createDirectoryIfMissing False $ homeDir </> dbDir theType
  return $ Context
      []
      0
      []
      dataset
      False
-}

getDebugMsg::ContextM String
getDebugMsg = do
  cxt <- get
  return $ concat $ reverse $ vmTrace cxt

addDebugMsg::String->ContextM ()
addDebugMsg msg = do
  cxt <- get
  put cxt{vmTrace=msg:vmTrace cxt}

clearDebugMsg::ContextM ()
clearDebugMsg = do
  cxt <- get
  put cxt{vmTrace=[]}

addNeededBlockHashes::[SHA]->ContextM ()
addNeededBlockHashes blockHashes = do
  cxt <- get
  put cxt{neededBlockHashes=reverse blockHashes ++ neededBlockHashes cxt}

  db <- getSQLDB
  flip SQL.runSqlPool db $
    forM_ blockHashes $ \blockHash -> SQL.insert $ NeededBlockHash $ blockHash


clearNeededBlockHashes::ContextM ()
clearNeededBlockHashes = do
  cxt <- get
  put cxt{neededBlockHashes=[]}

