{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getDebugMsg,
  addDebugMsg,
  clearDebugMsg,
  addNeededBlockHashes,
  clearNeededBlockHashes,
  getHashCount
  ) where


import Control.Monad.Trans.Resource
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Database.Esqueleto as E
import qualified Database.Persist.Postgresql as SQL

import Blockchain.Data.DataDefs
import Blockchain.Data.Peer
import Blockchain.DB.SQLDB
import Blockchain.SHA

--import Debug.Trace

data Context =
  Context {
    contextSQLDB::SQLDB,
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
  db <- getSQLDB
  flip SQL.runSqlPool db $
    forM_ blockHashes $ \blockHash -> SQL.insert $ NeededBlockHash $ blockHash

clearNeededBlockHashes::ContextM ()
clearNeededBlockHashes = do
  db <- getSQLDB
  flip SQL.runSqlPool db $
    E.delete $ E.from $ \(_::E.SqlExpr (E.Entity NeededBlockHash)) -> return ()

getHashCount::HasSQLDB m=>m Int
getHashCount = do
  res <- 
    sqlQuery $
      E.select $
        E.from $ \(_::E.SqlExpr (E.Entity NeededBlockHash)) -> do
          return E.countRows

  case res of
    [x] -> return $ E.unValue x
    _ -> error "wrong format in response from SQL call in getHashCount"
