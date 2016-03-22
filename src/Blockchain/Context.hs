{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getDebugMsg,
  addDebugMsg,
  getBlockHeaders,
  putBlockHeaders,
  getRequestedHashes,
  setRequestedHashes,
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

import Blockchain.Data.BlockHeader
import Blockchain.Data.DataDefs
import Blockchain.DB.SQLDB
import Blockchain.SHA

--import Debug.Trace

data Context =
  Context {
    contextSQLDB::SQLDB,
    miningDataset::B.ByteString,
    vmTrace::[String],
    blockHeaders::[BlockHeader],
    requestedHashes::[(E.Key NeededBlockHash, SHA)]
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

getBlockHeaders::ContextM [BlockHeader]
getBlockHeaders = do
  cxt <- get
  return $ blockHeaders cxt

putBlockHeaders::[BlockHeader]->ContextM ()
putBlockHeaders headers = do
  cxt <- get
  put cxt{blockHeaders=headers}

addDebugMsg::String->ContextM ()
addDebugMsg msg = do
  cxt <- get
  put cxt{vmTrace=msg:vmTrace cxt}

clearDebugMsg::ContextM ()
clearDebugMsg = do
  cxt <- get
  put cxt{vmTrace=[]}

getRequestedHashes::ContextM [(E.Key NeededBlockHash, SHA)]
getRequestedHashes = do
  cxt <- get
  return $ requestedHashes cxt

setRequestedHashes::[(E.Key NeededBlockHash, SHA)]->ContextM ()
setRequestedHashes hashes = do
  cxt <- get
  put cxt{requestedHashes=hashes}

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
