{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getDebugMsg,
  addDebugMsg,
  getBlockHeaders,
  putBlockHeaders,
  syncedBlock,
  setIsSynced,
  clearDebugMsg
  ) where


import Control.Monad.Trans.Resource
import Control.Monad.State
import qualified Data.ByteString as B

import Blockchain.Data.BlockHeader
import Blockchain.DB.SQLDB

--import Debug.Trace

data Context =
  Context {
    contextSQLDB::SQLDB,
    miningDataset::B.ByteString,
    vmTrace::[String],
    blockHeaders::[BlockHeader],
    contextSynced::Maybe Integer
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

syncedBlock::ContextM (Maybe Integer)
syncedBlock = do
  cxt <- get
  return $ contextSynced cxt

setIsSynced::Integer->ContextM ()
setIsSynced number = do
  cxt <- get
  put cxt{contextSynced=Just number}


addDebugMsg::String->ContextM ()
addDebugMsg msg = do
  cxt <- get
  put cxt{vmTrace=msg:vmTrace cxt}

clearDebugMsg::ContextM ()
clearDebugMsg = do
  cxt <- get
  put cxt{vmTrace=[]}

