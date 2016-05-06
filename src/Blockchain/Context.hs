{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getDebugMsg,
  addDebugMsg,
  getBlockHeaders,
  putBlockHeaders,
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
    blockHeaders::[BlockHeader]
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

getDebugMsg::MonadState Context m=>
             m String
getDebugMsg = do
  cxt <- get
  return $ concat $ reverse $ vmTrace cxt

getBlockHeaders::MonadState Context m=>
                 m [BlockHeader]
getBlockHeaders = do
  cxt <- get
  return $ blockHeaders cxt

putBlockHeaders::MonadState Context m=>
                 [BlockHeader]->m ()
putBlockHeaders headers = do
  cxt <- get
  put cxt{blockHeaders=headers}

addDebugMsg::MonadState Context m=>
             String->m ()
addDebugMsg msg = do
  cxt <- get
  put cxt{vmTrace=msg:vmTrace cxt}

clearDebugMsg::MonadState Context m=>
               m ()
clearDebugMsg = do
  cxt <- get
  put cxt{vmTrace=[]}

