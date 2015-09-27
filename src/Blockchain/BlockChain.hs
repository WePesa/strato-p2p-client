{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.BlockChain (
  addBlocks
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Text.Printf

import Blockchain.Context
import Blockchain.Data.BlockDB

--import Debug.Trace

addBlocks::[Block]->ContextM ()
addBlocks blocks = do
  before <- liftIO $ getPOSIXTime 

  putBlocks blocks

  after <- liftIO $ getPOSIXTime 

  liftIO $ putStrLn $ "#### Added " ++ show (length blocks) ++ " blocks, insertion time = " ++ printf "%.4f" (realToFrac $ after - before::Double) ++ "s"

