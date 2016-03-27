{-# LANGUAGE OverloadedStrings #-}

module Blockchain.BlockSynchronizer (
  getLastBlocks
  ) where

import Control.Lens
import Control.Monad
import Network.Kafka

import Blockchain.Data.BlockDB

--import Debug.Trace

getLastBlocks::IO [Block]
getLastBlocks = do
  ret <-
    runKafka (mkKafkaState "strato-p2p-client" ("127.0.0.1", 9092)) $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 "block"
      when (lastOffset == 0) $ error "Block stream is empty, you need to run strato-setup to insert the genesis block."
      let offset = max (lastOffset - 100) 0
      fetchBlocks offset

  case ret of
    Left e -> error $ show e
    Right v -> return v

