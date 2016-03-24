{-# LANGUAGE OverloadedStrings #-}

module Blockchain.BlockSynchronizer (
  getLastBlockHashes
  ) where

import Control.Lens
import Network.Kafka

import Blockchain.Data.BlockDB
import Blockchain.SHA

--import Debug.Trace

getLastBlockHashes::IO [SHA]
getLastBlockHashes = do
  ret <-
    runKafka (mkKafkaState "strato-p2p-client" ("127.0.0.1", 9092)) $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 "block"
      let offset = max (lastOffset - 10) 0
      fetchBlocks offset

  case ret of
    Left e -> error $ show e
    Right v -> return $ map blockHash v
