{-# LANGUAGE OverloadedStrings #-}

module Blockchain.BlockSynchronizer (
                          handleNewBlockHashes,
                          handleNewBlocks,
                          getLowestHashes
                         ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Function
import Data.List
import Data.Maybe
import qualified Database.Esqueleto as E
import GHC.Int
import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Protocol hiding (Message)
import Safe

import Data.Time.Clock.POSIX
import Text.Printf

    
import qualified Blockchain.Colors as CL
--import Blockchain.Communication
import Blockchain.Context
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Wire
import Blockchain.DB.KafkaTools
import Blockchain.DB.SQLDB
import Blockchain.Event
import Blockchain.Format
import Blockchain.Options
import Blockchain.SHA

--import Debug.Trace

data GetBlockHashesResult = NeedMore SHA | NeededHashes [SHA] deriving (Show)

removeHashes :: (MonadResource m, HasSQLDB m)=>E.Key NeededBlockHash->E.Key NeededBlockHash->m ()
removeHashes min max = do
  sqlQuery $
    E.delete $
      E.from $ \bh -> do
        E.where_ ((bh E.^. NeededBlockHashId E.<=. E.val max)
                  E.&&. (bh E.^. NeededBlockHashId E.>=. E.val min))


getLowestHashes :: (MonadResource m, HasSQLDB m)=>Int64->m [(E.Key NeededBlockHash, SHA)]
getLowestHashes n = do
  res <- 
    sqlQuery $
      E.select $
        E.from $ \bh -> do
          E.orderBy [E.desc $ bh E.^. NeededBlockHashId]
          E.limit n
          return (bh E.^. NeededBlockHashId, bh E.^. NeededBlockHashHash)

  return $ map (\(x, y) -> (E.unValue x, E.unValue y)) res

findFirstHashAlreadyInDB::[SHA]->ContextM (Maybe SHA)
findFirstHashAlreadyInDB hashes = do
  lastBlockHashes <- liftIO getLastBlockHashes
  return $ headMay $ filter (`elem` lastBlockHashes) hashes

getLastBlockHashes::IO [SHA]
getLastBlockHashes = do
  ret <-
    runKafka (mkKafkaState "strato-p2p-client" ("127.0.0.1", 9092)) $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 "thetopic"
      let offset = max (lastOffset - 10) 0
      fetchBlocks offset

  case ret of
    Left e -> error $ show e
    Right v -> return $ map blockHash v


handleNewBlockHashes::[SHA]->Conduit Event ContextM Message
--handleNewBlockHashes _ list | trace ("########### handleNewBlockHashes: " ++ show list) $ False = undefined
handleNewBlockHashes [] = do
  --this really shouldn't happen, but the go client was doing it
  --For now I will just reset the hash sync when this happens, the client will restart the sync

  --error "handleNewBlockHashes called with empty list"

  liftIO $ putStrLn $ CL.red "peer unexpectedly responded with no blocks, so for now I will reset the sync"

  lift clearNeededBlockHashes
  
handleNewBlockHashes blockHashes = do
  result <- lift $ findFirstHashAlreadyInDB blockHashes
  case result of
    Nothing -> do
      --liftIO $ putStrLn "Requesting more block hashes"
      lift $ addNeededBlockHashes blockHashes
      yield $ GetBlockHashes (last blockHashes) 0x500
    Just hashInDB -> do
      liftIO $ putStrLn $ "Found a serverblock already in our database: " ++ format hashInDB
      lift $ addNeededBlockHashes $ takeWhile (/= hashInDB) blockHashes
      askForSomeBlocks
  
askForSomeBlocks::Conduit Event ContextM Message
askForSomeBlocks = do
  --error "askForSomeBlocks called"
  --lift $ removeLoadedHashes
  neededHashes <- lift $ getLowestHashes 128
  when (length neededHashes > 0) $
    yield $ GetBlocks $ map snd neededHashes
  lift $ setRequestedHashes neededHashes


handleNewBlocks::[Block]->Conduit Event ContextM Message
handleNewBlocks [] = error "handleNewBlocks called with empty block list"
handleNewBlocks blocks = do
  --error "blocks are being loaded"

  let incomingHashes = map blockHash blocks

  requestedHashes' <- lift getRequestedHashes
  
  when (not $ incomingHashes `isPrefixOf` map snd requestedHashes') $
    error $ "hashes don't match: got\n" ++ unlines (map format incomingHashes) ++ "\nexpected\n" ++ unlines (map (format . snd) requestedHashes')
  
  let orderedBlocks =
        sortBy (compare `on` blockDataNumber . blockBlockData) blocks

  maybeParentBlock <- lift $ getBlock (blockDataParentHash $ blockBlockData $ head $ orderedBlocks) --head OK, [] weeded out

  hashCount <- lift $ getHashCount

  case (hashCount, maybeParentBlock) of
    (0, Nothing) -> do
      liftIO $ putStrLn $ CL.red $ "Resynching!!!!!!!!"
      handleNewBlockHashes [blockHash $ head orderedBlocks] -- head OK, [] weeded out
    --(_, Nothing) ->
    --  liftIO $ putStrLn $ CL.red "Warning: a new block has arrived before another block sync is in progress.  This block will be thrown away for now, and re-requested later."
    --(_, Just _) -> do
    (_, _) -> do
      lift $ produceBlocks $ sortBy (compare `on` blockDataNumber . blockBlockData) blocks
      lift $ removeHashes (minimum $ map fst $ take (length incomingHashes) requestedHashes') (maximum $ map fst $ take (length incomingHashes) requestedHashes')
      askForSomeBlocks
