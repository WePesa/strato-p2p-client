{-# LANGUAGE OverloadedStrings #-}

module Blockchain.BlockSynchronizer (
                          addBlocks,
                          handleNewBlockHashes,
                          handleNewBlocks,
                          removeLoadedHashes,
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
import Blockchain.DB.SQLDB
import Blockchain.Event
import Blockchain.Format
import Blockchain.Options
import Blockchain.SHA

--import Debug.Trace

data GetBlockHashesResult = NeedMore SHA | NeededHashes [SHA] deriving (Show)

--Only use for debug purposes, to trick the peer to rerun VM code for a particular block
{-

import qualified Data.ByteString as B
import Blockchain.Data.RLP
import Blockchain.DBM

debug_blockDBGet::HasBlockDB m=>B.ByteString->m (Maybe B.ByteString)
debug_blockDBGet hash' = do
  maybeBlockBytes <- blockDBGet hash'
  case maybeBlockBytes of
    Nothing -> return Nothing
    Just blockBytes -> do
      let theBlock = rlpDecode . rlpDeserialize $ blockBytes
      if blockDataNumber (blockBlockData theBlock) > 99263
        then return Nothing
        else return maybeBlockBytes
-}






-- {-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

                
addBlocks::[Block]->ContextM ()
addBlocks blocks = do
  before <- liftIO $ getPOSIXTime

  --when (blockDataNumber (blockBlockData $ head blocks) > 100000) $ error "adding block over 100000"
            
  if flags_kafka
    then do
    putBlocksKafka blocks
    return ()
    else do
    putBlocks blocks False
    return ()

  after <- liftIO $ getPOSIXTime

  liftIO $ putStrLn $ "#### Added " ++ show (length blocks) ++ " blocks, insertion time = " ++ printf "%.4f" (realToFrac $ after - before::Double) ++ "s"

getBlockExists :: (MonadResource m, HasSQLDB m)=>SHA->m Bool
getBlockExists h = do
  fmap (not . null) $
      sqlQuery $
      E.select $
         E.from $ \bd -> do
                     E.where_ (bd E.^. BlockDataRefHash E.==. E.val h)
                     return $ bd E.^. BlockDataRefHash

removeLoadedHashes :: (MonadResource m, HasSQLDB m)=>m ()
removeLoadedHashes = do
  sqlQuery $
    E.delete $
      E.from $ \bh -> do
        E.where_ (bh E.^. NeededBlockHashHash `E.in_` (
          E.subList_select $
            E.from $ \bd -> do
              return (bd E.^. BlockDataRefHash)))

removeHashes :: (MonadResource m, HasSQLDB m)=>[SHA]->m ()
removeHashes hashes = do
  sqlQuery $
    E.delete $
      E.from $ \bh -> do
        E.where_ (bh E.^. NeededBlockHashHash `E.in_` E.valList hashes)

removeHashes' :: (MonadResource m, HasSQLDB m)=>E.Key NeededBlockHash->E.Key NeededBlockHash->m ()
removeHashes' min max = do
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

findFirstHashAlreadyInDB'::[SHA]->ContextM (Maybe SHA)
findFirstHashAlreadyInDB' hashes =
  fmap headMay $ filterM getBlockExists hashes

findFirstHashAlreadyInDB::[SHA]->ContextM (Maybe SHA)
findFirstHashAlreadyInDB hashes = do
  lastBlockHashes <- liftIO getLastBlockHashes
  return $ headMay $ filter (`elem` lastBlockHashes) hashes

fourth4::(a,b,c,d)->d
fourth4 (_, _, _, x) = x

fifth5::(a,b,c,d,e)->e
fifth5 (_, _, _, _, x) = x

getLastBlockHashes::IO [SHA]
getLastBlockHashes = do
  ret <-
    runKafka (mkKafkaState "qqqqkafkaclientidqqqq" ("127.0.0.1", 9092)) $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 "thetopic"
      let offset = max (lastOffset - 10) 0
      result <- fetch (Offset $ fromIntegral offset) 0 "thetopic"
      let qq = concat $ map (map (_kafkaByteString . fromJust . _valueBytes . fifth5 . _messageFields .  _setMessage)) $ map _messageSetMembers $ map fourth4 $ head $ map snd $ _fetchResponseFields result
      return $ fmap (rlpDecode . rlpDeserialize) qq

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
  
  if (not $ incomingHashes `isPrefixOf` map snd requestedHashes')
    then error $ "hashes don't match: got\n" ++ unlines (map format incomingHashes) ++ "\nexpected\n" ++ unlines (map (format . snd) requestedHashes')
    else liftIO $ putStrLn "hashes match"
  
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
      liftIO $ putStrLn "Submitting new blocks"
      lift $ addBlocks $ sortBy (compare `on` blockDataNumber . blockBlockData) blocks
      liftIO $ putStrLn $ show (length blocks) ++ " blocks have been submitted"
      liftIO $ putStrLn "removing hashes"
      --liftIO $ putStrLn $ "hashesToDelete: " ++ unlines (map format incomingHashes)
      liftIO $ putStrLn "removing hashes"
      --lift $ removeHashes incomingHashes
      lift $ removeHashes' (minimum $ map fst $ take (length incomingHashes) requestedHashes') (maximum $ map fst $ take (length incomingHashes) requestedHashes')
      liftIO $ putStrLn "done removing hashes"
      askForSomeBlocks
