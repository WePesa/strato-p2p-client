{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Blockchain.Event (
  Event(..),
  handleEvents
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans
import Data.Conduit
import Data.List
import Data.Maybe
import qualified Data.Set as S

import qualified Blockchain.Colors as C
import Blockchain.Context
import Blockchain.Data.DataDefs
import Blockchain.Data.Wire
import Blockchain.Data.BlockDB
import Blockchain.Data.BlockHeader
import Blockchain.Data.BlockOffset
import Blockchain.Data.NewBlk
import Blockchain.Data.Transaction
import Blockchain.DB.SQLDB
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Stream.VMEvent
import Blockchain.Verification

data Event = MsgEvt Message | NewTX RawTransaction | NewBL Block Integer deriving (Show)

setTitleAndProduceBlocks::HasSQLDB m=>[Block]->m Int
setTitleAndProduceBlocks blocks = do
  lastVMEvents <- liftIO $ fetchLastVMEvents 200
  let lastBlockHashes = [blockHash b | ChainBlock b <- lastVMEvents]
  let newBlocks = filter (not . (`elem` lastBlockHashes) . blockHash) blocks
  when (not $ null newBlocks) $ do
    liftIO $ putStrLn $ "Block #" ++ show (maximum $ map (blockDataNumber . blockBlockData) newBlocks)
    liftIO $ C.setTitle $ "Block #" ++ show (maximum $ map (blockDataNumber . blockBlockData) newBlocks)
    _ <- produceVMEvents $ map ChainBlock newBlocks
    return ()

  return $ length newBlocks

fetchLimit = 50


filterRequestedBlocks::[SHA]->[Block]->[Block]
filterRequestedBlocks _ [] = []
filterRequestedBlocks [] _ = []
filterRequestedBlocks (h:hRest) (b:bRest) | blockHash b == h = b:filterRequestedBlocks hRest bRest
filterRequestedBlocks hashes (_:bRest) = filterRequestedBlocks hashes bRest

maxReturnedHeaders::Int
maxReturnedHeaders=1000

handleEvents::(MonadIO m, HasSQLDB m, MonadState Context m)=>
              Conduit Event m Message
handleEvents = awaitForever $ \msg -> do
  case msg of
   MsgEvt Hello{} -> error "A hello message appeared after the handshake"
   MsgEvt Ping -> yield Pong
   MsgEvt (NewBlock block' _) -> do
              lift $ putNewBlk $ blockToNewBlk block'
              let parentHash = blockDataParentHash $ blockBlockData block'
              blockOffsets <- lift $ getBlockOffsetsForHashes [parentHash]
              case blockOffsets of
               [x] | blockOffsetHash x == parentHash -> do
                       _ <- lift $ setTitleAndProduceBlocks [block']
                       return ()
               _ -> do
                 liftIO $ putStrLn "#### New block is missing its parent, I am resyncing"
                 syncFetch

   MsgEvt (Status{latestHash=_, genesisHash=gh}) -> error "A status message appeared after the handshake"

   MsgEvt (Transactions txs) -> lift $ insertTXIfNew Nothing txs

   MsgEvt (NewBlockHashes _) -> syncFetch

   MsgEvt (GetBlockHeaders start max' 0 Forward) -> do
          blockOffsets <-
           case start of
            BlockNumber n -> lift $ fmap (map blockOffsetOffset) $ getBlockOffsetsForNumber $ fromIntegral n
            BlockHash h -> lift $ getOffsetsForHashes [h]

          liftIO $ putStrLn $ "blockOffsets: " ++ show blockOffsets
         
          blocks <-
           case blockOffsets of
            [] -> return []
            (blockOffset:_) -> do
                    vmEvents <- liftIO $ fmap (fromMaybe []) $ fetchVMEventsIO $ fromIntegral blockOffset
                    return $ [b | ChainBlock b <- vmEvents]

          let blocksWithHashes = map (\b -> (blockHash b, b)) blocks
          existingHashes <- lift $ fmap (map blockOffsetHash) $ getBlockOffsetsForHashes $ map fst blocksWithHashes
          let existingBlocks = map snd $ filter ((`elem` existingHashes) . fst) blocksWithHashes
                
          yield $ BlockHeaders $ nub $ map blockToBlockHeader  $ take max' $ filter ((/= MP.StateRoot "") . blockDataStateRoot . blockBlockData) existingBlocks
          return ()

   MsgEvt (GetBlockBodies []) -> yield $ BlockBodies []
   MsgEvt (GetBlockBodies headers@(first:rest)) -> do
          offsets <- lift $ getOffsetsForHashes [first]
          case offsets of
            [] -> error $ "########### Warning: peer is asking for a block I don't have: " ++ format first
            (o:_) -> do
              vmEvents <- liftIO $ fmap (fromMaybe (error "Internal error: an offset in SQL points to a value ouside of the block stream.")) $ fetchVMEventsIO $ fromIntegral o
              let blocks = [b | ChainBlock b <- vmEvents]
              let requestedBlocks = filterRequestedBlocks headers blocks
              yield $ BlockBodies $ map blockToBody requestedBlocks





   MsgEvt (BlockHeaders headers) -> do
               alreadyRequestedHeaders <- lift getBlockHeaders
               when (null alreadyRequestedHeaders) $ do
                 let headerHashes = S.fromList $ map headerHash headers
                     neededParentHashes = S.fromList $ map parentHash $ filter ((/= 0) . number) headers
                     allNeeded = headerHashes `S.union` neededParentHashes
                 found <- lift $ fmap (S.fromList . map blockOffsetHash) $ getBlockOffsetsForHashes $ S.toList allNeeded
                 let unfoundParents = S.toList $ neededParentHashes S.\\ headerHashes S.\\found

                 when (not $ null unfoundParents) $ do
                      error $ "incoming blocks don't seem to have existing parents: " ++ unlines (map format $ unfoundParents)

                 let neededHeaders = filter (not . (`elem` found) . headerHash) headers

                 lift $ putBlockHeaders neededHeaders
                 liftIO $ putStrLn $ "putBlockHeaders called with length " ++ show (length neededHeaders)
                 let neededHashes = map headerHash neededHeaders
                 --when (length neededHeaders /= length (S.toList $ S.fromList neededHashes)) $ error "duplicates in neededHeaders"
                 yield $ GetBlockBodies neededHashes




   MsgEvt (BlockBodies []) -> return ()
   MsgEvt (BlockBodies bodies) -> do
               headers <- lift getBlockHeaders
               let verified = and $ zipWith (\h b -> transactionsRoot h == transactionsVerificationValue (fst b)) headers bodies
               when (not verified) $ error "headers don't match bodies"
               --when (length headers /= length bodies) $ error "not enough bodies returned"
               liftIO $ putStrLn $ "len headers is " ++ show (length headers) ++ ", len bodies is " ++ show (length bodies)
               newCount <- lift $ setTitleAndProduceBlocks $ zipWith createBlockFromHeaderAndBody headers bodies
               let remainingHeaders = drop (length bodies) headers
               lift $ putBlockHeaders remainingHeaders
               if null remainingHeaders
                 then 
                    if newCount > 0
                      then yield $ GetBlockHeaders (BlockHash $ headerHash $ last headers) maxReturnedHeaders 0 Forward
                      else return ()
                 else yield $ GetBlockBodies $ map headerHash remainingHeaders
   NewTX tx -> do
               when (not $ rawTransactionFromBlock tx) $ do
                   yield $ Transactions [rawTX2TX tx]
   NewBL b _ -> yield $ NewBlock b 0
           
   _-> return ()



syncFetch::(MonadIO m, MonadState Context m)=>
           Conduit Event m Message
syncFetch = do
  blockHeaders' <- lift getBlockHeaders
  when (null blockHeaders') $ do
    lastVMEvents <- liftIO $ fetchLastVMEvents fetchLimit
    let lastBlocks = [b | ChainBlock b <- lastVMEvents]
    if null lastBlocks
      then error "overflow in syncFetch"
      else do
        let lastBlockNumber = blockDataNumber . blockBlockData . last $ lastBlocks
        yield $ GetBlockHeaders (BlockNumber lastBlockNumber) maxReturnedHeaders 0 Forward
