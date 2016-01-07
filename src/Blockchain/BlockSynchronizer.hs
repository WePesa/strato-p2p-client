module Blockchain.BlockSynchronizer (
                          addBlocks,
                          handleNewBlockHashes,
                          handleNewBlocks,
                          removeLoadedHashes,
                          getLowestHashes
                         ) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Function
import Data.List
import qualified Database.Esqueleto as E
import GHC.Int
import Safe

import Data.Time.Clock.POSIX
import Text.Printf

    
import qualified Blockchain.Colors as CL
--import Blockchain.Communication
import Blockchain.Context
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.Wire
import Blockchain.DB.SQLDB
import Blockchain.Format
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

  _ <- putBlocks blocks

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

getLowestHashes :: (MonadResource m, HasSQLDB m)=>Int64->m [SHA]
getLowestHashes n = do
  res <- 
    sqlQuery $
      E.select $
        E.from $ \bh -> do
          E.orderBy [E.desc $ bh E.^. NeededBlockHashId]
          E.limit n
          return (bh E.^. NeededBlockHashHash)

  return $ map E.unValue res

findFirstHashAlreadyInDB::[SHA]->ContextM (Maybe SHA)
findFirstHashAlreadyInDB hashes =
  fmap headMay $ filterM getBlockExists hashes

handleNewBlockHashes::[SHA]->Conduit Message ContextM Message
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
                yield $ GetBlockHashes [last blockHashes] 0x500
    Just hashInDB -> do
                liftIO $ putStrLn $ "Found a serverblock already in our database: " ++ format hashInDB
                lift $ addNeededBlockHashes $ takeWhile (/= hashInDB) blockHashes
                askForSomeBlocks
  
askForSomeBlocks::Conduit Message ContextM Message
askForSomeBlocks = do
  lift $ removeLoadedHashes
  neededHashes <- lift $ getLowestHashes 128
  when (length neededHashes > 0) $
    yield $ GetBlocks neededHashes


handleNewBlocks::[Block]->Conduit Message ContextM Message
handleNewBlocks [] = error "handleNewBlocks called with empty block list"
handleNewBlocks blocks = do
  let orderedBlocks =
        sortBy (compare `on` blockDataNumber . blockBlockData) blocks

  maybeParentBlock <- lift $ getBlock (blockDataParentHash $ blockBlockData $ head $ orderedBlocks) --head OK, [] weeded out

  hashCount <- lift $ getHashCount

  case (hashCount, maybeParentBlock) of
    (0, Nothing) -> do
      liftIO $ putStrLn $ CL.red $ "Resynching!!!!!!!!"
      handleNewBlockHashes [blockHash $ head orderedBlocks] -- head OK, [] weeded out
    (_, Nothing) ->
      liftIO $ putStrLn $ CL.red "Warning: a new block has arrived before another block sync is in progress.  This block will be thrown away for now, and re-requested later."
    (_, Just _) -> do
      liftIO $ putStrLn "Submitting new blocks"
      lift $ addBlocks $ sortBy (compare `on` blockDataNumber . blockBlockData) blocks
      liftIO $ putStrLn $ show (length blocks) ++ " blocks have been submitted"
      askForSomeBlocks
