
module Blockchain.Display (
  addPingCount,
  setPeers,
  displayMessage
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State

import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.Data.DataDefs
import Blockchain.Data.Peer
import Blockchain.Format
import Blockchain.Data.Wire

setTitle::String->IO()
setTitle value = do
  putStr $ "\ESC]0;" ++ value ++ "\007"
          
updateStatus::ContextM ()
updateStatus = do
  cxt <- get
  count <- getHashCount
  
  liftIO $ setTitle $
    "pingCount = " ++ show (pingCount cxt)
    ++ ", peer count=" ++ show (length $ peers cxt)
    ++ ", hashes requested=" ++ show count

addPingCount::ContextM ()
addPingCount = do
  cxt <- get
  put cxt{pingCount = pingCount cxt + 1}
  updateStatus

setPeers::[Peer]->ContextM ()
setPeers p = do
  cxt <- get
  put cxt{peers = p}
  updateStatus

prefix::Bool->String
prefix True = CL.green "msg>>>>>: "
prefix False = CL.cyan "msg<<<<: "
       
  
displayMessage::Bool->Message->ContextM ()
displayMessage _ Ping = return ()
displayMessage _ Pong = return ()
displayMessage _ GetPeers = return ()
displayMessage _ (Peers _) = return ()
displayMessage outbound (GetBlocks blocks) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "GetBlocks: " ++ "(Requesting " ++ show (length blocks) ++ " blocks)"
displayMessage outbound (Transactions transactions) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "Transactions: " ++ "(Received " ++ show (length transactions) ++ " transactions)"
displayMessage _ QqqqPacket = return ()
displayMessage outbound (BlockHashes shas) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "BlockHashes: " ++ "(" ++ show (length shas) ++ " new hashes)"
  updateStatus
displayMessage outbound (Blocks []) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "Blocks: [no blocks]"
  updateStatus
displayMessage outbound (Blocks blocks) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "Blocks: " ++ "(" ++ show (length blocks) ++ " new blocks, ending with #" ++ show (blockDataNumber $ blockBlockData $ last blocks) ++ ")" --last OK because the [] input was pattern matched above
  updateStatus
displayMessage outbound msg =
  liftIO $ putStrLn $ (prefix outbound) ++ format msg
