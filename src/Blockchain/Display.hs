
module Blockchain.Display (
  displayMessage
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State

import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.Data.BlockHeader
import Blockchain.Data.DataDefs
import Blockchain.Data.Peer
import Blockchain.Format
import Blockchain.Data.Wire

setTitle::String->IO()
setTitle value = do
  putStr $ "\ESC]0;" ++ value ++ "\007"
          
updateStatus::IO ()
updateStatus = do
--  cxt <- get
--  count <- getHashCount

  let count = 1
  
  liftIO $ setTitle $ "hashes requested=" ++ show count

prefix::Bool->String
prefix True = CL.green "msg>>>>>: "
prefix False = CL.cyan "msg<<<<: "
       
  
displayMessage::Bool->Message->IO ()
displayMessage _ Ping = return ()
displayMessage _ Pong = return ()
displayMessage outbound (GetBlocks blocks) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "GetBlocks: " ++ "(Requesting " ++ show (length blocks) ++ " blocks)"
displayMessage outbound (Transactions transactions) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "Transactions: " ++ "(Received " ++ show (length transactions) ++ " transactions)"
displayMessage outbound (BlockHeaders []) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "BlockHeaders: No headers"
displayMessage outbound (BlockHeaders headers) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "BlockHeaders: " ++ "(" ++ show (length headers) ++ " new headers ending with #" ++ show (number $ last $ headers) ++ ")"
  updateStatus
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
