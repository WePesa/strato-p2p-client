
module Blockchain.Display (
  displayMessage
  ) where

import Control.Monad.IO.Class

import qualified Blockchain.Colors as CL
import Blockchain.Data.BlockHeader
import Blockchain.Format
import Blockchain.Data.Wire

prefix::Bool->String
prefix True = CL.green "msg>>>>>: "
prefix False = CL.cyan "msg<<<<: "
       
  
displayMessage::Bool->Message->IO ()
displayMessage _ Ping = return ()
displayMessage _ Pong = return ()
displayMessage outbound (Transactions transactions) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "Transactions: " ++ "(Received " ++ show (length transactions) ++ " transactions)"
displayMessage outbound (BlockHeaders []) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "BlockHeaders: No headers"
displayMessage outbound (BlockHeaders headers) = do
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "BlockHeaders: " ++ "(" ++ show (length headers) ++ " new headers ending with #" ++ show (number $ last $ headers) ++ ")"
displayMessage outbound (GetBlockBodies hashes) =
  liftIO $ putStrLn $ prefix outbound ++ CL.blue "GetBlockBodies" ++ " (" ++ show (length hashes) ++ " hashes)"
displayMessage outbound (BlockBodies bodies) = do
  let transactionCount = length $ concat $ map fst bodies
  putStrLn $ prefix outbound ++ CL.blue "BlockBodies: "
    ++ "(" ++ show (length bodies)
    ++ " bodies, includes " ++ show transactionCount
    ++ " transaction" ++ (if transactionCount == 1 then "" else "s") ++ ")"
displayMessage outbound msg =
  liftIO $ putStrLn $ (prefix outbound) ++ format msg
