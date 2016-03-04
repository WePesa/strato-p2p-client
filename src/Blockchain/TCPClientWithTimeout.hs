
--see http://stackoverflow.com/questions/35022378/how-do-i-change-runtcpclient-timeout-duration

module Blockchain.TCPClientWithTimeout (
  runTCPClientWithConnectTimeout
  ) where

import Control.Concurrent
import Control.Exception
import Data.Conduit.Network

threadDelaySeconds :: Double -> IO ()
threadDelaySeconds secs =
  threadDelay (ceiling $ secs * 1e6)

runTCPClientWithConnectTimeout::ClientSettings->Double->(AppData->IO ())->IO ()
runTCPClientWithConnectTimeout settings secs cont = do
  race <- newChan
  resultMVar <- newEmptyMVar
  
  timerThreadID <- forkIO $ do
    threadDelaySeconds secs
    writeChan race False
    
  clientThreadID <- forkIO $ do
    result <-
      try $
      runTCPClient settings $ \appData -> do
        writeChan race True
        cont appData
    writeChan race True --second call needed because first call won't be hit in the case of an error caught by try
    putMVar resultMVar result
      
  timedOut <- readChan race
  
  if timedOut
    then do
      killThread timerThreadID --don't want a buildup of timer threads....
      result' <- readMVar resultMVar
      case result' of
       Left e -> throw (e::SomeException)
       Right x -> return x
    else do
      _ <- error "runTCPClientWithConnectTimeout: could not connect in time"
      killThread clientThreadID
