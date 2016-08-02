{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.Data.PeerDB where

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Blockchain.Data.DataDefs
    
import           Control.Monad.Logger    (runNoLoggingT)
import qualified Database.Persist.Postgresql as SQL
import Blockchain.EthConf
import Blockchain.SHA

getAvailablePeers::IO [PPeer]
getAvailablePeers = do
  currentTime <- getCurrentTime
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  fmap (map SQL.entityVal) $ flip SQL.runSqlPool sqldb $ 
    SQL.selectList [PPeerEnableTime SQL.<. currentTime] []

{-    E.select $
      E.from $ \peer -> do
        E.orderBy [E.rand]
        E.limit 1
        return peer
   
  case result of
   [] -> return Nothing
   (x:_) -> return $ Just x

-}

defaultPeer::PPeer
defaultPeer = PPeer{
  pPeerPubkey=Nothing,
  pPeerIp="",
  pPeerPort=30303,
  pPeerNumSessions=0,
  pPeerLastMsg="",
  pPeerLastMsgTime=posixSecondsToUTCTime 0,
  pPeerEnableTime=posixSecondsToUTCTime 0,
  pPeerLastTotalDifficulty=0,
  pPeerLastBestBlockHash=SHA 0,
  pPeerVersion=""
  }

disablePeerForSeconds::PPeer->Int->IO ()
disablePeerForSeconds peer seconds = do
  currentTime <- getCurrentTime
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  flip SQL.runSqlPool sqldb $ 
    SQL.updateWhere [PPeerIp SQL.==. pPeerIp peer, PPeerPort SQL.==. pPeerPort peer] [PPeerEnableTime SQL.=. fromIntegral seconds `addUTCTime` currentTime]
  return ()
  
