{-# LANGUAGE FlexibleContexts #-}

module Blockchain.ExtMergeSources (
  mergeSourcesCloseForAny
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.TMChan
  
data ConduitMsg a = ConduitMsg a | ConduitClose

mergeSourcesCloseForAny::(MonadIO mo, MonadResource mi, MonadBaseControl IO mi)=>
               [Source mi a]->Int->mi (Source mo a)
mergeSourcesCloseForAny x y = fmap (=$= exitOnClose) $ mergeSources (map (=$= wrapCloseMsg) x) y

exitOnClose::Monad m=>ConduitM (ConduitMsg a) a m ()
exitOnClose = do
  x <- await
  case x of
   Just (ConduitMsg val) -> do
     yield val
     exitOnClose
   _ -> return ()

wrapCloseMsg::(MonadResource m, MonadBaseControl IO m)=>ConduitM a (ConduitMsg a) m ()
wrapCloseMsg = do
  x <- await
  case x of
   Just val -> do
     yield $ ConduitMsg val
     wrapCloseMsg
   Nothing -> yield ConduitClose
