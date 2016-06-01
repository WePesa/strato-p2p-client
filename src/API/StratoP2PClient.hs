{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module API.StratoP2PClient (
   stratoP2PClientAPIMain,
   P2PClientStatus(..),
   statusAPI
  ) where

import API.Route.Status
import API.Handler.Status
import API.Model.Status

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

app :: Application
app = serve statusAPI statusGet
 
statusAPI :: Proxy StatusAPI
statusAPI = Proxy

stratoP2PClientAPIMain :: IO ()
stratoP2PClientAPIMain = run 8086 app
