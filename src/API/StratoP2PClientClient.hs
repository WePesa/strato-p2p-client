{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module API.StratoP2PClientClient (
   p2pClientStatusRoute,
   P2PClientStatus(..)
  ) where

import API.StratoP2PClient
import Control.Monad.Trans.Either

import Servant.Client
 
p2pClientStatusRoute :: BaseUrl -> EitherT ServantError IO P2PClientStatus
p2pClientStatusRoute = client statusAPI
