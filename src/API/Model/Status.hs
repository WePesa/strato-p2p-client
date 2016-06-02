
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module API.Model.Status where

import Data.Aeson.TH

data P2PClientStatus = P2PClientStatus {
    p2pClientMessage :: String,
    p2pClientTimestamp :: String   -- replace with UTCTime
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''P2PClientStatus)
