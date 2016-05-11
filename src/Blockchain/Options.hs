{-# LANGUAGE TemplateHaskell #-}

module Blockchain.Options where

import HFlags

defineFlag "debug" False "turn debug info on or off"
defineFlag "sqlPeers" False "Choose peers from the SQL DB, not the config file"
defineFlag "testnet" False "connect to testnet"
defineFlag "cNetworkID" (-1::Int) "set a custom network ID for the client"
defineFlag "syncBacktrackNumber" (10::Integer) "block number to go back when syncing"
