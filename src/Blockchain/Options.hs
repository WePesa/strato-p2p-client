{-# LANGUAGE TemplateHaskell #-}

module Blockchain.Options where

import HFlags

defineFlag "altGenBlock" False "use the alternate stablenet genesis block"
defineFlag "debug" False "turn debug info on or off"
defineFlag "sqlPeers" False "Choose peers from the SQL DB, not the config file"
defineFlag "testnet" False "connect to testnet"
defineFlag "wrapTransactions" False "build dummy blocks using new transactions"
