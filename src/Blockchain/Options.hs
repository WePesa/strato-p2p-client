{-# LANGUAGE TemplateHaskell #-}

module Blockchain.Options where

import HFlags

defineFlag "fetchLimit" (50::Int) "number of items in block history to go back when synching"
