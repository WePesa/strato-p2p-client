{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Logger
import HFlags

main::IO ()    
main = do
  args <- $initHFlags "Strato Peer Client"
  flip runLoggingT printLogMsg $ stratoP2PClient args
