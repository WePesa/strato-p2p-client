
module Blockchain.Event where

import Blockchain.Data.DataDefs
import Blockchain.Data.Wire


data Event = MsgEvt Message | NewTX RawTransaction | NewBL Block Integer deriving (Show)

