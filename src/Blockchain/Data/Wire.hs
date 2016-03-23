{-# LANGUAGE OverloadedStrings #-}
--TODO : Take this next line out
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.Data.Wire (
  Message(..),
  BlockHashOrNumber(..),
  Direction(..),
  Capability(..),
  obj2WireMessage,
  wireMessage2Obj
  ) where

import Crypto.Types.PubKey.ECC
import qualified Data.ByteString as B
import Data.List
import Data.Word
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Data.BlockDB
import Blockchain.Data.BlockHeader
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util

--import Debug.Trace

data Capability = ETH Integer | SHH Integer deriving (Show)

name2Cap::Integer->String->Capability
name2Cap ver "eth" = ETH ver
name2Cap ver "shh" = SHH ver
name2Cap _ x = error $ "Unknown capability string: " ++ x

instance RLPSerializable Capability where
    rlpEncode (ETH ver) = RLPArray [rlpEncode ("eth"::B.ByteString), rlpEncode ver]
    rlpEncode (SHH ver) = RLPArray [rlpEncode ("shh"::B.ByteString), rlpEncode ver]

    rlpDecode (RLPArray [name, ver]) = name2Cap (rlpDecode ver) $ rlpDecode name
    rlpDecode x = error $ "wrong format given to rlpDecode for Capability: " ++ show (pretty x)

data TerminationReason =
  DisconnectRequested
  | TCPSubSystemError
  | BreachOfProtocol
  | UselessPeer
  | TooManyPeers
  | AlreadyConnected
  | IncompatibleP2PProtocolVersion
  | NullNodeIdentityReceived
  | ClientQuitting
  | UnexpectedIdentity
  | ConnectedToSelf
  | PingTimeout
  | OtherSubprotocolReason deriving (Show)

numberToTerminationReason::Integer->TerminationReason
numberToTerminationReason 0x00 = DisconnectRequested
numberToTerminationReason 0x01 = TCPSubSystemError
numberToTerminationReason 0x02 = BreachOfProtocol
numberToTerminationReason 0x03 = UselessPeer
numberToTerminationReason 0x04 = TooManyPeers
numberToTerminationReason 0x05 = AlreadyConnected
numberToTerminationReason 0x06 = IncompatibleP2PProtocolVersion
numberToTerminationReason 0x07 = NullNodeIdentityReceived
numberToTerminationReason 0x08 = ClientQuitting
numberToTerminationReason 0x09 = UnexpectedIdentity
numberToTerminationReason 0x0a = ConnectedToSelf
numberToTerminationReason 0x0b = PingTimeout
numberToTerminationReason 0x10 = OtherSubprotocolReason
numberToTerminationReason x = error $ "numberToTerminationReasion called with unsupported number: " ++ show x


terminationReasonToNumber::TerminationReason->Integer
terminationReasonToNumber DisconnectRequested = 0x00
terminationReasonToNumber TCPSubSystemError = 0x01
terminationReasonToNumber BreachOfProtocol = 0x02
terminationReasonToNumber UselessPeer = 0x03
terminationReasonToNumber TooManyPeers = 0x04
terminationReasonToNumber AlreadyConnected = 0x05
terminationReasonToNumber IncompatibleP2PProtocolVersion = 0x06
terminationReasonToNumber NullNodeIdentityReceived = 0x07
terminationReasonToNumber ClientQuitting = 0x08
terminationReasonToNumber UnexpectedIdentity = 0x09
terminationReasonToNumber ConnectedToSelf = 0x0a
terminationReasonToNumber PingTimeout = 0x0b
terminationReasonToNumber OtherSubprotocolReason = 0x10
  
data BlockHashOrNumber = BlockHash SHA | BlockNumber Int deriving (Show)

instance Format BlockHashOrNumber where
  format (BlockHash x) = format x
  format (BlockNumber x) = "Number: " ++ show x

instance RLPSerializable BlockHashOrNumber where
  rlpEncode (BlockHash x) = rlpEncode x
  rlpEncode (BlockNumber x) = rlpEncode $ toInteger x
  rlpDecode val@(RLPString s) | B.length s == 32 = BlockHash $ rlpDecode val
  rlpDecode val = BlockNumber $ fromInteger $ rlpDecode val

data Direction = Forward | Reverse deriving (Show)

instance RLPSerializable Direction where
  rlpEncode Forward = rlpEncode (0::Integer)
  rlpEncode Reverse = rlpEncode (1::Integer)
  rlpDecode x | rlpDecode x == (0::Integer) = Forward
  rlpDecode _ = Reverse

data Message =
  --p2p wire protocol
  Hello { version::Int, clientId::String, capability::[Capability], port::Int, nodeId::Point } |
  Disconnect TerminationReason |
  Ping |
  Pong |

  --ethereum wire protocol
  Status { protocolVersion::Int, networkID::Int, totalDifficulty::Integer, latestHash::SHA, genesisHash:: SHA } |
  NewBlockHashes [(SHA, Int)] |
  Transactions [Transaction] | 
  GetBlockHeaders {block::BlockHashOrNumber, maxHeaders::Int, skip::Int, direction::Direction} |
  BlockHeaders [BlockHeader] |
  GetBlockBodies [SHA] |
  BlockBodies [([Transaction], [BlockHeader])] |
  NewBlock Block Integer |

  --Obsolete, will soon be removed
  GetBlockHashes { parentSHA::SHA, numChildItems::Integer } |
  BlockHashes [SHA] |
  GetBlocks [SHA] |
  Blocks [Block] |

  
  PacketCount Integer |
  WhisperProtocolVersion Int deriving (Show)

instance Format Point where
  format (Point x y) = padZeros 64 (showHex x "") ++ padZeros 64 (showHex y "")
  format PointO = "PointO"

instance Format Message where
  format Hello{version=ver, clientId=c, capability=cap, port=p, nodeId=n} =
    CL.blue "Hello" ++
      "    version: " ++ show ver ++ "\n" ++
      "    cliendId: " ++ show c ++ "\n" ++
      "    capability: " ++ intercalate ", " (show <$> cap) ++ "\n" ++
      "    port: " ++ show p ++ "\n" ++
      "    nodeId: " ++ take 20 (format n) ++ "...."
  format (Disconnect reason) = CL.blue "Disconnect" ++ "(" ++ show reason ++ ")"
  format Ping = CL.blue "Ping"
  format Pong = CL.blue "Pong"

  --ethereum wire protocol
  format Status{ protocolVersion=ver, networkID=nID, totalDifficulty=d, latestHash=lh, genesisHash=gh } =
    CL.blue "Status" ++
      "    protocolVersion: " ++ show ver ++ "\n" ++
      "    networkID: " ++ show nID ++ "\n" ++
      "    totalDifficulty: " ++ show d ++ "\n" ++
      "    latestHash: " ++ format lh ++ "\n" ++
      "    genesisHash: " ++ format gh
  format (Transactions transactions) =
    CL.blue "Transactions:\n    " ++ tab (intercalate "\n    " (format <$> transactions))
  format (GetBlockHeaders b max skip direction) =
    CL.blue "GetBlockHeaders" ++ " (max: " ++ show max ++ ", " ++ show direction ++ ", skip " ++ show skip ++ "): "
    ++ format b
  format (BlockHeaders headers) = CL.blue "BlockHeaders:"
                                  ++ tab ("\n" ++ unlines (format <$> headers))
  format (GetBlockBodies hashes) =
    CL.blue "GetBlockBodies" ++ " (" ++ show (length hashes) ++ " hashes)"
  format (BlockBodies bodies) =
    CL.blue "BlockBodies:"
    ++ tab ("\n" ++ unlines (formatBody <$> bodies))
    where
      formatBody (transactions, uncles) = "BlockBody:" ++ tab (formatTransactions transactions ++ formatUncles uncles)
      formatTransactions [] = "No transactions, "
      formatTransactions transactions = "\nTransactions:" ++ tab ("\n" ++ unlines (map format transactions))
      formatUncles [] = "No uncles"
      formatUncles uncles = "\nUncles:" ++ tab ("\n" ++ unlines (map format uncles))
      
--Short version
  format (BlockHashes shas) =
    CL.blue "BlockHashes " ++ "(" ++ show (length shas) ++ " new hashes)" 
--Long version
{-  format (BlockHashes shas) =
    CL.blue "BlockHashes:" ++  
   tab ("\n" ++ intercalate "\n    " (show . pretty <$> shas))-}

  format (GetBlocks shas) =
    CL.blue "GetBlocks:" ++ 
    tab ("\n" ++ intercalate "\n    " (format <$> shas))
  format (Blocks blocks) = CL.blue "Blocks:" ++ tab("\n" ++ intercalate "\n    " (format <$> blocks))
  format (GetBlockHashes hash' numChild) =
    CL.blue "GetBlockHashes" ++ " (max: " ++ show numChild ++ "): " ++ format hash'
  format (NewBlock block d) = CL.blue "NewBlock" ++ " (" ++ show d ++ ")" ++ tab ("\n" ++ format block)
  format (PacketCount c) =
    CL.blue "PacketCount:" ++ show c
  format (NewBlockHashes items) = CL.blue "NewBlockHashes"  ++ tab("\n" ++ intercalate "\n    " ((\(hash, number) -> "(" ++ format hash ++ ", " ++ show number ++ ")") <$> items))
  format (WhisperProtocolVersion ver) = CL.blue "WhisperProtocolVersion " ++ show ver


instance RLPSerializable Point where
  rlpEncode (Point x y) =
    rlpEncode $ B.pack $ (word256ToBytes $ fromInteger x) ++ (word256ToBytes $ fromInteger y)
  rlpEncode PointO = error "rlpEncode for Point called for PointO"
  rlpDecode o =
    Point (toInteger $ bytesToWord256 $ B.unpack x) (toInteger $ bytesToWord256 $ B.unpack y)
    where
      (x, y) = B.splitAt 32 $ rlpDecode o

obj2WireMessage::Word8->RLPObject->Message
obj2WireMessage 0x0 (RLPArray [ver, cId, RLPArray cap, p, nId]) =
  Hello (fromInteger $ rlpDecode ver) (rlpDecode cId) (rlpDecode <$> cap) (fromInteger $ rlpDecode p) (rlpDecode nId)
obj2WireMessage 0x1 (RLPArray [reason]) =
  Disconnect (numberToTerminationReason $ rlpDecode reason)
obj2WireMessage 0x2 (RLPArray []) = Ping
obj2WireMessage 0x2 (RLPArray [RLPArray []]) = Ping
obj2WireMessage 0x3 (RLPArray []) = Pong
obj2WireMessage 0x10 (RLPArray [ver, nID, d, lh, gh]) = 
    Status {
  protocolVersion=fromInteger $ rlpDecode ver,
  networkID = fromInteger $ rlpDecode nID,
  totalDifficulty = rlpDecode d,
  latestHash=rlpDecode lh,
  genesisHash=rlpDecode gh
}

obj2WireMessage 0x11 (RLPArray items) =
  NewBlockHashes $ map (\(RLPArray [hash, number]) -> (rlpDecode hash, fromInteger $ rlpDecode number)) $ items
obj2WireMessage 0x12 (RLPArray transactions) =
  Transactions $ rlpDecode <$> transactions


--obj2WireMessage 0x13 (RLPArray [hash', num]) =
--  GetBlockHashes (rlpDecode hash') (rlpDecode num)
obj2WireMessage 0x13 (RLPArray [b, mh, s, d]) =
  GetBlockHeaders (rlpDecode b) (fromInteger $ rlpDecode mh) (fromInteger $ rlpDecode s) (rlpDecode d)
--obj2WireMessage 0x14 (RLPArray items) =
--  BlockHashes $ rlpDecode <$> items
obj2WireMessage 0x14 (RLPArray items) =
  BlockHeaders $ rlpDecode <$> items
obj2WireMessage 0x15 (RLPArray hashes) =
  GetBlockBodies $ rlpDecode <$> hashes


obj2WireMessage 0x15 (RLPArray items) =
  GetBlocks $ rlpDecode <$> items
--obj2WireMessage 0x16 (RLPArray blocks) =
--  Blocks $ rlpDecode <$> blocks
obj2WireMessage 0x16 (RLPArray bodies) =
  BlockBodies $ (\(RLPArray [RLPArray transactions, RLPArray uncles]) -> (map rlpDecode transactions, map rlpDecode uncles)) <$> bodies
obj2WireMessage 0x17 (RLPArray [block, td]) =
  NewBlock (rlpDecode block) (rlpDecode td)
obj2WireMessage 0x18 (RLPArray [c]) =
  PacketCount $ rlpDecode c

obj2WireMessage 0x20 (RLPArray [ver]) =
  WhisperProtocolVersion $ fromInteger $ rlpDecode ver

obj2WireMessage x y = error ("Missing case in obj2WireMessage: " ++ show x ++ ", " ++ show (pretty y))






wireMessage2Obj::Message->(Word8, RLPObject)
wireMessage2Obj Hello { version = ver,
                        clientId = cId,
                        capability = cap,
                        port = p,
                        nodeId = nId } =
  (128, RLPArray [
           rlpEncode $ toInteger ver,
           rlpEncode cId,
           RLPArray $ rlpEncode <$> cap,
           rlpEncode $ toInteger p,
           rlpEncode nId
          ])
wireMessage2Obj (Disconnect reason) = (0x0, RLPArray [rlpEncode $ terminationReasonToNumber reason])
wireMessage2Obj Ping = (0x2, RLPArray [])
wireMessage2Obj Pong = (0x3, RLPArray [])
wireMessage2Obj (Status ver nID d lh gh) =
    (0x10, RLPArray [rlpEncode $ toInteger ver, rlpEncode $ toInteger nID, rlpEncode d, rlpEncode lh, rlpEncode gh])
wireMessage2Obj (GetBlockHeaders b max skip direction) =
  (0x13, RLPArray [rlpEncode b, rlpEncode $ toInteger max, rlpEncode $ toInteger skip, rlpEncode direction])
wireMessage2Obj (Transactions transactions) = (0x12, RLPArray (rlpEncode <$> transactions))
wireMessage2Obj (GetBlockHashes hash' numChildren) = 
    (0x13, RLPArray [rlpEncode hash', rlpEncode numChildren])
wireMessage2Obj (BlockHashes shas) = 
  (0x14, RLPArray (rlpEncode <$> shas))
wireMessage2Obj (GetBlockBodies shas) = 
  (0x15, RLPArray (rlpEncode <$> shas))
wireMessage2Obj (GetBlocks shas) = 
  (0x15, RLPArray (rlpEncode <$> shas))
wireMessage2Obj (Blocks blocks) =
  (0x16, RLPArray (rlpEncode <$> blocks))
wireMessage2Obj (NewBlock block d) =
  (0x17, RLPArray [rlpEncode block, rlpEncode d])
wireMessage2Obj (PacketCount c) =
  (0x18, RLPArray [rlpEncode c])

wireMessage2Obj (WhisperProtocolVersion ver) = 
  (0x20, RLPArray [rlpEncode $ toInteger ver])



