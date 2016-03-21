{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (
  main
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Trans.Resource
import Crypto.PubKey.ECC.DH
import Crypto.Types.PubKey.ECC
import Crypto.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import qualified Data.Text as T
import Data.Time.Clock
import qualified Database.Persist.Postgresql as SQL
import HFlags
import Network
import qualified Network.Haskoin.Internals as H
import System.Random
import System.IO

import Blockchain.Frame
import Blockchain.UDP hiding (Ping,Pong)
import Blockchain.RLPx

import Blockchain.BlockSynchronizer
import qualified Blockchain.Colors as C
--import Blockchain.Communication
import Blockchain.Constants
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
--import Blockchain.Data.SignedTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.Wire
import Blockchain.Database.MerklePatricia
import Blockchain.DB.DetailsDB
--import Blockchain.DB.ModifyStateDB
import Blockchain.Display
import Blockchain.Error
import Blockchain.Event
import Blockchain.ExtMergeSources
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.PeerUrls
import Blockchain.RawTXNotify
import Blockchain.Options
--import Blockchain.SampleTransactions
import Blockchain.PeerDB
import Blockchain.SHA
--import Blockchain.SigningTools
--import Blockchain.Verifier
import Blockchain.TCPClientWithTimeout
import Blockchain.Util
import qualified Data.ByteString.Base16 as B16
--import Debug.Trace

import Data.Word
import Data.Bits
import Data.Maybe

handleMsg::Point->Conduit Event ContextM Message
handleMsg peerId = do
  yield $ Hello {
              version = 4,
              clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
              capability = [ETH ethVersion], -- , SHH shhVersion],
              port = 0,
              nodeId = peerId
            }

  awaitForever $ \msg ->
    case msg of
        MsgEvt Hello{} -> do
               bestBlock <- lift getBestBlock
               genesisBlockHash <- lift getGenesisBlockHash
               yield Status{
                           protocolVersion=fromIntegral ethVersion,
                           networkID=if flags_networkID == -1
                                     then (if flags_testnet then 0 else 1) 
                                     else flags_networkID,
                           totalDifficulty=0,
                           latestHash=blockHash bestBlock,
                           genesisHash=genesisBlockHash
                         }
        MsgEvt Ping -> do
               lift addPingCount
               yield Pong
        MsgEvt GetPeers -> do
               yield $ Peers []
               yield GetPeers
        MsgEvt (Peers thePeers) -> do
               lift $ setPeers thePeers
        MsgEvt (BlockHashes blockHashes) -> do
               handleNewBlockHashes blockHashes
        MsgEvt (GetBlocks _) -> do
               yield $ Blocks []
        MsgEvt (Blocks blocks) -> do
               handleNewBlocks blocks
               --NewBlockPacket block baseDifficulty -> do
        MsgEvt (NewBlockPacket block _) -> do
               handleNewBlocks [block]
               --ifBlockInDBSubmitNextBlock baseDifficulty block

        MsgEvt (Status{latestHash=lh, genesisHash=gh}) -> do
               genesisBlockHash <- lift getGenesisBlockHash
               when (gh /= genesisBlockHash) $ error "Wrong genesis block hash!!!!!!!!"
               previousLowestHash <- lift $ getLowestHashes 1
               case previousLowestHash of
                 [] -> handleNewBlockHashes [lh]
                 [x] -> yield $ GetBlockHashes (snd x) 0x500
                 _ -> error "unexpected multiple values in call to getLowetHashes 1"
        MsgEvt (GetTransactions _) -> do
               yield $ Transactions []
               --liftIO $ sendMessage handle GetTransactions
        --MsgEvt (Transactions transactions) -> return ()
        NewTX tx -> do
               when (not $ rawTransactionFromBlock tx) $ do
                   yield $ Transactions [rawTX2TX tx]
           
        _-> return ()

{-
createTransaction::Transaction->ContextM SignedTransaction
createTransaction t = do
    userNonce <- lift $ addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=userNonce}

createTransactions::[Transaction]->ContextM [SignedTransaction]
createTransactions transactions = do
    userNonce <- lift $ addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    forM (zip transactions [userNonce..]) $ \(t, n) -> do
      liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=n}
-}

pointToByteString::Point->B.ByteString
pointToByteString (Point x y) = B.pack $ word256ToBytes (fromInteger x) ++ word256ToBytes (fromInteger y)
pointToByteString PointO = error "pointToByteString got value PointO, I don't know what to do here"

{-
doit::Point->ContextM ()
doit myPublic = do
    liftIO $ putStrLn "Connected"

    --lift $ addCode B.empty --This is probably a bad place to do this, but I can't think of a more natural place to do it....  Empty code is used all over the place, and it needs to be in the database.
    --lift (setStateDBStateRoot . blockDataStateRoot . blockBlockData =<< getBestBlock)

  --signedTx <- createTransaction simpleTX
  --signedTx <- createTransaction outOfGasTX
  --signedTx <- createTransaction simpleStorageTX
  --signedTx <- createTransaction createContractTX
  --signedTx <- createTransaction sendMessageTX

  --signedTx <- createTransaction createContractTX
  --signedTx <- createTransaction paymentContract
  --signedTx <- createTransaction sendCoinTX
  --signedTx <- createTransaction keyValuePublisher
  --signedTx <- createTransaction sendKeyVal

  --liftIO $ print $ whoSignedThisTransaction signedTx

                
  --sendMessage socket $ Transactions [signedTx]

  --signedTxs <- createTransactions [createMysteryContract]
  --liftIO $ sendMessage socket $ Transactions signedTxs
-}


--cbSafeTake::Monad m=>Int->Consumer B.ByteString m B.ByteString
cbSafeTake::Monad m=>Int->ConduitM BC.ByteString o m BC.ByteString
cbSafeTake i = do
    ret <- fmap BL.toStrict $ CB.take i
    if B.length ret /= i
       then error "safeTake: not enough data"
       else return ret
           
getRLPData::Monad m=>Consumer B.ByteString m B.ByteString
getRLPData = do
  first <- fmap (fromMaybe $ error "no rlp data") CB.head
  case first of
    x | x < 128 -> return $ B.singleton x
    x | x >= 192 && x <= 192+55 -> do
               rest <- cbSafeTake $ fromIntegral $ x - 192
               return $ x `B.cons` rest
    x | x >= 0xF8 && x <= 0xFF -> do
               length' <- cbSafeTake $ fromIntegral x-0xF7
               rest <- cbSafeTake $ fromIntegral $ bytes2Integer $ B.unpack length'
               return $ x `B.cons` length' `B.append` rest
    x -> error $ "missing case in getRLPData: " ++ show x 

bytesToMessages::Conduit B.ByteString ContextM Message
bytesToMessages = do
  msgTypeData <- cbSafeTake 1
  let word = fromInteger (rlpDecode $ rlpDeserialize msgTypeData::Integer)

  objBytes <- getRLPData
  yield $ obj2WireMessage (if word == 128 then 0 else word) $ rlpDeserialize objBytes
  bytesToMessages
          
messagesToBytes::Conduit Message ContextM B.ByteString
messagesToBytes = do
  maybeMsg <- await
  case maybeMsg of
    Nothing -> return ()
    Just msg -> do
        let (theWord, o) = wireMessage2Obj msg
        yield $ theWord `B.cons` rlpSerialize o
        messagesToBytes
             
theCurve::Curve
theCurve = getCurveByName SEC_p256k1

hPubKeyToPubKey::H.PubKey->Point
hPubKeyToPubKey pubKey = Point (fromIntegral x) (fromIntegral y)
  where
    x = fromMaybe (error "getX failed in prvKey2Address") $ H.getX hPoint
    y = fromMaybe (error "getY failed in prvKey2Address") $ H.getY hPoint
    hPoint = H.pubKeyPoint pubKey

--This must exist somewhere already
tap::MonadIO m=>(a->m ())->Conduit a m a
tap f = do
  awaitForever $ \x -> do
      lift $ f x
      yield x
  
runPeer::String->PortNumber->Point->PrivateNumber->IO ()
runPeer ipAddress thePort otherPubKey myPriv = do
  putStrLn $ C.blue "Welcome to strato-p2p-client"
  putStrLn $ C.blue "============================"
  putStrLn $ C.green " * " ++ "Attempting to connect to " ++ C.yellow (ipAddress ++ ":" ++ show thePort)

  let myPublic = calculatePublic theCurve myPriv
  putStrLn $ C.green " * " ++ "my pubkey is: " ++ C.yellow (take 30 (format $ pointToByteString myPublic) ++ "...")
  --putStrLn $ "my NodeID is: " ++ (format $ pointToByteString $ hPubKeyToPubKey $ H.derivePubKey $ fromMaybe (error "invalid private number in main") $ H.makePrvKey $ fromIntegral myPriv)

  putStrLn $ C.green " * " ++ "server pubkey is : " ++ C.yellow (take 30 (format $ pointToByteString otherPubKey) ++ "...")

  --cch <- mkCache 1024 "seed"

  dataset <- return "" -- mmapFileByteString "dataset0" Nothing

  runTCPClientWithConnectTimeout (clientSettings (fromIntegral thePort) $ BC.pack ipAddress) 5 $ \server -> 
      runResourceT $ do
        pool <- runNoLoggingT $ SQL.createPostgresqlPool
                "host=localhost dbname=eth user=postgres password=api port=5432" 20
      
        _ <- flip runStateT (Context pool 0 [] dataset [] []) $ do
          (_, (outCxt, inCxt)) <-
            transPipe liftIO (appSource server) $$+
            ethCryptConnect myPriv otherPubKey `fuseUpstream`
            transPipe liftIO (appSink server)

          let handleError::SomeException->IO a
              handleError e = error' (show e)

          eventSource <- mergeSourcesCloseForAny [
            transPipe (liftIO . flip catch handleError) (appSource server) =$=
            transPipe (liftIO . flip catch handleError) (ethDecrypt inCxt) =$=
            bytesToMessages =$=
            tap (displayMessage False) =$=
            CL.map MsgEvt,
            transPipe liftIO txNotificationSource =$= CL.map NewTX
            ] 2

          eventSource =$=
            handleMsg myPublic =$=
            tap (displayMessage True) =$=
            messagesToBytes =$=
            ethEncrypt outCxt $$
            transPipe liftIO (appSink server)


        return ()

getPubKeyRunPeer::String->PortNumber->Maybe Point->IO ()
getPubKeyRunPeer ipAddress thePort maybePubKey = do
  entropyPool <- liftIO createEntropyPool

  let g = cprgCreate entropyPool :: SystemRNG
      (myPriv, _) = generatePrivate g $ getCurveByName SEC_p256k1

  case maybePubKey of
    Nothing -> do
      putStrLn $ "Attempting to connect to " ++ show ipAddress ++ ":" ++ show thePort ++ ", but I don't have the pubkey.  I will try to use a UDP ping to get the pubkey."
      eitherOtherPubKey <- getServerPubKey (fromMaybe (error "invalid private number in main") $ H.makePrvKey $ fromIntegral myPriv) ipAddress thePort
      case eitherOtherPubKey of
            Right otherPubKey -> do
                               putStrLn $ "#### Success, the pubkey has been obtained: " ++ (format $ pointToByteString otherPubKey)
                               runPeer ipAddress thePort otherPubKey myPriv
            Left e -> putStrLn $ "Error, couldn't get public key for peer: " ++ show e
    Just otherPubKey -> runPeer ipAddress thePort otherPubKey myPriv
                      

runPeerInList::[(String, PortNumber, Maybe Point)]->Maybe Int->IO ()
runPeerInList addresses maybePeerNumber = do
  peerNumber <- case maybePeerNumber of
                  Just x -> return x
                  Nothing -> randomRIO (0, length addresses - 1)

  let (ipAddress, thePort, maybePubKey) = addresses !! peerNumber

  getPubKeyRunPeer ipAddress thePort maybePubKey
               
main::IO ()    
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  args <- $initHFlags "The Ethereum Haskell Peer"

  let maybePeerNumber =
        case args of
          [] -> Nothing
          [x] -> return $ read x
          _ -> error "usage: ethereumH [servernum]"

  if flags_sqlPeers
    then sequence_ $ repeat $ runPeerInList (map (\peer -> (T.unpack $ pPeerIp peer, fromIntegral $ pPeerPort peer, Just $ pPeerPubkey peer)) ipAddressesDB) maybePeerNumber
    else sequence_ $ repeat $ runPeerInList (map (\peer -> (fst peer, snd peer, Nothing)) ipAddresses) maybePeerNumber
