{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module Executable.StratoP2PClient (
  stratoP2PClient
  ) where

import Control.Exception.Lifted
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
import qualified Database.Persist.Postgresql as SQL
import Network
import qualified Network.Haskoin.Internals as H
import System.Random

import Blockchain.Frame
import Blockchain.UDP hiding (Ping,Pong)
import Blockchain.RLPx

import Blockchain.BlockNotify
import qualified Blockchain.Colors as C
--import Blockchain.Communication
import Blockchain.Constants
import Blockchain.Context
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
--import Blockchain.Data.SignedTransaction
import Blockchain.Data.Wire
import Blockchain.DB.DetailsDB
import Blockchain.DB.SQLDB
--import Blockchain.DB.ModifyStateDB
import Blockchain.Display
import Blockchain.Error
import Blockchain.EthConf hiding (genesisHash,port)
import Blockchain.Event
import Blockchain.ExtMergeSources
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.ClientOptions
import Blockchain.PeerUrls
import Blockchain.RawTXNotify
--import Blockchain.SampleTransactions
import Blockchain.PeerDB
import Blockchain.TCPClientWithTimeout
import Blockchain.Util

--import Debug.Trace

import Data.Maybe



awaitMsg::MonadIO m=>
          ConduitM Event Message m (Maybe Message)
awaitMsg = do
  x <- await
  case x of
   Just (MsgEvt msg) -> return $ Just msg
   Nothing -> return Nothing
   _ -> awaitMsg

handleMsg::(MonadIO m, MonadState Context m, HasSQLDB m, MonadLogger m)=>
           Point->Conduit Event m Message
handleMsg peerId' = do
  yield $ Hello {
              version = 4,
              clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
              capability = [ETH ethVersion], -- , SHH shhVersion],
              port = 0,
              nodeId = peerId'
            }

  helloResponse <- awaitMsg

  case helloResponse of
   Just Hello{} -> do
     bestBlock <- lift getBestBlock
     genesisBlockHash <- lift getGenesisBlockHash
     yield Status{
       protocolVersion=fromIntegral ethVersion,
       networkID=if flags_cNetworkID == -1
                 then (if flags_cTestnet then 0 else 1) 
                 else flags_cNetworkID,
                      totalDifficulty=0,
       latestHash=blockHash bestBlock,
       genesisHash=genesisBlockHash
       }
   Just _ -> error "Peer sent message before handshake was complete"
   Nothing -> error "Peer hung up before handshake was complete"
   
  statusResponse <- awaitMsg

  case statusResponse of
   Just Status{latestHash=_, genesisHash=gh} -> do
     genesisBlockHash <- lift getGenesisBlockHash
     when (gh /= genesisBlockHash) $ error "Wrong genesis block hash!!!!!!!!"
     --lastBlockNumber <- liftIO $ fmap (maximum . map (blockDataNumber . blockBlockData)) $ fetchLastBlocks fetchLimit
     let lastBlockNumber = 0
     yield $ GetBlockHeaders (BlockNumber (max (lastBlockNumber - flags_syncBacktrackNumber) 0)) maxReturnedHeaders 0 Forward
   Just _ -> error "Peer sent message before handshake was complete"
   Nothing -> error "Peer hung up before handshake was complete"

  handleEvents





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

bytesToMessages::Monad m=>Conduit B.ByteString m Message
bytesToMessages = forever $ do
  msgTypeData <- cbSafeTake 1
  let word = fromInteger (rlpDecode $ rlpDeserialize msgTypeData::Integer)

  objBytes <- getRLPData
  yield $ obj2WireMessage word $ rlpDeserialize objBytes
          
messagesToBytes::Monad m=>Conduit Message m B.ByteString
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

{-
hPubKeyToPubKey::H.PubKey->Point
hPubKeyToPubKey pubKey = Point (fromIntegral x) (fromIntegral y)
  where
    x = fromMaybe (error "getX failed in prvKey2Address") $ H.getX hPoint
    y = fromMaybe (error "getY failed in prvKey2Address") $ H.getY hPoint
    hPoint = H.pubKeyPoint pubKey
-}
  
runPeer::(MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadThrow m)=>
         String->PortNumber->Point->PrivateNumber->m ()
--runPeer::String->PortNumber->Point->PrivateNumber->LoggingT IO ()
runPeer ipAddress thePort otherPubKey myPriv = do
  logInfoN $ T.pack $ C.blue "Welcome to strato-p2p-client"
  logInfoN $ T.pack $ C.blue "============================"
  logInfoN $ T.pack $ C.green " * " ++ "Attempting to connect to " ++ C.yellow (ipAddress ++ ":" ++ show thePort)

  let myPublic = calculatePublic theCurve myPriv
  logInfoN $ T.pack $ C.green " * " ++ "my pubkey is: " ++ C.yellow (take 30 (format $ pointToByteString myPublic) ++ "...")
  --logInfoN $ T.pack $ "my NodeID is: " ++ (format $ pointToByteString $ hPubKeyToPubKey $ H.derivePubKey $ fromMaybe (error "invalid private number in main") $ H.makePrvKey $ fromIntegral myPriv)

  logInfoN $ T.pack $ C.green " * " ++ "server pubkey is : " ++ C.yellow (take 30 (format $ pointToByteString otherPubKey) ++ "...")

  --cch <- mkCache 1024 "seed"

  runTCPClientWithConnectTimeout (clientSettings (fromIntegral thePort) $ BC.pack ipAddress) 5 $ \server -> 
      runResourceT $ ((do
        pool <- runNoLoggingT $ SQL.createPostgresqlPool
                connStr' 20
      
        _ <- flip runStateT (Context pool [] []) $ ((do
          (_, (outCxt, inCxt)) <-
            transPipe liftIO (appSource server) $$+
            transPipe liftIO (ethCryptConnect myPriv otherPubKey) `fuseUpstream`
            transPipe liftIO (appSink server)

          let --handleError::SomeException->LoggingT IO a
              handleError e = error' (show (e::SomeException))

          eventSource <- mergeSourcesCloseForAny [
            transPipe (flip catch handleError) (appSource server) =$=
            transPipe (flip catch handleError) (ethDecrypt inCxt) =$=
            transPipe (flip catch handleError) bytesToMessages =$=
            transPipe (flip catch handleError) (tap (displayMessage False "")) =$=
            CL.map MsgEvt,
            txNotificationSource =$= CL.map NewTX,
            blockNotificationSource =$= CL.map (flip NewBL 0)
            ] 2

          eventSource =$=
            handleMsg myPublic =$=
            transPipe lift (tap (displayMessage True "")) =$=
            messagesToBytes =$=
            ethEncrypt outCxt $$
            transPipe liftIO (appSink server)))


        return ()))

getPubKeyRunPeer::(MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadThrow m)=>
                  String->PortNumber->Maybe Point->m ()
getPubKeyRunPeer ipAddress thePort maybePubKey = do
  entropyPool <- liftIO createEntropyPool

  let g = cprgCreate entropyPool :: SystemRNG
      (myPriv, _) = generatePrivate g $ getCurveByName SEC_p256k1

  case maybePubKey of
    Nothing -> do
      logInfoN $ T.pack $ "Attempting to connect to " ++ show ipAddress ++ ":" ++ show thePort ++ ", but I don't have the pubkey.  I will try to use a UDP ping to get the pubkey."
      eitherOtherPubKey <- liftIO $ getServerPubKey (fromMaybe (error "invalid private number in main") $ H.makePrvKey $ fromIntegral myPriv) ipAddress thePort
      case eitherOtherPubKey of
            Right otherPubKey -> do
              logInfoN $ T.pack $ "#### Success, the pubkey has been obtained: " ++ (format $ pointToByteString otherPubKey)
              runPeer ipAddress thePort otherPubKey myPriv
            Left e -> logInfoN $ T.pack $ "Error, couldn't get public key for peer: " ++ show e
    Just otherPubKey -> runPeer ipAddress thePort otherPubKey myPriv
                      

runPeerInList::(MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadThrow m)=>
               [(String, PortNumber, Maybe Point)]->Maybe Int->m ()
runPeerInList addresses maybePeerNumber = do
  peerNumber <- case maybePeerNumber of
                  Just x -> return x
                  Nothing -> liftIO $ randomRIO (0, length addresses - 1)

  let (ipAddress, thePort, maybePubKey) = addresses !! peerNumber

  getPubKeyRunPeer ipAddress thePort maybePubKey
               
stratoP2PClient::[String]->LoggingT IO ()    
stratoP2PClient args = do
  let maybePeerNumber =
        case args of
          [] -> Nothing
          [x] -> return $ read x
          _ -> error "usage: ethereumH [servernum]"

  if flags_sqlPeers
    then sequence_ $ repeat $ runPeerInList (map (\peer -> (T.unpack $ pPeerIp peer, fromIntegral $ pPeerPort peer, Just $ pPeerPubkey peer)) ipAddressesDB) maybePeerNumber
    else sequence_ $ repeat $ runPeerInList (map (\peer -> (fst peer, snd peer, Nothing)) ipAddresses) maybePeerNumber

