{-# LANGUAGE OverloadedStrings #-}

module DB.EthDB (
  --showAllKeyVal,
  SHAPtr(..),
  blankRoot,
  isBlankDB,
  getKeyVals,
  putKeyVal,
) where

import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Function
import Data.Functor
import Data.List
import qualified Database.LevelDB as DB
import qualified Data.Map as M
import Numeric

import Colors
import Data.RLP
import DB.DBs
import Format
import qualified Data.NibbleString as N
import SHA

--import Debug.Trace

{-
showAllKeyVal::DB->ResourceT IO ()
showAllKeyVal db = do
  i <- DB.iterOpen (stateDB db) def
  DB.iterFirst i
  valid <- DB.iterValid i
  if valid
    then showAllKeyVal' i
    else liftIO $ putStrLn "no keys"
  where
    showAllKeyVal'::DB.Iterator->ResourceT IO ()
    showAllKeyVal' i = do
      Just key <- DB.iterKey i
      Just val <- (getNodeData db{stateRoot=SHAPtr key})::ResourceT IO (Maybe (NodeData B.ByteString))
      --Just byteStringValue <- DB.iterValue i
      --let val = (rlpDecode $ rlpDeserialize $ byteStringValue)::NodeData
      liftIO $ putStrLn $ "----------\n" ++ format (SHAPtr key)
      liftIO $ putStrLn $ format val
      DB.iterNext i
      v <- DB.iterValid i
      if v
        then showAllKeyVal' i
        else return ()
-}

{-
sha2SHAPtr::SHA->SHAPtr
sha2SHAPtr (SHA x) = SHAPtr $ B.pack $ word256ToBytes x
-}

blankRoot::SHAPtr
blankRoot = sha2SHAPtr (hash "")

isBlankDB::SHAPtr->Bool
isBlankDB x | blankRoot == x = True
isBlankDB _ = False

getNodeData::DB->ResourceT IO (Maybe NodeData)
getNodeData db@DB{stateRoot=SHAPtr p} = do
  fmap bytes2NodeData <$> DB.get (stateDB db) def p
        where
          bytes2NodeData::B.ByteString->NodeData
          bytes2NodeData bytes | B.null bytes = EmptyNodeData
          bytes2NodeData bytes = rlpDecode $ rlpDeserialize bytes


pairOrPtr2NodeData::DB->PairOrPtr->ResourceT IO (Maybe NodeData)
pairOrPtr2NodeData db (APair key val) = return $ Just $ ShortcutNodeData key $ Right val
pairOrPtr2NodeData db (APtr p) = getNodeData db{stateRoot=p}


pairOrPtr2KeyVals::DB->PairOrPtr->N.NibbleString->ResourceT IO [(N.NibbleString, RLPObject)]
pairOrPtr2KeyVals db (APair key val) key' | key' `N.isPrefixOf` key = return [(key, val)]
pairOrPtr2KeyVals db (APtr p) key = getKeyVals db{stateRoot = p} key


getKeyVals::DB->N.NibbleString->ResourceT IO [(N.NibbleString, RLPObject)]
getKeyVals db key = do
  maybeNodeData <- getNodeData db
  let nodeData =case maybeNodeData of
                  Nothing -> error $ "Error calling getKeyVals, stateRoot doesn't exist: " ++ format (stateRoot db)
                  Just x -> x
  nextVals <- 
    case nodeData of
      FullNodeData {choices=cs} -> do
        if N.null key
          then concat <$> sequence [fmap (prependToKey (N.singleton nextN)) <$> pairOrPtr2KeyVals db pairOrPtr "" | (nextN, Just pairOrPtr) <- zip [0..] cs]
          else case cs!!fromIntegral (N.head key) of
          Just pairOrPtr -> fmap (prependToKey $ N.singleton $ N.head key) <$> pairOrPtr2KeyVals db pairOrPtr (N.tail key)
          Nothing -> return []
      ShortcutNodeData{nextNibbleString=s,nextVal=Right v} | key `N.isPrefixOf` s ->
        return [(s, v)]
      ShortcutNodeData{nextNibbleString=s,nextVal=Left nextP} | key `N.isPrefixOf` s -> 
        fmap (prependToKey s) <$> getKeyVals db{stateRoot=nextP} ""
      ShortcutNodeData{nextNibbleString=s,nextVal=Left nextP} | s `N.isPrefixOf` key ->
        fmap (prependToKey s) <$> getKeyVals db{stateRoot=nextP} (N.drop (N.length s) key)
      _ -> return []
  case (N.null key, nodeData) of
    (True, FullNodeData{nodeVal = Just v}) -> return (("", v):nextVals)
    _ -> return nextVals

nodeDataSerialize::NodeData->B.ByteString
nodeDataSerialize EmptyNodeData = B.empty
nodeDataSerialize x = rlpSerialize $ rlpEncode x

putNodeData::DB->NodeData->ResourceT IO SHAPtr
putNodeData db nd = do
  let bytes = nodeDataSerialize nd
      ptr = C.hash 256 bytes
  DB.put (stateDB db) def ptr bytes
  return $ SHAPtr ptr

slotIsEmpty::[Maybe PairOrPtr]->N.Nibble->Bool
slotIsEmpty [] _ = error ("slotIsEmpty was called for value greater than the size of the list")
slotIsEmpty (Nothing:_) 0 = True
slotIsEmpty _ 0 = False
slotIsEmpty (_:rest) n = slotIsEmpty rest (n-1)

replace::Integral i=>[a]->i->a->[a]
replace list i newVal = left ++ [newVal] ++ right
            where
              (left, _:right) = splitAt (fromIntegral i) list

list2Options::N.Nibble->[(N.Nibble, PairOrPtr)]->[Maybe PairOrPtr]
list2Options start _ | start > 15 = error $ "value of 'start' in list2Option is greater than 15, it is: " ++ show start
list2Options start [] = replicate (fromIntegral $ 0x10 - start) Nothing
list2Options start ((firstNibble, firstPtr):rest) =
    replicate (fromIntegral $ firstNibble - start) Nothing ++ [Just firstPtr] ++ list2Options (firstNibble+1) rest

getCommonPrefix::Eq a=>[a]->[a]->([a], [a], [a])
getCommonPrefix (c1:rest1) (c2:rest2) | c1 == c2 = prefixTheCommonPrefix c1 (getCommonPrefix rest1 rest2)
                                      where
                                        prefixTheCommonPrefix c (p, x, y) = (c:p, x, y)
getCommonPrefix x y = ([], x, y)

newShortcut::DB->N.NibbleString->Either SHAPtr RLPObject->ResourceT IO PairOrPtr
newShortcut _ key (Right val) | 32 > B.length bytes = return $ APair key val
                      where 
                        key' = termNibbleString2String True key
                        bytes = rlpSerialize $ RLPArray [rlpEncode $ BC.unpack key', val]
newShortcut db key val = APtr <$> putNodeData db (ShortcutNodeData key val)


getNewNodeDataFromPut::DB->N.NibbleString->RLPObject->NodeData->ResourceT IO NodeData
getNewNodeDataFromPut _ key val EmptyNodeData = return $
  ShortcutNodeData key $ Right val

getNewNodeDataFromPut db key val (FullNodeData options nodeValue)
  | options `slotIsEmpty` N.head key = do
  tailNode <- newShortcut db (N.tail key) $ Right val
  return $ FullNodeData (replace options (N.head key) $ Just tailNode) nodeValue
getNewNodeDataFromPut db key val (FullNodeData options nodeValue) = do
  let Just conflictingNode = options!!fromIntegral (N.head key)
  --TODO- add nicer error message if stateRoot doesn't exist
  Just conflictingNodeData <- (pairOrPtr2NodeData db conflictingNode::ResourceT IO (Maybe NodeData))
  newNodeData <- getNewNodeDataFromPut db (N.tail key) val conflictingNodeData
  newNode <- APtr <$> putNodeData db newNodeData
  return $ FullNodeData (replace options (N.head key) $ Just newNode) nodeValue

getNewNodeDataFromPut _ key1 val (ShortcutNodeData key2 (Right _)) | key1 == key2 =
  return $ ShortcutNodeData key1 $ Right val
--getNewNodeDataFromPut _ key1 val (ShortcutNodeData key2 (Left _)) | key1 == key2 =
getNewNodeDataFromPut _ key1 _ (ShortcutNodeData key2 (Left _)) | key1 == key2 =
  error "getNewNodeDataFromPut not defined for shortcutnodedata with ptr"
--getNewNodeDataFromPut db key1 val1 (ShortcutNodeData key2 val2) | N.null key1 = do
getNewNodeDataFromPut _ key1 val1 (ShortcutNodeData k (Right _)) | N.null key1 = do
  return $ ShortcutNodeData k $ Right val1
  {-
  node1 <- putNodeData db $ ShortcutNodeData (N.drop (N.length key1) key2) val2
  let options = list2Options 0 [(N.head $ N.drop (N.length key1) key2, node1)]
  midNode <- putNodeData db $ FullNodeData options $ Just val1
  return $ ShortcutNodeData key1 $ Left midNode
-}
getNewNodeDataFromPut db key1 val1 (ShortcutNodeData key2 val2) | key1 `N.isPrefixOf` key2 = do
  node1 <- newShortcut db (N.drop (N.length key1) key2) val2
  let options = list2Options 0 [(N.head $ N.drop (N.length key1) key2, node1)]
  midNode <- putNodeData db $ FullNodeData options $ Just val1
  return $ ShortcutNodeData key1 $ Left midNode
getNewNodeDataFromPut db key1 val1 (ShortcutNodeData key2 (Right val2)) | key2 `N.isPrefixOf` key1 = do
  node1 <- newShortcut db (N.drop (N.length key2) key1) $ Right val1
  let options = list2Options 0 [(N.head $ N.drop (N.length key2) key1, node1)]
  midNode <- putNodeData db $ FullNodeData options $ Just val2
  return $ ShortcutNodeData key2 $ Left midNode
getNewNodeDataFromPut db key1 val1 (ShortcutNodeData key2 (Left val2)) | key2 `N.isPrefixOf` key1 = do
  Just nodeData <- getNodeData db{stateRoot=val2}
  newNodeData <- getNewNodeDataFromPut db (N.drop (N.length key2) key1) val1 nodeData 
  newNode <- putNodeData db newNodeData
  return $ ShortcutNodeData key2 $ Left newNode
getNewNodeDataFromPut db key1 val1 (ShortcutNodeData key2 val2) | N.head key1 == N.head key2 = do
  node1 <- newShortcut db (N.pack $ tail suffix1) $ Right val1
  node2 <- newShortcut db (N.pack $ tail suffix2) val2
  let options = list2Options 0 (sortBy (compare `on` fst) [(head suffix1, node1), (head suffix2, node2)])
  midNode <- putNodeData db $ FullNodeData options Nothing
  return $ ShortcutNodeData (N.pack commonPrefix) $ Left midNode
      where
        (commonPrefix, suffix1, suffix2) = getCommonPrefix (N.unpack key1) (N.unpack key2)
getNewNodeDataFromPut db key1 val1 (ShortcutNodeData key2 val2) = do
  tailNode1 <- newShortcut db (N.tail key1) $ Right val1
  tailNode2 <- newShortcut db (N.tail key2) val2
  return $ FullNodeData
      (list2Options 0 (sortBy (compare `on` fst) [(N.head key1, tailNode1), (N.head key2, tailNode2)]))
      Nothing

--getNewNodeDataFromPut _ key _ nd = error ("Missing case in getNewNodeDataFromPut: " ++ format nd ++ ", " ++ format key)

putKeyVal::DB->N.NibbleString->RLPObject->ResourceT IO DB
putKeyVal db key val = do
  --TODO- add nicer error message if stateRoot doesn't exist
  Just curNodeData <- getNodeData db
  nextNodeData <- getNewNodeDataFromPut db key val curNodeData
  let k = C.hash 256 $ nodeDataSerialize nextNodeData 
  DB.put (stateDB db) def k $ nodeDataSerialize nextNodeData
  return db{stateRoot=SHAPtr k}

prependToKey::N.NibbleString->(N.NibbleString, RLPObject)->(N.NibbleString, RLPObject)
prependToKey prefix (key, val) = (prefix `N.append` key, val)

data PairOrPtr = APair N.NibbleString RLPObject | APtr SHAPtr deriving (Show)

instance Format PairOrPtr where
    format (APtr x) = "Ptr: " ++ format x
    format (APair key val) = "Pair: " ++ format key ++ ": " ++ format val

data NodeData =
  EmptyNodeData |
  FullNodeData {
    --choices::M.Map N.Nibble (Maybe PairOrPtr),
    choices::[Maybe PairOrPtr],
    nodeVal::Maybe RLPObject
  } |
  ShortcutNodeData {
    nextNibbleString::N.NibbleString,
    nextVal::Either SHAPtr RLPObject
  } deriving Show

formatVal::Maybe RLPObject->String
formatVal Nothing = red "NULL"
formatVal (Just x) = green (format x)

instance Format NodeData where
  format EmptyNodeData = "    <EMPTY>"
  format (ShortcutNodeData s (Left p)) = "    " ++ format s ++ " -> " ++ format p
  format (ShortcutNodeData s (Right val)) = "    " ++ format s ++ " -> " ++ green (format val)
  format (FullNodeData cs val) = "    val: " ++ formatVal val ++ "\n        " ++ intercalate "\n        " (showChoice <$> zip ([0..]::[Int]) cs)
    where
      showChoice (v, Just p) = blue (showHex v "") ++ ": " ++ green (format p)
      showChoice (v, Nothing) = blue (showHex v "") ++ ": " ++ red "NULL"

string2TermNibbleString::String->(Bool, N.NibbleString)
string2TermNibbleString [] = error "string2TermNibbleString called with empty String"
string2TermNibbleString (c:rest) = 
  (terminator, s)
  where
    w = c2w c
    (flags, extraNibble) = if w > 0xF then (w `shiftR` 4, 0xF .&. w) else (w, 0)
    terminator = flags `shiftR` 1 == 1
    oddLength = flags .&. 1 == 1
    s = if oddLength then N.OddNibbleString extraNibble (BC.pack rest) else N.EvenNibbleString (BC.pack rest)

termNibbleString2String::Bool->N.NibbleString->B.ByteString
termNibbleString2String terminator s = 
  case s of
    (N.EvenNibbleString s') -> B.singleton (extraNibble `shiftL` 4) `B.append` s'
    (N.OddNibbleString n rest) -> B.singleton (extraNibble `shiftL` 4 + n) `B.append` rest
  where
    {-
    nibbleString2String::N.NibbleString->String
    nibbleString2String (N.OddNibbleString c s) = w2c c:BC.unpack s
    nibbleString2String (N.EvenNibbleString s) = BC.unpack s
    -}
    extraNibble =
        (if terminator then 2 else 0) +
        (if odd $ N.length s then 1 else 0)

instance RLPSerializable NodeData where
  rlpEncode EmptyNodeData = error "rlpEncode should never be called on EmptyNodeData.  Use rlpSerialize instead."
  rlpEncode (FullNodeData {choices=cs, nodeVal=val}) = RLPArray ((encodeChoice <$> cs) ++ [encodeVal val])
    where
      encodeChoice Nothing = rlpEncode (0::Integer)
      encodeChoice (Just (APtr (SHAPtr x))) = rlpEncode x
      encodeChoice (Just (APair key val)) = RLPArray [rlpEncode $ BC.unpack key', val]
          where 
            key' = termNibbleString2String True key
      encodeVal::Maybe RLPObject->RLPObject
      encodeVal Nothing = rlpEncode (0::Integer)
      encodeVal (Just x) = x
  rlpEncode (ShortcutNodeData {nextNibbleString=s, nextVal=val}) = 
    RLPArray[rlpEncode $ BC.unpack $ termNibbleString2String terminator s, encodeVal val] 
    where
      terminator = 
        case val of
          Left _ -> False
          Right _ -> True
      encodeVal (Left x) = rlpEncode x
      encodeVal (Right x) = x



  rlpDecode (RLPArray [a, val]) = 
    if terminator
    then ShortcutNodeData s $ Right val
    else ShortcutNodeData s (Left $ SHAPtr (BC.pack $ rlpDecode val))
    where
      (terminator, s) = string2TermNibbleString $ rlpDecode a
  rlpDecode (RLPArray x) | length x == 17 =
    FullNodeData (fmap getPtr <$> (\p -> case p of RLPScalar 0 -> Nothing; RLPString "" -> Nothing; _ -> Just p) <$> childPointers) val
    where
      childPointers = init x
      val = case last x of
        RLPScalar 0 -> Nothing
        RLPString "" -> Nothing
        x' -> Just x'
      getPtr::RLPObject->PairOrPtr
      getPtr (RLPArray [key, val]) = APair (snd $ string2TermNibbleString $ rlpDecode key) val
      getPtr p = APtr $ SHAPtr $ rlpDecode p
  rlpDecode x = error ("Missing case in rlpDecode for NodeData: " ++ format x)
