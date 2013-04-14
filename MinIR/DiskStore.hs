{-# LANGUAGE DeriveGeneric #-}

module MinIR.DiskStore ( Obj
                       , DiskStore
                       -- * Creating disk stores
                       , fromHandle
                       , new
                       , open
                       , close
                       -- * Manipulating the root object
                       , getRoot
                       , setRoot
                       -- * Adding and retriving objects
                       , getObj
                       , appendObj
                       ) where

import Control.Concurrent.MVar
import Control.Applicative
import GHC.Generics
import System.IO
import System.Directory
import Control.Monad

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

newtype Obj a = Obj Word64
              deriving (Show, Eq, Ord, Generic)

instance Binary (Obj a)

withReadLock :: DiskStore root -> IO b -> IO b
withReadLock dstore m = withMVar (readLock dstore) (const m)

getObj :: Binary a => DiskStore root -> Obj a -> IO a
getObj dstore (Obj offset) = withReadLock dstore $ do
    hSeek (handle dstore) AbsoluteSeek (fromIntegral offset)
    a <- BSL.hGetContents (handle dstore)
    return $! decode a

appendObj :: Binary a => DiskStore root -> a -> IO (Obj a)
appendObj dstore a = withReadLock dstore $ do
    let DStore {handle=h} = dstore
    hSeek h SeekFromEnd 0
    offset <- hTell h
    BSL.hPut h $ encode a
    return $ Obj $ fromIntegral offset

headerMagic = 0xdeadbeef :: Word64

data Header a = Header { magic :: !Word64
                       , root  :: !(Maybe (Obj a))
                       }
              deriving (Generic)

instance Binary (Header a) where
    put h = do putWord64le (magic h)
               case root h of
                 Just a  -> putWord8 1 >> put a
                 Nothing -> putWord8 0 >> put (Obj 0)
    get = Header <$> getWord64le <*> get

data DiskStore a = DStore { handle :: !Handle
                          , readLock :: !(MVar ())
                          }

headerObj = Obj 0 :: Obj (Header a)

getHeader :: DiskStore a -> IO (Maybe (Header a))
getHeader dstore = do
    header <- getObj dstore headerObj
    if magic header == headerMagic
      then return $ Just header
      else return Nothing

putHeader :: DiskStore a -> Header a -> IO ()
putHeader dstore header = withReadLock dstore $ do
    let DStore {handle=h} = dstore
    hSeek h AbsoluteSeek 0
    BSL.hPut h $ encode header

getRoot :: Binary a => DiskStore a -> IO (Maybe a)
getRoot dstore = do
    header <- getHeader dstore
    case header of
      Just hdr -> case root hdr of
                    Just o  -> Just <$> getObj dstore o
                    Nothing -> return Nothing
      Nothing  -> fail "Invalid header"

setRoot :: DiskStore a -> Maybe (Obj a) -> IO (Maybe ())
setRoot dstore root = do
    header <- getHeader dstore
    case header of
      Just hdr -> Just <$> putHeader dstore (hdr {root=root})
      Nothing  -> return Nothing

fromHandle :: Handle -> IO (Maybe (DiskStore a))
fromHandle h = do
    readLock <- newMVar ()
    let dstore = DStore h readLock
    header <- getHeader dstore
    case header of
      Just _  -> return $ Just dstore
      Nothing -> return Nothing

new :: Binary a => FilePath -> IO (DiskStore a)
new fname = do
    exists <- doesFileExist fname
    when exists $ fail "DiskStore already exists"
    h <- openBinaryFile fname ReadWriteMode
    readLock <- newMVar ()
    let dstore = DStore h readLock
    putHeader dstore $ Header headerMagic Nothing
    return dstore

open :: FilePath -> IO (Maybe (DiskStore a))
open fname = do
    openBinaryFile fname ReadWriteMode >>= fromHandle

close :: DiskStore a -> IO ()
close dstore = hClose (handle dstore)
