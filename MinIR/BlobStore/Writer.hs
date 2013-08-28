module MinIR.BlobStore.Writer ( StoreWriter
                              , StoreRef
                              , open
                              , store
                              ) where

import MinIR.BlobStore.Types
import System.IO
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Pipes
import qualified Data.ByteString.Lazy as LBS
import Data.Binary

data StoreWriter = StoreWriter { storeHandle    :: !Handle
                               , storeWriteLock :: !(TMVar ())
                               }

open :: MonadIO m => FilePath -> m StoreWriter
open fname = liftIO $ do
    h <- openFile fname AppendMode
    writeLock <- newTMVarIO ()
    return $ StoreWriter h writeLock

store :: MonadIO m
      => StoreWriter
      -> Producer LBS.ByteString m ()
      -> m StoreRef
store f producer = do
    liftIO $ atomically $ takeTMVar lock
    startPos <- liftIO $ hTell h
    go (fromIntegral startPos) producer
  where go :: MonadIO m => Offset -> Producer LBS.ByteString m () -> m StoreRef
        go startPos prod = do
          a <- next prod
          case a of
            Left ()          -> do endPos <- liftIO $ hTell h
                                   liftIO $ LBS.hPut h $ encode
                                       $ SInfo startPos (fromIntegral endPos - startPos)
                                   liftIO $ atomically $ putTMVar lock ()
                                   return $ SRef (fromIntegral endPos)
            Right (a, prod') -> do liftIO $ LBS.hPut h a
                                   go startPos prod'
        h = storeHandle f
        lock = storeWriteLock f
