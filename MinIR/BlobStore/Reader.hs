module MinIR.BlobStore.Reader ( StoreReader
                              , StoreRef
                              , open
                              , fetch
                              , FetchError(..)
                              ) where

import Control.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.IO.MMap
import Data.Binary
import Data.Binary.Get
import Control.Monad (when)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import MinIR.BlobStore.Types

data StoreReader = StoreReader { storeMap   :: !BS.ByteString
                               }

open :: MonadIO m => FilePath -> EitherT String m StoreReader
open fname = do
    bs <- liftIO $ mmapFileByteString fname Nothing
    return $ StoreReader bs

data FetchError = BeyondEnd
                | InvalidEncoding String
                deriving (Show)

fetch :: StoreReader -> StoreRef -> Either FetchError BS.ByteString
fetch (StoreReader bs) (SRef offset) = do
    when (fromIntegral offset >= BS.length bs) $ Left BeyondEnd
    sinfo <- case runGetOrFail get $ LBS.fromStrict $ BS.drop (fromIntegral offset) bs of
               Left (_,_,e)   -> Left (InvalidEncoding e)
               Right (_,_,a)  -> Right a
    return $ BS.take (fromIntegral $ storeLength sinfo)
           $ BS.drop (fromIntegral $ storeStart sinfo) bs
