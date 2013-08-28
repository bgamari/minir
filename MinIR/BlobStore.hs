module MinIR.BlobStore ( 
                         StoreRef
                         -- * Reading
                       , StoreReader
                       , openReader
                       , fetch
                         -- * Writing
                       , StoreWriter
                       , openWriter
                       , store
                       ) where

import Control.Error
import MinIR.BlobStore.Reader
import MinIR.BlobStore.Writer
import Control.Monad.IO.Class

openReader :: MonadIO m => FilePath -> EitherT String m StoreReader
openReader = MinIR.BlobStore.Reader.open

openWriter :: MonadIO m => FilePath -> m StoreWriter
openWriter = MinIR.BlobStore.Writer.open
