module MinIR.BlobStore.Types where

import Data.Binary
import Control.Monad
import Control.Applicative

type Offset = Word64

newtype StoreRef = SRef Offset
                 deriving (Show, Eq, Ord)
                 
instance Binary StoreRef where
    get = SRef <$> get
    put (SRef o) = put o

magic :: Word64        
magic = 0xfeed1234dadaffff

data StoreInfo = SInfo { storeStart    :: Offset
                       , storeLength   :: Word64
                       }

instance Binary StoreInfo where
    get = do m <- get
             when (m /= magic) $ fail "BlobStore: Invalid StoreInfo magic number"
             SInfo <$> get <*> get 
    put (SInfo s l) = put magic >> put s >> put l
