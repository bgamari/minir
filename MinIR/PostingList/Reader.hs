module MinIR.PostingList.Reader ( PostingList
                                , open
                                , lookup
                                , toProducer
                                ) where

import Prelude hiding (lookup)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Error
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS

import Pipes
import qualified Pipes.Prelude as PP
import qualified BTree
import MinIR.PostingList.Types
import qualified MinIR.BlobStore.Reader as Blob
import qualified MinIR.Stream as Stream

data PostingList doc term = PL { plIndex     :: BTree.LookupTree term Blob.StoreRef
                               , plPostings  :: Blob.StoreReader
                               }

open :: FilePath -> EitherT String IO (PostingList doc term)
open dir = do
    index <- EitherT $ BTree.open (dir++"/index.btree")
    postings <- Blob.open (dir++"/postings.blob")
    return $ PL index postings

lookup :: (Monad m, Ord term, Binary term, Binary doc)
       => PostingList doc term
       -> term
       -> Maybe (Producer (Posting doc) m (Either String ()))
lookup pl term =
    case BTree.lookup (plIndex pl) term of
      Nothing   -> Nothing
      Just sref -> either (error . show) Just  $ produceDocTerms pl sref

-- | Produce
toProducer :: (Monad m, Binary term, Binary doc)
           => PostingList doc term
           -> Producer (term, Producer (Posting doc) m (Either String ())) m ()
toProducer pl =
    void (BTree.walkLeaves (plIndex pl))
    >-> PP.map f
  where f (BTree.BLeaf term sref) =
            case produceDocTerms pl sref of
              Left _      -> error "PostingList.Reader.toProducer: This should never happen"
              Right prod  -> (term, prod)

produceDocTerms :: (Monad m, Binary doc)
                => PostingList doc term
                -> Blob.StoreRef
                -> Either Blob.FetchError (Producer (Posting doc) m (Either String ()))
produceDocTerms pl sref = do
    bs <- Blob.fetch (plPostings pl) sref
    return $ fmap (fmapR (const ())) $ readBinary $ LBS.fromStrict bs

-- | Return a 'Producer' producing elements decoded with the 'Binary' instance
readBinary :: (Monad m, Binary a)
           => LBS.ByteString -> Producer a m (Either String LBS.ByteString)
readBinary = go
  where go bs =
            case runGetOrFail get bs of
              Left (rest,_,err)  -> return (Left err)
              Right (rest,o,a)   -> do
                yield a
                if LBS.null rest
                  then return $ Right rest
                  else go rest
