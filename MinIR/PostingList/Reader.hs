module MinIR.PostingList.Reader ( PostingListReader
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
import MinIR.BlobStore.Reader as Blob
import qualified MinIR.Stream as Stream

data PostingListReader = PLR { plrIndex     :: BTree.LookupTree TermId StoreRef
                             , plrPostings  :: Blob.StoreReader
                             }

open :: FilePath -> EitherT String IO PostingListReader
open dir = do
    index <- EitherT $ BTree.open (dir++"/index.btree")
    postings <- Blob.open (dir++"/postings.blob")
    return $ PLR index postings

lookup :: Monad m
       => PostingListReader -> TermId -> Maybe (Producer DocTerm m (Either String ()))
lookup plr term =
    case BTree.lookup (plrIndex plr) term of
      Nothing   -> Nothing
      Just sref -> hush $ produceDocTerms plr sref

-- | Produce
toProducer :: Monad m
           => PostingListReader
           -> Producer (TermId, Producer DocTerm m (Either String ())) m ()
toProducer plr =
    void (BTree.walkLeaves (plrIndex plr))
    >-> PP.map f
  where f (BTree.BLeaf term sref) =
            case produceDocTerms plr sref of
              Left _      -> error "PostingList.Reader.toProducer: This should never happen"
              Right prod  -> (term, prod)

produceDocTerms :: Monad m
                => PostingListReader -> StoreRef
                -> Either Blob.FetchError (Producer DocTerm m (Either String ()))
produceDocTerms plr sref = do
    bs <- Blob.fetch (plrPostings plr) sref
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
