module MinIR.PostingList.Builder ( build
                                 , PostingProducer
                                 ) where

import Data.Function (on)
import Data.Ord (comparing)
import Control.Monad.IO.Class
import Control.Error
import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Interleave
import Data.Binary
import qualified BTree
import qualified MinIR.PostingList.Reader as Reader
import MinIR.BlobStore as Blob
import MinIR.PostingList.Types

-- | A 'Producer' generating pairs of terms and a 'Producer' emitting
-- 'DocTerms'.
--
-- The 'Terms' must be emitted in order, as must the 'DocTerms'.
type PostingProducer m = Producer (TermId, Producer DocTerm m ()) m

-- | Build a posting list given a set of 'PostingProducers'. Each of these
-- must be annotated with a size 
build :: MonadIO m
      => FilePath
      -> [(Int, PostingProducer m ())]
      -> EitherT String m ()
build destDir sizedProducers = do
    postingStore <- liftIO $ Blob.openWriter (destDir++"/postings.blob")
    let groupTerms :: Monad m
                   => [PostingProducer m ()]
                   -> Producer [(TermId, Producer DocTerm m ())] m ()
        groupTerms = groupBy ((==) `on` fst) . interleave (comparing fst)

    let producers = map snd sizedProducers
        size = fromIntegral $ sum $ map fst sizedProducers
        writtenTerms = mapMP (accumAndWriteTerms postingStore) (groupTerms producers)
    lift $ BTree.fromOrderedToFile 100 size (destDir++"/index.btree") writtenTerms

accumAndWriteTerms :: MonadIO m
                   => StoreWriter
                   -> [(TermId, Producer DocTerm m ())]
                   -> m (BTree.BLeaf TermId StoreRef)
accumAndWriteTerms postingStore postings@((term,_):_) = do
    sref <- store postingStore $ mergeDocTerms (map snd postings) >-> PP.map encode
    return $ BTree.BLeaf term sref
                
mapMP :: Monad m => (a -> m b) -> Producer a m r -> Producer b m r
mapMP f = go
  where go prod = do
          r <- lift $ next prod
          case r of
            Left a           -> return a
            Right (x, prod') -> do lift (f x) >>= yield >> go prod'
            

mergeDocTerms :: Monad m => [Producer DocTerm m ()] -> Producer DocTerm m ()
mergeDocTerms = merge (comparing docId) addDocTerms
  where addDocTerms :: Monad m => DocTerm -> DocTerm -> m DocTerm
        addDocTerms (DocTerm n1 doc) (DocTerm n2 _) = return $ DocTerm (n1+n2) doc
        docId :: DocTerm -> DocumentId
        docId (DocTerm _ d) = d
