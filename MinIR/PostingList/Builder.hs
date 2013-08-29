module MinIR.PostingList.Builder ( build
                                 , PostingProducer
                                 ) where

import Data.Function (on)
import Data.Ord (comparing)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Error
import System.Directory
import Data.Binary

import Pipes
import Pipes.Interleave
import qualified Pipes.Prelude as PP
import qualified BTree
import qualified MinIR.PostingList.Reader as Reader
import MinIR.BlobStore as Blob
import MinIR.PostingList.Types

-- | A 'Producer' generating pairs of terms and a 'Producer' emitting
-- 'Postings'.
--
-- The terms must be emitted in order, as must the 'Postings'.
type PostingProducer doc term m = Producer (term, Producer (Posting doc) m ()) m

-- | Build a posting list given a set of 'PostingProducers'. Each of these
-- must be annotated with a size 
build :: (MonadIO m, Binary term, Ord term, Binary doc, Ord doc)
      => FilePath
      -> [(Int, PostingProducer doc term m ())]
      -> EitherT String m ()
build destDir sizedProducers = do
    exists <- liftIO $ doesDirectoryExist destDir
    when exists $ left "PostingList.Builder: destination already exists"
    liftIO $ createDirectoryIfMissing True destDir
    postingStore <- liftIO $ Blob.openWriter (destDir++"/postings.blob")
    let groupTerms :: (Monad m, Ord term)
                   => [PostingProducer doc term m ()]
                   -> Producer [(term, Producer (Posting doc) m ())] m ()
        groupTerms = groupBy ((==) `on` fst) . interleave (comparing fst)

    let producers = map snd sizedProducers
        size = fromIntegral $ sum $ map fst sizedProducers
        writtenTerms = mapMP (accumAndWriteTerms postingStore) (groupTerms producers)
    lift $ BTree.fromOrderedToFile 100 size (destDir++"/index.btree") writtenTerms
    Blob.closeWriter postingStore

accumAndWriteTerms :: (MonadIO m, Ord doc, Binary doc)
                   => StoreWriter
                   -> [(term, Producer (Posting doc) m ())]
                   -> m (BTree.BLeaf term StoreRef)
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
            

mergeDocTerms :: (Monad m, Ord doc)
              => [Producer (Posting doc) m ()]
              -> Producer (Posting doc) m ()
mergeDocTerms = mergeM (comparing docId) addDocTerms
  where addDocTerms :: Monad m => Posting doc -> Posting doc -> m (Posting doc)
        addDocTerms (Posting n1 doc) (Posting n2 _) = return $ Posting (n1+n2) doc
        docId :: Posting doc -> doc
        docId (Posting _ d) = d
