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
import qualified BTree
import qualified MinIR.PostingList.Reader as Reader
import MinIR.BlobStore as Blob
import MinIR.PostingList.Types

type PostingProducer m = Producer (TermId, Producer DocTerm m ()) m

build :: MonadIO m
      => FilePath
      -> [PostingProducer m ()]
      -> EitherT String m ()
build destDir producers = do
    postings <- liftIO $ Blob.openWriter (destDir++"/postings.blob")
    let --groupedTerms :: Producer [(TermId, Producer DocTerm m ())] m ()
        groupedTerms = groupBy ((==) `on` fst)
                       $ interleave (comparing fst) producers
        -- >-> PP.map mergeDocTerms >-> writeDocTerms postings
    return ()

mergeDocTerms :: Monad m => [Producer DocTerm m ()] -> Producer DocTerm m ()
mergeDocTerms = merge (comparing docId) addDocTerms
  where addDocTerms :: Monad m => DocTerm -> DocTerm -> m DocTerm
        addDocTerms (DocTerm n1 doc) (DocTerm n2 _) = return $ DocTerm (n1+n2) doc
        docId :: DocTerm -> DocumentId
        docId (DocTerm _ d) = d
