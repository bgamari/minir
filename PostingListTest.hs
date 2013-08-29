{-# LANGUAGE DeriveGeneric #-}            

import Control.Monad
import Control.Monad.IO.Class
import Control.Error
import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.Foldable as F
import Data.Binary    
import qualified Data.Map as M    
import Control.Lens hiding (each)
import GHC.Generics
import qualified MinIR.PostingList as PL
import MinIR.FreqMap as FM
import MinIR.TermIndex as TI
import Pipes
import qualified Pipes.Prelude as PP

newtype DocId = DocId Int deriving (Show, Ord, Eq, Generic)
instance Binary DocId
newtype TermId = TermId Int deriving (Show, Ord, Eq, Generic)
instance Binary TermId

termIdx :: TermIndex DocId TermId
termIdx =
    F.foldMap (\(d,t)->TI.fromTerm (DocId d) (TermId t))
    [ (0, 0)
    , (0, 1)
    , (0, 3)
    , (1, 1)
    , (1, 2)
    , (1, 4)
    , (2, 0)
    , (2, 2)
    , (2, 5)
    ]

termIndexToProducer :: (Monad m, Ord term)
                    => TermIndex doc term
                    -> (Int, PL.PostingProducer doc term m ())
termIndexToProducer tidx = (TI.termsSize tidx, producer)
  where producer = each $ map (\(term,freqMap)->(term, postingProducer freqMap))
                   $ M.assocs (tidx ^. tFreq)
        postingProducer = each . map (\(doc,n)->PL.Posting n doc)
                               . M.assocs . view FM.fFreqs

main = print =<< runEitherT go

go = do
    a <- PL.build "hello" [termIndexToProducer termIdx]
    liftIO $ print a

    pl <- PL.open "hello" :: EitherT String IO (PL.PostingList DocId TermId)
    l <- hoistEither $ note "Lookup failed" $ PL.lookup pl (TermId 0)
    liftIO $ print $ PP.toList (void l)
    return ()
