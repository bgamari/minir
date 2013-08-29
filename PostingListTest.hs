{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Error
import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.Foldable.Strict as F
import Data.Binary    
import qualified Data.Map as M    
import Control.Lens hiding (each)
import GHC.Generics
import qualified MinIR.PostingList as PL
import MinIR.FreqMap as FM
import MinIR.TermIndex as TI
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Prelude as PP
import System.Random.MWC
import Data.Monoid

newtype DocId = DocId Int deriving (Show, Ord, Eq, Generic, Enum)
instance Binary DocId
newtype TermId = TermId Int deriving (Show, Ord, Eq, Generic, Enum)
instance Binary TermId

termIdx :: TermIndex DocId TermId
termIdx =
    F.foldMap' (\(d,t)->TI.fromTerm (DocId d) (TermId t))
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
    
randomTermIndex :: DocId -> TermId -> Int -> IO (TermIndex DocId TermId)
randomTermIndex docs terms draws =
    withSystemRandom $ asGenIO $ \mwc->do
      foldM (draw mwc) mempty [1..draws]
  where draw mwc tidx _ = do
          doc <- uniformR (0, fromEnum docs) mwc
          term <- uniformR (0, fromEnum terms) mwc
          return $! tidx `mappend` TI.fromTerm (toEnum doc) (toEnum term)

main = print =<< runEitherT go

go = do
    termIndex <- liftIO $ randomTermIndex (DocId 50000) (TermId 10000) 2000000
    a <- PL.build "hello" [TI.termIndexToProducer termIndex]
    liftIO $ print a

    pl <- PL.open "hello" :: EitherT String IO (PL.PostingList DocId TermId)
    l <- hoistEither $ note "Lookup failed" $ PL.lookup pl (TermId 0)
    liftIO $ print $ PP.toList (void l)
    return ()
