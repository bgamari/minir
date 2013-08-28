module MinIR.PostingList.Types where

import Data.Binary
import Control.Applicative

newtype DocumentId = DocId Word64
                   deriving (Show, Eq, Ord)

instance Binary DocumentId where
    get = DocId <$> get
    put (DocId i) = put i

newtype TermId = TermId Word64
               deriving (Show, Eq, Ord)

instance Binary TermId where
    get = TermId <$> get
    put (TermId i) = put i

data DocTerm = DocTerm !Int !DocumentId
             deriving (Show)
     
instance Binary DocTerm where
    get = DocTerm <$> get <*> get
    put (DocTerm count doc) = put count >> put doc
