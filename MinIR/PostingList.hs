module MinIR.PostingList ( -- * Postings
                           Posting(..)
                           -- * Reading posting lists
                         , PostingList
                         , lookup
                         , toProducer
                           -- * Building posting lists
                         , build
                         , PostingProducer
                         ) where

import Prelude hiding (lookup)
import MinIR.PostingList.Types
import MinIR.PostingList.Reader
import MinIR.PostingList.Builder
