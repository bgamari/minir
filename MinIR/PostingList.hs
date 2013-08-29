module MinIR.PostingList ( -- * Basic types
                           Posting(..)
                         , PostingListDir
                           -- * Reading posting lists
                         , PostingList
                         , open
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

