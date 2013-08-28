module MinIR.PostingList ( -- * Reading posting lists
                           PostingListReader
                         , lookup
                         , toProducer
                           -- * Building posting lists
                         , build
                         ) where

import Prelude hiding (lookup)
import MinIR.PostingList.Reader
import MinIR.PostingList.Builder
