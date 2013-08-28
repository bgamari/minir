module MinIR.PostingList ( -- * Reading posting lists
                           PostingListReader
                         , lookup
                         , toProducer
                           -- * Building posting lists
                         , build
                         ) where

import MinIR.PostingList.Reader
import MinIR.PostingList.Builder
