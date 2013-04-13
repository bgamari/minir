module Dictionary ( Key
                  , Dictionary
                  , empty
                  , lookupKey, lookupTerm
                  , getKey
                  ) where

import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import Data.Hashable

type Key = Int

data Dictionary term = Dict { rev :: IntMap term
                            , fwd :: HashMap term Key
                            }
                     deriving (Show)

empty :: Dictionary term
empty = Dict IM.empty HM.empty

lookupKey :: (Hashable term, Eq term)
          => term -> Dictionary term -> Maybe Key
lookupKey term (Dict _ fwd) = HM.lookup term fwd

nextKey :: Dictionary term -> Key
nextKey (Dict rev _)
    | IM.null rev = 0
    | otherwise   = succ $ fst $ IM.findMax rev

getKey :: (Hashable term, Eq term)
        => term -> Dictionary term -> (Key, Dictionary term)
getKey term dict@(Dict rev fwd)
    | Just k <- HM.lookup term fwd = (k, dict)
    | otherwise                    =
        let k' = nextKey dict
            dict' = Dict (IM.insert k' term rev) (HM.insert term k' fwd)
        in (k', dict')

lookupTerm :: Key -> Dictionary term -> Maybe term
lookupTerm key (Dict rev _) = IM.lookup key rev
