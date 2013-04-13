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
import Data.Binary
import Control.Applicative ((<$>))
import Data.List (foldl')

type Key = Int

data Dictionary term = Dict { rev :: IntMap term
                            , fwd :: HashMap term Key
                            }
                     deriving (Show)

instance (Hashable term, Eq term, Binary term) => Binary (Dictionary term) where
    put = put . toList
    get = fromList <$> get

toList :: Dictionary term -> [(Key, term)]
toList (Dict rev _) = IM.toList rev

fromList :: (Hashable term, Eq term) => [(Key, term)] -> Dictionary term
fromList xs = Dict rev fwd
  where (rev, fwd) = foldl' (\(f,r) (k,v)->(IM.insert k v f, HM.insert v k r))
                     (IM.empty, HM.empty) xs

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
