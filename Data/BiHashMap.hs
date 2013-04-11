module Data.BiHashMap ( BiHashMap
                      , fromHashMap
                      , singleton
                      , insertWith
                      , unionWith
                      , delete
                      , member
                      , lookup
                      , lookupV
                      , size
                      , toAscListV
                      , toDescListV
                      , forward
                      ) where

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Tuple (swap)

data BiHashMap k v = BiHashMap !(HM.HashMap k v) !(M.Map v (S.Set k))
                  deriving (Show)

-- | Construct a BiHashMap from a HashMap
fromHashMap :: (Ord k, Ord v) => HM.HashMap k v -> BiHashMap k v
fromHashMap m = BiHashMap m rev
  where rev = M.unionsWith S.union
              $ map (\(k,v)->M.singleton v (S.singleton k)) $ HM.toList m

-- | A singleton BiHashMap
singleton :: (Hashable k) => k -> v -> BiHashMap k v
singleton k v = BiHashMap (HM.singleton k v) (M.singleton v (S.singleton k))

-- | Insert into a BiHashMap
insertWith :: (Hashable k, Ord k, Eq k, Ord v)
           => (v -> v -> v) -> k -> v -> BiHashMap k v -> BiHashMap k v
insertWith f k v (BiHashMap fwd rev) = BiHashMap fwd' rev'
  where oldValue = HM.lookup k fwd
        newValue = maybe v (flip f v) oldValue
        fwd' = HM.insertWith f k v fwd
        rev' = M.insertWith S.union newValue (S.singleton k)
               $ maybe rev (\oldV->M.update (maybeSet . S.delete k) oldV rev) oldValue

maybeSet :: S.Set x -> Maybe (S.Set x)
maybeSet s | S.null s  = Nothing
           | otherwise = Just s

-- | Delete a key
delete :: (Ord k, Ord v, Hashable k)
       => k -> BiHashMap k v -> BiHashMap k v
delete k (BiHashMap fwd rev) = BiHashMap fwd' rev'
  where fwd' = HM.delete k fwd
        v = maybe (error "BiHashMap.delete: Internal error") id $ HM.lookup k fwd
        rev' = M.update (maybeSet . S.delete k) v rev

-- | Test for key membership
member :: (Eq k, Hashable k) => k -> BiHashMap k v -> Bool
member k (BiHashMap fwd _) = HM.member k fwd

-- | Lookup by key
lookup :: (Eq k, Hashable k) => k -> BiHashMap k v -> Maybe v
lookup k (BiHashMap fwd _) = HM.lookup k fwd

-- | Lookup by value
lookupV :: (Ord v) => v -> BiHashMap k v -> S.Set k
lookupV v (BiHashMap _ rev) = M.findWithDefault S.empty v rev

-- | Take the union of two BiHashMaps. This is not a great operation
-- performance-wise as one must rebuild the reverse map from scratch
unionWith :: (Hashable k, Eq k, Ord k, Ord v)
          => (v -> v -> v) -> BiHashMap k v -> BiHashMap k v -> BiHashMap k v
unionWith f (BiHashMap fa _) (BiHashMap fb _) =
    fromHashMap $ HM.unionWith f fa fb

size :: BiHashMap k v -> Int
size (BiHashMap fwd _) = HM.size fwd

-- | Keys and values sorted by value in ascending order
toAscListV :: Ord v => BiHashMap k v -> [(k, v)]
toAscListV (BiHashMap _ rev) =
    concatMap (\(k,vs)->map (\v->(v,k)) $ S.toList vs) $ M.toAscList rev

-- | Keys and values sorted by value in descending order
toDescListV :: Ord v => BiHashMap k v -> [(k, v)]
toDescListV (BiHashMap _ rev) =
    concatMap (\(k,vs)->map (\v->(v,k)) $ S.toList vs) $ M.toDescList rev

forward :: BiHashMap k v -> HM.HashMap k v
forward (BiHashMap fwd _) = fwd
