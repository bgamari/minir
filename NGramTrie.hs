module NGramTrie ( NGramTrie
                 , insert
                 , update
                 , isEmpty
                 , findSub
                 , lookup
                 ) where

import Prelude hiding (lookup, all)
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Foldable
import Data.Hashable

data NGramTrie n v = NGTrie { value :: !v
                            , children :: !(HM.HashMap n (NGramTrie n v))
                            }
                   deriving (Show)

instance (Hashable n, Eq n) => Functor (NGramTrie n) where
    fmap f (NGTrie v c) = NGTrie (f v) (fmap (fmap f) c)

instance (Hashable n, Eq n) => Foldable (NGramTrie n) where
    fold (NGTrie value children) = mappend value $ fold (fmap fold children)

instance (Hashable n, Eq n, Monoid v) => Monoid (NGramTrie n v) where
    mempty = NGTrie mempty HM.empty
    NGTrie va ca `mappend` NGTrie vb cb =
        NGTrie (va `mappend` vb) (HM.unionWith mappend ca cb)

insert :: (Hashable n, Eq n, Monoid v) => [n] -> v -> NGramTrie n v -> NGramTrie n v
insert []       v (NGTrie value children) = NGTrie (mappend value v) children
insert (n:rest) v (NGTrie value children) = NGTrie value children'
  where children' = let child = maybe (NGTrie mempty HM.empty) id
                              $ HM.lookup n children
                        child' = insert rest v child
                    in HM.insert n child' children

update :: (Hashable n, Eq n, Monoid v) => (v -> v) -> [n] -> NGramTrie n v -> NGramTrie n v
update f []       (NGTrie value children) = NGTrie (f value) children
update f (n:rest) (NGTrie value children) = NGTrie value children'
  where children' = HM.adjust (update f rest) n children

isEmpty :: (Hashable n, Eq n, Monoid v, Eq v) => NGramTrie n v -> Bool
isEmpty (NGTrie value children)
    | value /= mempty  = False
    | otherwise        = all isEmpty children

findSub :: (Hashable n, Eq n) => [n] -> NGramTrie n v -> Maybe (NGramTrie n v)
findSub []       trie                = Just trie
findSub (n:rest) (NGTrie _ children) =
    case HM.lookup n children of
      Just child -> findSub rest child
      Nothing    -> Nothing

lookup :: (Hashable n, Eq n) => [n] -> NGramTrie n v -> Maybe v
lookup path = fmap value . findSub path
