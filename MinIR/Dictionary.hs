module MinIR.Dictionary ( Dictionary
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

data Dictionary key term = Dict { next :: !key
                                , rev :: !(IntMap term)
                                , fwd :: !(HashMap term key)
                                }
                     deriving (Show)

instance (Hashable term, Eq term, Binary term, Enum key)
         => Binary (Dictionary key term) where
    put (Dict next rev _) = put (fromEnum next) >> put (IM.toList rev)
    get = do next <- get
             xs <- get
             let (rev, fwd) = foldl' (\(f,r) (k,v)->(IM.insert k v f, HM.insert v (toEnum k) r))
                              (IM.empty, HM.empty) xs
             return $ Dict (toEnum next) rev fwd

empty :: key -> Dictionary key term
empty k = Dict k IM.empty HM.empty

lookupKey :: (Hashable term, Eq term)
          => term -> Dictionary key term -> Maybe key
lookupKey term (Dict _ _ fwd) = HM.lookup term fwd

getKey :: (Hashable term, Eq term, Enum key)
        => term -> Dictionary key term -> (key, Dictionary key term)
getKey term dict@(Dict k' rev fwd)
    | Just k <- lookupKey term dict = (k, dict)
    | otherwise                     =
        let dict' = Dict (succ k') (IM.insert (fromEnum k') term rev) (HM.insert term k' fwd)
        in (k', dict')

lookupTerm :: Enum key => key -> Dictionary key term -> Maybe term
lookupTerm key (Dict _ rev _) = IM.lookup (fromEnum key) rev
