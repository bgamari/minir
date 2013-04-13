{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

import           Criterion.Main
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Directory
import Control.Applicative
import Data.Binary
import Data.Char
import Data.Monoid
import Data.Foldable
import Data.Hashable (Hashable)
import Control.Monad
import System.FilePath
import Control.Monad.State.Strict
import Control.Lens

import qualified Dictionary as D
import qualified CorpusStats as CS
import TermIndex as TI

getTerms :: FilePath -> IO [(FilePath, [T.Text])]
getTerms fname = do
     d <- TIO.readFile fname
     let terms = filter (\s->T.length s > 2 && T.length s < 15)
                 $ T.words
                 $ T.filter (\c->isAlpha c || isSpace c)
                 $ T.toLower d
     return [(fname, terms)]

term = Term 1

newtype Term = Term Int deriving (Eq, Show, Enum, Ord, Binary)

mapTerms :: (Eq term, Hashable term)
         => [(doc, [term])] -> ([(doc, [Term])], D.Dictionary Term term)
mapTerms terms = runState (terms & traverse . _2 . traverse %%~ (state . D.getKey)) (D.empty $ Term 0)

getFiles :: IO [FilePath]
getFiles = do
    dents <- getDirectoryContents "data"
    filterM doesFileExist $ map (\d->joinPath ["data",d]) dents

main = do
    files <- getFiles
    (terms,dict) <- mapTerms <$> foldlM (\a fname -> mappend a <$> getTerms fname)
                    mempty files
    putStrLn $ "Documents: "++show (length terms)
    putStrLn $ "Terms: "++show (Prelude.sum $ map (\(n,d)->length d) terms)
    let cstats = foldMap' (\(d,n)->CS.fromDocument d (length n)) terms
        idx = foldMap' (uncurry TI.fromTerms) terms
        smallTerms = map (\(d,n)->(d, take 100 n)) terms
        smallIdx = foldMap' (uncurry TI.fromTerms) smallTerms
    smallIdx `seq` smallTerms `seq` return ()
    defaultMain
      [ -- bench "build corpus stats"
        --   $ whnf (foldMap' (\(d,n)->CS.fromDocument d (length n)))
        --     smallTerms
        bench "build term index"
          $ whnf (foldMap' (uncurry TI.fromTerms)) smallTerms
      --, bench "build both"
      --    $ whnf (foldMap' (\(d,n)->(TI.fromTerms d n, CS.fromDocument d $ length n))) smallTerms
      , bench "query"
          $ nf (termScore 0.1 cstats idx) term
      , bench "encode"
          $ encodeFile "index" idx
      , bench "decode"
          $ whnfIO (decodeFile "index" :: IO (TermIndex FilePath Term))
      ]

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = foldl' (\a b->mappend a $ f b) mempty xs
