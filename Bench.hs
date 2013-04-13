{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Directory
import Control.Applicative
import TermIndex as TI
import Data.Char
import Data.Monoid
import Data.Foldable
import Data.List
import Data.Hashable (Hashable)
import Control.Monad
import System.FilePath
import Control.Monad.State.Strict
import qualified Dictionary as D
import Control.Lens

getTerms :: FilePath -> IO [(FilePath, [T.Text])]
getTerms fname = do
     d <- TIO.readFile fname
     let terms = filter (\s->T.length s > 2 && T.length s < 15)
                 $ T.words
                 $ T.filter (\c->isAlpha c || isSpace c)
                 $ T.toLower d
     return [(fname, terms)]

term = 1

mapTerms :: (Eq term, Hashable term)
         => [(doc, [term])] -> ([(doc, [D.Key])], D.Dictionary term)
mapTerms terms = runState (terms & traverse . _2 . traverse %%~ (state . D.getKey)) D.empty

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
    let idx = foldMap (uncurry TI.indexTerms) terms
    defaultMain
      [ bench "index" $ whnf (foldMap (uncurry TI.indexTerms))
                      $ map (\(n,d)->(n,take 100 d)) terms
      , bench "query" $ nf (termScore 0.1 idx) term
      ]
