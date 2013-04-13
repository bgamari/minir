{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.Environment
import System.Exit
import TermIndex as TI
import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Function
import Data.List
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO.Error

readPDF :: FilePath -> IO T.Text
readPDF fname = spoon $ do
    (inp,out,err,pid) <- runInteractiveProcess "/usr/bin/pdftotext" [fname, "-"]
                         Nothing Nothing
    d <- TIO.hGetContents out
    code <- waitForProcess pid
    case code of
      ExitSuccess -> return d
      otherwise   -> return T.empty
  where spoon :: IO T.Text -> IO T.Text
        spoon a = do b <- tryIOError a
                     case b of
                       Left error -> do putStrLn $ "Failed to read "++fname
                                        return T.empty
                       Right res  -> return res

indexPDF :: FilePath -> IO (TermIndex String T.Text)
indexPDF fname = TI.indexTerms fname . T.words . T.toLower <$> readPDF fname

main = do
    term:args <- getArgs
    --Prelude.mapM_ (\fname->readPDF fname >>= print) args
    idx <- foldlM (\a fname->mappend a <$> indexPDF fname) mempty args
    --print idx
    print $ take 10 $ topN idx (T.pack term)

topN :: (Ord term, Ord doc)
     => TermIndex doc term -> term -> [(doc, Score)]
topN idx term = sortBy (flip compare `on` snd) $ termScore 0.1 idx term