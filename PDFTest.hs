{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Foldable
import           Data.Function
import           Data.Hashable
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           SequentialDependence as SD
import           System.Environment
import           System.Exit
import           System.IO.Error
import           System.Process
import           Types

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

indexPDF :: FilePath -> IO (SeqDepIndex String T.Text)
indexPDF fname = SD.fromDocument 2 fname . T.words . T.toLower <$> readPDF fname

main = do
    term:args <- getArgs
    idx <- foldlM (\a fname->mappend a <$> indexPDF fname) mempty args
    print $ take 10 $ topN idx [T.pack term]

topN :: (Ord term, Ord doc, Hashable term, Eq term)
     => SeqDepIndex doc term -> [term] -> [(doc, Score)]
topN idx = sortBy (flip compare `on` snd) . scoreTerms defaultParams idx
