import           Control.Applicative
import           Data.Foldable
import           Data.Function
import           Data.Char
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment
import           System.Exit
import           System.IO.Error
import           System.Process
import           Data.Binary
import           System.Directory
import           Data.Text.Binary

import           NLP.Stemmer
import           MinIR.SequentialDependence as SD
import           MinIR.Types

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

readTerms :: FilePath -> IO T.Text
readTerms fname
    | ".pdf" `isSuffixOf` fname   = readPDF fname
    | ".txt" `isSuffixOf` fname   = TIO.readFile fname
    | otherwise                   = do putStrLn $ "Unknown file type: "++fname
                                       return T.empty

indexFile :: FilePath -> IO (SeqDepIndex T.Text T.Text)
indexFile fname =
    SD.fromDocument 2 (T.pack fname) . extractTerms <$> readTerms fname

extractTerms :: T.Text -> [T.Text]
extractTerms =
      map T.pack . stemWords English . map T.unpack
    . filter (\a->T.length a > 2 && T.length a < 20)
    . map (T.filter isAlpha)
    . T.words
    . T.toLower

main = do
    exists <- doesFileExist "index"
    idx <- if exists then decodeFile "index" else return mempty
    files <- getArgs
    idx' <- foldlM (\a fname->mappend a <$> indexFile fname) mempty files
    encodeFile "index" (idx <> idx')
