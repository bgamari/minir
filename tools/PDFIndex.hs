import           Control.Applicative
import           Data.Foldable
import           Data.Function
import           Data.Hashable
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

indexPDF :: FilePath -> IO (SeqDepIndex String T.Text)
indexPDF fname = SD.fromDocument 2 fname . T.words . T.toLower <$> readPDF fname

main = do
    exists <- doesFileExist "index"
    idx <- if exists then decodeFile "index" else return mempty
    files <- getArgs
    idx' <- foldlM (\a fname->mappend a <$> indexPDF fname) mempty files
    encodeFile "index" (idx <> idx')
