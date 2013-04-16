import MinIR.DiskMap.Internal as DMI
import Control.Monad
import Data.Hashable

xs :: [Int]
xs = reverse [-30.. -10]++[1..20]++[-300000.. -2000]

f = hash
main = do
    dmap <- DMI.new "hello" :: IO (DMI.DiskMap Int)
    root <- DMI.getRoot dmap
    root' <- foldM (\t i->DMI.insert dmap (f i) i t) root xs
    DMI.putRoot dmap root'
    --putStrLn $ showTree root'
    --mapM_ (\i->DMI.lookup (f i) dmap >>= print) xs
    DMI.lookup (f $ head xs) dmap >>= print
