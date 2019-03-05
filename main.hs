import Codec.G86
import System.IO
import System.Environment
import qualified Data.ByteString as B
import Data.List.Split (chunksOf)

main = do
   args <- getArgs
   case args of
      [] -> putStr =<< unlines . chunksOf 80 . encode <$> B.getContents
      ["-d"] -> B.putStr =<< decode <$> getContents
      _ -> hPutStrLn stderr "Usage: g86 [-d] < input > output"

