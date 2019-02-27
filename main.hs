import G86
import System.IO
import System.Environment
import qualified Data.ByteString as B

main = do
   args <- getArgs
   case args of
      [] -> putStrLn =<< encode <$> B.getContents
      ["-d"] -> B.putStr =<< decode <$> getContents
      _ -> hPutStrLn stderr "Usage: g86 [-d] < input > output"

