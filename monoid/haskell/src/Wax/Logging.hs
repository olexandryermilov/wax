module Wax.Logging where

--------------------------------------------------------------------------------
import Prelude
import System.IO
import Data.Monoid ((<>))
import Control.Monad (forever)

--------------------------------------------------------------------------------
type Logger = String -> IO ()

consoleLogger :: IO Logger
consoleLogger = return putStrLn

fileLogger :: FilePath -> IO Logger
fileLogger filePath = do
  putStrLn "Debug: fileLogger is called"
  handle <- openFile filePath WriteMode
  hSetBuffering handle NoBuffering
  return (hPutStr handle)

main :: IO ()
main = consoleLogger <> fileLogger "/Users/borysb/logging.log"
  >>= run

run :: Logger -> IO ()
run logger = forever $ logger . formatInput <$> getLine

formatInput :: String -> String
formatInput input = "User input: " <> input <> "\n"
