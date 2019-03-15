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
  return (hPutStrLn handle)

run :: Logger -> IO ()
run logger = forever $ do
  putStr "Please give me some input: "
  hFlush stdout
  input <- getLine
  logger . formatInput $ input

formatInput :: String -> String
formatInput input = "User input: " <> input

--------------------------------------------------------------------------------
main :: IO ()
main =  consoleLogger <> fileLogger "logging.log"
  >>= run
