{-# LANGUAGE DeriveGeneric #-}

module Wax.Books ( Book(..)
                 , readBook
                 , readBooks
                 , toWordsMap
                 , WordsMap(..)
                 ) where

--------------------------------------------------------------------------------
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.HashMap.Lazy (HashMap, empty, unionWith)
import           Data.List hiding (words)
import           GHC.Generics (Generic)
import           Path
import           Path.IO
import qualified Prelude as P (words)
import           Prelude hiding (words, Word)
import           Data.Semigroup

--------------------------------------------------------------------------------
data WordsMap = WordsMap !(HashMap String Int) deriving (Show, Generic)

instance Semigroup WordsMap where
  WordsMap m1 <> WordsMap m2 = WordsMap $ unionWith (+) m1 m2

instance Monoid WordsMap where
  mempty  = WordsMap empty
  mappend = (<>)

instance NFData WordsMap

--------------------------------------------------------------------------------
data Book
  = Book
  { filePath :: !(Path Abs File)
  , allWords :: ![String]
  } deriving (Show, Generic)

instance NFData Book

book :: Path Abs File -> String -> Book
book fp content
  = Book
  { filePath = fp
  , allWords = words
  } where words = filter (not . null) $ tokenise <$> P.words content

--------------------------------------------------------------------------------
toWordsMap :: Book -> WordsMap
toWordsMap = WordsMap . foldr (\x r -> r & at x . non 0 +~ 1) empty . allWords

--------------------------------------------------------------------------------
tokenise :: String -> String
tokenise = map toLower
         . dropWhileEnd (not . isAlpha)
         . dropWhile (not . isAlpha)

--------------------------------------------------------------------------------
listBooks :: (MonadIO m) => Path Abs Dir -> m [Path Abs File]
listBooks dir = snd <$> listDirRecur dir

--------------------------------------------------------------------------------
readBook :: (MonadIO m) => Path Abs File -> m Book
readBook fp = liftIO . fmap (book fp) . readFile $ toFilePath fp

--------------------------------------------------------------------------------
readBooks :: (MonadIO m) => Path Abs Dir -> m [Book]
readBooks dir = listBooks dir >>= mapM readBook
