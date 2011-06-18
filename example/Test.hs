{-# LANGUAGE OverloadedStrings #-}

module EnumExamples where

import qualified Data.ByteString.Char8 as B

import qualified Data.Enumerator.Binary as EB
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Enumerator.List as EL

import Data.Enumerator


fromEnumerator :: Monad m => Iteratee B.ByteString m BL.ByteString
fromEnumerator = EB.consume

toEnumerator :: Monad m => BL.ByteString -> Enumerator B.ByteString m a
toEnumerator = enumList 1 . BL.toChunks

main :: IO ()
main = do
  str <- run_ $ toEnumerator (BL.pack "abcd") $$ fromEnumerator
  BL.putStrLn str
