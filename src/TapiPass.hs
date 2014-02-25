{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST.Safe
import Data.Text
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO

type TapiPassHT s = HT.HashTable s Text Text

hashTest :: ST s Text
hashTest = do
  ht <- HT.new :: ST s (HT.HashTable s Text Text)
  HT.insert ht "key" "val"
  mb <- HT.lookup ht "key"
  return $ fromMaybe "nothing" mb

main :: IO()
main = TIO.putStrLn $ runST hashTest
