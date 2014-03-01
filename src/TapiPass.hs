{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (words)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.Text.Lazy
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.Trans.Error (runErrorT)
import Control.Monad (forever)
import System.IO

import YubiKey

type TapiPass a = StateT TPState IO a
type TPState = (Maybe YubiKey, Map.Map Text Text)

printPromptGetLine :: IO Text
printPromptGetLine = do
  TIO.putStr promptStr
  hFlush stdout
  TIO.getLine

tapiPassPrompt :: TapiPass Text
tapiPassPrompt = liftIO printPromptGetLine >>= exec

exec :: Text -> TapiPass Text
exec args = do
  (_,map) <- get
  let arglist = words args
  return $ case Prelude.head arglist of
    "list" -> pack . show $ map
    _ -> "Invalid command"

promptStr :: Text
promptStr = ">> "

main :: IO()
main = forever $ evalStateT tapiPassPrompt (Nothing, Map.empty)  >>= TIO.putStrLn

{-
  ok <- runErrorT $ withYubiKey $ \_ -> putStrLn "YubiKey found"
  case ok of
    Right _ -> putStrLn "YubiKey was found and function ran correctly"
    Left err -> putStrLn $ "Error: " ++ err
-}
