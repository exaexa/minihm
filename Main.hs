module Main where

import MiniHM

import Control.Exception
import Data.Char (isSpace)
import System.Console.Haskeline
import Control.Monad.Trans.Class (lift)

{-
 - user front-end
 -}
main :: IO ()
main = runInputT defaultSettings repl
  where
    repl = do
      l <- getInputLine "hm> "
      case l of
        Nothing -> pure ()
        Just s
          | all isSpace s -> repl
          | otherwise -> do
            lift $ inferType s putStrLn fail `catch` \e -> print (e :: IOException)
            lift $ putStrLn ""
            repl
