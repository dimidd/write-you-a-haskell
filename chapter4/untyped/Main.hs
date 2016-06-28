module Main where

import Syntax
import Parser
import Eval
import Pretty
import Counter

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import Control.Monad.State

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn ((replicate d ' ') ++ "=> " ++ ppexpr x)

process' :: Counter -> String -> InputT (StateT [String] IO) ()
process' c line = do
    lift $ modify (++ [line])
    if line == "%hist"
     then
        showHist c
     else
        liftIO $ process c line

process :: Counter -> String -> IO ()
process c line =
    if ((length line) > 0)
       then
        if (head line) /= '%'
            then do
                let res = parseExpr line
                case res of
                    Left err -> print err
                    Right ex -> do
                        let (out, ~steps) = runEval ex
                        mapM_ showStep steps
                        out_ps1 c $ show out
        else
            out_ps1 c "unknown cmd"
    -- TODO: don't increment counter for empty lines
    else do
      putStrLn ""

showHist :: Counter -> InputT (StateT [String] IO) ()
showHist c = do
      let out_count_io = c 0
      out_count <- liftIO $ out_count_io
      hist <- lift get
      let firstHist = head hist
      liftIO $ putStrLn $ "Out[" ++ (show out_count) ++ "]: " ++ firstHist
      liftIO $ putStrLn ""

out_ps1 :: Counter -> String -> IO ()
out_ps1 c out = do
      let out_count_io = c 0
      out_count <- out_count_io
      putStrLn $ "Out[" ++ (show out_count) ++ "]: " ++ out
      putStrLn ""

{-
handle_cmd :: String -> String
handle_cmd line = 
                    if line == "%hist"
                     then
                        showHist
                     else
                        "unknown cmd"
-}

main :: IO ()
main = do
    c <- makeCounter
    repl c

repl :: Counter -> IO ()
repl c = evalStateT (runInputT defaultSettings $ loop c) []

loop :: Counter -> InputT (StateT [String] IO) ()
loop c = do
    minput <- getLineIO $ in_ps1 $ c
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (process' c input) >> loop c

getLineIO :: (MonadException m) => IO String -> InputT m (Maybe String)
getLineIO ios = do
    s <- liftIO ios
    getInputLine s

in_ps1 :: Counter -> IO String
in_ps1 c = do
    let ion = c 1
    n <- ion
    let s = "Untyped In[" ++ (show n) ++ "]> "
    return s 
