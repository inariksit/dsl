{-# LANGUAGE QuasiQuotes                      #-}

module Main where

import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO
import Control.Monad      ( when )
import Data.Maybe         ( fromMaybe )
import Text.RE.PCRE
import Data.Function

-- when you run l4 test1.l4 > stdoutfile 2> stderrfile
-- you might get "syntax error at line M, column N before ...
-- so, if you call showbug origfile stdoutfile stderrfile
-- you will get a view of the origfile where the error message complains.
-- note: what ought to be stderr is in stdout

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [origfile, stdoutfile, stderrfile] -> do
      stderr <- return.lines =<< readFile stderrfile
      stdout <- return.lines =<< readFile stdoutfile
      orig   <- return.lines =<< readFile origfile
      let coords = getErrorCoordinates $ stdout ++ stderr
      case (coords, stderr) of
        (Nothing,[]) -> mapM putStrLn stdout                 >> exitSuccess
        (Nothing,_)  -> mapM putStrLn stderr                 >> exitFailure
        (Just xy,_)  -> mapM putStrLn (showAtCoords orig xy) >> exitFailure

usage = do
  putStrLn "showbug test1.l4 out/test1.out out/test1.err"
      
getErrorCoordinates inlines = do
  mt <- matchedText $ unlines inlines ?=~ syntaxError
  case matches $ mt *=~ [re|\d+|] of
    [x,y] -> return (read x, read y, mt)
    otherwise -> Nothing
  where
    syntaxError = [re|syntax error at line \d+, column \d+.*|]

showAtCoords origlines (line,col,errormessage) =
  [ "* " ++ errormessage
  , "| " ++ origlines !! (line-2)
  , "| " ++ origlines !! (line-1)
  , "  " ++ (nspaces $ col - 1) ++ "^"
  , "| " ++ origlines !! (line-0)
  ]
  where
    nspaces n = take n $ repeat ' '

