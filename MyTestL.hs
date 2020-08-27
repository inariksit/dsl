-- Program to test parser, automatically generated by BNF Converter.

module Main where

import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )
import Text.Pretty.Simple (pPrint, pShow)
import qualified Data.Text.Lazy as T
import ToProlog

import LexL    ( Token )
import ParL    ( pRules, myLexer )
import SkelL   ()
import PrintL  ( Print, printTree )
import AbsL   
import LayoutL ( resolveLayout )

type Err = Either String
type ParseFun a = [Token] -> Err a

myLLexer = resolveLayout True . myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a, ToProlog a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a, ToProlog a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = case p ts of
    Left s -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn s
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree

      exitSuccess
  where
  ts = myLLexer s


showTree :: (Show a, Print a, ToProlog a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ T.unpack (pShow tree)
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
      putStrV v $ "\n[Prolog version]\n\n" ++ toProlog tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pRules
    "-s":fs -> mapM_ (runFile 0 pRules) fs
    fs -> mapM_ (runFile 2 pRules) fs

