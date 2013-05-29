module Main where

import System.IO
import System.Cmd (system)
import System.Environment ( getArgs )
import GHC.IO.Exception (ExitCode)

import Lexer (alexScanTokens, Token (..))
import Syntax (Core)
import Parser (parse)
import MaybeMessageMonad
import StateMaybeMonad
import TypeSystem
import CoreCompiler
import Generator

data Args =
     Source String
   | AsmFile String
   | Output String
   | Assembling
   | Compiling
   | Interactive
   deriving (Eq,Show)

default_asmfile = "a.s"
default_bsfile = "a.s.bc"
default_execfile = "a.out"
llvmpath = "/usr/local/llvm/bin/"

parseArgs :: [String] -> [Args]

parseArgs [] = []

parseArgs ("-i":args) = Interactive : parseArgs args

parseArgs ("-c":args) = Compiling : parseArgs args

parseArgs ("-s":args) = Assembling : parseArgs args

parseArgs ("-o":fn:args) = Output fn : parseArgs args

parseArgs (fn:args) = Source fn : parseArgs args

-- 

compState :: Core -> Maybe String
compState e = let SM(f) = comp e [] entryresult in 
                  case snd (f emptyState) of
                       Just (bs,fs,ts,d,end) -> Just $ dump (typeOf e) bs fs ts d end
                       Nothing -> Nothing 

printComp e = case compState e of
                   Just s -> putStr s
                   Nothing -> putStr "" 

asmfile args = if elem Assembling args then 
               case [s | Output s <- args] of
                    [] -> default_asmfile
                    s:ss -> s
               else default_asmfile

compile asmfile e = 
         case typecheck e [] of
              Result e -> 
                case compState e of
                     Just s -> writeFile asmfile s
                     Nothing -> putStr "Error compiling."
              Message s -> putStr s
         
compileandlink args e = 
               do 
               compile (asmfile args) e                    -- compile
               system $ llvmpath++"llvm-as "++asmfile args  -- assemble
               system $ llvmpath++"llvm-ld "++asmfile args++".bc runtime.s.bc" -- link
               
run = 
    do 
    system "./a.out"               

main :: IO ExitCode

-- main = let e = parse (alexScanTokens "1+1+1") in do putStrLn (show (eval e))
               
main = 
     do 
     args <- getArgs
     let parsedArgs = parseArgs args in
       if parsedArgs == [] || elem Interactive parsedArgs then
          do 
          putStr "Core> "
          hFlush stdout
          str <- getLine
          e <- return $ parse (alexScanTokens str)
          -- putStr $ show e++"\n"
          compileandlink parsedArgs e
          main
       else 
          do 
          str <- readFile $ head [ f | Source f <- parsedArgs ]
          let e = parse (alexScanTokens str) in
                  compileandlink parsedArgs e
          main

