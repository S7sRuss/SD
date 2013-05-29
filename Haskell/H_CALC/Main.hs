module Main where

import Lexer (alexScanTokens, Token (..))
import Syntax (Exp,unparse)
import Parser (parse)
import Semantics (eval)
import System.IO

main :: IO ()
-- main = let e = parse (alexScanTokens "1+1+1") in do putStrLn (show (eval e))
               
main = do putStr "> "
          hFlush stdout
          str <- getLine
          let e = parse (alexScanTokens str) in
                    putStrLn (show (eval e))
          main
