module Main where

import Language.JavaScript.SpiderMonkey.Parser
import Data.Aeson
import qualified Data.ByteString.Lazy as BS

import System.Environment( getArgs )

main :: IO ()
main = do [fileName] <- getArgs
          s <- BS.readFile fileName
          case eitherDecode s :: Either String Program of
           Left e     -> error $ "Parse failed: " ++ e
           Right actual -> do
             print actual
             print "Success"


