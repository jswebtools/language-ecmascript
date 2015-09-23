module Test.Unit where

import Test.Tasty
import Test.Tasty.HUnit

import System.Exit
import System.Directory
import qualified System.FilePath as FilePath
import Language.ECMAScript5.Parser
import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.SourceDiff
import Control.Monad

tests_unit :: IO TestTree
tests_unit =
  do allFiles <- getDirectoryContents testDir
     let validFiles = filter (\x -> FilePath.takeExtension x == ".js") allFiles
     return $ testGroup "Parser Unit tests" $  map genTest validFiles


genTest :: FilePath -> TestTree
genTest file = testCase file $ parsePrettyTest (testDir `FilePath.combine` file) 

testDir = "test/parse-pretty"

-- | tests the parser with pre-defined test-cases
parsePrettyTest :: FilePath -> Assertion
parsePrettyTest filename =
  readFile filename >>= \src ->
  case parseFromString src of
    Left err -> assertFailure $ "Can't parse a test-case: " ++ filename
    Right js -> let str = show $ prettyPrint js
                in  case parseFromString str of
                  Left err ->
                    let msg = "Can't parse pretty-printed code. The error was: "
                              ++ (show err)
                              ++ "\nThe pretty-printed code in question:\n" ++ str
                    in assertFailure msg
                  Right js' -> do
                    let str' = show $ prettyPrint js'
                    unless (str == str') $ do
                      let msg = "The parse of the pretty-printed AST didn't match the original\n"
                                ++ "Diff:\n" ++ jsDiff js js'
                      assertFailure msg

