module Test.Pretty where

import Test.Tasty
import Test.Tasty.QuickCheck

import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary()
import Language.ECMAScript3.Syntax.Annotations
--import System.Exit
import Language.ECMAScript3.SourceDiff

tests_pretty :: TestTree
tests_pretty = testProperty "Parse is the inverse of pretty" prettyParseEquivalence

-- -- main :: IO ()
-- -- main = 
-- --   let qcArgs = Args {maxSuccess = 50
-- --                     ,maxDiscardRatio = 10
-- --                     ,maxSize = 10
-- --                     ,replay = Nothing
-- --                     ,chatty = False}
-- --   in quickCheckWithResult qcArgs prettyParseEquivalence >>= \res ->
-- --   case res of
-- --     Success {} -> putStrLn "All tests passes"
-- --     GaveUp {} -> putStrLn "Gave up"
-- --     Failure {} -> putStrLn "Test failed" >> exitFailure
-- --     NoExpectedFailure {} -> putStrLn "Unexpected failure" >> exitFailure

prettyParseEquivalence :: JavaScript () -> Property
prettyParseEquivalence orig =
  let pp = show $ prettyPrint orig
  in case parseFromString pp of
    Left e -> 
      let err = "Can't parse pretty-printed code. The error was: " ++ (show e) ++
                "\nThe pretty-printed code in question:\n" ++ pp
      in whenFail (putStrLn err) False
    Right parsed ->
      let eq = (removeAnnotations parsed) == orig
          msg ="The parse of the pretty-printed AST didn't match the original\n"
               ++"Diff:\n" ++ jsDiff orig (reannotate (const ()) parsed)
      in whenFail (putStrLn msg) eq
