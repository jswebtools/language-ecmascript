-- | Experimental and very simple quasi-quotation of ECMAScript in
-- Haskell. Doesn't support anti-quotation as of now.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Language.ECMAScript5.Syntax.QuasiQuote (js, jsexpr, jsstmt) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding (parse)
import Control.Monad.Identity
import Data.Data (Data)

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Parser
import Language.ECMAScript5.ParserState

jsexpr :: QuasiQuoter
jsexpr = QuasiQuoter {quoteExp = quoteJSExpr}

jsstmt :: QuasiQuoter
jsstmt = QuasiQuoter {quoteExp = quoteJSStmt}

js :: QuasiQuoter
js = QuasiQuoter {quoteExp = quoteJS}

quoteJSExpr :: String -> TH.ExpQ
quoteJSExpr = quoteCommon expression

quoteJSStmt :: String -> TH.ExpQ
quoteJSStmt = quoteCommon statement

quoteJS :: String -> TH.ExpQ
quoteJS = quoteCommon program

quoteCommon :: forall a. (Data (Positioned a)) => PosParser a -> String -> TH.ExpQ
quoteCommon p s = do loc <- TH.location
                     let fname = TH.loc_filename loc
                     let (line, col)  = TH.loc_start loc
                     let p2  :: PosParser a
                         p2 = do pos <- getPosition
                                 setPosition $ (flip setSourceName) fname $
                                               (flip setSourceLine) line $
                                               (flip setSourceColumn) col $ pos
                                 res <- p
                                 eof
                                 return res
                     case parseString p2 s of
                       Left err -> do TH.reportError $ show err
                                      return $ TH.UnboxedTupE []
                       Right x  -> dataToExpQ (const Nothing) x

parseString :: PosParser a -> String -> Either ParseError (a ParserAnnotation)
parseString p s = parse p "" s
