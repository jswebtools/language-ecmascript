-- | This isn't a lexer in the sense that it provides a JavaScript
-- token-stream. This module provides character-parsers for various
-- JavaScript tokens.

module Language.ECMAScript3.Lexer(lexeme,identifier,reserved,operator,reservedOp,charLiteral,
                        stringLiteral,
--                        natural,integer,float,naturalOrFloat,
--                        decimal,
--                                 hexadecimal,octal,
                                 symbol,whiteSpace,parens,
                        braces,brackets,squares,semi,comma,colon,dot,
                        identifierStart
                                 ,hexIntLit,decIntLit, decDigits, decDigitsOpt, exponentPart, decLit) where

import Prelude hiding (lex)
import Data.Char
import Data.Monoid ((<>), mconcat)
import qualified Data.CharSet                  as Set
import qualified Data.CharSet.Unicode.Category as Set
import Text.Parsec
import qualified Text.Parsec.Token as T
import Language.ECMAScript3.Parser.State
import Language.ECMAScript3.Parser.Type
import Control.Monad.Identity
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isNothing)

identifierStartCharSet :: Set.CharSet
identifierStartCharSet =
  (filterBmpChars $ mconcat
    [ Set.fromDistinctAscList "$_"
    , Set.lowercaseLetter
    , Set.uppercaseLetter
    , Set.titlecaseLetter
    , Set.modifierLetter
    , Set.otherLetter
    , Set.letterNumber
    ])

identifierRestCharSet :: Set.CharSet
identifierRestCharSet =
  identifierStartCharSet
    <> (filterBmpChars $ mconcat
         [ Set.nonSpacingMark
         , Set.spacingCombiningMark
         , Set.decimalNumber
         , Set.connectorPunctuation
         ])

filterBmpChars :: Set.CharSet -> Set.CharSet
filterBmpChars = Set.filter (< '\65536')

identifierStart :: Stream s Identity Char => Parser s Char
identifierStart = satisfy (flip Set.member identifierStartCharSet) <?> "letter, '$', '_'"

identifierRest :: Stream s Identity Char => Parser s Char
identifierRest = satisfy (flip Set.member identifierRestCharSet) <?> "letter, digits, '$', '_' ..."

javascriptDef :: Stream s Identity Char =>T.GenLanguageDef s ParserState Identity
javascriptDef =
  T.LanguageDef "/*"
                "*/"
                "//"
                False -- no nested comments
                identifierStart
                identifierRest
                (oneOf "{}<>()~.,?:|&^=!+-*/%!") -- operator start
                (oneOf "=<>|&+") -- operator rest
                ["break", "case", "catch", "const", "continue", "debugger", 
                 "default", "delete", "do", "else", "enum", "false", "finally",
                 "for", "function", "if", "instanceof", "in", "let", "new", 
                 "null", "return", "switch", "this", "throw", "true", "try", 
                 "typeof", "var", "void", "while", "with"]
                ["|=", "^=", "&=", "<<=", ">>=", ">>>=", "+=", "-=", "*=", "/=", 
                 "%=", "=", ";", ",", "?", ":", "||", "&&", "|", "^", "&", 
                 "===", "==", "=", "!==", "!=", "<<", "<=", "<", ">>>", ">>", 
                 ">=", ">", "++", "--", "+", "-", "*", "/", "%", "!", "~", ".", 
                 "[", "]", "{", "}", "(", ")","</","instanceof"]
                 True -- case-sensitive
            
lex :: Stream s Identity Char => T.GenTokenParser s ParserState Identity
lex = T.makeTokenParser javascriptDef

-- everything but commaSep and semiSep
identifier :: Stream s Identity Char => Parser s String
identifier = T.identifier	 lex
reserved :: Stream s Identity Char => String -> Parser s ()
reserved = T.reserved	 lex
operator :: Stream s Identity Char => Parser s String
operator = T.operator	 lex
reservedOp :: Stream s Identity Char => String -> Parser s ()
reservedOp = T.reservedOp lex	
charLiteral :: Stream s Identity Char => Parser s Char
charLiteral = T.charLiteral lex	
stringLiteral :: Stream s Identity Char => Parser s String
stringLiteral = T.stringLiteral lex
-- natural :: Stream s Identity Char => Parser s Integer
-- natural = T.natural lex	
-- integer :: Stream s Identity Char => Parser s Integer
-- integer = T.integer lex	
-- float :: Stream s Identity Char => Parser s Double
-- float = T.float lex
-- naturalOrFloat :: Stream s Identity Char => Parser s (Either Integer Double)
-- naturalOrFloat = T.naturalOrFloat lex
-- decimal :: Stream s Identity Char => Parser s Integer
-- decimal = T.decimal lex	
-- hexadecimal :: Stream s Identity Char => Parser s Integer
-- hexadecimal = T.hexadecimal lex	
-- octal :: Stream s Identity Char => Parser s Integer
-- octal = T.octal lex
symbol :: Stream s Identity Char => String -> Parser s String
symbol = T.symbol lex
whiteSpace :: Stream s Identity Char => Parser s ()
whiteSpace = T.whiteSpace lex	
parens :: Stream s Identity Char => Parser s a -> Parser s a
parens = T.parens	 lex
braces :: Stream s Identity Char => Parser s a -> Parser s a
braces = T.braces	 lex
squares :: Stream s Identity Char => Parser s a -> Parser s a
squares = T.squares lex	
semi :: Stream s Identity Char => Parser s String
semi = T.semi	 lex
comma :: Stream s Identity Char => Parser s String
comma = T.comma	 lex
colon :: Stream s Identity Char => Parser s String
colon = T.colon lex
dot :: Stream s Identity Char => Parser s String
dot = T.dot lex
brackets :: Stream s Identity Char => Parser s a -> Parser s a
brackets = T.brackets lex
lexeme :: Stream s Identity Char => Parser s a -> Parser s a
lexeme = T.lexeme lex

-- 7.8.3
decIntLit :: Stream s Identity Char => Parser s String
decIntLit = digit >>= \d -> case d of
  '0' -> return [d]
  _   -> (d:) <$> decDigitsOpt

decDigitsOpt :: Stream s Identity Char => Parser s String
decDigitsOpt = many digit

decDigits :: Stream s Identity Char => Parser s String
decDigits = many1 digit

hexIntLit :: Stream s Identity Char => Parser s String
hexIntLit = do try (char '0' >> oneOf "xX")
               many1 hexDigit

exponentPart :: Stream s Identity Char => Parser s String
exponentPart = do ei <- oneOf "eE"
                  sgn<- option "" $ oneOf "+-" >>= \x -> return [x]
                  si <- decDigits
                  return (ei:(sgn++si))

-- data Sign = Plus | Minus

-- signedInteger :: Stream s Identity Char => Parser s (Sign, String)
-- signedInteger = do sgn <- option Plus (char '+' >> return Plus)
--                                    <|>(char '+' >> return Minus)
--                    s <- decDigits
--                    return (sgn, s)

-- | returns (s, True) if the number is an integer, an (s, False)
-- otherwise
decLit :: Stream s Identity Char => Parser s (String, Bool)
decLit =   
  choice [do whole <- decIntLit
             mfrac <- optionMaybe ((:) <$> char '.' <*> decDigitsOpt)
             mexp  <- optionMaybe exponentPart
             let isint = isNothing mfrac && isNothing mexp
             return (whole ++ marr mfrac ++ marr mexp, isint)
         ,do frac <- (:) <$> (char '.') <*> decDigits
             exp <- option "" exponentPart
             return ('0':frac++exp, True)             
         ]

marr (Just ar) = ar
marr Nothing = []
