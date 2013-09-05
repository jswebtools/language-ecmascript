{-# LANGUAGE Rank2Types #-}

module Language.ECMAScript5.Lexer 
       ( ws
       , plbrace, prbrace, plparen, prparen, plbracket, prbracket, pdot, psemi
       , pcomma, plangle, prangle, pleqt, pgeqt, peq, pneq, pseq, psneq
       , pplus, pminus, pmul, pmod, pplusplus, pminusminus, pshl, pshr
       , pushr, pband, pbor, pbxor, pnot, pbnot, pand, por, pquestion, pcolon
       , passign, passignadd, passignsub, passignmul, passignmod, passignshl
       , passignshr, passignushr, passignband, passignbor, passignbxor, pdiv, passigndiv
       , kbreak, kcase, kcatch, kcontinue, kdebugger, kdefault, kdelete
       , kdo, kelse, kfinally, kfor, kfunction, kif, kin, kinstanceof, knew
       , kreturn, kswitch, kthis, kthrow, ktry, ktypeof, kvar, kvoid, kwhile, kwith
       , sget, sset
       , inParens
       , inBraces
       , inBrackets
       , identifier
       , identifierName
       , stringLiteral
       , numericLiteral
       , literal
       ) where

import Text.Parsec 
import Language.ECMAScript5.Parser.Util
import Language.ECMAScript5.Parser.Unicode
import Language.ECMAScript5.ParserState

import Language.ECMAScript5.Syntax
import Data.Default.Class
import Data.Default.Instances.Base

import Data.Char
import Data.Maybe (fromMaybe)
import Numeric(readDec,readOct,readHex)

import Control.Monad.Identity
import Control.Applicative ((<$>), (<*), (*>), (<*>), (<$))
import Data.Int (Int32)

lexeme :: Show a => Parser a -> Parser a
lexeme p = p <* ws

--7.2

ws :: Parser WhiteSpaceState
ws = do pos <- getPosition
        isNewLine <- many (False <$ whiteSpace <|> False <$ comment <|> True <$ lineTerminator)
        setNewLineState (any id isNewLine, pos)
  where whiteSpace :: Parser ()
        whiteSpace = forget $ choice [uTAB, uVT, uFF, uSP, uNBSP, uBOM, uUSP]

--7.3
uCRalone :: Parser Char
uCRalone = do uCR <* notFollowedBy uLF

lineTerminator :: Parser ()
lineTerminator = forget (uLF <|> uCR <|> uLS <|> uPS)
lineTerminatorSequence  :: Parser ()
lineTerminatorSequence = forget (uLF <|> uCRalone <|> uLS <|> uPS ) <|> forget uCRLF

--7.4
comment :: Parser String
comment = try multiLineComment <|> try singleLineComment

singleLineCommentChar :: Parser Char
singleLineCommentChar  = notFollowedBy lineTerminator *> noneOf ""

multiLineComment :: Parser String
multiLineComment = 
  do string "/*"
     comment <- concat <$> many insideMultiLineComment
     string "*/"
     modifyState $ modifyComments (MultiLineComment comment:)
     return comment

singleLineComment :: Parser String
singleLineComment = 
  do string "//" 
     comment <- many singleLineCommentChar
     modifyState $ modifyComments (SingleLineComment comment :)
     return comment

insideMultiLineComment :: Parser [Char]
insideMultiLineComment = noAsterisk <|> try asteriskInComment
 where
  noAsterisk =
    stringify $ noneOf "*"
  asteriskInComment =
    (:) <$> char '*' <*> (stringify (noneOf "/*") <|> "" <$ lookAhead (char '*') )

--7.6
identifier :: PosParser Expression
identifier = withPos $ VarRef def <$> identifierName

identifierName :: PosParser Id
identifierName = withPos $ lexeme $ flip butNot reservedWord $ fmap (Id def) $
                 (:)
                 <$> identifierStart
                 <*> many identifierPart

identifierStart :: Parser Char
identifierStart = unicodeLetter <|> char '$' <|> char '_' <|> unicodeEscape

unicodeEscape :: Parser Char
unicodeEscape = char '\\' >> unicodeEscapeSequence

identifierPart :: Parser Char
identifierPart = identifierStart <|> unicodeCombiningMark <|> unicodeDigit <|>
                 unicodeConnectorPunctuation <|> uZWNJ <|> uZWJ

--7.6.1
reservedWord :: Parser ()
reservedWord = choice [forget keyword, forget futureReservedWord, forget nullLiteral, forget booleanLiteral]

makeKeyword :: String -> Parser WhiteSpaceState
makeKeyword word = try (string word <* notFollowedBy identifierPart) *> ws

--7.6.1.1
keyword :: Parser WhiteSpaceState
keyword = choice [kbreak, kcase, kcatch, kcontinue, kdebugger, kdefault, kdelete,
                  kdo, kelse, kfinally, kfor, kfunction, kif, kin, kinstanceof, knew,
                  kreturn, kswitch, kthis, kthrow, ktry, ktypeof, kvar, kvoid, kwhile, kwith]

-- ECMAScript keywords
kbreak, kcase, kcatch, kcontinue, kdebugger, kdefault, kdelete,
  kdo, kelse, kfinally, kfor, kfunction, kif, kin, kinstanceof, knew,
  kreturn, kswitch, kthis, kthrow, ktry, ktypeof, kvar, kvoid, kwhile, kwith
  :: Parser WhiteSpaceState
kbreak      = makeKeyword "break"
kcase       = makeKeyword "case"
kcatch      = makeKeyword "catch"
kcontinue   = makeKeyword "continue"
kdebugger   = makeKeyword "debugger"
kdefault    = makeKeyword "default"
kdelete     = makeKeyword "delete"
kdo         = makeKeyword "do"
kelse       = makeKeyword "else"
kfinally    = makeKeyword "finally"
kfor        = makeKeyword "for"
kfunction   = makeKeyword "function"
kif         = makeKeyword "if"
kin         = makeKeyword "in"
kinstanceof = makeKeyword "instanceof"
knew        = makeKeyword "new"
kreturn     = makeKeyword "return"
kswitch     = makeKeyword "switch"
kthis       = makeKeyword "this"
kthrow      = makeKeyword "throw"
ktry        = makeKeyword "try"
ktypeof     = makeKeyword "typeof"
kvar        = makeKeyword "var"
kvoid       = makeKeyword "void"
kwhile      = makeKeyword "while"
kwith       = makeKeyword "with"

sget, sset :: Parser WhiteSpaceState
sget = makeKeyword "get"
sset = makeKeyword "set"

--7.6.1.2
futureReservedWord :: Parser WhiteSpaceState
futureReservedWord = choice [kclass, kconst, kenum, kexport, kextends, kimport, ksuper]

kclass, kconst, kenum, kexport, kextends, kimport, ksuper :: Parser WhiteSpaceState
kclass   = makeKeyword "class"
kconst   = makeKeyword "const"
kenum    = makeKeyword "enum"
kexport  = makeKeyword "export"
kextends = makeKeyword "extends"
kimport  = makeKeyword "import"
ksuper   = makeKeyword "super"

--7.7
punctuator :: Parser ()
punctuator = choice [ passignadd, passignsub, passignmul, passignmod,
                      passignshl, passignshr,
                      passignushr, passignband, passignbor, passignbxor,
                      pshl, pshr, pushr,
                      pleqt, pgeqt,
                      plbrace, prbrace, plparen, prparen, plbracket,
                      prbracket, pdot, psemi, pcomma,
                      plangle, prangle, pseq, peq, psneq, pneq,
                      pplusplus, pminusminus,
                      pplus, pminus, pmul,
                      pand, por,
                      pmod, pband, pbor, pbxor, pnot, pbnot,
                      pquestion, pcolon, passign ]

plbrace, prbrace, plparen, prparen, plbracket, prbracket, pdot, psemi,
  pcomma, plangle, prangle, pleqt, pgeqt, peq, pneq, pseq, psneq,
  pplus, pminus, pmul, pmod, pplusplus, pminusminus, pshl, pshr,
  pushr, pband, pbor, pbxor, pnot, pbnot, pand, por, pquestion, pcolon,
  passign, passignadd, passignsub, passignmul, passignmod, passignshl,
  passignshr, passignushr, passignband, passignbor, passignbxor, pdiv, passigndiv
  :: Parser ()

makeOp :: Show a => Parser a -> Parser ()
makeOp op = forget $ lexeme $ try op

plbrace       = makeOp $ char '{'
prbrace       = makeOp $ char '}'
plparen       = makeOp $ char '('
prparen       = makeOp $ char ')'
plbracket     = makeOp $ char '['
prbracket     = makeOp $ char ']'
pdot          = makeOp $ char '.'
psemi         = makeOp $ char ';'
pcomma        = makeOp $ char ','
plangle       = makeOp $ char '<' *> notFollowedBy (oneOf "=<")
prangle       = makeOp $ char '>' *> notFollowedBy (oneOf "=>")
pleqt         = makeOp $ string "<="
pgeqt         = makeOp $ string ">="
peq           = makeOp $ string "==" *> notFollowedBy (char '=')
pneq          = makeOp $ string "!=" *> notFollowedBy (char '=')
pseq          = makeOp $ string "==="
psneq         = makeOp $ string "!=="
pplus         = makeOp $ char '+' *> notFollowedBy (oneOf "=+")
pminus        = makeOp $ char '-' *> notFollowedBy (oneOf "=-")
pmul          = makeOp $ char '*' *> notFollowedBy (char '=')
pmod          = makeOp $ char '%' *> notFollowedBy (char '=')
pplusplus     = makeOp $ string "++"
pminusminus   = makeOp $ string "--"
pshl          = makeOp $ string "<<" *> notFollowedBy (char '=')
pshr          = makeOp $ string ">>" *> notFollowedBy (oneOf ">=")
pushr         = makeOp $ string ">>>" *> notFollowedBy (char '=')
pband         = makeOp $ char '&' *> notFollowedBy (oneOf "&=")
pbor          = makeOp $ char '|' *> notFollowedBy (oneOf "|=")
pbxor         = makeOp $ char '^' *> notFollowedBy (char '=')
pnot          = makeOp $ char '!' *> notFollowedBy (char '=')
pbnot         = makeOp $ char '~'
pand          = makeOp $ string "&&"
por           = makeOp $ string "||"
pquestion     = makeOp $ char '?'
pcolon        = makeOp $ char ':'
passign       = makeOp $ char '=' *> notFollowedBy (char '=')
passignadd    = makeOp $ string "+="
passignsub    = makeOp $ string "-="
passignmul    = makeOp $ string "*="
passignmod    = makeOp $ string "%="
passignshl    = makeOp $ string "<<="
passignshr    = makeOp $ string ">>="
passignushr   = makeOp $ string ">>>="
passignband   = makeOp $ string "&="
passignbor    = makeOp $ string "|="
passignbxor   = makeOp $ string "^="
pdiv          = makeOp $ do char '/' *> notFollowedBy (char '=')
passigndiv    = makeOp $ try (string "/=")

--7.8
literal :: PosParser Expression
literal = choice [nullLiteral, booleanLiteral, numericLiteral, stringLiteral, regularExpressionLiteral]

--7.8.1
nullLiteral :: PosParser Expression
nullLiteral = withPos (makeKeyword "null" >> return (NullLit def))

--7.8.2
booleanLiteral :: PosParser Expression
booleanLiteral = withPos $ BoolLit def
                 <$> (True <$ makeKeyword "true" <|> False <$ makeKeyword "false")

--7.8.3
numericLiteral :: PosParser Expression
numericLiteral = hexIntegerLiteral <|> decimalLiteral

-- Creates a decimal value from a whole, fractional and exponent parts.
mkDecimal :: Integer -> Integer -> Integer -> Integer -> Double
mkDecimal whole frac fracLen exp =
  ((fromInteger whole) + ((fromInteger frac) * (10 ^^ (-fracLen)))) * (10 ^^ exp)

-- Creates an integer value from a whole and exponent parts.
mkInteger :: Integer -> Integer -> Int
mkInteger whole exp = fromInteger $ whole * (10 ^ exp)

decimalLiteral :: PosParser Expression
decimalLiteral = withPos $ lexeme $
  (do whole <- decimalInteger
      mfraclen <- optionMaybe (pdot >> decimalDigitsWithLength)
      mexp  <- optionMaybe exponentPart
      if (mfraclen == Nothing && mexp == Nothing)
        then return $ NumLit def $ Left $ fromInteger whole
        else let (frac, flen) = fromMaybe (0, 0) mfraclen
                 exp          = fromMaybe 0 mexp
             in  return $ NumLit def $ Right $ mkDecimal whole frac flen exp)
  <|>
  (do (frac, flen) <- pdot >> decimalDigitsWithLength
      exp <- option 0 exponentPart
      return $ NumLit def $ Right $ mkDecimal 0 frac flen exp)

decimalDigitsWithLength :: Parser (Integer, Integer)
decimalDigitsWithLength = do digits <- many decimalDigit
                             return $ digits2NumberAndLength digits

digits2NumberAndLength :: [Integer] -> (Integer, Integer)
digits2NumberAndLength is =
  let (_, n, l) = foldr (\d (pow, acc, len) -> (pow*10, acc + d*pow, len+1))
                        (1, 0, 0) is
  in (n, l)

decimalIntegerLiteral :: PosParser Expression
decimalIntegerLiteral = withPos $ lexeme $ decimalInteger >>=
                        \i -> return $ NumLit def $ Left $ fromInteger i

decimalInteger :: Parser Integer
decimalInteger = (char '0' >> return 0)
              <|>(do d  <- nonZeroDecimalDigit
                     ds <- many decimalDigit
                     return $ fst $ digits2NumberAndLength (d:ds))

-- the spec says that decimalDigits should be intead of decimalIntegerLiteral, but that seems like an error
signedInteger :: Parser Integer
signedInteger = (char '+' >> decimalInteger) <|>
                (char '-' >> negate <$> decimalInteger) <|>
                decimalInteger

decimalDigit :: Parser Integer
decimalDigit  = do c <- decimalDigitChar
                   return $ toInteger $ ord c - ord '0'

decimalDigitChar :: Parser Char
decimalDigitChar = rangeChar '0' '9'

nonZeroDecimalDigit :: Parser Integer
nonZeroDecimalDigit  = do c <- rangeChar '1' '9'
                          return $ toInteger $ ord c - ord '0'

--hexDigit = ParsecChar.hexDigit

exponentPart :: Parser Integer
exponentPart = (char 'e' <|> char 'E') >> signedInteger

fromHex :: String -> Int32
fromHex = fst . head . Numeric.readHex

fromDecimal :: String -> Float
fromDecimal = fst . head . Numeric.readDec

hexIntegerLiteral :: PosParser Expression
hexIntegerLiteral = withPos $ lexeme $ do
  try (char '0' >> oneOf ['x', 'X'])
  NumLit def . Left . fromHex <$> many1 hexDigit

--7.8.4
dblquote :: Parser Char
dblquote = char '"'
quote :: Parser Char
quote = char '\''
backslash :: Parser Char
backslash = char '\\'
inDblQuotes :: Parser a -> Parser a
inDblQuotes x = between dblquote dblquote x
inQuotes :: Parser a -> Parser a
inQuotes x = between quote quote x
inParens :: Parser a -> Parser a
inParens x = between plparen prparen x
inBrackets :: Parser a -> Parser a
inBrackets x = between plbracket prbracket x
inBraces :: Parser a -> Parser a
inBraces x = between plbrace prbrace x

stringLiteral :: PosParser (Expression)
stringLiteral =  withPos $ lexeme $
                 do s <- ((inDblQuotes $ concatM $ many doubleStringCharacter)
                          <|>
                          (inQuotes $ concatM $ many singleStringCharacter))
                    return $ StringLit def s

doubleStringCharacter :: Parser String
doubleStringCharacter =
  stringify ((anyChar `butNot` choice[forget dblquote, forget backslash, lineTerminator])
             <|> backslash *> escapeSequence)
  <|> lineContinuation

singleStringCharacter :: Parser String
singleStringCharacter =
  stringify ((anyChar `butNot` choice[forget quote, forget backslash, forget lineTerminator])
             <|> backslash *> escapeSequence)
  <|> lineContinuation

lineContinuation :: Parser String
lineContinuation = backslash >> lineTerminatorSequence >> return ""

escapeSequence :: Parser Char
escapeSequence = characterEscapeSequence
              <|>(char '0' >> notFollowedBy decimalDigitChar >> return cNUL)
              <|>hexEscapeSequence
              <|>unicodeEscapeSequence

characterEscapeSequence :: Parser Char
characterEscapeSequence = singleEscapeCharacter <|> nonEscapeCharacter

singleEscapeCharacter :: Parser Char
singleEscapeCharacter = choice $ map (\(ch, cod) -> (char ch >> return cod))
                        [('b', cBS), ('t', cHT), ('n', cLF), ('v', cVT),
                         ('f', cFF), ('r', cCR), ('"', '"'), ('\'', '\''),
                         ('\\', '\\')]

nonEscapeCharacter :: Parser Char
nonEscapeCharacter = anyChar `butNot` (forget escapeCharacter <|> lineTerminator)

escapeCharacter :: Parser Char
escapeCharacter = singleEscapeCharacter
               <|>decimalDigitChar
               <|>char 'x'
               <|>char 'u'

hexEscapeSequence :: Parser Char
hexEscapeSequence =  chr . int32toInt . fromHex <$> (char 'x' *> count 2 hexDigit)

int32toInt :: Int32 -> Int
int32toInt = fromIntegral . toInteger

unicodeEscapeSequence :: Parser Char
unicodeEscapeSequence = chr . int32toInt . fromHex <$> (char 'u' *> count 4 hexDigit)

--7.8.5 and 15.10.4.1
regularExpressionLiteral :: PosParser Expression
regularExpressionLiteral =
    withPos $ lexeme $ do
      body <- between pdiv pdiv regularExpressionBody
      (g, i, m) <- regularExpressionFlags
      return $ RegexpLit def body g i m

-- TODO: The spec requires the parser to make sure the body is a valid
-- regular expression; were are not doing it at present.
regularExpressionBody :: Parser String
regularExpressionBody = do c <- regularExpressionFirstChar
                           cs <- concatM regularExpressionChars
                           return (c++cs)

regularExpressionChars :: Parser [String]
regularExpressionChars = many regularExpressionChar

regularExpressionFirstChar :: Parser String
regularExpressionFirstChar =
  choice [
    stringify $ regularExpressionNonTerminator `butNot` oneOf ['*', '\\', '/', '[' ],
    regularExpressionBackslashSequence,
    regularExpressionClass ]

regularExpressionChar :: Parser String
regularExpressionChar =
  choice [
    stringify $ regularExpressionNonTerminator `butNot` oneOf ['\\', '/', '[' ],
    regularExpressionBackslashSequence,
    regularExpressionClass ]

regularExpressionBackslashSequence :: Parser String
regularExpressionBackslashSequence = do c <-char '\\'
                                        e <- regularExpressionNonTerminator
                                        return (c:[e])

regularExpressionNonTerminator :: Parser Char
regularExpressionNonTerminator = anyChar `butNot` lineTerminator

regularExpressionClass :: Parser String
regularExpressionClass = do l <- char '['
                            rc <- concatM $ many regularExpressionClassChar
                            r <- char ']'
                            return (l:(rc++[r]))

regularExpressionClassChar :: Parser String
regularExpressionClassChar =
  stringify (regularExpressionNonTerminator `butNot` oneOf [']', '\\'])
  <|> regularExpressionBackslashSequence

regularExpressionFlags :: Parser (Bool, Bool, Bool) -- g, i, m
regularExpressionFlags = regularExpressionFlags' (False, False, False)

regularExpressionFlags' :: (Bool, Bool, Bool) -> Parser (Bool, Bool, Bool)
regularExpressionFlags' (g, i, m) =
    (char 'g' >> (if not g then regularExpressionFlags' (True, i, m) else unexpected "duplicate 'g' in regular expression flags")) <|>
    (char 'i' >> (if not i then regularExpressionFlags' (g, True, m) else unexpected "duplicate 'i' in regular expression flags")) <|>
    (char 'm' >> (if not m then regularExpressionFlags' (g, i, True) else unexpected "duplicate 'm' in regular expression flags")) <|>
    return (g, i, m)
