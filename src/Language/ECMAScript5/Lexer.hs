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
import Data.Maybe (fromMaybe, isNothing)
import Numeric(readDec,readOct,readHex,readFloat)

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
identifier :: PosParser Id
identifier = withPos $ flip butNot reservedWord $ identifierName


identifierName :: PosParser Id
identifierName = withPos $ lexeme $ fmap (Id def) $
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
numericLiteral = withPos $ lexeme $ NumLit def <$> parseNumber

parseNumber:: Parser (Either Int32 Double) 
parseNumber = hexNumber <|> decimalNumber

hexNumber :: Parser (Either Int32 Double)
hexNumber = do s <- hexIntLit
               Left <$> wrapReadS Numeric.readHex s

hexIntLit :: Parser String
hexIntLit = do try (char '0' >> oneOf "xX")
               many1 hexDigit

decimalNumber :: Parser (Either Int32 Double)
decimalNumber = do (s, i) <- decLit
                   if i then Left <$> wrapReadS readDec s
                        else Right <$> wrapReadS readFloat s

-- | returns (s, True) if the number is an integer, an (s, False)
-- otherwise
decLit :: Parser (String, Bool)
decLit =
  let marr (Just ar) = ar
      marr Nothing = []
  in choice [do frac <- (:) <$> (char '.') <*> decDigits
                exp <- option "" exponentPart
                return ('0':frac++exp, False)
            ,do whole <- decIntLit
                mfrac <- optionMaybe ((:) <$> char '.' <*> decDigitsOpt)
                mexp  <- optionMaybe exponentPart
                let isint = isNothing mfrac && isNothing mexp
                return (whole ++ marr mfrac ++ marr mexp, isint)
            ]                          

decIntLit :: Parser String
decIntLit = digit >>= \d -> case d of
  '0' -> return [d]
  _   -> (d:) <$> decDigitsOpt

wrapReadS :: (Monad m) => ReadS a -> String -> m a
wrapReadS r s = case r s of
  [(a, "")] -> return a
  _         -> fail "Bad parse: could not convert a string to a Haskell value"

fromHex :: String -> Parser Int
fromHex = wrapReadS readHex

decDigitsOpt :: Parser String
decDigitsOpt = many digit

decDigits :: Parser String
decDigits = many1 digit

exponentPart :: Parser String
exponentPart = do ei <- oneOf "eE"
                  sgn<- option "" $ oneOf "+-" >>= \x -> return [x]
                  si <- decDigits
                  return (ei:(sgn++si))

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
  lineContinuation <|>
  stringify ((anyChar `butNot` choice[forget dblquote, forget backslash, lineTerminator])
             <|> backslash *> escapeSequence)

singleStringCharacter :: Parser String
singleStringCharacter =
  lineContinuation <|>
  stringify ((anyChar `butNot` choice[forget quote, forget backslash, forget lineTerminator])
             <|> backslash *> escapeSequence)


lineContinuation :: Parser String
lineContinuation = try (backslash >> lineTerminatorSequence >> return "")

escapeSequence :: Parser Char
escapeSequence = characterEscapeSequence
              <|>(char '0' >> notFollowedBy digit >> return cNUL)
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
               <|>digit
               <|>char 'x'
               <|>char 'u'

hexEscapeSequence :: Parser Char
hexEscapeSequence =  (char 'x' *> count 2 hexDigit) >>=
                     fromHex >>= return . chr

unicodeEscapeSequence :: Parser Char
unicodeEscapeSequence = (char 'u' *> count 4 hexDigit) >>=
                        fromHex >>= return . chr

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
