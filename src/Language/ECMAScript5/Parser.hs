{-# LANGUAGE Rank2Types #-}

module Language.ECMAScript5.Parser (parse
                                   , parseScriptFromString
                                   , parseJavaScriptFromFile
                                   , parseScript
                                   , parseExpression
                                   , parseString
                                   , ParsedStatement
                                   , ParsedExpression
                                   , parseSimpleExpr'
                                   , parseBlockStmt
                                   , parseStatement
                                   , StatementParser
                                   , ExpressionParser
                                   , assignExpr
                                   ) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Parser.Util
import Language.ECMAScript5.Parser.Unicode
import Data.Default.Class
import Data.Default.Instances.Base
import Text.Parsec hiding (parse, spaces)
import Text.Parsec.Char (char, string, satisfy, oneOf, noneOf, hexDigit, anyChar)
import Text.Parsec.Char as ParsecChar hiding (spaces)
import Text.Parsec.Combinator
import Text.Parsec.Prim

import Control.Monad(liftM,liftM2)
import Control.Monad.Trans (MonadIO,liftIO)
import Numeric(readDec,readOct,readHex)
import Data.Char
import Control.Monad.Identity
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Applicative ((<$>))
import Control.Arrow

--import Numeric as Numeric

-- the statement label stack
type ParserState = [String]
type PosParser x = Parser (x SourceSpan)

type Parser a = forall s. Stream s Identity Char => ParsecT s ParserState Identity a

type SourceSpan = (SourcePos, SourcePos)

initialParserState :: ParserState
initialParserState = []

-- | checks if the label is not yet on the stack, if it is -- throws
-- an error; otherwise it pushes it onto the stack
pushLabel :: String -> Parser ()
pushLabel lab = do labs <- getState
                   pos <- getPosition
                   if lab `elem` labs 
                     then fail $ "Duplicate label at " ++ show pos
                     else putState (lab:labs)

popLabel :: Parser ()
popLabel = modifyState safeTail
  where safeTail [] = []
        safeTail (_:xs) = xs

clearLabels :: ParserState -> ParserState
clearLabels _ = []

withFreshLabelStack :: Parser a -> Parser a
withFreshLabelStack p = do oldState <- getState
                           putState $ clearLabels oldState
                           a <- p
                           putState oldState
                           return a

-- a convenience wrapper to take care of the position, "with position"
withPos   :: (HasAnnotation x) => PosParser x -> PosParser x
withPos p = do start <- getPosition
               result <- p
               end <- getPosition
               return $ setAnnotation (start, end) result

-- Below "x.y.z" are references to ECMAScript 5 spec chapters that discuss the corresponding grammar production
--7.2
whiteSpace :: Parser ()
whiteSpace = forget $ choice [uTAB, uVT, uFF, uSP, uNBSP, uBOM, uUSP]

spaces :: Parser ()
spaces = skipMany (whiteSpace <|> comment <?> "")

lexeme :: Parser a -> Parser a
lexeme p = do{ x <- p; spaces; return x}

--7.3
uCRalone :: Parser Char
uCRalone = do c <- uCR
              notFollowedBy uLF
              return c

lineTerminator :: Parser ()
lineTerminator = forget (uLF <|> uCR <|> uLS <|> uPS)
lineTerminatorSequence  :: Parser ()
lineTerminatorSequence = (forget (uLF <|> uCRalone <|> uLS <|> uPS )) <|> (forget uCRLF)

--7.4
comment :: Parser ()
comment = multiLineComment <|> singleLineComment

singleLineCommentChars :: Parser ()
singleLineCommentChars = singleLineCommentChar >> singleLineCommentChars

singleLineCommentChar :: Parser ()
singleLineCommentChar  = notP lineTerminator
multiLineCommentChars :: Parser ()
multiLineCommentChars  = (multiLineNotAsteriskChar >> multiLineCommentChars) 
                      <|>(char '*' >> postAsteriskCommentChars)
multiLineComment :: Parser ()
multiLineComment = forget (string "/*" >> optional multiLineCommentChars >> string "*/")
singleLineComment :: Parser ()
singleLineComment = string "//" >> optional singleLineCommentChars
multiLineNotAsteriskChar :: Parser ()
multiLineNotAsteriskChar = notP $ char '*'
multiLineNotForwardSlashOrAsteriskChar :: Parser Char
multiLineNotForwardSlashOrAsteriskChar = noneOf "/*"
postAsteriskCommentChars :: Parser ()
postAsteriskCommentChars = (multiLineNotForwardSlashOrAsteriskChar >>
                            forget (optionMaybe multiLineCommentChars)) 
                        <|>(char '*' >>
                            forget (optionMaybe postAsteriskCommentChars))

--7.5
--token = identifierName <|> punctuator <|> numericLiteral <|> stringLiteral

--7.6
identifier :: PosParser Expression
identifier = lexeme $ withPos $ do name <- identifierName `butNot` reservedWord
                                   return $ VarRef def name

identifierName :: PosParser Id
identifierName = withPos $ do c  <- identifierStart
                              cs <- many identifierPart
                              return $ Id def (c:cs)

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

--7.6.1.1
keyword :: Parser String
keyword = choice [kbreak, kcase, kcatch, kcontinue, kdebugger, kdefault, kdelete,
                  kdo, kelse, kfinally, kfor, kfunction, kif, kin, kinstanceof, knew,
                  kreturn, kswitch, kthis, kthrow, ktry, ktypeof, kvar, kvoid, kwhile, kwith]

-- ECMAScript keywords
kbreak :: Parser String
kbreak = string "break"
kcase :: Parser String
kcase  = string "case"
kcatch :: Parser String
kcatch = string "catch"
kcontinue :: Parser String
kcontinue = string "continue"
kdebugger :: Parser String
kdebugger = string "debugger"
kdefault :: Parser String
kdefault = string "default"
kdelete :: Parser String
kdelete = string "delete"
kdo :: Parser String
kdo = string "do"
kelse :: Parser String
kelse = string "else"
kfinally :: Parser String
kfinally = string "finally"
kfor :: Parser String
kfor = string "for"
kfunction :: Parser String
kfunction = string "function"
kif :: Parser String
kif = string "if"
kin :: Parser String
kin = string "in"
kinstanceof :: Parser String
kinstanceof = string "instanceof"
knew :: Parser String
knew = string "new"
kreturn :: Parser String
kreturn = string "return"
kswitch :: Parser String
kswitch = string "switch"
kthis :: Parser String
kthis = string "this"
kthrow :: Parser String
kthrow = string "throw"
ktry :: Parser String
ktry = string "try"
ktypeof :: Parser String
ktypeof = string "typeof"
kvar :: Parser String
kvar = string "var"
kvoid :: Parser String
kvoid = string "void"
kwhile :: Parser String
kwhile = string "while"
kwith :: Parser String
kwith = string "with"

--7.6.1.2
futureReservedWord :: Parser String
futureReservedWord = choice [kclass, kconst, kenum, kexport, kextends, kimport, ksuper]
kclass :: Parser String
kclass = string "class"
kconst :: Parser String
kconst = string "const"
kenum :: Parser String
kenum = string "enum"
kexport :: Parser String
kexport = string "export"
kextends :: Parser String
kextends = string "extends"
kimport :: Parser String
kimport = string "import"
ksuper :: Parser String
ksuper = string "super"

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
plbrace :: Parser ()
plbrace = forget $ lexeme $ char '{'
prbrace :: Parser ()
prbrace = forget $ lexeme $ char '}'
plparen :: Parser ()
plparen = forget $ lexeme $ char '('
prparen :: Parser ()
prparen = forget $ lexeme $ char ')'
plbracket :: Parser ()
plbracket = forget $ lexeme $ char '['
prbracket :: Parser ()
prbracket = forget $ lexeme $ char ']'
pdot :: Parser ()
pdot = forget $ lexeme $ char '.'
psemi :: Parser ()
psemi = forget $ lexeme $ char ';'
pcomma :: Parser ()
pcomma = forget $ lexeme $ char ','
plangle :: Parser ()
plangle = forget $ lexeme $ do char '<'
                               lookAhead $ notP $ choice [char '=', char '<']
prangle :: Parser ()
prangle = forget $ lexeme $ do char '>'
                               lookAhead $ notP $ choice [char '=', char '>']
pleqt :: Parser ()
pleqt = forget $ lexeme $ string "<="
pgeqt :: Parser ()
pgeqt = forget $ lexeme $ string ">="
peq :: Parser ()
peq  = forget $ lexeme $ string "=="
pneq :: Parser ()
pneq = forget $ lexeme $ string "!="
pseq :: Parser ()
pseq = forget $ lexeme $ string "==="
psneq :: Parser ()
psneq = forget $ lexeme $ string "!=="
pplus :: Parser ()
pplus = forget $ lexeme $ do char '+'
                             lookAhead $ notP $ choice [char '=', char '+']
pminus :: Parser ()
pminus = forget $ lexeme $ do char '-'
                              lookAhead $ notP $ choice [char '=', char '-']
pmul :: Parser ()
pmul = forget $ lexeme $ do char '*'
                            lookAhead $ char '='
pmod :: Parser ()
pmod = forget $ lexeme $ do char '%'
                            lookAhead $ char '='
pplusplus :: Parser ()
pplusplus = forget $ lexeme $ string "++"
pminusminus :: Parser ()
pminusminus = forget $ lexeme $ string "--"
pshl :: Parser ()
pshl = forget $ lexeme $ string "<<"
pshr :: Parser ()
pshr = forget $ lexeme $ string ">>"
pushr :: Parser ()
pushr = forget $ lexeme $ string ">>>"
pband :: Parser ()
pband = forget $ lexeme $ do char '&'
                             lookAhead $ notP $ char '&'
pbor :: Parser ()
pbor = forget $ lexeme $ do char '|'
                            lookAhead $ notP $ choice [char '|', char '=']
pbxor :: Parser ()
pbxor = forget $ lexeme $ do char '^'
                             lookAhead $ notP $ choice [char '=']
pnot :: Parser ()
pnot = forget $ lexeme $ do char '!'
                            lookAhead $ notP $ char '='
pbnot :: Parser ()
pbnot = forget $ lexeme $ char '~'
pand :: Parser ()
pand = forget $ lexeme $ string "&&"
por :: Parser ()
por = forget $ lexeme $ string "||"
pquestion :: Parser ()
pquestion = forget $ lexeme $ char '?'
pcolon :: Parser ()
pcolon = forget $ lexeme $ char ':'
passign :: Parser ()
passign = forget $ lexeme $ do char '='
                               lookAhead $ notP $ char '='
passignadd :: Parser ()
passignadd = forget $ lexeme $ string "+="
passignsub :: Parser ()
passignsub = forget $ lexeme $ string "-="
passignmul :: Parser ()
passignmul = forget $ lexeme $ string "*="
passignmod :: Parser ()
passignmod = forget $ lexeme $ string "%="
passignshl :: Parser ()
passignshl = forget $ lexeme $ string "<<="
passignshr :: Parser ()
passignshr = forget $ lexeme $ string ">>="
passignushr :: Parser ()
passignushr = forget $ lexeme $ string ">>>="
passignband :: Parser ()
passignband = forget $ lexeme $ string "&="
passignbor :: Parser ()
passignbor = forget $ lexeme $ string "|="
passignbxor :: Parser ()
passignbxor = forget $ lexeme $ string "^="
divPunctuator :: Parser ()
divPunctuator = choice [ passigndiv, pdiv ]

passigndiv :: Parser ()
passigndiv = forget $ lexeme $ string "/="
pdiv :: Parser ()
pdiv = forget $ lexeme $ do char '/'
                            lookAhead $ notP $ char '='

--7.8
literal :: PosParser Expression
literal = choice [nullLiteral, booleanLiteral, numericLiteral, stringLiteral, regularExpressionLiteral]

--7.8.1
nullLiteral :: PosParser Expression
nullLiteral = lexeme $ withPos (string "null" >> return (NullLit def))

--7.8.2
booleanLiteral :: PosParser Expression
booleanLiteral = lexeme $ withPos $ ((string "true" >> return (BoolLit def True)) 
                                 <|> (string "false" >> return (BoolLit def False)))

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
decimalLiteral = lexeme $ withPos $
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
decimalIntegerLiteral = lexeme $ withPos $ decimalInteger >>= 
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
       

fromHex digits = do [(hex,"")] <- return $ Numeric.readHex digits
                    return hex
                    
fromDecimal digits = do [(hex,"")] <- return $ Numeric.readDec digits
                        return hex
hexIntegerLiteral :: PosParser Expression
hexIntegerLiteral = lexeme $ withPos $ do
  try (char '0' >> (char 'x' <|> char 'X'))
  digits <- many1 hexDigit
  n <- fromHex digits
  return $ NumLit def $ Left $ fromInteger n
                         
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

stringLiteral :: PosParser Expression
stringLiteral =  lexeme $ withPos $ 
                 do s <- ((inDblQuotes $ concatM $ many doubleStringCharacter)
                          <|> 
                          (inQuotes $ concatM $ many singleStringCharacter))
                    return $ StringLit def s

doubleStringCharacter :: Parser String
doubleStringCharacter = (stringify ((anyChar `butNot` choice[forget dblquote, forget backslash, lineTerminator]) <|>(backslash >> escapeSequence)))
                     <|>lineContinuation 

singleStringCharacter :: Parser String
singleStringCharacter =  (stringify ((anyChar `butNot` choice[forget quote, forget backslash, forget lineTerminator])<|> (backslash >> escapeSequence)))
                     <|>lineContinuation

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
hexEscapeSequence =  do digits <- (char 'x' >> count 2 hexDigit)
                        hex <- fromHex digits
                        return $ chr hex

unicodeEscapeSequence :: Parser Char
unicodeEscapeSequence = do digits <- char 'u' >> count 4 hexDigit
                           hex <- fromHex digits
                           return $ chr hex

--7.8.5 and 15.10.4.1
regularExpressionLiteral :: PosParser Expression
regularExpressionLiteral = 
    lexeme $ withPos $ do 
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

regularExpressionBackslashSequence :: 
                                   Parser String
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
    
regularExpressionFlags ::  
                       Parser (Bool, Bool, Bool) -- g, i, m    
regularExpressionFlags = regularExpressionFlags' (False, False, False)
  
regularExpressionFlags' :: (Bool, Bool, Bool) 
                        -> Parser (Bool, Bool, Bool)

regularExpressionFlags' (g, i, m) = 
    (char 'g' >> (if not g then regularExpressionFlags' (True, i, m) else unexpected "duplicate 'g' in regular expression flags")) <|>
    (char 'i' >> (if not i then regularExpressionFlags' (g, True, m) else unexpected "duplicate 'i' in regular expression flags")) <|>
    (char 'm' >> (if not m then regularExpressionFlags' (g, i, True) else unexpected "duplicate 'm' in regular expression flags")) <|>
    return (g, i, m)
    
-- | 7.9 || TODO: write tests based on examples from Spec 7.9.2, once I
-- get the parser finished! Automatic Semicolon Insertion algorithm,
-- rule 1; to be used in place of `semi`/`char 'x'` in parsers for
-- emptyStatement, variableStatement, expressionStatement,
-- doWhileStatement, continuteStatement, breakStatement,
-- returnStatement and throwStatement.
autoSemi :: Parser ()
autoSemi = psemi
        <|>lineTerminator
        <|>prbrace
  
-- | Automatic Semicolon Insertion algorithm, rule 2;
-- to be used at the end of the program
endOfProgram :: Parser ()
endOfProgram = forget (char ';') <|> eof
           
-- | Automatic Semicolon Insertion algorithm, rule 3; it takes 2
-- parsers: 'left' that parses whatever is to the left of [no
-- LineTerminator here] and 'right' that parses whatever is to the
-- right; if after parsing 'left' and any number of whiteSpaces a
-- lineTerminator is found, 'right' is not invoked and (l, Nothing) is
-- returned, where 'l' is the result of left; otherwise (l, Just r) is
-- returned, where 'l' and 'r' are results of left and right
-- respectively.
noLineTerminator :: 
                 Parser a -> Parser b -> Parser (a, Maybe b)
noLineTerminator left right = do l <- left
                                 spaces
                                 ((try lineTerminator >>
                                  return (l, Nothing)) <|>
                                  (right >>= (\r-> return (l, Just r))))

-- 11.1
-- primary expressions
primaryExpression :: PosParser Expression
primaryExpression = choice [lexeme $ withPos (kthis >> return (ThisRef def))
                           ,identifier
                           ,literal
                           ,arrayLiteral
                           ,parenExpression]

parenExpression :: PosParser Expression
parenExpression = lexeme $ withPos (between plparen prparen expression)
                                    
-- 11.1.4
arrayLiteral :: PosParser Expression
arrayLiteral = lexeme $ withPos $ 
               do plbracket
                  e <- elementsListWithElision
                  prbracket
                  return $ ArrayLit def e

elementsListWithElision :: Parser [Maybe (Expression SourceSpan)]
elementsListWithElision = (optionMaybe assignmentExpression) `sepBy` pcomma
  
-- 11.1.5
objectLiteral :: PosParser Expression
objectLiteral = lexeme $ withPos $
                do plbrace
                   props <- propertyAssignment `sepBy` pcomma
                   optional pcomma
                   prbrace
                   return $ ObjectLit def props

propertyAssignment :: Parser (PropAssign SourceSpan)
propertyAssignment = lexeme $ withPos $
                     (do lexeme $ string "get"
                         pname <- propertyName
                         prparen
                         plparen
                         plbrace
                         body <- functionBody
                         prbrace
                         return $ PGet def pname body)
                  <|>(do lexeme $ string "set"
                         pname <- propertyName
                         prparen
                         param <- identifierName
                         plparen
                         plbrace
                         body <- functionBody
                         prbrace
                         return $ PSet def pname param body)
                  <|>(do pname <- propertyName
                         pcolon
                         e <- assignmentExpression
                         return $ PExpr def pname e)

propertyName :: PosParser Prop
propertyName = lexeme $ withPos $
               (identifierName >>= id2Prop)
            <|>(stringLiteral >>= string2Prop)
            <|>(numericLiteral >>= num2Prop)
  where id2Prop (Id a s) = return $ PropId a s
        string2Prop (StringLit a s) = return $ PropString a s
        num2Prop (NumLit a i) = return $ PropNum a i

-- 11.2
memberExpression :: PosParser Expression
memberExpression = functionExpression
                <|>(lexeme $ withPos $ do obj <- memberExpression
                                          plbracket
                                          field <- expression
                                          prbracket
                                          return $ BracketRef def obj field)
                <|>(lexeme $ withPos $ do obj <- memberExpression
                                          pdot
                                          field <- identifierName
                                          return $ DotRef def obj field)
                <|>(lexeme $ withPos $ do knew
                                          ctor <- memberExpression
                                          args <- arguments
                                          return $ NewExpr def ctor args)
                <|>primaryExpression
                                          
newExpression :: PosParser Expression
newExpression = (lexeme $ withPos $ do knew
                                       ctor <- newExpression
                                       args <- arguments
                                       return $ NewExpr def ctor args)
             <|>memberExpression

callExpression :: PosParser Expression
callExpression = (lexeme $ withPos $ do func <- callExpression
                                        args <- arguments
                                        return $ CallExpr def func args)
              <|>(lexeme $ withPos $ do obj <- callExpression
                                        plbracket
                                        field <- expression
                                        prbracket
                                        return $ BracketRef def obj field)
              <|>(lexeme $ withPos $ do obj <- callExpression
                                        pdot
                                        field <- identifierName
                                        prbracket
                                        return $ DotRef def obj field)
              <|>(lexeme $ withPos $ do func <- memberExpression
                                        args <- arguments
                                        return $ CallExpr def func args)

arguments :: Parser [Expression SourceSpan]
arguments = lexeme $ do plbrace
                        args <- assignmentExpression `sepBy` pcomma
                        prbrace
                        return args
                        
leftHandSideExpression :: PosParser Expression
leftHandSideExpression = newExpression <|> callExpression

-- 11.3
postfixExpression :: PosParser Expression
postfixExpression = 
  lexeme $ withPos $ leftHandSideExpression `noLineTerminator` 
  ((pplusplus >> return PostfixInc) <|>
   (pminusminus >> return PostfixDec)) >>= \(e, mIsPlus) ->
  case mIsPlus of
    Nothing -> return e
    Just op -> return $ UnaryAssignExpr def op e

-- 11.4
unaryExpression :: PosParser Expression
unaryExpression = 
  (lexeme $ withPos $ 
       (choice [pplusplus >> return PrefixInc
               ,pminusminus >> return PrefixDec
               ] >>= \op ->
         liftM (UnaryAssignExpr def op) unaryExpression)
   <|> (choice [kdelete >> return PrefixDelete
               ,kvoid   >> return PrefixVoid
               ,ktypeof >> return PrefixTypeof
               ,pplus   >> return PrefixPlus
               ,pminus  >> return PrefixMinus
               ,pbnot   >> return PrefixBNot
               ,pnot    >> return PrefixLNot
               ] >>= \op -> 
       liftM (PrefixExpr def op) unaryExpression))
  <|> postfixExpression

-- 11.5
multiplicativeExpression :: PosParser Expression
multiplicativeExpression = (lexeme $ withPos $ do
                               lhs <- multiplicativeExpression
                               op  <- choice [pmul >> return OpMul
                                             ,pdiv >> return OpDiv
                                             ,pmod >> return OpMod]
                               rhs <- unaryExpression
                               return $ InfixExpr def op lhs rhs)
                        <|>unaryExpression

-- 11.6
additiveExpression :: PosParser Expression
additiveExpression = (lexeme $ withPos $ do
                         lhs <- additiveExpression
                         op  <- choice [pplus >> return OpAdd
                                       ,pminus >> return OpSub]
                         rhs <- multiplicativeExpression
                         return $ InfixExpr def op lhs rhs)
                  <|> multiplicativeExpression

-- 11.7
shiftExpression :: PosParser Expression
shiftExpression = (lexeme $ withPos $ do
                      lhs <- shiftExpression
                      op  <- choice [pshl >> return OpLShift
                                    ,pshr >> return OpSpRShift
                                    ,pushr >> return OpZfRShift]
                      rhs <- additiveExpression
                      return $ InfixExpr def op lhs rhs)
               <|> additiveExpression

-- 11.8
relationalExpression :: Bool -> PosParser Expression
relationalExpression yesIn = 
  (lexeme $ withPos $ do
      let in_ = if yesIn then [kin >> return OpIn] else [] 
      lhs <- relationalExpression yesIn
      op  <- choice $ in_ ++ [plangle >> return OpLT
                             ,prangle >> return OpGT
                             ,pleqt   >> return OpLEq
                             ,pgeqt   >> return OpGEq
                             ,kinstanceof >> return OpInstanceof]
      rhs <- shiftExpression
      return $ InfixExpr def op lhs rhs)
  <|> shiftExpression

-- 11.9
equalityExpression :: Bool -> PosParser Expression
equalityExpression yesIn = (lexeme $ withPos $ do
                               lhs <- equalityExpression yesIn
                               op  <- choice [peq >> return OpEq
                                             ,pneq >> return OpNEq
                                             ,pseq >> return OpStrictEq
                                             ,psneq >> return OpStrictNEq]
                               rhs <- relationalExpression yesIn
                               return $ InfixExpr def op lhs rhs)
                        <|> relationalExpression yesIn

functionExpression :: PosParser Expression
functionExpression = undefined

assignmentExpression :: PosParser Expression
assignmentExpression = undefined

expression :: PosParser Expression
expression = undefined

functionBody :: Parser [Statement SourceSpan]
functionBody = undefined

parseScriptFromString = undefined
parseJavaScriptFromFile = undefined
parseScript = undefined
parseExpression = undefined
parseString = undefined
type ParsedStatement = Statement
type ParsedExpression = Expression
parseSimpleExpr' = undefined
parseBlockStmt = undefined
parseStatement = undefined
type StatementParser = PosParser ParsedStatement
type ExpressionParser = PosParser ParsedExpression
assignExpr = undefined
