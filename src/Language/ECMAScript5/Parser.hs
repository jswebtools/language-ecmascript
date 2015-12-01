{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}

module Language.ECMAScript5.Parser (parse
                                   , PosParser
                                   , Parser
                                   , ParseError
                                   , SourcePos
                                   , SourceSpan
                                   , Positioned
                                   , expression
                                   , statement
                                   , program
                                   , parseFromString
                                   , parseFromFile
                                   ) where



import Language.ECMAScript5.Lexer
import Language.ECMAScript5.ParserState

import Language.ECMAScript5.Syntax hiding (pushEnclosing, pushLabel)
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Parser.Util
import Language.ECMAScript5.Parser.Unicode
import Data.Default.Class
import Data.Default.Instances.Base
import Text.Parsec hiding (parse, spaces)
import Text.Parsec.Char (char, string, satisfy, oneOf, noneOf, hexDigit, anyChar)
import Text.Parsec.Char as ParsecChar hiding (spaces)
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Pos
import Text.Parsec.Expr
import Text.Parsec.Error (Message (..))

import Control.Monad(liftM,liftM2)
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Monad.Error.Class

import Data.Char
import Control.Monad.Identity
import Data.Maybe (isJust, isNothing, fromMaybe, maybeToList)
import Control.Applicative ((<$>), (<*), (*>), (<*>), (<$))
import Control.Arrow
import Data.Maybe (fromJust, isNothing)

-- | 7.9. Automatic Semicolon Insertion algorithm, rule 1; to be used
-- in place of `semi` in parsers for emptyStatement,
-- variableStatement, expressionStatement, doWhileStatement,
-- continuteStatement, breakStatement, returnStatement and
-- throwStatement.
autoSemi :: Parser ()
autoSemi = psemi <|> hadNewLine <|> lookAhead prbrace <|> eof

-- 11.1
-- primary expressions
primaryExpression :: PosParser Expression
primaryExpression = choice [ withPos $ ThisRef def <$ kthis
                           , withPos $ VarRef def <$> identifier
                           , literal
                           , arrayLiteral
                           , objectLiteral
                           , parenExpression]

parenExpression :: PosParser Expression
parenExpression = withPos $ inParens expression

-- 11.1.4
arrayLiteral :: PosParser Expression
arrayLiteral = withPos $
               ArrayLit def
               <$> inBrackets elementsListWithElision

elementsListWithElision :: Parser [Maybe (Positioned Expression)]
elementsListWithElision = optionMaybe assignmentExpression `sepBy` pcomma

-- 11.1.5
objectLiteral :: PosParser Expression
objectLiteral = withPos $
                ObjectLit def
                <$> inBraces (propertyAssignment `sepEndBy` pcomma)


propertyAssignment :: PosParser PropAssign
propertyAssignment = withPos $
                     (do try (sget <* notFollowedBy pcolon)
                         pname <- propertyName
                         prparen
                         plparen
                         body <- inBraces functionBody
                         return $ PGet def pname body)
                  <|>(do try (sset <* notFollowedBy pcolon)
                         pname <- propertyName
                         param <- inParens identifier
                         body <- inBraces functionBody
                         return $ PSet def pname param body)
                  <|>(do pname <- propertyName
                         pcolon
                         e <- assignmentExpression
                         return $ PValue def pname e)

propertyName :: Parser (Positioned Prop)
propertyName = withPos $
                id2Prop     <$> identifierName
            <|> string2Prop <$> stringLiteral
            <|> num2Prop    <$> numericLiteral
  where id2Prop (Id a s)            = PropId a s
        string2Prop (StringLit a s) = PropString a s
        num2Prop (NumLit a i)       = PropNum a i
           
bracketed, dotref, called :: Parser (Positioned Expression -> Positioned Expression)
bracketed = postfixWithPos $ flip (BracketRef def) <$> inBrackets expression
dotref    = postfixWithPos $ flip (DotRef def)     <$> try (pdot *> identifierName)
called    = postfixWithPos $ flip (CallExpr def)   <$> arguments

newExpression :: PosParser Expression
newExpression =
  withPostfix [bracketed, dotref] $
    withPos(NewExpr def <$ knew <*> newExpression <*> option [] arguments)
    <|> primaryExpression
    <|> functionExpression

-- 11.2

arguments :: Parser [Positioned Expression]
arguments = inParens $ assignmentExpression `sepBy` pcomma

leftHandSideExpression :: PosParser Expression
leftHandSideExpression = withPostfix [bracketed, dotref, called] newExpression

functionExpression :: PosParser Expression
functionExpression = withPos $
  FuncExpr def
  <$  kfunction
  <*> optionMaybe identifier
  <*> inParens formalParameterList
  <*> withFreshEnclosing (inBraces functionBody)

assignmentExpressionGen :: PosInParser Expression
assignmentExpressionGen = withPos $
  do l <- logicalOrExpressionGen
     assignment l <|> conditionalExpressionGen l <|> return l
  where
    assignment :: Positioned Expression -> PosInParser Expression
    assignment l =
     do op <- liftIn True assignOp
        unless (validLHS l) $
          fail "Invalid left-hand-side assignment"
        AssignExpr def op l <$> assignmentExpressionGen
        
validLHS :: Expression a -> Bool
validLHS e = case e of
  VarRef {}              -> True
  DotRef {}              -> True
  BracketRef {}          -> True
  _                      -> False

assignOp :: Parser AssignOp
assignOp = choice
  [ OpAssign         <$ passign
  , OpAssignAdd      <$ passignadd
  , OpAssignSub      <$ passignsub
  , OpAssignMul      <$ passignmul
  , OpAssignDiv      <$ passigndiv
  , OpAssignMod      <$ passignmod
  , OpAssignLShift   <$ passignshl
  , OpAssignSpRShift <$ passignshr
  , OpAssignZfRShift <$ passignushr
  , OpAssignBAnd     <$ passignband
  , OpAssignBXor     <$ passignbxor
  , OpAssignBOr      <$ passignbor
  ]

assignmentExpression, assignmentExpressionNoIn :: PosParser Expression
assignmentExpression     = withIn   assignmentExpressionGen
assignmentExpressionNoIn = withNoIn assignmentExpressionGen

conditionalExpressionGen :: Positioned Expression -> PosInParser Expression
conditionalExpressionGen l = 
  CondExpr def l 
  <$  liftIn True pquestion 
  <*> assignmentExpressionGen 
  <*  liftIn True pcolon
  <*> assignmentExpressionGen
  
type InOp s = Operator s InParserState Identity (Positioned Expression)

mkOp :: Show a => Parser a -> InParser a
mkOp p = liftIn True $ try p

makeInfixExpr :: Stream s Identity Char => Parser () -> InfixOp -> InOp s
makeInfixExpr str constr = 
  Infix (infixWithPos $ InfixExpr def constr <$ mkOp str) AssocLeft 
      
inExpr :: Stream s Identity Char => InOp s
inExpr =
  Infix (infixWithPos $ InfixExpr def OpIn <$ (assertInAllowed <* mkOp kin)) AssocLeft

makePostfixExpr :: Stream s Identity Char => Parser () -> UnaryAssignOp -> InOp s
makePostfixExpr str constr =
  Postfix $ postfixWithPos $ UnaryAssignExpr def constr <$ (liftIn True hadNoNewLine >> mkOp str)

makePrefixExpr :: Stream s Identity Char => Parser () -> UnaryAssignOp -> InOp s
makePrefixExpr str constr =
  Prefix $ prefixWithPos $ UnaryAssignExpr def constr <$ mkOp str

makeUnaryExpr pfxs =
  let mkPrefix :: Parser () -> PrefixOp -> InParser (Positioned Expression -> Positioned Expression)
      mkPrefix p op = PrefixExpr def op <$ mkOp p
  in  
    Prefix $ makePrefix (map (prefixWithPos . uncurry mkPrefix) pfxs)

exprTable:: Stream s Identity Char => [[InOp s]]
exprTable =
  [ [ makePostfixExpr pplusplus PostfixInc
    , makePostfixExpr pminusminus PostfixDec
    ]
  , [ makePrefixExpr  pplusplus PrefixInc
    , makePrefixExpr  pminusminus PrefixDec
    ]
  , [ makeUnaryExpr [ (pnot     , PrefixLNot)
                    , (pbnot     , PrefixBNot)
                    , (pplus     , PrefixPlus)
                    , (pminus    , PrefixMinus)
                    , (forget ktypeof, PrefixTypeof)
                    , (forget kvoid  , PrefixVoid)
                    , (forget kdelete, PrefixDelete)
                    ]
    ]
  , [ makeInfixExpr pmul OpMul
    , makeInfixExpr pdiv OpDiv
    , makeInfixExpr pmod OpMod
    ]
  , [ makeInfixExpr pplus OpAdd
    , makeInfixExpr pminus OpSub
    ]
  , [ makeInfixExpr pushr OpZfRShift
    , makeInfixExpr pshr  OpSpRShift
    , makeInfixExpr pshl  OpLShift
    ]
  , [ makeInfixExpr pleqt OpLEq
    , makeInfixExpr plangle OpLT
    , makeInfixExpr pgeqt OpGEq
    , makeInfixExpr prangle  OpGT
    , makeInfixExpr (forget kinstanceof) OpInstanceof
    , inExpr
    ]
  , [ makeInfixExpr pseq OpStrictEq
    , makeInfixExpr psneq OpStrictNEq
    , makeInfixExpr peq  OpEq
    , makeInfixExpr pneq  OpNEq
    ]
  , [ makeInfixExpr pband  OpBAnd ]
  , [ makeInfixExpr pbxor  OpBXor ]
  , [ makeInfixExpr pbor  OpBOr ]
  , [ makeInfixExpr pand OpLAnd ]
  , [ makeInfixExpr por OpLOr ]
  ]


logicalOrExpressionGen :: PosInParser Expression
logicalOrExpressionGen = withPos $
  do inAllowed <- allowIn <$> getState
     buildExpressionParser exprTable (liftIn inAllowed leftHandSideExpression) <?> "simple expression"

-- avoid putting comma expression on everything
-- probably should be binary op in the table
makeExpression :: Default a => [Expression a] -> Expression a
makeExpression [] = error "Unexpected empty expression list in makeExpression"
makeExpression [x] = x
makeExpression xs = CommaExpr def xs

-- | A parser that parses ECMAScript expressions
expression, expressionNoIn :: PosParser Expression
expression     = withPos $ makeExpression <$> assignmentExpression     `sepBy1` pcomma
expressionNoIn = withPos $ makeExpression <$> assignmentExpressionNoIn `sepBy1` pcomma

functionBody :: Parser [Positioned Statement]
functionBody = sourceElements

sourceElements :: Parser [Positioned Statement]
sourceElements = many sourceElement

sourceElement :: PosParser Statement
sourceElement = functionDeclaration <|> parseStatement

functionDeclaration :: PosParser Statement
functionDeclaration = withPos $
  FunctionStmt def
  <$  kfunction
  <*> identifier
  <*> inParens formalParameterList
  <*> withFreshEnclosing (inBraces functionBody)

formalParameterList :: Parser [Positioned Id]
formalParameterList =
  identifier `sepBy` pcomma

parseStatement :: PosParser Statement
parseStatement =
  choice
  [ parseBlock
  , labelledStatement
  , variableStatement
  , expressionStatement
  , ifStatement
  , iterationStatement
  , continueStatement
  , breakStatement
  , returnStatement
  , withStatement
  , switchStatement
  , throwStatement
  , tryStatement
  , debuggerStatement
  , emptyStatement ]

statementList :: Parser [Positioned Statement]
statementList = many (withPos parseStatement)

parseBlock :: PosParser Statement
parseBlock =
  withPos $ inBraces $ (pushEnclosing EnclosingOther) >>
  BlockStmt def <$> statementList <* popEnclosing

variableStatement :: PosParser Statement
variableStatement =
  withPos $
  VarDeclStmt def
  <$  kvar
  <*> variableDeclarationList
  <*  autoSemi

variableDeclarationList :: Parser [Positioned VarDecl]
variableDeclarationList =
  variableDeclaration `sepBy` pcomma

variableDeclaration :: PosParser VarDecl
variableDeclaration =
  withPos $ VarDecl def <$> identifier <*> optionMaybe initializer

initializer :: PosParser Expression
initializer =
  passign *> assignmentExpression

variableDeclarationListNoIn :: Parser [Positioned VarDecl]
variableDeclarationListNoIn =
  variableDeclarationNoIn `sepBy` pcomma

variableDeclarationNoIn :: PosParser VarDecl
variableDeclarationNoIn =
  withPos $ VarDecl def <$> identifier <*> optionMaybe initalizerNoIn

initalizerNoIn :: PosParser Expression
initalizerNoIn =
  passign *> assignmentExpressionNoIn

emptyStatement :: PosParser Statement
emptyStatement =
  withPos $ EmptyStmt def <$ psemi

expressionStatement :: PosParser Statement
expressionStatement =
  withPos $
  notFollowedBy (plbrace <|> forget kfunction)
   >> ExprStmt def
  <$> expression
  <*  autoSemi

ifStatement :: PosParser Statement
ifStatement =
  withPos $
  IfStmt def
  <$  (kif >> pushEnclosing EnclosingOther)
  <*> inParens expression
  <*> parseStatement
  <*> option (EmptyStmt def) (kelse *> parseStatement)
  <*  popEnclosing

iterationStatement :: PosParser Statement
iterationStatement = doStatement <|> whileStatement <|> forStatement

doStatement :: PosParser Statement
doStatement =
  withPos $
  DoWhileStmt def
  <$  (kdo >> pushEnclosing EnclosingIter)
  <*> parseStatement
  <*  kwhile
  <*> inParens expression
  <*  autoSemi
  <*  popEnclosing

whileStatement :: PosParser Statement
whileStatement =
  withPos $
  WhileStmt def
  <$  (kwhile >> pushEnclosing EnclosingIter)
  <*> inParens expression
  <*> parseStatement
  <*  popEnclosing

forStatement :: PosParser Statement
forStatement =
  withPos $
  kfor
  >>  pushEnclosing EnclosingIter
  >>  inParens (forStmt <|> forInStmt)
  <*> parseStatement
  <*  popEnclosing
  where
    forStmt :: Parser (Positioned Statement -> Positioned Statement)
    forStmt =
      ForStmt def
      <$> try (choice [ VarInit <$> (kvar *> variableDeclarationListNoIn)
                 , ExprInit <$> expressionNoIn
                 , return NoInit ]
      <* psemi) <*> optionMaybe expression
      <* psemi  <*> optionMaybe expression
    forInStmt :: Parser (Positioned Statement -> Positioned Statement)
    forInStmt =
      ForInStmt def
      <$> (ForInVar <$> (kvar *> variableDeclarationNoIn) <|>
           ForInExpr <$> leftHandSideExpression )
      <* kin
      <*> expression

restricted :: (HasAnnotation e)
           =>  Parser WhiteSpaceState -> (ParserAnnotation -> a -> e ParserAnnotation) -> Parser a -> Parser a -> Parser (e ParserAnnotation)
restricted keyword constructor null parser =
  withPos $
  do wsSt <- keyword 
     rest <- if fst wsSt
       then null
       else parser
     autoSemi
     return (constructor def rest)

validatedRestricted :: (HasAnnotation e)
                    => Parser WhiteSpaceState
                    -> (ParserAnnotation -> a -> (Parser (e ParserAnnotation)))
                    -> Parser a -> Parser a
                    -> Parser (e ParserAnnotation)
validatedRestricted keyword constructor null parser =
  withPos $
  do wsSt <- keyword 
     rest <- if fst wsSt
       then null
       else parser
     result <- constructor def rest
     autoSemi
     return result


continueStatement :: PosParser Statement
continueStatement =
   validatedRestricted kcontinue constructValidate (return Nothing) (optionMaybe identifier)
 where
   constructValidate a mlab = do 
     encIter <- filter isIter <$> getEnclosing
     case mlab of
       Nothing  -> when (null encIter) $ unexpected
                   "Continue is not nested in an iteration statement"
       Just lab -> unless (any (elem (unId lab) . getLabelSet) encIter) $
                   unexpected $ "Labelled continue is not nested in an iteration\
                                \ statement with the specified label"
     return $ ContinueStmt a mlab

breakStatement :: PosParser Statement
breakStatement = 
    validatedRestricted kbreak constructValidate (return Nothing) (optionMaybe identifier)
  where
    constructValidate a mlab = do
      enc <- getEnclosing
      case mlab of
        Nothing ->
          unless (any isIterSwitch enc) $
          unexpected "Break is not nested in an iteration or switch statement"
        Just lab -> unless (any (elem (unId lab) . getLabelSet) enc) $
                    unexpected "Break is not nested in a statement with the specified label"
      return $ BreakStmt a mlab

throwStatement :: PosParser Statement
throwStatement =
  restricted kthrow ThrowStmt (fail "Illegal newline after throw") expression

returnStatement :: PosParser Statement
returnStatement =
  restricted kreturn ReturnStmt (return Nothing) (optionMaybe expression)

withStatement :: PosParser Statement
withStatement =
  withPos $
  WithStmt def
  <$  (kwith >> pushEnclosing EnclosingOther)
  <*> inParens expression
  <*> parseStatement
  <*  popEnclosing

labelledStatement :: PosParser Statement
labelledStatement =
  withPos $
  LabelledStmt def
  <$> try (identifier <* pcolon >>= pushLabel)
  <*> parseStatement
  <*  clearLabelSet

switchStatement :: PosParser Statement
switchStatement = withPos $
  SwitchStmt def
  <$  (kswitch >> pushEnclosing EnclosingSwitch)
  <*> inParens expression
  <*> caseBlock
  <*  popEnclosing
  where
    makeCaseClauses cs d cs2 = cs ++ maybeToList d ++ cs2
    caseBlock =
      inBraces $
      makeCaseClauses
      <$> many caseClause
      <*> optionMaybe defaultClause
      <*> many caseClause
    caseClause :: Parser (Positioned CaseClause)
    caseClause =
      withPos $
      CaseClause def
      <$  kcase
      <*> expression <* pcolon
      <*> statementList
    defaultClause :: Parser (Positioned CaseClause)
    defaultClause =
      withPos $
      kdefault <* pcolon
       >> CaseDefault def
      <$> statementList

tryStatement :: PosParser Statement
tryStatement =
  withPos $
  TryStmt def
  <$  ktry
  <*> block
  <*> optionMaybe catch
  <*> optionMaybe finally
  where
    catch :: Parser (Positioned CatchClause)
    catch = withPos $
            CatchClause def
            <$  kcatch
            <*> inParens identifier
            <*> block
    finally :: Parser [Positioned Statement]
    finally = kfinally *>
              block

block :: Parser [Positioned Statement]
block = inBraces statementList

debuggerStatement :: PosParser Statement
debuggerStatement =
  withPos $ DebuggerStmt def <$ kdebugger <* autoSemi

-- | A parser that parses ECMAScript statements
statement :: PosParser Statement
statement = parseStatement

-- | A parser that parses an ECMAScript program.
program :: PosParser Program
program = ws *> withPos (Program def <$> sourceElements) <* eof

-- | Parse from a stream given a parser, same as 'Text.Parsec.parse'
-- in Parsec. We can use this to parse expressions or statements
-- alone, not just programs.
parse :: Stream s Identity Char
      => PosParser x -- ^ The parser to use
      -> SourceName -- ^ Name of the source file
      -> s -- ^ The stream to parse, usually a 'String' or a 'ByteString'
      -> Either ParseError (x ParserAnnotation)
parse p = runParser p initialParserState

-- | A convenience function that takes a filename and tries to parse
-- the file contents an ECMAScript program, it fails with an error
-- message if it can't.
parseFromFile :: (Error e, MonadIO m, MonadError e m) => String -- ^ file name
              -> m (Program ParserAnnotation)
parseFromFile fname =
  liftIO (readFile fname) >>= \source ->
  case parse program fname source of
    Left err -> throwError $ strMsg $ show err
    Right js -> return js

-- | A convenience function that takes a 'String' and tries to parse
-- it as an ECMAScript program:
--
-- > parseFromString = parse program ""
parseFromString :: String -- ^ JavaScript source to parse
                -> Either ParseError (Program ParserAnnotation)
parseFromString = parse program ""
