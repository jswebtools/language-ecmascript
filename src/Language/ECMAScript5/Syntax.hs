-- |ECMAScript 5 syntax. /Spec/ refers to the ECMA-262 specification,
-- 5th edition.
module Language.ECMAScript5.Syntax (Program(..)
                                   ,unProgram
                                   ,Statement(..)
                                   ,isIterationStmt
                                   ,CaseClause(..)
                                   ,CatchClause(..)
                                   ,ForInit(..)
                                   ,ForInInit(..)
                                   ,VarDecl(..)
                                   ,Expression(..)
                                   ,InfixOp(..)
                                   ,AssignOp(..)
                                   ,Id(..)
                                   ,unId
                                   ,PrefixOp(..)
                                   ,Prop(..)
                                   ,UnaryAssignOp(..)
                                   ,PropAssign(..)
                                   ,Number
                                   ,isValidIdStart
                                   ,isValidIdPart
                                   ,isReservedWord
                                   ,isValid
                                   ,EnclosingStatement(..)
                                   ,isIterSwitch
                                   ,pushLabel
                                   ,isIter
                                   ,pushEnclosing
                                   ,HasLabelSet(..)
                                   ) where

import Text.Parsec.Pos(initialPos,SourcePos) -- used by data JavaScript
import Data.Data(Data)
import Data.Typeable(Typeable)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Default.Class
import Data.Int (Int32)
import Data.Char
import Control.Monad.State
import Data.Generics.Uniplate.Data
import Control.Arrow

-- | The complete program
data Program a = Program a [Statement a] 
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)

instance Default a => Default (Program a) where
  def = Program def []

-- | Projects statements out of a program
unProgram :: Program a -> [Statement a]
unProgram (Program _ stmts) = stmts

data Id a = Id a String 
          deriving (Read,Show,Eq,Ord,Data,Typeable,Functor,Foldable,Traversable)

unId :: Id a -> String
unId (Id _ s) = s

-- | Infix operators: see spec 11.5-11.11
data InfixOp = OpLT -- ^ @<@
             | OpLEq -- ^ @<=@
             | OpGT -- ^ @>@
             | OpGEq -- ^ @>=@
             | OpIn -- ^ @in@
             | OpInstanceof -- ^ @instanceof@
             | OpEq -- ^ @==@
             | OpNEq -- ^ @!=@
             | OpStrictEq -- ^ @===@
             | OpStrictNEq -- ^ @!===@
             | OpLAnd -- ^ @&&@
             | OpLOr -- ^ @||@
             | OpMul -- ^ @*@
             | OpDiv -- ^ @/@
             | OpMod -- ^ @%@
             | OpSub -- ^ @-@
             | OpLShift -- ^ @<<@
             | OpSpRShift -- ^ @>>@
             | OpZfRShift -- ^ @>>>@
             | OpBAnd -- ^ @&@
             | OpBXor -- ^ @^@
             | OpBOr -- ^ @|@
             | OpAdd -- ^ @+@
    deriving (Read,Show,Data,Typeable,Eq,Ord,Enum)

-- | Assignment operators: see spec 11.13
data AssignOp = OpAssign -- ^ simple assignment, @=@
              | OpAssignAdd -- ^ @+=@
              | OpAssignSub -- ^ @-=@
              | OpAssignMul -- ^ @*=@
              | OpAssignDiv -- ^ @/=@
              | OpAssignMod -- ^ @%=@
              | OpAssignLShift -- ^ @<<=@
              | OpAssignSpRShift -- ^ @>>=@
              | OpAssignZfRShift -- ^ @>>>=@
              | OpAssignBAnd -- ^ @&=@
              | OpAssignBXor -- ^ @^=@
              | OpAssignBOr -- ^ @|=@
  deriving (Read,Show,Data,Typeable,Eq,Ord)

-- | Unary assignment operators: see spec 11.3, 11.4.4, 11.4.5
data UnaryAssignOp = PrefixInc -- ^ @++x@
                   | PrefixDec -- ^ @--x@
                   | PostfixInc -- ^ @x++@
                   | PostfixDec -- ^ @x--@
  deriving (Read,Show, Data, Typeable, Eq, Ord)

-- | Prefix operators: see spec 11.4 (excluding 11.4.4, 11.4.5)
data PrefixOp = PrefixLNot -- ^ @!@
              | PrefixBNot -- ^ @~@
              | PrefixPlus -- ^ @+@
              | PrefixMinus -- ^ @-@
              | PrefixTypeof -- ^ @typeof@
              | PrefixVoid -- ^ @void@
              | PrefixDelete -- ^ @delete@
  deriving (Read,Show,Data,Typeable,Eq,Ord)

type Number = Either Int32 Double

-- | Property names in an object initializer: see spec 11.1.5
data Prop a = PropId a String -- ^ property name is an identifier, @foo@
            | PropString a String -- ^ property name is a string, @\"foo\"@
            | PropNum a Number -- ^ property name is a number, @42@
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)

-- | Property assignments, see spec 11.1.5
data PropAssign a = 
  PValue a (Prop a) (Expression a) -- ^ assigns a value to the
                                  -- property, @foo: 42@
  | PGet  a (Prop a) [Statement a] -- ^ defines a getter for a
                                   -- property, @get foo () {42;}@
  | PSet  a (Prop a) (Id a) [Statement a] -- ^ defines a setter for a
                                          -- property, @set foo (x)
                                          -- {baz = x;}@
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)

-- | Expressions, see spec 11
data Expression a
  = StringLit a String -- ^ @\"foo\"@, spec 11.1.3, 7.8
  | RegexpLit a String Bool Bool Bool
    -- ^ @RegexpLit a regexp global? case_insensitive? multiline?@
    -- regular expression, see spec 11.1.3, 7.8
  | NumLit a Number -- ^ @41.99999@, spec 11.1.3, 7.8
  | BoolLit a Bool -- ^ @true@, spec 11.1.3, 7.8
  | NullLit a -- ^ @null@, spec 11.1.3, 7.8
  | ArrayLit a [Maybe (Expression a)] -- ^ @[1,2,3]@, spec 11.1.4
  | ObjectLit a [PropAssign a] -- ^ @{foo:\"bar\", get baz
                                         -- () {42;}}@, spec 11.1.5
  | ThisRef a -- ^ @this@, spec 11.1.1
  | VarRef a (Id a) -- ^ @foo@, spec 11.1.2
  | DotRef a (Expression a) (Id a) -- ^ @foo.bar@, spec 11.2.1
  | BracketRef a (Expression a) {- container -} (Expression a) {- key -} 
    -- ^ @foo[bar@, spec 11.2.1
  | NewExpr a (Expression a) {- constructor -} [Expression a] 
    -- ^ @new foo(bar)@, spec 11.2.2
  | PrefixExpr a PrefixOp (Expression a) 
    -- ^ @\@e@, spec 11.4 (excluding 11.4.4, 111.4.5)
  | UnaryAssignExpr a UnaryAssignOp (Expression a) 
    -- ^ @++x@, @x--@ etc., spec 11.3, 11.4.4, 11.4.5
  | InfixExpr a InfixOp (Expression a) (Expression a) 
    -- ^ @e1\@e2@, spec 11.5, 11.6, 11.7, 11.8, 11.9, 11.10, 11.11
  | CondExpr a (Expression a) (Expression a) (Expression a)
    -- ^ @e1 ? e2 : e3@, spec 11.12
  | AssignExpr a AssignOp (Expression a) (Expression a)
    -- ^ @e1 \@=e2@, spec 11.13
  | CallExpr a (Expression a) [Expression a] -- ^ @f(x,y,z)@, spec 11.2.3
  | CommaExpr a [Expression a] -- ^ @e1, e2@, spec 11.14
  --funcexprs are optionally named
  | FuncExpr a (Maybe (Id a)) [Id a] [Statement a]
    -- ^ @function f (x,y,z) {...}@, spec 11.2.5, 13
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)

-- | Case clauses, spec 12.11
data CaseClause a = CaseClause a (Expression a) [Statement a]
                    -- ^ @case e: stmts;@
                  | CaseDefault a [Statement a]
                    -- ^ @default: stmts;@
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)

-- | Catch clause, spec 12.14
data CatchClause a = CatchClause a (Id a) [Statement a]
                     -- ^ @catch (x) {...}@
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)

-- | A variable declaration, spec 12.2
data VarDecl a = VarDecl a (Id a) (Maybe (Expression a)) 
                 -- ^ @var x = e;@
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)
  
-- | for initializer, spec 12.6
data ForInit a = NoInit -- ^ empty
               | VarInit [VarDecl a] -- ^ @var x, y=42@
               | ExprInit (Expression a) -- ^ @expr@
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)

-- | for..in initializer, spec 12.6
data ForInInit a = ForInVar (VarDecl a) -- ^ @var x@
                 | ForInExpr (Expression a) -- ^ @foo.baz@, @foo[bar]@, @z@
 deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)
  
-- | Statements, spec 12.
data Statement a 
  = BlockStmt a [Statement a] -- ^ @{stmts}@, spec 12.1
  | EmptyStmt a -- ^ @;@, spec 12.3
  | ExprStmt a (Expression a) -- ^ @expr;@, spec 12.4
  | IfStmt a (Expression a) (Statement a) (Statement a) 
    -- ^ @if (e) then els@, spec 12.5
  | SwitchStmt a (Expression a) [CaseClause a]
    -- ^ @switch (e) clauses@, spec 12.11
  | WhileStmt a (Expression a) (Statement a)
    -- ^ @while (e) do stmt@, spec 12.6
  | DoWhileStmt a (Statement a) (Expression a)
    -- ^ @do stmt while (e);@, spec 12.6
  | BreakStmt a (Maybe (Id a)) -- ^ @break lab;@, spec 12.8
  | ContinueStmt a (Maybe (Id a)) -- ^ @continue lab;@, spec 12.7
  | LabelledStmt a (Id a) (Statement a) -- ^ @lab: stmt@, spec 12.12
  | ForInStmt a (ForInInit a) (Expression a) (Statement a) 
    -- ^ @for (x in o) stmt@, spec 12.6
  | ForStmt a (ForInit a)        
              (Maybe (Expression a)) -- test
              (Maybe (Expression a)) -- increment
              (Statement a)          -- body 
    -- ^ @ForStmt a init test increment body@, @for (init; test,
    -- increment) body@, spec 12.6
  | TryStmt a [Statement a] {-body-} (Maybe (CatchClause a))
      (Maybe [Statement a]) {-finally-}
    -- ^ @try stmt catch(x) stmt finally stmt@, spec 12.14
  | ThrowStmt a (Expression a)
    -- ^ @throw expr;@, spec 12.13
  | ReturnStmt a (Maybe (Expression a))
    -- ^ @return expr;@, spec 12.9
  | WithStmt a (Expression a) (Statement a)
    -- ^ @with (o) stmt@, spec 12.10
  | VarDeclStmt a [VarDecl a]
    -- ^ @var x, y=42;@, spec 12.2
  | FunctionStmt a (Id a) {-name-} [Id a] {-args-} [Statement a] {-body-}
    -- ^ @function f(x, y, z) {...}@, spec 13
  | DebuggerStmt a
  deriving (Read,Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable)  

-- | Returns 'True' if the statement is an /IterationStatement/
-- according to spec 12.6.
isIterationStmt :: Statement a -> Bool
isIterationStmt s = case s of
  WhileStmt {}   -> True
  DoWhileStmt {} -> True
  ForStmt {} -> True
  ForInStmt {} -> True
  _                 -> False
  
-- | The ECMAScript standard defines certain syntactic restrictions on
-- programs (or, more precisely, statements) that aren't easily
-- enforced in the AST datatype. These restrictions have to do with
-- labeled statements and break/continue statement, as well as
-- identifier names. Thus, it is possible to manually generate AST's
-- that correspond to syntactically incorrect programs. Use this
-- predicate to check if a 'Program' corresponds to a syntactically
-- correct ECMAScript program.
isValid :: forall a. (Data a, Typeable a) => Program a -> Bool
-- =From ECMA-262-5=
-- A program is considered syntactically incorrect if either of the
-- following is true:
-- * The program contains a continue statement without the optional
-- Identifier, which is not nested, directly or indirectly (but not
-- crossing function boundaries), within an IterationStatement.
-- * The program contains a continue statement with the optional
-- Identifier, where Identifier does not appear in the label set of an
-- enclosing (but not crossing function boundaries) IterationStatement.
-- * The program contains a break statement without the optional
-- Identifier, which is not nested, directly or indirectly (but not
-- crossing function boundaries), within an IterationStatement or a
-- SwitchStatement.
-- * The program contains a break statement with the optional
-- Identifier, where Identifier does not appear in the label set of an
-- enclosing (but not crossing function boundaries) Statement.
-- * The program contains a LabelledStatement that is enclosed by a
-- LabelledStatement with the same Identifier as label. This does not
-- apply to labels appearing within the body of a FunctionDeclaration
-- that is nested, directly or indirectly, within a labelled
-- statement.
-- * The identifiers should be valid. See spec 7.6
isValid js = checkIdentifiers js && checkBreakContinueLabels js
  where checkIdentifiers :: (Data a, Typeable a) => Program a -> Bool
        checkIdentifiers js =
          and $ map isValidIdentifierName [n | (Id _ n) :: Id a <- universeBi js]
        checkBreakContinueLabels js@(Program _ body) = and $ map checkStmt $
           body ++ concat ([body | FuncExpr _ _ _ body <- universeBi js] ++
                           [body | FunctionStmt _ _ _ body <- universeBi js])

checkStmt :: Statement a -> Bool
checkStmt s = evalState (checkStmtM s) ([], [])

checkStmtM :: Statement a -> State ([Label], [EnclosingStatement]) Bool
checkStmtM stmt = case stmt of
  ContinueStmt a mlab -> do
    encls <- gets snd
    let enIts = filter isIter encls
    return $ case mlab of
      Nothing  -> not $ null enIts
      Just lab -> any (elem (unId lab) . getLabelSet) enIts
  BreakStmt a mlab -> do
    encls <- gets snd
    return $ case mlab of
      Nothing  -> any isIterSwitch encls
      Just lab -> any (elem (unId lab) . getLabelSet) encls
  LabelledStmt _ lab s -> do
    labs <- gets fst
    if (unId lab) `elem` labs then return False
      else pushLabel lab $ checkStmtM s
  WhileStmt _ _ s   -> iterCommon s
  DoWhileStmt _ s _ -> iterCommon s
  ForStmt _ _ _ _ s -> iterCommon s
  ForInStmt _ _ _ s -> iterCommon s
  SwitchStmt _ _ cs -> pushEnclosing EnclosingSwitch $ liftM and $ mapM checkCaseM cs
  BlockStmt _ ss -> pushEnclosing EnclosingOther $ liftM and $ mapM checkStmtM ss
  IfStmt _ _ t e -> liftM2 (&&) (checkStmtM t) (checkStmtM e)
  TryStmt _ body mcatch mfinally -> liftM2 (&&) (liftM and $ mapM checkStmtM body) $
    liftM2 (&&) (maybe (return True) checkCatchM mcatch)
                (maybe (return True) (liftM and . mapM checkStmtM) mfinally)
  WithStmt _ _ body -> checkStmtM body
  _ -> return True

iterCommon s = pushEnclosing EnclosingIter $ checkStmtM s

pushEnclosing :: Monad m => ([Label] -> EnclosingStatement)
              -> StateT ([Label], [EnclosingStatement]) m a
              -> StateT ([Label], [EnclosingStatement]) m a
pushEnclosing ctor = bracketState (\(labs, encls) -> ([], ctor labs:encls))

pushLabel :: Monad m => Id b -> StateT ([Label], [EnclosingStatement]) m a
          -> StateT ([Label], [EnclosingStatement]) m a
pushLabel l = bracketState (first (unId l:))

checkCaseM c = let ss = case c of
                     CaseClause _ _ body -> body
                     CaseDefault _ body -> body
               in liftM and $ mapM checkStmtM ss

checkCatchM (CatchClause _ _ body) = liftM and $ mapM checkStmtM body

bracketState :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
bracketState f m = do original <- get
                      modify f
                      rv <- m
                      put original
                      return rv

-- | Checks if an identifier name is valid according to the spec
isValidIdentifier :: Id a -> Bool
isValidIdentifier (Id _ name) = isValidIdentifierName name

-- | Checks if the 'String' represents a valid identifier name
isValidIdentifierName :: String -> Bool
isValidIdentifierName name = case name of
  "" -> False
  (c:cs) -> isValidIdStart c && and (map isValidIdPart cs) && (not $ isReservedWord name)

-- | Checks if a string is in the list of reserved ECMAScript words
isReservedWord :: String -> Bool
isReservedWord = (`elem` reservedWords)
  where reservedWords = keyword ++ futureReservedWord ++ nullKw ++ boolLit
        keyword = ["break", "case", "catch", "continue", "default", "delete"
                  ,"do", "else", "finally", "for", "function", "if", "in"
                  ,"instanceof", "new", "return", "switch", "this", "throw"
                  ,"try", "typeof", "var", "void", "while", "with"]
        futureReservedWord = ["abstract", "boolean", "byte", "char", "class"
                             ,"const", "debugger", "enum", "export", "extends"
                             ,"final", "float", "goto", "implements", "int"
                             ,"interface", "long", "native", "package", "private"
                             ,"protected", "short", "static", "super"
                             ,"synchronized", "throws", "transient", "volatile"]
        nullKw = ["null"]
        boolLit = ["true", "false"]

-- | Checks if a character is valid at the start of an identifier
isValidIdStart :: Char -> Bool
isValidIdStart c = unicodeLetter c || c == '$' || c == '_'
  where unicodeLetter c = case generalCategory c of
          UppercaseLetter -> True
          LowercaseLetter -> True
          TitlecaseLetter -> True
          ModifierLetter  -> True
          OtherLetter     -> True
          LetterNumber    -> True
          _               -> False

-- | Checks if a character is valid in an identifier part
isValidIdPart :: Char -> Bool
isValidIdPart c = isValidIdStart c || isValidIdPartUnicode c
  where isValidIdPartUnicode c = case generalCategory c of
          NonSpacingMark       -> True
          SpacingCombiningMark -> True
          DecimalNumber        -> True
          ConnectorPunctuation -> True
          _                    -> False
        
          
data EnclosingStatement = EnclosingIter [Label]
                          -- ^ The enclosing statement is an iteration statement
                        | EnclosingSwitch [Label]
                          -- ^ The enclosing statement is a switch statement
                        | EnclosingOther [Label]
                          -- ^ The enclosing statement is some other
                          -- statement.  Note, `EnclosingOther` is
                          -- never pushed if the current `labelSet` is
                          -- empty, so the list of labels in this
                          -- constructor should always be non-empty

instance Show EnclosingStatement where
  show (EnclosingIter ls) = "iteration" ++ show ls
  show (EnclosingSwitch ls) = "switch" ++ show ls
  show (EnclosingOther ls) = "statement" ++ show ls

isIter :: EnclosingStatement -> Bool
isIter (EnclosingIter _) = True
isIter _                 = False

isIterSwitch :: EnclosingStatement -> Bool
isIterSwitch (EnclosingIter _)   = True
isIterSwitch (EnclosingSwitch _) = True
isIterSwitch _                   = False

class HasLabelSet a where
  getLabelSet :: a -> [Label]
  setLabelSet :: [Label] -> a -> a

modifyLabelSet :: HasLabelSet a => ([Label] -> [Label]) -> a -> a
modifyLabelSet f a = setLabelSet (f $ getLabelSet a) a

instance HasLabelSet EnclosingStatement where
  getLabelSet e = case e of
    EnclosingIter ls   -> ls
    EnclosingSwitch ls -> ls
    EnclosingOther ls  -> ls
  setLabelSet ls e = case e of
    EnclosingIter _   -> EnclosingIter ls
    EnclosingSwitch _ -> EnclosingSwitch ls
    EnclosingOther _  -> EnclosingOther ls

type Label = String
