{-# LANGUAGE FlexibleInstances, UndecidableInstances, CPP, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}

-- | Pretty-printing JavaScript. Note that one can modify the behavior
-- of the pretty-printer: instead of using the 'Show' instance, use
-- 'renderPretty', 'renderCompact', 'displayDecorated' and 'display'
-- from 'Text.PrettyPrint.Annotated.Leijen'. Some examples below:
--
-- >>> display . renderPretty 0.4 80 . prettyPrint
--     Just pretty print the progra. This is equivalent to the 'Show' instance.
--
-- >>> display . renderCompact . prettyPrint
--     Minified printing
--
-- >>> showAnnotations . renderPretty 1.0 80 . prettyPrint
--     Pretty-print source, with annotations in comments

module Language.ECMAScript5.PrettyPrint (Pretty (..)
                                        ,unsafeInExprStmt) where

import Text.PrettyPrint.Annotated.Leijen hiding (Pretty)
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Prelude hiding (maybe)

-- | Print the document to a string, rendering annotations in JS comments
showAnnotations :: Show a => SimpleDoc a -> String
showAnnotations =
  displayDecorated (\a s ->
                     let sa = show a
                     in if length sa > 0 then "/*" ++ show a ++ "*/ " ++ s else s)

alignedParens :: Doc a -> Doc a
alignedParens = parens . align

-- | A class of pretty-printable ECMAScript AST nodes.
class Pretty s a | s -> a where
  -- | Pretty-print an ECMAScript AST node. Use 'render' or 'show' to
  -- convert 'Doc' to 'String'.
  prettyPrint :: s -> Doc a

instance Pretty (Program a) a where
  prettyPrint (Program _ ss) = vcat $ map prettyPrint ss

instance Pretty (Expression a) a where
  prettyPrint = ppExpression True

instance Pretty (Statement a) a where
  prettyPrint = ppStatement

instance Pretty (CatchClause a) a where
  prettyPrint (CatchClause a id ss) =
    annotate a $ "catch" <+> (alignedParens.prettyPrint) id <+> asBlock ss

instance Pretty (ForInit a) a where
  prettyPrint t = case t of
    NoInit     -> empty
    VarInit vs -> "var" <+> cat (punctuate comma $ map (ppVarDecl False) vs)
    ExprInit e -> ppExpression False e

instance Pretty (ForInInit a) a where
  prettyPrint t = case t of
    ForInVar vd   -> "var" <+> ppVarDecl False vd
    ForInExpr exp -> ppExpression False exp

instance Pretty (VarDecl a) a where
  prettyPrint = ppVarDecl True

instance Pretty (CaseClause a) a where
  prettyPrint c =
    case c of
    CaseClause a e ss -> clause a (text "case" <+> ppExpression True e) ss
    CaseDefault a ss -> clause a (text "default") ss
    where
      clause a cas [] = annotate a $ cas <> colon
      clause a cas ss =
        annotate a $ cas <> colon <> nestBlock (linebreak <> vcat (map prettyPrint ss))

instance Pretty (Prop a) a where
  prettyPrint p = case p of
    PropId a id -> annotate a $ prettyPrint (Id a id)
    PropString a str -> annotate a $ ppString str
    PropNum a n -> annotate a $ prettyNumber n

prettyNumber = let p :: (Show a) => a -> Doc b
                   p = text . show in either p p

instance Pretty (Id a) a where
  prettyPrint (Id a str) = annotate a $ text str

-- Displays the statement in { ... }, unless it is a block itself.
inBlock:: Statement a -> Doc a
inBlock s@(BlockStmt a _) = prettyPrint s
inBlock s                 = asBlock [s]

asBlock :: [Statement a] -> Doc a
asBlock [] = lbrace <$$> rbrace
asBlock ss = lbrace <> line <> (indentBlock $ vcat $ map prettyPrint ss) <$$> rbrace

indentBlock :: Doc a -> Doc a
indentBlock = indent indentation

indentation = 3

nestBlock :: Doc a -> Doc a
nestBlock = nest indentation

nestStmt :: Statement a -> Doc a
nestStmt stmt@BlockStmt {} = prettyPrint stmt -- block has its own indentation
nestStmt stmt = nestBlock $ prettyPrint stmt

ppString = dquotes . text . jsEscape

ppVarDecl :: Bool -> VarDecl a -> Doc a
ppVarDecl hasIn vd = case vd of
  VarDecl a ident Nothing  -> annotate a $ prettyPrint ident
  VarDecl a ident (Just e) ->
      annotate a $ prettyPrint ident <+> equals
      </> alignNonFunctions (ppAssignmentExpression hasIn e)
      where
          alignNonFunctions =
              case e of
              FuncExpr {} -> Prelude.id
              _ -> align

-- | Print a list of items in parenthesis
parenList :: (a -> Doc b) -> [a] -> Doc b
parenList ppElem = encloseSep (text "(") (text ")") comma . map ppElem

isIf :: Statement a -> Bool
isIf IfStmt {} = True
isIf _ = False

ppStatement :: Statement a -> Doc a
ppStatement s = annotate (getAnnotation s) $ case s of
  BlockStmt a ss -> asBlock ss
  EmptyStmt a -> semi
  ExprStmt a e | unsafeInExprStmt e -> alignedParens (nest 4 (ppExpression True e)) <> semi
  ExprStmt _ e | otherwise          -> nest 4 (ppExpression True e) <> semi
  IfStmt _ test cons (EmptyStmt _) -> "if" <+>
                                      alignedParens (ppExpression True test) </>
                                      nestStmt cons
  IfStmt _ test cons alt -> "if" <+> alignedParens (ppExpression True test) </>
                            nestStmt cons </> "else"
                            <+> if isIf alt
                                then prettyPrint alt
                                else nestStmt alt
  SwitchStmt _ e cases ->
    "switch" <+> alignedParens (ppExpression True e) <> line <> lbrace <>
    nestBlock (line <> vcat (map prettyPrint cases)) <> line <> rbrace
  WhileStmt _ test body -> "while" <+> alignedParens (ppExpression True test) </>
                           nestStmt body
  ReturnStmt _ Nothing -> "return"
  ReturnStmt _ (Just e) -> "return" <+> nest 4 (ppExpression True e)
  DoWhileStmt _ s e ->
    "do" </> prettyPrint s </> "while" <+> alignedParens (ppExpression True e) <> semi
  BreakStmt _ Nothing ->  "break" <> semi
  BreakStmt _ (Just label) -> "break" <+> prettyPrint label <> semi
  ContinueStmt _ Nothing -> "continue" <> semi
  ContinueStmt _ (Just label) -> "continue" <+> prettyPrint label <> semi
  LabelledStmt _ label s -> prettyPrint label <> colon <+> prettyPrint s
  ForInStmt p init e body ->
    "for" <+>
    alignedParens (prettyPrint init <+> "in" <+> ppExpression True e) </>
    prettyPrint body
  ForStmt _ init incr test body ->
    "for" <+>
    alignedParens (prettyPrint init <> semi <>
                   maybe (\e -> space <> ppExpression True e) incr <>
                   semi <> maybe (\e -> space <> ppExpression True e) test) </>
    prettyPrint body
  TryStmt _ stmts mcatch mfinally ->
    "try" </> asBlock stmts </> ppCatch </> ppFinally
    where ppFinally = case mfinally of
            Nothing -> empty
            Just stmts -> "finally" <> asBlock stmts
          ppCatch = case mcatch of
            Nothing -> empty
            Just cc -> prettyPrint cc
  ThrowStmt _ e -> "throw" <+> ppExpression True e <> semi
  WithStmt _ e s -> "with" <+> alignedParens (ppExpression True e) </> prettyPrint s
  VarDeclStmt _ decls ->
    "var" <+> cat (punctuate comma (map (ppVarDecl True) decls)) <> semi
  FunctionStmt _ name args body ->
    "function" <+> prettyPrint name <>
    parenList prettyPrint args <+>
    asBlock body
  DebuggerStmt _ -> "debugger" <> semi

-- | A predicate to tell if the expression --when pretty-printed--
-- will begin with "function" or '{' and be thus unsafe to use in an
-- expression statement without wrapping it in '()'.
unsafeInExprStmt :: Expression a -> Bool
-- property: forall e. unsafeInExprStmt(e) <==> prettyPrint(e) begins
-- with "function" or '{'
unsafeInExprStmt = unsafeInExprStmt_ 15
  where unsafeInExprStmt_ prec e =
          case e of
            ObjectLit {} -> True
            DotRef _ obj _ | prec >= 1 -> unsafeInExprStmt_ 1 obj
            BracketRef _ obj _ | prec > 0 -> unsafeInExprStmt_ 1 obj
            UnaryAssignExpr a op lv | (op `elem` [PostfixInc, PostfixDec])
                                      && (prec > 3) -> unsafeInExprStmt_ 2 lv
            InfixExpr _ _ l _ | prec >= 5  -> unsafeInExprStmt_ 5 l
            CondExpr _ c _ _ | prec >= 12 -> unsafeInExprStmt_ 12 c
            AssignExpr _ _ lv _ | prec >= 13 -> unsafeInExprStmt_ 2 lv
            CommaExpr _ (e:_) | prec >= 14 -> unsafeInExprStmt_ 14 e
            CallExpr _ e _ | prec >= 2 -> unsafeInExprStmt_ 2 e
            FuncExpr {} -> True
            _ -> False

infixOp op = case op of
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpAdd -> "+"
  OpSub -> "-"
  OpLShift -> "<<"
  OpSpRShift -> ">>"
  OpZfRShift -> ">>>"
  OpLT -> "<"
  OpLEq -> "<="
  OpGT -> ">"
  OpGEq -> ">="
  OpIn -> "in"
  OpInstanceof -> "instanceof"
  OpEq -> "=="
  OpNEq -> "!="
  OpStrictEq -> "==="
  OpStrictNEq -> "!=="
  OpBAnd -> "&"
  OpBXor -> "^"
  OpBOr -> "|"
  OpLAnd -> "&&"
  OpLOr -> "||"


prefixOp op = case op of
  PrefixLNot -> "!"
  PrefixBNot -> "~"
  PrefixPlus -> "+"
  PrefixMinus -> "-"
  PrefixTypeof -> "typeof"
  PrefixVoid -> "void"
  PrefixDelete -> "delete"


assignOp op = case op of
  OpAssign -> "="
  OpAssignAdd -> "+="
  OpAssignSub -> "-="
  OpAssignMul -> "*="
  OpAssignDiv -> "/="
  OpAssignMod -> "%="
  OpAssignLShift -> "<<="
  OpAssignSpRShift -> ">>="
  OpAssignZfRShift -> ">>>="
  OpAssignBAnd -> "&="
  OpAssignBXor -> "^="
  OpAssignBOr -> "|="

-- Based on:
--   http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide:Literals
jsEscape:: String -> String
jsEscape "" = ""
jsEscape (ch:chs) = sel ch ++ jsEscape chs where
    sel '\b' = "\\b"
    sel '\f' = "\\f"
    sel '\n' = "\\n"
    sel '\r' = "\\r"
    sel '\t' = "\\t"
    sel '\v' = "\\v"
    sel '\'' = "\\'"
    sel '\"' = "\\\""
    sel '\\' = "\\\\"
    sel x    = [x]
    -- We don't have to do anything about \X, \x and \u escape sequences.

-- 11.1
ppPrimaryExpression :: Expression a -> Doc a
ppPrimaryExpression e = case e of
  ThisRef a -> annotate a $ "this"
  VarRef a id -> annotate a $ prettyPrint id
  NullLit a -> annotate a $ "null"
  BoolLit a True -> annotate a "true"
  BoolLit a False -> annotate a $ "false"
  NumLit  a (Left i) -> annotate a $ text (show i)
  NumLit  a (Right d) -> annotate a $ text (show d)
  StringLit a str -> annotate a $ dquotes (text (jsEscape str))
  RegexpLit a reg g i m -> annotate a $  "/" <> (text (jsEscape reg)) <> "/" <>
                          (if g then "g" else empty) <>
                          (if i then "i" else empty) <>
                          (if m then "m" else empty)
  ArrayLit a es -> annotate a $ brackets $ cat $ punctuate comma (map ppArrayElement es)
  ObjectLit a pas ->  annotate a $ encloseSep lbrace rbrace comma $ map prettyPrint pas
  _ -> alignedParens $ ppExpression True e

ppArrayElement = maybe (ppAssignmentExpression True)

instance Pretty (PropAssign a) a where
  prettyPrint pa = annotate (getAnnotation pa) $ case pa of
    PValue _ p e       -> prettyPrint p <> colon <+> ppAssignmentExpression True e
    PGet  _ p body    -> "get" <+> prettyPrint p <> alignedParens empty <+> asBlock body
    PSet  _ p id body -> "set" <+> prettyPrint p <> alignedParens (prettyPrint id) <+> asBlock body

-- 11.2
ppMemberExpression :: Expression a -> Doc a
ppMemberExpression e = case e of
  FuncExpr a name params body ->  annotate a $
    "function" <+> maybe (\n -> prettyPrint n <> space) name <>
    parenList prettyPrint params <+>
    asBlock body
  DotRef a obj id -> annotate a $ ppObjInDotRef obj ppMemberExpression <> "." <> prettyPrint id
  BracketRef a obj key ->  annotate a $
    ppMemberExpression obj <> brackets (ppExpression True key)
  NewExpr a ctor args ->  annotate a $
    "new" <+> ppMemberExpression ctor <> ppArguments args
  _ -> ppPrimaryExpression e

ppObjInDotRef :: Expression a -> (Expression a -> Doc a) -> Doc a
ppObjInDotRef i@(NumLit _ _) _ = annotate (getAnnotation i) $ alignedParens (ppPrimaryExpression i)
ppObjInDotRef e p              = p e

ppCallExpression :: Expression a -> Doc a
ppCallExpression e = case e of
  CallExpr a f args -> annotate a $ ppCallExpression f <> ppArguments args
  DotRef a obj id -> annotate a $ ppObjInDotRef obj ppCallExpression <> "." <> prettyPrint id
  BracketRef a obj key -> annotate a $ ppCallExpression obj <> brackets (ppExpression True key)
  _ -> ppMemberExpression e

ppArguments :: [Expression a] -> Doc a
ppArguments = parenList (ppAssignmentExpression True)

ppLHSExpression :: Expression a -> Doc a
ppLHSExpression = ppCallExpression

-- 11.3
ppPostfixExpression :: Expression a -> Doc a
ppPostfixExpression e = case e of
  UnaryAssignExpr a PostfixInc e' -> annotate a $ ppLHSExpression e' <> "++"
  UnaryAssignExpr a PostfixDec e' -> annotate a $ ppLHSExpression e' <> "--"
  _ -> ppLHSExpression e

-- 11.4
ppUnaryExpression :: Expression a -> Doc a
ppUnaryExpression e = case e of
  PrefixExpr a op e' -> annotate a $ prefixOp op <+> ppUnaryExpression e'
  UnaryAssignExpr a PrefixInc e' -> annotate a $ "++" <> ppLHSExpression e'
  UnaryAssignExpr a PrefixDec e' -> annotate a $ "--" <> ppLHSExpression e'
  _ -> ppPostfixExpression e

-- 11.5
ppMultiplicativeExpression :: Expression a -> Doc a
ppMultiplicativeExpression e = case e of
  InfixExpr a op e1 e2 | op `elem` [OpMul, OpDiv, OpMod] -> annotate a $
    ppMultiplicativeExpression e1 </> infixOp op </> ppUnaryExpression e2
  _ -> ppUnaryExpression e

-- 11.6
ppAdditiveExpression :: Expression a -> Doc a
ppAdditiveExpression e = case e of
  InfixExpr a op e1 e2 | op `elem` [OpAdd, OpSub] -> annotate a $
    ppAdditiveExpression e1 </> infixOp op </> ppMultiplicativeExpression e2
  _ -> ppMultiplicativeExpression e

-- 11.7
ppShiftExpression :: Expression a -> Doc a
ppShiftExpression e = case e of
  InfixExpr a op e1 e2 | op `elem` [OpLShift, OpSpRShift, OpZfRShift] ->
    annotate a $ppShiftExpression e1 </> infixOp op </> ppAdditiveExpression e2
  _ -> ppAdditiveExpression e

-- 11.8.
-- | @ppRelationalExpression True@ is RelationalExpression,
-- @ppRelationalExpression False@ is RelationalExpressionNoIn
ppRelationalExpression :: Bool -> Expression a -> Doc a
ppRelationalExpression hasIn e =
  let opsNoIn = [OpLT, OpGT, OpLEq, OpGEq, OpInstanceof]
      ops     = if hasIn then OpIn:opsNoIn else opsNoIn
  in case e of
    InfixExpr a op e1 e2 | op `elem` ops -> annotate a $
      ppRelationalExpression hasIn e1 </> infixOp op </> ppShiftExpression e2
    _ -> ppShiftExpression e

-- 11.9
ppEqualityExpression :: Bool -> Expression a -> Doc a
ppEqualityExpression hasIn e = case e of
  InfixExpr a op e1 e2 | op `elem` [OpEq, OpNEq, OpStrictEq, OpStrictNEq] ->
    annotate a $ ppEqualityExpression hasIn e1 </> infixOp op </>
                 ppRelationalExpression hasIn e2
  _ -> ppRelationalExpression hasIn e

-- 11.10
ppBitwiseANDExpression :: Bool -> Expression a -> Doc a
ppBitwiseANDExpression hasIn e = case e of
  InfixExpr a op@OpBAnd e1 e2 -> annotate a $
                                 ppBitwiseANDExpression hasIn e1 </>
                                 infixOp op </>
                                 ppEqualityExpression hasIn e2
  _ -> ppEqualityExpression hasIn e

ppBitwiseXORExpression :: Bool -> Expression a -> Doc a
ppBitwiseXORExpression hasIn e = case e of
  InfixExpr a op@OpBXor e1 e2 -> annotate a $
                                 ppBitwiseXORExpression hasIn e1 </>
                                 infixOp op </>
                                 ppBitwiseANDExpression hasIn e2
  _ -> ppBitwiseANDExpression hasIn e

ppBitwiseORExpression :: Bool -> Expression a -> Doc a
ppBitwiseORExpression hasIn e = case e of
  InfixExpr a op@OpBOr e1 e2 -> annotate a $
                                ppBitwiseORExpression hasIn e1 </>
                                infixOp op </>
                                ppBitwiseXORExpression hasIn e2
  _ -> ppBitwiseXORExpression hasIn e

-- 11.11
ppLogicalANDExpression :: Bool -> Expression a -> Doc a
ppLogicalANDExpression hasIn e = case e of
  InfixExpr a op@OpLAnd e1 e2 -> annotate a $
                                 ppLogicalANDExpression hasIn e1 </>
                                 infixOp op </>
                                 ppBitwiseORExpression hasIn e2
  _ -> ppBitwiseORExpression hasIn e

ppLogicalORExpression :: Bool -> Expression a -> Doc a
ppLogicalORExpression hasIn e = case e of
  InfixExpr a op@OpLOr e1 e2 -> annotate a $
                                ppLogicalORExpression hasIn e1 </>
                                infixOp op </>
                                ppLogicalANDExpression hasIn e2
  _ -> ppLogicalANDExpression hasIn e

-- 11.12
ppConditionalExpression :: Bool -> Expression a -> Doc a
ppConditionalExpression hasIn e = case e of
  CondExpr a c et ee -> annotate a $
                        ppLogicalORExpression hasIn c </> "?" <+>
                        ppAssignmentExpression hasIn et </> colon <+>
                        ppAssignmentExpression hasIn ee
  _ -> ppLogicalORExpression hasIn e

-- 11.13
ppAssignmentExpression :: Bool -> Expression a -> Doc a
ppAssignmentExpression hasIn e = case e of
  AssignExpr a op l r -> annotate a $ ppExpression False l </> assignOp op </>
                         ppAssignmentExpression hasIn r
  _ -> ppConditionalExpression hasIn e

-- 11.14
ppExpression :: Bool -> Expression a -> Doc a
ppExpression hasIn e = case e of
  CommaExpr a es -> annotate a $ cat $ punctuate comma (map (ppExpression hasIn) es)
  _ -> ppAssignmentExpression hasIn e

maybe :: (a -> Doc b) -> Maybe a -> Doc b
maybe _ Nothing  = empty
maybe f (Just a) = f a
