-- |Pretty-printing JavaScript.
module Language.ECMAScript5.PrettyPrint
  ( 
  javaScript
  , renderStatements
  , renderExpression
  ) where

import Text.PrettyPrint.HughesPJ
import Language.ECMAScript5.Syntax
import Prelude hiding (maybe)

-- | Renders a list of statements as a 'String'
renderStatements :: [Statement a] -> String
renderStatements = render . stmtList

-- | Renders a list of statements as a 'String'
renderExpression :: Expression a -> String
renderExpression = render . (ppExpression True)

-- Displays the statement in { ... }, unless it is a block itself.
inBlock:: Statement a -> Doc
inBlock s@(BlockStmt _ _) = ppStatement s
inBlock s                 = asBlock [s]

asBlock :: [Statement a] -> Doc
asBlock ss = lbrace $+$ nest 2 (stmtList ss) $$ rbrace

ppId (Id _ str) = text str

forInit :: ForInit a -> Doc
forInit t = case t of
  NoInit     -> empty
  VarInit vs -> text "var" <+> cat (punctuate comma $ map (ppVarDecl False) vs)
  ExprInit e -> ppExpression False e

forInInit :: ForInInit a -> Doc  
forInInit t = case t of
  ForInVar vd   -> text "var" <+> ppVarDecl False vd
  ForInLVal exp -> ppExpression False exp

caseClause :: CaseClause a -> Doc
caseClause (CaseClause _ e ss) =
  text "case" $+$ ppExpression True e <+> colon $$ nest 2 (stmtList ss)
caseClause (CaseDefault _ ss) =
  text "default:" $$ nest 2 (stmtList ss)

ppVarDecl :: Bool -> VarDecl a -> Doc
ppVarDecl hasIn vd = case vd of
  VarDecl _ id Nothing  -> ppId id
  VarDecl _ id (Just e) -> ppId id <+> equals <+> ppAssignmentExpression hasIn e

ppStatement :: Statement a -> Doc
ppStatement s = case s of
  BlockStmt _ ss -> asBlock ss
  EmptyStmt _ -> semi
  ExprStmt _ e@(CallExpr _ (FuncExpr {}) _ ) -> 
    parens (ppExpression True e) <> semi
  ExprStmt _ e -> ppExpression True e <> semi
  IfSingleStmt _ test cons -> text "if" <+> 
                              parens (ppExpression True test) $$ 
                              ppStatement cons
  IfStmt _ test cons alt -> text "if" <+> parens (ppExpression True test) $$ 
                            ppStatement cons $$ text "else" <+> ppStatement alt
  SwitchStmt _ e cases ->
    text "switch" <+> parens (ppExpression True e) $$ 
    braces (nest 2 (vcat (map caseClause cases)))
  WhileStmt _ test body -> text "while" <+> parens (ppExpression True test) $$
                           ppStatement body
  ReturnStmt _ Nothing -> text "return"
  ReturnStmt _ (Just e) -> text "return" <+> ppExpression True e
  DoWhileStmt _ s e -> 
    text "do" $$ 
    (ppStatement s <+> text "while" <+> parens (ppExpression True e) <> semi)
  BreakStmt _ Nothing ->  text "break" <> semi
  BreakStmt _ (Just label) -> text "break" <+> ppId label <> semi
  ContinueStmt _ Nothing -> text "continue" <> semi
  ContinueStmt _ (Just label) -> text"continue" <+> ppId label <> semi
  LabelledStmt _ label s -> ppId label <> colon $$ ppStatement s
  ForInStmt p init e body -> 
    text "for" <+> 
    parens (forInInit init <+> text "in" <+> ppExpression True e) $+$ 
    ppStatement body
  ForStmt _ init incr test body ->
    text "for" <+> 
    parens (forInit init <> semi <+> maybe (ppExpression True) incr <> 
            semi <+> maybe (ppExpression True) test) $$ 
    ppStatement body
  TryStmt _ stmt mcatch mfinally ->
    text "try" $$ inBlock stmt $$ ppCatch $$ ppFinally 
    where ppFinally = case mfinally of
            Nothing -> empty
            Just stmt -> text "finally" <> inBlock stmt
          ppCatch = case mcatch of
            Nothing -> empty
            Just (CatchClause _ id s) -> 
              text "catch" <+> (parens.ppId) id <+> inBlock s
  ThrowStmt _ e -> text "throw" <+> ppExpression True e <> semi
  WithStmt _ e s -> text "with" <+> parens (ppExpression True e) $$ ppStatement s
  VarDeclStmt _ decls ->
    text "var" <+> cat (punctuate comma (map (ppVarDecl True) decls)) <> semi
  FunctionStmt _ name args body ->
    text "function" <+> ppId name <> 
    parens (cat $ punctuate comma (map ppId args)) $$ 
    asBlock body

stmtList :: [Statement a] -> Doc
stmtList = vcat . map ppStatement

ppProp :: Prop a -> Doc
ppProp p = case p of
  PropId a id -> ppId (Id a id)
  PropString _ str -> doubleQuotes (text (jsEscape str))
  PropNum _ n -> text (show n)

infixOp op = text $ case op of
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


prefixOp op = text $ case op of
  PrefixLNot -> "!"
  PrefixBNot -> "~"
  PrefixPlus -> "+"
  PrefixMinus -> "-"
  PrefixTypeof -> "typeof"
  PrefixVoid -> "void"
  PrefixDelete -> "delete"


assignOp op = text $ case op of
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

ppLValue :: LValue a -> Doc
ppLValue (LVar _ x) = text x
ppLValue (LDot _ e x) = ppMemberExpression e <> text "." <> text x
ppLValue (LBracket _ e1 e2) = ppMemberExpression e1 <> 
                              brackets (ppExpression True e2)

-- 11.1
ppPrimaryExpression :: Expression a -> Doc
ppPrimaryExpression e = case e of
  ThisRef _ -> text "this"
  VarRef _ id -> ppId id
  NullLit _ -> text "null"
  BoolLit _ True -> text "true"
  BoolLit _ False -> text "false"
  NumLit  _ (Left i) -> text (show i)
  NumLit  _ (Right d) -> text (show d)
--  IntLit _ n ->  text (show n)
  StringLit _ str -> doubleQuotes (text (jsEscape str))
  RegexpLit _ reg g i m -> text "/" <> (text (jsEscape reg)) <> text "/" <> 
                          (if g then text "g" else empty) <> 
                          (if i then text "i" else empty) <>
                          (if m then text "m" else empty)
  ArrayLit _ es -> 
    brackets $ cat $ punctuate comma (map ppArrayElement es)
  ObjectLit _ pas -> braces $ hsep $ punctuate comma (map ppPropAssign pas)
  _ -> parens $ ppExpression True e

ppArrayElement = maybe (ppAssignmentExpression True)

ppPropAssign :: PropAssign a -> Doc
ppPropAssign pa = case pa of
  PExpr _ p e       -> ppProp p <> colon <+> ppAssignmentExpression True e
  PGet  _ p body    -> ppProp p <> parens empty <+> asBlock body
  PSet  _ p id body -> ppProp p <> parens (ppId id) <+> asBlock body

-- 11.2
ppMemberExpression :: Expression a -> Doc
ppMemberExpression e = case e of
  FuncExpr _ name params body -> 
    text "function" <+> maybe ppId name <+>
    parens (cat $ punctuate comma (map ppId params)) $$ 
    asBlock body
  DotRef _ obj id -> ppMemberExpression obj <> text "." <> ppId id
  BracketRef _ obj key -> 
    ppMemberExpression obj <> brackets (ppExpression True key)  
  NewExpr _ ctor args -> 
    text "new" <+> ppMemberExpression ctor <> ppArguments args
  _ -> ppPrimaryExpression e

ppCallExpression :: Expression a -> Doc
ppCallExpression e = case e of
  CallExpr _ f args -> ppCallExpression f <> ppArguments args
  DotRef _ obj id -> ppCallExpression obj <> text "." <> ppId id
  BracketRef _ obj key ->ppCallExpression obj <> brackets (ppExpression True key)
  _ -> ppMemberExpression e  
    
ppArguments :: [Expression a] -> Doc
ppArguments es = 
  parens $ cat $ punctuate comma (map (ppAssignmentExpression True) es)

ppLHSExpression :: Expression a -> Doc
ppLHSExpression = ppCallExpression

-- 11.3
ppPostfixExpression :: Expression a -> Doc
ppPostfixExpression e = case e of
  UnaryAssignExpr _ PostfixInc e' -> ppLHSExpression e' <> text "++"
  UnaryAssignExpr _ PostfixDec e' -> ppLHSExpression e' <> text "--"
  _ -> ppLHSExpression e
  
-- 11.4
ppUnaryExpression :: Expression a -> Doc
ppUnaryExpression e = case e of
  PrefixExpr _ op e' -> prefixOp op <+> ppUnaryExpression e'
  UnaryAssignExpr _ PrefixInc e' -> text "++" <> ppLHSExpression e'
  UnaryAssignExpr _ PrefixDec e' -> text "--" <> ppLHSExpression e'
  _ -> ppPostfixExpression e

-- 11.5
ppMultiplicativeExpression :: Expression a -> Doc
ppMultiplicativeExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpMul, OpDiv, OpMod] -> 
    ppMultiplicativeExpression e1 <+> infixOp op <+> ppUnaryExpression e2
  _ -> ppUnaryExpression e
  
-- 11.6
ppAdditiveExpression :: Expression a -> Doc
ppAdditiveExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpAdd, OpSub] -> 
    ppAdditiveExpression e1 <+> infixOp op <+> ppMultiplicativeExpression e2
  _ -> ppMultiplicativeExpression e

-- 11.7
ppShiftExpression :: Expression a -> Doc
ppShiftExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpLShift, OpSpRShift, OpZfRShift] -> 
    ppShiftExpression e1 <+> infixOp op <+> ppAdditiveExpression e2  
  _ -> ppAdditiveExpression e

-- 11.8.  
-- | @ppRelationalExpression True@ is RelationalExpression,
-- @ppRelationalExpression False@ is RelationalExpressionNoIn
ppRelationalExpression :: Bool -> Expression a -> Doc
ppRelationalExpression hasIn e = 
  let opsNoIn = [OpLT, OpGT, OpLEq, OpGEq, OpInstanceof]
      ops     = if hasIn then OpIn:opsNoIn else opsNoIn
  in case e of    
    InfixExpr _ op e1 e2 | op `elem` ops -> 
      ppRelationalExpression hasIn e1 <+> infixOp op <+> ppShiftExpression e2
    _ -> ppShiftExpression e
    
-- 11.9
ppEqualityExpression :: Bool -> Expression a -> Doc
ppEqualityExpression hasIn e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpEq, OpNEq, OpStrictEq, OpStrictNEq] ->
    ppEqualityExpression hasIn e1 <+> infixOp op <+> 
    ppRelationalExpression hasIn e2
  _ -> ppRelationalExpression hasIn e
  
-- 11.10
ppBitwiseANDExpression :: Bool -> Expression a -> Doc
ppBitwiseANDExpression hasIn e = case e of
  InfixExpr _ op@OpBAnd e1 e2 -> ppBitwiseANDExpression hasIn e1 <+> 
                                 infixOp op <+>
                                 ppEqualityExpression hasIn e2
  _ -> ppEqualityExpression hasIn e
  
ppBitwiseXORExpression :: Bool -> Expression a -> Doc
ppBitwiseXORExpression hasIn e = case e of
  InfixExpr _ op@OpBXor e1 e2 -> ppBitwiseXORExpression hasIn e1 <+>
                                 infixOp op <+>
                                 ppBitwiseANDExpression hasIn e2
  _ -> ppBitwiseANDExpression hasIn e
  
ppBitwiseORExpression :: Bool -> Expression a -> Doc
ppBitwiseORExpression hasIn e = case e of
  InfixExpr _ op@OpBOr e1 e2 -> ppBitwiseORExpression hasIn e1 <+>
                                infixOp op <+>
                                ppBitwiseXORExpression hasIn e2
  _ -> ppBitwiseXORExpression hasIn e

-- 11.11
ppLogicalANDExpression :: Bool -> Expression a -> Doc
ppLogicalANDExpression hasIn e = case e of
  InfixExpr _ op@OpLAnd e1 e2 -> ppLogicalANDExpression hasIn e1 <+>
                                 infixOp op <+>
                                 ppBitwiseORExpression hasIn e2
  _ -> ppBitwiseORExpression hasIn e                                 
                                 
ppLogicalORExpression :: Bool -> Expression a -> Doc
ppLogicalORExpression hasIn e = case e of
  InfixExpr _ op@OpLOr e1 e2 -> ppLogicalORExpression hasIn e1 <+>
                                infixOp op <+>
                                ppLogicalANDExpression hasIn e2
  _ -> ppLogicalANDExpression hasIn e
  
-- 11.12
ppConditionalExpression :: Bool -> Expression a -> Doc
ppConditionalExpression hasIn e = case e of
  CondExpr _ c et ee -> ppLogicalORExpression hasIn c <+> text "?" <+> 
                        ppAssignmentExpression hasIn et <+> colon <+>
                        ppAssignmentExpression hasIn ee
  _ -> ppLogicalORExpression hasIn e

-- 11.13
ppAssignmentExpression :: Bool -> Expression a -> Doc
ppAssignmentExpression hasIn e = case e of
  AssignExpr _ l op r -> ppExpression False l <+> assignOp op <+> 
                         ppAssignmentExpression hasIn r
  _ -> ppConditionalExpression hasIn e
  
-- 11.14
ppExpression :: Bool -> Expression a -> Doc
ppExpression hasIn e = case e of
  ListExpr _ es -> cat $ punctuate comma (map (ppExpression hasIn) es)
  _ -> ppAssignmentExpression hasIn e

maybe :: (a -> Doc) -> Maybe a -> Doc
maybe _ Nothing  = empty
maybe f (Just a) = f a

-- | Renders a JavaScript program as a document, the show instance of
-- 'Doc' will pretty-print it automatically
javaScript :: JavaScript a -> Doc
javaScript (Script _ ss) = stmtList ss
