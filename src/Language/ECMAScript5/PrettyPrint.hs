{-# LANGUAGE FlexibleInstances, UndecidableInstances, CPP #-}

-- |Pretty-printing JavaScript.
module Language.ECMAScript5.PrettyPrint (Pretty (..)
                                        ,unsafeInExprStmt) where

import Text.PrettyPrint.Leijen hiding (Pretty)
import Language.ECMAScript5.Syntax
#if __GLASGOW_HASKELL__ > 708
import Prelude hiding (maybe, (<$>))
#else 
import Prelude hiding (maybe)
#endif

-- | A class of pretty-printable ECMAScript AST nodes.
class Pretty a where
  -- | Pretty-print an ECMAScript AST node. Use 'render' or 'show' to
  -- convert 'Doc' to 'String'.
  prettyPrint :: a -> Doc

instance Pretty (Program a) where
  prettyPrint (Program _ ss) = prettyPrint ss

instance Pretty [Statement a] where 
  prettyPrint = vcat . map prettyPrint

instance Pretty (Expression a) where 
  prettyPrint = ppExpression True

instance Pretty (Statement a) where
  prettyPrint = ppStatement

instance Pretty (CatchClause a) where
  prettyPrint (CatchClause _ id ss) =
    text "catch" <+> (parens.prettyPrint) id <+> asBlock ss
    
instance Pretty (ForInit a) where 
  prettyPrint t = case t of
    NoInit     -> empty
    VarInit vs -> text "var" <+> cat (punctuate comma $ map (ppVarDecl False) vs)
    ExprInit e -> ppExpression False e

instance Pretty (ForInInit a) where
  prettyPrint t = case t of
    ForInVar vd   -> text "var" <+> ppVarDecl False vd
    ForInExpr exp -> ppExpression False exp
    
instance Pretty (VarDecl a) where
  prettyPrint = ppVarDecl True

instance Pretty (CaseClause a) where
  prettyPrint (CaseClause _ e ss) =
    text "case" <+> ppExpression True e <> colon </> nestBlock (prettyPrint ss)
  prettyPrint (CaseDefault _ ss) = text "default:" </> nestBlock (prettyPrint ss)
  
instance Pretty InfixOp where
  prettyPrint = infixOp

instance Pretty AssignOp where
  prettyPrint = assignOp

instance Pretty PrefixOp where
  prettyPrint = prefixOp

instance Pretty (Prop a) where
  prettyPrint p = case p of
    PropId a id -> prettyPrint (Id a id)
    PropString _ str -> ppString str
    PropNum _ n -> prettyPrint n

instance Pretty Number where
  prettyPrint = let p :: (Show a) => a -> Doc
                    p = text . show in either p p

instance Pretty (Id a) where
  prettyPrint (Id _ str) = text str

-- Displays the statement in { ... }, unless it is a block itself.
inBlock:: Statement a -> Doc
inBlock s@(BlockStmt _ _) = prettyPrint s
inBlock s                 = asBlock [s]

asBlock :: [Statement a] -> Doc
asBlock [] = lbrace <$$> rbrace
asBlock ss = lbrace <> line <> (indentBlock $ prettyPrint ss) <$$> rbrace

indentBlock :: Doc -> Doc
indentBlock = indent indentation

indentation = 3

nestBlock :: Doc -> Doc
nestBlock = nest indentation

ppString = dquotes . text . jsEscape



ppVarDecl :: Bool -> VarDecl a -> Doc
ppVarDecl hasIn vd = case vd of
  VarDecl _ id Nothing  -> prettyPrint id
  VarDecl _ id (Just e) -> prettyPrint id <+> equals <+> ppAssignmentExpression hasIn e

ppStatement :: Statement a -> Doc
ppStatement s = case s of
  BlockStmt _ ss -> asBlock ss
  EmptyStmt _ -> semi
  ExprStmt _ e | unsafeInExprStmt (e) -> parens (ppExpression True e) <> semi
  ExprStmt _ e | otherwise            -> ppExpression True e <> semi
  IfStmt _ test cons (EmptyStmt _) -> text "if" <+> 
                                      parens (ppExpression True test) </> 
                                      (nestBlock $ prettyPrint cons)
  IfStmt _ test cons alt -> text "if" <+> parens (ppExpression True test) </> 
                            (nestBlock $ prettyPrint cons) </> text "else"
                            <+> (nestBlock $ prettyPrint alt)
  SwitchStmt _ e cases ->
    text "switch" <+> parens (ppExpression True e) <$> lbrace <> line <>
    nestBlock (vcat (map prettyPrint cases)) <> line <> rbrace
  WhileStmt _ test body -> text "while" <+> parens (ppExpression True test) </>
                           nestBlock (prettyPrint body)
  ReturnStmt _ Nothing -> text "return"
  ReturnStmt _ (Just e) -> text "return" <+> ppExpression True e
  DoWhileStmt _ s e -> 
    text "do" </> prettyPrint s </> text "while" <+> parens (ppExpression True e) <> semi
  BreakStmt _ Nothing ->  text "break" <> semi
  BreakStmt _ (Just label) -> text "break" <+> prettyPrint label <> semi
  ContinueStmt _ Nothing -> text "continue" <> semi
  ContinueStmt _ (Just label) -> text "continue" <+> prettyPrint label <> semi
  LabelledStmt _ label s -> prettyPrint label <> colon <+> prettyPrint s
  ForInStmt p init e body ->
    text "for" <+> 
    parens (prettyPrint init <+> text "in" <+> ppExpression True e) </> 
    prettyPrint body
  ForStmt _ init incr test body ->
    text "for" <+> 
    parens (prettyPrint init <> semi <>
            maybe (\e -> space <> ppExpression True e) incr <> 
            semi <> maybe (\e -> space <> ppExpression True e) test) </> 
    prettyPrint body
  TryStmt _ stmts mcatch mfinally ->
    text "try" </> asBlock stmts </> ppCatch </> ppFinally 
    where ppFinally = case mfinally of
            Nothing -> empty
            Just stmts -> text "finally" <> asBlock stmts
          ppCatch = case mcatch of
            Nothing -> empty
            Just cc -> prettyPrint cc    
  ThrowStmt _ e -> text "throw" <+> ppExpression True e <> semi
  WithStmt _ e s -> text "with" <+> parens (ppExpression True e) </> prettyPrint s
  VarDeclStmt _ decls ->
    text "var" <+> cat (punctuate comma (map (ppVarDecl True) decls)) <> semi
  FunctionStmt _ name args body ->
    text "function" <+> prettyPrint name <> 
    parens (cat $ punctuate comma (map prettyPrint args)) <+>
    asBlock body    
  DebuggerStmt _ -> text "debugger" <> semi

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

-- 11.1
ppPrimaryExpression :: Expression a -> Doc
ppPrimaryExpression e = case e of
  ThisRef _ -> text "this"
  VarRef _ id -> prettyPrint id
  NullLit _ -> text "null"
  BoolLit _ True -> text "true"
  BoolLit _ False -> text "false"
  NumLit  _ (Left i) -> text (show i)
  NumLit  _ (Right d) -> text (show d)
--  IntLit _ n ->  text (show n)
  StringLit _ str -> dquotes (text (jsEscape str))
  RegexpLit _ reg g i m -> text "/" <> (text (jsEscape reg)) <> text "/" <> 
                          (if g then text "g" else empty) <> 
                          (if i then text "i" else empty) <>
                          (if m then text "m" else empty)
  ArrayLit _ es -> 
    brackets $ cat $ punctuate comma (map ppArrayElement es)
  ObjectLit _ pas -> encloseSep lbrace rbrace comma $ map prettyPrint pas
  _ -> parens $ ppExpression True e

ppArrayElement = maybe (ppAssignmentExpression True)

instance Pretty (PropAssign a) where
  prettyPrint pa = case pa of
    PValue _ p e       -> prettyPrint p <> colon <+> ppAssignmentExpression True e
    PGet  _ p body    -> text "get" <+> prettyPrint p <> parens empty <+> asBlock body
    PSet  _ p id body -> text "set" <+> prettyPrint p <> parens (prettyPrint id) <+> asBlock body

-- 11.2
ppMemberExpression :: Expression a -> Doc
ppMemberExpression e = case e of
  FuncExpr _ name params body ->
    text "function" <+> maybe (\n -> prettyPrint n <> space) name <>
    parens (cat $ punctuate comma (map prettyPrint params)) <+>
    asBlock body    
  DotRef _ obj id -> ppObjInDotRef obj ppMemberExpression <> text "." <> prettyPrint id
  BracketRef _ obj key -> 
    ppMemberExpression obj <> brackets (ppExpression True key)  
  NewExpr _ ctor args -> 
    text "new" <+> ppMemberExpression ctor <> ppArguments args
  _ -> ppPrimaryExpression e

ppObjInDotRef :: Expression a -> (Expression a -> Doc) -> Doc
ppObjInDotRef i@(NumLit _ _) _ = parens (ppPrimaryExpression i)
ppObjInDotRef e p              = p e

ppCallExpression :: Expression a -> Doc
ppCallExpression e = case e of
  CallExpr _ f args -> ppCallExpression f <> ppArguments args
  DotRef _ obj id -> ppObjInDotRef obj ppCallExpression <> text "." <> prettyPrint id
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
  AssignExpr _ op l r -> ppExpression False l <+> assignOp op <+> 
                         ppAssignmentExpression hasIn r
  _ -> ppConditionalExpression hasIn e
  
-- 11.14
ppExpression :: Bool -> Expression a -> Doc
ppExpression hasIn e = case e of
  CommaExpr _ es -> cat $ punctuate comma (map (ppExpression hasIn) es)
  _ -> ppAssignmentExpression hasIn e

maybe :: (a -> Doc) -> Maybe a -> Doc
maybe _ Nothing  = empty
maybe f (Just a) = f a
