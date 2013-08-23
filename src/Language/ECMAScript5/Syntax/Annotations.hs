-- | A few helpers to work with the AST annotations
module Language.ECMAScript5.Syntax.Annotations where

import Language.ECMAScript5.Syntax
import Data.Traversable
import Control.Applicative
import Control.Arrow
import Control.Monad.State hiding (mapM)
import Prelude hiding (mapM)

-- | Removes annotations from a tree
removeAnnotations :: Traversable t => t a -> t ()
removeAnnotations = reannotate (const ())

-- | Changes all the labels in the tree to another one, given by a
-- function.
reannotate :: Traversable t => (a -> b) -> t a -> t b
reannotate f tree = traverse (pure . f) tree ()

-- | add an extra field to the AST labels (the label would look like @
-- (a, b) @)
addExtraAnnotationField :: Traversable t => b -> t a -> t (a, b)
addExtraAnnotationField def t = traverse (\z -> pure (z, def)) t ()

-- | remove an extra field
removeExtraAnnotationField :: Traversable t => t (a, b) -> t a
removeExtraAnnotationField t = traverse (pure . fst) t ()


-- | Assigns unique numeric (Int) ids to each node in the AST. Returns
-- a pair: the tree annotated with UID's and the last ID that was
-- assigned.
assignUniqueIds :: Traversable t => Int -- ^ starting id
                                 -> t a -- ^ tree root
                                 -> (t (a, Int), Int) 
assignUniqueIds first tree =
  (returnA *** \i -> i-1) $ runState (mapM f tree) first
  where f :: a -> State Int (a, Int)
        f a = do i <- get
                 put (i+1)
                 return (a, i)

class HasAnnotation a where
  -- | Returns the annotation of the root of the tree
  getAnnotation :: a b -> b
  -- | Sets the annotation of the root of the tree  
  setAnnotation :: b -> a b -> a b

-- | Modify the annotation of the root node of the syntax tree
withAnnotation :: (HasAnnotation a) => (b -> b) -> a b -> a b
withAnnotation f x = setAnnotation (f $ getAnnotation x) x

instance HasAnnotation Expression where
  getAnnotation e = case e of
   (StringLit a s)              -> a
   (RegexpLit a s g i m)        -> a
   (NumLit a d)                 -> a
--   (IntLit a i)                 -> a
   (BoolLit a b)                -> a
   (NullLit a)                  -> a
   (ArrayLit a exps)            -> a
   (ObjectLit a props)          -> a
   (ThisRef a)                  -> a
   (VarRef a id)                -> a
   (DotRef a exp id)            -> a
   (BracketRef a container key) -> a
   (NewExpr a ctor params)      -> a
   (PrefixExpr a op e)          -> a
   (UnaryAssignExpr a op lv)    -> a
   (InfixExpr a op e1 e2)       -> a
   (CondExpr a g et ef)         -> a
   (AssignExpr a op lv e)       -> a
   (ListExpr a es)              -> a
   (CallExpr a fn params)       -> a
   (FuncExpr a mid args s)      -> a
  setAnnotation a e = case e of
    (StringLit _ s)              -> (StringLit a s)
    (RegexpLit _ s g i m)        -> (RegexpLit a s g i m)
    (NumLit _ d)                 -> (NumLit a d)
--    (IntLit _ i)                 -> (IntLit a i)
    (BoolLit _ b)                -> (BoolLit a b)
    (NullLit _)                  -> (NullLit a)
    (ArrayLit _ exps)            -> (ArrayLit a exps)
    (ObjectLit _ props)          -> (ObjectLit a props)
    (ThisRef _)                  -> (ThisRef a)
    (VarRef _ id)                -> (VarRef a id)
    (DotRef _ exp id)            -> (DotRef a exp id)
    (BracketRef _ container key) -> (BracketRef a container key)
    (NewExpr _ ctor params)      -> (NewExpr a ctor params)
    (PrefixExpr _ op e)          -> (PrefixExpr a op e)
    (UnaryAssignExpr _ op lv)    -> (UnaryAssignExpr a op lv)
    (InfixExpr _ op e1 e2)       -> (InfixExpr a op e1 e2)
    (CondExpr _ g et ef)         -> (CondExpr a g et ef)
    (AssignExpr _ op lv e)       -> (AssignExpr a op lv e)
    (ListExpr _ es)              -> (ListExpr a es)
    (CallExpr _ fn params)       -> (CallExpr a fn params)
    (FuncExpr _ mid args s)      -> (FuncExpr a mid args s)

instance HasAnnotation Statement where
  getAnnotation s = case s of
    BlockStmt a _        -> a
    EmptyStmt a          -> a
    ExprStmt a _         -> a
    IfStmt a _ _ _       -> a
    IfSingleStmt a _ _   -> a
    SwitchStmt a _ _     -> a
    WhileStmt a _ _      -> a
    DoWhileStmt a _ _    -> a
    BreakStmt a _        -> a
    ContinueStmt a _     -> a
    LabelledStmt a _ _   -> a
    ForInStmt a _ _ _    -> a
    ForStmt a _ _ _ _    -> a
    TryStmt a _ _ _      -> a
    ThrowStmt a _        -> a
    ReturnStmt a _       -> a
    WithStmt a _ _       -> a
    VarDeclStmt a _      -> a
    FunctionStmt a _ _ _ -> a
  setAnnotation a s = case s of
    BlockStmt _ ss       -> BlockStmt a ss
    EmptyStmt _          -> EmptyStmt a
    ExprStmt _ e         -> ExprStmt a e
    IfStmt _ g t e       -> IfStmt a g t e
    IfSingleStmt _ g t   -> IfSingleStmt a g t
    SwitchStmt _ g cs    -> SwitchStmt a g cs
    WhileStmt _ g ss     -> WhileStmt a g ss
    DoWhileStmt _ ss g   -> DoWhileStmt a ss g
    BreakStmt _ l        -> BreakStmt a l
    ContinueStmt _ l     -> ContinueStmt a l
    LabelledStmt _ l s   -> LabelledStmt a l s
    ForInStmt _ i o ss   -> ForInStmt a i o ss
    ForStmt _ i t inc ss -> ForStmt a i t inc ss
    TryStmt _ tb mcb mfb -> TryStmt a tb mcb mfb
    ThrowStmt _ e        -> ThrowStmt a e
    ReturnStmt _ e       -> ReturnStmt a e
    WithStmt _ o b       -> WithStmt a o b
    VarDeclStmt _ vds    -> VarDeclStmt a vds
    FunctionStmt _ n as b-> FunctionStmt a n as b    
    
instance HasAnnotation LValue where
  getAnnotation lv = case lv of
    LVar a _ -> a
    LDot a _ _ -> a
    LBracket a _ _ -> a
  setAnnotation a lv = case lv of
    LVar _ n -> LVar a n
    LDot _ o f -> LDot a o f
    LBracket a o fe -> LBracket a o fe    
  
instance HasAnnotation VarDecl where
  getAnnotation (VarDecl a _ _) = a
  setAnnotation a (VarDecl _ vn e) = VarDecl a vn e  

instance HasAnnotation Prop  where
  getAnnotation p = case p of
    PropId a _ -> a
    PropString a _ -> a
    PropNum a _ -> a
  setAnnotation a p = case p of
    PropId _ id -> PropId a id
    PropString _ s -> PropString a s
    PropNum _ n -> PropNum a n    
  
instance HasAnnotation CaseClause where
  getAnnotation c = case c of
    CaseClause a _ _ -> a
    CaseDefault a _ -> a
  setAnnotation a c = case c of
    CaseClause _ e b -> CaseClause a e b
    CaseDefault _ b  -> CaseDefault a b    
    
instance HasAnnotation CatchClause where
  getAnnotation (CatchClause a _ _) = a
  setAnnotation a (CatchClause _ id b) = CatchClause a id b  

instance HasAnnotation Id where
  getAnnotation (Id a _) = a
  setAnnotation a (Id _ s) = Id a s
  
instance HasAnnotation PropAssign where
  getAnnotation p = case p of
    PExpr a _ _   -> a
    PGet  a _ _   -> a
    PSet  a _ _ _ -> a
  setAnnotation a p = case p of
    PExpr _ pr e    -> PExpr a pr e
    PGet  _ pr b    -> PGet a pr b
    PSet  _ pr id b -> PSet a pr id b
