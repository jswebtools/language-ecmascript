{-# LANGUAGE TemplateHaskell #-}
-- | QuickCheck $Arbitrary$ instances for ECMAScript 3 abstract
-- syntax.

module Language.ECMAScript3.Syntax.Arbitrary where

import Language.ECMAScript3.Syntax
import Test.QuickCheck (sized, Gen, vectorOf, choose)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Data.Map hiding (map,null,filter,foldr,toList,singleton)
import Data.List (nub,delete)
import Data.Data
import Data.Char
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Str
import Control.Monad
import Control.Monad.State
import Data.Maybe (maybeToList)
import Test.Feat
import Test.Feat.Class
import Test.Feat.Enumerate
import Test.Feat.Modifiers
import Control.Arrow

fmap concat $ traverse deriveEnumerable 
  [ ''AssignOp
  , ''InfixOp
  , ''UnaryAssignOp
  , ''PrefixOp
  , ''Id
  , ''CaseClause
  , ''CatchClause
  , ''Prop
  , ''LValue
  , ''ForInit
  , ''ForInInit
  , ''VarDecl
  , ''Expression
  , ''Statement
  , ''JavaScript
  ]


instance Arbitrary (AssignOp) where
  arbitrary = sized uniform

instance Arbitrary (InfixOp) where
  arbitrary = sized uniform

instance Arbitrary (UnaryAssignOp) where
  arbitrary = sized uniform
  
instance Arbitrary (PrefixOp) where
  arbitrary = sized uniform

instance (Enumerable a, Arbitrary a) => Arbitrary (Id a) where
  arbitrary = sized uniform >>= fixUp
  shrink (Id a s) = [Id na ns | ns <- shrink s, na <- shrink a]

instance (Enumerable a, Arbitrary a, Data a) => Arbitrary (CaseClause a) where
  arbitrary = sized uniform
  shrink (CaseClause a expr stmts) = 
    [CaseClause na ne ns | na <- shrink a, ne <- shrink expr, ns <- shrink stmts]
  shrink (CaseDefault a stmts) = 
    [CaseDefault na ns | na <- shrink a, ns <- shrink stmts]
                         
instance (Enumerable a, Arbitrary a) => Arbitrary (Prop a) where
  arbitrary = sized uniform
  shrink (PropId a id) = [PropId na nid | nid <- shrink id, na <- shrink a] 
  shrink (PropString a s) = [PropString na ns | ns <- shrink s, na <- shrink a] 
  shrink (PropNum a i) = [PropNum na ni | ni <- shrink i, na <- shrink a] 
  
instance (Data a, Enumerable a, Arbitrary a) => Arbitrary (LValue a) where  
  arbitrary = sized uniform
  shrink (LVar a s) = [LVar na ns | ns <- shrink s, na <- shrink a]
  shrink (LDot a e s) = [LDot na ne ns | ne <- shrink e, ns <-shrink s, na <-shrink a]
  shrink (LBracket a e1 e2) = [LBracket na ne1 ne2 | ne1 <- shrink e1, ne2 <-shrink e2, na <- shrink a]
  
cshrink :: Arbitrary a => [a] -> [a]
cshrink = concat . shrink

instance (Enumerable a, Arbitrary a, Data a) => Arbitrary (Expression a) where
  arbitrary = sized uniform >>= fixUp
  shrink (StringLit a s) = [StringLit na ns | na <- shrink a, ns <- shrink s]
  shrink (RegexpLit a s b1 b2) = [RegexpLit na ns nb1 nb2 | na <- shrink a, nb1 <- shrink b1, nb2 <- shrink b2, ns <- shrink s]
  shrink (NumLit a d) = [NumLit na nd | na <- shrink a, nd <- shrink d]
  shrink (IntLit a i) = [IntLit na ni | na <- shrink a, ni <- shrink i]
  shrink (BoolLit a b) = [BoolLit na nb | na <- shrink a, nb <- shrink b]
  shrink (NullLit a) = [NullLit na | na <- shrink a]
  shrink (ArrayLit a xs) = (cshrink xs) ++ xs ++ [ArrayLit na nxs | na <- shrink a, nxs <- shrink xs]
  shrink (ObjectLit a xs) =  
    let es = map snd xs in
    (cshrink es) ++ es ++
    [ObjectLit na nxs | na <- shrink a, nxs <- shrink xs]
  shrink (ThisRef a) = [ThisRef na | na <- shrink a]
  shrink (VarRef a id) = [VarRef na nid | na <- shrink a, nid <- shrink id]
  shrink (DotRef a e id) = [DotRef na ne nid | na <-shrink a, nid <- shrink id,  ne <- shrink e]
  shrink (BracketRef a o k) = [BracketRef na no nk | na <- shrink a, no <-shrink o, nk <- shrink k]
  shrink (NewExpr a c xs) = (shrink c) ++ [c] ++ (cshrink xs) ++ xs ++ [NewExpr na nc nxs | na <- shrink a, nc <- shrink c,  nxs <- shrink xs]
  shrink (PrefixExpr a op e) = (shrink e) ++ [e] ++ [PrefixExpr na nop ne | na <- shrink a, nop <-shrink op, ne <- shrink e]
  shrink (UnaryAssignExpr a op v) = [UnaryAssignExpr na nop nv | na <- shrink a, nop <- shrink op, nv <- shrink v]
  shrink (InfixExpr a op e1 e2) = (shrink e1) ++ [e1] ++ (shrink e2) ++ [e2] ++ [InfixExpr na nop ne1 ne2 | na <- shrink a, nop <- shrink op, ne1 <- shrink e1, ne2 <- shrink e2]
  shrink (CondExpr a e1 e2 e3) = (shrink e1) ++ [e1] ++ (shrink e2) ++ [e2] ++ (shrink e3) ++ [e3] ++ [CondExpr na ne1 ne2 ne3 | na <- shrink a, ne1 <- shrink e1, ne2 <- shrink e2, ne3 <- shrink e3]
  shrink (AssignExpr a op v e) = (shrink e) ++ [e] ++ [AssignExpr na nop nv ne | na <- shrink a, nop <- shrink op, nv <- shrink v, ne <-shrink e] 
  shrink (ListExpr a es) = (cshrink es) ++ es ++ [ListExpr na nes | na <- shrink a, nes <- shrink es]
  shrink (CallExpr a e es) = (shrink e) ++ [e] ++ (cshrink es) ++ es ++ [CallExpr na ne nes | na <- shrink a, ne <- shrink e, nes <- shrink es]
  shrink (FuncExpr a mid ids s) = [FuncExpr na nmid nids ns | na <- shrink a, nmid <-  shrink mid, nids <- shrink ids, ns <- shrink s]

instance (Enumerable a, Arbitrary a, Data a) => Arbitrary (ForInInit a) where
  arbitrary = sized uniform >>= fixUp
  shrink (ForInVar id) = [ForInVar nid | nid <- shrink id]
  shrink (ForInLVal id) = [ForInLVal nid | nid <- shrink id]
  
instance (Enumerable a, Arbitrary a, Data a) => Arbitrary (ForInit a) where  
  arbitrary = sized uniform >>= fixUp
  shrink (NoInit) = []
  shrink (VarInit vds) = [VarInit nvds | nvds <- shrink vds]
  shrink (ExprInit e) = [ExprInit ne | ne <- shrink e]

instance (Enumerable a, Arbitrary a, Data a) => Arbitrary (CatchClause a) where
  arbitrary = sized uniform >>= fixUp
  shrink (CatchClause a id s) = [CatchClause na nid ns | na <- shrink a, nid <- shrink id, ns <- shrink s]
  
instance (Enumerable a, Arbitrary a, Data a) => Arbitrary (VarDecl a) where
  arbitrary = sized uniform >>= fixUp
  shrink (VarDecl a id me) = [VarDecl na nid nme | na <- shrink a, nid <- shrink id, nme <- shrink me]

instance (Enumerable a, Arbitrary a, Data a) => Arbitrary (Statement a) where
  arbitrary = sized uniform >>= fixUp
  shrink (BlockStmt a body) = emptyStmtShrink a ++ 
                              [BlockStmt as bs | as <- shrink a, bs <- shrink body]
  shrink (EmptyStmt a) = emptyStmtShrink a
  shrink (ExprStmt a e) = emptyStmtShrink a ++ 
                          [ExprStmt as es | as <- shrink a, es <- shrink e]
  shrink (IfStmt a e th el) = emptyStmtShrink a ++
                              [IfStmt as es ths els | as <- shrink a, es <- shrink e, ths <- shrink th, els <- shrink el]
  shrink (IfSingleStmt a e th) = emptyStmtShrink a ++
                                 [IfSingleStmt as es ths | as <- shrink a, es <- shrink e, ths <- shrink th]
  shrink (SwitchStmt a e cases) = emptyStmtShrink a ++
                                  [SwitchStmt as es cs | as <- shrink a, es <-shrink e, cs <- shrink cases] 
  shrink (WhileStmt a e b) = emptyStmtShrink a ++
                             [WhileStmt as es bs | as <- shrink a, es <- shrink e, bs <- shrink b]
  shrink (DoWhileStmt a b e) = emptyStmtShrink a ++  
                               [DoWhileStmt as bs es | as <- shrink a, es <- shrink e, bs <- shrink b]
  shrink (BreakStmt a l) = emptyStmtShrink a ++
                           [BreakStmt as ls | as <- shrink a, ls <- shrink l]
  shrink (ContinueStmt a l) = emptyStmtShrink a ++
                              [ContinueStmt as ls | as <- shrink a, ls <- shrink l]
  shrink (LabelledStmt a l s) = emptyStmtShrink a ++
                                [LabelledStmt as ls ss | as <- shrink a, ls <- shrink l, ss <- shrink s]
  shrink (ForInStmt a i o s) = emptyStmtShrink a ++
                               [ForInStmt as is os ss | as <- shrink a, is <-shrink i, os <-shrink o, ss <- shrink s]
  shrink (ForStmt a i e1 e2 s) = emptyStmtShrink a ++
                                 [ForStmt as is e1s e2s ss | as <- shrink a, is <- shrink i, e1s <- shrink e1, e2s <- shrink e2, ss <- shrink s]
  shrink (TryStmt a b cs mf) = emptyStmtShrink a ++
                               [TryStmt as bs css mfs | as <- shrink a, bs <- shrink b, css <- shrink cs, mfs <- shrink mf]
  shrink (ThrowStmt a e) = emptyStmtShrink a ++
                           [ThrowStmt as es | as <- shrink a, es <- shrink e]
  shrink (ReturnStmt a e) = emptyStmtShrink a ++
                            [ReturnStmt as es | as <- shrink a, es <- shrink e]
  shrink (WithStmt a o s) = emptyStmtShrink a ++
                            [WithStmt as os ss | as <- shrink a, os <- shrink o, ss <- shrink s]
  shrink (VarDeclStmt a vds) = emptyStmtShrink a ++
                               [VarDeclStmt as vdss | as <- shrink a, vdss <- shrink vds]
  shrink (FunctionStmt a n pars b) = emptyStmtShrink a ++
                                     [FunctionStmt as ns parss bs | as <- shrink a, ns <- shrink n, parss <- shrink pars, bs <- shrink b]
    
emptyStmtShrink a = [EmptyStmt a2 | a2 <- shrink a]    

type LabelSubst   = Map (Id ()) (Id ())
emptyConstantPool = Data.Map.empty

instance (Data a, Arbitrary a, Enumerable a) => Arbitrary (JavaScript a) where
  arbitrary = sized uniform >>= fixUp
  shrink (Script a ss) = [Script na nss | na <- shrink a, nss <- shrink ss]


-- | A class of AST elements that need fixup after generation
class Fixable a where
  fixUp :: a -> Gen a

instance (Data a) => Fixable (JavaScript a) where
  fixUp  = transformBiM (return . identifierFixup
                         :: Id a -> Gen (Id a))
                     >=>transformBiM (fixUpFunExpr
                                      :: Expression a -> Gen (Expression a))
                     >=>transformBiM (fixUpListExpr
                                      :: Expression a -> Gen (Expression a))
                     >=>transformBiM (fixUpFunStmt
                                      :: Statement a -> Gen (Statement a))
                     >=>transformBiM (return . fixLValue
                                      :: LValue a -> Gen (LValue a))
                     >=>(\(Script a ss)-> fmap (Script a) $ fixBreakContinue ss)

instance (Data a) => Fixable (Expression a) where
  fixUp = (fixUpFunExpr . transformBi (identifierFixup :: Id a -> Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))
          
instance (Data a) => Fixable (Statement a) where
  fixUp = (fixUpFunStmt . transformBi (identifierFixup :: Id a -> Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))

instance (Data a) => Fixable (CaseClause a) where
  fixUp = transformBiM (return . identifierFixup :: Id a -> Gen (Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))

instance (Data a) => Fixable (CatchClause a) where
  fixUp = transformBiM (return . identifierFixup :: Id a -> Gen (Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))
          
instance (Data a) => Fixable (ForInit a) where
  fixUp = transformBiM (return . identifierFixup :: Id a -> Gen (Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))
          
instance (Data a) => Fixable (ForInInit a) where
  fixUp = transformBiM (return . identifierFixup :: Id a -> Gen (Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))
          
instance (Data a) => Fixable (VarDecl a) where
  fixUp = transformBiM (return . identifierFixup :: Id a -> Gen (Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))

instance Fixable (Id a) where
  fixUp = return . identifierFixup

instance (Data a) => Fixable (Prop a) where
  fixUp = transformBiM (return . identifierFixup :: Id a -> Gen (Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))

instance (Data a) => Fixable (LValue a) where
  fixUp = transformBiM (return . identifierFixup :: Id a -> Gen (Id a))
       >=>transformBiM (fixUpFunExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpListExpr :: Expression a -> Gen (Expression a))
       >=>transformBiM (fixUpFunStmt :: Statement a -> Gen (Statement a))
       >=>transformBiM (return . fixLValue :: LValue a -> Gen (LValue a))
       >=>(return . fixLValue)

fixLValue :: LValue a -> LValue a
fixLValue lv = case lv of
  LVar a n -> LVar a $ identifierNameFixup n
  LDot a o n -> LDot a o $ identifierNameFixup n
  LBracket {} -> lv

fixUpFunExpr :: (Data a) => Expression a -> Gen (Expression a)
fixUpFunExpr e = case e of
  FuncExpr a mid params body -> fmap (FuncExpr a mid params) $ fixBreakContinue body
  _ -> return e

fixUpListExpr :: (Data a) => Expression a -> Gen (Expression a)
fixUpListExpr e = case e of
  ListExpr a [] -> return $ NullLit a
  _ -> return e

fixUpFunStmt :: (Data a) => Statement a -> Gen (Statement a)
fixUpFunStmt s = case s of
  FunctionStmt a id params body -> fmap (FunctionStmt a id params) $ fixBreakContinue body
  _ -> return s

identifierFixup :: Id a -> Id a
identifierFixup (Id a n) = Id a $ identifierNameFixup n

-- | Renames empty identifiers, as well as identifiers that are
-- keywords or future reserved words by prepending a '_' to them. Also
-- substitutes illegal characters with a "_" as well.
identifierNameFixup :: String -> String
identifierNameFixup s =
  let fixStart c = if isValidIdStart c then c else '_'
      fixPart c  = if isValidIdPart c  then c else '_'
  in case s of
    "" -> "_"
    (start:part) -> let fixed_id = (fixStart start):(map fixPart part)
                    in if isReservedWord fixed_id then '_':fixed_id
                       else fixed_id

-- | Fixes an incorrect nesting of break/continue, making the program
-- abide by the ECMAScript spec (page 92): any continue without a
-- label should be nested within an iteration stmt, any continue with
-- a label should be nested in a labeled statement (not necessarily
-- with the same label); any break statement without a label should be
-- nested in an iteration or switch stmt, any break statement with a
-- label should be nested in a labeled statement (not necessarily with
-- the same label). This is done by either assigning a label (from the
-- set of labels in current scope) to a break/continue statement that
-- doesn't have one (or has a label that's not present in the current
-- scope). Additionally, it removes nested labelled statements with
-- duplicate labels (also a requirement imposed by the spec).
fixBreakContinue :: (Data a) => [Statement a] -> Gen [Statement a]
fixBreakContinue = mapM $ \stmt -> evalStateT (fixBC stmt) ([], [])
    where
      fixBC :: Data a => Statement a -> StateT ([String], [EnclosingStatement]) Gen (Statement a)
      fixBC stmt@(LabelledStmt a lab s) =
        do labs <- gets fst
           if (unId lab) `elem` labs
             -- if duplicate label, delete the current statement (but
             -- keep it's child statement)
             then descendM fixBC s
             else pushLabel lab $ descendM fixBC stmt
      fixBC stmt@(BreakStmt a mlab) =
        do encls <- gets snd
           case (mlab, encls) of
             (_, [])  -> return $ EmptyStmt a
             (Nothing, _)  -> if all isIterSwitch encls
                         then return stmt
                         -- if none of the enclosing statements is an
                         -- iteration or switch statement, substitute
                         -- the break statement for an empty statement
                         else return $ EmptyStmt a
             (Just lab@(Id b _), _) ->
               if any (elem (unId lab) . getLabelSet) encls
               then return stmt
               else if all isIterSwitch encls
                    then case concatMap getLabelSet encls of
                      -- if none of the enclosing statements have
                      -- labels, remove the label from the break
                      -- statement
                      [] -> return $ BreakStmt a Nothing
                      -- if some of them have labels, add the first
                      -- label to the break statement
                      ls -> do newLab <- lift $ selectRandomElement ls
                               return $ BreakStmt a $ Just $ Id b newLab
                    -- if none of the enclosing statements is an
                    -- iteration or switch statement, substitute
                    -- the break statement for an empty statement
                    else return $ EmptyStmt a
      fixBC stmt@(ContinueStmt a mlab) =
        do encls <- gets snd
           let enIts = filter isIter encls
           case (mlab, enIts) of
             -- if none of the enclosing statements are
             -- iteration statements, substitute the
             -- continue statement for an empty statement
             (_, [])  -> return $ EmptyStmt a
             (Nothing, _)  -> return stmt
             (Just lab@(Id b _), _) ->
               if any (elem (unId lab) . getLabelSet) enIts
               then return stmt
               else case concatMap getLabelSet enIts of
                      -- if none of the enclosing statements have
                      -- labels, remove the label from the continue
                      -- statement
                      [] -> if not $ null enIts then return $ ContinueStmt a Nothing
                            -- if none of the enclosing statements are
                            -- iteration statements, substitute the
                            -- continue statement for an empty
                            -- statement
                            else return $ EmptyStmt a
                            -- if some of them have labels, add the first
                            -- label to the break statement
                      ls -> do newLab <- lift $ selectRandomElement ls
                               return $ ContinueStmt a (Just $ Id b newLab)
      fixBC s@(WhileStmt {})   = iterCommon s
      fixBC s@(DoWhileStmt {}) = iterCommon s
      fixBC s@(ForStmt {})     = iterCommon s
      fixBC s@(ForInStmt {})   = iterCommon s
      fixBC s@(SwitchStmt {})  = pushEnclosing EnclosingSwitch $ descendM fixBC s
      fixBC s@(BlockStmt {})   = pushEnclosing EnclosingOther $ descendM fixBC s
      fixBC s                  = descendM fixBC s
      iterCommon s             = pushEnclosing EnclosingIter $ descendM fixBC s

-- | choose n elements from a list randomly
rChooseElem :: [a] -> Int -> Gen [a]
rChooseElem xs n | n > 0 && (not $ null xs) = 
  if n >= length xs then return xs
  else (vectorOf n $ choose (0, n-1)) >>=
       (\subst -> return $ foldr (\n ys -> (xs!!n):ys) [] subst)
rChooseElem _  _ = return [] 
      
-- | Selects a random element of the list
selectRandomElement :: [a] -> Gen a
selectRandomElement xs = 
  let l = length xs in
  do n <- arbitrary
     return $ xs !! (n `mod` l - 1)

isSwitchStmt :: Statement a    -> Bool
isSwitchStmt (SwitchStmt _ _ _) = True
isSwitchStmt _                  = False
