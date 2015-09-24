{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Utility combinator functions for simplifying writing programmatic
-- generation of ECMAScript code. Recommended to use together with the
-- -XOverloadedStrings GHC extension.
module Language.ECMAScript5.Syntax.CodeGen where

import Language.ECMAScript5.Syntax
import Data.Default.Class
import Data.String
import Data.Int (Int32)

program :: Default a => [Statement a] -> Program a
program = Program def

instance Default a => IsString (Id a) where
  fromString = Id def

instance Default a => IsString (Prop a) where
  fromString = PropString def

ident :: Default a => String -> Id a
ident = Id def

propId :: Default a => String -> Prop a
propId = PropId def

propS :: Default a => String -> Prop a
propS = PropString def

propN :: Default a => Number -> Prop a
propN = PropNum def

instance Default a => IsString (Expression a) where
  fromString = StringLit def

string :: Default a => String -> Expression a
string = StringLit def

regexp :: Default a => String -> Bool -> Bool -> Bool -> Expression a
regexp = RegexpLit def

bool :: Default a => Bool -> Expression a
bool = BoolLit def

int :: Default a => Int32 -> Expression a
int = NumLit def . Left

decimal :: Default a => Double -> Expression a
decimal = NumLit def . Right

null_ :: Default a => Expression a
null_   = NullLit def

array :: Default a => [Maybe (Expression a)] -> Expression a
array = ArrayLit def

object :: Default a => [PropAssign a] -> Expression a
object = ObjectLit def

get :: Default a => Prop a -> [Statement a] -> PropAssign a
get = PGet def

set :: Default a => Prop a -> Id a -> [Statement a] -> PropAssign a
set = PSet def

value :: Default a => Prop a -> Expression a -> PropAssign a
value = PValue def

this :: Default a => Expression a
this = ThisRef def

var :: Default a => Id a -> Expression a
var = VarRef def

dot :: Default a => Expression a -> Id a -> Expression a
dot = DotRef def

brack :: Default a => Expression a -> Expression a -> Expression a
brack = BracketRef def

new :: Default a => Expression a -> [Expression a] -> Expression a
new = NewExpr def

prefix :: Default a => PrefixOp -> Expression a -> Expression a
prefix = PrefixExpr def

lnot :: Default a => Expression a -> Expression a
lnot = prefix PrefixLNot

bnot :: Default a => Expression a -> Expression a
bnot = prefix PrefixBNot

plus :: Default a => Expression a -> Expression a
plus = prefix PrefixPlus

minus :: Default a => Expression a -> Expression a
minus = prefix PrefixMinus

typeof :: Default a => Expression a -> Expression a
typeof = prefix PrefixTypeof

void :: Default a => Expression a -> Expression a
void = prefix PrefixVoid

delete :: Default a => Expression a -> Expression a
delete = prefix PrefixDelete

uassign :: Default a => UnaryAssignOp -> Expression a -> Expression a
uassign = UnaryAssignExpr def

preinc :: Default a => Expression a -> Expression a
preinc = uassign PrefixInc

predec :: Default a => Expression a -> Expression a
predec = uassign PrefixDec

postinc :: Default a => Expression a -> Expression a
postinc = uassign PostfixInc

postdec :: Default a => Expression a -> Expression a
postdec = uassign PostfixDec

infixe
  :: Default a =>
     InfixOp -> Expression a -> Expression a -> Expression a
infixe = InfixExpr def

lt :: Default a => Expression a -> Expression a -> Expression a
lt = infixe OpLT

le :: Default a => Expression a -> Expression a -> Expression a
le = infixe OpLEq

gt :: Default a => Expression a -> Expression a -> Expression a
gt = infixe OpGT

ge :: Default a => Expression a -> Expression a -> Expression a
ge = infixe OpGEq

in_ :: Default a => Expression a -> Expression a -> Expression a
in_= infixe OpIn

instanceof
  :: Default a => Expression a -> Expression a -> Expression a
instanceof = infixe OpInstanceof

eq :: Default a => Expression a -> Expression a -> Expression a
eq = infixe OpEq

neq :: Default a => Expression a -> Expression a -> Expression a
neq = infixe OpNEq

steq :: Default a => Expression a -> Expression a -> Expression a
steq = infixe OpStrictEq

stneq :: Default a => Expression a -> Expression a -> Expression a
stneq = infixe OpStrictNEq

land :: Default a => Expression a -> Expression a -> Expression a
land = infixe OpLAnd

lor :: Default a => Expression a -> Expression a -> Expression a
lor  = infixe OpLOr

mul :: Default a => Expression a -> Expression a -> Expression a
mul = infixe OpMul

div :: Default a => Expression a -> Expression a -> Expression a
div = infixe OpDiv

mod :: Default a => Expression a -> Expression a -> Expression a
mod = infixe OpMod

sub :: Default a => Expression a -> Expression a -> Expression a
sub = infixe OpSub

lshift :: Default a => Expression a -> Expression a -> Expression a
lshift = infixe OpLShift

srshift
  :: Default a => Expression a -> Expression a -> Expression a
srshift = infixe OpSpRShift

zrshift
  :: Default a => Expression a -> Expression a -> Expression a
zrshift = infixe OpZfRShift

band :: Default a => Expression a -> Expression a -> Expression a
band = infixe OpBAnd

bor :: Default a => Expression a -> Expression a -> Expression a
bor = infixe OpBOr

xor :: Default a => Expression a -> Expression a -> Expression a
xor = infixe OpBXor

add :: Default a => Expression a -> Expression a -> Expression a
add = infixe OpAdd

cond
  :: Default a =>
     Expression a -> Expression a -> Expression a -> Expression a
cond = CondExpr def

assignop :: Default a => AssignOp -> Expression a -> Expression a -> Expression a
assignop = AssignExpr def

assign
  :: Default a => Expression a -> Expression a -> Expression a
assign = assignop OpAssign

assignadd :: Default a => Expression a -> Expression a -> Expression a
assignadd = assignop OpAssignAdd

assignsub :: Default a => Expression a -> Expression a -> Expression a
assignsub = assignop OpAssignSub

assignmul :: Default a => Expression a -> Expression a -> Expression a
assignmul = assignop OpAssignMul

assigndiv :: Default a => Expression a -> Expression a -> Expression a
assigndiv = assignop OpAssignDiv

assignmod :: Default a => Expression a -> Expression a -> Expression a
assignmod = assignop OpAssignMod

assignlshift
  :: Default a => Expression a -> Expression a -> Expression a
assignlshift = assignop OpAssignLShift

assignsrshift
  :: Default a => Expression a -> Expression a -> Expression a
assignsrshift = assignop OpAssignSpRShift

assignzrshift
  :: Default a => Expression a -> Expression a -> Expression a
assignzrshift = assignop OpAssignZfRShift

assignband :: Default a => Expression a -> Expression a -> Expression a
assignband = assignop OpAssignBAnd

assignxor :: Default a => Expression a -> Expression a -> Expression a
assignxor = assignop OpAssignBXor

assignbor :: Default a => Expression a -> Expression a -> Expression a
assignbor = assignop OpAssignBOr

list :: Default a => [Expression a] -> Expression a
list = CommaExpr def

call :: Default a => Expression a -> [Expression a] -> Expression a
call = CallExpr def

func
  :: Default a => Id a -> [Id a] -> [Statement a] -> Expression a
func id = FuncExpr def (Just id)

lambda :: Default a => [Id a] -> [Statement a] -> Expression a
lambda = FuncExpr def Nothing

casee :: Default a => Expression a -> [Statement a] -> CaseClause a
casee = CaseClause def

defaultc :: Default a => [Statement a] -> CaseClause a
defaultc = CaseDefault def

vardecl :: Default a => Id a -> VarDecl a
vardecl id = VarDecl def id Nothing

varinit :: Default a => Id a -> Expression a -> VarDecl a
varinit id = VarDecl def id . Just

block :: Default a => [Statement a] -> Statement a
block = BlockStmt def

empty :: Default a => Statement a
empty = EmptyStmt def

expr :: Default a => Expression a -> Statement a
expr = ExprStmt def

ifte
  :: Default a =>
     Expression a -> Statement a -> Statement a -> Statement a
ifte = IfStmt def

switch
  :: Default a => Expression a -> [CaseClause a] -> Statement a
switch = SwitchStmt def

while :: Default a => Expression a -> Statement a -> Statement a
while = WhileStmt def

dowhile :: Default a => Statement a -> Expression a -> Statement a
dowhile = DoWhileStmt def

break :: Default a => Maybe (Id a) -> Statement a
break = BreakStmt def

continue :: Default a => Maybe (Id a) -> Statement a
continue = ContinueStmt def

label :: Default a => Id a -> Statement a -> Statement a
label = LabelledStmt def

forin
  :: Default a =>
     ForInInit a -> Expression a -> Statement a -> Statement a
forin = ForInStmt def

for
  :: Default a =>
     ForInit a
     -> Maybe (Expression a)
     -> Maybe (Expression a)
     -> Statement a
     -> Statement a
for = ForStmt def

try :: Default a => [Statement a] -> Statement a
try b = TryStmt def b Nothing Nothing

trycatch
  :: Default a =>
     [Statement a] -> (Id a, [Statement a]) -> Maybe [Statement a] -> Statement a
trycatch b (exid, cb) = TryStmt def b (Just $ CatchClause def exid cb)

tryfinally
  :: Default a => [Statement a] -> [Statement a] -> Statement a
tryfinally b f = TryStmt def b Nothing (Just f)

trycatchfinally
  :: Default a =>
     [Statement a] -> (Id a, [Statement a]) -> [Statement a] -> Statement a
trycatchfinally b (exid, cb) fb = TryStmt def b (Just $ CatchClause def exid cb) (Just fb)

throw :: Default a => Expression a -> Statement a
throw = ThrowStmt def

returns :: Default a => Expression a -> Statement a
returns = ReturnStmt def . Just

ret :: Default a => Statement a
ret = ReturnStmt def Nothing

with :: Default a => Expression a -> Statement a -> Statement a
with = WithStmt def

vardecls :: Default a => [VarDecl a] -> Statement a
vardecls = VarDeclStmt def

function
  :: Default a => Id a -> [Id a] -> [Statement a] -> Statement a
function = FunctionStmt def

debugger :: Default a => Statement a
debugger = DebuggerStmt def

-- | Convert an identifier to a String literal
id2string :: Id a -> Expression a
id2string (Id a s) = StringLit a s
