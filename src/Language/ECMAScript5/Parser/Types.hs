-- | Data type definitions for the parser

{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Language.ECMAScript5.Parser.Types where

import Data.Data                   (Data)
import Data.Default.Class
import Data.Typeable               (Typeable)
import Language.ECMAScript5.Syntax
import Lens.Simple
import Text.Parsec.Pos             (SourcePos, initialPos, sourceColumn,
                                    sourceLine)
import Text.Parsec hiding (labels) 


data Comment
  = SingleLineComment SourceSpan String
  | MultiLineComment SourceSpan String
    deriving (Show, Data, Typeable)

data SourceSpan =
  SourceSpan {spanBegin :: SourcePos
             ,spanEnd   :: SourcePos}
  deriving (Data, Typeable)

-- | Tells if an expression was in parenthesis in the source text
newtype Parenthesized = Parenthesized Bool

instance Default Parenthesized where
  def = Parenthesized False

instance Default SourcePos where
  def = initialPos ""

instance Default SourceSpan where
  def = SourceSpan def def

instance Show SourceSpan where
  show (SourceSpan p1 p2) = let
    l1 = show $ sourceLine p1 - 1
    c1 = show $ sourceColumn p1 - 1
    l2 = show $ sourceLine p2 - 1
    c2 = show $ sourceColumn p2 - 1
    s1 = l1 ++ "-" ++ c1
    s2 = l2 ++ "-" ++ c2
    in "(" ++ show (s1 ++ "/" ++ s2) ++ ")"


data ParserAnnotation = ParserAnnotation {_position     :: SourceSpan
                                         ,_comments :: [Comment]}
                        deriving (Show)
makeLenses ''ParserAnnotation

instance Default ParserAnnotation where
  def = ParserAnnotation def []

data ExpressionAnnotation = ExpressionAnnotation {_parenthesized :: Parenthesized
                                                 ,_parserAnn     :: ParserAnnotation
                                                 }

makeLenses ''ExpressionAnnotation

data WhiteSpaceState = WhiteSpaceState {_hadNewLine :: Bool, _whiteStart :: SourcePos}
                       deriving (Show)
makeLenses ''WhiteSpaceState

instance Default WhiteSpaceState where
  def = WhiteSpaceState False def

data ParserState = ParserState {_whiteSpaceState :: WhiteSpaceState, _currentComments :: [Comment], _enclosing :: [EnclosingStatement], _labelSet :: [Label], _allowIn :: Bool }
                 deriving (Show)

makeLenses ''ParserState

instance Default ParserState where
  def = ParserState def [] [] [] False

type Parser   a = forall s. Stream s Identity Char => ParsecT s ParserState Identity a 
type PosParser x = Parser (Positioned x)

type ExprParser = Parser (Expression ExpressionAnnotation)
 
type Positioned x = x ParserAnnotation 
