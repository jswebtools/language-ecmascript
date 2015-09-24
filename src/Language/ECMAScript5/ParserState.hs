{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
module Language.ECMAScript5.ParserState 
       ( Comment(..)
       , SourceSpan(..) 
       , Positioned
       , ParserAnnotation
       , ParserState
       , InParserState
       , HasComments
       , Parser
       , PosParser
       , InParser
       , PosInParser
       , withPos
       , postfixWithPos
       , prefixWithPos
       , infixWithPos
       , getComments
       , allowIn
       , liftIn
       , withIn
       , withNoIn
       , assertInAllowed
       , changeState
       , initialParserState
       , modifyNewLine
       , modifyLabelSet
       , modifyComments
       , clearLabelSet
       , popEnclosing
       , withFreshEnclosing
       , setNewLineState 
       , hadNewLine 
       , hadNoNewLine
       , getEnclosing
       , isIter
       , isIterSwitch
       , HasLabelSet (..)
       , EnclosingStatement (..)
       , spanBegin
       , spanEnd
       , WhiteSpaceState
       , pushEnclosing
       , pushLabel
       ) where 
 
import Text.Parsec hiding (labels) 
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Prim
import Language.ECMAScript5.Syntax hiding (pushEnclosing, pushLabel)
import Language.ECMAScript5.Syntax.Annotations 
import Data.Default.Class 
import Data.Default.Instances.Base 
import Control.Monad.Identity 
import Control.Applicative
import Control.Monad.State (modify)
 
import Data.Data (Data)
import Data.Typeable (Typeable)

type Positioned x = x ParserAnnotation 
 
type Parser   a = forall s. Stream s Identity Char => ParsecT s ParserState Identity a 
type InParser a =  forall s. Stream s Identity Char => ParsecT s InParserState Identity a 
type PosParser x = Parser (Positioned x)
type PosInParser x = InParser (Positioned x)
 
type WhiteSpaceState = (Bool, SourcePos)

data ParserState = ParserState { whiteSpaceState :: WhiteSpaceState, comments :: [Comment], enclosing :: [EnclosingStatement], labelSet :: [Label] }
                 deriving (Show)
data InParserState = InParserState { allowIn :: Bool, baseState :: ParserState } 

instance HasLabelSet ParserState where
  getLabelSet ps = labelSet ps
  setLabelSet ls ps = ps {labelSet = ls}

type Label = String

data SourceSpan =  
  SourceSpan (SourcePos, SourcePos)
  deriving (Data, Typeable)

spanBegin :: SourceSpan -> SourcePos
spanBegin (SourceSpan (b, _)) = b

spanEnd :: SourceSpan -> SourcePos
spanEnd (SourceSpan (_, e)) = e

data Comment  
  = SingleLineComment String  
  | MultiLineComment String  
    deriving (Show, Data, Typeable)
 
class HasWhiteSpacePos a where
  getWhiteSpaceStartPos :: a -> SourcePos

instance HasWhiteSpacePos ParserState where
  getWhiteSpaceStartPos = snd . whiteSpaceState
    
instance HasWhiteSpacePos InParserState where
  getWhiteSpaceStartPos = snd . whiteSpaceState . baseState

class HasComments a where 
  getComments :: a -> [Comment] 
  setComments :: a -> [Comment] -> a 
  modifyComments :: ([Comment] -> [Comment]) -> a -> a 
  modifyComments f st = setComments st (f $ getComments st) 
 
instance HasComments ParserState where 
  getComments = comments 
  setComments st cs = st { comments = cs } 
 
instance HasComments InParserState where 
  getComments = comments . baseState 
  setComments st cs = st { baseState = setComments (baseState st) cs } 
 
type ParserAnnotation = (SourceSpan, [Comment]) 
 
instance Default SourcePos where 
  def = initialPos "" 
 
instance Default SourceSpan where 
  def = SourceSpan def 
 
instance Show SourceSpan where 
  show (SourceSpan (p1,p2)) = let 
    l1 = show $ sourceLine p1 - 1 
    c1 = show $ sourceColumn p1 - 1 
    l2 = show $ sourceLine p2 - 1 
    c2 = show $ sourceColumn p2 - 1 
    s1 = l1 ++ "," ++ c1 
    s2 = l2 ++ "," ++ c2 
    in "(" ++ s1 ++ "--" ++ s2 ++ ")" 
 
consumeComments :: (HasComments state) => Stream s Identity Char => ParsecT s state Identity [Comment] 
consumeComments = do comments <- getComments <$> getState 
                     modifyState $ modifyComments (const []) 
                     return comments 
 
-- a convenience wrapper to take care of the position, "with
-- position". Whenever we return something `Positioned` we need to use
-- it.
withPos   :: (HasAnnotation x, HasComments state, HasWhiteSpacePos state, Stream s Identity Char) 
          => ParsecT s state Identity (Positioned x) 
          -> ParsecT s state Identity (Positioned x) 
withPos p = do start <- getPosition 
               comments <- consumeComments 
               result <- p 
               end <- getWhiteSpaceStartPos <$> getState 
               return $ setAnnotation (SourceSpan (start, end), comments) result 
 
postfixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char) => 
                  ParsecT s state Identity (Positioned x -> Positioned x) ->  
                  ParsecT s state Identity (Positioned x -> Positioned x) 
postfixWithPos p = do 
  f <- p 
  high <- getPosition 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan (low, _), _) = getAnnotation e  
                 in setAnnotation (SourceSpan (low, high), comments) (f e) 
 
prefixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char) => 
                  ParsecT s state Identity (Positioned x -> Positioned x) ->  
                  ParsecT s state Identity (Positioned x -> Positioned x) 
prefixWithPos p = do 
  low <- getPosition 
  f <- p 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan (_, high), _) = getAnnotation e  
                 in setAnnotation (SourceSpan (low, high), comments) (f e) 

infixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char) =>
                ParsecT s state Identity (Positioned x -> Positioned x -> Positioned x) ->
                ParsecT s state Identity (Positioned x -> Positioned x -> Positioned x)
infixWithPos p = 
  liftAnnotations2 combinePos <$> p
  where combinePos (SourceSpan (low, _), _) (SourceSpan (_, high), _) = (SourceSpan (low, high), [])
        liftAnnotations2 f g x y = setAnnotation (f (getAnnotation x) (getAnnotation y)) (g x y)



liftIn :: Bool -> Parser a -> InParser a 
liftIn x p = changeState (InParserState x) baseState p 
 
withIn, withNoIn :: InParser a -> Parser a 
withIn   p = changeState baseState (InParserState True) p 
withNoIn p = changeState baseState (InParserState False) p 
 
assertInAllowed :: InParser () 
assertInAllowed = getState >>= guard.allowIn 
 
changeState 
  :: forall m s u v a . (Functor m, Monad m) 
  => (u -> v) 
  -> (v -> u) 
  -> ParsecT s u m a 
  -> ParsecT s v m a 
changeState forward backward = mkPT . transform . runParsecT 
  where 
    mapState f st = st { stateUser = f (stateUser st) } 
    mapReply f (Ok a st err) = Ok a (mapState f st) err 
    mapReply _ (Error e) = Error e 
    transform p st = (fmap . fmap . fmap) (mapReply forward) (p (mapState backward st)) 
 
modifyNewLine  f st = st { whiteSpaceState = f (whiteSpaceState st) }
modifyEnclosing f st = st { enclosing = f (enclosing st) }
 
initialParserState :: ParserState 
initialParserState = ParserState (False, initialPos "") [] [] []

-- | checks if the label is not yet on the stack, if it is -- throws 
-- an error; otherwise it pushes it onto the stack 
pushLabel :: Id a -> Parser (Id a)
pushLabel ident = do ps <- getState 
                     pos <- getPosition
                     encs <- getEnclosing
                     let lab  = unId ident
                     let labs = labelSet ps ++ concatMap getLabelSet encs
                     if lab `elem` labs 
                       then fail $ "Duplicate label at " ++ show pos 
                       else putState (ps {labelSet = (lab:(labelSet ps))}) >> return ident

modifyLabelSet f a = setLabelSet (f $ getLabelSet a) a

clearLabelSet :: Parser ()
clearLabelSet = modifyState $ modifyLabelSet (const [])

pushEnclosing :: ([Label] -> EnclosingStatement) -> Parser ()
pushEnclosing ctr = do labs <- getLabelSet <$> getState
                       modifyState (modifyEnclosing (ctr labs:))
                       clearLabelSet

popEnclosing :: Parser () 
popEnclosing = modifyState (modifyEnclosing safeTail) 
  where safeTail [] = [] 
        safeTail (_:xs) = xs 

clearEnclosing = modifyEnclosing (const [])

getEnclosing :: Parser [EnclosingStatement]
getEnclosing = enclosing <$> getState

withFreshEnclosing :: Parser a -> Parser a 
withFreshEnclosing p = do oldEnclosing <- getEnclosing
                          modifyState clearEnclosing
                          clearLabelSet
                          a <- p 
                          modifyState $ modifyEnclosing (const oldEnclosing)
                          return a 
 
setNewLineState :: WhiteSpaceState -> Parser WhiteSpaceState
setNewLineState st = do
    modifyState $ modifyNewLine (const st) 
    return st
 
hadNewLine :: Parser () 
hadNewLine = fst . whiteSpaceState <$> getState >>= guard 
 
hadNoNewLine :: Parser () 
hadNoNewLine = fst . whiteSpaceState <$> getState >>= guard.not
