-- | Interface to the parser state
{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}

module Language.ECMAScript5.Parser.State 
       ( Comment(..)
       , SourceSpan(..) 
       , Positioned
       , ParserAnnotation
       , ParserState
       , HasComments
       , Parser
       , PosParser
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
       , initialParserState
       , pushLabel
       , clearLabelSet
       , pushEnclosing
       , popEnclosing
       , withFreshEnclosing
       , setNewLineState 
       , expectNewLine 
       , expectNoNewLine
       , getEnclosing
       , HasLabelSet (..)
       , EnclosingStatement (..)
       , spanBegin
       , spanEnd
       , WhiteSpaceState
       ) where 
 
import Text.Parsec hiding (labels) 
import Text.Parsec.Pos (initialPos)
--import Test.Parsec.Prim
import Language.ECMAScript5.Syntax 
import Language.ECMAScript5.Syntax.Annotations 
import Data.Default.Class 
import Data.Default.Instances.Base 
import Control.Monad.Identity 
import Control.Applicative
import Control.Monad.State (modify)
import Language.ECMAScript5.Parser.Types
import Lens.Simple
import Prelude hiding (Span)
import Control.Monad.State.Class

class HasPosition x where
  getPos :: x -> SourceSpan
  setPos :: SourceSpan -> x -> x

modifyPos :: (HasPosition x) => (SourceSpan -> SourceSpan) -> x -> x
modifyPos f x = setPos (f $ getPos x) x

instance HasPosition ParserAnnotation where
  getPos = view position
  setPos = set position

instance HasPosition ExpressionAnnotation where
  getPos = view $ parserAnn . position
  setPos = set $ parserAnn . position

class HasComments x where
  getComments :: x -> [Comment]
  setComments :: [Comment] -> x -> x

instance HasComments ParserAnnotation where
  getComments = view comments
  setComments = set comments

instance HasComments ExpressionAnnotation where
  getComments = view $ parserAnn . comments
  setComments = set $ parserAnn . comments

instance HasComments ParserState where 
  getComments = view currentComments 
  setComments = set currentComments

consumeComments :: Parser [Comment] 
consumeComments = (getComments <$> getState) <* (modifyState (currentComments .~ []))



-- | A convenience wrapper to take care of the position, "with
-- position". Whenever we return something `Positioned` we need to use
-- it.
withPos   :: (HasAnnotation x, HasPosition a, HasComments a) => Parser (x a) -> Parser (x a)
withPos p = do start <- getPosition 
               cmts <- consumeComments 
               result <- p 
               end <- (view $ whiteSpaceState.whiteStart) <$> getState
               return $ withAnnotation (setComments cmts . setPos (SourceSpan start end))
                 result 

postfixWithPos :: (HasAnnotation x, HasPosition a, HasComments a) => Parser (x a -> x a) ->  
                  Parser (x a -> x a)
postfixWithPos p = do 
  f <- p 
  high <- getPosition 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan low _) = getPos $ getAnnotation e  
                 in withAnnotation (setPos (SourceSpan low high) . setComments comments) (f e) 
 
prefixWithPos :: (HasAnnotation x) => Parser (Positioned x -> Positioned x) ->  
                 Parser (Positioned x -> Positioned x) 
prefixWithPos p = do 
  low <- getPosition 
  f <- p 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan _ high) = getPos $ getAnnotation e  
                 in withAnnotation (setPos (SourceSpan low high) . setComments comments) (f e) 

infixWithPos :: (HasAnnotation x) =>
                Parser (Positioned x -> Positioned x -> Positioned x) ->
                Parser (Positioned x -> Positioned x -> Positioned x)
infixWithPos p = 
  liftAnnotations2 combinePos <$> p
  where combinePos an1 an2 =
          let (SourceSpan _ high) = getPos an2
          in modifyPos (\(SourceSpan low _) -> SourceSpan low high) an2
        liftAnnotations2 f g x y = setAnnotation (f (getAnnotation x) (getAnnotation y)) (g x y)


liftIn :: Stream s Identity Char => Bool -> ParsecT s ParserState Identity a
       -> ParsecT s ParserState Identity a
liftIn ain p = do
  oldin <- view allowIn <$> getState
  modifyState (allowIn .~ ain)
  rp <- p
  modifyState (allowIn .~ oldin)
  return rp
 
withIn, withNoIn :: Stream s Identity Char => ParsecT s ParserState Identity a
                 -> ParsecT s ParserState Identity a
withIn   = liftIn True
withNoIn = liftIn False
 
assertInAllowed :: Parser () 
assertInAllowed = view allowIn <$> getState >>= guard  
 
-- changeState 
--   :: forall m s u v a . (Functor m, Monad m) 
--   => (u -> v) 
--   -> (v -> u) 
--   -> ParsecT s u m a 
--   -> ParsecT s v m a 
-- changeState forward backward = mkPT . transform . runParsecT 
--   where 
--     mapState f st = st { stateUser = f (stateUser st) } 
--     mapReply f (Ok a st err) = Ok a (mapState f st) err 
--     mapReply _ (Error e) = Error e 
--     transform p st = (fmap . fmap . fmap) (mapReply forward) (p (mapState backward st)) 
 
initialParserState :: ParserState 
initialParserState = def

-- | checks if the label is not yet on the stack, if it is -- throws 
-- an error; otherwise it pushes it onto the stack 
pushLabel :: Id a -> Parser (Id a)
pushLabel ident = do ps <- getState 
                     pos <- getPosition
                     encs <- getEnclosing
                     let lab  = unId ident
                     let labs = view labelSet ps ++ concatMap getLabelSet encs
                     if lab `elem` labs 
                       then fail $ "Duplicate label at " ++ show pos 
                       else putState (over labelSet (lab:) ps) >> return ident

clearLabelSet :: Parser ()
clearLabelSet = modifyState $ labelSet .~ []

pushEnclosing :: ([Label] -> EnclosingStatement) -> Parser ()
pushEnclosing ctr = do labs <- view labelSet <$> getState
                       modifyState (over enclosing (ctr labs:))
                       clearLabelSet

popEnclosing :: Parser () 
popEnclosing = modifyState (over enclosing safeTail) 
  where safeTail [] = [] 
        safeTail (_:xs) = xs 

clearEnclosing = enclosing .~ []

getEnclosing :: Parser [EnclosingStatement]
getEnclosing = view enclosing <$> getState

withFreshEnclosing :: Parser a -> Parser a 
withFreshEnclosing p = do oldEnclosing <- getEnclosing
                          modifyState clearEnclosing
                          clearLabelSet
                          a <- p 
                          modifyState $ enclosing .~ oldEnclosing
                          return a 
 
setNewLineState :: WhiteSpaceState -> Parser WhiteSpaceState
setNewLineState st =  modifyState (whiteSpaceState .~ st) >> return st
 
expectNewLine :: Parser () 
expectNewLine = (view $ whiteSpaceState.hadNewLine) <$> getState >>= guard 
 
expectNoNewLine :: Parser () 
expectNoNewLine = (view $ whiteSpaceState.hadNewLine) <$> getState >>= guard.not
