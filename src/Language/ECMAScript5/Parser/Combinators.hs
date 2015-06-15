{- Additional parser combinators -}
module Language.ECMAScript5.Parser.Combinators where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Control.Applicative ((<$>), (<*>), (<$))
import Control.Monad

-- | parses any character in the range [left .. right]
rangeChar :: (Monad m, Stream s m Char)
          => Char -> Char -> ParsecT s st m Char
rangeChar left right = satisfy (\x -> x >= left && x <= right)

-- | parses anything parsable by the first parser, and not the second
butNot :: (Monad m, Stream s m c, Show a, Show a1)
       => ParsecT s st m a -> ParsecT s st m a1 -> ParsecT s st m a
butNot positive negative = do notFollowedBy negative
                              positive

-- | to accomodate the requirement that <CR><LF> should be considered
-- as a single token
stringify :: (Monad m, Stream s m Char)
          => ParsecT s st m Char -> ParsecT s st m String
stringify p = do c <- p
                 return [c]

-- | a parser that forgets the output of the parameter
forget :: ParsecT s st m a -> ParsecT s st m ()
forget = void

-- | concat in a monad
concatM   :: Monad m => m [[a]] -> m [a]
concatM = liftM concat

makePostfix :: Stream s m t => [ParsecT s u m (a -> a)] -> ParsecT s u m (a -> a)
makePostfix ps =
  foldr (flip (.)) id <$> many (choice ps)

makePrefix :: Stream s m t => [ParsecT s u m (a -> a)] -> ParsecT s u m (a -> a)
makePrefix ps =
  foldl (.) id <$> many (choice ps)

withPostfix :: Stream s m t => [ParsecT s u m (b -> b)] -> ParsecT s u m b -> ParsecT s u m b
withPostfix qs p =
  flip ($) <$> p <*> makePostfix qs
withPrefix :: Stream s m t => [ParsecT s u m (b -> b)] -> ParsecT s u m b -> ParsecT s u m b
withPrefix qs p =
  ($) <$> makePrefix qs <*> p

-- | Fail with an error message at a specific position. A wrapper
-- around `newErrorMessage`
posError :: (Monad m, Stream s m t) => Message -> SourcePos -> ParsecT s u m a
posError msg pos = (mkPT . const . return . Empty . return . Error) $ newErrorMessage msg pos



