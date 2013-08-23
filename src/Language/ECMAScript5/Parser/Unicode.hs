{- Definitions of unicode character parsers -}

module Language.ECMAScript5.Parser.Unicode where

import Data.Char
import Text.Parsec hiding (space, letter)

satisfyCategory :: Stream s m Char => [GeneralCategory] -> ParsecT s u m Char
satisfyCategory = satisfy . flip (elem . generalCategory)

--7.1
uZWNJ, uZWJ :: Stream s m Char => ParsecT s u m Char
uZWNJ = char '\x200C'
uZWJ  = char '\x200D' -- the spec mistakenly lists it as \x200C

--7.2
uTAB, uVT, uFF, uSP, uNBSP, uBOM, uUSP  :: Stream s m Char => ParsecT s u m Char
uTAB  = char '\x0009'
uVT   = char '\x000B'
uFF   = char '\x000C'
uSP   = char '\x0020'
uNBSP = char '\x00A0'
uBOM  = char '\xFEFF'
uUSP  = satisfyCategory [Space]

--7.3
uLF, uCR, uLS, uPS :: Stream s m Char => ParsecT s u m Char
uLF = char '\x000A'
uCR = char '\x000D'
uLS = char '\x2028'
uPS = char '\x2029'
uCRLF :: Stream s m Char => ParsecT s u m [Char]
uCRLF = string "\x000D\x000A"

--7.6
unicodeLetter, unicodeCombiningMark, unicodeDigit, unicodeConnectorPunctuation
  :: Stream s m Char => ParsecT s u m Char
unicodeLetter = satisfyCategory
  [ UppercaseLetter
  , LowercaseLetter
  , TitlecaseLetter
  , ModifierLetter
  , OtherLetter
  , LetterNumber ]
unicodeCombiningMark = satisfyCategory
  [ NonSpacingMark
  , SpacingCombiningMark ]
unicodeDigit = satisfyCategory
  [ DecimalNumber ]
unicodeConnectorPunctuation = satisfyCategory
  [ ConnectorPunctuation ]

--7.8.4
--character code definitions
cNUL = '\x0000'
cBS  = '\x0008'
cHT  = '\x0009'
cLF  = '\x000A'
cVT  = '\x000B'
cFF  = '\x000C'
cCR  = '\x000D'
