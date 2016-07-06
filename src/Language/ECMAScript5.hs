-- |Re-exports commonly used modules.
module Language.ECMAScript5
  ( module Language.ECMAScript5.Syntax
  , module Language.ECMAScript5.Parser
  , module Language.ECMAScript5.PrettyPrint
  , module Language.ECMAScript5.Syntax.Arbitrary
  , module Language.ECMAScript5.Syntax.Annotations
  ) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Arbitrary
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Parser hiding (identifier, objectLiteral)
import Language.ECMAScript5.PrettyPrint
