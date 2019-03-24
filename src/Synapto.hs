module Synapto
  ( ErrorReason
  , Parser(Parser)
  , parse
  , item
  , unit
  , bind
  , failure
  , option
  , (<|>)
  , (<?>)
  , (<??>)
  , satisfy
  , oneOf
  , noneOf
  , chainl1
  , chainl
  , chainr1
  , chainr
  , sepBy1
  , sepBy
  , endBy1
  , endBy
  , between
  , notFollowedBy
  , eof
  , try
  , choice
  , debug
  , char
  , string
  , digit
  , alphabetic
  , alphaNum
  , integer
  , parenthesized
  , doubleQuoted
  , singleQuoted
  , spaces
  , token
  ) where

import           Synapto.Combinators
import           Synapto.Primitives
import           Synapto.Token