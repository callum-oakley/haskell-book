{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cipher where

import Data.Char (ord, chr)

newtype AsciiInt = AsciiInt { unAsciiInt :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Bounded AsciiInt
  where
    -- https://en.wikipedia.org/wiki/ASCII#Printable_characters
    minBound = AsciiInt 0x20
    maxBound = AsciiInt 0x7E

wrapToBounds :: (Bounded a, Integral a) => a -> a
wrapToBounds n = mod (n - minBound) (maxBound + 1 - minBound) + minBound

shift :: Int -> Char -> Char
shift n c = chr $ unAsciiInt $ wrapToBounds $ AsciiInt $ ord c + n

caesar :: Int -> String -> String
caesar = map . shift

unCaesar :: Int -> String -> String
unCaesar = caesar . negate
