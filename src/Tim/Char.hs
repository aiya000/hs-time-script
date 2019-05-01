-- | Kinds of characters
module Tim.Char where

import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import RIO
import Tim.Megaparsec
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data AlphaNumChar = AlphaNumAlpha AlphaChar
                  | AlphaNumDigit DigitChar
  deriving (Show, Eq, Ord)

alphaNumToChar :: AlphaNumChar -> Char
alphaNumToChar (AlphaNumAlpha x) = alphaToChar x
alphaNumToChar (AlphaNumDigit x) = digitToChar x

alphaNumChar :: CodeParsing m => m AlphaNumChar
alphaNumChar =
  AlphaNumAlpha <$> alphaChar <|>
  AlphaNumDigit <$> digitChar

data AlphaChar = AlphaLower LowerChar
               | AlphaUpper UpperChar
  deriving (Show, Eq, Ord)

alphaToChar :: AlphaChar -> Char
alphaToChar (AlphaLower x) = lowerToChar x
alphaToChar (AlphaUpper x) = upperToChar x

charToAlpha :: Char -> Maybe AlphaChar
charToAlpha x = P.parseMaybe alphaChar [x]

alphaChar :: CodeParsing m => m AlphaChar
alphaChar =
  AlphaLower <$> lowerChar <|>
  AlphaUpper <$> upperChar

data UpperChar = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Show, Eq, Ord)

upperToChar :: UpperChar -> Char
upperToChar = fromJust . flip Map.lookup uppers

uppers :: Map UpperChar Char
uppers = Map.fromList
  [ (A, 'A'), (B, 'B'), (C, 'C')
  , (D, 'D'), (E, 'E'), (F, 'F')
  , (G, 'G'), (H, 'H'), (I, 'I')
  , (J, 'J'), (K, 'K'), (L, 'L')
  , (M, 'M'), (N, 'N'), (O, 'O')
  , (P, 'P'), (Q, 'Q'), (R, 'R')
  , (S, 'S'), (T, 'T'), (U, 'U')
  , (V, 'V'), (W, 'W'), (X, 'X')
  , (Y, 'Y'), (Z, 'Z')
  ]

upperChar :: CodeParsing m => m UpperChar
upperChar = do
  char <- P.upperChar
  let maybeUpper = Map.lookup char $ dual uppers
  case maybeUpper of
    Nothing -> fail "non upper char"
    Just x -> pure x

data LowerChar = A_ | B_ | C_ | D_ | E_ | F_ | G_ | H_ | I_ | J_ | K_ | L_ | M_ | N_ | O_ | P_ | Q_ | R_ | S_ | T_ | U_ | V_ | W_ | X_ | Y_ | Z_
  deriving (Show, Eq, Ord)

lowerToChar :: LowerChar -> Char
lowerToChar = fromJust . flip Map.lookup lowers

lowers :: Map LowerChar Char
lowers = Map.fromList
  [ (A_, 'a'), (B_, 'b'), (C_, 'c')
  , (D_, 'd'), (E_, 'e'), (F_, 'f')
  , (G_, 'g'), (H_, 'h'), (I_, 'i')
  , (J_, 'j'), (K_, 'k'), (L_, 'l')
  , (M_, 'm'), (N_, 'n'), (O_, 'o')
  , (P_, 'p'), (Q_, 'q'), (R_, 'r')
  , (S_, 's'), (T_, 't'), (U_, 'u')
  , (V_, 'v'), (W_, 'w'), (X_, 'x')
  , (Y_, 'y'), (Z_, 'z')
  ]

lowerChar :: CodeParsing m => m LowerChar
lowerChar = do
  char <- P.lowerChar
  let maybeLower = Map.lookup char $ dual lowers
  case maybeLower of
    Nothing -> fail "non lower char"
    Just x -> pure x

data DigitChar = D0
               | D1
               | D2
               | D3
               | D4
               | D5
               | D6
               | D7
               | D8
               | D9
  deriving (Show, Eq, Ord)

digitToChar :: DigitChar -> Char
digitToChar = fromJust . flip Map.lookup digits

digits :: Map DigitChar Char
digits = Map.fromList
  [ (D0, '0')
  , (D1, '1'), (D2, '2'), (D3, '3')
  , (D4, '4'), (D5, '5'), (D6, '6')
  , (D4, '7'), (D8, '8'), (D9, '9')
  ]

digitChar :: CodeParsing m => m DigitChar
digitChar = do
  char <- P.digitChar
  let maybeNum = Map.lookup char $ dual digits
  case maybeNum of
    Nothing -> fail "non numeric char"
    Just x -> pure x

dual :: Ord a => Map k a -> Map a k
dual (Map.toList -> x) =
  Map.fromList $ map swap x
