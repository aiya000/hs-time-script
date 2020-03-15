{
{-# LANGUAGE QuasiQuotes #-}

-- | Combinators for Tim.Parser.Types and Tim.Parser.
module Tim.String.Parser
  ( parseCamel
  ) where

import Data.String.Here (i)
import RIO
import RIO.List.Partial ((!!)) -- For happy processing
import Tim.Char
import Tim.Lexer.Types (Token)
import Tim.Processor
import Tim.String hiding (parseCamel)
}

%error { parseError }
%errorhandlertype explist
%monad { Either String }
%name parseCamel
%tokentype { Char }

%token
  'a' { 'a' }
  'b' { 'b' }
  'c' { 'c' }
  'd' { 'd' }
  'e' { 'e' }
  'f' { 'f' }
  'g' { 'g' }
  'h' { 'h' }
  'i' { 'i' }
  'j' { 'j' }
  'k' { 'k' }
  'l' { 'l' }
  'm' { 'm' }
  'n' { 'n' }
  'o' { 'o' }
  'p' { 'p' }
  'q' { 'q' }
  'r' { 'r' }
  's' { 's' }
  't' { 't' }
  'u' { 'u' }
  'v' { 'v' }
  'w' { 'w' }
  'x' { 'x' }
  'y' { 'y' }
  'z' { 'z' }

  'A' { 'A' }
  'B' { 'B' }
  'C' { 'C' }
  'D' { 'D' }
  'E' { 'E' }
  'F' { 'F' }
  'G' { 'G' }
  'H' { 'H' }
  'I' { 'I' }
  'J' { 'J' }
  'K' { 'K' }
  'L' { 'L' }
  'M' { 'M' }
  'N' { 'N' }
  'O' { 'O' }
  'P' { 'P' }
  'Q' { 'Q' }
  'R' { 'R' }
  'S' { 'S' }
  'T' { 'T' }
  'U' { 'U' }
  'V' { 'V' }
  'W' { 'W' }
  'X' { 'X' }
  'Y' { 'Y' }
  'Z' { 'Z' }

  '0' { '0' }
  '1' { '1' }
  '2' { '2' }
  '3' { '3' }
  '4' { '4' }
  '5' { '5' }
  '6' { '6' }
  '7' { '7' }
  '8' { '8' }
  '9' { '9' }

%%

Camel :: { Camel }
  : Alpha AlphaNums { Camel $1 $2 }

AlphaNums :: { [AlphaNumChar] }
  : {- empty -}        { []      }
  | AlphaNum AlphaNums { $1 : $2 }

AlphaNum :: { AlphaNumChar }
  : Alpha { AlphaNumAlpha $1 }
  | Digit { AlphaNumDigit $1 }

Alpha :: { AlphaChar }
  : 'a' { AlphaLower A_ }
  | 'b' { AlphaLower B_ }
  | 'c' { AlphaLower C_ }
  | 'd' { AlphaLower D_ }
  | 'e' { AlphaLower E_ }
  | 'f' { AlphaLower F_ }
  | 'g' { AlphaLower G_ }
  | 'h' { AlphaLower H_ }
  | 'i' { AlphaLower I_ }
  | 'j' { AlphaLower J_ }
  | 'k' { AlphaLower K_ }
  | 'l' { AlphaLower L_ }
  | 'm' { AlphaLower M_ }
  | 'n' { AlphaLower N_ }
  | 'o' { AlphaLower O_ }
  | 'p' { AlphaLower P_ }
  | 'q' { AlphaLower Q_ }
  | 'r' { AlphaLower R_ }
  | 's' { AlphaLower S_ }
  | 't' { AlphaLower T_ }
  | 'u' { AlphaLower U_ }
  | 'v' { AlphaLower V_ }
  | 'w' { AlphaLower W_ }
  | 'x' { AlphaLower X_ }
  | 'y' { AlphaLower Y_ }
  | 'z' { AlphaLower Z_ }
  | 'A' { AlphaUpper A  }
  | 'B' { AlphaUpper B  }
  | 'C' { AlphaUpper C  }
  | 'D' { AlphaUpper D  }
  | 'E' { AlphaUpper E  }
  | 'F' { AlphaUpper F  }
  | 'G' { AlphaUpper G  }
  | 'H' { AlphaUpper H  }
  | 'I' { AlphaUpper I  }
  | 'J' { AlphaUpper J  }
  | 'K' { AlphaUpper K  }
  | 'L' { AlphaUpper L  }
  | 'M' { AlphaUpper M  }
  | 'N' { AlphaUpper N  }
  | 'O' { AlphaUpper O  }
  | 'P' { AlphaUpper P  }
  | 'Q' { AlphaUpper Q  }
  | 'R' { AlphaUpper R  }
  | 'S' { AlphaUpper S  }
  | 'T' { AlphaUpper T  }
  | 'U' { AlphaUpper U  }
  | 'V' { AlphaUpper V  }
  | 'W' { AlphaUpper W  }
  | 'X' { AlphaUpper X  }
  | 'Y' { AlphaUpper Y  }
  | 'Z' { AlphaUpper Z  }

Digit :: { DigitChar }
  : '0' { D0 }
  | '1' { D1 }
  | '2' { D2 }
  | '3' { D3 }
  | '4' { D4 }
  | '5' { D5 }
  | '6' { D6 }
  | '7' { D7 }
  | '8' { D8 }
  | '9' { D9 }

{

parseError :: (String, [String]) -> Either String b
parseError (invalidIdent, _) =
  Left [i|The identifier must be named a camel case "[a-zA-Z][a-zA-Z0-9]+" here: got "${invalidIdent}"|]

}
