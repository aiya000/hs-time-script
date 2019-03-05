{
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Parses codes, commands, and expressions.
--
-- NOTICE:
-- This only parses syntaxes, doesn't check valid syntaxes strictly.
-- Please use Tim.Checker if you want.
--
-- e.g.
-- These are parsed successfully.
-- `let x: String = 10` (invalid assigning)
-- `1.0` (1.0 is not a command, commands are not allowed at top level)
module Tim.Parser
  ( parse
  ) where

import Control.Arrow ((>>>))
import Control.Monad.Except (throwError)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict (Map)
import Data.String.Here (i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude
import Tim.Lexer.Types (Token)
import Tim.Lexer.Types.Idents (pattern LetIdent)
import Tim.Parser.Types
import Tim.Processor (Processor, runProcessor, TokenPos, Failure(..))
import qualified Data.Map.Strict as Map
import qualified Tim.Lexer.Types as Token
import qualified Tim.Lexer.Types.Idents as Ident
}

%error { parseError }
%errorhandlertype explist
%monad { Processor }
%name parseCode
%tokentype { (Token, TokenPos) }

%token
  let           { (Token.Command LetIdent, _)                        }
  varIdent      { (Token.Var $$, _)                                  }
  ':'           { (Token.Colon, _)                                   }
  typeIdent     { (Token.Type $$, _)                                 }
  '='           { (Token.Assign, _)                                  }
  nat           { (Token.Literal (Token.Nat $$), _)                  }
  int           { (Token.Literal (Token.Int $$), _)                  }
  float         { (Token.Literal (Token.Float $$), _)                }
  singleQString { (Token.Literal (Token.String Token.SingleQ $$), _) }
  doubleQString { (Token.Literal (Token.String Token.DoubleQ $$), _) }
  '['           { (Token.ListBegin, _)                               }
  ']'           { (Token.ListEnd, _)                                 }
  '{'           { (Token.DictBegin, _)                               }
  '}'           { (Token.DictEnd, _)                                 }
  '('           { (Token.ParenBegin, _)                              }
  ')'           { (Token.ParenEnd, _)                                }
  ','           { (Token.Comma, _)                                   }
  lineBreak     { (Token.LineBreak, _)                               }

%%

AST :: { AST }
  : Literal  { Literal $1                         }
  | Code     { Code $1                            }
  | varIdent { VarIdent (Ident.simpleVarIdent $1) }

Code :: { Code }
  : {- empty -}           { []      }
  | Syntax lineBreak Code { $1 : $3 }

Syntax :: { Syntax }
  : let Lhs ':' typeIdent '=' Rhs { Let $2 (Just $4) $6 }
  | let Lhs '=' Rhs               { Let $2 Nothing $4   }

Lhs :: { Lhs }
  : varIdent         { LVar $1     }
  | '[' DestVars ']' { LDestuct $2 }

-- Destructive assignee variables
DestVars :: { NonEmpty Ident.VarIdent }
  : varIdent              { ($1 :| []) }
  | varIdent ',' DestVars { $1 <| $3   }

Rhs :: { Rhs }
  : varIdent { RVar $1 }
  | Literal  { RLit $1 }

Literal :: { Literal }
  : nat               { Nat $1    }
  | int               { Int $1    }
  | float             { Float $1  }
  | StringLit         { String $1 }
  | '(' Literal ')'   { Parens $2 }
  | '[' ListInner ']' { List $2   }
  | '{' DictInner '}' { Dict $2   }

StringLit :: { StringLit }
  : singleQString { SingleQuoted $1 }
  | doubleQString { DoubleQuoted $1 }

ListInner :: { [Literal] }
  : {- empty -}           { []      }
  | Literal               { [$1]    }
  | Literal ',' ListInner { $1 : $3 }

DictInner :: { Map StringLit Literal }
  : {- empty -}                         { Map.empty           }
  | StringLit ':' Literal ',' DictInner { Map.insert $1 $3 $5 }

{
parse :: [(Token, TokenPos)] -> Either Failure AST
parse = runProcessor . parseCode

flattenMargins :: String -> String
flattenMargins = replace . unlines . filter (/= "") . map (dropWhile (== ' ')) . lines
  where
    replace [] = []
    replace ('\n' : xs) = ' ' : replace xs
    replace (x : xs) = x : replace xs

parseError :: ([(Token, TokenPos)], [String]) -> Processor a
parseError (((got, pos):_), expected) =
  throwError . flip Failure pos $ flattenMargins [i|
    got a token `${show $ pretty got}`
    at ${show $ pretty pos},
    but ${expected} are expected at here.
  |]
parseError (_, _) =
  error $ flattenMargins [i|
    fatal error!
    Sorry, please report an issue :(
    <- parseError at ${(__FILE__ :: String)}:L${(__LINE__ :: Int)}
  |]
}
