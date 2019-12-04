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
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict (Map)
import Data.String.Here (i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude
import RIO.List
import Text.Megaparsec (parseMaybe)
import Tim.Char (DigitChar(..))
import Tim.Lexer.Types (Token, Register(..), Option(..), Scope(..))
import Tim.Parser.Types
import Tim.Processor
import Tim.String
import qualified Data.List.NonEmpty as List
import qualified Data.Map.Strict as Map
import qualified Tim.Char as Char
import qualified Tim.Lexer.Types as Token
import qualified Tim.String as String hiding (parseCamel)
import qualified Tim.String.Parser as String
}

%error { parseError }
%errorhandlertype explist
%monad { Processor }
%name parseAST
%tokentype { (Token, TokenPos) }

%token
  ':'           { (Token.Colon, _)                                   }
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
  "->"          { (Token.Arrow, _)                                   }
  '|'           { (Token.Bar, _)                                     }
  lineBreak     { (Token.LineBreak, _)                               }

  -- Important command identifiers
  let { (Token.Ident Token.Let, _) }

  -- variable identifiers
  varScopedG { (ScopedIdent G $$, _) }
  varScopedS { (ScopedIdent S $$, _) }
  varScopedL { (ScopedIdent L $$, _) }
  varScopedA { (ScopedIdent A $$, _) }
  varScopedV { (ScopedIdent V $$, _) }
  varScopedB { (ScopedIdent B $$, _) }
  varScopedW { (ScopedIdent W $$, _) }
  varScopedT { (ScopedIdent T $$, _) }

  varRegUnnamed   { (RegisterIdent Unnamed, _)         }
  varRegSmallDel  { (RegisterIdent SmallDelete, _)     }
  varRegReadOnlyC { (RegisterIdent ReadOnlyColon, _)   }
  varRegReadonlyD { (RegisterIdent ReadOnlyDot, _)     }
  varRegReadOnlyP { (RegisterIdent ReadOnlyPercent, _) }
  varRegBuffer    { (RegisterIdent Buffer, _)          }
  varRegExpr      { (RegisterIdent Expression, _)      }
  varRegClipS     { (RegisterIdent ClipboardStar, _)   }
  varRegClipP     { (RegisterIdent ClipboardPlus, _)   }
  varRegBlackHole { (RegisterIdent BlackHole, _)       }
  varRegSeached   { (RegisterIdent Searched, _)        }
  varRegNum       { (RegisterIdent (Numeric $$), _)    } -- 1-9
  varRegAlpha     { (RegisterIdent (Alphabetic $$), _) } -- a-zA-Z

  varOption  { (OptionIdent $$, _)  }
  varLOption { (LOptionIdent $$, _) }
  varGOption { (GOptionIdent $$, _) }

  -- An another identifier, e.g.
  -- - An unscoped variable identifier
  -- - A type identifier
  ident { (Token.Ident (Token.unIdent -> $$), pos) }

%%

AST :: { AST }
  : Code { Code $1 }
  | Rhs  { Rhs $1  }

Code :: { Code }
  : {- empty -}           { []      }
  | Syntax                { [$1]    }
  | Syntax lineBreak Code { $1 : $3 }

Syntax :: { Syntax }
  : let Lhs ':' Type '=' Rhs { Let $2 (Just $4) $6 }
  | let Lhs '=' Rhs          { Let $2 Nothing $4   }

Lhs :: { Lhs }
  : Variable         { LVar $1     }
  | '[' DestVars ']' { LDestuct $2 }

Type :: { Type }
  : Type "->" Type { Arrow $1 $3  }
  | Type '|'  Type { Union $1 $3  }
  | '(' Type ')'   { Parens $2    }
  | Camel TypeArgs { Constr $1 $2 }

-- TODO: 4 or more arguments?
TypeArgs :: { [Type] }
  : {- empty -}       { []                                         }
  | Camel             { [Constr $1 []]                             }
  | Camel Camel       { [Constr $1 [], Constr $2 []]               }
  | Camel Camel Camel { [Constr $1 [], Constr $2 [], Constr $3 []] }

Camel :: { Camel }
  : ident {% resolveParsingCamel pos $ String.parseCamel $1 }

Variable :: { Variable }
  : varScopedG      { Scoped G $1                    }
  | varScopedS      { Scoped S $1                    }
  | varScopedL      { Scoped L $1                    }
  | varScopedA      { Scoped A $1                    }
  | varScopedV      { Scoped V $1                    }
  | varScopedB      { Scoped B $1                    }
  | varScopedW      { Scoped W $1                    }
  | varScopedT      { Scoped T $1                    }
  | varRegUnnamed   { Register Unnamed               }
  | varRegSmallDel  { Register SmallDelete           }
  | varRegReadOnlyC { Register ReadOnlyColon         }
  | varRegReadonlyD { Register ReadOnlyDot           }
  | varRegReadOnlyP { Register ReadOnlyPercent       }
  | varRegBuffer    { Register Buffer                }
  | varRegExpr      { Register Expression            }
  | varRegClipS     { Register ClipboardStar         }
  | varRegClipP     { Register ClipboardPlus         }
  | varRegBlackHole { Register BlackHole             }
  | varRegSeached   { Register Searched              }
  | varRegNum       { Register $ Numeric $1          }
  | varRegAlpha     { Register $ Alphabetic $1       }
  | varLOption      { Option $ LocalScopedOption $1  }
  | varGOption      { Option $ GlobalScopedOption $1 }
  | varOption       { Option $ UnscopedOption $1     }
  | ident           { SimpleLocal $1                 }

-- Destructive assignee variables
DestVars :: { List.NonEmpty Variable }
  : Variable              { ($1 :| []) }
  | Variable ',' DestVars { $1 <| $3   }

Rhs :: { Rhs }
  : Variable    { RVar $1    }
  | Literal     { RLit $1    }
  | '(' Rhs ')' { RParens $2 }

Literal :: { Literal }
  : nat               { Nat $1    }
  | int               { Int $1    }
  | float             { Float $1  }
  | StringLit         { String $1 }
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
  | StringLit ':' Literal               { Map.singleton $1 $3 }
  | StringLit ':' Literal ',' DictInner { Map.insert $1 $3 $5 }

{
pattern ScopedIdent :: Scope -> String -> Token
pattern ScopedIdent s x = Token.Ident (Token.QualifiedIdent (Token.Scoped s x))

pattern RegisterIdent :: Register -> Token
pattern RegisterIdent r = Token.Ident (Token.QualifiedIdent (Token.Register r))

pattern OptionIdent :: LowerString -> Token
pattern OptionIdent x = Token.Ident (Token.QualifiedIdent (Token.Option (Token.UnscopedOption x)))

pattern LOptionIdent :: LowerString -> Token
pattern LOptionIdent x = Token.Ident (Token.QualifiedIdent (Token.Option (Token.LocalScopedOption x)))

pattern GOptionIdent :: LowerString -> Token
pattern GOptionIdent x = Token.Ident (Token.QualifiedIdent (Token.Option (Token.GlobalScopedOption x)))

parse :: [(Token, TokenPos)] -> Either Failure AST
parse = runProcessor . parseAST

-- TODO: Show all [(Token, TokenPos)] if --verbose specified on cli.
parseError :: ([(Token, TokenPos)], [String]) -> Processor a
parseError ((got, pos) : _, expected) =
  throwError . flip Failure (OnAToken pos) $ flattenMargins [i|
    got a token `${show $ pretty got}`,
    but ${expected} are expected at here.
  |]
parseError ([], expected) =
  throwError $ Failure [i|got EOF, but ${makePluralForm expected} are expected at here.|] EOF
  where
    -- ["a", "b"]      -> "a or b"
    -- ["a", "b", "c"] -> "a, b, or c"
    makePluralForm [] = []
    makePluralForm [x] = x
    makePluralForm (x : y : words) =
      case uncons $ reverse words of
        Nothing ->  [i|${x} or ${y}|]
        Just (tail, reverse -> body) -> [i|${foldl comma "" body}, or ${tail}|]

    comma :: String -> String -> String
    comma x y = [i|${x}, ${y}|]

flattenMargins :: String -> String
flattenMargins = replace . unlines . filter (/= "") . map (dropWhile (== ' ')) . lines
  where
    replace [] = []
    replace ('\n' : xs) = ' ' : replace xs
    replace (x : xs) = x : replace xs

resolveParsingCamel :: TokenPos -> Either String Camel -> Processor Camel
resolveParsingCamel _ (Right x) = pure x
resolveParsingCamel pos (Left x) =
  throwError $ Failure x (OnAToken pos)
}
