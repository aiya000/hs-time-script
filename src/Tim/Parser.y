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

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Exception.Safe (displayException)
import Control.Monad.Except (throwError)
import Data.Char.Cases (DigitChar(..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Strict (Map)
import Data.String.Cases
import Data.String.Here (i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Prelude
import RIO.List
import Text.Megaparsec (runParser)
import Tim.Lexer.Types (Token, Register(..), Option(..), Scope(..))
import Tim.Megaparsec
import Tim.Parser.Types
import Tim.Processor
import qualified Data.List.NonEmpty as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.String.Cases as String
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Tim.Lexer.Types as Token
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
  '.'           { (Token.Dot, _)                                     }
  "->"          { (Token.Arrow, _)                                   }
  '|'           { (Token.Bar, _)                                     }
  '#'           { (Token.Sharp, _)                                   }
  lineBreak     { (Token.LineBreak, _)                               }

  -- Important commands identifiers
  let         { (Token.Ident LetI, _)         }
  function    { (Token.Ident FunctionI, _)    }
  endfunction { (Token.Ident EndFunctionI, _) }

  -- variable identifiers
  varG       { (ScopedIdent G "", _) }
  varS       { (ScopedIdent S "", _) }
  varL       { (ScopedIdent L "", _) }
  varV       { (ScopedIdent V "", _) }
  varB       { (ScopedIdent B "", _) }
  varW       { (ScopedIdent W "", _) }
  varT       { (ScopedIdent T "", _) }
  -- varA (the identifier 'a:') is parsed by parseAScopeVar
  varScopedG { (ScopedIdent G $$, pos) }
  varScopedS { (ScopedIdent S $$, pos) }
  varScopedL { (ScopedIdent L $$, pos) }
  varScopedA { (ScopedIdent A $$, pos) }
  varScopedV { (ScopedIdent V $$, pos) }
  varScopedB { (ScopedIdent B $$, pos) }
  varScopedW { (ScopedIdent W $$, pos) }
  varScopedT { (ScopedIdent T $$, pos) }

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

%right '|'
%right "->"

%%

AST :: { AST }
  : Code { Code $1 }
  | Rhs  { Rhs $1  }

Code :: { Code }
  : {- empty -}           { []      }
  | Syntax                { [$1]    }
  | Syntax lineBreak Code { $1 : $3 }

Syntax :: { Syntax }
  : Let      { $1 }
  | Function { $1 }

Function :: { Syntax }
  : function FuncName '(' ')' endfunction { Function $2 [] Nothing [] [] }

FuncName :: { FuncName }
  : UnqualifiedVar   { UnqualifiedFuncName $1                 }
  | ScopedVar        { ScopedFuncName $1                      }
  | DictVar          { DictFuncName $1                        }
  | AutoloadFuncName { AutoloadFuncName $ NonEmpty.reverse $1 }

AutoloadFuncName :: { List.NonEmpty Snake }
  : UnqualifiedVar '#' UnqualifiedVar   { $3 :| [$1] }
  | UnqualifiedVar '#' AutoloadFuncName { $1 <| $3   }

Let :: { Syntax }
  : let Lhs ':' Type '=' Rhs { Let $2 (Just $4) $6 }
  | let Lhs '=' Rhs          { Let $2 Nothing $4   }

Lhs :: { Lhs }
  : Variable         { LVar $1     }
  | '[' DestVars ']' { LDestuct $2 }

-- Destructive assignee variables
DestVars :: { List.NonEmpty Variable }
  : Variable              { ($1 :| []) }
  | Variable ',' DestVars { $1 <| $3   }

Type :: { Type }
  : Type "->" Type { Arrow $1 $3 }
  | Type '|'  Type { Union $1 $3 }
  | Camel          { Con $1      }
  | '(' Type ')'   { $2          }
  | TypeApp        { $1          }

-- lefty bias
TypeApp :: { Type }
  : Type Camel        { App $1 (Con $2) }
  | Type '(' Type ')' { App $1 $3       }

Camel :: { Camel }
  : ident {% runParserInProcessor pos String.parseCamel $1 }

Variable :: { Variable }
  : ScopedVariable      { $1 }
  | DictVariable        { $1 }
  | RegisterVariable    { $1 }
  | OptionVariable      { $1 }
  | UnqualifiedVariable { $1 }

ScopedVariable :: { Variable }
  : ScopedVar { ScopedVar $1 }

ScopedVar :: { ScopedVar }
  : varG       { GScopeVar EmptyScopedName                                                       }
  | varS       { SScopeVar EmptyScopedName                                                       }
  | varL       { LScopeVar EmptyScopedName                                                       }
  | varV       { VScopeVar EmptyScopedName                                                       }
  | varB       { BScopeVar EmptyScopedName                                                       }
  | varW       { WScopeVar EmptyScopedName                                                       }
  | varT       { TScopeVar EmptyScopedName                                                       }
  | varScopedG {% fmap (GScopeVar . NonEmptyScopedName) $ runParserInProcessor pos parseSnake $1 }
  | varScopedS {% fmap (SScopeVar . NonEmptyScopedName) $ runParserInProcessor pos parseSnake $1 }
  | varScopedL {% fmap (LScopeVar . NonEmptyScopedName) $ runParserInProcessor pos parseSnake $1 }
  | varScopedV {% fmap (VScopeVar . NonEmptyScopedName) $ runParserInProcessor pos parseSnake $1 }
  | varScopedB {% fmap (BScopeVar . NonEmptyScopedName) $ runParserInProcessor pos parseSnake $1 }
  | varScopedW {% fmap (WScopeVar . NonEmptyScopedName) $ runParserInProcessor pos parseSnake $1 }
  | varScopedT {% fmap (TScopeVar . NonEmptyScopedName) $ runParserInProcessor pos parseSnake $1 }
  | varScopedA {% fmap AScopeVar $ runParserInProcessor pos parseAScopeVar $1                    }

DictVariable :: { Variable }
  : DictVar { DictVar $1 }

DictVar :: { DictVar }
  : DictSelf '[' Variable ']'   { IndexAccessDictVar $1 $3         }
  | DictSelf '.' UnqualifiedVar { PropertyAccessDictVar $1 $3      }
  | DictVar '[' Variable ']'    { IndexAccessChainDictVar $1 $3    }
  | DictVar '.' UnqualifiedVar  { PropertyAccessChainDictVar $1 $3 }

DictSelf :: { DictSelf }
  : UnqualifiedVar { UnqualifiedVarDictSelf $1 }
  | ScopedVar      { ScopedVarDictSelf $1      }

RegisterVariable :: { Variable }
  : varRegUnnamed   { RegisterVar Unnamed         }
  | varRegSmallDel  { RegisterVar SmallDelete     }
  | varRegReadOnlyC { RegisterVar ReadOnlyColon   }
  | varRegReadonlyD { RegisterVar ReadOnlyDot     }
  | varRegReadOnlyP { RegisterVar ReadOnlyPercent }
  | varRegBuffer    { RegisterVar Buffer          }
  | varRegExpr      { RegisterVar Expression      }
  | varRegClipS     { RegisterVar ClipboardStar   }
  | varRegClipP     { RegisterVar ClipboardPlus   }
  | varRegBlackHole { RegisterVar BlackHole       }
  | varRegSeached   { RegisterVar Searched        }
  | varRegNum       { RegisterVar $ Numeric $1    }
  | varRegAlpha     { RegisterVar $ Alphabetic $1 }

OptionVariable :: { Variable }
  : varLOption { OptionVar $ LocalScopedOption $1  }
  | varGOption { OptionVar $ GlobalScopedOption $1 }
  | varOption  { OptionVar $ UnscopedOption $1     }

UnqualifiedVariable :: { Variable }
  : UnqualifiedVar { UnqualifiedVar $1 }

UnqualifiedVar :: { Snake }
  : ident {% runParserInProcessor pos parseSnake $1 }

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


pattern LetI :: Token.Ident
pattern LetI = Token.UnqualifiedIdent (String.NonEmpty 'l' "et")

pattern FunctionI :: Token.Ident
pattern FunctionI = Token.UnqualifiedIdent (String.NonEmpty 'f' "unction")

pattern EndFunctionI :: Token.Ident
pattern EndFunctionI = Token.UnqualifiedIdent (String.NonEmpty 'e' "ndfunction")


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

runParserInProcessor :: TokenPos -> CodeParsec a -> String -> Processor a
runParserInProcessor pos parser input =
  case runParser parser "time-script" input of
    Right x -> pure x
    Left  e -> throwError $ Failure (displayException e) (OnAToken pos)

parseAScopeVar :: CodeParsec AScopeName
parseAScopeVar =
    varAll <|>
    varNum <|>
    nonEmptyName <|>
    emptyName
  where
    varAll = VarAllAScopeName <$ P.string "000"
    varNum = VarNumAScopeName <$> P.decimal
    nonEmptyName = NameAScopeName . NonEmptyScopedName <$> parseSnake
    emptyName = NameAScopeName EmptyScopedName <$ P.string ""
}
