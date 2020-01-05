module Tim.TypeChecker where

import RIO
import Tim.Parser.Types

type Error = String

check :: AST -> Either Error AST
check _ = error "TODO (Tim.TypeChecker.check)"

isValid :: AST -> Bool
isValid = isRight . check
