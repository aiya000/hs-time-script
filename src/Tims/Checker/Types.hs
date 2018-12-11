module Tims.Checker.Types where

-- | g: variables
newtype GVar = GVar
  { unGVar :: VarIdent
  } deriving (Show, Eq)

-- | s: variables
newtype SVar = SVar
  { unSVar :: VarIdent
  } deriving (Show, Eq)

-- | v: variables
newtype VVar = VVar
  { unVVar :: VarIdent
  } deriving (Show, Eq)
