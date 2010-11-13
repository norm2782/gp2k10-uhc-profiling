module Core where

import Name

-- Core functional language
data Expr = Var String
          | Cons Name [Expr]
          | Func Name [Expr]
          | App String [Expr]
          | Case String [(Expr, Expr)]
          deriving Show        

-- Function definition
data Def = Def Name [Expr] Expr deriving Show