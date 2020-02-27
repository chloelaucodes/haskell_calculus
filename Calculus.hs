module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = Val . fromInteger
  negate      = UnApp Neg
  (+)         = BinApp Add
  (*)         = BinApp Mul
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = Val . fromRational
  (/)          = BinApp Div
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key valuePair
  = fromJust (lookup key valuePair)

showExp :: Exp -> String
showExp
  = undefined

eval :: Exp -> Env -> Double
eval (Val d) _
  = d
eval (Id s) env
  = lookUp s env
eval (UnApp unop expr) env
  = lookUp unop unOpList (eval expr env)
  where
    unOpList = [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log)]
eval (BinApp binop e1 e2) env
  = lookUp binop binOpList (eval e1 env) (eval e2 env)
  where
    binOpList = [(Add, (+)), (Mul, (*)), (Div, (/))]

diff :: Exp -> String -> Exp
diff (Val constant) _
  = 0
diff (Id v1) env
  | v1 == env  = 1
  | otherwise = 0
diff (BinApp Add e1 e2) env
  = (+) (diff e1 env) (diff e2 env)
diff (BinApp Mul e1 e2) env
  = (+) ((*) e1 (diff e2 env)) ((*) (diff e1 env) e2)
diff (BinApp Div e1 e2) env
  = (/) exp1 exp2
  where
    exp1  = (+) exp'1 exp'2
    exp2  = (*) e2 e2
    exp'1 = (*) (diff e1 env) e2
    exp'2 = negate ((*) e1 (diff e2 env))
diff (UnApp Sin expr) env
  = (*) (cos expr) (diff expr env)
diff (UnApp Cos expr) env
  = negate ((*) (sin expr) (diff expr env))
diff (UnApp Neg expr) env
  = negate (diff expr env)
diff (UnApp Log expr) env
  =  (/) (diff expr env) expr

maclaurin :: Exp -> Double -> Int -> Double
maclaurin expr point numTerms
  = foldl1 (+) terms
  where
    terms = take numTerms (zipWith3 (\x y z -> x * y / z) power diffl fact)
    power = iterate (* point) 1
    fact  = scanl (*) 1 [1..]
    diffl = map (flip eval [("x", 0)]) (iterate (flip diff "x") expr)

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = (+) (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
