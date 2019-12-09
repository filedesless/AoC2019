module Day02Impl where

import Data.Sequence
import Control.Monad.State
import Control.Applicative

type Memory = Seq Int
type Triplet = (Int, Int, Int)
type BinaryOp = (Int -> Int -> Int)

data OpCode
  = Add Triplet
  | Mul Triplet

compute :: BinaryOp -> Triplet -> Memory -> Memory
compute f (op1, op2, dst) mem = maybe mem upd $ liftA2 f (mem !? op1) (mem !? op2)
  where upd s = update dst s mem

eval :: OpCode -> State Memory ()
eval (Add triplet) = modify $ compute (+) triplet
eval (Mul triplet) = modify $ compute (*) triplet

getOperands :: Memory -> Int -> Maybe Triplet
getOperands mem pc= do
  op1 <- mem !? (pc + 1)
  op2 <- mem !? (pc + 2)
  dst <- mem !? (pc + 3)
  return (op1, op2, dst)

run :: Int -> State Memory Memory
run pc = do
  mem <- get
  let res = eval <$> case mem !? pc of
        Just 1 -> Add <$> getOperands mem pc
        Just 2 -> Mul <$> getOperands mem pc
        _ -> Nothing
  maybe get (\s -> s >> run (pc + 4)) res
