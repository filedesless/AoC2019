module Day02Impl where

import Data.Sequence
import Control.Monad.State
import Control.Applicative

data OpCode
  = Add Triplet
  | Mul Triplet
data Operand = Pos Int | Imm Int

type Memory = Seq Int
type Triplet = (Operand, Operand, Int)
type BinaryOp = (Int -> Int -> Int)
type Input = [Int]
type Output = [Int]
type Computer = (Input, Output, Memory, Int)

fetch :: Memory -> Operand -> Maybe Int
fetch mem (Pos i) = mem !? i
fetch _ (Imm i) = Just i

compute :: BinaryOp -> Triplet -> Memory -> Memory
compute f (op1, op2, dst) mem =
  maybe mem upd $ liftA2 f (fetch mem op1) (fetch mem op2)
  where upd s = update dst s mem

eval :: OpCode -> State Computer ()
eval (Add triplet) =
  modify (\(input, output, mem, pc) ->
            (input, output, compute (+) triplet mem, pc + 4))
eval (Mul triplet) =
  modify (\(input, output, mem, pc) ->
            (input, output, compute (*) triplet mem, pc + 4))

getBinaryOperands :: Memory -> Int -> Maybe Triplet
getBinaryOperands mem pc = do
  op1 <- mem !? (pc + 1)
  op2 <- mem !? (pc + 2)
  dst <- mem !? (pc + 3)
  return (Pos op1, Pos op2, dst)

run :: State Computer Memory
run = do
  (_, _, mem, pc) <- get
  let stepi (Just 1) = Add <$> getBinaryOperands mem pc
      stepi (Just 2) = Mul <$> getBinaryOperands mem pc
      stepi _ = Nothing
  case eval <$> stepi (mem !? pc) of
    Just s -> s >> run
    Nothing -> gets (\(_, _, m, _) -> m)
