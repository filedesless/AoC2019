module Day02Impl where

import Data.Sequence
import Control.Monad.State
import Control.Applicative

type Addr = Int
type Value = Int
data Operand = Pos Addr | Imm Value
type Triplet = (Operand, Operand, Addr)
data OpCode = Add Triplet | Mul Triplet | Str Addr | Out Operand
type Memory = Seq Value
type BinaryOp = (Value -> Value -> Value)
type Input = [Value]
type Output = [Value]
type Computer = (Input, Output, Memory, Addr)

fetch :: Memory -> Operand -> Maybe Value
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
eval (Str i) =
  modify (\(input, output, mem, pc) ->
            (tail input, output, update i (head input) mem, pc + 2))
eval (Out op) =
  modify (\(input, output, mem, pc) -> let Just i = fetch mem op in
            (input, i : output, mem, pc + 2))

getBinaryOperands :: Memory -> Addr -> Maybe Triplet
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
      stepi (Just 3) = Str <$> mem !? succ pc
      stepi (Just 4) = Out . Pos <$> mem !? succ pc
      stepi _ = Nothing
  case eval <$> stepi (mem !? pc) of
    Just s -> s >> run
    Nothing -> gets (\(_, _, m, _) -> m)
