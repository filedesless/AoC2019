module Day02Impl where

import Data.Sequence
import Control.Monad.State
import Control.Applicative

type Addr = Int
type Value = Int
data Operand = Pos Addr | Imm Value deriving (Show, Eq)
type Triplet = (Operand, Operand, Addr)
data OpCode = Add Triplet | Mul Triplet | Str Addr | Out Operand | Halt
  deriving (Show, Eq)
type Memory = Seq Value
type BinaryOp = (Value -> Value -> Value)
type Input = [Value]
type Output = [Value]
type Computer = (Input, Output, Memory, Addr)
data OpMode = Position | Immediate

fetch :: Memory -> Operand -> Maybe Value
fetch mem (Pos i) = mem !? i
fetch _ (Imm i) = Just i

compute :: BinaryOp -> Triplet -> Memory -> Memory
compute f (op1, op2, dst) mem =
  maybe mem upd $ liftA2 f (fetch mem op1) (fetch mem op2)
  where upd s = update dst s mem

eval :: OpCode -> Maybe (State Computer ())
eval (Add triplet) = Just $
  modify (\(input, output, mem, pc) ->
            (input, output, compute (+) triplet mem, pc + 4))
eval (Mul triplet) = Just $
  modify (\(input, output, mem, pc) ->
            (input, output, compute (*) triplet mem, pc + 4))
eval (Str i) = Just $
  modify (\(input, output, mem, pc) ->
            (tail input, output, update i (head input) mem, pc + 2))
eval (Out op) = Just $
  modify (\(input, output, mem, pc) -> let Just i = fetch mem op in
            (input, i : output, mem, pc + 2))
eval Halt = Nothing

getBinaryOperands :: Memory -> Addr -> Maybe Triplet
getBinaryOperands mem pc = do
  op1 <- mem !? (pc + 1)
  op2 <- mem !? (pc + 2)
  dst <- mem !? (pc + 3)
  return (Pos op1, Pos op2, dst)

parseOp :: Memory -> Addr -> OpCode
parseOp = undefined

run :: State Computer Memory
run = do
  (_, _, mem, pc) <- get
  let stepi (Just 1) = Add <$> getBinaryOperands mem pc
      stepi (Just 2) = Mul <$> getBinaryOperands mem pc
      stepi (Just 3) = Str <$> mem !? succ pc
      stepi (Just 4) = Out . Pos <$> mem !? succ pc
      stepi _ = Nothing
  case stepi (mem !? pc) >>= eval of
    Just s -> s >> run
    Nothing -> gets (\(_, _, m, _) -> m)
