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

fetch :: Memory -> Operand -> Maybe Value
fetch mem (Pos i) = mem !? i
fetch _ (Imm i) = Just i

compute :: BinaryOp -> Triplet -> Memory -> Memory
compute f (op1, op2, dst) mem =
  maybe mem upd $ liftA2 f (fetch mem op1) (fetch mem op2)
  where upd s = update dst s mem

arithmetic :: (Memory -> Memory) -> (Addr -> Addr) -> State Computer ()
arithmetic fmem fpc = modify (\(i, o, mem, pc) -> (i, o, fmem mem, fpc pc))

eval :: OpCode -> Maybe (State Computer ())
eval (Add triplet) = Just $ arithmetic (compute (+) triplet) (+ 4)
eval (Mul triplet) = Just $ arithmetic (compute (*) triplet) (+ 4)
eval (Str i) = Just $ modify
  (\(input, output, mem, pc) ->
      (tail input, output, update i (head input) mem, pc + 2))
eval (Out op) = Just $ modify
  (\(input, output, mem, pc) -> let Just i = fetch mem op in
      (input, i : output, mem, pc + 2))
eval Halt = Nothing

getBinaryOperands :: Memory -> Addr -> (Value -> Operand) -> (Value -> Operand)
                  -> Maybe Triplet
getBinaryOperands mem pc m1 m2 = do
  op1 <- mem !? (pc + 1)
  op2 <- mem !? (pc + 2)
  dst <- mem !? (pc + 3)
  return (m1 op1, m2 op2, dst)

parseOp :: Memory -> Addr -> Maybe OpCode
parseOp mem pc = do
  word <- mem !? pc
  let (_:_:argModes) = getMode <$> Prelude.reverse (show word) ++ repeat '0'
  case word `mod` 100 of
    1 -> Add <$> getBinaryOperands mem pc (head argModes) (argModes !! 1)
    2 -> Mul <$> getBinaryOperands mem pc (head argModes) (argModes !! 1)
    3 -> Str <$> mem !? succ pc
    4 -> let next_word = succ pc in
           Out . head argModes <$> mem !? next_word
    99 -> Just Halt
    _ -> Nothing
  where getMode '1' = Imm
        getMode  _  = Pos

run :: State Computer Memory
run = do
  (_, _, mem, pc) <- get
  case parseOp mem pc >>= eval of
    Just s -> s >> run
    Nothing -> gets (\(_, _, m, _) -> m)
