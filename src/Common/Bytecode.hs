module Common.Bytecode where

import Data.List.Split (splitOn)
import Relude.List ((!!?))

data Operation
    = Acc Int
    | Jmp Int
    | Nop Int

instance Show Operation where
    show (Acc v) = "acc " ++ show v
    show (Jmp v) = "jmp " ++ show v
    show (Nop v) = "nop"

type Program = [Operation]

data ExecutionState = ExecutionState
    { program :: Program
    , pc :: Int
    , acc :: Int
    }

parseInstruction :: String -> Operation
parseInstruction s
    | op == "acc" = Acc v
    | op == "jmp" = Jmp v
    | op == "nop" = Nop v
    | otherwise = error "Invalid opcode!"
    where
        (op:v':[]) = splitOn " " s
        v = (if head v' == '-' then -1 else 1) * (read . tail $ v') :: Int

parseProgram :: String -> Program
parseProgram = map parseInstruction . lines

initExecution :: Program -> ExecutionState
initExecution p = ExecutionState { program=p, pc=0, acc=0 }

stepExecution :: ExecutionState -> ExecutionState
stepExecution s = case op of
    Just (Acc v) -> s { pc = 1 + pc s, acc = v + acc s }
    Just (Jmp v) -> s { pc = v + pc s }
    Just (Nop v) -> s { pc = 1 + pc s }
    Nothing      -> s
    where op = (!!? (pc s)) . program $ s

isHalted :: ExecutionState -> Bool
isHalted s = pc s == length (program s)