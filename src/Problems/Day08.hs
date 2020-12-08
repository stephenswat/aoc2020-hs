module Problems.Day08 (solution) where

import Data.Set (Set, empty, member, insert)
import Control.Lens
import Control.Monad.State

import Common.Solution (Day)
import Common.Bytecode

stepWithState :: ExecutionState -> State (Set Int) ExecutionState
stepWithState s
    = do {
        m <- get;
        if member (pc s) m || isHalted s then
            return s;
        else do {
            modify . insert . pc $ s;
            stepWithState $ stepExecution s;
        }
    }

runStatefully :: Program -> ExecutionState
runStatefully p = evalState (stepWithState (initExecution p)) empty

flipNopJmp :: Operation -> Operation
flipNopJmp (Jmp v) = Nop v
flipNopJmp (Nop v) = Jmp v
flipNopJmp n       = n

isNopJmp :: Operation -> Bool
isNopJmp (Nop _) = True
isNopJmp (Jmp _) = True
isNopJmp _       = False

solveB :: Program -> Int
solveB p
    = acc
    . runStatefully
    . head
    . filter (isHalted . runStatefully)
    $ [element n %~ flipNopJmp $ p | (n, i) <- zip [0..] p, isNopJmp i]

solution :: Day
solution =
    ( show . acc . runStatefully . parseProgram
    , show . solveB . parseProgram
    )
