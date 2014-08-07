{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module VM where

import Parser (ByteCode, OpCode(..))
import qualified Data.IntMap as IM
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.List (intercalate)
import Utility

data VM = VM {
	byteCode :: Vector Int,
	programCounter :: Int,
	stack :: [Int],
	memory :: IM.IntMap Int,
	isErr :: Bool,
	tickCount :: Int
	}
	deriving (Show)

fromCode :: ByteCode -> VM
fromCode code = VM { byteCode = Vector.fromList code, programCounter = 0, stack = [0, 0], memory = IM.empty, isErr = False, tickCount = 0 }

step :: VM -> VM
step vm = next { tickCount = 1 + tickCount next }
	where
		bc = byteCode vm
		pc = programCounter vm
		st = stack vm
		mm = memory vm
		inst = toEnum $ bc ! pc
		pop1 = tail st
		pop2 = tail pop1
		top1 = head st
		top2 = head pop1
		nextPc = pc + 1
		next = case inst of
			Pop -> vm { stack = pop1, programCounter = nextPc }
			Push -> vm { stack = bc ! nextPc : st, programCounter = pc + 2 }
			Add -> vm { stack = (top1 + top2) : pop2, programCounter = nextPc }
			Sub -> vm { stack = (top2 - top1) : pop2, programCounter = nextPc }
			Mult -> vm { stack = (top1 * top2) : pop2, programCounter = nextPc }
			Div -> vm { stack = (top2 `div` top1) : pop2, programCounter = nextPc }
			Store -> vm { stack = pop2, programCounter = nextPc, memory = IM.insert top1 top2 mm }
			Load
				| IM.member top1 mm -> vm { stack = mm IM.! top1 : pop1, programCounter = nextPc }
				| otherwise			-> vm { isErr = True, programCounter = nextPc }
			Jmp -> vm { stack = pop1, programCounter = top1 }
			Cmp -> vm { stack = signum (top2 - top1) : pop2, programCounter = nextPc }
			Not -> vm { stack = (if top1 > 0 then -1 else 1) : pop1, programCounter = nextPc }
			Br -> vm { stack = pop2, programCounter = if top2 > 0 then top1 else nextPc } 
			Dup -> vm { stack = top1 : st, programCounter = nextPc }
			Inc -> vm { stack = (top1 + 1) : pop1, programCounter = nextPc } 
			Dec -> vm { stack = (top1 - 1) : pop1, programCounter = nextPc }
			Swp -> vm { stack = top2 : top1 : pop2, programCounter = nextPc }

endState :: VM -> Bool
endState vm = programCounter vm >= Vector.length (byteCode vm)

errState :: VM -> Bool
errState vm = st || pc || err
	where
		st = case stack vm of
			[]  -> True
			[_] -> True
			_   -> False
		pc = programCounter vm < 0
		err = isErr vm

run :: VM -> VM
run = until endState step

runCount :: VM -> (Int, VM)
runCount = untilCount endState step

debug :: (VM -> String) -> VM -> (VM, [String])
debug watch vm = if endState vm then (vm, []) else (nextVm, watch vm : logs)
	where
		(nextVm, logs) = debug watch (step vm)

instructionLogger :: VM -> String
instructionLogger vm = show (toEnum $ byteCode vm ! programCounter vm :: OpCode)

watch :: Int -> VM -> String
watch n vm = case IM.lookup n (memory vm) of
	Nothing -> "undefined"
	Just a -> show a

composeLoggers :: [VM -> String] -> VM -> String
composeLoggers loggers vm = (intercalate "  " . map ($ vm)) loggers

printDebug :: (VM -> String) -> VM -> IO ()
printDebug f v = putStr $ unlines $ snd $ debug f v