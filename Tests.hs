module Tests where

import qualified Parser as Parser
import qualified VM as VM
import qualified Data.IntMap as IM

bubble :: String
bubble = unlines [
	"push 0", "push 1000", "store",
	"push 0", "push 1001", "store",
	"push 1000", "load", "load",
	"push 1001", "load", "load",
	"cmp",
	"push 38", "br",
	"push 1000", "load", "load",
	"push 1001", "load", "load",
	"push 1000", "load", "store",
	"push 1001", "load", "store",
	"push 1001", "load", "inc", "dup", "push 1001", "store",
	"push 500",
	"cmp", "not",
	"push 10", "br", 
	"push 0", "push 1001", "store",
	"push 1000", "load", "inc", "dup", "push 1000", "store",	
	"push 500",
	"cmp", "not",
	"push 10", "br"
	]

vm :: VM.VM
vm = VM.fromCode $ Parser.sourceToByteCode bubble

vmWithData :: VM.VM
vmWithData = vm { VM.memory = IM.fromList $ zip [0..500] [500, 499..0] }

main :: IO ()
main = print $ fst $ VM.runCount $ vmWithData

dbg :: IO ()
dbg = VM.printDebug (VM.composeLoggers [VM.instructionLogger, VM.watch 101, show . VM.programCounter]) vmWithData
{-
for i from 0 to 10
	for j from 0 to 10
		if m[i] > m[j]
			swap m, i, j
-}