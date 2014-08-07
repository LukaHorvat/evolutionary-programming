module Environment where

import VM
import Parser
import Data.Vector ((!))

data Environment = Environment {
	qualifier :: VM -> Bool,
	stepFitness :: VM -> Float,
	resultsFitness :: Stats -> Float,
	endCondition :: VM -> Bool,
	settings :: Settings
	}

data Settings = Settings {
	maxCycles :: Int
	}

data Stats = Stats {
	stores :: [(Int, Int)],
	cycles :: Int
	}

empty :: Stats
empty = Stats { stores = [], cycles = 0 }

incCycles :: Stats -> Stats
incCycles stats = stats { cycles = cycles stats + 1 }

combineStats :: Stats -> Stats -> Stats
combineStats a b = Stats { stores = stores a ++ stores b, cycles = max (cycles a) (cycles b) }

type StatLogger = VM -> Stats

memoryLogger :: StatLogger
memoryLogger vm = if (toEnum $ byteCode vm ! programCounter vm) == Store
	then empty { stores = [(top1, top2)] }
	else empty
	where
		top1 = head $ stack vm
		top2 = head $ tail $ stack vm

composeStatLoggers :: [StatLogger] -> StatLogger
composeStatLoggers loggers vm = foldl combineStats empty $ map ($ vm) loggers

evaluate :: Environment -> VM -> Maybe (Stats, VM)
evaluate env vm = if qualifier env vm then Just $ evaluateAcc vm empty else Nothing
	where
		evaluateAcc vm stats
			| endCondition env vm == True			     = (stats, vm)
			| cycles stats == (maxCycles $ settings env) = (stats, vm)
			| otherwise								     = evaluateAcc (step vm) (incCycles $ combineStats stats $ memoryLogger vm)