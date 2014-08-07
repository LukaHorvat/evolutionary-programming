{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Genetics where

import VM
import Parser
import System.Random
import Utility
import qualified Data.Vector as Vector

test :: (VM -> Float) -> (VM -> Float) -> (VM -> Bool) -> VM -> Float 
test stepFitness finalFitness terminationPred vm = 
	if endState vm then
		finalFitness vm 
	else if terminationPred vm then
		0 
	else 
		stepFitness vm + test stepFitness finalFitness terminationPred (step vm)

mutate :: StdGen -> ByteCode -> (ByteCode, StdGen)
mutate rand code = (
	if rn > 0.2 
		then mutateBit randomBit code
	else if rn > 0.1
		then insertBit randomBit code
	else
		removeBit randomBit code
	, next4
	)
	where
		len = length code
		rn :: Float
		(rn, next1) = random rand
		(randomBit, next2) = randomR (0, len - 1) next1
		(increment, next3) = random next2
		(insertCmd, next4) = randomR (fromEnum $ (minBound :: OpCode), fromEnum $ (maxBound :: OpCode)) next3
		
		mutateBit bit code = take bit code ++ ((if increment then h + 1 else h - 1) : tail rest)
			where 
				rest = drop bit code
				h = head rest
		insertBit bit code = take bit code ++ (insertCmd : drop bit code)
		removeBit bit code = take bit code ++ drop (bit + 1) code