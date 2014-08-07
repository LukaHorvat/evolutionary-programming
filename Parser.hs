{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parser where

import Data.Char (toUpper)

type ByteCode = [Int]

data OpCode = Push | Pop | Add | Sub | Mult | Div | Store | Load | Jmp | Cmp | Not | Br | Dup | Inc | Dec | Swp
	deriving (Enum, Read, Show, Ord, Eq, Bounded)

charIsNumeric :: Char -> Bool
charIsNumeric c = '0' <= c && '9' >= c

stringIsNumeric :: String -> Bool
stringIsNumeric ('-' : s) = all charIsNumeric s
stringIsNumeric s = all charIsNumeric s

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

wordToByteCode :: String -> Int
wordToByteCode str = if stringIsNumeric str then read str else fromEnum opCodeEnum
	where
		opCodeEnum :: OpCode
		opCodeEnum = read $ capitalize str

stringToByteCode :: String -> ByteCode
stringToByteCode = map wordToByteCode . words

sourceToByteCode :: String -> ByteCode
sourceToByteCode = map wordToByteCode . concatMap words . lines

data StaticCheckerOption = ValidInstruction

check :: [StaticCheckerOption] -> ByteCode -> Bool
check opts code = all (($ map toEnum code) . checkOption) opts
	where
		opCodes = [minBound :: OpCode .. maxBound :: OpCode]
		checkOption ValidInstruction code = case code of
			[] 			  			  -> True
			Push : _ : xs 			  -> checkOption ValidInstruction xs
			x : xs | x `elem` opCodes -> checkOption ValidInstruction xs
			_						  -> False
{-		instructionsOf code index set = case code of
			[] 						 -> set
			Push : _ : xs			 -> instructionsOf xs (index + 2) (Set.insert index set)
			x : xs					 -> instructionsOf xs (index + 1) (Set.insert index set)
		instructionIndices = instructionsOf (map toEnum code) 0 Set.empty --unused -}