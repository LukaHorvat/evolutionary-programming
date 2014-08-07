{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Utility where

untilCount :: (a -> Bool) -> (a -> a) -> a -> (Int, a)
untilCount pred next state = if pred state then (0, state) else (1 + countRest, nextState)
	where
		(countRest, nextState) = untilCount pred next (next state) 
		
imap :: (Int -> a -> b) -> [a] -> [b]
imap f list = imapAcc list 0
	where
		imapAcc [] _ = []
		imapAcc (x : xs) n = f n x : imapAcc xs (n + 1)