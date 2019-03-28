-- Michael Bilan
-- CS 331
-- Assn. 5 Exercise C
-- This Haskell program takes any sequence of integers from the user
-- (one per line), terminating with an empty line. When terminated,
-- the program prints the median of the entered values, and asks the
-- user if they would like to go again.
--
-- NOTE:	This program does not check input validity; it assumes all
--			users will 'play nice' with instructions.

module Main where

import System.IO
import Data.List

main :: IO ()
main = do
    putStrLn ""
    putStrLn "This program will find the median value of a sequence of integer inputs."
    putStrLn "(Note: Bad input will crash the program!)"
    putStrLn ""
	hflush stdout
	
    inputs <- getUserInputs
	print (sort inputs)
	
    _ <-getLine
    return()


-- getUserInputs
getUserInputs :: IO [Int]
getUserInputs = do
    input <- getLine
	let n = read input
	
	if input == ""
	    then return []
	else do
		nextInput <- getUserInputs
		return (input : nextInput)