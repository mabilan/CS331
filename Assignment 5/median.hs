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

import Data.List
import System.IO

-- getUserInputs
-- Returns IO wrapped list of user inputs.
getUserInputs = do
    putStrLn "Enter an integer (blank line to end): "
    input <- getLine
    if input == "" then
        return ([])
    else do
        let thisInt = (read input) :: Int
        nextInt <- getUserInputs
        return (thisInt:nextInt)

-- getMedian
-- Returns median of list
getMedian list = list !! ((length list) `div` 2)

main = do
    putStrLn ""
    putStrLn "Enter a list of integers to find their median."
    putStrLn "(Note: I don't like bad input)"
    putStrLn ""
    userInputs <- getUserInputs

    if (null userInputs) then do
        putStrLn "Empty lists do not have medians"
        restartCheck
    else do
        putStrLn ""
        putStrLn ("Sorted List: " ++ show (sort userInputs))
        putStrLn ("Median: " ++ show (getMedian (sort userInputs)))

        restartCheck
    where
        restartCheck = do
            putStrLn ""
            putStr "Find another median [Y/n] "
            choice <- getChar
            putStrLn ""
            if choice == 'Y' then do
                main
            else if choice == 'n' then do
                putStrLn "Good-bye!"
                return ()
            else
                restartCheck