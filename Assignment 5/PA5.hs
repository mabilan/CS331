-- PA5.hs
-- Glenn G. Chappell, Michael Bilan
--
-- For CS F331 / CSCE A331 Spring 2019
-- Solutions to Assignment 5 Exercise B

module PA5 where


-- collatzCounts
-- List of all collatz sequence counts
-- Uses collatzAll
collatzCounts :: [Integer]
collatzCounts = map collatzAll [0..]


-- collatzAll
-- Generates the collatz sequence count for all integers
-- Uses collatzIt


-- collatzIt
-- Given an integer n > 0, apply Collatz function
-- c(n) = 
--		| n/2 if n is even
--		| 3n+1 if n is odd
collatzIt :: Int -> Int
collatzIt n
	| even n	= n div 2
	| otherwise	= 3 * n + 1


--https://stackoverflow.com/questions/49502376/haskell-count-of-all-elements-in-list-of-lists
--	http://notes-on-haskell.blogspot.com/2007/06/solving-collatz-sequences.html



-- findList
findList :: Eq a => [a] -> [a] -> Maybe Int
findList [x] [x]
	|
	|

-- operator ##
(##) :: Eq a => [a] -> [a] -> Int
_ ## _ = 42  -- DUMMY; REWRITE THIS!!!

-- filterAB
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB _ _ bs = bs  -- DUMMY; REWRITE THIS!!!
-- sumEvenOdd
sumEvenOdd :: Num a => [a] -> (a, a)
{-
  The assignment requires sumEvenOdd to be written using a fold.
  Something like this:
    sumEvenOdd xs = fold* ... xs where
        ...
  Above, "..." should be replaced by other code. The "fold*" must be
  one of the following: foldl, foldr, foldl1, foldr1.
-}
sumEvenOdd _ = (0, 0)  -- DUMMY; REWRITE THIS!!!