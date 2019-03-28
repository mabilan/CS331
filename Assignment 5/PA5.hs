-- PA5.hs
-- Glenn G. Chappell, Michael Bilan
--
-- For CS F331 / CSCE A331 Spring 2019
-- Solutions to Assignment 5 Exercise B

module PA5 where

---------------------------------------
-- Collatz Sequence Counting Problem --
---------------------------------------

-- collatzCounts
-- List of all collatz sequence counts
-- Uses collatzCounter
collatzCounts :: [Int]
collatzCounts = map collatzCounter [1..]

-- collatzCounter
-- Counts the number of items in the Collatz Sequence for a given integer
collatzCounter n = length (collatzSequence n)


-- collatzSequence
-- Generates the list of collatz sequence values for a given starting integer
-- Uses collatzFunction
collatzSequence :: Int -> [Int]
collatzSequence = terminate . iterate collatzFunction
    where
        terminate (1:_) = []
        terminate (x:xs) = x:terminate xs

-- collatzFunction
-- Given an integer n > 0, apply Collatz function
-- c(n) = 
--		| n/2 if n is even
--		| 3n+1 if n is odd
collatzFunction :: Int -> Int
collatzFunction 1 = 1
collatzFunction n
        | even n    = n `div` 2
        | otherwise = 3 * n + 1



----------------------------------
-- Find Contiguous List Problem --
----------------------------------

-- findList
-- Determines starting index of contiguous sublist given a list, if one is found
findList :: Eq a => [a] -> [a] -> Maybe Int
findList needle haystack = subListChecker needle haystack 0

-- subListChecker
-- Recursive helper for findList to manage indexes checked
subListChecker :: Eq a => [a] -> [a] -> Int -> Maybe Int
subListChecker [] _ _ = Just 0
subListChecker _ [] _ = Nothing
subListChecker needle haystack index =
    if needle == take (length needle) haystack then Just index
    else subListChecker needle (tail haystack) (index+1)



----------------------------
-- Infix Operator Problem --
----------------------------

-- Infix operator ##
-- Compares two lists of the same type, returns the number of indices
-- that match value
(##) :: Eq a => [a] -> [a] -> Int
_ ## [] = 0
[] ## _ = 0
list1 ## list2 = length $ filter (True==) $ zipWith (==) list1 list2


----------------------------
-- Boolean Filter Problem --
----------------------------

-- filterAB
-- Applies boolean expression to list 1; for each true result, make new list
-- containing corresponding list 2 index
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB _ [] _ = []
filterAB _ _ [] = []
filterAB predicate list1 list2 = map snd $ filter ((==True) . fst) $ zip (map predicate list1) list2


--------------------------
-- Sum Even-Odd Problem --
--------------------------

-- sumEvenOdd
-- Given a list, returns the sum of all even indexes and sum of all odd indexes
-- as a pair (odd, even)
sumEvenOdd :: Num a => [a] -> (a, a)
sumEvenOdd xs = (foldl (+) 0 evenNums, foldl (+) 0 oddNums)
    where evenNums = map fst $ filter (even . snd) $ zip xs [0..]
          oddNums = map fst $ filter (odd . snd) $ zip xs [0..]