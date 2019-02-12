-- Michael Bilan
-- CS 331 Homework Assignment 2
-- Created: 8 Feb 2019
-- Updated: 12 Feb 2019
-- This file is a Lua module containing 4 module functions:
--		mapTable, concatMax, collatz, and backSubs
-- Requirements for each function is defined in the assignment, though a brief description
-- of each functions' tasks is provided.

local pa2 = {}

	-- function mapTable
	-- takes a function f and table t; function f is a one-parameter function accepting any value in table t
	-- mapTable returns a table of key-value pairs, such  that the newPair = oldPair[k, f(v)]
	function pa2.mapTable(f, t)
		local tempTable = {}
		
		-- iterate over all key-value pairs, storing new key-value results in a local table
		for k,v in pairs(t) do
			tempTable[k] = f(v)
		end
		
		-- return the new result table
		return tempTable
	end


	-- function concatMax
	-- takes a string and an integer
	-- returns a string which is as many concatonations of the given string of length not exceeding given integer
	-- if no valid string exists (e.g. string length is greater than integer provided), returns the empty string
	function pa2.concatMax(myString, maxLength)
		-- empty string is the base case
		local concatonatedString = ""
		local stringLength = string.len(myString)
		
		for i = stringLength, maxLength, stringLength do
			concatonatedString = concatonatedString .. myString
		end
		
		return concatonatedString
	end

	-- function collatz
	-- takes an integer k and returns an iterator that produces one or more integers that are entries in the Collatz sequence
	-- c(n) = 3n+1 if n=odd
	-- c(n) = n/2 if n=even
	-- Sequqence terminates when c(n) = 1
	function pa2.collatz(k)
		local function collatzIterator()
				-- confirm k is greater than 1 (still actively evaluating collatz #s)
				if k>1 then
					local saved_k = k
					-- case 1: k is odd
					if k%2==1 then
						k = 3*k + 1
					-- case 2: k is even
					else
						k = k/2
					end
					return saved_k
				-- terminating case: k==1
				elseif k==1 then
					k = 0
					return 1
				else
					return nil
				end
		end
		return collatzIterator, nil, nil
	end

	-- function backSubs
	-- takes a single string parameter s and yields all sub-strings of the reverse of s (starting from empty string)
	function pa2.backSubs(s)		
		-- base-case yield empty string
		coroutine.yield("")
		
		local stringLength = string.len(s)
		local reversedString = ""
		-- reverse the string by iterating over all characters, concatonating current char with previous chars (prepended)
		for char = 1, stringLength do
			reversedString = string.sub(s, char, char)..reversedString
		end
		
		-- nested loop to print all substrings from size 1 to stringLength (full reversed string)
		for substringLength = 1, stringLength do
			for startChar = 1, stringLength-(substringLength-1) do
				coroutine.yield(string.sub(reversedString, startChar, startChar + substringLength - 1))
			end
		end
	end

return pa2