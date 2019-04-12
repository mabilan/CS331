-- lexit.lua
-- Michael Bilan
-- Started: 2019/02/19
-- Last Modified: 2019/02/20
-- Based on code from:
-- 		lexit.lua
--		Glenn G. Chappell
-- *********************************************************************
-- Module Table Initialization
-- *********************************************************************
local lexit = {}  

-- *********************************************************************
-- Public Constants
-- *********************************************************************
	-- Numeric constants representing lexeme categories
	lexit.KEY    = 1
	lexit.ID     = 2
	lexit.NUMLIT = 3
	lexit.STRLIT = 4
	lexit.OP     = 5
	lexit.PUNCT  = 6
	lexit.MAL    = 7

	-- catnames
	-- Array of names of lexeme categories.
	-- Human-readable strings. Indices are above numeric constants.
	lexit.catnames = {
		"Keyword",
		"Identifier",
		"NumericLiteral",
		"StringLiteral",
		"Operator",
		"Punctuation",
		"Malformed"
	}

	-- keywords
	-- Local table containing all lexeme keywords
	local keywords = {
		"cr",
		"def",
		"else",
		"elseif",
		"end",
		"false",
		"if",
		"readnum",
		"return",
		"true",
		"while",
		"write",
	}

-- *********************************************************************
-- Kind-of-Character Functions
-- *********************************************************************
	-- All functions return false when given a string whose length is not
	-- exactly 1.
	-- isLetter
	-- Returns true if string char is a letter character, false otherwise.
	local function isLetter(char)
		if char:len() ~= 1 then
			return false
		elseif char >= "A" and char <= "Z" then
			return true
		elseif char >= "a" and char <= "z" then
			return true
		else
			return false
		end
	end
	-- isDigit
	-- Returns true if string char is a digit character, false otherwise.
	local function isDigit(char)
		if char:len() ~= 1 then
			return false
		elseif char >= "0" and char <= "9" then
			return true
		else
			return false
		end
	end
	-- isWhitespace
	-- Returns true if string char is a whitespace character, false otherwise.
	local function isWhitespace(char)
		if char:len() ~= 1 then
			return false
		elseif char == " " or char == "\t" or char == "\n" or char == "\r"
		  or char == "\f" then
			return true
		else
			return false
		end
	end
	-- isIllegal
	-- Returns true if string char is an illegal character, false otherwise.
	local function isIllegal(char)
		if char:len() ~= 1 then
			return false
		elseif isWhitespace(char) then
			return false
		elseif char >= " " and char <= "~" then
			return false
		else
			return true
		end
	end
	-- isKeyword
	-- Returns true if the string lexeme matches a keyword from the lexeme
	-- specification, false otherwise.
	local function isKeyword(lexeme)
		for index, value in ipairs(keywords) do
			if lexeme == value then
				return true
			end
		end
		return false
	end
	-- isOperator
	-- Returns true if the string lexeme matches an operator from the lexeme
	-- specification, false otherwise.
	local function isOperator(lexeme)
		for index, value in ipairs(operators) do
			if lexeme == value then
				return true
			end
		end
		return false
	end
	
	-- *********************************************************************
	-- The Lexer
	-- *********************************************************************
	-- lex
	-- Our lexer
	-- Intended for use in a for-in loop:
	--     for lexString, cat in lexit.lex(program) do
	-- Here, lexString is the string form of a lexeme, and cat is a number
	-- representing a lexeme category. (See Public Constants.)
	function lexit.lex(program)
		-- ***** Variables (like class data members) *****
		local position  -- Index of next character in program
						-- INVARIANT: when getLexeme is called, position is
						--  EITHER the index of the first character of the
						--  next lexeme OR program:len()+1
		local state     -- Current state for our state machine
		local char      -- Current character
		local lexString  -- The lexeme, so far
		local category  -- Category of lexeme, set when state set to DONE
		local handlers  -- Dispatch table; value created later
		local previousLexeme	-- copy of the most recent lexeme, if one exists
		local previousCategory	-- copy of the most recent lexeme category, if one exists
		
		-- ***** States *****
		local DONE   = 0
		local START  = 1
		local LETTER = 2
		local NUMERIC = 3
		local EXPONENTIAL = 4
		local STRING = 5
		local NUMOPERATOR = 6
		local BOOLOPERATOR = 7
		
		-- ***** Character-Related Utility Functions *****
		-- currChar
		-- Return the current character, at index position in program. Return
		-- value is a single-character string, or the empty string if position is
		-- past the end.
		local function currChar()
			return program:sub(position, position)
		end
		-- nextChar
		-- Return the next character, at index position+1 in program. Return
		-- value is a single-character string, or the empty string if position+1
		-- is past the end.
		local function nextChar()
			return program:sub(position+1, position+1)
		end
		-- nextNextChar
		-- Return the next next character, at index position+2 in program. Return
		-- value is a single-character string, or the empty string if position+2
		-- is past the end.
		local function nextNextChar()
			return program:sub(position+2, position+2)
		end
		-- drop1
		-- Move position to the next character.
		local function drop1()
			position = position+1
		end
		-- add1
		-- Add the current character to the lexeme, moving position to the next
		-- character.
		local function add1()
			lexString = lexString .. currChar()
			drop1()
		end
		-- skipWhitespace
		-- Skip whitespace and comments, moving position to the beginning of
		-- the next lexeme, or to program:len()+1.
		local function skipWhitespace()
			while true do      -- In whitespace
				while isWhitespace(currChar()) do
					drop1()
				end
				if currChar() ~= "#" then -- Comment?
					break
				end
				drop1()
				while true do  -- In comment
					if currChar() == "\n" then
						drop1()
						break
					elseif currChar() == "" then  -- End of input?
					   return
					end
					drop1()
				end
			end
		end
		-- munchOverride
		-- Returns true if previous lexeme would override the max munch rule
		-- based on lexeme spec, false otherwise.
		local function munchOverride()
			return previousLexeme == ")" or previousLexeme == "]"
				or previousLexeme == "true" or previousLexeme == "false"
				or previousCategory == lexit.ID or previousCategory == lexit.NUMLIT
		end
		-- ***** State-Handler Functions *****
		-- A function with a name like handle_XYZ is the handler function
		-- for state XYZ
		local function handle_DONE()
			io.write("ERROR: 'DONE' state should not be handled\n")
			assert(0)
		end
		local function handle_START()
			if isIllegal(char) then
				add1()
				state = DONE
				category = lexit.MAL
			elseif isLetter(char) or char == "_" then
				add1()
				state = LETTER
			elseif isDigit(char) then
				add1()
				state = NUMERIC
			elseif char == "\'" or char == "\"" then
				add1()
				state = STRING
			elseif char == "+" or char == "-" then
				if isDigit(nextChar()) and not munchOverride() then
					add1()
					state = NUMERIC
				else
					add1()
					state = DONE
					category = lexit.OP
				end
			elseif char == "*" or char == "%" or char == "/"
				or char == "[" or char == "]" then
					add1()
					state = DONE
					category = lexit.OP
			elseif char == "!" or char == "=" or char == "<" or char == ">" then
				add1()
				state = NUMOPERATOR
			elseif char == "|" or char == "&" then
				add1()
				state = BOOLOPERATOR
			else
				add1()
				state = DONE
				category = lexit.PUNCT
			end
		end
		local function handle_LETTER()
			if isLetter(char) or isDigit(char) or char == "_" then
				add1()
			else
				state = DONE
				if isKeyword(lexString) then
					category = lexit.KEY
				else
					category = lexit.ID
				end
			end
		end
		local function handle_NUMERIC()
			if isDigit(char) then
				add1()
			elseif char == "E" or char == "e" then
				if isDigit(nextChar()) then
					add1()
					add1()
					state = EXPONENTIAL
				elseif nextChar() == "+" and isDigit(nextNextChar()) then
					add1()
					add1()
					add1()
					state = EXPONENTIAL
				else
					state = DONE
					category = lexit.NUMLIT
				end
			else
				state = DONE
				category = lexit.NUMLIT
			end
		end
		local function handle_EXPONENTIAL()
			if isDigit(char) then
				add1()
			else
				state = DONE
				category = lexit.NUMLIT
			end
		end
		local function handle_STRING()
			if char == "\n" or char == "" then
				add1()
				state = DONE
				category = lexit.MAL
			elseif char == string.sub(lexString, 1, 1) then
				add1()
				state = DONE
				category = lexit.STRLIT
			else
				add1()
			end
		end
		local function handle_NUMOPERATOR()
			if char == "=" then
				add1()
			end
			state = DONE
			category = lexit.OP
		end
		local function handle_BOOLOPERATOR()
			if char == lexString then
				add1()
				state = DONE
				category = lexit.OP
			else
				state = DONE
				category = lexit.PUNCT
			end
		end
		
		-- ***** Table of State-Handler Functions *****
		handlers = {
			[DONE]=handle_DONE,
			[START]=handle_START,
			[LETTER]=handle_LETTER,
			[NUMERIC]=handle_NUMERIC,
			[EXPONENTIAL]=handle_EXPONENTIAL,
			[STRING]=handle_STRING,
			[NUMOPERATOR]=handle_NUMOPERATOR,
			[BOOLOPERATOR]=handle_BOOLOPERATOR,
		}
		-- ***** Iterator Function *****
		-- getLexeme
		-- Called each time through the for-in loop.
		-- Returns a pair: lexeme-string (string) and category (int), or
		-- nil, nil if no more lexemes.
		local function getLexeme(dummy1, dummy2)
			if position > program:len() then
				return nil, nil
			end
			lexString = ""
			state = START
			while state ~= DONE do
				char = currChar()
				handlers[state]()
			end
			skipWhitespace()
			previousLexeme, previousCategory = lexString, category
			return lexString, category
		end
		-- ***** Body of Function lex *****
		-- Initialize & return the iterator function
		position = 1
		skipWhitespace()
		return getLexeme, nil, nil
	end
	-- *********************************************************************
	-- Module Table Return
	-- *********************************************************************
return lexit
