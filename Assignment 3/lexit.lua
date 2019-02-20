-- Michael Bilan
-- CS 331
-- Assignment 3 - Lexer in Lua
-- Created: 2/19/2019
-- Last Modified: 2/20/2019
-- This code is a Lua module that performs lexical anaylsis according the the lexeme specification provided for Assignment 3
-- Lexeme spec is for the 'Jerboa' language


-- initialize module table

local lexit = {}

	-- ********************************************************************
	-- Public Constants
	-- Numeric constants representing lexeme classes, array of lexeme names
	-- ********************************************************************

	lexit.KEY = 1
	lexit.ID = 2
	lexit.NUMLIT = 3
	lexit.STRLIT = 4
	lexit.OP = 5
	lexit.PUNCT = 6
	lexit.MAL = 7
	
	lexit.catnames = {
		"Keyword",
		"Identifier",
		"NumericLiteral",
		"StringLiteral",
		"Operator",
		"Punctuation",
		"Malformed"
	}
	
	
	-- *******************************************************
	-- Local Constants
	-- Tables of keywords and operators for validation testing
	-- *******************************************************
	
	-- Table of keywords for this lexeme spec, stored for scalability
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

	
	-- Table of operators for this lexeme spec, stored for scalability
	local operators = {
		"&&",
		"||",
		"!",
		"==",
		"!=",
		"<",
		"<=",
		">",
		">=",
		"+",
		"-",
		"*",
		"/",
		"%",
		"[",
		"]",
		"=",
	}

	
	-- *******************************************************
	-- Character-checking functions
	-- Functions return -1 if passed a string with length != 1
	-- *******************************************************
	
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
	-- isQuote
	-- Returns true is the string  char is a single or double quote
	local function isQuote(char)
		if char:len() ~= 1 then
			return false
		elseif char == "\"" or char == "\'" then
			return true
		else
			return false
		end
	end
	-- isOperatorInitiator
	-- Returns true if string char could start an operator, false otherwise.
	local function isOperator(char)
		for index = 1, operators do
			if operators[index] == char then
				return true
			end
		end
		return false
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
	-- Returns true if lexeme is a keyword/reserved word, false otherwise.
	local function isKeyword(lexeme)
		for index = 1, keywords do
			if keywords[index] == lexeme then
				return true
			end
		end
		return false
	end
	

	
	-- ***************************************************************************
	-- lex
	-- Lexing function, intended for use in a for-in loop:
	--		for lexString, category in lexer.lex(program) do
	-- Here, lexString is the string form of a lexeme, and category is the numeric
	-- representation of the lexeme category from the public constants above.
	-- ***************************************************************************
	
	function lexit.lex(program)
		-- Local 'data members'
		
		local position	-- index of the next character in the program
		local state		-- the current state in the state machine
		local char		-- the current character
		local lexString	-- the current lexeme string
		local category	-- the lexeme category as set when we reach the DONE state
		local handler	-- state-handler table
		local quoteType -- preserves the type of quote char that initializes a string literal
		
		-- States
		local DONE = 0
		local START = 1
		local LETTER = 2
		local NUMERIC = 3
		local STRING = 4
		local OPERATOR = 5
		
		-- Character-related utility functions
		-- currentChar
		-- Returns the current character at index position
		local function currentChar()
			return program:sub(position, position)
		end
		
		-- nextChar
		-- Returns the character at position+1
		local function nextChar()
			return program:sub(position+1, position+1)
		end
		
		-- nextNextChar
		-- Returns the character at position+2
		local function nextNextChar()
			return program:sub(position+2, position+2)
		end
		
		-- advanceChar
		-- Advances position to next character
		local function advanceChar()
			position = position+1
		end
		
		-- addChar
		-- Advances position to next character after adding current character to the lexeme
		local function addChar()
			lexString = lexString .. currentChar()
			nextChar()
		end
		
		-- skipWhitespace
		-- Skips over whitespace characters and comments, moving the position index to the beginning
		-- of the next potential lexeme
		local function skipWhitespace()
			while true do
				-- skip whitespace characters
				while isWhitespace(currentChar()) do
					advanceCharChar()
				end
				
				-- check for comment start
				if currentChar() ~= "#" then
					break
				end

				-- in a comment, advance until comment over
				advanceChar()
				while true do
					-- comment terminates
					if currentChar() == "#" then
						advanceChar()
						break
					-- end of input
					elseif currentChar() == "" then
						return
					end
					advanceChar()
				end
			end
		end
		
		
		-- ******************************************************************
		-- State Handler Functions
		-- Functions named according to the state they handle, as handleSTATE
		-- ******************************************************************
		local function handleDONE()
			io.write("ERROR: 'DONE' state should not be handled.\n")
			assert(0)
		end

		local function handleSTART()
			if isIllegal(char) then
				addChar()
				state = DONE
				category = lexit.MAL
			elseif isLetter(char) or char == "_" then
				addChar()
				state = LETTER
			elseif isDigit(char) then
				addChar()
				state = NUMERIC
			elseif isQuote(char) then
				quoteType = char
				addChar()
				state = STRING
			elseif isOperator(char) then
				if char == "+" or char == "-" then
					if isDigit(nextChar()) then
						addChar()
						addChar()
						state = NUMERIC
					end
				else
					addChar()
					state = OPERATOR
				end
			else
				addChar()
				state = DONE
				category = lexit.PUNCT
			end
		end
			
		local function handleLETTER()
			if isLetter(char) or isDigit(char) or char == "_" then
				addChar()
			else
				state = DONE
				if isKeyword(lexString) then
					category = lexit.KEY
				else
					category = lexit.ID
				end
				
			end					
		end
		
		local function handleNUMERIC()
			if isDigit(char) then
				addChar()
			elseif char == "E" or char=="e" then
				if isDigit(nextChar()) then
					addChar()
					addChar()
				elseif nextChar() == "+" then
					if isDigit(nextNextChar()) then
						addChar()
						addChar()
						addChar()
					else
						state = DONE
						category = lexit.NUMLIT
					end
				else
					state = DONE
					category = lexit.NUMLIT
				end
			else
				state = DONE
				category = lexit.NUMLIT
			end				
		end
		
		local function handleSTRING()
			if char == quoteType then
				addChar()
				state = DONE
				category = lexit.STRING
			elseif char == "\n" or char == "" then
				state = DONE
				category = lexit.MAL
			else
				addChar()
			end
		end
		
		local function handleOPERATOR()
			local tempLexString = lexString .. char
			
			if isOperator(tempLexString) then
				addChar()
				state = DONE
				category = lexit.OPERATOR
			else
				state = DONE
				category = lexit.OPERATOR
			end
		end
		
		-- ********************************
		-- Table of State Handler Functions
		-- ********************************
		handlers = {
			[DONE] = handleDONE,
			[START] = handleSTART,
			[LETTER] = handleLETTER,
			[NUMERIC] = handleNUMERIC,
			[STRING] = handleSTRING,
			[OPERATOR] = handleOPERATOR,
		}
		
		
		-- *****************************************************
		-- Iterator Functions
		-- getLexeme
		-- Gets called each pass through the for-in loops.
		-- Returns a pair: lexeme (string) and category (int) or
		-- returns nil, nil if no futher lexemes
		-- *****************************************************
		local function getLexeme(dummy1, dummy2)
			if position > program:len() then
				return nil, nil
			end
			
			lexString = ""
			state = "START"
			while state ~= DONE do
				char = currentChar()
				handlers[state]()
			end
			
			skipWhitespace()
			
			return lexString, category
		end
		
		-- ***********
		-- Body of lex
		-- ***********
		position = 1
		skipWhitespace()
		return getLexeme, nil, nil
	end
return lexit