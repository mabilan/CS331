-- parseit.lua
-- Michael Bilan
-- 8 Mar 2019
-- For CS 331 Assignment 4
-- Parses Jerboa language code.
-- REQUIRES: lexit.lua
-- Based on rdparser4.lua, by G. Chappell

-- Grammar
--		program		→	stmt_list
--		stmt_list	→	{ statement }
--		statement	→	‘write’ ‘(’ write_arg { ‘,’ write_arg } ‘)’
--				|  	‘def’ ID ‘(’ ‘)’ stmt_list ‘end’
--				|  	‘if’ expr stmt_list { ‘elseif’ expr stmt_list } [ ‘else’ stmt_list ] ‘end’
--				|  	‘while’ expr stmt_list ‘end’
--				|  	‘return’ expr
--				|  	ID ( ‘(’ ‘)’ | [ ‘[’ expr ‘]’ ] ‘=’ expr )
--		write_arg	→  	‘cr’
--				|  	STRLIT
--				|  	expr
--		expr		→  	comp_expr { ( ‘&&’ | ‘||’ ) comp_expr }
--		comp_expr	→  	‘!’ comp_expr
--				|  	arith_expr { ( ‘==’ | ‘!=’ | ‘<’ | ‘<=’ | ‘>’ | ‘>=’ ) arith_expr }
--		arith_expr	→  	term { ( ‘+’ | ‘-’ ) term }
--		term		→  	factor { ( ‘*’ | ‘/’ | ‘%’ ) factor }
--		factor		→  	‘(’ expr ‘)’
--				|  	( ‘+’ | ‘-’ ) factor
--				|  	NUMLIT
--				|  	( ‘true’ | ‘false’ )
--				|  	‘readnum’ ‘(’ ‘)’
--				|  	ID [ ‘(’ ‘)’ | ‘[’ expr ‘]’ ]
--
-- All operators are left-associative.
--
-- AST Spec:
--		For program → stmt_list, the AST for the program is the AST for the stmt_list.
--		For stmt_list, the AST is an array with first item STMT_LIST. If there are no statements,
--			the AST is simply {STMT_LIST}
--		For statement: write, the AST is an array with first item WRITE_STMT.
--		For statement: function definition (def), the AST is {FUNC_DEF, II, SSS}, where II is
--			the string form of the ID lexeme, SSS is the AST for stmt_list. 
--		For statement: if, the AST is an array with first item IF_STMT.
--		For statement: while, the AST the array {WHILE_STMT, EEE, SSS} where EEE is the AST for
--			the expression (expr), and SSS is the AST for the stmt_list.
--		For statement: return, the AST is the array {RETURN_STMT, EEE} where EEE is the AST for
--			the expression (expr).
--		For statement: beginning with identifier:
--			If the ID is followed by parantheses, the AST is {FUNC_CALL, II}, where II is the
--				string form of the ID lexem.
--			If the ID is followed by "=" (no brackets), the AST is {ASSN_STMT, {SIMPLE_VAR,II}, EEE}
--				where II is the string for of the ID lexeme, and EEE is the AST for expr.
--			If the ID is followed by "[", the AST is {ASSN_STMT, {ARRAY_VAR, II, EEE}, FFF}, where
--				II is the string form of the ID lexeme, EEE is the AST for the expr between brackets,
--				and FFF is the AST for the expr after an "="
--		For write argument: carriage-return (cr), the AST is {CR_OUT}
---		For write argument: string literal (STRLIT), the AST is {STRLIT_OUT, SS}, where SS is the
--			string form of the STRLIT lexeme
--		For write argument: expression,  the AST is the AST for expr
--		For expression, the AST is the AST for comp_expr if there is a single comparison expression.
--			Otherwise, the AST is {{BIN_OP, OO}, AAA, BBB} where OO is the string form of the LAST
--			operator, AAA is the string for everything preceding it, and BBB is the AST for the last
--			comp_expr.
--		For comparison expression: not, the AST is {{UN_OP, "!"}, CCC} where CCC is the comp_expr
--			of  the right-hand side.
--		For comparison expression: arithmetic expression (arith_expr), the AST is that of arith_expr
--			for a single arithmetic expression. Otherwise, the AST is {{BIN_OP, OO}, AAA, BBB} where
--			OO is the string form of the LAST operator, AAA is the AST for everything preceding it,
--			BBB is the AST for the last arith_expr.
--		For arithmetic expression, the AST is the AST for term if there is a single arith_expr.
--			Otherwise, the AST is {{BIN_OP, OO} AAA, BBB} where OO is the string form of the LAST
--			operator, AAA is the AST preceding it, and BBB is the AST for the last term.
--		For term, the AST is the AST for factor for a single term. Otherwise, the AST is
--			{{BIN_OP, OO}, AAA, BBB} where OO is the string for of the LAST operator, AAA is the AST
--			for everything preceding it, and BBB is the AST for the last factor.
--		For factor: paranthesized expression, the AST is the AST of the expression.
--		For factor: unary operator, the AST is {{UN_OP, OO}, FFF} where OO is the string form of the
--			operator, and FFF is the AST for the factor on the right-hand side.
--		For factor: numerical literal, the AST is {NUMLIT_VAL, NN} where NN is the string form of the 
--			NUMLIT lexeme.
--		For factor: boolean literal, the AST is {BOOLLIT_VAL, BB}, where BB is the string form of the
--			boolean-literal KEY lexeme ("true" or "false").
--		For factor: readnum, the AST is {READNUM_CALL}.
--		For factor: beginning with identifier:
--			If the ID is followed by parantheses, the AST if {FUNC_CALL, II} where II is the string
--				form of the ID lexeme.
--			If the ID is followed by "[", then the AST is {ARRAY_VAR, II, EEE} where II is the string
--				form of the ID lexeme, EEE is the AST for the expression between brackets.
--			If the ID is neither of the above, the AST is {SIMPLE_VAR, II} where II is the string
--				form of the ID lexeme



local parseit = {}

local lexit = require "lexit"


-- *****************************
-- *** Variable Declarations ***
-- *****************************

-- for lexeme iteration
local iteration
local state
local lexit_out_s
local lexit_out_c

-- for current lexeme
-- category 0 indicates EOF
local lexemeString = ""
local lexemeCategory = 0

-- symbolic constants for ASTs
STMT_LIST    = 1
WRITE_STMT   = 2
FUNC_DEF     = 3
FUNC_CALL    = 4
IF_STMT      = 5
WHILE_STMT   = 6
RETURN_STMT  = 7
ASSN_STMT    = 8
CR_OUT       = 9
STRLIT_OUT   = 10
BIN_OP       = 11
UN_OP        = 12
NUMLIT_VAL   = 13
BOOLLIT_VAL  = 14
READNUM_CALL = 15
SIMPLE_VAR   = 16
ARRAY_VAR    = 17



-- *************************
-- *** Utility Functions ***
-- *************************

-- advance
-- Go to next lexeme and load it into lexemeString, lexemeCategory.
-- Should be called once before any parsing is done.
-- Function init must be called before this function is called.
local function advance()
    -- Advance the iterator
    lexer_out_s, lexer_out_c = iter(state, lexer_out_s)

    -- If we're not past the end, copy current lexeme into vars
    if lexer_out_s ~= nil then
        lexemeString, lexemeCategory = lexer_out_s, lexer_out_c
    else
        lexemeString, lexemeCategory = "", 0
    end
end

-- init
-- Initial call. Sets input for parsing functions.
local function init(prog)
    iter, state, lexer_out_s = lexit.lex(prog)
    advance()
end

-- atEnd
-- Return true if pos has reached end of input.
-- Function init must be called before this function is called.
local function atEnd()
    return lexemeCategory == 0
end

-- matchString
-- Given string, see if current lexeme string form is equal to it. If
-- so, then advance to next lexeme & return true. If not, then do not
-- advance, return false.
-- Function init must be called before this function is called.
local function matchString(s)
    if lexemeString == s then
        advance()
        return true
    else
        return false
    end
end

-- matchCat
-- Given lexeme category (integer), see if current lexeme category is
-- equal to it. If so, then advance to next lexeme & return true. If
-- not, then do not advance, return false.
-- Function init must be called before this function is called.
local function matchCat(c)
    if lexemeCategory == c then
        advance()
        return true
    else
        return false
    end
end

-- isStatement
-- Returns true if current lexeme indicates the start of any
-- of the statements defined by the grammar (see above)
local function isStatement()
	return lexemeString == "write"
		or lexemeString == "def"
		or lexemeString == "if"
		or lexemeString == "while"
		or lexemeString == "return"
		or lexemeCategory == lexit.ID
end

-- *****************************
-- *** Primary Function Call ***
-- *****************************

-- local statemets for non-terminal parsing functions
local parse_program
local parse_stmt_list
local parse_statement
local parse_write_arg
local parse_expr
local parse_comp_expr
local parse_arith_expr
local parse_term
local parse_factor

-- local statements for terminal statement handler functions
local handle_statement_def
local handle_statement_if
local handle_statement_while
local handle_statement_return
local handle_statement_ID

-- parse
-- Given program, initialize parser and call parsing function for start
-- symbol. Returns pair of booleans & AST. First boolean indicates
-- successful parse or not. Second boolean indicates whether the parser
-- reached the end of the input or not. AST is only valid if first
-- boolean is true.
function parseit.parse(prog)
    -- Initialization
    init(prog)

    -- Get results from parsing
    local good, ast = parse_program()  -- Parse start symbol
    local done = atEnd()

    -- And return them
    return good, done, ast
end



-- ************************************
-- *** Parsing Functions & Handlers ***
-- ************************************

-- Parsing functions below parse a nonterminal from the Jerboa grammer.
-- Functions parse the nonterminal in their name (e.g. parse_program parses
-- the "program" nonterminal) and returns a pair:
--		{Parse Success Status, AST}
-- When parse success is true, the AST is valid. Otherwise, the AST is
-- invalid, and no guarantees can be made about the current lexeme. The
-- AST grammar and spec above outline the formatting of returned ASTs.
-- Handler functions assist in terminal evaluation for their parent
-- parsing function. (e.g. handle_statement_def assists in parsing the "def"
-- terminal for parent non-terminal stamement)


-- NOTE: Function init must be called prior to any parsing function calls


-- parse_program
-- Parsing function for nonterminal "program"
function parse_program()
	return parse_stmt_list()
end

-- parse_stmt_list
-- Parsing function for nonterminal "stmt_list"
function parse_stmt_list()
	local good, newast
	local ast = {STMT_LIST}
	
	while isStatement() do
		good, newast = parse_statement()
		if not good then
			return false, nil
		end
		ast[#ast+1] = newast
	end
	
	return true, ast
end

-- parse_statement
-- Parsing function for nonterminal "statement"
-- Must only be called from parse_stmt_list. Will only
-- be called if valid statement is detected.
function parse_statement()
	if matchString("write") then
		return parse_write_arg()
	elseif matchString("def") then
		return handle_statement_def()
	elseif matchString("if") then
		return handle_statement_if()
	elseif matchString("while") then
		return handle_statement_while()
	elseif matchString("return") then
		return handle_statement_return()
	else
		return handle_statement_ID()
	end
end

-- parse_write_arg
-- Parsing function for nonterminal "write_arg"
-- This function must only be called from parse_statement
function parse_write_arg()
    local ast = {WRITE_STMT}
    local good, newast
	local currentLexeme
	
    if not matchString("(") then
        return false, nil
    end

    repeat
        currentLexeme = lexemeString

        if matchString("cr") then
            ast[#ast + 1] = {CR_OUT}
        elseif matchCat(lexit.STRLIT) then
            ast[#ast + 1] = {STRLIT_OUT, currentLexeme}
        else
            good, newast = parse_expr()

            if not good then
                return false, nil
            end

            ast[#ast + 1] = newast
        end
    until not matchString(",")

    if matchString(")") then
        return true, ast
    end
    return false, nil
end

-- handle_statement_def
-- Handles terminal def resolution for parent nonterminal
-- statement. (Function Definition)
-- This function must only be called from parse_statement.
function handle_statement_def()
	local good, ast
	local currentLexeme = lexemeString
	
	if matchCat(lexit.ID) and matchString("(") and matchString(")") then
		good, ast = parse_stmt_list()
		
		if good and matchString("end") then
			return true, {FUNC_DEF, currentLexeme, ast}
		end
	end
	
	return false, nil
end

-- handle_statement_if
-- Handles terminal if resolution for parent nonterminal
-- statement.
-- This function must only be called from parse_statement.
function handle_statement_if()
    local ast = {IF_STMT}
	local good, newast
	
	repeat
		good, newast = parse_expr()
		if not good then
			return false, nil
		end
		ast[#ast+1] = newast
		
		good, newast = parse_stmt_list()
		
		if not good then
			return false, nil
		end
		
		ast[#ast+1] = newast
	until not matchString("elseif")
	
    if matchString("else") then
        good, newast = parse_stmt_list()

        if not good then
            return false, nil
        end
        ast[#ast + 1] = newast
    end

    if not matchString("end") then
        return false, nil
    end

    return true, ast
end

-- handle_statement_while
-- Handles terminal while resolution for parent nonterminal
-- statement.
-- This function must only be called from parse_statement.
function handle_statement_while()
	local ast
	local good, newast = parse_expr()

	if not good then
		return false, nil
	end			
	
	ast = {WHILE_STMT, newast}

	good, newast = parse_stmt_list()

	if not good then
		return false, nil
	end
	
	ast[#ast+1] = newast
	
	if matchString("end") then
		return true, ast
	end
	
	return false, nil
end


-- handle_statement_return
-- Handles terminal return resolution for parent nonterminal
-- statement.
-- This function must only be called from parse_statement.
function handle_statement_return()
	local good, ast = parse_expr()
	
	if not good then
		return false, nil
	end
	
	return true, {RETURN_STMT, ast}
end

-- handle_statement_ID
-- Handles terminal ID resolution for parent nonterminal
-- statement. Resulting statement may be function call, array
-- variable, or simple variable.
-- This function must only be called from parse_statement.
function handle_statement_ID()
	local currentLexeme = lexemeString
	local good, newast
	local ast = {ASSN_STMT}
	
	if not matchCat(lexit.ID) then
		return false, nil
	end
	
	if matchString("(") and matchString(")") then
		return true, {FUNC_CALL, currentLexeme}
	elseif matchString("[") then
		good, newast = parse_expr()
		
		if not good or not matchString("]") then
			return false, nil
		end
		
		ast[2] = {ARRAY_VAR, currentLexeme, newast}
	else
		ast[2] = {SIMPLE_VAR, currentLexeme}
	end
	
	if matchString("=") then
		good, newast = parse_expr()
		if not good then
			return false, nil
		end
		
		ast[3] = newast
		return true, ast
	end
	
	return false, nil
end

-- parse_expr
-- Parsing function for nonterminal "expr"
function parse_expr()
	local newast, currentLexeme
    local good, ast = parse_comp_expr()

    if not good then
        return false, nil
    end

    currentLexeme = lexemeString
    while matchString("&&") or matchString("||") do
        good, newast = parse_comp_expr()
		
        if not good then
            return false, nil
        end
        ast = {{BIN_OP, currentLexeme}, ast, newast}
        currentLexeme = lexemeString
    end

    return true, ast
end

-- parse_comp_expr
-- Parsing function for nonterminal "comp_expr"
function parse_comp_expr()
    local good, ast, newast, currentLexeme
	
    if matchString("!") then
        good, ast = parse_comp_expr()

        if not good then
            return false, nil
		end
        return true, {{UN_OP, "!"}, ast}
	end

    good, ast = parse_arith_expr()

    if not good then
        return false, nil
    end

    currentLexeme = lexemeString
    while matchString("==") or matchString("!=")
			or matchString("<") or matchString("<=")
			or matchString(">") or matchString(">=") do

        good, newast = parse_arith_expr()

        if not good then
            return false, nil
        end
        ast = {{BIN_OP, currentLexeme}, ast, newast}
        currentLexeme = lexemeString
    end

    return true, ast
end

-- parse_arith_expr
-- Parsing function for nonterminal "arith_expr"
function parse_arith_expr()
	local good, ast, newast, currentLexeme
	good, ast = parse_term()

	if not good then
		return false, nil
	end
	
	currentLexeme = lexemeString
	while matchString("+") or matchString("-") do
		good, newast = parse_term()
		
		if not good then
			return false, nil
		end
		
		ast = {{BIN_OP, currentLexeme}, ast, newast}
		currentLexeme = lexemeString
	end
	
	return true, ast
end

-- parse_term
-- Parsing function for nonterminal "term"
function parse_term()
    local newast, currentLexeme
    local good, ast = parse_factor()

    if not good then
        return false, nil
    end

    currentLexeme = lexemeString
    while matchString("*") or matchString("/") or matchString("%") do
        good, newast = parse_factor()

        if not good then
            return false, nil
        end

        ast = {{BIN_OP, currentLexeme}, ast, newast}
        currentLexeme = lexemeString
    end

    return true, ast
end

-- parse_factor
-- Parsing function for nonterminal "factor"
function parse_factor()
	local good, ast
    local currentLexeme = lexemeString

    if matchString("(") then
        good, ast = parse_expr()

        if good and matchString(")") then
            return true, ast
        else
			return false, nil
		end
    elseif matchString("+") or matchString("-") then
        good, ast = parse_factor()
		
        if not good then
            return false, nil
        end
		
		return true, {{UN_OP, currentLexeme}, ast}
    elseif matchCat(lexit.NUMLIT) then
        return true, {NUMLIT_VAL, currentLexeme}
    elseif matchString("true") or matchString("false") then
        return true, {BOOLLIT_VAL, currentLexeme}
    elseif matchString("readnum") then
        if matchString("(") and matchString(")") then
            return true, {READNUM_CALL}
        end
    elseif matchCat(lexit.ID) then
        if matchString("(") and matchString(")") then
                return true, {FUNC_CALL, currentLexeme}
        elseif matchString("[") then
            good, ast = parse_expr()

            if not good or not matchString("]") then
                return false, nil
            end
			
            return true, {ARRAY_VAR, currentLexeme, ast}
		else
            return true,{SIMPLE_VAR, currentLexeme}
        end
    end

    return false, nil
end



-- *********************
-- *** Module Export ***
-- *********************

return parseit