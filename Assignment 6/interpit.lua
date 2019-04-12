-- interpit.lua
-- Michael Bilan (Completed)
-- Glenn G. Chappell (Template)
-- 11 Apr 2019
--
-- For CS F331 / CSCE A331 Spring 2019
-- Interpret AST from parseit.parse
-- For Assignment 6, Exercise B


-- *******************************************************************
-- * To run a Jerboa program, use jerboa.lua (which uses this file). *
-- *******************************************************************


local interpit = {}  -- Our module


-- ***** Variables *****


-- Symbolic Constants for AST

local STMT_LIST    = 1
local WRITE_STMT   = 2
local FUNC_DEF     = 3
local FUNC_CALL    = 4
local IF_STMT      = 5
local WHILE_STMT   = 6
local RETURN_STMT  = 7
local ASSN_STMT    = 8
local CR_OUT       = 9
local STRLIT_OUT   = 10
local BIN_OP       = 11
local UN_OP        = 12
local NUMLIT_VAL   = 13
local BOOLLIT_VAL  = 14
local READNUM_CALL = 15
local SIMPLE_VAR   = 16
local ARRAY_VAR    = 17



-- ***** Utility Functions *****


-- numToInt
-- Given a number, return the number rounded toward zero.
local function numToInt(n)
    assert(type(n) == "number")

    if n >= 0 then
        return math.floor(n)
    else
        return math.ceil(n)
    end
end


-- strToNum
-- Given a string, attempt to interpret it as an integer. If this
-- succeeds, return the integer. Otherwise, return 0.
local function strToNum(s)
    assert(type(s) == "string")

    -- Try to do string -> number conversion; make protected call
    -- (pcall), so we can handle errors.
    local success, value = pcall(function() return 0+s end)

    -- Return integer value, or 0 on error.
    if success then
        return numToInt(value)
    else
        return 0
    end
end


-- numToStr
-- Given a number, return its string form.
local function numToStr(n)
    assert(type(n) == "number")

    return ""..n
end


-- boolToInt
-- Given a boolean, return 1 if it is true, 0 if it is false.
local function boolToInt(b)
    assert(type(b) == "boolean")

    if b then
        return 1
    else
        return 0
    end
end


-- astToStr
-- Given an AST, produce a string holding the AST in (roughly) Lua form,
-- with numbers replaced by names of symbolic constants used in parseit.
-- A table is assumed to represent an array.
-- See the Assignment 4 description for the AST Specification.
--
-- THIS FUNCTION IS INTENDED FOR USE IN DEBUGGING ONLY!
-- IT SHOULD NOT BE CALLED IN THE FINAL VERSION OF THE CODE.
function astToStr(x)
    local symbolNames = {
        "STMT_LIST", "WRITE_STMT", "FUNC_DEF", "FUNC_CALL", "IF_STMT",
        "WHILE_STMT", "RETURN_STMT", "ASSN_STMT", "CR_OUT",
        "STRLIT_OUT", "BIN_OP", "UN_OP", "NUMLIT_VAL", "BOOLLIT_VAL",
        "READNUM_CALL", "SIMPLE_VAR", "ARRAY_VAR"
    }
    if type(x) == "number" then
        local name = symbolNames[x]
        if name == nil then
            return "<Unknown numerical constant: "..x..">"
        else
            return name
        end
    elseif type(x) == "string" then
        return '"'..x..'"'
    elseif type(x) == "boolean" then
        if x then
            return "true"
        else
            return "false"
        end
    elseif type(x) == "table" then
        local first = true
        local expressionValue = "{"
        for k = 1, #x do
            if not first then
                expressionValue = expressionValue .. ","
            end
            expressionValue = expressionValue .. astToStr(x[k])
            first = false
        end
        expressionValue = expressionValue .. "}"
        return expressionValue
    elseif type(x) == "nil" then
        return "nil"
    else
        return "<"..type(x)..">"
    end
end


-- ***** Primary Function for Client Code *****


-- interp
-- Interpreter, given AST returned by parseit.parse.
-- Parameters:
--   ast     - AST constructed by parseit.parse
--   state   - Table holding Jerboa variables & functions
--             - AST for function xyz is in state.f["xyz"]
--             - Value of simple variable xyz is in state.v["xyz"]
--             - Value of array item xyz[42] is in state.a["xyz"][42]
--   incall  - Function to call for line input
--             - incall() inputs line, returns string with no newline
--   outcall - Function to call for string output
--             - outcall(str) outputs str with no added newline
--             - To print a newline, do outcall("\n")
-- Return Value:
--   state, updated with changed variable values
function interpit.interp(ast, state, incall, outcall)
    -- Each local interpretation function is given the AST for the
    -- portion of the code it is interpreting. The function-wide
    -- versions of state, incall, and outcall may be used. The
    -- function-wide version of state may be modified as appropriate.


    -- Forward declare local functions
    local interp_stmt_list
    local interp_stmt
    local eval_expr

    -- interp_stmt_list
    -- Handles STMT_LIST rooted asts
    function interp_stmt_list(ast)
        assert(ast[1] == STMT_LIST, "stmt list AST must start w/ STMT_LIST")

        for i = 2, #ast do
            interp_stmt(ast[i])
        end
    end

    -- interp_stmt
    -- Handles individual statement rooted asts
    function interp_stmt(ast)
        if (ast[1] == WRITE_STMT) then
            for i = 2,#ast do
                if (ast[i][1] == CR_OUT) then
                    outcall("\n")
                elseif (ast[i][1] == STRLIT_OUT) then
                    local str = ast[i][2]
                    outcall(str:sub(2, str:len()-1))
                else
                    local value = eval_expr(ast[i])
                    if (value == nil) then
                        value = 0
                    end
                    outcall(numToStr(value))
                end
            end
        elseif (ast[1] == FUNC_DEF) then
            state.f[ast[2]] = ast[3]
        elseif (ast[1] == IF_STMT) then
            local endif = false
            for i = 2, #ast-1, 2 do
                if (eval_expr(ast[i]) ~= 0) then
                    expressionValue = interp_stmt_list(ast[i+1])
                    endif = true
                    break
                end
            end
            if (not endif and (#ast % 2 == 0)) then
                interp_stmt_list(ast[#ast])
            end
        elseif (ast[1] == WHILE_STMT) then
            while (eval_expr(ast[2]) ~= 0) do
                interp_stmt_list(ast[3])
            end
        elseif (ast[1] == RETURN_STMT) then
            state.v["return"] = eval_expr(ast[2])
        elseif (ast[1] == FUNC_CALL) then
            local statements = state.f[ast[2]]

            if (statements == nil) then
                statements = {STMT_LIST}
            end

            interp_stmt_list(statements)
        elseif (ast[1] == ASSN_STMT) then
            local arrayName = ast[2][2]
            local value = eval_expr(ast[3])

            if (ast[2][1] == SIMPLE_VAR) then
                state.v[arrayName] = value

            elseif(ast[2][1] == ARRAY_VAR) then
                local arrayIndex = eval_expr(ast[2][3])
                if (state.a[arrayName] == nil) then -- If array doesn't exist, make a new one
                    state.a[arrayName] = {}
                end
                state.a[arrayName][arrayIndex] = value
            end

        else
            assert(false, "Illegal statement: " .. astToStr(ast))
        end
    end

    -- eval_expr
    -- Handles expression evaluation
    function eval_expr(ast)
        local expressionValue

        if (ast[1] == NUMLIT_VAL) then
            expressionValue = strToNum(ast[2])
			return expressionValue
        elseif (ast[1] == SIMPLE_VAR) then
            expressionValue = state.v[ast[2]]
            if (expressionValue == nil) then
                expressionValue = 0
            end
			return expressionValue
        elseif (ast[1] == ARRAY_VAR) then
            local index = eval_expr(ast[3])
            if (state.a[ast[2]] ~= nil) then
                expressionValue = state.a[ast[2]][index]
				if (expressionValue == nil) then
					expressionValue = 0
				end
            else
                expressionValue = 0
            end
			return expressionValue
        elseif (ast[1] == FUNC_CALL) then
            local statements = state.f[ast[2]]
            if (statements == nil) then
                statements = {STMT_LIST}
            end
            interp_stmt_list(statements)
            expressionValue = state.v["return"]
        elseif (ast[1] == BOOLLIT_VAL) then
            if (ast[2] == "true") then
                expressionValue = boolToInt(true)
				return expressionValue
            elseif (ast[2] == "false") then
                expressionValue = boolToInt(false)
				return expressionValue
            else
                assert(false, "Unresolved use of BOOLLIT_VAL: "  .. astToStr(ast))
            end
        elseif (ast[1] == READNUM_CALL) then
            expressionValue = strToNum(incall())
			return expressionValue

		elseif (ast[1][1] == BIN_OP) then
			local lhs = eval_expr(ast[2])
			local rhs = eval_expr(ast[3])
			local operator = ast[1][2]

			if (operator == "+") then
				expressionValue = numToInt(lhs + rhs)
				return expressionValue
			elseif (operator == "-") then
				expressionValue = numToInt(lhs - rhs)
				return expressionValue
			elseif (operator == "*") then
				expressionValue = numToInt(lhs * rhs)
				return expressionValue
			elseif (operator == "/") then
				if rhs == 0 then
					expressionValue = 0
					return expressionValue
				else
					expressionValue = numToInt(lhs / rhs)
					return expressionValue
				end
			elseif (operator == "%") then
				if rhs == 0 then
					expressionValue = 0
					return expressionValue
				else
					expressionValue = numToInt(lhs % rhs)
					return expressionValue
				end
			elseif (operator == "&&") then
				expressionValue = boolToInt(lhs ~= 0 and rhs ~= 0)
				return expressionValue
			elseif (operator == "||") then
				expressionValue = boolToInt(lhs ~= 0 or rhs ~= 0)
				return expressionValue
			elseif (operator == "==") then
				expressionValue = boolToInt(lhs == rhs)
				return expressionValue
			elseif (operator == "!=") then
				expressionValue = boolToInt(lhs ~= rhs)
				return expressionValue
			elseif (operator == "==") then
				expressionValue = boolToInt(lhs == rhs)
				return expressionValue
			elseif (operator == "<") then
				expressionValue = boolToInt(lhs < rhs)
				return expressionValue
			elseif (operator == "<=") then
				expressionValue = boolToInt(lhs <= rhs)
				return expressionValue
			elseif (operator == ">") then
				expressionValue = boolToInt(lhs > rhs)
				return expressionValue
			elseif (operator == ">=") then
				expressionValue = boolToInt(lhs >= rhs)
				return expressionValue
			else
				assert(false, "Unresolved use of BIN_OP: "  .. astToStr(ast))
			end

		elseif (ast[1][1] == UN_OP) then
			local operator = ast[1][2]

			if (operator == "!") then
				if (eval_expr(ast[2]) ~= 0) then
					expressionValue = 0
					return expressionValue
				else
					expressionValue = 1
					return expressionValue
				end
			elseif (operator == "-") then
				expressionValue = - eval_expr(ast[2])
				return expressionValue
			elseif (operator == "+") then
				expressionValue = eval_expr(ast[2])
				return expressionValue
			else
				assert(false, "Unresolved use of UN_OP: " .. astToStr(ast))
			end
		else
			assert(false, "Unresolved use of EXPR: " .. astToStr(ast))
		end
        return expressionValue
    end

    -- ***** Body of Function interp *****
    interp_stmt_list(ast)
    return state
end


-- ***** Module Export *****

return interpit

