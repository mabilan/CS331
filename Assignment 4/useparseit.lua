#!/usr/bin/env lua
-- useparseit.lua
-- Glenn G. Chappell
-- 22 Feb 2019
--
-- For CS F331 / CSCE A331 Spring 2019
-- Simple Main Program for parseit Module
-- Requires parseit.lua

parseit = require "parseit"


-- String forms of symbolic constants
-- Used by writeAST_parseit
symbolNames = {
  [1]="STMT_LIST",
  [2]="WRITE_STMT",
  [3]="FUNC_DEF",
  [4]="FUNC_CALL",
  [5]="IF_STMT",
  [6]="WHILE_STMT",
  [7]="RETURN_STMT",
  [8]="ASSN_STMT",
  [9]="CR_OUT",
  [10]="STRLIT_OUT",
  [11]="BIN_OP",
  [12]="UN_OP",
  [13]="NUMLIT_VAL",
  [14]="BOOLLIT_VAL",
  [15]="READNUM_CALL",
  [16]="SIMPLE_VAR",
  [17]="ARRAY_VAR",
}


-- writeAST_parseit
-- Write an AST, in (roughly) Lua form, with numbers replaced by the
-- symbolic constants used in parseit.
-- A table is assumed to represent an array.
-- See the Assignment 4 description for the AST Specification.
function writeAST_parseit(x)
    if type(x) == "number" then
        local name = symbolNames[x]
        if name == nil then
            io.write("<ERROR: Unknown constant: "..x..">")
        else
            io.write(name)
        end
    elseif type(x) == "string" then
        io.write('"'..x..'"')
    elseif type(x) == "boolean" then
        if x then
            io.write("true")
        else
            io.write("false")
        end
    elseif type(x) == "table" then
        local first = true
        io.write("{")
        for k = 1, #x do  -- ipairs is problematic
            if not first then
                io.write(", ")
            end
            writeAST_parseit(x[k])
            first = false
        end
        io.write("}")
    elseif type(x) == "nil" then
        io.write("nil")
    else
        io.write("<ERROR: "..type(x)..">")
    end
end


-- check
-- Given a "program", check its syntactic correctness using parseit.
-- Print results.
function check(program)
    dashstr = "-"
    io.write(dashstr:rep(72).."\n")
    io.write("Program: "..program.."\n")

    local good, done, ast = parseit.parse(program)

    if good and done then
        io.write("AST: ")
        writeAST_parseit(ast)
        io.write("\n")
    elseif good and not done then
        io.write("Bad - extra characters at end\n")
    elseif not good and done then
        io.write("Unfinished - please add more\n")
    else  -- not good and not done
        io.write("Bad - syntax error\n")
    end
end


-- Main program
-- Check several "programs".
io.write("Recursive-Descent Parser: Jerboa\n")
check("")
check("write(cr)")
check("write(cr) write(cr) write(cr)")
check("write(cr, cr, cr)")
check("write('abc')")
check("a=3")
check("a=a+1")
check("a=readnum()")
check("write(a+1)")
check("def f()write('yo')end f()")
check("a=3write(a+b,cr)")
check("a[e*2+1]=2")
check("write(cr)elseif")
io.write("### Above should be ")
io.write("\"Bad - extra characters at end\"\n")
check("def foo() write(cr")
io.write("### Above should be ")
io.write("\"Unfinished - please add more\"\n")
check("+abc()")
io.write("### Above should be ")
io.write("\"Bad - syntax error\"\n")

