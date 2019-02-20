lexit = require "lexit"

program = "x = 3 # Set a variable\n write(x+4, cr)\n"

for lexstr, cat in lexit.lex(program) do
    print(lexstr, lexit.catnames[cat])
end