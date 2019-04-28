\ collcount.fs
\ Michael Bilan
\ 28 Apr 2019
\ CS 331 - Assignment 7

\ collcount
\ Given positive integer n, returns c, the number of iterations of the Collatz function
\ required to take n to 1.
: collcount ( n -- c )		
	0				\ Stack: n c -- c starts at 0
	swap			\ Stack: c n
	begin			\ Start loop
		dup 1 = if	\ Duplicate n, check if n = 1 -- if n=1 do nothing
		else		\ -- if n != 1 do loop
			swap	\ Stack: n c
			1 +		\ Stack: n c+1
			swap	\ Stack: c+1 n
			collatz		\ Stack: c+1 C[n]
		then
		dup 1 =		\ Loop until C[n] is 1
	until
	drop			\ Stack: c
;

\ collatz
\ Given an integer n, return the Collatz function value.
\ c(n) = 3n + 1    if n is odd
\      = n/2      if n is even
: collatz ( n -- C[n] )
	dup 2 mod 0 = if	\ Duplicate stack item, then if even
		2 /				
	else				\ Else if stack item was odd
		3 * 1 + 		
	then				\ Stack: ... C[n]
;