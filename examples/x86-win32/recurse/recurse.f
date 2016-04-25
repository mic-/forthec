(
	recurse.f
 	Simple example of a recursive function
	/Mic, 2004
)


defer n!		\ Define word for forward reference


\ Calculate the factorial of n
: n! ( n1 -- n2 )
	dup 1 > if
		dup 1- n! *
	then ;


." 9! = " 9 n! .

cr newline ." Press any key.." key bye
