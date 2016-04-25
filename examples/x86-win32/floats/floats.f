\	floats.f
\ 	Prints floor(sin(x)), where x goes from 0 to 2*PI in steps of PI/2
\	/Mic, 2004


3.14159e0 fconstant PI


." sin 0..2PI = "
0.5e0 5 0 do fdup i float PI f* f* fsin floor . loop

cr newline ." Press any key.." key bye

