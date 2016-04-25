(
	mmx.f
	Test some of the SIMD features of ForthEC
	/Mic, 2004
)

variable foo
variable bar
variable grill

\ Create two 4-element byte vectors
0x04030201 foo !
0x04040404 bar !

foo p@		\ Load a packed 32-bit value
pdup		\ Duplicate this value
pc2pw		\ Convert packed chars to packed words
bar p@		\ Load another packed value
pc2pw		\ Convert..
pw*		\ Do a packed multiply (16-bit)
pw2pc		\ Convert packed words to packed chars
pc+		\ Do a packed addition (8-bit)
grill p!	\ Store
emms		\ Empty SIMD stack


." [1 2 3 4] * 5 = [" grill dup dup dup c@ . 1 + c@ . 2 + c@ . 3 + c@ . ." ]"

cr newline cr newline ." Press any key.." key bye


