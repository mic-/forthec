(
	test2.f
 	Example of a DLL written in Forth
	/Mic, 2004
)


include ..\..\include\windef.f

: LibMain { hInstDLL reason reserved }
	reason DLL_PROCESS_ATTACH = if
		1
	else
		0
	then
	\ Pop the arguments of the stack, leaving the return-value
	swap drop swap drop swap drop ;@


: say_it { message }
	MB_OK z" Message" message NULL call MessageBox drop ;

	