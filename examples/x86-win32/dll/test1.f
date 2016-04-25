(
	test1.f
 	Loads a DLL
	/Mic, 2004
)

variable lib
variable fptr

\ Load the dll
z" test.dll" call LoadLibrary a@ lib !  lib @ 0= if bye then

\ Get the address of say_it
z" say_it" lib @ call GetProcAddress a@ fptr !  

\ Call it indirectly
z" Hello from test.dll !" fptr @ calli

\ Close the dll and exit the program
lib @ call FreeLibrary
bye

