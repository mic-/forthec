(
	winhello.f
	Opens a messagebox
	/Mic, 2004
)

include ..\..\include\windef.f

MB_OK			\ Push parameters in reverse order
z" Message"		\ ...
z" Hello world!"	\ ...
NULL			\ ...
call MessageBox		\ Call external function
bye			\ Close application


