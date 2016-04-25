(
	window.f
 	Creates a window and wait for the user to press Esc
	/Mic, 2004
)


include ..\..\include\windef.f


\ Create two null-terminated strings. The constants will contain the addresses of the strings.
z" Window example" constant #appName
z" MyClass" constant #className


\ Allocate 28 bytes for the msg struct (actually 4+28)
variable msg 28 allot
variable hwnd



\ Window event callback
: win-proc { hWnd uMsg wParam lParam -- val }
	uMsg case
		WM_KEYDOWN of
			wParam VK_ESCAPE =
			if
				NULL NULL WM_CLOSE hWnd call PostMessage
			then
			endof
		
		WM_CLOSE of
			0 call PostQuitMessage
			endof

		WM_DESTROY of
			0 call PostQuitMessage
			endof
		
		default
			\ Call the default window proc. The parameters are already
			\ on the stack, so there's no need to push them.
			\
			\ a@ is used to push the accumulator (eax) onto the stack, which is
			\ necessary if you want to get the returned value from a windows api
			\ function.
			call DefWindowProc a@ exit
		
		endcase
		
		\ Pop all the arguments of the stack.
		\ The ;@ word pops a value of the stack and places it in register eax before returning
		drop drop drop drop 0 ;@



: win-main { hInst -- } 
	here						\ Push dictionary pointer

	\ The , (comma) stores one item (4 bytes) in the dictionary
	CS_HREDRAW CS_VREDRAW or ,			\ style 
	' win-proc ,					\ lpfnWndProc
	0 , 0 ,
	hInst ,						\ hInstance
	IDI_APPLICATION NULL call LoadIcon a@ ,		\ hIcon
	IDC_ARROW NULL call LoadCursor a@ ,		\ hCursor
	COLOR_WINDOWFRAME ,				\ hbrBackground
	0 ,
	#className ,					\ lpszClassName
	call RegisterClass
	
	NULL
	hInst
	NULL
	NULL
	128 300 CW_USEDEFAULT CW_USEDEFAULT
	WS_OVERLAPPEDWINDOW 
	#appName
	#className
	NULL
	call CreateWindowEx a@ hwnd !
	
	SW_SHOWDEFAULT hwnd @ call ShowWindow
	hwnd @ call UpdateWindow

	
	begin
		0 0 hwnd @ msg call GetMessage a@
		0 = ?leave

		msg call TranslateMessage 
		msg call DispatchMessage 
	false until ;


NULL call GetModuleHandle a@ win-main
bye 
	
		