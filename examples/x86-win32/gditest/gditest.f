(
	gditest.f
 	Creates a window and uses GDI to draw some text to the window
	/Mic, 2004
)


include ..\..\include\windef.f

notouch				
include gdi32.inc
includelib gdi32.lib
touch				


\ Create some null-terminated strings. The constants will contain the addresses of the strings.
z" GDI test" 	constant #appName
z" MyClass" 	constant #className
z" georgia" 	constant #font-face
z" The quick brown fox jumps over the lazy dog" constant #message


\ Allocate room for some variables
variable msg 28 allot
variable ps 64 allot
variable hwnd
variable hdc
variable hfont


\ Window event callback
: win-proc { hWnd uMsg wParam lParam -- val }
	uMsg case
		WM_CREATE of
			\ Get a device context
			hWnd call GetDC a@ hdc !
			
			\ Set the foreground and background text color
			0x00ffaab0 hdc @ call SetTextColor
			0 hdc @ call SetBkColor
			
			\ Create a font to use
			#font-face
			FF_DONTCARE
			ANTIALIASED_QUALITY
			CLIP_DEFAULT_PRECIS
			OUT_OUTLINE_PRECIS
			ANSI_CHARSET
			0
			0
			1
			FW_MEDIUM
			0
			0
			0
			15
			call CreateFont a@ hfont !

			\ Set the font as the current one for this DC
			hfont @ hdc @ call SelectObject        	            
			endof
			
		WM_PAINT of
			ps hWnd call BeginPaint
			43 #message 40 20 hdc @ call TextOut
			ps hWnd call EndPaint
			endof
			
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
			hfont @ call DeleteObject
			hdc @ hWnd call ReleaseDC
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
	COLOR_MENUTEXT ,				\ hbrBackground
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
	
		