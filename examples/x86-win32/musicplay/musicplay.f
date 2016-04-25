(
	musicplay.f
 	Plays a song using winamp plugins
	/Mic, 2004/2005
)


include ..\..\include\windef.f

notouch				
include gdi32.inc
includelib gdi32.lib
touch				


\ Create some null-terminated strings. The constants will contain the addresses of the strings.
z" Music player" 	constant #appName
z" MyClass" 		constant #className
z" ./out_wave.dll"	constant #output
z" ./in_littlesid.dll"	constant #input
z" Error"		constant #szError
z" Missing out_wave.dll" constant #szMissingOut
z" Missing in_littlesid.dll" constant #szMissingIn
z" winampGetOutModule"	constant #szOutMod
z" winampGetInModule2" 	constant #szInMod
z" arial"		constant #font-face
z" foobar"		constant #message
z" ./no_name_02.sid" 	constant #song-name
z" %02d:%02d"		constant #format-time

8 	constant IN_hMainWindow
12 	constant IN_hDllInstance
28 	constant IN_Config
32 	constant IN_About
36 	constant IN_Init
40 	constant IN_Quit
56 	constant IN_Play
72 	constant IN_Stop
80 	constant IN_GetOutputTime
88 	constant IN_SetVolume
148 	constant IN_outMod

12 	constant OUT_hMainWindow
16 	constant OUT_hDllInstance
28 	constant OUT_Init
32 	constant OUT_Quit



\ Allocate room for some variables
variable msg 28 allot
variable ps 64 allot
variable szBuffer 256 allot
variable hwnd
variable hdc
variable hfont
variable hOut
variable out-mod
variable hIn
variable in-mod
variable plugParent
variable seconds
variable minutes
variable n-chars


\ A function that does nothing
: void-func
	;

\ A function that returns zero
: return-zero
	0 ;@



: load-plugins
	\ Load output plugin
	
	#output call LoadLibrary a@ hOut !
	hOut @ 0 = if
		MB_ICONERROR #szError #szMissingOut 0 call MessageBox
		0 call ExitProcess
	then
	
	#szOutMod hOut @ call GetProcAddress
	\ Call the address returned by GetProcAddress, with 0 as the argument
	a@ 0 swap calli

	a@ out-mod !
	plugParent @ out-mod @ OUT_hMainWindow + !
	hOut @ out-mod @ OUT_hDllInstance + !
	out-mod @ OUT_Init + @ calli 


	\ Load input plugin
	
	#input call LoadLibrary a@ hIn !
	hIn @ 0 = if
		MB_ICONERROR #szError #szMissingIn 0 call MessageBox
		0 call ExitProcess
	then
	
	#szInMod hIn @ call GetProcAddress
	a@ 0 swap calli
	a@ in-mod !
	
	plugParent @ in-mod @ IN_hMainWindow + !
	hIn @ in-mod @ IN_hDllInstance + !	

	\ Fill the in-mod struct with some function pointers
	in-mod @ 96 +
	' void-func !r+
	' void-func !r+
	' void-func !r+
	' return-zero !r+
	' void-func !r+
	' void-func !r+
	' return-zero !r+
	' void-func !r+
	' void-func !r+
	' return-zero !r+
	' return-zero !r+
	4 +
	' void-func !r+
	drop
	
	out-mod @ in-mod @ IN_outMod + !
	in-mod @ IN_Init + @ calli

	40
	in-mod @ IN_SetVolume + @ calli drop ;
 
 
 
 : close-plugins
 	in-mod @ IN_Stop + @ calli
 	out-mod @ OUT_Quit + @ calli
 	in-mod @ IN_Quit + @ calli ;

 

: play-song
	#song-name
	in-mod @ IN_Play + @ calli
	a@ 0 <> if
		MB_ICONERROR #szError #song-name 0 call MessageBox
		0 call ExitProcess
	then	
	drop 
	
	30
	in-mod @ IN_SetVolume + @ calli drop
	
	NULL 1000 777 hwnd @ call SetTimer ;
	



\ Window event callback
: win-proc { hWnd uMsg wParam lParam -- val }
	uMsg case
		WM_CREATE of
			\ Get a device context
			hWnd call GetDC a@ hdc !
			
			\ Set the foreground and background text color
			0x0022f0ff hdc @ call SetTextColor
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
			FW_BOLD
			0
			0
			0
			24
			call CreateFont a@ hfont !

			\ Set the font as the current one for this DC
			hfont @ hdc @ call SelectObject        	            
			endof
		
		WM_TIMER of
			seconds @ 1 + seconds !
			seconds @ 60 = if
				0 seconds !
				minutes @ 1 + minutes !
			then
			seconds @ minutes @ #format-time szBuffer call wsprintf
			drop drop drop drop
			szBuffer call lstrlen a@ n-chars !
			0 NULL hWnd call InvalidateRect
			endof
			
		WM_PAINT of
			ps hWnd call BeginPaint
			n-chars @ szBuffer 40 64 hdc @ call TextOut
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
			777 hWnd call KillTimer
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
	128 200 CW_USEDEFAULT CW_USEDEFAULT
	WS_OVERLAPPEDWINDOW 
	#appName
	#className
	NULL
	call CreateWindowEx a@ hwnd !
	hwnd @ plugParent !
	
	SW_SHOWDEFAULT hwnd @ call ShowWindow
	hwnd @ call UpdateWindow

	load-plugins
	play-song
	
	begin
		0 0 hwnd @ msg call GetMessage a@
		0 = ?leave

		msg call TranslateMessage 
		msg call DispatchMessage 
	false until
	
	close-plugins ;


NULL call GetModuleHandle a@ win-main
bye 
	
		