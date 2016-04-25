(
	fire.f
	Fire effect for ForthEC

	/Mic, 2004
)


include ..\..\include\windef.f
notouch
	include gdi32.inc
	includelib gdi32.lib
touch


\ Create two null-terminated strings. The constants will contain the addresses of the strings.
z" Forth on fire" constant #appName
z" MyClass" constant #className


\ Allocate 28 bytes for the msg struct (actually 4+28)
variable msg 28 allot
variable bih 36 allot
variable palette 1020 allot
variable hwnd
variable hdc
variable done
variable rng
variable rngh
variable video-buffer
variable rect 16 allot
variable last-time


\ Random number generator (borrowed from Paul Dixon)
: random ( -- n )
	rngh rng notouch
	pop esi
	pop edi
	mov edx,[edi]
	shr edx,1
	mov eax,[esi]
	mov ecx,eax
	rcr ecx,1
	rcl edx,1
	mov [edi],edx
	shl eax,12
	xor ecx,eax
	mov eax,ecx
	shr eax,20
	xor ecx,eax
	mov [esi],ecx
	and ecx,191
	mov eax,ecx
	touch a@ ;



\ This draws the actual fire
: flames
	12160 video-buffer @ +
	12798 video-buffer @ +

	160 0 do
		over over
		82 0 do
			dup dup dup dup
			c@ swap 2 + c@ + swap 4 + c@ + swap 644 + c@ + 2 >>
			dup 0 <> if 1- then
			dup 8 << or	
			dup 3 pick w! 2 pick 320 + w! swap 640 + swap 640 + loop
		over 640 - random 10 + swap c!
		drop drop swap 2 + swap 2 + loop drop drop ;
				
		
: draw-fire
	flames

	\ Set up the bitmapinfoheader	
	40 bih !
	320 bih 4 + !
	-200 bih 8 + !
	1 bih 12 + w!
	8 bih 14 + w!
	BI_RGB bih 16 + !

	\ Limit to about 58 fps by suspending the process until at least 17 ms have
	\ passed since the last frame was drawn.
	17 call GetTickCount a@ last-time @ - - dup 0 > if
		call Sleep
	else
		drop
	then
	
	hwnd @ call GetDC a@ hdc !
	rect hwnd @ call GetClientRect
	
	SRCCOPY
	DIB_RGB_COLORS
	bih
	video-buffer @
	200 320 0 0
	rect 12 + @ rect 8 + @ 0 0
	hdc @
	call StretchDIBits 
	
	hdc @ hwnd @ call ReleaseDC
	call GetTickCount a@ last-time ! ;
	
	

: setup-palette
	palette 1024 -1 fill
	palette 
	33 1 do
		i 3 << 1-	\ pal c
		over 0 swap !
		over 2 + over swap c!
		over 128 + 0 swap c!
		over 129 + over swap c!
		over 256 + c!
		4 + loop drop ;
	

	

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

		

: create-window { hInst win-width win-height -- } 
	here						\ Push dictionary pointer

	\ The , (comma) stores one item (4 bytes) in the dictionary
	CS_HREDRAW CS_VREDRAW or ,			\ style 
	' win-proc ,					\ lpfnWndProc
	0 , 0 ,
	hInst ,						\ hInstance
	IDI_APPLICATION NULL call LoadIcon a@ ,		\ hIcon
	IDC_ARROW NULL call LoadCursor a@ ,		\ hCursor
	COLOR_WINDOW 1+ ,				\ hbrBackground
	0 ,
	#className ,					\ lpszClassName
	call RegisterClass
	
	NULL
	hInst
	NULL
	NULL
	win-height win-width CW_USEDEFAULT CW_USEDEFAULT
	WS_OVERLAPPED_NOMAX 
	#appName
	#className
	NULL
	call CreateWindowEx a@ hwnd !
	
	SW_SHOWDEFAULT hwnd @ call ShowWindow
	hwnd @ call UpdateWindow ;
	


: win-main { hInst -- }
	228 360 hInst create-window
	setup-palette

	\ Allocate room for a 320x200 bitmap, plus a little more
	65000 LMEM_ZEROINIT call LocalAlloc a@ video-buffer !
	video-buffer @ 64000 0 fill
	
	19 rng !
	1 rngh !

	call GetTickCount a@ last-time !
	
	false done !
	
	\ Keep looping until the window is closed
	begin begin
		true while
			PM_NOREMOVE 0 0 hwnd @ msg call PeekMessage a@ 0= ?leave
			0 0 hwnd @ msg call GetMessage a@ 0= if
				true done !
				leave
			then
			msg call TranslateMessage
			msg call DispatchMessage
			repeat
		draw-fire
		done @ until
	
	video-buffer @ call LocalFree
  	hwnd @ call DestroyWindow ;
  	



NULL call GetModuleHandle a@ win-main
bye 
