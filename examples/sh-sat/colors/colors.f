\ Simple back screen demo for SEGA Saturn

include vdp.f

variable scrollY


: rgb { blue green red }
	blue 10 << green 5 << or red or swap drop swap drop swap drop ;
	

0x8110 	#TVMD w!	\ Display on, 320x240 pixels
0 	#EXTEN w!
0 	#BGON w!	\ All BGs off
0 	#LCTAL w!	\ Line color table
0 	#LCTAU w!
0 	#BKTAL w!	\ Back screen table
0x8000 	#BKTAU w!	\ One color per line


\ Set up line color table
512 0 do
	i 2 >> 0x1F and i 2/ 0x1F and i 0x1F and rgb #VDP2_VRAM i 2* + w! loop

	
0 scrollY !
begin
	\ Wait for vblank
	wait-vblank-in
	
	\ Set starting line of back screen
	scrollY @ #BKTAL w!
	scrollY @ 2 - 0xFF and scrollY !

	\ Wait for end of vblank	
	wait-vblank-out
	
	repeat	\ Loop forever

