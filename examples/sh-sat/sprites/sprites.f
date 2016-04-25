\ SEGA Saturn sprite demo

include vdp.f

extern constant arrow
extern constant smfont


variable mesh-flag
variable list-index
variable xpos
variable ypos
variable xspeed
variable yspeed

\ Command table elements 
0 	constant #CMDCTRL 
1 	constant #CMDLINK 
2 	constant #CMDPMOD 
3 	constant #CMDCOLR 
4 	constant #CMDSRCA 
5 	constant #CMDSIZE 
6 	constant #CMDXA   
7 	constant #CMDYA   
8 	constant #CMDXB   
9 	constant #CMDYB   
10 	constant #CMDXC   
11 	constant #CMDYC   
12 	constant #CMDXD   
13 	constant #CMDYD   
14 	constant #CMDGRDA 
15 	constant #CMDDUMY 



: RGB ( r g b -- rgb )
	10 << swap 5 << or or 0x8000 or ;

: MESH ( n -- )
	0<> if 0x100 else 0 then mesh-flag ! ;
	

\ Clear command list
: list-clear ( -- )
	16 0 do
		0xFFFF #VDP1_VRAM i wa+ w! loop
	0x4000 #VDP1_VRAM #CMDCTRL wa+ w!
	1 list-index ! ;
	

\ Add end marker to command list
: list-end ( -- )
	0x8000 #VDP1_VRAM list-index @ 5 << + #CMDCTRL wa+ w!
	list-index @ 1+ list-index ! ;
	

\ Add system clip to command list
: list-set-system-clip { y x }
	#VDP1_VRAM list-index @ 5 << +
	0x0009 over #CMDCTRL wa+ w!	\ Set system clip
	0x0000 over #CMDLINK wa+ w!	\ Jump address
	x over #CMDXC wa+ w!		\ x
	y over #CMDYC wa+ w!		\ y
	2drop drop
	list-index @ 1+ list-index ! ;
	

\ Add user clip to command list	
: list-set-user-clip { y2 x2 y1 x1 }
	#VDP1_VRAM list-index @ 5 << +
	0x0008 over #CMDCTRL 2* + w!	\ Set user clip
	0x0000 over #CMDLINK 2* + w!	\ Jump address
	x1 over #CMDXA 2* + w!
	y1 over #CMDYA 2* + w!
	x2 over #CMDXC 2* + w!
	y2 over #CMDYC 2* + w!
	2drop 2drop drop
	list-index @ 1+ list-index ! ;
	

\ Add local coords to command list 
: list-set-local-coords { y x }
	#VDP1_VRAM list-index @ 5 << +
	0x000A over #CMDCTRL 2* + w!	\ Set local coords
	0x0000 over #CMDLINK 2* + w!	\ Jump address
	x over #CMDXA 2* + w!
	y over #CMDYA 2* + w!
	2drop drop
	list-index @ 1+ list-index ! ;
	

: list-normal-sprite { y1 x1 height width vflip hflip src }
	#VDP1_VRAM list-index @ 5 << +
	0x0000 hflip 4 << vflip 5 << or or over #CMDCTRL wa+ w!	\ Normal sprite
	0x0000 over #CMDLINK wa+ w!				\ Jump address
	0x00A8 mesh-flag @ or over #CMDPMOD wa+ w!
	0 over #CMDCOLR wa+ w!
	src 3 >> over #CMDSRCA wa+ w!
	width 3 >> 0x3f and 8 << height 0xff and or over #CMDSIZE wa+ w!
	x1 over #CMDXA wa+ w!
	y1 over #CMDYA wa+ w!
	2drop 2drop 2drop 2drop
	list-index @ 1+ list-index ! ;



\ Copy font data to VDP1 VRAM
: copy-font ( -- )
	0x25C40000
	6 0 do
		16 0 do
			smfont j 8 << i + 4 << wa+ swap			
			16 0 do
				16 0 do
					over j 8 << i + wa+ w@
					over w! 2 + loop 
				loop swap drop
			loop
		loop drop ;


\ Draw a string at x,y using sprites
: draw-string { y x msg }
	msg
	32 0 do
		dup c@ dup 0= ?leave
		32 - 9 << 0x40000 + 0 0 16 16 x i 4 << + y list-normal-sprite
		1+ loop
	2drop 2drop drop ;


\ Update the position of the arrow sprite
: move-arrow ( -- )
        xpos @ xspeed @ +
        dup -10 = if xspeed @ negate xspeed ! then
        dup 266 = if xspeed @ negate xspeed ! then xpos !

        ypos @ yspeed @ +
        dup -10 = if yspeed @ negate yspeed ! then
        dup 128 = if yspeed @ negate yspeed ! then ypos ! ;
        


\ ------------------
\     Main code
\ ------------------

1 list-index !
0 mesh-flag !

vdp-init


0x8110 	#TVMD w!	\ Display on, 320x240 pixels
0 	#EXTEN w!
0 	#BGON w!	\ All BGs off
0 	#LCTAL w!	\ Line color table
0 	#LCTAU w!
0 	#BKTAL w!	\ Back screen table
0x8000 	#BKTAU w!	\ One color per line


\ Set up line color table with a red gradient
256 0 do
	i 3 >> dup 31 > if drop 31 then #VDP2_VRAM i wa+ w! loop


\ Set up VDP1 registers
0x0000 #TVMR w!
0x0000 #FBCR w!
0x0002 #PTMR w!
0x8000 #EWDR w!
0x0000 #EWLR w!
0x50DF #EWRR w!

copy-font

\ Copy arrow sprite to VRAM
0x25C4C000
4096 0 do
	arrow i wa+ w@ over w! 2 + loop drop
	
	
\ Display on
0x8000 #TVMD w!

\ Init command list
list-clear list-end

240 xpos !
100 ypos !
2 xspeed !
-2 yspeed !

begin 
	wait-vblank-in
	wait-vblank-out
	
	0x8110 #TVMD w!
	0 #TVMR w!
	1 #FBCR w!

	\ Make a new list 
        list-clear
        320 224 list-set-system-clip
        0 0 320 224 list-set-user-clip
        0 0 list-set-local-coords
	0 MESH
	
	z" Saturn sprites" 48 182 draw-string
	
	0x4C000 xspeed @ 0 < if 1 else 0 then yspeed @ 0 < if 0 else 1 then
	64 64 xpos @ ypos @ list-normal-sprite
	
        list-end
        
	move-arrow
	
	repeat
	
    	vdp-shutdown
	bye




