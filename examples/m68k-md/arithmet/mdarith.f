\ Arithmetic test for SEGA Genesis

include genesis.f

extern constant font		\ This label exists in font.o
extern constant htimer		\ This label exists in md_crt0.o
extern constant vtimer		\ This label exists in md_crt0.o


variable strbuf 8 allot
variable hstart


\ Wait for the next vertical blank
: wait_vsync
	vtimer @
	begin true while
		dup vtimer @ <> ?leave repeat drop ;
		
		
\ Setup VDP registers
: init_vdp
	#VDP0_E_HBI #VDP0_E_DISPLAY #VDP0_PLTT_FULL or or 0 write_vdp_reg
	#VDP1_E_VBI #VDP1_E_DISPLAY #VDP1_PAL #VDP1_RESERVED or or or 1 write_vdp_reg
	0xE000 10 >> 2 write_vdp_reg	\ Map A address 
	0xE000 10 >> 3 write_vdp_reg	\ Window address
	0xE000 13 >> 4 write_vdp_reg	\ Map B address
	0xFC00 9 >> 5 write_vdp_reg	\ Sprite list address
	0 6 write_vdp_reg		
	0 7 write_vdp_reg		\ Border color
	1 8 write_vdp_reg
	1 9 write_vdp_reg
	0 10 write_vdp_reg		\ Lines per HBI
	4 11 write_vdp_reg		\ 2-cell vertical scrolling
	#VDP12_SCREEN_V224 #VDP12_SCREEN_H256 #VDP12_PROGRESSIVE or or 12 write_vdp_reg
	0x6000 10 >> 13 write_vdp_reg
	2 15 write_vdp_reg
	#VDP16_MAP_V32 #VDP16_MAP_H32 or 16 write_vdp_reg
	0 17 write_vdp_reg
	0xFF 18 write_vdp_reg ;


\ Load font tiles into VRAM
: load_charset
	0x0000 set_vram_addr
	font
	1536 0 do	\ 96 tiles = 1536 words
		dup w@ #VDPDATA w! 2 + loop drop ;


\ Set the first two colors (used by the font)
: set_colors
	0x0000 set_cram_addr
	0x0000 #VDPDATA w!	\ Black
	0x0EEE #VDPDATA w! ;	\ White


\ Write a string to the screen
: puts ( strptr x y -- )
	0xE000 swap 6 << + swap 2* + set_vram_addr	
	begin true while
		dup c@ dup 0= ?leave 32 - 0x8000 or #VDPDATA w!
		1+ repeat drop drop ;


\ Print a signed integer on the screen in decimal format
: print ( val x y -- )
	rot						\ Put value on top
	dup 0 < if 1 swap negate else 0 swap then 	\ Check sign			
	strbuf 7 + 0 over c! 1-						

	begin true while
		 over 10 mod 48 + over c!		\ Put one char in the buffer
		 swap 10 / dup 0= ?leave		\ Last digit?
		 swap 1- repeat
	 drop swap 0<> if 1- '-' over c! then		\ Put a leading '-' for negative numbers 
	 rot rot puts ;					

	 

init_vdp
wait_vsync wait_vsync wait_vsync
load_charset set_colors
wait_vsync

htimer @ hstart !	\ Get initial value

z" ForthEC M68k arithmetic test" 2 0 puts
z" ----------------------------" 2 1 puts

z" -5 ABS:"     2 3 puts   -5 abs     14 3 print
z" 5 4 *:"      2 5 puts   5   4 *    14 5 print
z" -5 4 *:"     2 7 puts   -5  4 *    14 7 print
z" 30 7 /:"     2 9 puts   30  7 /    14 9 print
z" 30 7 MOD:"   2 11 puts  30  7 MOD  14 11 print
z" -30 7 /MOD:" 2 13 puts  -30 7 /MOD swap 14 13 print 18 13 print

htimer @ hstart @ -
z" Tests took " 2 15 puts 13 15 print  z" scanlines" 17 15 puts

\ Loop forever
begin false until 		


