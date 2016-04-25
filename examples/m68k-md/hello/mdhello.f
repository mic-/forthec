\ Hello world for SEGA Genesis

include genesis.f

\ This label exists in font.o
extern constant font

z" Hello world!" constant #HELLO


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
	1 10 write_vdp_reg		\ Lines per HBI
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


\ Print "Hello world!" at the center of the screen
: say_hello
	0xE000 13 5 << 10 + 2* + set_vram_addr	\ Row 13, column 10
	#HELLO
	begin true while
		dup c@ dup 0= ?leave 32 - 0x8000 or #VDPDATA w!
		1+ repeat drop ;


init_vdp load_charset set_colors say_hello

\ Loop forever
begin false until 		


