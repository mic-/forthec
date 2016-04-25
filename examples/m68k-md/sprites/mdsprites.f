\ Sprite example for SEGA Genesis

include genesis.f

extern constant ball	\ This label exists in ball.o
extern constant sin32	\ This label exists in sin32.o
extern constant vtimer	\ This label exists in crt0.o

variable sprite_data
variable angle
variable scroll


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
	0xFC00 9  >> 5 write_vdp_reg	\ Sprite list address
	0 6 write_vdp_reg		
	0 7 write_vdp_reg		\ Border color
	1 8 write_vdp_reg
	1 9 write_vdp_reg
	1 10 write_vdp_reg		\ Lines per HBI
	0 11 write_vdp_reg		\ whole screen vertical scrolling
	#VDP12_SCREEN_V224 #VDP12_SCREEN_H256 #VDP12_PROGRESSIVE or or 12 write_vdp_reg
	0x6000 10 >> 13 write_vdp_reg	\ Hscroll list
	2 15 write_vdp_reg
	#VDP16_MAP_V32 #VDP16_MAP_H32 or 16 write_vdp_reg
	0 17 write_vdp_reg
	0xFF 18 write_vdp_reg ;


\ Load tiles into VRAM
: load_tiles
	0x0000 set_vram_addr
	ball
	64 0 do		\ 4 tiles = 64 words
		w@+ #VDPDATA w! loop drop 
	\ Create two tiles filled with colors 7 and 8, respectively
	16 0 do
		0x7777 #VDPDATA w! loop
	16 0 do
		0x8888 #VDPDATA w! loop ;
		
	

\ Set colors 
: set_colors
	0x0000 set_cram_addr
	\ Sprite colors

	0x0EEE 0x088E 0x000E 0x000E 0x000E 0x000A 0x0000      
	#VDPDATA w! #VDPDATA w! #VDPDATA w! #VDPDATA w! #VDPDATA w! #VDPDATA w! #VDPDATA w!
	\ Background colors
	0x00d0 0x0d00 #VDPDATA w! #VDPDATA w! ;



\ Initialise sprite data
: init_sprites
	here dup sprite_data !				\ Store the data in the dictionary
	128 w,		\ Y
	5 c,		\ Size: 16x16
	1 c,		\ Next sprite in list
	0x8000 w,	\ Priority 1, tile 0
	128 w,		\ X

	128 w, 5 c, 2 c, 0x8000 w, 128 w,		\ ..same as above, but compacted
	128 w, 5 c, 3 c, 0x8000 w, 128 w,		\ ...
	128 w, 5 c, 0 c, 0x8000 w, 128 w, ;		\ Last sprite (link byte is 0)
	

\ Draw an xor-pattern on the background
: draw_xor_pattern
	0xE000 set_vram_addr
	32 0 do			\ rows
		32 0 do		\ columns
			i j xor 1 >> 1 and 4 + 0x8000 or #VDPDATA w! loop
		loop ;
		
		
\ Rotate the sprites around the center of the screen	
: move_sprites
	angle @ 1+ dup angle !				\ Increase angle
	\ Update Y-coordinates

	4 0 do
		dup i 6 << +				\ Add i*64 to the angle
		0xFF and sin32 + <c@ 240 +		\ Get sine, 240 = vertical center 
		sprite_data @ i 3 << +  w! loop  
	\ Update X-coordinates
	4 0 do
		dup i 6 << + 64 +			\ Add i*64 to the angle
		0xFF and sin32 + <c@ 256 +		\ Get cosine, 256 = horzontal center 
		sprite_data @ i 3 << + 6 + w! loop
	drop 

	\ Now copy this to VRAM
	0xFC00 set_vram_addr
	sprite_data @
	16 0 do
		w@+ #VDPDATA w! loop drop ;		

		
: scroll_bg ( -- )
	scroll @ 1+ 255 and dup dup dup dup scroll !
	0x0000 set_vscroll_addr #VDPDATA w! #VDPDATA w!
	0x6000 set_hscroll_addr #VDPDATA w! #VDPDATA w! ;
	


init_vdp load_tiles set_colors init_sprites draw_xor_pattern

\ Loop forever
begin move_sprites scroll_bg wait_vsync false until 		


