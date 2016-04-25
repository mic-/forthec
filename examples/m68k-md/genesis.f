\ Various definitions and functions for the Genesis


0xC00000	constant #VDPDATA 		
0xC00004	constant #VDPCNT

0x10		constant #VDP0_E_HBI
0x02		constant #VDP0_E_DISPLAY 
0x04		constant #VDP0_PLTT_FULL 

0x80		constant #VDP1_SMS_MODE
0x40		constant #VDP1_E_DISPLAY
0x20		constant #VDP1_E_VBI
0x10		constant #VDP1_E_DMA
0x00		constant #VDP1_NTSC
0x08		constant #VDP1_PAL
0x04		constant #VDP1_RESERVED

0x08		constant #VDP12_SPR_SHADOWS
0x00		constant #VDP12_SCREEN_V224
0x04		constant #VDP12_SCREEN_V448
0x00		constant #VDP12_PROGRESSIVE
0x02		constant #VDP12_INTERLACED
0x00		constant #VDP12_SCREEN_H256
0x81		constant #VDP12_SCREEN_H320

0x00		constant #VDP16_MAP_V32
0x10		constant #VDP16_MAP_V64
0x30		constant #VDP16_MAP_V128
0x00		constant #VDP16_MAP_H32
0x01		constant #VDP16_MAP_H64
0x03		constant #VDP16_MAP_H128


: write_vdp_reg ( val reg -- )
	8 << 0x8000 or or #VDPCNT w! ;


: set_vram_addr ( addr -- )
	dup 0x3FFF and 0x4000 or 16 << swap 14 >> or #VDPCNT ! ;


: set_cram_addr ( addr -- )
	dup 0x3FFF and 0xC000 or 16 << swap 14 >> or #VDPCNT ! ;


: set_vscroll_addr ( addr -- )
	dup 0x3FFF and 0x4000 or 16 << swap 14 >> 0x10 or or #VDPCNT ! ;


: set_hscroll_addr ( addr -- )
	dup 0x3FFF and 0x4000 or 16 << swap 14 >> or #VDPCNT ! ;
	

