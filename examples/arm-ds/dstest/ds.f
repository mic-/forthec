(
	ds.f
	Registers definitions and common functions for the DS
	/Mic, 2005
)

0x5000000	constant #PALETTE_RAM	
0x5000400	constant #PALETTE_SUB_RAM	
0x0000400	constant #PALETTE_RAM_SIZE	
0x6000000	constant #BG_RAM		
0x6200000	constant #BG_SUB_RAM	

0x4000000	constant #DISPCNT
0x4000004 	constant #DISPSTAT		
0x4000008	constant #BG0CNT		
0x400000A	constant #BG1CNT		
0x400000C	constant #BG2CNT		
0x400000E	constant #BG3CNT		
0x4000020	constant #BG2PA		
0x4000022	constant #BG2PB		
0x4000024	constant #BG2PC		
0x4000026	constant #BG2PD		
0x4000028	constant #BG2X		
0x400002C	constant #BG2Y
0x4000050	constant #BLDCNT
0x4000052	constant #BLDALPHA
0x4000054	constant #BLDY		
0x40000D4	constant #DM3SAD
0x40000D8	constant #DM3DAD
0x4000240	constant #VRAMCNT_A
0x4000241	constant #VRAMCNT_B
0x4000242	constant #VRAMCNT_C
0x4000243	constant #VRAMCNT_D
0x4000244	constant #VRAMCNT_E
0x4000304	constant #POWERCNT		
0x4001000	constant #DISPCNT_SUB	
0x4001008	constant #BG0CNT_SUB	
0x4001010	constant #BG0HOFS_SUB	
0x4001012	constant #BG0VOFS_SUB	

0x10000		constant #BG_MODE_0
0x10001		constant #BG_MODE_1
0x10002		constant #BG_MODE_2
0x10003		constant #BG_MODE_3
0x10003		constant #BG_MODE_4
0x10003		constant #BG_MODE_5
0x10006		constant #BG_MODE_6
0x20000		constant #FB_MODE_0
0x60000		constant #FB_MODE_1
0xA0000		constant #FB_MODE_2
0xE0000		constant #FB_MODE_3

0x0100		constant #BG0_ENABLE
0x0200		constant #BG1_ENABLE
0x0400		constant #BG2_ENABLE
0x0800		constant #BG3_ENABLE
0x1000		constant #OBJ_ENABLE
0x40000000	constant #EXTPAL_ENABLE

0		constant #PRIORITY_0
1		constant #PRIORITY_1
2		constant #PRIORITY_2
3		constant #PRIORITY_3
0x80		constant #COLORMODE_1x256
0x00		constant #COLORMODE_16x16
0x0000		constant #SCREENSIZE_TEXT_256x256
0x4000		constant #SCREENSIZE_TEXT_512x256
0x8000		constant #SCREENSIZE_TEXT_256x512
0xC000		constant #SCREENSIZE_TEXT_512x512
8		constant #SCREENBASE_SHIFT

0x80		constant #BANK_ENABLE	
0x00		constant #BANK_A_LCDC_6800000
0x01		constant #BANK_A_BG_6000000

0x30F		constant #POWER_ALL
0x8000		constant #SWAP_SCREENS

0		constant #MAIN
1		constant #SUB


\ Wait for the next vblank period
: vsync ( -- )
	begin #DISPSTAT w@ 1 and 0<> while repeat
	begin #DISPSTAT w@ 1 and 0=  while repeat ;



\ 16-bit transfer of cnt bytes from src to dest using DMA3
: dm3-copy-16 ( cnt dest src -- )
	#DM3SAD swap !r+ swap !r+ swap 1 >> 0x80000000 or !r+ drop ;

 