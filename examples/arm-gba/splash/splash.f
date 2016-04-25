(
	splash.f
	Mode 4 splash screen
	/Mic, 2004
)

0x4000000 	constant #DISPCNT
0x4000004 	constant #DISPSTAT
0x40000D4	constant #DM3SAD

\ These constants are labels in sdata.s
extern constant bitmap
extern constant palette

 
: vsync
	begin #DISPSTAT w@ 1 and 0<> while repeat
	begin #DISPSTAT w@ 1 and 0=  while repeat ;
 	
 
\ Wait a few frames
vsync vsync vsync

\ Set mode 4 and enable BG2
0x404 #DISPCNT !

\ Transfer some data by DMA
#DM3SAD palette !r+ 0x5000000 !r+ 0x84000080 !r+ drop
#DM3SAD bitmap  !r+ 0x6000000 !r+ 0x84002580 !r+ drop

begin vsync false until bye

 
 	
 
 
 	
 