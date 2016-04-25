(
	dstest.f
	Displays some text on BG0 of the main core in mode 0
	/Mic, 2005
)


include dstest\ds.f

\ Declared in dsdata.s
extern constant font


: draw-text ( lpszText x y vram_address -- )
	swap 3 >> 6 << + swap 3 >> 1 << +
	256 0 do
		over c@ dup 0= if drop leave then
		dup 32 = if drop 0 else 37 - then
		over w! swap 1+ swap 2 + loop drop drop ; 



\ Wait a few frames
vsync vsync

\ Turn on both screens, main screen on top
#POWER_ALL #SWAP_SCREENS or #POWERCNT w!


\ Map VRAM bank A to main BG
#BANK_ENABLE #BANK_A_BG_6000000 or #VRAMCNT_A c!


\ Set up palette (colors 96..152 = fading orange every 8th color)
#PALETTE_RAM 96 1 << +
8 0 do 31 i 1 << - dup 1 >> 5 << or over i 4 << + w! loop drop


\ Copy font data to VRAM
5376 #BG_RAM font dm3-copy-16


\ Set mode 0 and enable BG0
#BG_MODE_0 #BG0_ENABLE or #DISPCNT !


\ BG0: text bg, 256 colors, 256x256 pixels, screen base 0x4000, priority 0
#COLORMODE_1x256 #SCREENSIZE_TEXT_256x256 or 8 #SCREENBASE_SHIFT << or
#PRIORITY_0 or #BG0CNT w!


\ Clear BG0
#BG_RAM 256 0 do 0 over w! 2 + loop drop


\ Draw some text at 80,88 on BG0
z" HELLO WORLD)" 80 88 #BG_RAM 0x4000 + draw-text


\ Loop forever
begin vsync false until bye
