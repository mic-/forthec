(
	ffire.f
	Fire effect for ForthEC/ARM
	/Mic, 2004
)


0x4000000 	constant #DISPCNT
0x4000004 	constant #DISPSTAT
0x400000C	constant #BG2CNT
0x6000000	constant #VIDEO


variable done
variable rng
variable rngh

defer random


: vsync ( -- )
	begin #DISPSTAT w@ 1 and 0<> while repeat
	begin #DISPSTAT w@ 1 and 0=  while repeat ;



\ This draws the actual fire
: flames
	1440 #VIDEO +	\ Address to write to
	1918 #VIDEO +	\ Address to read from

	120 0 do
		2dup
		162 8 do
			dup dup dup dup
			w@ 255 and swap 2 + w@ 255 and + swap 4 +
			w@ 255 and + swap 484 + w@ 255 and + 2 >>
			dup 0<> if 1- then
			dup 8 << or
			dup 3 pick w! 2 pick 240 + w! swap 480 + swap 480 + 2 +loop
		random 8 + 191 and #VIDEO 38400 i 2* + + w!
		2drop swap 2 + swap 2 + loop 2drop ;
				

: setup-palette
	0x5000000
	32 0 do
		i over w!			\ black -> red
		i 5 << 0x1F or over 64 + w!	\ red -> yellow
		i 10 << 0x3FF or over 128 + w!	\ yellow -> white
		2 + loop 128 +
	
	160 0 do
		0x7fff over w! 2 + loop drop ;	\ Fill with white	


\ Random number generator (borrowed from Paul Dixon)
: random ( -- n )
	rngh rng notouch
	ldr r2,[r10],#4
	ldr r1,[r2]
	movs r1,r1,lsr#1
	ldr r3,[r0]
	movs r4,r3,rrx
	movcc r5,#0
	movcs r5,#1
	orr r1,r5,r1,lsl#1		
	str r1,[r2]
	mov r3,r3,lsl#12
	eor r4,r4,r3
	mov r3,r4,lsr#20
	eor r4,r4,r3
	str r4,[r0]
	mov r0,r4
	touch ;


\ Wait a few frames
vsync vsync vsync

\ Set mode 4 and enable BG2
0x404 #DISPCNT !

setup-palette

19 rng !
1 rngh !

begin
	vsync
	flames
false until bye

