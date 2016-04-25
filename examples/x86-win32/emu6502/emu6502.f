(
	emu6502.f
	A simple 6502 emulation benchmark
	/Mic, 2004


	Only 11 opcodes are implemented. The memory layout is as follows:
	
		 2 kB RAM at 0000-07ff, mirrored throughout 0800-7fff
		16 kB ROM at 8000-bfff, mirrored at c000
)


variable ram 2048 allot
variable rom 16384 allot
variable cycle

\ 6502 registers
variable reg-a
variable reg-x
variable reg-y
variable reg-s
variable reg-pc

\ 6502 flags
variable flag-c
variable flag-n
variable flag-z
variable flag-v

\ Timing variables
variable perf-cnt1 8 allot
variable perf-cnt2 8 allot
variable perf-freq 8 allot



: read-byte { address -- }
	address 0x8000 < if
		address 0x7FF and ram + c@
	else
		address 0x3FFF and rom + c@
	then
	nip ;


: read-word { address -- }
	address 0x8000 < if
		address 0x7FF and ram + w@
	else
		address 0x3FFF and rom + w@
	then
	nip ;

	


: execute-until { threshold -- }	\ Keep executing 6502 code until cycle >= threshold
	begin
	cycle @ threshold < while
	
		reg-pc dup @ 0x3fff and rom + c@ swap @ 1+ reg-pc !
		case
	
		\ JMP aaaa
		0x4C of
			reg-pc @ 0x3fff and rom + w@ reg-pc !
			cycle @ 3 + cycle !
			endof
		
		\ LDA aa
		0xA5 of
			reg-pc @ 0x3fff and rom + c@ ram + c@ dup dup reg-a !
			flag-z ! 0x80 and flag-n !
			reg-pc @ 1+ reg-pc !
			cycle @ 3 + cycle !
			endof

		\ STA aa
		0x85 of
			reg-a @ reg-pc @ 0x3fff and rom + c@ ram + c! 
			reg-pc @ 1+ reg-pc !
			cycle @ 3 + cycle !
			endof
			
		\ BEQ <aa
		0xF0 of
			flag-z @ 0 = if
				reg-pc @ dup 0x3fff and rom + cs@ 1+ + reg-pc !
			else
				reg-pc @ 1+ reg-pc !
			then
			cycle @ 3 + cycle !
			endof

		\ BNE <aa
		0xD0 of
			flag-z @ 0 <> if
				reg-pc @ dup 0x3fff and rom + cs@ 1+ + reg-pc !
			else
				reg-pc @ 1+ reg-pc !
			then
			cycle @ 3 + cycle !
			endof
		
		\ LDA #aa
		0xA9 of
			reg-pc @ 0x3fff and rom + c@ dup dup reg-a !
			flag-z ! 0x80 and flag-n !
			reg-pc @ 1+ reg-pc !
			cycle @ 2 + cycle !
			endof
		
		\ DEX
		0xCA of
			reg-x @ 1- 0xFF and dup dup reg-x !
			flag-z ! 0x80 and flag-n !
			cycle @ 2 + cycle !
			endof

		\ DEY
		0x88 of
			reg-y @ 1- 0xFF and dup dup reg-y !
			flag-z ! 0x80 and flag-n !
			cycle @ 2 + cycle !
			endof

		\ INC aa
		0xE6 of
			reg-pc @ 0x3fff and rom + c@ ram + dup c@ 1+ 0xFF and tuck swap c! dup
			flag-z ! 0x80 and flag-n !
			reg-pc @ 1+ reg-pc !
			cycle @ 3 + cycle !
			endof
			
		\ LDY #aa
		0xA0 of
			reg-pc @ 0x3fff and rom + c@ dup dup reg-y ! 
			flag-z ! 0x80 and flag-n !
			reg-pc @ 1+ reg-pc !
			cycle @ 2 + cycle !
			endof	
			
		\ LDX #aa
		0xA2 of
			reg-pc @ 0x3fff and rom + c@ dup dup reg-x !
			flag-z ! 0x80 and flag-n !
			reg-pc @ 1+ reg-pc !
			cycle @ 2 + cycle !
			endof			
		
		endcase
	repeat
	drop ;



\ Store some 6502 code
here
0xA9 c, 0x00 c,		\ start: LDA #0
0x85 c, 0x08 c,		\ STA 08
0xA2 c, 0x0A c,		\ LDX #10
0xA0 c, 0x0A c,		\ loop1: LDY #10
0xE6 c, 0x08 c,		\ loop2: inc 08
0x88 c,			\ DEY
0xD0 c, 0xFB c,		\ BNE loop2
0xCA c,			\ DEX
0xD0 c, 0xF6 c,		\ BNE loop1
0x4C c, 0x00 c, 0x80 c,	\ JMP start
rom 20 cmove


\ Initialize the 6502 program counter
0x8000 reg-pc !
0 cycle !


." Executing 2,000,000 cycles..."

\ Read cycle count
perf-cnt1 call QueryPerformanceCounter

\ Execute 2,000,000 cycles
2000000 execute-until

\ Read cycle count again, and the frequency
perf-cnt2 call QueryPerformanceCounter
perf-freq call QueryPerformanceFrequency

." DONE" cr newline cr newline 
\ Print execution speed
." Emulation speed was " 2000000 float perf-freq @ float f* perf-cnt2 @ perf-cnt1 @ - float f/ 1000000 float f/ floor . ." MHz" 

cr newline cr newline
." 6502 state:" cr newline
." PC: " reg-pc @ . cr newline
." A:  " reg-a  @ . cr newline
." X:  " reg-x  @ . cr newline
." Y:  " reg-y  @ . cr newline

cr newline cr newline ." Press any key.."
key bye



