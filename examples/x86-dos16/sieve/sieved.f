(
	sieved.f
	Prime sieve benchmark for the ForthEC compiler, DOS version
	/Mic, 2004/2005
)

50 	constant #BATCH
10 	constant #BENCH_TIME	\ Benchmark time in seconds
500 	constant #SIZE		\ Find primes up to #SIZE*2+1

variable flags #SIZE allot	\ Allocate 4+#SIZE bytes
variable time
variable end-time
variable cycles


: get-time 
	notouch
	mov ah,0x2C
	int 21h
	push dx
	touch ;

: get-seconds 
	notouch
	mov ah,0x2C
	int 21h
	shr dx,8
	push dx
	touch ;


: sieve ( -- n )
	flags 1+ #SIZE true fill

	0
	#SIZE 1 do
		flags i + c@ false <> if
			i 2* 1+
			#SIZE 1+ over i + do
				 i #SIZE > ?leave
				false flags i + c!
			dup +loop drop
			1+ 
		then
	loop ;
	

." Prime sieve benchmark. Wait " #BENCH_TIME . ." seconds.." cr newline

0 cycles !
get-time time !
time @ 8 >> #BENCH_TIME + dup 60 > if 60 - then end-time !


begin
	get-seconds end-time @ <> while
		#BATCH 0 do
			sieve 167 <> if
				." Oops!" cr newline
			then
		loop
		cycles @ 1+ cycles !
	repeat

get-time end-time !

cycles @ float #BATCH float f* 100.0e0 f* 
end-time @ 8 >> time @ 8 >> - dup 0 < if 60 + then float 100.0e0 f*
end-time @ 0xff and time @ 0xff and - dup 0 < if swap 100 - swap negate then
float f+ f/ floor . ." sieves per second" cr newline cr newline


\ Display results
."  2  "			\ 2 is a prime too
#SIZE 1+ 1 do
	flags i + c@ false <> if
		i 2* 1+ . ."  "
	then 
loop

cr newline cr newline ." Press any key.." key bye


