(
	sieve.f
	Prime sieve benchmark for the ForthEC compiler
	/Mic, 2004
)

50 	constant #BATCH
10000 	constant #BENCH_TIME	\ Benchmark time in seconds
500 	constant #SIZE		\ Find primes up to #SIZE*2+1

variable flags #SIZE allot	\ Allocate 4+#SIZE bytes
variable time
variable cycles


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
	

." Prime sieve benchmark. Wait "
#BENCH_TIME 1000 / .
." seconds.." cr newline

0 cycles !
call GetTickCount a@ time !

begin
	call GetTickCount a@ time @ #BENCH_TIME + < while
		#BATCH 0 do
			sieve 167 <> if
				." Oops!" cr newline
			then
		loop
		#BATCH cycles @ + cycles !
	repeat

call GetTickCount a@ time @ - 
cycles @ 1000 * swap / .
." sieves per second"
cr newline cr newline



\ Display results
." 2  "			\ 2 is a prime too
#SIZE 1+ 1 do
	flags i + c@ false <> if
		i 2* 1+ .
	then 
loop

cr newline cr newline ." Press any key.." key bye


