(
	factest.f
	Faculty function benchmark for the ForthEC compiler
	/Mic, 2004
)

\ Number of times to execute the function
1000000 constant #TIMES

\ Timing variables
variable perf-cnt1 8 allot
variable perf-cnt2 8 allot
variable perf-cnt3 8 allot
variable perf-cnt4 8 allot
variable perf-freq 8 allot


\ Iterative faculty function (goes from n->2 with successive multiplications)
: n! ( n1 -- n2 )
	1 begin true while
	over 2 < ?leave	over * swap 1 - swap repeat nip ;


\ Recursive faculty function
: n!rec ( n1 -- n2 )
	dup 1 > if dup 1- n! * then ;			
			


." Faculty function benchmark. Wait.. " cr newline

perf-freq call QueryPerformanceFrequency


\ Read cycle count
perf-cnt1 call QueryPerformanceCounter

\ Calculate 11! one million times
 #TIMES 0 do
	11 n! drop 
	loop

\ Read cycle count again, and the frequency
perf-cnt2 call QueryPerformanceCounter

\ Same thing for recursive version
perf-cnt3 call QueryPerformanceCounter
 #TIMES 0 do
	11 n!rec drop 
	loop
perf-cnt4 call QueryPerformanceCounter



\ Print results
cr newline ." Result (11!): " 11 n!rec . cr newline cr newline
." Time needed for 1,000,000 calculations:" cr newline cr newline
."    Iterative version: " perf-cnt2 @ perf-cnt1 @ - 1000 * perf-freq @ / . ." ms" cr newline
."    Recursive version: " perf-cnt4 @ perf-cnt3 @ - 1000 * perf-freq @ / . ." ms"

cr newline cr newline ." Press any key.." key bye


