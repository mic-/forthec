(
	Test program for ForthEC/Blackfin
	Runs in the Zombie emulator shell
	/Mic, 2005
)


z" \n" constant #CRLF

\ Zombie allows strings to be printed by writing the string
\ address to 0xFFFC.
: write-string ( adr -- )
	0xFFFC ! ;


\ Zombie allows integers to be printed by writing the value
\ to 0xFFF8.
: write-int ( n -- )
	0xFFF8 ! ;


\ Iterative factorial function
: factorial ( n -- n! )
	1 begin over 2 < ?leave	over * swap 1- swap repeat nip ;


#CRLF write-string
z" 5 factorial: " write-string
5 factorial write-int
#CRLF write-string
	 
begin repeat	\ Loop forever

