(
	iotest.f
	Reads some text from the keyboard and prints it back to the screen
	/Mic, 2004
)

variable user-input 64 allot

." Enter some text, finish with [Return]: "

user-input 32 expect				\ Read max 32 characters and store in user-input
cr newline

." You entered " user-input ".
cr newline

." Press any key.." key bye