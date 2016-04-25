		-------------------------------------------------------
					ForthEC
				    A Forth compiler

				       /Mic, 2004/2005

		-------------------------------------------------------

	http://jiggawatt.org/badc0de/forthec/ | https://github.com/mic-/forthec


	About
	-----
	
	ForthEC is a Forth compiler written in Euphoria. It generates assembly code
	for x86, ARM, Blackfin, M68000 and SuperH processors which can be assembled
 	and linked into executables using MASM32, NASM, DevkitARM, bfin-elf-gcc,
	m68k-coff-gcc or sh-coff-gcc.
	See the links section at the end of this document for information on how to
	obtain these programs.

	Usage
	-----

	ForthEC is used in the following way:

		forthec -t <target> [options] infile outfile

	The available targets are:
	
		arm	ARM processor family. Not OS-specific.
		bfin	Blackfin processor family.
		dos16	16-bit DOS (for COM files). Generates code for NASM.
		win32	32-bit Windows. Generates code for MASM32.
		m68k	M68000 processor family.
		sh	SuperH processor family.

	Common options for all targets:

		-v	Verbose mode. Print information about what's going on.
		-nf	Turn on constant expression folding.
		-nm	Turn off name mangling (default when target is .dll)
		-O0	Turn optimisations off.
		-O1	Remove unused constants and redundant fp literals.
		-O2	Same as -O1 plus redundant integer instructions are removed.
		-O3	Same as -O2 plus redundant fpu instructions are removed.
		-O4	Same as -O3 plus optimise loops and branches.
		-O5	Same as -O4 plus collapse common integer instruction patterns.
		-O6	Same as -O5 plus do some addtional loop optimisations.

	Common options for targets dos16 and win32:

		-f	Use alternative floating point code.

	win32 specific options:

		-dll	Insert DLL initialisation code when target is .ASM
		-res	Use a resource script (must be named same as the input file, but .rc)

	Common options for targets ARM, bfin, m68k and sh:

		-cpu <name>	Specify target cpu (arm7tdmi for target arm, m68000 for target m68k)
		-fentry <name>	Override default forth entrypoint symbol (_main for target arm, main for target m68k)
		-crt0 <name>	Override default crt0 filename (crt0.o)
		-ls <name>	Override default linkscript filename (lnkscript)
	
	ARM specific options:

		-arch <name>	Specify target architechture (armv4)
		-eb		Big-endian
		-el		Little-endian (default)
		-nofpu		Disable fpu code generation
		-entry <name>	Override default entrypoint symbol (_start)

	Blackfin specific options:
	
		-ni		No inlining of words

	
	The default optimisation level is -O6. The ARM, m68k and sh targets only has two
	levels of optimisations; none (0) or full (6).


	
	Paths
	-----
	
	By default ForthEC will look for the assemblers/linkers in the following directories:
	
	c:\masm32
	c:\nasm
	c:\devkitarm
	c:\bfin-elf-binutils
	c:\m68k-coff-gcc
	c:\program\kpit\gnush\sh-coff
	
	You can alter these paths in the configuration file (forthec.cfg) to reflect your
	setup.
	
	

	Public symbols
	--------------
	
	By default ForthEC will mangle the names of all user-defined words, as well as
	deleting unreferenced words. Since the compiler only has a view of compile-time
	references and not link-time references it can potentially remove code that is
	referenced by another object file. To avoid this you can declare a word as public
	prior to its definition. E.g:
	
		public foobar
		: foobar ... ;
	
	This has two effect: 1) it disables name mangling for this word. 2) it prevents the
	compiler from removing this word even if it's not referenced within its own module.



	x86 notes
	---------
	
	When the target is dos16, all integer parameters are limited to 16 bits.

	The FPU stack is normally limited to 8 levels. If you need more than that, use
	the -f flag.

	When using -res you might get an error message from link.exe about not being able
	to execute cvtres.exe. I solved this by copying cvtres.exe from \masm32\bin to
	\windows\system32.

	If the file extension of outfile is .ASM, ForthEC will generate a file containing
	assembly code equivalent to your forth program.
	
	If the file extension of outfile is .DLL or .EXE, ForthEC will invoke MASM32
	to assemble and link the assembly code. It will look for MASM32 includes/libraries
	in the directory specified in forthec.cfg.

	If you're target is .DLL but you want to compile and assemble in separate steps,
	you need to use the -dll flag for the compiler to insert the required DLL 
	initialisation code.


	ARM notes
	---------
	
	If the output filename has a .BIN extension ForthEC will invoke DevkitARM to 
	assemble and link the code into a binary. It will look for the relevant
	executables in C:\DEVKITARM. You also need a linkscript and crt0, which should
	be in the same directory as the compiler. You can use your own crt0 if you wish,
	as long as its entry point is _start, and it jumps to label named _main.

	Some words available in the x86 version are missing in the ARM version at this
	point. These include /, MOD, TUCK, ROT, ON, ERASE, FILL and others.

	There is no floating point support


	M68k notes
	----------
	
	If the output filename has a .BIN extension ForthEC will invoke m68k-coff-gcc to 
	assemble and link the code into a binary. It will look for the relevant
	executables in C:\M68K-COFF-GCC. You also need a linkscript and crt0, which should
	be in the same directory as the compiler. You can use your own crt0 if you wish.

	Some words available in the x86 version are missing in the m68k version at this
	point. These include TUCK, ON, ERASE, FILL and others.

	The compiler does not currently take advantage of 68020/30/40 extensions when you
	specify	one of these with the -cpu option.

	There is no floating point support

	
	SuperH notes
	------------
	
	Due to the way immediate values larger than 8 bits are loaded on the SuperH, it
	is best to keep words relatively small (if the assembly output of a word exceeds
	512 instructions you may get errorneous output).
	
	

	Examples
	--------

	Some examples are included in the "examples" folder. The examples have already
	been compiled, but if you wish to recompile them you can do so by running
	make_examples.bat. If you have MASM32 installed somewhere other than c:\masm32
	you need to modify forthec.cfg to reflect this.

	\arm-ds\	Examples for the Nintendo DS (ARM946E-S)
	\arm-gba\	Examples for the Nintendo Gameboy Advance (ARM7)
	\bfin\		Examples for the Blackfin processor
	\m68k-md\	Examples for the SEGA Megadrive (M68000)
	\sh-sat\	Examples for the SEGA Saturn (SH2)
	\x86-dos16\	Examples for DOS (x86)
	\x86-win32\	Examples for Windows 32-bit (x86)



	Implementation notes
	--------------------
	
	Not all words of the Forth83 standard are implemented, and implementations may
	differ from the standard. See supported_words.txt for a list of words that are
	implemented.

	Always use BYE to terminate an application.

	There is no real dictionary. You've got 128 kB of static variable space, leaving
	no room for large arrays.

	If an external function calls a user-defined word with the stdcall convention
	you must balance the stack before returning, eg. using drop.

	If you need to put a value in register eax before returning, use ;@ instead of
	; to terminate a word definition.
	
	The x86 version of the compiler supports some SIMD constructs (MMX/SSE/SSE2).
	These words all begin with the letter P (e.g. PC+ does a packed char addition).
  	Only a few of these words have been implemented so far, and they're not handled
 	by the optimiser at this point.

	When calling an external function (eg. a Win32 API function) and you need the
	returned value, use the word a@ to push register eax on the Forth stack.



	Optimisations
	-------------
	
	Unless optimisations have been turned off using the -O0 flag, some basic
	optimisations will be done to the resulting assembly code.
	This involves removing unused constants and replacing push/pop instructions
	with mov instructions, or removing them completely if possible.
	To avoid optimising away DROPs, they are translated into LEA ESP,[ESP+4]
	instead of ADD ESP,4.

	If a compare-word (eg. =, >) is found right before an IF, WHILE or ?LEAVE
	the step of generating a Forth true/false value, pushing it on the stack
	and popping it back will be optimised away (assuming an optimisation level
	of 4 or higher is used).

	DO-loops are optimised by trying to but the loop counter (I) and the end
	value in registers ESI and EDI for innermost loops. This will not be done
	if a user-defined word is called within the loop, since the compiler isn't
	smart enough to know wether that word will contain any DO-loops.
	
	If the stack depth remains constant inside a loop, the optimiser will
	try to put the TOS in a register when possible.

	The ARM code optimiser will go one step further and try to put as many
	stack variables as possible in registers.

	FPU code optimisation is done by scanning through the code for common
	patterns and replacing these with more compact groups of instructions.
	
	Optimisation mode O5 does for integer instructions what O3 does for fpu
	instructions. The difference is that the code will be scanned over and 
	over up to ten times, or until no further optimisations are found. This
	can be time-consuming for very large sources, but in the general case
	the time overhead is neglectable. Even though this simply is a brute-
	force technique, some programs still benefit greatly from it, since
	the code generated by the compiler always follows certain patterns.
	
	Some tips on writing code that can be optimised by the compiler:

		3.0e0 fpops 2.0e0 fpops 1.0e0 fpops

	is better than

		1.0e0 2.0e0 3.0e0 fpops fpops fpops

	and

		fp1 f@ fp2 f@ fp3 f@ f+ f+ fp4 f@ f+

	is better than

		fp1 fp2 fp3 fp4 f@ f@ f@ f@ f+ f+ f+

	where fp1..fp4 are fvariables.



	Links
	-----

	MASM32:		    http://www.masm32.com,
	NASM:		    http://www.nasm.us/
	bfin-elf-binutils:  http://jiggawatt.org/badc0de/xgcc.htm
	DevkitARM:	    http://www.devkit.tk
	sh-coff-gcc:	    http://www.kpitgnutools.com/ (free registration required)

		