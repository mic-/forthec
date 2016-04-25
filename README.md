# ForthEC

ForthEC is a Forth cross compiler for Windows. It generates assembly code for the following processors:

* 80x86 (both 16-bit and 32-bit)
* ARM
* Blackfin
* M68000
* SuperH

For most of its targets, ForthEC relies on the GNU binutiles for assembly and linkage. You can find prebuilt Windows versions of binutils for several processors [here](http://jiggawatt.org/badc0de/xgcc.htm). For the 80x86 target [MASM32](http://www.masm32.com/) and [NASM](http://www.nasm.us/) are used. 

The compiler does not follow any standard and is missing some key aspects of ANS Forth, but it is never the less quite capable.
Included with the compiler is its full source code, as well as several examples for the target processors it supports. 

