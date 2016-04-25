.text
.globl _start
.extern _main

.equ USR_STACK, 0x03007F00

_start:
sp.h = USR_STACK;
sp.l = USR_STACK;

jump _main

after_main: jump after_main

