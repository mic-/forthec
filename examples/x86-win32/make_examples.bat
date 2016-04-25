..\..\forthec -t win32 dll\test1.f dll\test.exe
..\..\forthec -t win32 dll\test2.f dll\test.dll
del dll\test.lib
del dll\test.exp

..\..\forthec -t win32 -res dialog\dialog.f dialog\dialog.exe
..\..\forthec -t win32 emu6502\emu6502.f emu6502\emu6502.exe
..\..\forthec -t win32 factest\factest.f factest\factest.exe
..\..\forthec -t win32 fire\fire.f fire\fire.exe
..\..\forthec -t win32 floats\floats.f floats\floats.exe
..\..\forthec -t win32 gditest\gditest.f gditest\gditest.exe
..\..\forthec -t win32 glut\glut.f glut\glut.exe
..\..\forthec -t win32 iotest\iotest.f iotest\iotest.exe
..\..\forthec -t win32 mmx\mmx.f mmx\mmx.exe
..\..\forthec -t win32 musicplay\musicplay.f musicplay\musicplay.exe
..\..\forthec -t win32 recurse\recurse.f recurse\recurse.exe
..\..\forthec -t win32 -O1 sieve\sieve.f sieve\sieve-slow.exe
..\..\forthec -t win32 sieve\sieve.f sieve\sieve-opt.exe
..\..\forthec -t win32 window\window.f window\window.exe
..\..\forthec -t win32 winhello\winhello.f winhello\winhello.exe
