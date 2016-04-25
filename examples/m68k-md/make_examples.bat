@echo off
echo Recompiling these examples requires m68k-coff-gcc to be installed in
echo C:\m68k-coff-gcc 
echo on

..\..\forthec -t m68k -ls %cd%\md.ld -crt0 %cd%\md_crt0.o arithmet\mdarith.f arithmet\mdarith.bin arithmet\font.o
..\..\forthec -t m68k -ls %cd%\md.ld -crt0 %cd%\md_crt0.o hello\mdhello.f hello\mdhello.bin hello\font.o
..\..\forthec -t m68k -ls %cd%\md.ld -crt0 %cd%\md_crt0.o sprites\mdsprites.f sprites\mdsprites.bin sprites\ball.o sprites\sin32.o


