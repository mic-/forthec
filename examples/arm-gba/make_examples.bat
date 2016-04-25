@echo off
echo Recompiling these examples requires DevkitARM to be installed in
echo C:\DevkitARM. 
echo on

..\..\forthec -t arm -cpu arm7tdmi -nofpu -ls %cd%\lnkscript -crt0 %cd%\crt0.o ffire\ffire.f ffire\ffire.bin 
..\..\forthec -t arm -cpu arm7tdmi -nofpu -ls %cd%\lnkscript -crt0 %cd%\crt0.o splash\splash.f splash\splash.bin %cd%\splash\sdata.o 


