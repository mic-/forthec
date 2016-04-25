@echo off
echo Recompiling these examples requires DevkitARM to be installed in
echo C:\DevkitARM. 
echo on

..\..\forthec -t arm -arch armv5te -nofpu -ls %cd%\ds_arm9.ld -crt0 %cd%\ds_arm9_crt0.o -fentry main dstest\dstest.f dstest\dstest.bin dstest\dsdata.o


