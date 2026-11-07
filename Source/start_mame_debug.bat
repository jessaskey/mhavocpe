
rem *******************************************
rem * Driver name passed in %1
rem *******************************************


md ..\mame\roms\%1
rem copy .\rom\mhavocpe.* ..\mame\roms\%1\%1.*
copy .\rom\mhavocpe.1l ..\mame\roms\%1\mhpe100.1l
copy .\rom\mhavocpe.1mn ..\mame\roms\%1\mhpe100.1mn
copy .\rom\mhavocpe.1np ..\mame\roms\%1\mhpe100.1np
copy .\rom\mhavocpe.1q ..\mame\roms\%1\mhpe100.1q
copy .\rom\mhavocpe.6h ..\mame\roms\%1\mhpe100.6h
copy .\rom\mhavocpe.6jk ..\mame\roms\%1\mhpe100.6jk
copy .\rom\mhavocpe.6kl ..\mame\roms\%1\mhpe100.6kl
copy .\rom\mhavocpe.9s ..\mame\roms\%1\mhpe100.9s
copy .\rom\mhavocpe.x1 ..\mame\roms\%1\mhpex089.x1

cd ..\mame
hbmame.exe -debug -snapsize 1280x960 %1

