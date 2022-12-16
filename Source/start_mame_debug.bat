
rem *******************************************
rem * Driver name passed in %1
rem *******************************************


md ..\mame\roms\%1
copy .\rom\mhavocpe.* ..\mame\roms\%1\%1.*

cd ..\mame
hbmame.exe -debug -snapsize 1280x960 %1

