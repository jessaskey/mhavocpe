
rem *******************************************
rem * Define MAMEPATH in your Environment
rem * variables or the ROM's will go to a 
rem * default mame location of c:\Mame
rem *******************************************
rem * Driver name passed in %1
rem *******************************************

if "%MAMEPATH%"=="" (
	set MAMEPATH="c:\mame"
)

md %MAMEPATH%\roms\%
copy rom\mhpe.* %MAMEPATH%\roms\%1\%1.*
@echo on