@echo off
echo tasmx Batch Compiler
echo Jess M. Askey 2000
echo Project Name: Major Havoc 
echo .


rem We reqire a few files to exist for this whole enchilada to work
if not exist .\exp\mh_alpha.exp (copy /b NUL .\exp\mh_alpha.exp)
if not exist .\exp\auxpgm_0.exp (copy /b NUL .\exp\auxpgm_0.exp)
if not exist .\exp\auxpgm_1.exp (copy /b NUL .\exp\auxpgm_1.exp)
if not exist .\exp\auxpgm_2.exp (copy /b NUL .\exp\auxpgm_2.exp)
if not exist .\exp\auxpgm_3.exp (copy /b NUL .\exp\auxpgm_3.exp)
if not exist .\exp\auxpgm_4.exp (copy /b NUL .\exp\auxpgm_4.exp)
if not exist .\exp\auxpgm_5.exp (copy /b NUL .\exp\auxpgm_5.exp)
if not exist .\exp\auxpgm_6.exp (copy /b NUL .\exp\auxpgm_6.exp)
if not exist .\exp\auxpgm_7.exp (copy /b NUL .\exp\auxpgm_7.exp)


echo Compiling Program Pass 1
call mh_alpha.bat

echo Source Files compiled...

echo Compiling Program Error Logs...

copy auxpgm_0.err+auxpgm_1.err+auxpgm_2.err+auxpgm_3.err+auxpgm_4.err+auxpgm_5.err+auxpgm_6.err+auxpgm_7.err+mh_alpha.err havoc_errors.txt > copy.err
del auxpgm_0.err
del auxpgm_1.err
del auxpgm_2.err
del auxpgm_3.err
del auxpgm_4.err
del auxpgm_5.err
del auxpgm_6.err
del auxpgm_7.err
del mh_alpha.err
del copy.err


call build_roms.bat
call copy_mame.bat

start havoc_errors.txt
