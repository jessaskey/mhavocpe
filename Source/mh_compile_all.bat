@echo off
echo tasmx Batch Compiler
echo Jess M. Askey 2000
echo Project Name: Major Havoc 
echo .

rem We reqire a few files to exist for this whole enchilada to work
call mh_create_files.bat

echo Compiling Vector Pass 1
call mh_vector.bat

echo Compiling Program Pass 1
call mh_gamma.bat
call mh_alpha.bat

echo Compiling Vector Pass 2
call mh_vector.bat

echo Compiling Program Pass 2
call mh_gamma.bat
call mh_alpha.bat

echo Source Files compiled...

echo Compiling Program Error Logs...

copy mh_vrom0.err+mh_vrom1.err+mh_vrom2.err+mh_vrom3.err+mh_vrom.err+mh_gamma.err+auxpgm_0.err+auxpgm_1.err+auxpgm_2.err+auxpgm_3.err+auxpgm_4.err+auxpgm_5.err+auxpgm_6.err+auxpgm_7.err+mh_alpha.err havoc_errors.txt > copy.err
del mh_vrom.err
del mh_vrom0.err
del mh_vrom1.err
del mh_vrom2.err
del mh_vrom3.err
del mh_gamma.err
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
