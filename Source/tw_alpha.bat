@echo off
echo tasmx Batch Compiler
echo Jess M. Askey 2000
echo Project Name: Tolian Web/Major Havoc 
echo .


rem We require a few files to exist for this whole enchilada to work
type ".module alpha" >> .\exp\tw_alpha.exp
type NUL  >> .\mh_gamma.err
type NUL  >> .\auxpgm_0.err
type NUL  >> .\auxpgm_1.err
type NUL  >> .\auxpgm_2.err
type NUL  >> .\auxpgm_3.err



echo Compiling Modules Pass 1

echo    Alpha Main by Module...
tasmx -65 -b -y -s -fff -q tw_alpha.asm .\obj\tw_alpha.obj .\lst\tw_alpha.lst .\exp\tw_alpha.exp .\sym\tw_alpha.sym > tw_alpha.err


echo Compiling Program Pass 2

echo    Alpha Main by Module...
tasmx -65 -b -y -s -fff tw_alpha.asm .\obj\tw_alpha.obj .\lst\tw_alpha.lst .\exp\tw_alpha.exp .\sym\tw_alpha.sym > tw_alpha.err

echo Source Files compiled...

echo Compiling Vector Error Logs

copy mh_vrom0.err+mh_vrom1.err+mh_vrom2.err+mh_vrom3.err+mh_vrom.err vector_errors.txt > copy.err
del mh_vrom.err
del mh_vrom0.err
del mh_vrom1.err
del mh_vrom2.err
del mh_vrom3.err
del copy.err

echo Compiling Program Error Logs...

copy mh_gamma.err+auxpgm_0.err+auxpgm_1.err+auxpgm_2.err+auxpgm_3.err+tw_alpha.err tw_errors.txt > copy.err
del mh_gamma.err
del auxpgm_0.err
del auxpgm_1.err
del auxpgm_2.err
del auxpgm_3.err
del tw_alpha.err
del copy.err
