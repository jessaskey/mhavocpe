echo Arranging ROM Images

md rom

@echo off
split obj\mh_alpha.obj 16384
copy /b obj\mh_alpha.obj.1 rom\mhavocpe.1mn
copy /b obj\mh_alpha.obj.2 rom\mhavocpe.1l
copy /b obj\auxpgm_0.obj+obj\auxpgm_1.obj+obj\auxpgm_4.obj+obj\auxpgm_5.obj rom\mhavocpe.1q
copy /b obj\auxpgm_2.obj+obj\auxpgm_3.obj+obj\auxpgm_6.obj+obj\auxpgm_7.obj rom\mhavocpe.1np

copy /b obj\mh_vrom.obj rom\mhavocpe.6kl
copy /b obj\mh_vrom0.obj + obj\mh_vrom1.obj rom\mhavocpe.6h /B
copy /b obj\mh_vrom2.obj + obj\mh_vrom3.obj rom\mhavocpe.6jk /B
copy /b obj\mh_gamma.obj rom\mhavocpe.9s
split obj\mh_beta.obj 16384
copy /b obj\mh_beta.obj.1 rom\mhavocpe.1bc
copy /b obj\mh_beta.obj.2 rom\mhavocpe.1d

del obj\mh_alpha.obj.1
del obj\mh_alpha.obj.2
del obj\mh_beta.obj.1
del obj\mh_beta.obj.2

tasmx.exe -65 -g3 -c -q mhavocpe.asm .\obj\mhavocpe.obj .\lst\mhavocpe.lst .\exp\mhavocpe.exp
copy exp\mhavocpe.exp rom\mhavocpe.exp



