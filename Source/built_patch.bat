md rom\patch
::del /Q rom\patch\*
:: build patch file
copy .\bspatch\bspatch.exe .\rom\patch
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.210 rom\mhavocpe.6kl rom\patch\mhavocpe_6kl.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.216 rom\mhavocpe.1mn rom\patch\mhavocpe_1mn.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.217 rom\mhavocpe.1l rom\patch\mhavocpe_1l.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.215 rom\mhavocpe.1q rom\patch\mhavocpe_1q.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.318 rom\mhavocpe.1np rom\patch\mhavocpe_1np.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.106 rom\mhavocpe.6h rom\patch\mhavocpe_6h.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.107 rom\mhavocpe.6jk rom\patch\mhavocpe_6jk.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.108 rom\mhavocpe.9s rom\patch\mhavocpe_9s.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.217 rom\mhavocpe.1bc rom\patch\mhavocpe_1bc.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.217 rom\mhavocpe.1d rom\patch\mhavocpe_1d.bsp
bspatch\bsdiff.exe ..\havoc_prod\orig_rom\136025.210 rom\mhavocpe.x1 rom\patch\mhavocpe_x1.bsp

md rom\patch\test
del /Q rom\patch\test\*
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.210 .\rom\patch\test\mhavocpe.6kl .\rom\patch\mhavocpe_6kl.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.216 .\rom\patch\test\mhavocpe.1mn .\rom\patch\mhavocpe_1mn.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.217 .\rom\patch\test\mhavocpe.1l .\rom\patch\mhavocpe_1l.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.215 .\rom\patch\test\mhavocpe.1q .\rom\patch\mhavocpe_1q.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.318 .\rom\patch\test\mhavocpe.1np .\rom\patch\mhavocpe_1np.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.106 .\rom\patch\test\mhavocpe.6h .\rom\patch\mhavocpe_6h.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.107 .\rom\patch\test\mhavocpe.6jk .\rom\patch\mhavocpe_6jk.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.108 .\rom\patch\test\mhavocpe.9s .\rom\patch\mhavocpe_9s.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.217 .\rom\patch\test\mhavocpe.1bc .\rom\patch\mhavocpe_1bc.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.217 .\rom\patch\test\mhavocpe.1d	.\rom\patch\mhavocpe_1d.bsp
bspatch\bspatch.exe ..\havoc_prod\orig_rom\136025.210 .\rom\patch\test\mhavocpe.x1	.\rom\patch\mhavocpe_x1.bsp

comp /M rom\mhavocpe.6kl rom\patch\test\mhavocpe.6kl > rom\patch\test\mhavocpe.6kl.cmp
set cmp6kl=%ERRORLEVEL%
comp /M rom\mhavocpe.1mn rom\patch\test\mhavocpe.1mn > rom\patch\test\mhavocpe.1mn.cmp
set cmp1mn=%ERRORLEVEL%
comp /M rom\mhavocpe.1l rom\patch\test\mhavocpe.1l > rom\patch\test\mhavocpe.1l.cmp
set cmp1l=%ERRORLEVEL%
comp /M rom\mhavocpe.1q rom\patch\test\mhavocpe.1q > rom\patch\test\mhavocpe.1q.cmp
set cmp1q=%ERRORLEVEL%
comp /M rom\mhavocpe.1np rom\patch\test\mhavocpe.1np > rom\patch\test\mhavocpe.1np.cmp
set cmp1np=%ERRORLEVEL%
comp /M rom\mhavocpe.6h rom\patch\test\mhavocpe.6h > rom\patch\test\mhavocpe.6h.cmp
set cmp6h=%ERRORLEVEL%
comp /M rom\mhavocpe.6jk rom\patch\test\mhavocpe.6jk > rom\patch\test\mhavocpe.6jk.cmp
set cmp6jk=%ERRORLEVEL%
comp /M rom\mhavocpe.9s rom\patch\test\mhavocpe.9s > rom\patch\test\mhavocpe.9s.cmp
set cmp9s=%ERRORLEVEL%
comp /M rom\mhavocpe.1bc rom\patch\test\mhavocpe.1bc > rom\patch\test\mhavocpe.1bc.cmp
set cmp1bc=%ERRORLEVEL%
comp /M rom\mhavocpe.1d rom\patch\test\mhavocpe.1d > rom\patch\test\mhavocpe.1d.cmp
set cmp1d=%ERRORLEVEL%
comp /M rom\mhavocpe.x1 rom\patch\test\mhavocpe.x1 > rom\patch\test\mhavocpe.x1.cmp
set cmp1d=%ERRORLEVEL%

::del rom\patch\test\*.cmp
set /a "invalid=%cmp6kl%+%cmp1mn%+%cmp1l%+%cmp1q%+%cmp1np%+%cmp6h%+%cmp6jk%+%cmp9s%+%cmp1bc%+%cmp1d%"

if %invalid%==0 (goto filesValid)
copy nul rom\patch\test\INVALID.FILES
goto exit

:filesValid
copy nul rom\patch\test\VALID.FILES

:exit

