echo    Vector ROM Page 0
tasmx -65 -b -y -s mh_vrom0.asm .\obj\mh_vrom0.obj .\lst\mh_vrom0.lst .\exp\mh_vrom0.exp .\sym\mh_vrom0.sym > mh_vrom0.err

echo    Vector ROM Page 1
tasmx -65 -b -y -s mh_vrom1.asm .\obj\mh_vrom1.obj .\lst\mh_vrom1.lst .\exp\mh_vrom1.exp .\sym\mh_vrom1.sym > mh_vrom1.err

echo    Vector ROM Page 2
tasmx -65 -b -y -s mh_vrom2.asm .\obj\mh_vrom2.obj .\lst\mh_vrom2.lst .\exp\mh_vrom2.exp .\sym\mh_vrom2.sym > mh_vrom2.err

echo    Vector ROM Page 3
tasmx -65 -b -y -s mh_vrom3.asm .\obj\mh_vrom3.obj .\lst\mh_vrom3.lst .\exp\mh_vrom3.exp .\sym\mh_vrom3.sym > mh_vrom3.err

echo    Vector ROM...
tasmx -65 -b -y -s mh_vrom.asm .\obj\mh_vrom.obj .\lst\mh_vrom.lst .\exp\mh_vrom.exp .\sym\mh_vrom.sym > mh_vrom.err
