These are updated source files for MAME 0195 which add a 'mhavocpe' driver to support playing of the updated ROM images.

Notes on mhavoc file updates

1. I had to move double the size of the Alpha Paged ROM, which is where the VECTOR Paged ROM was in the original drivers. 
I moved the VECTOR paged ROM up to 0x20000 (length 0x8000). Each of the game drivers had their Alpha ROM space increased from 0x20000 to 0x28000 because of this. 
On the non Promised End drivers, the big gap from 0x18000 to 0x1FFFFF is not used, but it works.

2. Because the Paged VECTOR ROM moved, I had to modify the avgdvg.cpp video driver too which changed from the old base of 0x18000 to 0x20000.

3. The machine driver had to be slightly modified to allow for 3 paging bits on the Alpha Paged ROM (max 7, previously max 3 which was 2 paging bits).

4. Based upon those changes, there are other changes, which Im not describing here. 