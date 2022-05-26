This file will walk through the steps required to create playable ROM images 
for Major Havoc - The Promised End

NOTE: Patch files were used for the early BETA versions of the ROMs up through and including version 0.75. After that, the ROM naming for BETA ROM's are now driven by the HBMAME driver that matches their CSUM. At this point, the newer ROM files are complete AS-IS and no longer need to be generated via patching.



IF YOU ARE MAKING ROMs for versions 0.75 and earlier, then follow instructions below. Later versions DO NOT NEED TO DO THIS!

DISCLAIMER: The files on this site are patch files which will modify your existing ROM image backups into ROMs that play 
'THE PROMISED END'. I am not allowed to distribute Atari ROM images directly, so you will need to supply your own images.
These steps will apply patches to your ROM images to have updated features in 'THE PROMISED END'.

Instructions:

1. Create a working folder for all of these files, they all go in the same location.
2. You will need Major Havoc Version 3 ROM images to start, these are numbered .106,.107,.108,.210,.215,.216,.217 and .318
3. Download the bspatch.exe (part of bsdiff) file or get a copy from the internet for your platform - Im running Windows 10 64-bit.
4. Run the 'patch_roms.bat' file or manually apply the patches yourself.
5. You should now have 10 new files named 'mhpe.*' where the * is the PCB location of the file.
6. These ROM's can now be burned on your PCB (with modifications) or tested in MAME.
7. NOTE: The two ROM's at location 1B/C and 1D are *NOT REQUIRED* to be stuffed in your PCB and can be IGNORED currently.

The patching process uses the bsdiff compression tool which more info is available here - https://www.romhacking.net/utilities/929/ 

Reference: http://www.daemonology.net/papers/bsdiff.pdf 
