;***************************************************************
;* Major Havoc MainLine Program (BETA Processor)              *
;***************************************************************
    .title "Major Havoc BETA Processor"               
;*************************************************************** 

#locallabelchar "?"
#include "logic.ah"
#include "macros.ah"
#include "havoc.ah"

.module beta
#include "b_ram.ah"

    .org $8000
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_BL
	.byte MAJOR_VERSION,MINOR_VERSION

;#include ./tw_beta3d.asm
#include ./tw_beta3dobj.asm
	

chkb1    .chk $8000,$BFFF,IDENTIFIER_BL

#if $ > $BFFF \ .error "BETA LOW has extended outside of design size." \ #endif



	.nocodes		;So we dont have list file buffer overflows
	.fill $C000-*
	.codes
	
	.org $c000
;*****************************************************************
;* Alpha High ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_BH
	.byte MAJOR_VERSION,MINOR_VERSION
;*****************************************************************
#include ./tw_beta3dlib2.asm
#include ./tw_beta3dlib.asm
#include ./tw_betamain.asm





;********************************
;* Alpha High Checksum
;********************************
chkb2   .chk $C000,$FFFF,IDENTIFIER_BH

;******************************************
;* Don't forget to do the entry points!!! 
;******************************************	
	.org $fffa
	
		.word b_main
		.word b_main
		.word b_irq

	.end


.export comtestbase
	