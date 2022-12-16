 .locallabelchar "?"
#include "logic.ah"
#include "macros.ah"
#include "vector.ah"
#include "havoc.ah"
#include "./exp/mh_vrom.exp"
#include "./exp/mh_alpha.exp"
#include "./exp/mh_gamma.exp"

;For Message Calls
#include "./exp/auxpgm_0.exp"

___ROMPAGE	= 2
;********************************************
;* Major Havoc Auxiliary Program Page 2
;********************************************
	.module aux2
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P2
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL2."
;*****************************************************************

#include tw_mazeg.asm
#include tw_mazeo.asm

	#if $ > $3FFF \ .error "ROM Page 2 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
    
;********************************
;* Page 2 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P2
;********************************
    .end
        
.export init2,scbinit,gminit,react,sparks,posmo,locate,drpod2,movtng2,anio2,maxsound,stashkeys,unstashkeys


