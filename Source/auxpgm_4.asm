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


___ROMPAGE	= 4
;********************************************
;* Major Havoc Auxiliary Program Page 4     
;********************************************
	.module aux4
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P4
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL4."
;*****************************************************************
    
#include ./tw_spcmz.asm
;#include ./tw_hwstuff.asm		;Moved to Page 5
;#include ./tw_mazed2.asm       ;Moved to Page 7

	#if $ > $3FFF \ .error "ROM Page 4 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
    
;********************************
;* Page 4 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P4
;********************************

	.end
	
	