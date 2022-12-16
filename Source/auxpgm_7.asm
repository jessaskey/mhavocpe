 .locallabelchar "?"
#include "logic.ah"
;#include "macros.ah"
;#include "vector.ah"
#include "havoc.ah"
;#include "./exp/mh_vrom.exp"
#include "./exp/mh_alpha.exp"


___ROMPAGE	= 7
;********************************************
;* Major Havoc Auxiliary Program Page 7     
;********************************************
	.module aux7
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P7
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL7."
;*****************************************************************

;#include ./tw_spcmz.asm        ;Moved to Page 4
#include ./tw_mazed2.asm

#if $ > $3FFF \ .error "ROM Page 7 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
    
;********************************
;* Page 7 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P7
;********************************
	
    .end



	.export mzinit
