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

___ROMPAGE	= 5
;********************************************
;* Major Havoc Auxiliary Program Page 5  
;********************************************
	.module aux5
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P5
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL5."
;*****************************************************************

#include ./tw_tact.asm
#include ./tw_hwstuff.asm

	#if $ > $3FFF \ .error "ROM Page 5 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
       
;********************************
;* Page 5 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P5
;********************************
    .end
	
.export disptact,warpcol

;for self test 
.export wdigits

.export _stelevatordn,_strex,_stscroll,hwstat