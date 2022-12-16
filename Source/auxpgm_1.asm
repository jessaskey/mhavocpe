 .locallabelchar "?"
#include "logic.ah"
#include "macros.ah"
#include "vector.ah"
#include "havoc.ah"
#include "./exp/mh_vrom.exp"
#include "./exp/mh_alpha.exp"
#include "./exp/mh_gamma.exp"
#include "./exp/auxpgm_0.exp"   ;For Messages

___ROMPAGE	= 1
;********************************************
;* Major Havoc Auxiliary Program Page 1     
;********************************************
	.module aux1
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P1
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL1."
;*****************************************************************
#include ./tw_select.asm        
#include ./tw_sphere.asm 
#include ./tw_test2.asm
	
	#if $ > $3FFF \ .error "ROM Page 1 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
    
;********************************
;* Page 1 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P1
;********************************
    .end
	
.export enem0,adddis
