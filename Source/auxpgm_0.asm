 .locallabelchar "?"
#include "logic.ah"
#include "macros.ah"
#include "vector.ah"
#include "havoc.ah"
#include "./exp/mh_gamma.exp"
#include "./exp/mh_vrom.exp"
#include "./exp/mh_alpha.exp"
#include "./exp/auxpgm_7.exp"   ;For Maze Messages

___ROMPAGE	= 0
;********************************************
;* Major Havoc Auxiliary Program Page 0     *
;********************************************
	.module aux0
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P0
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL0."
;*****************************************************************
#include ./tw_tbls.asm
#include ./tw_story.asm 
#include ./tw_messd.asm   

    
	#if $ > $3FFF \ .error "ROM Page 0 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*

;********************************
;* Page 0 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P0
;********************************

   
	.end
	








