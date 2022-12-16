 .locallabelchar "?"
#include "logic.ah"
#include "macros.ah"
#include "vector.ah"
#include "havoc.ah"
#include "./exp/mh_vrom.exp"
#include "./exp/mh_alpha.exp"
#include "./exp/mh_gamma.exp"

#include "./exp/auxpgm_0.exp"	;For Message Calls

___ROMPAGE	= 3
;********************************************
;* Major Havoc Auxiliary Program Page 3
;********************************************
	.module aux3
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P3
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL3."
;*****************************************************************

#include ./tw_sfort.asm
#include ./tw_castle.asm
#include ./tw_emote.asm

	#if $ > $3FFF \ .error "ROM Page 3 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
    
;********************************
;* Page 3 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P3
;********************************

	.end
	
.export enem3,enem4,drawshield,ckshield,sc_state_fight,emote
;.export ckshieldh