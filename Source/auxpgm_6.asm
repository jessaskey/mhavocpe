 .locallabelchar "?"
#include "logic.ah"
#include "macros.ah"
#include "vector.ah"
#include "havoc.ah"
#include "./exp/mh_vrom.exp"
#include "./exp/mh_alpha.exp"
;#include "./exp/auxpgm_0.exp"       ;For maze message indexes
#include "./exp/auxpgm_7.exp"       ;For extended maze data 


___ROMPAGE	= 6
;********************************************
;* Major Havoc Auxiliary Program Page 6 
;********************************************
	.module aux6
	.org $2000
;*****************************************************************
;* Paged ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_P6
	.byte MAJOR_VERSION,MINOR_VERSION
	.TEXT "OL6."
;*****************************************************************

#include ./tw_mazed.asm
;#include ./tw_sphere.asm       ;Moved to Page 1

	#if $ > $3FFF \ .error "ROM Page 6 has extended outside of design size." \ #endif
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
        
;********************************
;* Page 6 Checksum
;******************************** 
    .org $3FFF
    .chk $2000,$3FFF,IDENTIFIER_P6
;********************************

	.end

 
 ;Exports
.export ispeed,trtbll,trpa00,mazdsiz

;Export for tw_motion
.export oxybonus

;Exports for tw_maze which is now in main Alpha ROM space
.export mpod,mzocd,mztdal,mztd,outime,reacsz,mazsrc,mzdc,mzar,mztr,mzlg,mone,mtite
.export mlock,mtran,mhand,mclock,mboots,mcan,mzor,mkeyp,mzty,dynamic_base,mtok,mscstl
;Moved to Main ROM: drawm,copym,trapal,newmaze,newarrow,trap
;map,dotmap ;moved into primary rom space

;Exports for tw_motion
.export maz0,m0ua,m0ub,m0uc,m0ud,m0ue,m0uf,m0u1,m0u2,m0u3,m0u4,m0u5,m0u6,m0u7,m0u8,m0ulast
.export maz1,m1ua,m1ub,m1uc,m1ud,m1ue,m1uf,m1u1,m1u2,m1u3,m1u4,m1u5,m1u6,m1u7,m1u8,m1u9,m1ulast
.export maz2,m2ua,m2ub,m2uc,m2ud,m2ue,m2uf,m2u1,m2u2,m2u3,m2u4,m2u5,m2u6,m2u7,m2u8,m2u9,m2u10,m2ulast
.export maz3,m3ua,m3ub,m3uc,m3ud,m3ue,m3uf,m3u1,m3u2,m3u3,m3u4,m3u5,m3u6,m3u7,m3u8,m3u9,m3u10,m3u11,m3u12,m3ulast
