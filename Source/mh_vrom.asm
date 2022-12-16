#include "vector.ah"
#include "macros.ah"
#include "logic.ah"
#include "havoc.ah"
#include "a_def.ah"

.module vrom
#include "./exp/mh_vrom0.exp"
#include "./exp/mh_vrom1.exp"
#include "./exp/mh_vrom2.exp"
#include "./exp/mh_vrom3.exp"



;********************************************
;* Major Havoc Vector ROM                   *
;* Copyright 1983 Atari                     *
;* Transcribed by Jess M. Askey 2000        *
;********************************************
	.title "TWVectors - Vector Tables"
	
	
		.org $5000	
;*****************************************************************
;* Vector ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_VR
	.byte MAJOR_VERSION,MINOR_VERSION
;*****************************************************************
		
_lastchk 	.equ	$

char_a		vctr(0d,12d,visible)
            vctr(12d,12d,visible)
            vctr(12d,0d,visible)
            vctr(0d,-24d,visible)
            vctr(-24d,12d,hidden)
            vctr(24d,0d,visible)
char_       vctr(8d,-12d,hidden)
            rtsl
            ;.db $20,$46,$26,$46,$26,$40,$20,$54,$14,$46,$2C,$40,$04,$5A,$00,$C0	
            
char_space	vctr(32d,0d,hidden)
            rtsl
            ;.db $00,$00,$20,$00,$00,$C0

char_b      vctr(0d,24d,visible)
            vctr(18d,0d,visible)
            vctr(6d,-6d,visible)
            vctr(0d,-18d,visible)
            vctr(-24d,0d,visible)
            vctr(0d,12d,hidden)
            vctr(24d,0d,visible)
            jmpl(char_)
            ;.db $20,$4C,$29,$40,$23,$5D,$20,$57,$34,$40,$00,$46,$2C,$40,$06,$E8

char_c      vctr(24d,0d,hidden)
            vctr(-12d,0d,visible)
            vctr(-12d,12d,visible)
            vctr(6d,12d,visible)
            vctr(18d,0d,visible)
            vctr(8d,-24d,hidden)
            rtsl
            ;.db $0C,$40,$3A,$40,$3A,$46,$23,$46,$29,$40,$04,$54,$00,$C0
            
char_d		vctr(0d,24d,visible)
            vctr(18d,0d,visible)
            vctr(6d,-12d,visible)
            vctr(-6d,-12d,visible)
            vctr(-18d,0d,visible)
            vctr(32d,0d,hidden)
            rtsl
            
char_e		vctr(24d,0d,visible)
            vctr(-24d,0d,hidden)
char_f      vctr(0d,24d,visible)
            vctr(24d,0d,visible)
            vctr(-6d,-12d,hidden)
            vctr(-18d,0d,visible)
            vctr(32d,-12d,hidden)
            rtsl
            
char_g		vctr(6d,0d,hidden)
            vctr(-6d,12d,visible)
            vctr(6d,12d,visible)
            vctr(18d,0d,visible)
            vctr(-12d,-12d,hidden)
            vctr(12d,0d,visible)
            vctr(0d,-12d,visible)
            vctr(-18d,0d,visible)
            vctr(26d,0d,hidden)
            rtsl
            ;.db $03,$40,$3D,$46,$23,$46,$29,$40,$1A,$5A,$26,$40,$20,$5A,$37,$40,$0D,$40,$00,$C0
            
char_h		vctr(0d,24d,visible)
            vctr(0d,-12d,hidden)
            vctr(24d,0d,visible)
            vctr(0d,12d,hidden)
            vctr(0d,-24d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $20,$4C,$00,$5A,$2C,$40,$00,$46,$20,$54,$04,$40,$00,$C0
            
char_i		vctr(4d,0d,hidden)
            vctr(16d,0d,visible)
            vctr(-8d,0d,hidden)
            vctr(0d,24d,visible)
            vctr(-8d,0d,hidden)
            vctr(16d,0d,visible)
            vctr(12d,-24d,hidden)
            rtsl
            ;.db $02,$40,$28,$40,$1C,$40,$20,$4C,$1C,$40,$28,$40,$06,$54,$00,$C0	
            
char_j		vctr(4d,6d,hidden)
            vctr(12d,-6d,visible)
            vctr(6d,6d,visible)
            vctr(0d,18d,visible)
            vctr(10d,-24d,hidden)
            rtsl
            ;.db $02,$43,$26,$5D,$23,$43,$20,$49,$05,$54,$00,$C0
            
char_k		vctr(0d,24d,visible)
            vctr(0d,-12d,hidden)
            vctr(18d,0d,visible)
            vctr(6d,12d,visible)
            vctr(-6d,-12d,hidden)
            vctr(6d,-12d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $20,$4C,$00,$5A,$29,$40,$23,$46,$1D,$5A,$23,$5A,$04,$40,$00,$C0
            
char_l		vctr(0d,24d,visible)
            vctr(0d,-24d,hidden)
            vctr(24d,0d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $20,$4C,$00,$54,$2C,$40,$04,$40,$00,$C0		
            
char_m		vctr(0d,24d,visible)
            vctr(12d,-12d,visible)
            vctr(12d,12d,visible)
            vctr(0d,-24d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $20,$4C,$26,$5A,$26,$46,$20,$54,$04,$40,$00,$C0
            
char_n		vctr(0d,24d,visible)
            vctr(24d,-24d,visible)
            vctr(0d,24d,visible)
            vctr(8d,-24d,hidden)
            rtsl
            ;.db $20,$4C,$2C,$54,$20,$4C,$04,$54,$00,$C0
            
char_q		vctr(18d,10d,hidden)
            vctr(10d,-10d,visible)
            vctr(-28d,0d,hidden)
char_o      vctr(18d,0d,hidden)
            vctr(6d,12d,visible)
            vctr(-6d,12d,visible)
char_o1     vctr(-12d,0d,visible)
            vctr(-6d,-12d,visible)
            vctr(6d,-12d,visible)
            vctr(12d,0d,visible)
            vctr(14d,0d,hidden)
            rtsl
            ;.db $09,$45,$25,$5B,$12,$40
            ;.db $09,$40,$23,$46,$3D,$46
            ;.db $3A,$40,$3D,$5A,$23,$5A,$26,$40,$07,$40,$00,$C0
            
char_r		vctr(24d,0d,hidden)
            vctr(-6d,10d,visible)
            vctr(-16d,-10d,hidden)
char_p      vctr(0d,24d,visible)
            vctr(18d,0d,visible)
            vctr(6d,-8d,visible)
            vctr(-8d,-6d,visible)
            vctr(-16d,0d,visible)
            vctr(32d,-10d,hidden)
            rtsl
            ;.db $0C,$40,$3D,$45,$18,$5B
            ;.db $20,$4C,$29,$40,$23,$5C,$3C,$5D,$38,$40,$F6,$1F,$20,$00,$00,$C0
            
char_s		vctr(18d,0d,visible)
            vctr(6d,6d,visible)
            vctr(-24d,12d,visible)
            vctr(6d,6d,visible)
            vctr(12d,0d,visible)
            vctr(14d,-24d,hidden)
            rtsl
            ;.db $29,$40,$23,$43,$34,$46,$23,$43,$26,$40,$07,$54,$00,$C0
            
char_t		vctr(12d,0d,hidden)
            vctr(0d,24d,visible)
            vctr(-12d,0d,hidden)
            vctr(24d,0d,visible)
            vctr(8d,-24d,hidden)
            rtsl
            ;.db $06,$40,$20,$4C,$1A,$40,$2C,$40,$04,$54,$00,$C0
            
char_u		vctr(0d,6d,hidden)
            vctr(0d,18d,visible)
            vctr(24d,0d,hidden)
            vctr(0d,-18d,visible)
            vctr(-12d,-6d,visible)
            vctr(-12d,6d,visible)
            vctr(32d,-6d,hidden)
            rtsl
            ;.db $00,$43,$20,$49,$0C,$40,$20,$57,$3A,$5D,$3A,$43,$FA,$1F,$20,$00,$00,$C0
            
char_v		vctr(4d,24d,hidden)
            vctr(16d,-24d,visible)
            vctr(0d,24d,visible)
            vctr(12d,-24d,hidden)
            rtsl
            ;.db $02,$4C,$28,$54,$20,$4C,$06,$54,$00,$C0
            
char_w		vctr(0d,24d,hidden)
            vctr(12d,-24d,visible)
            vctr(0d,24d,visible)
            vctr(12d,-24d,visible)
            vctr(0d,24d,visible)
            vctr(8d,-24d,hidden)
            rtsl
            ;.db $00,$4C,$26,$54,$20,$4C,$26,$54,$20,$4C,$04,$54,$00,$C0
            
char_x		vctr(24d,24d,visible)
            vctr(-24d,0d,hidden)
            vctr(24d,-24d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $2C,$4C,$14,$40,$2C,$54,$04,$40,$00,$C0
            
char_y		vctr(0d,24d,hidden)
            vctr(16d,-12d,visible)
            vctr(8d,12d,hidden)
            vctrl(-16d,-24d,visible)
            vctr(24d,0d,hidden)
            rtsl
            ;.db $00,$4C,$28,$5A,$04,$46,$E8,$1F,$F0,$3F,$0C,$40,$00,$C0
            
char_z		vctr(0d,24d,hidden)
            vctr(24d,0d,visible)
            vctr(-24d,-24d,visible)
            vctr(24d,0d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $00,$4C,$2C,$40,$34,$54,$2C,$40,$04,$40,$00,$C0
            
char_1		vctr(12d,0d,hidden)
            vctr(0d,24d,visible)
            vctr(20d,-24d,hidden)
            rtsl
            ;.db $06,$40,$20,$4C,$0A,$54,$00,$C0
            
char_2		vctr(0d,24d,hidden)
            vctr(24d,0d,visible)
            vctr(0d,-12d,visible)
            vctr(-24d,0d,visible)
            vctr(0d,-12d,visible)
            vctr(24d,0d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $00,$4C,$2C,$40,$20,$5A,$34,$40,$20,$5A,$2C,$40,$04,$40,$00,$C0
            
char_8		vctr(0d,24d,visible)
            vctr(0d,-24d,hidden)
char_3      vctr(0d,12d,hidden)
            vctr(24d,0d,visible)
            vctr(-24d,12d,hidden)
            vctr(24d,0d,visible)
            vctr(0d,-24d,visible)
            vctr(-24d,0d,visible)
            vctr(32d,0d,hidden)
            rtsl
            ;.db $20,$4C,$00,$54
            ;.db $00,$46,$2C,$40,$14,$46,$2C,$40,$20,$54,$34,$40,$00,$00,$20,$00,$00,$C0
            
char_9		vctr(24d,0d,visible)
            vctr(0d,24d,hidden)
            vctr(-24d,0d,visible)
            vctr(0d,-24d,hidden)
char_4      vctr(0d,24d,hidden)
            vctr(0d,-12d,visible)
            vctr(24d,0d,visible)
            vctr(0d,12d,hidden)
            vctr(0d,-24d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $2C,$40,$00,$4C,$34,$40,$00,$54
            ;.db $00,$4C,$20,$5A,$2C,$40,$00,$46,$20,$54,$04,$40,$00,$C0
            
char_6		vctr(0d,12d,visible)
            vctr(0d,-12d,hidden)
char_5      vctr(24d,0d,visible)
            vctr(0d,12d,visible)
            vctr(-24d,0d,visible)
            vctr(0d,12d,visible)
            vctr(24d,0d,visible)
            vctr(8d,-24d,hidden)
            rtsl
            ;.db $20,$46,$00,$5A
            ;.db $2C,$40,$20,$46,$34,$40,$20,$46,$2C,$40,$04,$54,$00,$C0
            
char_0		vctr(0d,24d,hidden)
            vctr(0d,-24d,visible)
            vctr(24d,0d,visible)
            vctr(-24d,0d,hidden)
char_7      vctr(0d,24d,hidden)
            vctr(24d,0d,visible)
            vctr(0d,-24d,visible)
            vctr(8d,0d,hidden)
            rtsl
            ;.db $00,$4C,$20,$54,$2C,$40,$14,$40
            ;.db $00,$4C,$2C,$40,$20,$54,$04,$40,$00,$C0

			;**********************************************************
			;Why is this here? Dunno really... can it be removed?
			;**********************************************************
			jsrl(char_space)
			jsrl(char_space)
			jsrl(char_space)
			vstat(sparkle_off,xflip_off,vpage0,$1,colred2)
			jsrl(havoc1)
			jsrl(char_space)
			jsrl(char_space)
			jsrl(char_space)
			jsrl(char_space)
			vstat(sparkle_off,xflip_off,vpage0,$E,colgreen)
            rtsl
			
char_percent	
            vctr(0d,18d,hidden)
            vctr(0d,6d,visible)
            vctr(6d,0d,visible)
            vctr(0d,-6d,visible)
            vctr(-6d,0d,visible)
            vctr(24d,6d,hidden)
            vctr(-24d,-24d,visible)
            vctr(18d,0d,hidden)
            vctr(6d,0d,visible)
            vctr(0d,6d,visible)
            vctr(-6d,0d,visible)
            vctr(0d,-6d,visible)
            vctr(14d,0d,hidden)
            rtsl
            ;.db $00,$49,$20,$43,$23,$40,$20,$5D,$3D,$40,$0C,$43,$34,$54,$09,$40,$23,$40,$20,$43,$3D,$40,$20,$5D,$07,$40,$00,$C0
            
char_colon	vctr(12d,0d,hidden)
            vctr(0d,6d,visible)
            vctr(6d,0d,visible)
            vctr(0d,-6d,visible)
            vctr(-6d,0d,visible)
            vctr(0d,12d,hidden)
            vctr(0d,6d,visible)
            vctr(6d,0d,visible)
            vctr(0d,-6d,visible)
            vctr(-6d,0d,visible)
            vctr(20d,-12d,hidden)
            rtsl
            ;.db $06,$40,$20,$43,$23,$40,$20,$5D,$3D,$40,$00,$46,$20,$43,$23,$40,$20,$5D,$3D,$40,$0A,$5A,$00,$C0
            
char_dash   vctr(0d,14d,hidden)
            vctr(24d,0d,visible)
            vctr(8d,-14d,hidden)
            rtsl
            ;.db $00,$47,$2C,$40,$04,$59,$00,$C0
            
char_comma	jsrl(char_dot)
            vctr(4d,0d,hidden)
            vctr(-8d,-14d,visible)
            vctr(28d,12d,hidden)
            rtsl
            ;.db $12,$A9,$02,$40,$3C,$59,$0E,$46,$00,$C0
            
char_period jsrl(char_dot)
            vctr(24d,-2d,hidden)
            rtsl
            ;.db $12,$A9,$0C,$5F,$00,$C0
            
char_dot	vctr(8d,2d,hidden)
            vctr(4d,0d,visible)
            vctr(-4d,0d,visible)
            rtsl
            ;.db $04,$41,$22,$40,$3E,$40,$00,$C0
            
char_excla	jsrl(char_dot)
            vctr(2d,6d,hidden)
            vctr(2d,16d,visible)
            vctr(-4d,0d,visible)
            vctr(2d,-16d,visible)
            vctr(22d,-8d,hidden)
            rtsl
            ;.db $12,$A9,$01,$43,$21,$48,$3E,$40,$21,$58,$0B,$5C,$00,$C0
            
char_half	vctr(0d,12d,hidden)
            vctr(0d,12d,visible)
            vctr(24d,0d,hidden)
            vctr(-24d,-24d,visible)
            vctr(24d,0d,hidden)
            vctr(-6d,0d,visible)
            vctr(0d,6d,visible)
            vctr(6d,0d,visible)
            vctr(0d,6d,visible)
            vctr(-6d,0d,visible)
            vctr(14d,-12d,hidden)
            rtsl
            ;.db $00,$46,$20,$46,$0C,$40,$34,$54,$0C,$40,$3D,$40,$20,$43,$23,$40,$20,$43,$3D,$40,$07,$5A,$00,$C0
			
						
char_oparen vctr(18d,24d,hidden)
            vctr(-4d,-8d,visible)
            vctr(0d,-8d,visible)
            vctr(4d,-8d,visible)
            vctr(6d,0d,hidden)
            rtsl
            ;.db $00,$46,$23,$46,$26,$40,$20,$5A,$3D,$40,$03,$40,$20,$5A,$04,$40,$00,$C0
            
char_cparen vctr(4d,24d,hidden)
            vctr(4d,-8d,visible)
            vctr(0d,-8d,visible)
            vctr(-4d,-8d,visible)
            vctr(20d,0d,hidden)
            rtsl
            ;.db $26,$40,$29,$4C,$11,$40,$26,$40,$29,$54,$04,$40,$00,$C0
          
; char_hexa   vctr(0d,12d,hidden)
            ; vctr(6d,12d,visible)
            ; vctr(18d,0d,visible)
            ; vctr(6d,-12d,visible)
            ; vctr(-6d,-12d,visible)
            ; vctr(-18d,0d,visible)
            ; vctr(-6d,12d,visible)
            ; vctr(32d,-12d,hidden)
            ; rtsl

char_fslash	vctr(6d,0d,hidden)
            vctr(12d,24d,visible)
            vctr(6d,-24d,hidden)
            rtsl
            ;.db $00,$4C,$2C,$40,$17,$40,$20,$54,$0D,$40,$00,$C0

;@ Symbol     
char_at 	vctr(16,8,hidden)
			vctr(0,8,visible)
			vctr(-8,0,visible)
			vctr(0,-8,visible)
			vctr(16,0,visible)
			vctr(0,16,visible)
			vctr(-24,0,visible)
			vctr(0,-24,visible) 
			vctr(24,0,visible)
			vctr(4,0,hidden)
			rtsl
			
;char_phi    vctr(0d,12d,hidden)
;            vctr(12d,6d,visible)
;            vctr(6d,-6d,visible)
;            vctr(-6d,-6d,visible)
;            vctr(-12d,6d,visible)
;            vctr(12d,12d,hidden)
;            vctr(0d,-24d,visible)
;            vctr(14d,0d,hidden)
;            rtsl
            ;.db $00,$46,$26,$43,$23,$5D,$3D,$5D,$3A,$43,$06,$46,$20,$54,$07,$40,$00,$C0

;Right Arrow - Pointing Right            
char_rarr	vctr(0d,12d,hidden)
            vctr(24d,0d,visible)
            vctr(-12d,12d,visible)
			vctr(12d,-12d,hidden)
            vctr(-12d,-12,visible)
            vctr(12d,0d,hidden)
            rtsl
			
;Left Triangle - Pointing Right           
char_ltri	vctr(2d,0d,hidden)
            vctr(0d,24d,visible)
            vctr(18d,-12d,visible)
            vctr(-18d,-12d,visible)
            vctr(24d,0,hidden)
            rtsl
            ;.db $00,$46,$29,$46,$20,$54,$37,$46,$0D,$5A,$00,$C0

;Double Headed Arrow - REMOVED NOT USED        
; char_doubar	vctr(6d,0d,hidden)
            ; vctr(-6d,12d,visible)
            ; vctr(6d,12d,visible)
            ; vctr(-6d,-12d,hidden)
            ; vctr(30d,0d,visible)
            ; vctr(-6d,12d,hidden)
            ; vctr(6d,-12d,visible)
            ; vctr(-6d,-12d,visible)
            ; vctr(14d,0d,hidden)
            ; rtsl
            ; ;.db $03,$40,$3D,$46,$23,$46,$1D,$5A,$2F,$40,$1D,$46,$23,$5A,$3D,$5A,$07,$40,$00,$C0

char_quest	vctr(2d,18d,hidden)
            vctr(6d,6d,visible)
            vctr(6d,0d,visible)
            vctr(6d,-6d,visible)
            vctr(0d,-4d,visible)
			vctr(-8d,-4d,visible)
			vctr(0d,-4d,visible)
			vctr(-2d,-6d,hidden)
			vctr(4d,0d,visible)
            vctr(-4d,0d,visible)
			vctr(16d,0d,hidden)
            rtsl
	
char_copy	vscal(ywin_off,binscal1,$60)
            jsrl(char_o)
            vctr(-27d,6d,hidden)
            vscal(ywin_off,binscal2,$60)
            jsrl(char_c)
            vscal(ywin_off,binscal2,$30)
            vctr(16d,-6d,hidden)
            rtsl           

char_quot 	vctr(6d,24d,hidden)
            vctr(0d,-8d,visible)
            vctr(8d,-16d,hidden)
            rtsl     

;Acute accent - example É
acc_acute	vctr(10d,28d,hidden)
			vctr(4d,4d,visible)
			vctr(-14d,-32d,hidden)
			rtsl 

;Grave accent - example È			
acc_grave	vctr(16d,32d,hidden)
			vctr(4d,-4d,visible)
			vctr(-20d,-28d,hidden)
			rtsl

;Circumflex accent - example Ê			
acc_circ	vctr(8d,28d,hidden)
			vctr(4d,4d,visible)
			vctr(4d,-4d,visible)
			vctr(-16d,-28d,hidden)
			rtsl
			
;Tilde - not needed yet
;acc_tilde

;Umlaut accent - example Ï	
acc_umlaut	vctr(8d,30d,hidden)
			vctr(2d,0d,visible)
			vctr(4d,0d,hidden)
			vctr(2d,0d,visible)
			vctr(-16d,-30d,hidden)
			rtsl


;Grave E - È
char_e1		jsrl(acc_grave)
			jmpl(char_e)
			
;Acute E - É
char_e2		jsrl(acc_acute)
			jmpl(char_e)

;Circumflex E - Ê
char_e3		jsrl(acc_circ)
			jmpl(char_e)

;Grave A - À
char_a1		jsrl(acc_grave)
			jmpl(char_a)		

;Circumflex A - Â
char_a2		jsrl(acc_circ)
			jmpl(char_a)

;Umlaut A - Ä
char_a3		jsrl(acc_umlaut)
			jmpl(char_a)
			
;Grave U - Ù
char_u1		jsrl(acc_grave)
			jmpl(char_u)
			
;Circumflex U - Û	
char_u2		jsrl(acc_circ)
			jmpl(char_u)

;Umlaut U - Ü				
char_u3		jsrl(acc_umlaut)
			jmpl(char_u)
			
;Umlaut I - Ï	
char_i1		jsrl(acc_umlaut)
			jmpl(char_i)
			
;Circumflex O - Ô	
char_o2		jsrl(acc_circ)
			jmpl(char_o)
			
;Umlaut O - Ö
char_o3		jsrl(acc_umlaut)
			jmpl(char_o)
			

			
			
lifech		vctr(2d,0d,hidden)
            vctr(6d,24d,visible)
            vctr(-8d,-8d,visible)
            vctr(4d,12d,visible)
            vctr(6d,3d,visible)
            vctr(-2d,1d,visible)
            vctr(0d,5d,visible)
            vctr(4d,2d,visible)
            vctr(4d,-2d,visible)
            vctr(0d,-5d,visible)
            vctr(-2d,-1d,visible)
            vctr(6d,-3d,visible)
            vctr(4d,-12d,visible)
            vctr(-8d,8d,visible)
            vctr(6d,-24d,visible)
            vctr(-7d,0d,visible)
            vctr(-3d,12d,visible)
            vctr(-3d,-12d,visible)
            vctr(-7d,0d,visible)
            vctr(30d,0d,hidden)
            rtsl
			
qmark		vctr(12d,0d,hidden)
            vctr(4d,0d,visible)
            vctr(0d,4d,visible)
            vctr(-4d,0d,visible)
            vctr(0d,-4d,visible)
            vctr(0d,8d,hidden)
            vctr(4d,0d,visible)
            vctr(0d,4d,visible)
            vctr(8d,8d,visible)
            vctr(0d,8d,visible)
            vctr(-8d,8d,visible)
            vctr(-8d,0d,visible)
            vctr(-8d,-8d,visible)
            vctr(4d,-4d,visible)
            vctr(4d,4d,visible)
            vctr(8d,0d,visible)
            vctr(0d,-8d,visible)
            vctr(-4d,-8d,visible)
            vctr(0d,-4d,visible)
            rtsl

__curmsgidx	= 0
;*******************************************************************************************
; Message Index Macro, will define a helper constant for each letter for indexing
; Example: regchar(char_space) will also define a variable called idxcchar_space which is 
; equal to 0 since this is the first char
;*******************************************************************************************
#define 	regchar(cptr)	\ jsrl(cptr)
#defcont					\idx+cptr = __curmsgidx
#defcont    				\__curmsgidx .set __curmsgidx+1

			
vgmsga
vgjchsp		regchar(char_space)		\.export idxchar_space
vgjch0		regchar(char_0)			\.export idxchar_0
vgjch1		regchar(char_1)			\.export idxchar_1
vgjch2		regchar(char_2)			\.export idxchar_2
vgjch3		regchar(char_3)			\.export idxchar_3
vgjch4		regchar(char_4)			\.export idxchar_4
vgjch5		regchar(char_5)			\.export idxchar_5
vgjch6		regchar(char_6)			\.export idxchar_6
vgjch7		regchar(char_7)			\.export idxchar_7
vgjch8		regchar(char_8)			\.export idxchar_8
vgjch9		regchar(char_9)			\.export idxchar_9
vgjcha		regchar(char_a)			\.export idxchar_a
vgjchb		regchar(char_b)			\.export idxchar_b
vgjchc		regchar(char_c)			\.export idxchar_c
vgjchd		regchar(char_d)			\.export idxchar_d
vgjche		regchar(char_e)			\.export idxchar_e
vgjchf		regchar(char_f)			\.export idxchar_f
vgjchg		regchar(char_g)			\.export idxchar_g
vgjchh		regchar(char_h)			\.export idxchar_h
vgjchi		regchar(char_i)			\.export idxchar_i
vgjchj		regchar(char_j)			\.export idxchar_j
vgjchk		regchar(char_k)			\.export idxchar_k
vgjchl		regchar(char_l)			\.export idxchar_l
vgjchm		regchar(char_m)			\.export idxchar_m
vgjchn		regchar(char_n)			\.export idxchar_n
vgjcho		regchar(char_o)			\.export idxchar_o
vgjchp		regchar(char_p)			\.export idxchar_p
vgjchq		regchar(char_q)			\.export idxchar_q
vgjchr		regchar(char_r)			\.export idxchar_r
vgjchs		regchar(char_s)			\.export idxchar_s    ;3A
vgjcht		regchar(char_t)			\.export idxchar_t	  ;3C
vgjchu		regchar(char_u)			\.export idxchar_u
vgjchv		regchar(char_v)			\.export idxchar_v
vgjchw		regchar(char_w)			\.export idxchar_w
vgjchx		regchar(char_x)			\.export idxchar_x
vgjchy		regchar(char_y)			\.export idxchar_y
vgjchz		regchar(char_z)			\.export idxchar_z
vgjchpe		regchar(char_period)	\.export idxchar_period
vgjchex		regchar(char_excla)		\.export idxchar_excla
vgjchda		regchar(char_dash)		\.export idxchar_dash
vgjchco		regchar(char_comma)		\.export idxchar_comma
vgjchpc		regchar(char_percent)	\.export idxchar_percent
vgjchcl		regchar(char_colon)		\.export idxchar_colon
vgjchhf		regchar(char_half)		\.export idxchar_half
vgjchop		regchar(char_oparen)	\.export idxchar_oparen
vgjchcp		regchar(char_cparen)	\.export idxchar_cparen
;vgjchhx		regchar(char_hexa)		\.export idxchar_hexa
vgjchfs		regchar(char_fslash)	\.export idxchar_fslash
vgjchat		regchar(char_at)		\.export idxchar_at
vgjchra     regchar(char_rarr)		\.export idxchar_rarr
vgjchlt		regchar(char_ltri)		\.export idxchar_ltri
vgjchqu		regchar(char_quest)		\.export idxchar_quest
vgjchcy		regchar(char_copy)		\.export idxchar_copy
vgjchqo		regchar(char_quot)		\.export idxchar_quot
vgjche1		regchar(char_e1)		\.export idxchar_e1
vgjche2		regchar(char_e2)		\.export idxchar_e2
vgjche3		regchar(char_e3)		\.export idxchar_e3
vgjcha1		regchar(char_a1)		\.export idxchar_a1
vgjcha2		regchar(char_a2)		\.export idxchar_a2
vgjchu1		regchar(char_u1)		\.export idxchar_u1
vgjchu2		regchar(char_u2)		\.export idxchar_u2
vgjchi1		regchar(char_i1)		\.export idxchar_i1
vgjcho1		regchar(char_o2)		\.export idxchar_o2
vgjchu3		regchar(char_u3)		\.export idxchar_u3
vgjcha3		regchar(char_a3)		\.export idxchar_a3
vgjcho3		regchar(char_o3)		\.export idxchar_o3

alphanum
		jsrl(char_space)		\.export idxchar_space
		jsrl(char_0)			\.export idxchar_0
		jsrl(char_1)			\.export idxchar_1
		jsrl(char_2)			\.export idxchar_2
		jsrl(char_3)			\.export idxchar_3
		jsrl(char_4)			\.export idxchar_4
		jsrl(char_5)			\.export idxchar_5
		jsrl(char_6)			\.export idxchar_6
		jsrl(char_7)			\.export idxchar_7
		jsrl(char_8)			\.export idxchar_8
		jsrl(char_9)			\.export idxchar_9
		jsrl(char_a)			\.export idxchar_a
		jsrl(char_b)			\.export idxchar_b
		jsrl(char_c)			\.export idxchar_c
		jsrl(char_d)			\.export idxchar_d
		jsrl(char_e)			\.export idxchar_e
		jsrl(char_f)			\.export idxchar_f
		jsrl(char_g)			\.export idxchar_g
		jsrl(char_h)			\.export idxchar_h
		jsrl(char_i)			\.export idxchar_i
		jsrl(char_j)			\.export idxchar_j
		jsrl(char_k)			\.export idxchar_k
		jsrl(char_l)			\.export idxchar_l
		jsrl(char_m)			\.export idxchar_m
		jsrl(char_n)			\.export idxchar_n
		jsrl(char_o)			\.export idxchar_o
		jsrl(char_p)			\.export idxchar_p
		jsrl(char_q)			\.export idxchar_q
		jsrl(char_r)			\.export idxchar_r
		jsrl(char_s)			\.export idxchar_s    ;3A
		jsrl(char_t)			\.export idxchar_t	  ;3C
		jsrl(char_u)			\.export idxchar_u
		jsrl(char_v)			\.export idxchar_v
		jsrl(char_w)			\.export idxchar_w
		jsrl(char_x)			\.export idxchar_x
		jsrl(char_y)			\.export idxchar_y
		jsrl(char_z)			\.export idxchar_z
		rtsl            ;this is here to end the alpha-numerics for the crosshatch test pattern

			
onearw		vctr(-44d,28d,visible)
            vctr(44d,28d,visible)
            vctr(124d,0d,visible)
            vctr(-28d,-28d,visible)
            vctr(28d,-28d,visible)
            vctr(-124d,0d,visible)
            rtsl
           
onevln		vctr(0d,48d,visible)
            vctr(24d,-48d,hidden)
            vctr(0d,48d,visible)
            rtsl
     
onearln     vctr(-4d,-128d,hidden)
            jsrl(onevln) ;$551E
            vctr(-60d,0d,hidden)
            jsrl(onearw)    ;$5508
            vctr(0d,104d,hidden)
            jsrl(onearw)    ;$5508
            vctr(36d,56d,hidden)
            jmpl(onevln)                       
            
onesigt		jsrl(char_o)
            jsrl(char_n)
            jsrl(char_e)
            vctr(-88d,-104d,hidden)
            jsrl(char_w)
            jsrl(char_a)
            jmpl(char_y)

onesigr		jsrl(onearln)   ;Arrows and Vert Lines
            vctr(-64d,-88d,hidden)
            rtsl

onesigl		jsrl(onearln)
            vctr(24d,-88d,hidden)
            rtsl
			
crosshatch	vstat(sparkle_off,xflip_off,vpage0,$C,colwhite)
            vctr(-1024d,0d,hidden)
            vctr(768d,-864d,visible)
            vctr(256d,288d,visible)
            vctr(-512d,576d,visible)
            vctr(-512d,-576d,visible)
            vctr(256d,-288d,visible)
            vctr(768d,864d,visible)
            vctr(0d,-864d,hidden)
            vctr(-768d,864d,visible)
            vctr(-256d,-288d,visible)
            vctr(512d,-576d,visible)
            vctr(512d,576d,visible)
            vctr(-256d,288d,visible)
            vctr(-768d,-864d,visible)
            vcntr
            vscal(ywin_off,binscal2,$20)
            vctr(-600d,-128d,hidden)
            jmpl(alphanum)            ;Shows the Alpha Numerics         
            vctr(1023d,0d,visible)
            vctr(0d,-2d,visible)
            vctr(-1024d,0d,visible)
            vctr(0d,-2d,visible)
            rtsl

			
mazet		jsrl(maze1)
			jsrl(maze2)
			jsrl(maze3)
			jsrl(maze4)
			jsrl(maze5)
			jsrl(maze6)
			jsrl(maze7)

mapet		jsrl(mape1)
			jsrl(mape2)
			jsrl(mape3)
			jsrl(mape4)
			jsrl(mape5)
			jsrl(mape6)
			jsrl(mape7)

;macro to define handy pic enums for each index
__curpicidx = 0;
#DEFINE 	addpic(picdata) \idx_+picdata = __curpicidx
#DEFCONT    				\__curpicidx .set __curpicidx+1
#DEFCONT					\ .dw ((((picdata)&$7fff)>>1)|$a000)
;#DEFCONT					\.export idx_+picdata   ; Have to do this manually below, macro doesn't support it currently

mansrc		addpic(pic0)      ;Stride Left
			addpic(pic1)      ;Recoil Left
			addpic(pic2)      ;Passing Left
			addpic(pic3)      ;High Right
			addpic(pic4)      ;Higher Right
			addpic(pic5)      ;Contact Right
			addpic(pic6)      ;Passing Right
			addpic(pic7)      ;High Left
			addpic(pic8)
			addpic(pic9)
			addpic(pic10)
			addpic(pic11)
			addpic(pic12)     ;Squat #1 (lowest)
			addpic(pic13)     ;Squat #2 (lower)
			addpic(pic14)     ;Squat #3 (higher)
			addpic(pic15)     ;Jump 
			addpic(pic16)     ;=pic14
			addpic(pic17)     ;=pic13
			addpic(pic18)     ;=pic14
			addpic(pic19)
			addpic(pic20)
			addpic(pic21)     ;Standing, Arms out
			addpic(pic22)     ;=pic20
			addpic(pic23)
			addpic(pic24)     ;=pic21
			addpic(pic25)     ;=pic19
			addpic(pic26)     ;=pic20
			addpic(pic27)     ;Waiting, arms crossed              
			addpic(pic28)     ;Leaning                
			addpic(pic29)     ;Smack Wall             
			addpic(pic30)     ;Fall Off Wall          
			addpic(pic31)     ;Sitting After Wall  
            addpic(pic8a)     ;Arm Extending
            addpic(pic8b)     ;Arm Extended

;these are all static pictures which are not in the sequences            
			addpic(pic32)     ;Choking #1 (Arms Higher)
			addpic(pic33)     ;Choking #1 (Arms Lower)  
			addpic(pic34)     ;Choking #2 (Arms Higher)  
			addpic(pic35)     ;Choking #2 (Arms Lower)   
			addpic(pic36)     ;Dead on ground           
			addpic(pic37)     ;Skeleton
			addpic(pic38)     ;Teeter
            addpic(pic21a)	  ;Elevator closing on Standing Rex
            addpic(pic21b)	  ;Elevator closing on Standing Rex
			addpic(pic21c)
			addpic(pic21d)
			addpic(pic21e)
			addpic(pic21f)
			
			;jsrl(pic39)     ;Not Used?
			;jsrl(pic40)     ;Not Used?
			;jsrl(pic41)     ;Not Used?
			
;floss pics			
			addpic(flossa)
			addpic(flossb)
			addpic(flossc)
			addpic(flossd)
			addpic(flosse)
			addpic(flossf)
			addpic(flossg)
			addpic(flossh)
			addpic(flossi)
			addpic(flossj)
			addpic(flossk)
			addpic(flossl)
			addpic(flossm)
;dab pics
			addpic(dab02)
			addpic(dab04)
			addpic(dab06)
			
rods		jsrl(rod0)
			jsrl(rod1)
			jsrl(rod2)
			jsrl(rod3)
			
bigrods		jsrl(bigrod0)
			jsrl(bigrod1)
			jsrl(bigrod2)
			jsrl(bigrod3)
			
heads		jsrl(head0)
			jsrl(head0)
			jsrl(head1)
			jsrl(head2)
			jsrl(head3)
			jsrl(head4)
			jsrl(head5)
			jsrl(head6)
			
tails		jsrl(tail0)
			jsrl(tail1)

guns		jsrl(gun0)
			jsrl(gun1)
			jsrl(gun2)
			jsrl(gun3)

eyes		jsrl(eye0)
			jsrl(eye1)
			jsrl(eye2)

cann		jsrl(mount)
			jsrl(lgun0)
			jsrl(lgun1)
			jsrl(lgun2)
			jsrl(lgun3)
			jsrl(brl00)
			jsrl(brl10)
			jsrl(brl20)
			jsrl(brl30)
			jsrl(brl01)
			jsrl(brl11)
			jsrl(brl21)
			jsrl(brl31)
			jsrl(brl02)
			jsrl(brl12)
			jsrl(brl22)
			jsrl(brl32)
			jsrl(laz00)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz30)
			jsrl(laz01)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz31)
			jsrl(laz02)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz32)
			jsrl(laz03)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz33)
			
maxheads    jsrl(maxhead0)
            jsrl(maxhead1)
            jsrl(maxhead2)
            jsrl(maxhead3)
            jsrl(maxhead4)
            jsrl(maxhead5)
            jsrl(maxhead6)
            jsrl(maxhead7)
            
maxbods     jsrl(maxbody0)
            jsrl(maxbody1)
            jsrl(maxbody2)
            jsrl(maxbody3)
            jsrl(maxbody4)
            jsrl(maxbody5)
            jsrl(maxbody6)
            jsrl(maxbody7)

maxeyes     jsrl(maxeye0)
            jsrl(maxeye1)
            jsrl(maxeye2)
            jsrl(maxeye3)
            jsrl(maxeye4)
            jsrl(maxeye5)
            jsrl(maxeye6)
            jsrl(maxeye7)

#IF MAX_LEVITATIONS != 0              
lvts		jsrl(levt0)
		    jsrl(levt1)
			jsrl(levt2)
			jsrl(levt3)
#ENDIF

;These are indeed 1 based, there is no zero index 
;and these are shifted on load so there is no dummy 
;record
planes		jsrl(plne1)		;Index $01: Flat - 5th rotation right
			jsrl(plne2)		;Index $02: Flat - 4th rotation right
			jsrl(plne3)		;Index $03: Flat - 3rd rotation right
			jsrl(plne4)		;Index $04: Flat - 2nd rotation right
			jsrl(plne5)		;Index $05: Flat - 1st rotation right
			jsrl(plne6)		;Index $06: Flat - Pointed forward/no rotation
			
			jsrl(plne8)		;Index $07: Pitched Right - 5th rotation right
			jsrl(plne9)		;Index $08: Pitched Right - 4th rotation right
			jsrl(plne10)	;Index $09: Pitched Right - 3rd rotation right
			jsrl(plne11)	;Index $0A: Pitched Right - 2nd rotation right
			jsrl(plne12)	;Index $0B: Pitched Right - 1st rotation right
			jsrl(plne13)	;Index $0C: Pitched Right - Pointed forward 
			
			jsrl(plne15)	;Index $0D: Pitched Left - 5th rotation right
			jsrl(plne16)	;Index $0E: Pitched Left - 4th rotation right
			jsrl(plne17)	;Index $0F: Pitched Left - 3rd rotation right
			jsrl(plne18)	;Index $10: Pitched Left - 2nd rotation right
			jsrl(plne19) 	;Index $11: Pitched Left - 1st rotation right
			jsrl(plne20)	;Index $12: Pitched Left - Pointed forward
			
			;1st to 3rd Person perspective transitions
			jsrl(plne21)	;Index $13: Coming Up - left to up 1
			jsrl(plne22)	;Index $14: Coming Up - left to up 2
			jsrl(plne23)	;Index $15: Coming Up - left to up 3 - almost vertical
			jsrl(plne24)	;Index $16: Straight Up
			jsrl(plne25)	;Pointed Up - lean left
			jsrl(plne26)	;Tube Pic
			
beapic		jsrl(beacn0)
			jsrl(beacn1)
			jsrl(beacn2)
			jsrl(beacn3)
			jsrl(beacn4)
			jsrl(beacn5)
			jsrl(beacn6)
			jsrl(beacn7)

;Launch Tube Triangles
movet		;Sequence - 1 Triangle
			jsrl(tri07)
			jsrl(tri17)
			jsrl(tri27)
			jsrl(tri37)
			;Sequence - 2 Triangles
			jsrl(tri06)
			jsrl(tri16)
			jsrl(tri26)
			jsrl(tri36)
			;Sequence - 3 Triangles
			jsrl(tri05)
			jsrl(tri15)
			jsrl(tri25)
			jsrl(tri35)
			;Sequence - 4 Triangles
			jsrl(tri04)
			jsrl(tri14)
			jsrl(tri24)
			jsrl(tri34)
			;Sequence - 5 Triangles
			jsrl(tri03)
			jsrl(tri13)
			jsrl(tri23)
			jsrl(tri33)
			;Sequence - 6 Triangles
			jsrl(tri02)
			jsrl(tri12)
			jsrl(tri22)
			jsrl(tri32)
			;Sequence - 7 Triangles
			jsrl(tri01)
			jsrl(tri11)
			jsrl(tri21)
			jsrl(tri31)
			;Sequence - 8 Triangles - Not used, these are rendered individually
			jsrl(tri00)
			jsrl(tri10)
			jsrl(tri20)
			jsrl(tri30)
			
;Landing View of Space Stations						
bases		jsrl(st0)
			jsrl(st1)
			jsrl(st2)
			jsrl(st3)
			jsrl(st4)		;Final level CUBE

;Space Wave Play view of Space Stations
fbase		jsrl(ltlsh)
			jsrl(hexsh)
			jsrl(ftrsh)
			jsrl(dmbsh)
            jsrl(hwsh)
			jsrl(maynard)
			

enemys		jsrl(enmy0)
			jsrl(enmy1)
			jsrl(enmy2)
			jsrl(enmy3)
			jsrl(enmy4)
			jsrl(enmy5)
			jsrl(enmy6)
			jsrl(enmy7)
			jsrl(enmy8)
			
shtexp		jsrl(exp9)
			jsrl(exp10)
			jsrl(exp11)
			jsrl(exp12)
			jsrl(exp13)
			jsrl(exp14)
			jsrl(exp15)
			jsrl(exp16)
			
shtexpnc	jsrl(exp9+2)
			jsrl(exp10+2)
			jsrl(exp11+2)
			jsrl(exp12+2)
			jsrl(exp13+2)
			jsrl(exp14+2)
			jsrl(exp15+2)
			jsrl(exp16+2)

fexps		jsrl(fexp1)
			jsrl(fexp2)
			jsrl(fexp3)
			jsrl(fexp4)
			jsrl(fexp5)
			jsrl(fexp6)
			jsrl(fexp7)
			jsrl(fexp8)

;Mothership Explosion Pieces
bxp0s		jsrl(bx00)
			jsrl(bx01)
			jsrl(bx02)
			jsrl(bx03)
			jsrl(bx04)
			jsrl(bx05)
			jsrl(bx06)
			jsrl(bx07)
			
bxp1s		jsrl(bx10)
			jsrl(bx11)
			jsrl(bx12)
			jsrl(bx13)
			jsrl(bx14)
			jsrl(bx15)
			jsrl(bx16)
			jsrl(bx17)

; Player Ship Explosion Table
sexps		jsrl(sexp0)
			jsrl(sexp1)
			jsrl(sexp2)
			jsrl(sexp3)
			jsrl(sexp4)

;Ship Explosion Pieces
sxp0s		jsrl(pc00)
			jsrl(pc01)
			jsrl(pc02)
			jsrl(pc03)
			jsrl(pc04)
			jsrl(pc05)
			jsrl(pc06)
			jsrl(pc07)

sxp1s		jsrl(pc10)
			jsrl(pc11)
			jsrl(pc12)
			jsrl(pc13)
			jsrl(pc14)
			jsrl(pc15)
			jsrl(pc16)
			jsrl(pc17)

sxp2s		jsrl(pc20)
			jsrl(pc21)
			jsrl(pc22)
			jsrl(pc23)
			jsrl(pc24)
			jsrl(pc25)
			jsrl(pc26)
			jsrl(pc27)

sxp3s		jsrl(pc30)
			jsrl(pc31)
			jsrl(pc32)
			jsrl(pc33)
			jsrl(pc34)
			jsrl(pc35)
			jsrl(pc36)
			jsrl(pc37)

;Ship Explosion Pieces Lookup 
sxps		.word sxp0s
			.word sxp1s
			.word sxp2s
			.word sxp3s

;Smart Bomb Lookups
smtb		jsrl(smtb0)
			jsrl(smtb1)
			jsrl(smtb2)
			jsrl(smtb3)

mazarw		jsrl(rtarrow)
			jsrl(ltarrow)
			jsrl(uparrow)
			jsrl(dnarrow)
			jsrl(nearrow)
			jsrl(swarrow)
			jsrl(nwarrow)
			jsrl(searrow)
			jsrl(qmark)
			jsrl(outrw0)
			jsrl(outrw1)
			jsrl(outrw2)
			jsrl(outrw3)
mazoutwrd	jsrl(outwrd)
					
lightning	jsrl(ltng0)
			jsrl(ltng1)
			jsrl(ltng2)
			jsrl(ltng3)
			jsrl(ltng4)
			jsrl(ltng5)
			jsrl(ltng6)
			jsrl(ltng7)
			jsrl(ltng8)
			
			jsrl(ltng0x)
			jsrl(ltng1x)
			jsrl(ltng2x)
			jsrl(ltng3x)
			jsrl(ltng4x)
			jsrl(ltng5x)
			jsrl(ltng6x)
			jsrl(ltng7x)
			jsrl(ltng8x)
			
cerpup		jsrl(pupl00)
			jsrl(pupl10)
			jsrl(pupl20)
			jsrl(pupl30)
			jsrl(pupl40)
			jsrl(pupl50)

cerwng		jsrl(wing00)
			jsrl(wing01)
			jsrl(wing02)
			jsrl(wing03)
			jsrl(wing10)
			jsrl(wing11)
			jsrl(wing12)
			jsrl(wing13)
			jsrl(wing20)
			jsrl(wing21)
			jsrl(wing22)
			jsrl(wing23)
			jsrl(wing30)
			jsrl(wing31)
			jsrl(wing32)
			jsrl(wing33)
			jsrl(wing40)
			jsrl(wing41)
			jsrl(wing42)
			jsrl(wing43)
			jsrl(wing50)
			jsrl(wing51)
			jsrl(wing52)
			jsrl(wing53)
			
onesign		jsrl(onesigr)
			jsrl(onesigl)
			jsrl(onesigt)

newshot		jsrl(nwsht0)
			jsrl(nwsht1)
			jsrl(nwsht2)
			jsrl(nwsht3)
			jsrl(nwsht4)
			jsrl(nwsht5)
			jsrl(nwsht6)
			jsrl(nwsht7)

cerstf		jsrl(dod0)
			jsrl(dod1)
			jsrl(dod2)
			
cerbng		jsrl(bang0)
			jsrl(ngwi0)
			jsrl(ngwi1)
			jsrl(ngwi2)
			jsrl(ngwi3)
			jsrl(ngwi4)
			jsrl(ngwi5)
			jsrl(coil0)
			jsrl(coil1)
			jsrl(coil2)
			jsrl(coil3)
			jsrl(coil4)
			jsrl(coil5)
			jsrl(wdgt0)
			jsrl(wdgt1)
			jsrl(wdgt2)
			jsrl(wdgt3)

gclock		jsrl(sqr)
			jsrl(dial)

gboot		jsrl(shoes)
			jsrl(magic0)
			jsrl(magic1)
			jsrl(magic2)
			jsrl(magic3)
			jsrl(bootz1)
			; ;jsrl(bootz2)

gtite		jsrl(ovhng)

glock		jsrl(lock)
gkey		jsrl(key)
gkeyp		jsrl(keyp)

gpod		jsrl(epod1)
			jsrl(epod2)
			jsrl(epod3)
			jsrl(epod4)
			jsrl(epod5)
			jsrl(epod6)	
			
			jsrl(escpod)
			
			jsrl(crash0)			
			jsrl(crash1)
			jsrl(crash2)
			jsrl(crash3)
			jsrl(crash4)
			jsrl(crash5)
			jsrl(crash6)
			jsrl(crash7)
			jsrl(flame0)
			jsrl(flame1)
			jsrl(flame2)
			jsrl(flame3)
			jsrl(flame4)
			jsrl(flame5)
			jsrl(flame6)
			jsrl(flame7)
			jsrl(flame8)
			jsrl(flame9)
			jsrl(flamea)
			jsrl(flameb)
			jsrl(flamec)
			jsrl(flamed)
			jsrl(flamee)
			jsrl(flamef)

;NOT TECHNICALLY NEEDED ANYMORE, BUT WILL MAKE NEED TO REBURN ALL ROMS IF COMMENTED OUT			
gtran		jsrl(booth3)		;Main Transporter Body
			jsrl(booth4)		;Top and Bottom inside plates
			jsrl(booth5)		;Booth nuts

;Transporter Sparkles			
gtransp		jsrl(star0)
			jsrl(star1)
			jsrl(star2)
			jsrl(star3)

;Hand Parts
ghand		jsrl(hand)
			jsrl(box)
			jsrl(swtch0)
			jsrl(swtch1)

;Hidden maze tokens
tokens		jsrl(raditok)
			jsrl(startok)
			jsrl(cubetok)
			jsrl(fuzetok)	
			jsrl(hometok)
            
;Crosshair Vector Buffers, there are two
crsbufs 	jsrl(crsbuf)
			jsrl(crsbuf+$800)

;For each object, this is the JSRL to the appropriate crosshair		
objsr   	jsrl(crman)
			jsrl(crreac)
			jsrl(crfire)
			jsrl(crlsht)
			jsrl(crcann)
			jsrl(crrob)
			jsrl(crrob)			;for shots
			jsrl(crmax)
		
;*********************************************
;* End lookup Tables... Start RAW Data       *
;*********************************************

;****************************************
; Escape Pod Pics
;****************************************
epod6		vctr(56d,-104d,hidden)
            jsrl(char_e)
            vctr(-88d,104d,hidden)
            ;.db $98,$1F,$38,$00,$22,$A8,$68,$00,$A8,$1F
epod5	    vctr(56d,-72d,hidden)
            jsrl(char_p)
            vctr(-88d,72d,hidden)
            ;.db $B8,$1F,$38,$00,$71,$A8,$48,$00,$A8,$1F
epod4		vctr(56d,-40d,hidden)
            jsrl(char_a)
            vctr(-88d,40d,hidden)
            ;.db $D8,$1F,$38,$00,$00,$A8,$28,$00,$A8,$1F
epod3		vctr(56d,-8d,hidden)
            jsrl(char_c)
            vctr(-88d,8d,hidden)
            ;.db $F8,$1F,$38,$00,$13,$A8,$08,$00,$A8,$1F
epod2		vctr(56d,24d,hidden)
            jsrl(char_s)
            vctr(-88d,-24d,hidden)
            ;.db $18,$00,$38,$00,$79,$A8,$E8,$1F,$A8,$1F
epod1		vctr(56d,56d,hidden)
            jsrl(char_e)
            vctr(-88d,-56d,hidden)
            rtsl
			
			
;*********************************************	
; Main Logo Graphics	
;*********************************************
havoc1		vctr(240d,-22d,hidden)
            vctr(-8d,-14d,visible)
            vctr(-176d,0d,visible)
            vctr(12d,28d,visible)
            vctr(-16d,0d,visible)
            vctr(-12d,-28d,visible)
            vctr(-214d,0d,visible)
            vctr(18d,44d,visible)
            vctr(-28d,-44d,visible)
            vctr(-22d,0d,visible)
            vctr(10d,44d,visible)
            vctr(-22d,-44d,visible)
            vctr(-20d,0d,visible)
            vctr(38d,64d,visible)
            vctr(20d,0d,visible)
            vctr(-8d,-48d,visible)
            vctr(32d,48d,visible)
            vctr(20d,0d,visible)
            vctr(-20d,-50d,visible)
            vctr(186d,0d,visible)
            vctr(22d,50d,visible)
            vctr(16d,0d,visible)
            vctr(-10d,-24d,visible)
            vctr(16d,0d,visible)
            vctr(10d,24d,visible)
            vctr(16d,0d,visible)
            vctr(-22d,-50d,visible)
            vctr(162d,0d,visible)
            vctr(-240d,22d,hidden)
            rtsl
			
havoc2		vctr(-128d,-8d,hidden)
            vctr(4d,0d,visible)
            vctr(8d,16d,visible)
            vctr(-24d,-28d,visible)
            vctr(-12d,0d,visible)
            vctr(32d,40d,visible)
            vctr(20d,0d,visible)
            vctr(-12d,-40d,visible)
            vctr(-20d,0d,visible)
            vctr(28d,8d,hidden)
            vctr(4d,12d,visible)
            vctr(12d,0d,visible)
            vctr(-4d,-8d,visible)
            vctr(8d,0d,visible)
            vctr(12d,28d,visible)
            vctr(12d,0d,visible)
            vctr(-16d,-40d,visible)
            vctr(-24d,0d,visible)
            vctr(-4d,8d,visible)
            vctr(48d,32d,hidden)
            vctr(32d,0d,visible)
            vctr(-16d,-40d,visible)
            vctr(-32d,0d,visible)
            vctr(16d,40d,visible)
            vctr(8d,-12d,hidden)
            vctr(8d,0d,visible)
            vctr(-8d,-16d,visible)
            vctr(-8d,0d,visible)
            vctr(8d,16d,visible)
            vctr(28d,-28d,hidden)
            vctr(-14d,0d,visible)
            vctr(16d,40d,visible)
            vctr(34d,0d,visible)
            vctr(4d,-8d,visible)
            vctr(-16d,-12d,visible)
            vctr(12d,-20d,visible)
            vctr(-16d,0d,visible)
            vctr(-10d,20d,visible)
            vctr(10d,8d,visible)
            vctr(-8d,0d,visible)
            vctr(-12d,-28d,visible)
            vctr(128d,12d,hidden)
            vctr(4d,0d,visible)
            vctr(8d,16d,visible)
            vctr(-20d,-28d,visible)
            vctr(-14d,0d,visible)
            vctr(30d,40d,visible)
            vctr(20d,0d,visible)
            vctr(-12d,-40d,visible)
            vctr(-20d,0d,visible)
            vctr(36d,40d,hidden)
            vctr(14d,0d,visible)
            vctr(-8d,-28d,visible)
            vctr(22d,32d,visible)
            vctr(14d,0d,visible)
            vctr(-30d,-44d,visible)
            vctr(-24d,0d,visible)
            vctr(12d,40d,visible)
            vctr(40d,0d,hidden)
            vctr(32d,0d,visible)
            vctr(-16d,-40d,visible)
            vctr(-32d,0d,visible)
            vctr(16d,40d,visible)
            vctr(8d,-12d,hidden)
            vctr(8d,0d,visible)
            vctr(-8d,-16d,visible)
            vctr(-8d,0d,visible)
            vctr(8d,16d,visible)
            vctr(12d,-28d,hidden)
            vctr(16d,40d,visible)
            vctr(32d,0d,visible)
            vctr(-6d,-16d,visible)
            vctr(-12d,0d,visible)
            vctr(2d,4d,visible)
            vctr(-8d,0d,visible)
            vctr(-8d,-16d,visible)
            vctr(8d,0d,visible)
            vctr(2d,4d,visible)
            vctr(12d,0d,visible)
            vctr(-6d,-16d,visible)
            vctr(-32d,0d,visible)
            vctr(-196d,20d,hidden)
            rtsl
			
havoc3		vctr(-228d,-36d,hidden)
            vctr(32d,56d,visible)
            vctr(8d,0d,visible)
            vctr(-8d,-48d,visible)
            vctr(8d,0d,visible)
            vctr(32d,48d,visible)
            vctr(8d,0d,visible)
            vctr(-16d,-48d,visible)
            vctr(200d,0d,visible)
            vctr(24d,56d,visible)
            vctr(-14d,-30d,hidden)
            vctr(32d,0d,visible)
            vctr(14d,30d,hidden)
            vctr(-24d,-56d,visible)
            vctr(168d,0d,visible)
            vctr(-236d,28d,hidden)
            rtsl

;**************************************
    .sbttl "Maze Pieces"
;**************************************
;maze_vpg    = $60|thispage

;Horizontal 
maze1   vctr(255d,0d,visible)
        rtsl
        
;Left Down
maze2   vctr(127d,0d,visible)
        vctr(0d,-127d,visible)
        vctr(127d,127d,hidden)
        rtsl

;Left Up        
maze3   vctr(127d,0d,visible)
        vctr(0d,127d,visible)
        vctr(127d,-127d,hidden)
        rtsl

;Right Up        
maze4   vctr(127d,127d,hidden)
        vctr(0d,-127d,visible)
        vctr(127d,0d,visible)
        rtsl

;Right Down
maze5   vctr(127d,-127d,hidden)
        vctr(0d,127d,visible)
        vctr(127d,0d,visible)
        rtsl
 
;Vertical 
maze6   vctr(127d,-127d,hidden)
        vctr(0d,255d,visible)
        vctr(127d,-127d,hidden)
        rtsl

;Blank        
maze7   vctr(255d,0d,hidden)
        rtsl

;Horizontal (small)        
mape1   vctr(16d,0d,visible)
        rtsl

;Left Down (small)        
mape2   vctr(8d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(8d,8d,hidden)
        rtsl
        
;Left Up (small)        
mape3   vctr(8d,0d,visible)
        vctr(0d,8d,visible)
        vctr(8d,-8d,hidden)
        rtsl

;Right Up (small)        
mape4   vctr(8d,8d,hidden)
        vctr(0d,-8d,visible)
        vctr(8d,0d,visible)
        rtsl
        
;Right Down (small)       
mape5   vctr(8d,-8d,hidden)
        vctr(0d,8d,visible)
        vctr(8d,0d,visible)
        rtsl
        
;Vertical (small)        
mape6   vctr(8d,-8d,hidden)
        vctr(0d,16d,visible)
        vctr(8d,-8d,hidden)
        rtsl
        
;Blank (small)        
mape7   vctr(16d,0d,hidden)
        rtsl


;mapdt_vpg   = $60|thispage        
mapdot  vctr(6d,0d,visible)
        vctr(-2d,-4d,hidden)
        vctr(0d,6d,visible)
        rtsl
		
;*************************************
; Crosshair Images
;*************************************
crman		vscal(ywin_off,binscal1,$20)
            vstat(sparkle_off,xflip_off,vpage2,$F,colcyan)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            
crreac		vscal(ywin_off,binscal1,$20)
            vstat(sparkle_off,xflip_off,vpage2,$F,colorange)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            
crfire		vscal(ywin_off,binscal1,$80)
            vstat(sparkle_off,xflip_off,vpage2,$F,colwhiter)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            
crlsht		vscal(ywin_off,binscal1,$80)
            vstat(sparkle_off,xflip_off,vpage2,$4,colred2)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            
crcann		vscal(ywin_off,binscal1,$40)
            vstat(sparkle_off,xflip_off,vpage2,$C,colwhite)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            
crrob		vscal(ywin_off,binscal1,$80)
            vstat(sparkle_off,xflip_off,vpage2,$E,colbluer)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl

crmax		vscal(ywin_off,binscal1,$80)
            vstat(sparkle_off,xflip_off,vpage2,$8,colred2)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl

longline	vctr(608d,0d,7)
            rtsl
			
			
;*********************************************************
; Maynard Bitches!
;*********************************************************
maynard		vctr(-200d,0d,hidden)
			vctr(-10d,-61d,visible)
			vctr(-29d,-56d,visible)
			vctr(-44d,-44d,visible)
			vctr(-56d,-29d,visible)
			vctr(-61d,-10d,visible)
			vctr(-61d,10d,visible)
			vctr(-56d,29d,visible)
			vctr(-44d,44d,visible)
			vctr(-29d,56d,visible)
			vctr(-10d,61d,visible)
			vctr(10d,61d,visible)
			vctr(29d,56d,visible)
			vctr(44d,44d,visible)
			vctr(56d,29d,visible)
			vctr(61d,10d,visible)
			vctr(61d,-10d,visible)
			vctr(56d,-29d,visible)
			vctr(44d,-44d,visible)
			vctr(29d,-56d,visible)
			vctr(10d,-61d,visible)
			vctr(-68d,147d,hidden)
			vstat(sparkle_off,xflip_off,vpage2,$A,colcyan)
			jsrl(mayring)
			vctr(10d,14d,visible)
			vctr(326d,190d,hidden)
			vctr(10d,-10d,hidden)	
			vstat(sparkle_off,xflip_off,vpage2,$A,colbluer)
			jsrl(mayring)
			vctr(5d,7d,visible)				;vctr(10d,14d,visible)
			vctr(326d,190d,hidden)
			vctr(10d,0d,hidden)
			vstat(sparkle_off,xflip_off,vpage2,$A,colred2)
			jsrl(mayring)
			;vctr(10d,14d,visible)
			rtsl

mayring		vctr(24d,4d,visible)
			vctr(22d,-10d,visible)
			vctr(10d,-18d,visible)
			vctr(2d,-24d,visible)
			vctr(-10d,-28d,visible)
			vctr(-16d,-22d,visible)
			vctr(-20d,-24d,visible)
			vctr(-26d,-26d,visible)
			vctr(-32d,-26d,visible)
			vctr(-24d,-16d,visible)
			vctr(-34d,-20d,visible)
			vctr(-30d,-16d,visible)
			vctr(-30d,-14d,visible)
			vctr(-32d,-10d,visible)
			vctr(-34d,-10d,visible)
			vctr(-34d,-6d,visible)
			vctr(-40d,2d,visible)
			vctr(-22d,8d,visible)
			vctr(-16d,20d,visible)
			vctr(0d,18d,visible)
			vctr(6d,14d,visible)
			rtsl
;*******************************************
	.sbttl "Self Test and other routines"
;*******************************************
spot_vpg = $60
		
spot		vcntr
            vscal(ywin_off,binscal2,$00)
            vctr(-300d,-300d,hidden)
            vctr(600d,600d,hidden)
            vcntr
            rtsl

clline		vctr(-256d,16d,hidden)
            rtsl
			
clpat       vctr(256d,0d,7)
            jsrl(clline)
clpt2       vctr(256d,0d,6)
            jsrl(clline)
            vctr(256d,0d,5)
            jsrl(clline)
            vctr(256d,0d,4)
            jsrl(clline)
            vctr(256d,0d,3)
            jsrl(clline)
            vctr(256d,0d,2)
            rtsl
			
cl73		vstat(sparkle_off,xflip_off,vpage0,$1,colwhite)
            vctr(64d,0d,2)
            vctr(-320d,16d,hidden)
            vctr(256d,0d,1)
            rtsl
	
frcfl       vcntr
            vscal(ywin_off,binscal2,$00)
frbox       vctr(512d,432d,hidden)
frbx2       vctr(-1024d,0d,visible)
            vctr(0d,-864d,visible)
            vctr(1024d,0d,visible)
            vctr(0d,864d,visible)
            rtsl

vline		vctr(0d,-864d,visible)
            vcntr
            rtsl
			
hline		vctr(1023d,0d,visible)
            vcntr
            rtsl
			
hystr		vcntr
            vscal(ywin_off,binscal1,$00)
            vctr(0d,192d,hidden)
            jsrl(hystr2)
            vctr(10d,0d,visible)
            vcntr
            vctr(0d,-192d,hidden)
            jsrl(hystr2)
            vctr(10d,0d,visible)
            vcntr
            vctr(256d,0d,hidden)
            jsrl(hystr2)
            vctr(0d,10d,visible)
            vcntr
            vctr(-256d,0d,hidden)
            jsrl(hystr2)
            vctr(0d,10d,visible)
            vcntr
            rtsl
			
hystr2  	vstat(sparkle_off,xflip_off,vpage0,$8,colwhite)
            vcntr
            vcntr
            rtsl

;This is a call to a black box that is drawn if the VG
;is running too long and Alpha assumes that it may have 
;run amok.
waste		;vstat(sparkle_off,xflip_off,vpage0,$0,colblack)
			vstat(sparkle_off,xflip_off,vpage0,$0,colred)
            jsrl(frcfl)
            vhalt

	orgchk($5FFF,$)
	
	;.nocodes		;So we dont have list file buffer overflows
	.fill $6000-*
    
;********************************
;* VROM Checksum
;******************************** 
    .org $5FFF
    .chk $5000,$5FFF,IDENTIFIER_VR
;********************************

	.org $6FFF
	.db 0
	.end
	
;**************************************
;* Main VROM exports	
.export char_a,char_space,char_b,char_c,char_d,char_e,char_f,char_g,char_h,char_i,char_j,char_k,char_l
.export char_m,char_n,char_q,char_o,char_r,char_p,char_s,char_t,char_u,char_v,char_w,char_x,char_y,char_z
.export char_1,char_2,char_8,char_3,char_9,char_4,char_6,char_5,char_0,char_7,char_percent,char_colon,char_dash,char_ltri
.export char_comma,char_period,char_dash,char_excla,char_half,lifech,qmark,vgmsga,onearw,onesigt,onesigr,onesigl,crosshatch
.export mazet,mapet,mansrc,rods,bigrods,heads,tails,guns,eyes,cann,planes,beapic,movet,bases,fbase,enemys
.export shtexp,fexps,bxp0s,bxp1s,sexps,sxp0s,sxp1s,sxp2s,sxp3s,sxps,smtb,mazarw,mazoutwrd,lightning,cerpup,cerwng,onesign
.export tactc0,tactc1,tactc2,tactc3,tactc4
.export newshot,cerstf,cerbng,gclock,gkey,keypbox,gboot,gtite,glock,gkeyp,gpod,gtran,gtransp,ghand,booth3,booth4,booth5
.export epod1,epod2,epod3,epod4,epod5,epod6
.export havoc1,havoc2,havoc3
;.export glint0,glint1,glint2,glint3,glint4,glint5,glint6,glint7,glint8,glint9,glinta,glintb,glintc,glintd,glinte,glintf
.export crman,crreac,crfire,crlsht,crcann,crrob,crmax,longline
.export spot,clpat,clpt2,cl73,frcfl,frbox,frbx2,vline,hline,hystr,waste,shtexpnc,flare

#IF MAX_LEVITATIONS != 0  
.export lvts
#ENDIF

.export tokens,tokhex,maynard

;**************************************
; Character JSRL exports
.export vgjch0,vgjch1,vgjch2,vgjch3,vgjch4,vgjch5,vgjch6,vgjch7,vgjch8,vgjch9,vgjcha,vgjchb,vgjchc,vgjchd,vgjche,vgjchf
.export vgjchg,vgjchh,vgjchi,vgjchj,vgjchk,vgjchl,vgjchm,vgjchn,vgjcho,vgjchp,vgjchq,vgjchr,vgjchs,vgjcht,vgjchu,vgjchv
.export vgjchw,vgjchx,vgjchy,vgjchz,vgjchsp,vgjchop,vgjchcp,vgjchpe,vgjchco

;**************************************
;* VROM Page 0 exports
.export body,bodyt,bigbody

;**************************************
;* VROM Page 1 exports
.export shield,pic27,pic28,pic38,dab06,pic34,pic37,leg1,leg2,brickp,padlep,brline,lrsrbx
.export tactd0,tactd1,tactd2,tactd3,tactd4,tactd5,tactd6,tactd7,tactd8,tactd9

;**************************************
;* VROM Page 2 exports
.export mapdot,shipsh
.export spinner0,spinner1,spinner2,spinner3

.export lvert00,lvert01,lvert02,lvert03,lvert04,lvert05,lvert06,lvert07,lvert08
.export lvert10,lvert11,lvert12,lvert13,lvert14,lvert15,lvert16,lvert17,lvert18
.export lvert20,lvert21,lvert22,lvert23,lvert24,lvert25,lvert26,lvert27,lvert28
.export lvert30,lvert31,lvert32,lvert33,lvert34,lvert35,lvert36,lvert37,lvert38
.export lvert40,lvert41,lvert42,lvert43,lvert44,lvert45,lvert46,lvert47,lvert48
.export lvert50,lvert51,lvert52,lvert53,lvert54,lvert55,lvert56,lvert57,lvert58

.export hfold20,hfold21,hfold22,hfold23,hfold24

.export lhorz00,lhorz01,lhorz02,lhorz03,lhorz04,lhorz05,lhorz06,lhorz07,lhorz08
.export lhorz10,lhorz11,lhorz12,lhorz13,lhorz14,lhorz15,lhorz16,lhorz17,lhorz18
.export lhorz20,lhorz21,lhorz22,lhorz23,lhorz24,lhorz25,lhorz26,lhorz27,lhorz28
.export lhorz30,lhorz31,lhorz32,lhorz33,lhorz34,lhorz35,lhorz36,lhorz37,lhorz38
.export lhorz40,lhorz41,lhorz42,lhorz43,lhorz44,lhorz45,lhorz46,lhorz47,lhorz48
.export lhorz50

.export maxheads,maxbods,maxeyes
.export maxbody0,maxhead0,maxeye0
;.export maxeye0

.export webln00,webln01,webln02,webln03,webln04,webln05,webln06
.export webln07,webln08,webln09,webln0a,webln0b

.export weblnh0,weblnh1,weblnh2,weblnh3,weblnh4,weblnh5,weblnh6
.export weblnh7,weblnh8,weblnh9,weblnha,weblnhb

.export cerexp0,cerexp1,cerexp2,cerexp3,cerexp4,cerexp5,cerexp6,cerexp7

.export elevat,elevatb,elevatc,elesign,elev_vpg,elesigt,elevp

.export trix0,trix1,trix2,trix3,trix4,trix5,trix6,trix7   

.export shoes

.export vgjchhf

.export lifech

.export idx_pic0,idx_pic1,idx_pic2,idx_pic3,idx_pic4,idx_pic5,idx_pic6,idx_pic7,idx_pic8,idx_pic9
.export idx_pic10,idx_pic11,idx_pic12,idx_pic13,idx_pic14,idx_pic15,idx_pic16,idx_pic17,idx_pic18,idx_pic19
.export idx_pic20,idx_pic21,idx_pic22,idx_pic23,idx_pic24,idx_pic25,idx_pic26,idx_pic27,idx_pic28,idx_pic29
.export idx_pic30,idx_pic31,idx_pic32,idx_pic33,idx_pic34,idx_pic35,idx_pic36,idx_pic37,idx_pic38

.export idx_pic21,idx_pic21a,idx_pic21b,idx_pic21c,idx_pic21d,idx_pic21e,idx_pic21f
.export idx_flossa,idx_flossb,idx_flossc,idx_flossd,idx_flosse,idx_flossf,idx_flossg
.export idx_flossh,idx_flossi,idx_flossj,idx_flossk,idx_flossl,idx_flossm
.export idx_dab02,idx_dab04,idx_dab06
;.export pic21,pic21a

.export horiz,horiz_vpg,mntnpo,volpart,fterra,fterrap

.export objsr,crsbufs

;.export catastro_0,catastro_vpg

;******** Stat ROM Page Variables **************
.export plane_vpg,fexps_vpg,sexps_vpg,tube_vpg,st_vpg,enm_vpg,bas_vpg,shld_vpg,mpic_vpg
.export tact_vpg,brick_vpg,shtex_vpg,smtb_vpg,live_vpg,body_vpg,rods_vpg,clock_vpg,boot_vpg
.export hand_vpg,tite_vpg,lock_vpg,pod_vpg,tran_vpg,ltg_vpg,shpsh_vpg,spot_vpg
.export becn_vpg,mzls_vpg,gun_vpg,lshot_vpg,tacct_vpg,maxrob_vpg,spinnr_vpg,spstat_vpg
.export flare_vpg,arrow_vpg,tok_vpg

;.export mapdt_vpg,maze_vpg

.export hmtok1,hmtok2,hmtok3,plne24