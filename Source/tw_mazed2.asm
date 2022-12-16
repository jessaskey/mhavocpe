;***************************************************
    .TEXT ".TWMAZED2."
;***************************************************
    .title "Maze Init Data"
;***************************************************
;* Maze Init Source Pointers 
mzinit  .word   mzsc00,mzsc01,mzsc02,mzsc03 ;Difficulty=0
        .word   mzsc10,mzsc11,mzsc12,mzsc13 ;Difficulty=1
        .word   mzsc20,mzsc21,mzsc22,mzsc23 ;Difficulty=2
        .word   mzsc30,mzsc31,mzsc32,mzsc33 ;Difficulty=3
        .word   mzsc40,mzsc41,mzsc42,mzsc43 ;Difficulty=4
        .word   mzsc50,mzsc51,mzsc52,mzsc53 ;Difficulty=5
        .word   mzsc60,mzsc61,mzsc62,mzsc63 ;Difficulty=6
		
;*******************************************************
;Maze hint pointers, can have two messages per level
;-1 means no message 
#DEFINE	mazehint(hint1,hint2)	\#IFDEF hint1 \ .db hint1 \#ELSE \ .db -1 \#ENDIF 							
#DEFCONT						\#IFDEF hint2 \ .db hint2 \#ELSE \ .db -1 \#ENDIF 

mazehints   
        mazehint(mmzh00a,mmzh00b)
		mazehint(mmzh01a,mmzh01b)
		mazehint(mmzh02a,mmzh02b)
		mazehint(mmzh03a,mmzh03b)
		mazehint(mmzh10a,mmzh10b)
		mazehint(mmzh11a,mmzh11b)
		mazehint(mmzh12a,mmzh12b)
		mazehint(mmzh13a,mmzh13b)
        mazehint(mmzh20a,mmzh20b)
		mazehint(mmzh21a,mmzh21b)
		mazehint(mmzh22a,mmzh22b)
		mazehint(mmzh23a,mmzh23b)
        mazehint(mmzh30a,mmzh30b)
		mazehint(mmzh31a,mmzh31b)
		mazehint(mmzh32a,mmzh32b)
		mazehint(mmzh33a,mmzh33b)
        mazehint(mmzh40a,mmzh40b)
		mazehint(mmzh41a,mmzh41b)
		mazehint(mmzh42a,mmzh42b)
		mazehint(mmzh43a,mmzh43b)
        mazehint(mmzh50a,mmzh50b)
		mazehint(mmzh51a,mmzh51b)
		mazehint(mmzh52a,mmzh52b)
		mazehint(mmzh53a,mmzh53b)
        mazehint(mmzh60a,mmzh60b)
		mazehint(mmzh61a,mmzh61b)
		mazehint(mmzh62a,mmzh62b)
		mazehint(mmzh63a,mmzh63b)

mazehintsr
		.db mmzr00a,mmzr00b
		.db mmzr01a,mmzr01b
		.db mmzr02a,mmzr02b
		.db mmzr03a,mmzr03b
		.db mmzr04a,-1
		.db mmzr05a,mmzr05b
		.db mmzr06a,mmzr06b
		.db mmzr07a,-1
		.db mmzr08a,-1
		.db mmzr09a,mmzr09b
		.db mmzr0aa,-1
		.db mmzr0ba,mmzr0bb
		.db mmzr0ca,mmzr0cb
		.db mmzr0da,mmzr0db
		.db mmzr0ea,-1
		.db mmzr0fa,-1
		
;****************************************************	
;* Macro Defs for Maze Messages
;****************************************************	
___zmsgnum = 0
___znumsgs = (28d*2)+18d	;Can have max 2 hints per level + 16 secret messages
___zmestmp = $
___zmeslen = 0

zmessptrl = $
zmessptrh = (zmessptrl + ___znumsgs)
zmessypos = (zmessptrh + ___znumsgs)
zmessxpos = (zmessypos + ___znumsgs)

; #define     zmess(zlit,zypos,zxpos)  \___zmestmp .set $
; #defcont 	\ .org (czmessptrl+___zmsgnum)
; #defcont    \ .byte (zlit&$FF)
; #defcont    \ .org (czmessptrh+___zmsgnum)
; #defcont    \ .byte ((zlit/$100)&$FF)
; #defcont    \ .org (czmessypos+___zmsgnum)
; #defcont    \ .byte zypos
; #defcont    \ .org (czmessxpos+___zmsgnum)
; #defcont    \ .byte zxpos
; #defcont    \m+zlit = ___zmsgnum
; #defcont    \___zmsgnum .set ___zmsgnum+1
; #defcont	\ .org ___zmestmp
; #defcont    \#if (___znumsgs < ___zmsgnum) 
; #defcont    \ .error "MSG: Number of Maze messages exceeds defined limit, increase please!"
; #defcont    \#endif 

#define     czmess(zlit,zypos,zlen)  \___zmestmp .set $
#defcont	\___zmeslen .set (-1*((zlen)*3))
#defcont 	\ .org (zmessptrl+___zmsgnum)
#defcont    \ .byte (zlit&$FF)
#defcont    \ .org (zmessptrh+___zmsgnum)
#defcont    \ .byte ((zlit/$100)&$FF)
#defcont    \ .org (zmessypos+___zmsgnum)
#defcont    \ .byte zypos
#defcont    \ .org (zmessxpos+___zmsgnum)
#defcont	\#if ((___zmeslen&$ff) < $80)
#defcont	\ .byte $80
#defcont	\#else
#defcont    \ .byte ___zmeslen
#defcont	\#endif
#defcont    \m+zlit = ___zmsgnum
#defcont    \___zmsgnum .set ___zmsgnum+1
#defcont	\ .org ___zmestmp
#defcont    \#if (___znumsgs < ___zmsgnum) 
#defcont    \ .error "MSG: Number of Maze messages exceeds defined limit, increase please!"
#defcont    \#endif 

	.org (zmessxpos + ___znumsgs)

;***************************************************************
;* Maze Messages for on top of each maze
;***************************************************************    
messagesbase


;**************************************************
;* Random Maze Messages for after homeworld
;**************************************************
#IF LANGUAGE = 2
#include "tw_mazdstr_fr.asm"
#ELSE
#IF LANGUAGE = 1
#include "tw_mazdstr_de.asm"
#ELSE
#include "tw_mazdstr_en.asm"
#ENDIF
#ENDIF
		

; ;Maze Message Positioning
        czmess(mzr00a,$50,mzr00a_)	;,$C7)
		czmess(mzr00b,$48,mzr00b_)	;,$D3)
		
        czmess(mzr01a,$50,mzr01a_)	;,$D9)
		czmess(mzr01b,$48,mzr01b_)	;,$B8)
		
        czmess(mzr02a,$50,mzr02a_)	;,$C7)
		czmess(mzr02b,$48,mzr02b_)	;,$CA)
		
        czmess(mzr03a,$50,mzr03a_)	;,$BB)
		czmess(mzr03b,$48,mzr03b_)	;,$C7)
		
        czmess(mzr04a,$50,mzr04a_)	;,$A6)
		
        czmess(mzr05a,$50,mzr05a_)	;,$C7)
		czmess(mzr05b,$48,mzr05b_)	;,$CD)
		
        czmess(mzr06a,$50,mzr06a_)	;,$B5)
		czmess(mzr06b,$48,mzr06b_)	;,$CD)
		
        czmess(mzr07a,$50,mzr07a_)	;,$AC)
		
        czmess(mzr08a,$50,mzr08a_)	;,$B8)	
		
        czmess(mzr09a,$50,mzr09a_)	;,$BB)			
		czmess(mzr09b,$48,mzr09b_)	;,$D0)	
		
		czmess(mzr0aa,$50,mzr0aa_)	;,$AC)
		
        czmess(mzr0ba,$50,mzr0ba_)	;,$B5)
		czmess(mzr0bb,$48,mzr0bb_)	;,$CA)
		
        czmess(mzr0ca,$50,mzr0ca_)	;,$B5)
		czmess(mzr0cb,$48,mzr0cb_)	;,$DF)
		
        czmess(mzr0da,$50,mzr0da_)	;,$A9)
		czmess(mzr0db,$48,mzr0db_)	;,$CD)	
		
        czmess(mzr0ea,$50,mzr0ea_)	;,$A6)	
		
        czmess(mzr0fa,$50,mzr0fa_)	;,$C1)





;**************************************************
    .sbttl "Reactor, Fireball, Robot Init"
;**************************************************
;* Init data in the following order:        
;*                                          
;*  Reactor     XL,XH,YL,YH                 
;*  Fireballs   XL,XH,YL,YH,VELXH,VELYH   
;*  Perkoids    XL,XH,YL,YH,VELXH,VELYH  
;*  Max         XL,XH,YL,YH,MAXDATA(XXSSDDDD)
;*                Where:
;*                     SS = Speed (00=Slowest,01=Slow,02=Normal,03=Agressive)
;*                   DDDD = Distance in Stamps before Max 'activates' and starts moving
;*
;*  #FE signals start of Perkoid data, used again, it signals start of Max data  
;*  #FF signals end of all data           
;**************************************************
#IF TOURNAMENT_EDITION != 0
#include ./tw_mazed2_te.asm
#ELSE
#include ./tw_mazed2_pe.asm
#ENDIF

        
;Export Message tables
.export mazehints,mazehintsr,messagesbase,zmessptrl,zmessptrh,zmessxpos,zmessypos

;Export Maze Data
.export mzsc00,mzsc01,mzsc02,mzsc03
.export mzsc10,mzsc11,mzsc12,mzsc13
.export mzsc20,mzsc21,mzsc22,mzsc23 
.export mzsc30,mzsc31,mzsc32,mzsc33 
.export mzsc40,mzsc41,mzsc42,mzsc43 
.export mzsc50,mzsc51,mzsc52,mzsc53 
.export mzsc60,mzsc61,mzsc62,mzsc63