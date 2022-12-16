
;***********************************************
; Random Maze Strings
; 
; ENGLISH
;**********************************************

mzr00a	.ctext "LINGERIE FLOOR 2..."		
mzr00b 	.ctext "REACTOR FLOOR 1"			
mzr01a 	.ctext "NOW HIRING..."				
mzr01b	.ctext "INQUIRE AT HR ON FLOOR 3"	
mzr02a 	.ctext "GARBAGE FACILITY IS"		
mzr02b	.ctext "LOCATED ON FLOOR 2"			
mzr03a  .ctext "SELF-DESTRUCT SYSTEM IS"	
mzr03b	.ctext "LOCATED IN BASEMENT"		
mzr04a  .ctext "AUTHORIZED SERVICE ROBOTS ONLY"				
mzr05a 	.ctext "ABANDON ALL HOPE..."		
mzr05b	.ctext "YE WHO ENTER HERE"			
mzr06a 	.ctext "I REALLY CANT BELIEVE YOU"	
mzr06b	.ctext "MADE IT THIS FAR!"			
mzr07a 	.ctext "THERE IS NO TURNING BACK NOW"	
mzr08a 	.ctext "WHAT ARE YOU LOOKING AT?"		
mzr09a 	.ctext "REMEMBER, THIS FACILITY"		
mzr09b	.ctext "CLOSES AT 10 PM!"				
mzr0aa 	.ctext "WHEN ARE YOU GOING TO LEAVE?"	
mzr0ba 	.ctext "WHITE ZONE IS FOR LOADING"		
mzr0bb	.ctext "AND UNLOADING ONLY"				
mzr0ca 	.ctext "YOU CANT PARK THAT"				
mzr0cb	.ctext "SHIP THERE!"								
mzr0da 	.ctext "10 SECONDS TILL SELF-DESTRUCT"	
mzr0db	.ctext "... JUST KIDDING!"				
mzr0ea 	.ctext "YOUR PARKING METER HAS EXPIRED"			
mzr0fa 	.ctext "DUDE, WHERES MY SHIP?"	


#IF TOURNAMENT_EDITION != 0
#include ./tw_mazedstr_en_te.asm
#ELSE
#include ./tw_mazedstr_en_pe.asm
#ENDIF

; ;********************************************************************
; ;  Dif 0 - Maze A - Level 1 - Level 1
; ;********************************************************************
; mzh00a  .ctext "FOLLOW ARROWS AND TOUCH REACTOR"
        ; czmess(mzh00a,$50,mzh00a_)	;,$A3)
		
; mzh00b  .ctext "EXIT MAZE BEFORE TIMER REACHES ZERO"
        ; czmess(mzh00b,$48,mzh00b_)	;,$97)
		
; ;********************************************************************
; ;  Dif 0 - Maze B - Level 2 - Level 2
; ;********************************************************************
; mzh01a  .ctext "USE SCANNER TO TRIGGER REACTOID"
        ; czmess(mzh01a,$50,mzh01a_)	;A3)
; mzh01b  .ctext "WHEN PATH IS CLEAR"
        ; czmess(mzh01b,$48,mzh01b_)	;,$CA)

; ;********************************************************************
; ;  Dif 0 - Maze C - Level 3 - Level 3
; ;********************************************************************
; mzh02a  .ctext "USE SHIELDS TO DESTROY "
        ; czmess(mzh02a,$50,mzh02a_)	;,$BB)
; mzh02b  .ctext "1 PERKOID ROBOT OR PYROID"
        ; czmess(mzh02b,$48,mzh02b_)	;,$B5)
		
; ;********************************************************************
; ;  Dif 1 - Maze D - Level 4 - Level 4
; ;********************************************************************
; mzh03a  .ctext "FIND KEYS TO OPEN DOORS"
        ; czmess(mzh03a,$50,mzh03a_)	;,$BB)
; mzh03b  .ctext "YELLOW OXYGEN IS MORE VALUABLE    "
        ; czmess(mzh03b,$48,mzh03b_)	;,$9A)
		
; ;********************************************************************
; ;  Dif 1 - Maze A - Level 5 - Level 5
; ;********************************************************************
; mzh10a  .ctext "RED TRIP PADS LAUNCH DEADLY FIREBALLS"
        ; czmess(mzh10a,$50,mzh10a_)	;,$91)
		
; ;********************************************************************
; ;  Dif 1 - Maze B - Level 6 - Level 6
; ;********************************************************************
; mzh11a  .ctext "IS THAT ONE WAY THE ONLY WAY?"
        ; czmess(mzh11a,$50,mzh11a_)	;,$A9)

; ;********************************************************************
; ;  Dif 1 - Maze C - Level 7 - Level 7
; ;********************************************************************
; mzh12a  .ctext "I CANT BELIEVE YOU MADE IT THIS FAR"
        ; czmess(mzh12a,$50,mzh12a_)	;,$97)
		
; ;********************************************************************
; ;  Dif 1 - Maze D - Level 8 - Level 8
; ;********************************************************************
; mzh13a  .ctext "IF YOU ARE SO GOOD, YOU FIND THE PATH!"
        ; czmess(mzh13a,$50,mzh13a_)	;,$8E)
		
; ;********************************************************************
; ;  Dif 2 - Maze A - Level 9 - Level 9
; ;********************************************************************
; mzh20a  .ctext "WATCH OUT FOR CHANGING WALLS AND FLOORS"
        ; czmess(mzh20a,$50,mzh20a_)	;,$8B)

; ;********************************************************************
; ;  Dif 2 - Maze B - Level 10 - Level 10
; ;********************************************************************		
; mzh21a  .ctext "THE PATH IN IS NOT THE PATH OUT"
        ; czmess(mzh21a,$50,mzh21a_)	;,$A3)
		
; ;********************************************************************
; ;  Dif 2 - Maze C - Level 11 - Level 11
; ;********************************************************************
; mzh22a  .ctext "FOLLOW THE PATH THROUGH CHANGING WALLS"
        ; czmess(mzh22a,$50,mzh22a_)	;,$8E)
		
; ;********************************************************************
; ;  Dif 3 - Maze D - Level 12 - Level 12
; ;********************************************************************
; mzh23a  .ctext "KEEP PLAYING, THE HOME WORLD IS NEAR"
        ; czmess(mzh23a,$50,mzh23a_)	;,$94)
; mzh23b  .ctext "WE MEAN IT THIS TIME!"
        ; czmess(mzh23b,$48,mzh23b_)	;,$C1)
		
; ;********************************************************************
; ;  Dif 3 - Maze A - Level 13 - Level 13
; ;********************************************************************
; mzh30a  .ctext "USE YOUR TIME WISELY"
        ; czmess(mzh30a,$50,mzh30a_)	;,$C4)
		
; ;********************************************************************
; ;  Dif 3 - Maze B - Level 14 - Level 14
; ;********************************************************************
; mzh31a  .ctext "ION CANNON SUPPORT IS NOT DEADLY"
        ; czmess(mzh31a,$50,mzh31a_)	;,$A0)
		
; ;********************************************************************
; ;  Dif 3 - Maze C - Level 15 - Level 15
; ;********************************************************************
; mzh32a  .ctext "BALANCE IS THE KEY TO EVERYTHING"
        ; czmess(mzh32a,$50,mzh32a_)	;,$A0)
		
; ;********************************************************************
; ;  Dif 4 - Maze D - Level 16 - Level 16
; ;********************************************************************
; mzh33a  .ctext "YOU MUST HAVE THE HIGH SCORE BY NOW"
        ; czmess(mzh33a,$50,mzh33a_)	;,$97)
		
; ;********************************************************************
; ;  Dif 4 - Maze A - Level 17 - Level 17
; ;********************************************************************
; mzh40a  .ctext "NO HAND TRICK THIS TIME"
        ; czmess(mzh40a,$50,mzh40a_)	;,$BB)
		
; ;********************************************************************
; ;  Dif 4 - Maze B - Level 18 - Level 18
; ;********************************************************************
; mzh41a  .ctext "HAVE YOU FOUND"
        ; czmess(mzh41a,$50,mzh41a_)	;,$D6)
; mzh41b  .ctext "ALL THE HIDDEN TOKENS YET?"
        ; czmess(mzh41b,$48,mzh41b_)	;,$B2)
		
; ;********************************************************************
; ;  Dif 4 - Maze C - Level 19 - Level 19
; ;********************************************************************
; mzh42a  .ctext "BOOTIES MASTERY REQUIRED"
        ; czmess(mzh42a,$50,mzh42a_)	;,$B8)
		
; ;********************************************************************
; ;  Dif 5 - Maze D - Level 20 - Level 20
; ;********************************************************************
; mzh43a  .ctext "HINT - THE SLOW SHOT IS YOUR BEST SHOT"
        ; czmess(mzh43a,$50,mzh43a_)	;,$8E)
		
; ;********************************************************************
; ;  Dif 5 - Maze A - Level 21 - Level 21
; ;********************************************************************
; mzh50a  .ctext "OK CLONES,RISE AND SHINE,"
        ; czmess(mzh50a,$50,mzh50a_)	;,$B5)
; mzh50b  .ctext "AND DONT FORGET YOUR BOOTIES"
        ; czmess(mzh50b,$48,mzh50b_)	;,$AC)

; ;********************************************************************
; ;  Dif 5 - Maze B - Level 22 - Level 22
; ;********************************************************************		
; mzh51a  .ctext "SHIELDS RECHARGE WHEN TRAVELING"
        ; czmess(mzh51a,$50,mzh51a_)	;,$A3)
; mzh51b  .ctext "THROUGH FLASHING TRANSPORTERS"
        ; czmess(mzh51b,$48,mzh51b_)	;,$A9)
		
; ;********************************************************************
; ;  Dif 5 - Maze C - Level 23 - Level 23
; ;********************************************************************
; mzh52a  .ctext "WATCH THAT FIRST STEP"
        ; czmess(mzh52a,$50,mzh52a_)	;,$C1)
; mzh52b  .ctext "ITS A DOOZIE!"
        ; czmess(mzh52b,$48,mzh52b_)	;,$D9)
		
; ;********************************************************************
; ;  Dif 6 - Maze D - Level 24 - Level 24
; ;********************************************************************
; mzh53a  .ctext "USE RED KEY TO UNLOCK"
        ; czmess(mzh53a,$50,mzh53a_)	;,$C1)
; mzh53b  .ctext "FINAL REACTOID CHAMBER"
        ; czmess(mzh53b,$48,mzh53b_)	;,$BE)
		
; ;********************************************************************
; ;  Dif 6 - Maze A - Level 25 - Level 25
; ;********************************************************************
; mzh60a  .ctext "LURE MAXOIDS AWAY FROM KEYS"
        ; czmess(mzh60a,$50,mzh60a_)	;,$AF)
		
; ;********************************************************************
; ;  Dif 6 - Maze B - Level 26 - Level 26
; ;********************************************************************
; mzh61a  .ctext "TRAP MAXOIDS BEHIND WALLS"
        ; czmess(mzh61a,$50,mzh61a_)	;,$B5)
		
; ;********************************************************************
; ;  Dif 6 - Maze C - Level 27 - Level 27
; ;********************************************************************
; mzh62a  .ctext "TO TRAP OR DESTROY"
        ; czmess(mzh62a,$50,mzh62a_)	;,$CA)
; mzh62b  .ctext "THAT IS THE QUESTION"
        ; czmess(mzh62b,$48,mzh62b_)	;,$C4)
		
; ;********************************************************************
; ;  Dif 7 - Maze D - Level 28 - Level 28
; ;********************************************************************
; mzh63a  .ctext "THE WILLS ACCOMPLISH EVERYTHING"
        ; czmess(mzh63a,$50,mzh63a_)	;,$A3)