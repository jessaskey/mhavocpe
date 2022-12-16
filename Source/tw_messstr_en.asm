

;***********************************************
; Message Strings
; 
; ENGLISH
;**********************************************

;******************************************************************************
    .sbttl "TW Messages"
;******************************************************************************
;* Data: Message Pointer, Color, Scale, Y Position, X Position
;******************************************************************************

		mess(tmes0,colcyan,1,$48,$C7)       ;Destroy fish robotos
        mess(tmes1,colcyan,1,$48,$C7)       ;Shoot all fighters
        mess(tmes2,colcyan,1,$48,$C1)       ;Hit...
        mess(tmes3,colcyan,1,$48,$A3)       ;Maneuver through maze
        mess(tmes4,colflash,1,$48,$C3)      ;Destroy enemy shields
   	
        mess(oxyg,colgreen,1,10d,$EB)       ;Oxygen
        mess(reac,colred,1,-30d,$AA)        ;Reactor
        mess(bonus,colgreen,1,$54,$2F)      ;Bonus
        mess(mlevel,colyellow,1,-$50,$9E)   ;Level
		
        mess(bolif,colcyan,1,$a0,$C8)       ;Bonus Every
        mess(elife,colpurple,1,$a8,$CD)     ;Lives
        mess(cont,colwhite,1,$60,$D0)       ;Condition
		
        mess(mwred,colred2,1,$a2,$C4)       ;Red (For Warp Description)
        mess(mwyel,colyellow,1,$a2,$C4)     ;Yellow 
        mess(mwgrn,colgreen,1,$a2,$C4)      ;Green
        mess(mwaqu,colcyan,1,$a2,$C7)       ;Aqua
        mess(mwblue,colblue,1,$a2,$C7)      ;Blue
        mess(mwpurp,colpurple,1,$a2,$C4)    ;Purple
        mess(mwpink,colpink,1,$a2,$C7)      ;Pink
        ;mess(mwspkl,flacol,1,$a2,$C3)       ;Flash   
        ;mess(mwstrn,colred2,1,$a2,$C4)      ;Strange           
        mess(ccrit,colred,1,$60,$10)        ;Critical
        mess(cred,colred,1,$60,$10)         ;Red
        mess(cyel,colyellow,1,$60,$10)      ;Yellow
        mess(cgrn,colgreen,1,$60,$10)       ;Green

        mess(garbage,colred,1,$54,$98)      ;Garbage Ejected
        mess(ahome,colflash,1,$54,$A0)      ;Approaching Homeworld!
        mess(conf,colwhite,2,$38,$42)       ;Confirmed
        mess(mdist,colcyan,2,-$38,$D4)      ;Range
        mess(etyp,colcyan,2,-$3e,$D4)       ;Enemy Type
        mess(cols,colcyan,2,-$38,$04)       ;Closing
        mess(hold,colcyan,2,-$38,$04)       ;Holding
        mess(sphr,colcyan,2,-$3e,$04)       ;Spheroids
        mess(figh,colcyan,2,-$3e,$04)       ;Fighters
        mess(fort,colcyan,2,-$3e,$04)       ;Space Fort
        mess(spin,colcyan,2,-$3e,$04)       ;Spinners
        mess(mainf,colred2,2,-$3e,$04)      ;Mainframe
        mess(warpi,colgreenr,1,$54,$B5)     ;Intercepted Message
        mess(warp0,colgreenr,1,$48,$96)     ;Enter XX for Red warp...
        mess(warp1,colgreenr,1,$48,$96)     ;Enter XX for Yellow warp...
        mess(warp2,colgreenr,1,$48,$96)     ;Enter XXX for Green warp...
        mess(warp3,colgreenr,1,$48,$96)     ;Enter XXX for Aqua warp...
        mess(warp4,colgreenr,1,$48,$96)     ;Enter XXX for Blue warp...
        mess(warp5,colgreenr,1,$48,$96)     ;Enter XXX for Purple warp...
        ;mess(warp6,colgreenr,1,$48,$96)     ;Enter XXX for Pink warp...

		
		;Story
		mess(text0,colwhite,1,$10,$90)     
		mess(text1,colwhite,1,$10,$90)
		mess(text2,colwhite,1,$10,$90)
		mess(text3,colwhite,1,$10,$90)
		mess(text4,colcyan,1,$10,$90)
		mess(text5,colcyan,1,$10,$90)
		mess(text6,colcyan,1,$10,$90)
		mess(text7,colcyan,1,$10,$90)
		mess(text8,colgreen,1,$10,$90)
		mess(text9,colgreen,1,$10,$90)
		mess(text10,colgreen,1,$10,$90)
		mess(text11,colgreen,1,$10,$90)
		mess(text12,colyellow,1,$10,$90)
		mess(text13,colyellow,1,$10,$90)
		mess(text14,colyellow,1,$10,$90)
		mess(text15,colyellow,1,$10,$90)
		mess(text16,colyellow,1,$10,$90)
		mess(text17,colyellow,1,$10,$90)
		mess(text18,colpurple,1,$10,$90)
		mess(text19,colpurple,1,$10,$90)
		mess(textend,colblue,1,$10,$90)
		
		mess(txtcmfr,colpurple,1,$10,$90)       ;Story Cost - Free
		mess(texteot,colblue,1,$10,$90)			;End of Text 
		
		;Special Credits
		mess(cretx00,colwhite,1,$10,$90)
		mess(cretx01,colwhite,1,$10,$90)
		mess(cretx02,colred,1,$10,$90)			;Design Crew
		mess(cretx03,colwhite,1,$10,$90)
		mess(cretx04,colyellow,1,$10,$90)
		mess(cretx05,colyellow,1,$10,$90)
		mess(cretx06,colyellow,1,$10,$90)
		mess(cretx07,colyellow,1,$10,$90)
		mess(cretx08,colyellow,1,$10,$90)
		mess(cretx09,colyellow,1,$10,$90)
		mess(cretx10,colyellow,1,$10,$90)
		mess(cretx11,colyellow,1,$10,$90)
		mess(cretx12,colyellow,1,$10,$90)			;Design Crew
		mess(cretx13,colyellow,1,$10,$90)
		mess(cretx14,colwhite,1,$10,$90)
		mess(cretx15,colblack,1,$10,$90)
		mess(cretx16,colpurple,1,$10,$90)
		mess(cretx17,colpurple,1,$10,$90)
		mess(cretx18,colpurple,1,$10,$90)
		mess(cretx19,colpurple,1,$10,$90)
		mess(cretx20,colorange,1,$10,$90)
		mess(cretx21,colorange,1,$10,$90)
		mess(cretx22,colorange,1,$10,$90)
		mess(cretx23,colorange,1,$10,$90)
		mess(cretx24,colblack,1,$10,$90)
		mess(cretx25,colwhite,1,$10,$90)
		mess(cretx26,colblack,1,$10,$90)
		mess(cretxend,colwhite,1,$10,$90)
		
		;Old MESS3
		;YPosition is replaced by index for picture index
		mess(sco0,colcyan,1,0,$D6)     ;Enemy list    
		mess(scoln1,colwhite,1,0,$80)
		mess(sco2a,colred,1,0,$D9)
		mess(sco3,colyellow,1,1,$9A)
		mess(scospc1,colblack,1,0,$9A)
		mess(sco4,colyellow,1,2,$9A)
		mess(scospc2,colblack,1,0,$9A)
		mess(sco5,colyellow,1,3,$9A)
		mess(scoln2,colwhite,1,0,$80)
		mess(sco6,colred,1,0,$DC)
		mess(sco7,colyellow,1,4,$9A)
		mess(scospc3,colblack,1,0,$9A)
		mess(sco8,colyellow,1,5,$9A)
		mess(scospc4,colblack,1,0,$9A)
		mess(sco8m,colyellow,1,6,$9A)
		mess(scoln3,colwhite,1,0,$80)
		mess(sco10,colred,1,0,$DC)
		mess(sco11,colyellow,1,7,$9A)
		mess(scospc5,colblack,1,0,$9A)
		mess(sco12,colyellow,1,8,$9A) 
		mess(scospc6,colblack,1,0,$9A)    
		mess(sco13,colyellow,1,9,$9A)
		mess(scoln4,colwhite,1,0,$80)
		mess(sco14,colred,1,0,$DC)
		mess(sco15,colyellow,1,0,$90)
		mess(sco16,colyellow,1,0,$90)
		mess(sco17,colyellow,1,0,$90)
		mess(sco18,colyellow,1,0,$90)
		mess(scoln5,colwhite,1,0,$80)
		mess(sco20,colblue,1,0,$DC)
		mess(sco21,colblue,1,0,$90)
		mess(sco22,colblue,1,0,$90)
		mess(sco23,colblue,1,0,$90)
		mess(sco24,colblue,1,0,$90)
		mess(sco25,colblue,1,0,$90)
		mess(sco26,colred,1,0,$FA)
		mess(sco27,colred,1,0,$90)
		
		mess(endtx00,colcyan,1,$10,$90)
		mess(endtx01,colcyan,1,$10,$90)
		mess(endtx02,colcyan,1,$10,$90)
		mess(endtx03,colcyan,1,$10,$90)
		mess(endtx04,colcyan,1,$10,$90)
		mess(endtx05,colcyan,1,$10,$90)
		mess(endtx06,colcyan,1,$10,$90)
		mess(endtx07,colcyan,1,$10,$90)
		mess(endtx08,colcyan,1,$10,$90)
		mess(endtx09,colcyan,1,$10,$90)
		mess(endtx0a,colcyan,1,$10,$90)
		mess(endtx0b,colcyan,1,$10,$90)
		mess(endtx0c,colcyan,1,$10,$90)
		mess(endtx0d,colcyan,1,$10,$90)
		mess(endtx0e,colcyan,1,$10,$90)
		mess(endtx0f,colcyan,1,$10,$90)
		mess(endtx10,colcyan,1,$10,$90)
		mess(endtx11,colcyan,1,$10,$90)
		mess(endtx12,colcyan,1,$10,$90)
		mess(endtx13,colcyan,1,$10,$90)
		mess(endtx14,colcyan,1,$10,$90)
		mess(endtx15,colcyan,1,$10,$90)
		mess(endtx16,colcyan,1,$10,$90)
		mess(endtx17,colcyan,1,$10,$90)
		mess(endtx18,colcyan,1,$10,$90)
		mess(endtx19,colcyan,1,$10,$90)
		mess(endtx1a,colcyan,1,$10,$90)
		mess(endtx1b,colcyan,1,$10,$90)
		mess(endtx1c,colcyan,1,$10,$90)
		mess(endtxxx,colcyan,1,$10,$90)

gamov      	.ctext "GAME  OVER"
			cmess(gamov,colyellow,1,$24,gamov_,0)     ;Game Over ($D8)
press       .ctext "PRESS START"
			cmess(press,colred,1,$24,press_,0)        ;Press Start		
addm2       .ctext "TO START AT LEVEL "
			cmess(addm2,colcyan,1,-6,addm2_,2)        ;Rest of Message
addm1       .ctext "PRESS START WITHIN    SECONDS"
			cmess(addm1,colcyan,1,4,addm1_,0)         ;Add Time Message
playr       .ctext "PLAYER "
			cmess(playr,colyellow,1,$24,playr_,1)     ;Player (Big)
			
tmes0       .ctext "DESTROY FISH ROBOTS"
tmes1       .ctext "DESTROY ENEMY SHIPS"
tmes2       .ctext "MANEUVER THROUGH MAZE"            
tmes3       .ctext "SHOOT FIGHTERS, AVOID BOOM"            
tmes4       .ctext "DESTROY ENEMY SHIELDS"


hint0       .ctext "DOCK ON WHITE PLATFORM"
			cmess(hint0,colred,1,$46,hint0_,0)        ;Dock...	

;* REMOVED MAZE MESSAGES AND MOVED INTO mazed2.asm  
oxyg        .ctext "OXYGEN "
reac        .ctext "REACTOR"
bonus       .ctext "BONUS "
mlevel      .ctext "LEVEL "

insert      .ctext "INSERT COINS"
        cmess(insert,colred,1,$24,insert_,0)       ;Insert Coins

atari		.ctext "cMCMLXXXIII ATARI"		;little c is Copyright symbol
		cmess(atari,colbluer,1,$98,atari_,0)		;MCMLXXXIII Atari

#IF (LEVEL_EDITOR > 0)
credi       .ctext "MHEDIT.ASKEY.ORG"
			cmess(credi,colcyan,1,$18,credi_,0)       ;MHEDIT.ASKEY.ORG
#ELSE          
credi       .ctext "CREDITS "
			cmess(credi,colcyan,1,$18,credi_,4)       ;Credits
#ENDIF

bolif       .ctext "BONUS EVERY "
elife       .ctext " LIVES PER PLAYER"
cont        .ctext "CONDITION "
ccrit       .ctext "CRITICAL"         
cred        .ctext "RED"
cyel        .ctext "YELLOW"
cgrn        .ctext "GREEN"
garbage     .ctext "GARBAGE EJECTED"  

tesser0		.ctext "ALPHA TESSERACT ENGAGED!"   
		cmess(tesser0,colflash,1,$4C,tesser0_,0)	;Alpha Tesseract engaged!

tesser1		.ctext "BETA TESSERACT ENGAGED!" 
		cmess(tesser1,colflash,1,$4C,tesser1_,0)	;Beta Tesseract engaged!

tesser2		.ctext "GAMMA TESSERACT ENGAGED!" 
		cmess(tesser2,colflash,1,$4C,tesser2_,0)	;Gamma Tesseract engaged!

tesser3		.ctext "ZETA TESSERACT ENGAGED!" 
		cmess(tesser3,colflash,1,$4C,tesser3_,0)	;Zeta Tesseract engaged!
		
ahome       .ctext "ALERT! HOMEWORLD SPACE STATION!"
conf        .ctext "CONFIRMED"
mdist       .ctext "RANGE "
etyp        .ctext "ENEMY TYPE "
cols        .ctext "CLOSING"
hold        .ctext "HOLDING"
figh        .ctext "FIGHTERS"
sphr        .ctext "SPHEROIDS"
fort        .ctext "SPACE FORT"         
spin        .ctext "SPINNERS"            
mainf       .ctext "MAINFRAME"
warpi       .ctext "---INTERCEPTED MESSAGE---"
warp0       .ctext "USE    FOR RED WARP TO LEVEL 4"
warp1       .ctext "USE    FOR YELLOW WARP TO LEVEL 9"
warp2       .ctext "USE     FOR GREEN WARP TO LEVEL 10"
warp3       .ctext "USE     FOR AQUA WARP TO LEVEL 13"           
warp4       .ctext "USE     FOR BLUE WARP TO LEVEL 17"            
warp5       .ctext "USE     FOR PURPLE WARP TO LEVEL 19"
                          
his     .ctext "HIGH SCORES"
        cmess(his,colred,0,$48,his_*2,0)          ;High Scores
		
enin    .ctext "ENTER YOUR INITIALS"
        cmess(enin,colyellow,1,$34,enin_,0)      ;Enter your initials

getout  .ctext "GET OUT"
        mess(getout,colred2,0,0,$EB)        ;Get Out!!

pub0    .ctext "HOLD  BUTTON"
        mess(pub0,colwhite,0,0,$DC)         ;Push Button

pub1    .ctext "FOR HIGHER JUMPS"
        mess(pub1,colwhite,1,0,$D0)         ;For Higher Jumps

gtsc    .ctext "GREAT SCORE"
		cmess(gtsc,colgreenr,0,$48,gtsc_*2,0)      ;Great Score

ledit   .ctext "MAJOR HAVOC LEVEL EDITOR"
		cmess(ledit,colwhite,1,0,ledit_,0)		;Major Havoc Level Editor
 

cmodd		.ctext "DEMO  MODE"
		 	cmess(cmodd,colorange,1,$2e,cmodd_,0)     ;Demo Mode -  $D8
cmode       .ctext "FREE  PLAY"
        	cmess(cmode,colgreen,1,$2e,cmode_,0)      ;Free Play - $D8		
cmod1       .ctext "1 COIN 2 PLAYS"
			cmess(cmod1,colgreen,1,$2e,cmod1_,0)      ;1 coin 2 plays - $D4
cmod2       .ctext "1 COIN 1 PLAY"
			cmess(cmod2,colgreen,1,$2e,cmod2_,0)      ;1 coin 1 play - $D4
cmod3       .ctext "2 COINS 1 PLAY"
			cmess(cmod3,colgreen,1,$2e,cmod3_,0)      ;2 coins 1 play - $D4
			
mwred       .ctext "ENTER RED WARP----- "
mwyel       .ctext "ENTER YELLOW WARP-- "
mwgrn       .ctext "ENTER GREEN WARP--- "
mwaqu       .ctext "ENTER AQUA WARP--- "            
mwblue      .ctext "ENTER BLUE WARP--- "        
mwpurp      .ctext "ENTER PURPLE WARP--- "            
mwpink      .ctext "ENTER PINK WARP--- " 

#IF TOURNAMENT_EDITION != 0
promise     .ctext "TOURNAMENT EDITION"
		mess(promise,colwhite,1,$00,$F0)	

#ELSE
promise     .ctext "THE PROMISED END"
		mess(promise,colwhite,1,$00,$00)

#ENDIF

;MESS2
text0       .ctext "   YOU ARE MAJOR HAVOC, THE LEADER OF A BRAVE"
text1       .ctext "LITTER OF CLONES. YOU ARE THEM, THEY ARE YOU,"
text2       .ctext "ALL FROM ONE, ONE FROM ALL, FIGHTING FOR"
text3       .ctext "HUMANITY....."
text4       .ctext "    EONS AGO THE EVIL VAXXIAN EMPIRE OVERRAN"
text5       .ctext " THE GALAXY. MOST OF YOUR ANCESTORS WERE"
text6       .ctext "ENSLAVED AND TAKEN TO THE VAXXIAN HOMEWORLD"
text7       .ctext "OF MAYNARD. ONLY A FEW SCIENTISTS ESCAPED."
text8       .ctext "   TODAY, THEIR EMPIRE IS ALL BUT VANISHED."
text9       .ctext "YET VAXXIAN SPACE STATIONS, CONTROLLED AND"
text10      .ctext "DEFENDED BY ROBOTS, STILL PATROL THE GALAXY"
text11      .ctext "AND KEEP YOUR PEOPLE PRISONER."
text12      .ctext "   THE SMALL BAND OF SCIENTISTS CLONED YOU,"
text13      .ctext "MAJOR HAVOC, TO FLY YOUR CATASTROFIGHTER"
text14      .ctext "THROUGH A WORMHOLE IN SPACE AND TO LEAD YOUR"
text15      .ctext "CLONE ARMY AGAINST THE DREADED VAXXIAN ROBOT"
text16      .ctext "ARMADA. DESTROY THE ENEMY SPACE STATIONS AND"
text17      .ctext "LAND ON THE PLANET OF VAXX TO FREE YOUR PEOPLE."
text18      .ctext " DEPOSIT COINS NOW TO FINANCE YOUR CLONE ARMY."
text19      .ctext "YOU MAY EVEN EARN A BONUS CLONE NOW AND THEN."
textend		.ctext " "


;NEW STUFF
cretx00		.ctext ".........................................."
cretx01		.ctext " "
cretx02     .ctext "       --- ORIGINAL ATARI TEAM ---" 
cretx03		.ctext " "
cretx04     .ctext "OWEN RUBIN   - ORIGINAL CONCEPT,DESIGN,CODE"
cretx05		.ctext "MARK CERNY   - DESIGN,CODE,TUNING,CREATIVE"
cretx06		.ctext "LYLE RAINS   - ART,ANIMATION,CREATIVE"
cretx07		.ctext "MORGAN HOFF  - PROJECT MANAGER,P.I.T.R."
cretx08		.ctext "RICK MONCRIEF- SAVIOR,PROJECT MANAGER"
cretx09		.ctext "DOUG SNYDER  - HARDWARE DESIGN,ENGINEERING"
cretx10		.ctext "STEVE CALFEE - DEPARTMENT MANAGER,CREATIVE"
cretx11		.ctext "MIKE ALBAUGH - CODEBASE,TOOLS,SOLVER"
cretx12		.ctext "DAVE SHEPPARD- VAX SUPPORT,ASSEMBLER,TOOLS"
cretx13		.ctext " "
cretx14     .ctext "       --- PROMISED END TEAM ---" 
cretx15		.ctext " "
cretx16     .ctext "              OWEN RUBIN" 
cretx17     .ctext "              JESS ASKEY" 
cretx18		.ctext "              BRYAN ROTH"
cretx19		.ctext "                JERKY"
cretx20		.ctext "         SCOTT451     JON K."
cretx21		.ctext "         LUKE D.      MARK S."
cretx22		.ctext "         JEROME V.    THOMAS S."
cretx23		.ctext "                HAXRUS"
cretx24		.ctext " "
cretx25     .ctext "      UPDATES AT MHEDIT.ASKEY.ORG" 
cretx26		.ctext " "
cretxend	.ctext " "

txtcmfr     .ctext "   CLONES ARE FREE, WE ARE GIVING THEM AWAY."
texteot    	.ctext "           -----END OF TEXT-----" 

;MESS3
sco0    	.ctext "THE ENEMY LIST"
scoln1  	.ctext "  ............................................."
sco2a   	.ctext "SPACE ENEMIES"
sco3    	.ctext "  FISHOIDS         1000           "        
scospc1 	.ctext "                                  "
sco4    	.ctext "  FLYBOIDS          500           "
scospc2 	.ctext "                                  " 
sco5    	.ctext "   MAZOIDS          500           "
scoln2  	.ctext "  ............................................."
sco6    	.ctext "MAZE ENEMIES"
sco7    	.ctext "   PYROIDS         1000           "
scospc3 	.ctext "                                  "        
sco8    	.ctext "  PERKOIDS         1000           "
scospc4 	.ctext "                                  "        
sco8m   	.ctext "   MAXOIDS         9000           "
scoln3  	.ctext "  ............................................."
sco10   	.ctext "OTHER POINTS"
sco11   	.ctext "  REACTOID         5000           "
scospc5 	.ctext "                                  "        
sco12   	.ctext "    OXOID           100           "
scospc6 	.ctext "                                  "        
sco13   	.ctext "    OXOID        VARIES           "
scoln4  	.ctext "  ............................................."
sco14   	.ctext "OTHER SCORES"
sco15   	.ctext "OXYGEN BONUS FOR EXITING MAZE IS 100 TIMES"
sco16   	.ctext "THE NUMBER OF OXYGEN COUNTS LEFT."
sco17   	.ctext "IN SPACE, FASTER COMPLETION OF EACH WAVE"
sco18   	.ctext "AWARDS MORE BONUS POINTS AT END OF WAVE."
scoln5  	.ctext "  ............................................."
sco20   	.ctext "INSTRUCTIONS"
sco21   	.ctext "1. USE TACT SCAN FOR WARPING AND INFORMATION"
sco22   	.ctext "2. DESTROY MAINFRAME DEFENSES"
sco23   	.ctext "3. LAND ON WHITE PLATFORM OF DEFEATED MAINFRAME"
sco24   	.ctext "4. IN MAZE, FIND THE REACTOID AND TOUCH IT"
sco25   	.ctext "5. EXIT MAZE BEFORE REACTOID BLOWS UP"
sco26   	.ctext " "
sco27   	.ctext "  NOTE--HOLD JUMP BUTTON FOR HIGHER JUMPS"

endtx00		.ctext "AGAINST ALL ODDS, YOU, MAJOR HAVOC,"	
endtx01  	.ctext "AND YOUR LITTER OF CLONES, HAVE REACHED"
endtx02		.ctext "THE VAXXIAN HOMEWORLD AND DEALT THE"
endtx03		.ctext "VAXXIAN EMPIRE A TERMINAL BLOW AND PURGED"
endtx04		.ctext "THEM FROM THE GALAXY."
endtx05 	.ctext " "
endtx06		.ctext "YOUR PEOPLE HAVE BEEN FREED TO RETURN"
endtx07		.ctext "TO THEIR LAND, GO PHISHING, BUILD A"
endtx08		.ctext "FIREWALL, ATTEND A BOOT CAMP, AND SEEK"
endtx09 	.ctext "A LIFE FREE OF THE VAXX. EVENTUALLY,"
endtx0a		.ctext "THEY WILL PROCESS THE FACT THAT THEY"
endtx0b		.ctext "ARE FREE."
endtx0c		.ctext " "
endtx0d     .ctext "BUT FOR YOU, THERE IS STILL WORK TO BE"
endtx0e		.ctext "DONE. SOME VAXXIAN SPACE STATIONS ARE"
endtx0f		.ctext "STILL ROAMING THE GALAXY, UNAWARE THAT"
endtx10		.ctext "THEIR HOMEWORLD HAS CRASHED."
endtx11		.ctext " "
endtx12		.ctext "NOW, YOU AND YOUR CLONES CAN WARP"
endtx13		.ctext "AROUND THE GALAXY, AND TOSS THE REST"
endtx14		.ctext "OF THESE SPACE STATIONS INTO THE GARBAGE"
endtx15		.ctext "COLLECTOR."
endtx16		.ctext " "
endtx17		.ctext "CONGRATULATIONS MAJOR HAVOC, YOU HAVE DONE WELL."
endtx18		.ctext " "
endtx19		.ctext " "
endtx1a		.ctext " --OWEN R. RUBIN - BENEVOLENT OVERLORD"
endtx1b		.ctext " "
endtx1c		.ctext " "
endtxxx		.ctext " "
