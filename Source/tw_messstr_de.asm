

;******************************************************************************
    .sbttl "TW Messages"
;******************************************************************************
;* mess macro takes: Message Pointer, Color, Scale, Y Position, X Position
;******************************************************************************
; GERMAN
;**********************************************

gamov       .ctext "SPIELENDE"
		cmess(gamov,colyellow,1,$24,gamov_,0)     ;Game Over
		
press       .ctext "DRvCKE START"
        cmess(press,colred,1,$24,press_,0)        ;Press Start
		
addm2       .ctext "REST DER NACHRICHT "
        cmess(addm2,colcyan,1,-6,addm2_,2)        ;Rest of Message
		
addm1       .ctext "DRvCKE START WwHREND    SEKUNDEN"
        cmess(addm1,colcyan,1,4,addm1_,0)         ;Add Time Message
		
playr       .ctext "SPIELER " 
        cmess(playr,colyellow,1,$24,playr_,1)     ;Player (Big)
		
tmes0       .ctext "ZERSTpRE FISCHROBOTER"
		mess(tmes0,colcyan,1,$48,$C7)       ;Destroy fish robotos

tmes1       .ctext "TREFFE ALLE SCHIFFE"
        mess(tmes1,colcyan,1,$48,$C7)       ;Shoot all fighters

tmes2       .ctext "TREFFER"  
        mess(tmes2,colcyan,1,$48,$C1)       ;Hit...
       
tmes3       .ctext "NAVIGIERE DURCH DAS LABYRINTH"     
        mess(tmes3,colcyan,1,$48,$A3)       ;Maneuver through maze
     
tmes4       .ctext "ZERSTpRE GEGNERSCHILDE"
        mess(tmes4,colflash,1,$48,$C3)      ;Destroy enemy shields   
		
hint0       .ctext "ANDOCKEN"
        cmess(hint0,colred,1,$46,hint0_,0)        ;Dock...		

oxyg        .ctext "SAUERSTOFF"
        mess(oxyg,colgreen,1,10d,$EB)       ;Oxygen

reac        .ctext "REAKTOR"
        mess(reac,colred,1,-30d,$AA)        ;Reactor

bonus       .ctext "BONUS"
        mess(bonus,colgreen,1,$54,$2F)      ;Bonus

mlevel      .ctext "LEVEL"
        mess(mlevel,colyellow,1,-$50,$9E)   ;Level

insert      .ctext "MvNZE EINWERFEN"
        cmess(insert,colred,1,$24,insert_,0)       ;Insert Coins

atari       .ctext "cMCMLXXXIII ATARI"        ;little c is Copyright symbol
		cmess(atari,colbluer,1,$98,atari_,0)		;MCMLXXXIII Atari

#IF (LEVEL_EDITOR > 0)
credi       .ctext "MHEDIT.ASKEY.ORG"
        cmess(credi,colcyan,1,$18,credi_,0)       ;MHEDIT.ASKEY.ORG
#ELSE          
credi       .ctext "CREDITS"
		cmess(credi,colcyan,1,$18,credi_,4)       ;Credits
#ENDIF

bolif       .ctext "BONUS ALLE "
        mess(bolif,colcyan,1,$a0,$C8)       ;Bonus Every

elife       .ctext " LEBEN"
        mess(elife,colpurple,1,$a8,$ed)     ;Lives per game

cont        .ctext "STATUS "
        mess(cont,colwhite,1,$60,$D0)       ;Condition
		
ccrit       .ctext "KRITISCH"     
        mess(ccrit,colred,1,$60,$10)        ;Critical

cred        .ctext "ROT"
        mess(cred,colred,1,$60,$10)         ;Red

cyel        .ctext "GELB"
        mess(cyel,colyellow,1,$60,$10)      ;Yellow

cgrn        .ctext "GRvN"
        mess(cgrn,colgreen,1,$60,$10)       ;Green

garbage     .ctext "MvLL VERKLAPPT"
        mess(garbage,colred,1,$54,$98)      ;Garbage Ejected

tesser0     .ctext "ALPHA TESSERAKT AKTIV!"
		cmess(tesser0,colflash,1,$4C,_tesser0,0)	;Alpha Tesseract engaged!

tesser1     .ctext "BETA TESSERAKT AKTIV!"
		cmess(tesser1,colflash,1,$4C,_tesser1,0)	;Beta Tesseract engaged!

tesser2     .ctext "GAMMA TESSERAKT AKTIV!"
		cmess(tesser2,colflash,1,$4C,_tesser2,0)	;Gamma Tesseract engaged!

tesser3     .ctext "ZETA TESSERAKT AKTIV!" 
		cmess(tesser3,colflash,1,$4C,_tesser3,0)	;Zeta Tesseract engaged!
		
ahome       .ctext "ANNwHERUNG AN HEIMATWELT!"
        mess(ahome,colflash,1,$54,$A0)      ;Approaching Homeworld!

conf        .ctext "BESTwTIGT"
        mess(conf,colwhite,2,$38,$42)       ;Confirmed

mdist       .ctext "DISTANZ"
        mess(mdist,colcyan,2,-$38,$D4)      ;Range

etyp        .ctext "GEGNERART"
        mess(etyp,colcyan,2,-$3e,$D4)       ;Enemy Type

cols        .ctext "ANNwHERND"
        mess(cols,colcyan,2,-$38,$04)       ;Closing

hold        .ctext "ABWARTEND"
        mess(hold,colcyan,2,-$38,$04)       ;Holding

figh        .ctext "SCHIFFE"
        mess(figh,colcyan,2,-$3e,$04)       ;Fighters

sphr        .ctext "SPHEROIDE"
        mess(sphr,colcyan,2,-$3e,$04)       ;Spheroids

fort        .ctext "RAUMFESTUNG" 
        mess(fort,colcyan,2,-$3e,$04)       ;Space Fort
     
spin        .ctext ""SPINNER"  
        mess(spin,colcyan,2,-$3e,$04)       ;Spinners
   
mainf       .ctext "MAINFRAME"
        mess(mainf,colred2,2,-$3e,$04)      ;Mainframe      
		
warpi       .ctext "---NACHRICHT AUFGEFANGEN---"
        mess(warpi,colgreenr,1,$54,$B5)     ;Intercepted Message

warp0       .ctext "CODE   FvR ROTEN SPRUNG ZU LEVEL 4"
        mess(warp0,colgreenr,1,$48,$96)     ;Enter XX for Red warp...

warp1       .ctext "CODE   FvR GELBEN SPRUNG ZU LEVEL 9"
        mess(warp1,colgreenr,1,$48,$96)     ;Enter XX for Yellow warp...

warp2       .ctext "CODE   FvR GRvNEN SPRUNG ZU LEVEL 10"
        mess(warp2,colgreenr,1,$48,$96)     ;Enter XXX for Green warp...

warp3       .ctext "CODE   FvR AQUA SPRUNG ZU LEVEL 13"
        mess(warp3,colgreenr,1,$48,$96)     ;Enter XXX for Aqua warp...
     
warp4       .ctext "CODE   FvR BLAUEN SPRUNG ZU LEVEL 17"  
        mess(warp4,colgreenr,1,$48,$96)     ;Enter XXX for Blue warp...
          
warp5       .ctext "CODE   FvR PURPURSPRUNG ZU LEVEL 19"
        mess(warp5,colgreenr,1,$48,$96)     ;Enter XXX for Purple warp...
		
;warp6       .ctext "CODE   FvR PINKEN SPRUNG ZU LEVEL 21"   
        ;mess(warp6,colgreenr,1,$48,$96)     ;Enter XXX for Pink warp... 
		
his         .ctext "HOHE ERGEBNISSE"
		cmess(his,colred,0,$48,his_*2,0)          ;High Scores

enin        .ctext "INITIALE EINGEBEN"
        cmess(enin,colyellow,1,$34,enin_,0)      ;Enter your initials

getout      .ctext "RAUS!!"
        mess(getout,colred2,0,0,$EB)        ;Get Out!!

pub0        .ctext "DRvCKE KNOPF"
        mess(pub0,colwhite,0,0,$DC)         ;Push Button

pub1        .ctext "FvR HpHERE SPRvNGE"
        mess(pub1,colwhite,1,0,$D0)         ;For Higher Jumps

gtsc        .ctext "KLASSE SCORE" 
        cmess(gtsc,colgreenr,0,$48,gtsc_*2,0)      ;Great Score

ledit       .ctext "MAJOR HAVOC LEVEL EDITOR"
		cmess(ledit,colwhite,1,0,ledit_,0)		;Major Havoc Level Editor

cmodd        .ctext "DEMOMODUS"
		cmess(cmodd,colorange,1,$2e,cmodd_,0)     ;Demo Mode

cmode       .ctext "FREISPIEL"
        cmess(cmode,colgreen,1,$2e,cmode_,0)      ;Free Play

cmod1       .ctext "1 MvNZE 2 SPIELE"
        cmess(cmod1,colgreen,1,$2e,cmod1_,0)      ;1 coin 2 plays

cmod2       .ctext "1 MvNZE 1 SPIEL"
        cmess(cmod2,colgreen,1,$2e,cmod2_,0)      ;1 coin 1 play

cmod3       .ctext "2 MvNZEN 1 SPIEL"
        cmess(cmod3,colgreen,1,$2e,cmod3_,0)      ;2 coins 1 play


mwred       .ctext "ROT "                   ; .ctext "-> RACCOURCI ROUGE----- "
        mess(mwred,colred2,1,$a2,$C4)       ;Red (For Warp Description)

mwyel       .ctext "GELB "
        mess(mwyel,colyellow,1,$a2,$C4)     ;Yellow 

mwgrn       .ctext "GRvN "
        mess(mwgrn,colgreen,1,$a2,$C4)      ;Green

mwaqu       .ctext "AQUA "   
        mess(mwaqu,colcyan,1,$a2,$C7)       ;Aqua
      
mwblue      .ctext "BLAU "  
        mess(mwblue,colblue,1,$a2,$C7)      ;Blue
     
mwpurp      .ctext "PURPUR "      
        mess(mwpurp,colpurple,1,$a2,$C4)    ;Purple
   
mwpink      .ctext "PINK " 
        mess(mwpink,colpink,1,$a2,$C7)      ;Pink
           
promise     .ctext "DAS VERSPROCHENE ENDE"
		mess(promise,colwhite,1,$00,$00)	;The Promised End

;MESS2
text0       .ctext "   DU BIST MAJOR HAVOC, DER ANFvHRER"
		mess(text0,colwhite,1,$10,$90)     

text1       .ctext "EINES MUTIGEN WURFES VON KLONEN."
		mess(text1,colwhite,1,$10,$90)

text2       .ctext "DU BIST SIE, SIE SIND DU, ALLE VON EINEM"
		mess(text2,colwhite,1,$10,$90)

text3       .ctext "EINER FvR ALLE KwMPFEN FvR DIE MENSCHHEIT..."
		mess(text3,colwhite,1,$10,$90)

text4       .ctext "  VOR URZEITEN vBERRANTE DAS BpSE VAXXIANISCHE"
		mess(text4,colcyan,1,$10,$90)

text5       .ctext " IMPERIUM DIE GALAXIE, DIE MEISTEN DEINER VORFAHREN" 
		mess(text5,colcyan,1,$10,$90)

text6       .ctext "WURDEN VERSKLAVT UND ZUR VAXXIANISCHEN HEIMATWELT"
		mess(text6,colcyan,1,$10,$90)

text7       .ctext "MAYNARD GEBRACHT. WENIGE WISSENSCHAFTLER ENTKAMEN."
		mess(text7,colcyan,1,$10,$90)

text8       .ctext "  HEUTE IST DAS IMPERIUM PRAKTISCH VERSCHWUNDEN."
		mess(text8,colgreen,1,$10,$90)

text9       .ctext "ABER VAXXIANISCHE RAUMSTATIONEN, KONTROLLIERT UND"
		mess(text9,colgreen,1,$10,$90)

text10      .ctext "VERTEIDIGT VON ROBOTERN, PATROLLIEREN WEITERHIN"
		mess(text10,colgreen,1,$10,$90)

text11      .ctext "DIE GALAXIE UND GEFwHRDEN ALLE."
		mess(text11,colgreen,1,$10,$90)

text12      .ctext "   DIE KLEINE GRUPPE WISSENSCHAFTLER KLONTE DICH,"
		mess(text12,colyellow,1,$10,$90)

text13      .ctext "MAJOR HAVOC, UM MIT DEM KATASTROFLIEGER DURCH EIN"
		mess(text13,colyellow,1,$10,$90)

text14      .ctext "WURMLOCH ZU FLIEGEN UND DIE KLONARMEE ANZUFvHREN"
		mess(text14,colyellow,1,$10,$90)

text15      .ctext "GEGEN DIESE GEFvRCHTETE ROBOTERARMADA."
		mess(text15,colyellow,1,$10,$90)

text16      .ctext "ZERSTpRE DIE FEINDLICHEN RAUMSTATIONEN UND LANDE"
		mess(text16,colyellow,1,$10,$90)

text17      .ctext "AUF DEM PLANETEN DER VAXX, UM DEINE LEUTE ZU BEFREIEN."
		mess(text17,colyellow,1,$10,$90)

text18      .ctext "KLONE SIND UMSONST, WIR VERSCHENKEN SIE."
		mess(text18,colpurple,1,$10,$90)

text19      .ctext "HIN UND WIEDER KANNST DU SOGAR EINEN BONUSKLON BEKOMMEN."
		mess(text19,colpurple,1,$10,$90)

textend     .ctext "----TEXTENDE----"
		mess(textend,colblue,1,$10,$90)

;NEW STUFF

cretx00     .ctext ".........................................."
		mess(cretx00,colwhite,1,$10,$90)

cretx01     .ctext " "
		mess(cretx01,colwhite,1,$10,$90)

cretx02     .ctext "       --- ORIGINALES ATARI TEAM ---" 
		mess(cretx02,colred,1,$10,$90)			;Design Crew

cretx03     .ctext " "
		mess(cretx03,colwhite,1,$10,$90)

cretx04     .ctext "OWEN RUBIN   - ORIGINALES KONZEPT,DESIGN,CODE"
		mess(cretx04,colyellow,1,$10,$90)

cretx05     .ctext "MARK CERNY   - DESIGN,CODE,OPTIMIERUNG"
		mess(cretx05,colyellow,1,$10,$90)

cretx06     .ctext "LYLE RAINS   - KUNST,ANIMATION"
		mess(cretx06,colyellow,1,$10,$90)

cretx07     .ctext "MORGAN HOFF  - PROJEKTMANAGER,P.I.T.R."
		mess(cretx07,colyellow,1,$10,$90)

cretx08     .ctext "RICK MONCRIEF- RETTER,PROJEKTMANAGER"
		mess(cretx08,colyellow,1,$10,$90)

cretx09     .ctext "DOUG SNYDER  - HW DESIGN,ENTWICKLUNG"
		mess(cretx09,colyellow,1,$10,$90)

cretx10     .ctext "STEVE CALFEE - ABTEILUNGSLEITER,IDEEN"
		mess(cretx10,colyellow,1,$10,$90)

cretx11     .ctext "MIKE ALBAUGH - BASISCODE,TOOLS,SERVER"
		mess(cretx11,colyellow,1,$10,$90)

cretx12     .ctext "DAVE SHEPPARD- VAX SUPPORT,ASSEMBLER,TOOLS"
		mess(cretx12,colyellow,1,$10,$90)			;Design Crew

cretx13     .ctext " "
		mess(cretx13,colyellow,1,$10,$90)

cretx14     .ctext "       --- 'DAS VERSPROCHENE ENDE' TEAM ---" 
		mess(cretx14,colwhite,1,$10,$90)

cretx15     .ctext " "
		mess(cretx15,colblack,1,$10,$90)

cretx16     .ctext "              OWEN RUBIN" 
		mess(cretx16,colpurple,1,$10,$90)

cretx17     .ctext "              JESS ASKEY" 
		mess(cretx17,colpurple,1,$10,$90)

cretx18     .ctext "              BRYAN ROTH"
		mess(cretx18,colpurple,1,$10,$90)

cretx19     .ctext "                JERKY"
		mess(cretx19,colpurple,1,$10,$90)

cretx20     .ctext "         SCOTT451     JON K."
		mess(cretx20,colorange,1,$10,$90)

cretx21     .ctext "         LUKE D.      MARK S."
		mess(cretx21,colorange,1,$10,$90)

cretx22		.ctext "         JEROME V.    THOMAS S."
		mess(cretx22,colorange,1,$10,$90)

cretx23		.ctext "                HAXRUS"
		mess(cretx23,colblack,1,$10,$90)
		
cretx24		.ctext " "
		mess(cretx24,colblack,1,$10,$90)
		
cretx25     .ctext "    UPDATES VIA MHEDIT.ASKEY.ORG"
		mess(cretx25,colwhite,1,$10,$90)

cretx26     .ctext " "
		mess(cretx26,colblack,1,$10,$90)

cretxend    .ctext " "
		mess(cretxend,colwhite,1,$10,$90)



txtcmfr     .ctext "   KLONE SIND UMSONST, WIR VERSCHENKEN SIE."
		mess(txtcmfr,colpurple,1,$10,$90)       ;Story Cost - Free

texteot     .ctext "           -----TEXTENDE-----" 
		mess(texteot,colblue,1,$10,$90)			;End of Text 

;Fort this group the mess macro has a 'picture index' instead of a Y Position since that is not used
sco0        .ctext "LISTE DER GEGNER"
		mess(sco0,colcyan,1,0,$D6)     ;Enemy list    

scoln1      .ctext "  ............................................."
		mess(scoln1,colwhite,1,0,$80)

sco2a       .ctext "WELTRAUMGEGNER"
		mess(sco2a,colred,1,0,$D9)

sco3        .ctext "  FISCHOIDEN        1000           "   
		mess(sco3,colyellow,1,1,$9A)
   
scospc1    	.ctext "                                  "
		mess(scospc1,colblack,1,0,$9A)

sco4        .ctext "  FLIEGOIDEN         500           "
		mess(sco4,colyellow,1,2,$9A)

scospc2    	.ctext "                                  " 
		mess(scospc2,colblack,1,0,$9A)

sco5        .ctext "   DREHOIDEN         500           "
		mess(sco5,colyellow,1,3,$9A)

scoln2      .ctext "  ............................................."
		mess(scoln2,colwhite,1,0,$80)

sco6        .ctext "LABYRINTHGEGNER"
		mess(sco6,colred,1,0,$DC)

sco7        .ctext "   PYROIDEN         1000           "
		mess(sco7,colyellow,1,4,$9A)

scospc3    	.ctext "                                  "        
		mess(scospc3,colblack,1,0,$9A)

sco8        .ctext "  PERKOIDEN         1000           "
		mess(sco8,colyellow,1,5,$9A)

scospc4    	.ctext "                                  "  
		mess(scospc4,colblack,1,0,$9A)
      
sco8m       .ctext "   MAXOIDEN         9000           "
		mess(sco8m,colyellow,1,6,$9A)
 
scoln3      .ctext "  ............................................."
		mess(scoln3,colwhite,1,0,$80)

sco10       .ctext "ANDERE PUNKTE"
		mess(sco10,colred,1,0,$DC)

sco11       .ctext "  REAKTOID          5000           "
		mess(sco11,colyellow,1,7,$9A)

scospc5    	.ctext "                                  "     
		mess(scospc5,colblack,1,0,$9A)
  
sco12       .ctext "    OXOID           100           "
		mess(sco12,colyellow,1,8,$9A) 

scospc6    	.ctext "                                  " 
		mess(scospc6,colblack,1,0,$9A)    
       
sco13       .ctext "    OXOID         VARIABEL           "
		mess(sco13,colyellow,1,9,$9A)

scoln4      .ctext "  ............................................."
		mess(scoln4,colwhite,1,0,$80)
 
sco14       .ctext "INFO/PUNKTE"
		mess(sco14,colred,1,0,$DC)

sco15       .ctext "SAUERSTOFFBONUS IST 100 x O2"
		mess(sco15,colyellow,1,0,$90)

sco16       .ctext "BEI LEVELENDE."
		mess(sco16,colyellow,1,0,$90)

sco17       .ctext "IM WELTALL IST DER ENDBONUS UMSO HpHER,"
		mess(sco17,colyellow,1,0,$90)

sco18       .ctext "JE SCHNELLER MAN DEN LEVEL BEENDET."
		mess(sco18,colyellow,1,0,$90)

scoln5      .ctext "  ............................................."
		mess(scoln5,colwhite,1,0,$80)

sco20       .ctext "INSTRUKTIONEN"
		mess(sco20,colblue,1,0,$DC)

sco21       .ctext "1. NUTZE TAKTISCHEN SCAN ZUM SPRUNG UND ZUR INFO."
		mess(sco21,colblue,1,0,$90)

sco22       .ctext "2. ZERSTpRE VERTEIDIGUNG DES MUTTERSCHIFFS"
		mess(sco22,colblue,1,0,$90)

sco23       .ctext "3. LANDE AUF DER WEISSEN PLATTFORM"
		mess(sco23,colblue,1,0,$90)

sco24       .ctext "4. FINDE UND BERvHRE DEN REAKTOIDEN IM LABYRINTH"
		mess(sco24,colblue,1,0,$90)

sco25       .ctext "5. VERLASSE DAS LABYRINTH BEVOR ES EXPLODIERT."
		mess(sco25,colblue,1,0,$90)

sco26       .ctext " "
		mess(sco26,colred,1,0,$FA)

sco27       .ctext "  --HALTE SPRUNGKNOPF FvR HpHERE SPRvNGE--"
		mess(sco27,colred,1,0,$90) 


endtx00     .ctext "GEGEN ALLE WIDRIGKEITEN HABEN DU," 	
		mess(endtx00,colcyan,1,$10,$90)

endtx01     .ctext "MAJOR HAVOC, UND DEINE KLONE ES ZUR VAXXIANISCHEN"  
		mess(endtx01,colcyan,1,$10,$90)

endtx02     .ctext "HEIMATWELT GESCHAFFT UND DADURCH DEM VAXXIANISCHEN"
		mess(endtx02,colcyan,1,$10,$90)

endtx03     .ctext "IMPERIUM DEN TODESSTOSS VERSETZT. DIE GALAXIE"
		mess(endtx03,colcyan,1,$10,$90)

endtx04     .ctext "IST NUN SICHERER."
		mess(endtx04,colcyan,1,$10,$90)

endtx05 	.ctext " "
		mess(endtx05,colcyan,1,$10,$90)

endtx06     .ctext "DEIN VOLK WURDE BEFREIT UND KANN NACH HAUSE,"
		mess(endtx06,colcyan,1,$10,$90)

endtx07     .ctext "UM DORT IN FREIHEIT VON DEN VAXX ZU LEBEN,"
		mess(endtx07,colcyan,1,$10,$90)

endtx08     .ctext "IHREN HOBBIES ZU FRpHNEN, EINE FIREWALL" 
		mess(endtx08,colcyan,1,$10,$90)

endtx09    	.ctext "AUFZUBAUEN ODER FISCHEN ZU GEHEN.."
		mess(endtx09,colcyan,1,$10,$90)

endtx0a     .ctext " "
		mess(endtx0a,colcyan,1,$10,$90)
 
endtx0b     .ctext "ABER AUF DICH WARTET WEITERE ARBEIT."
		mess(endtx0b,colcyan,1,$10,$90)

endtx0c     .ctext "EINIGE VAXXIANISCHE RAUMSTATIONEN"
		mess(endtx0c,colcyan,1,$10,$90)

endtx0d     .ctext "WANDERN NOCH DURCH DIE GALAXIE, SICH"
		mess(endtx0d,colcyan,1,$10,$90)

endtx0e     .ctext "NICHT BEWUSST, DASS IHRE HEIMATWELT"
		mess(endtx0e,colcyan,1,$10,$90)

endtx0f     .ctext "GEFALLEN IST."
		mess(endtx0f,colcyan,1,$10,$90)

endtx10     .ctext " "
		mess(endtx10,colcyan,1,$10,$90)

endtx11     .ctext "DU UND DEINE KLONE KpNNEN NUN IN DER"
		mess(endtx11,colcyan,1,$10,$90)

endtx12     .ctext "GALAXIE UMHERSPRINGEN, UM DIESEN LETZTEN"
		mess(endtx12,colcyan,1,$10,$90)

endtx13     .ctext "REST AN RAUMSTATIONEN IN DEN MvLLSAMMLER"
		mess(endtx13,colcyan,1,$10,$90)

endtx14     .ctext "ZU WERFEN."
		mess(endtx14,colcyan,1,$10,$90)

endtx15     .ctext " "
		mess(endtx15,colcyan,1,$10,$90)

endtx16     .ctext "GRATULATION MAJOR HAVOC, SEHR GUT GEMACHT."
		mess(endtx16,colcyan,1,$10,$90)

endtx17     .ctext " "
		mess(endtx17,colcyan,1,$10,$90)

endtx18     .ctext " "
		mess(endtx18,colcyan,1,$10,$90)

endtx19     .ctext " --OWEN R. RUBIN - GvTIGER OVERLORD"
		mess(endtx19,colcyan,1,$10,$90)

endtx1a     .ctext " "
		mess(endtx1a,colcyan,1,$10,$90)

endtx1b     .ctext " "
		mess(endtx1b,colcyan,1,$10,$90)

endtx1c     .ctext " "
		mess(endtx1c,colcyan,1,$10,$90)

endtxxx     .ctext " "
		mess(endtxxx,colcyan,1,$10,$90) 


