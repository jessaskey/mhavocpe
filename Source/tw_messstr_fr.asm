

;******************************************************************************
    .sbttl "TW Messages"
;******************************************************************************
;* mess macro takes: Message Pointer, Color, Scale, Y Position, X Position
;******************************************************************************
; FRENCH
;**********************************************


gamov       .ctext "FIN"
press       .ctext "PRESSEZ START"
addm2       .ctext "POUR DfMARRER AU NIVEAU "					;insertion: a space after "NIVEAU"
addm1       .ctext " PRESSEZ START DANS LES    SECONDES"		;1R: so a space before "PRESSEZ" (added here) allows to center the sentence
playr       .ctext "JOUEUR " 
tmes0       .ctext "DfTRUISEZ LES ROBOTS PISCIFORMES"			;6.5L todo (calculated considering the extra char./english and divided/2)
tmes1       .ctext "DfTRUISEZ LES NAVIRES ENNEMIS"				;5L todo(see above to calculate the position of the french text) 
tmes2       .ctext "MANOEUVREZ DANS LE LABYRINTHE"            	;4L todo
tmes3       .ctext "TIREZ SUR LES OPPOSANTS, FUYEZ"          	;2L todo
tmes4       .ctext "DfTRUISEZ LES BOUCLIERS ENNEMIS"			;5L todo
hint0       .ctext "AMARREZ-VOUS AU PONT BLANC"					;2L todo and "POSEZ-VOUS SUR LE" replaced by "AMARREZ-VOUS AU" (done)

oxyg        .ctext "OXYGeNE"									;1L todo
reac        .ctext "RfACTEUR"									;1L todo
bonus       .ctext "BONUS "										;insertion: a space after "BONUS" (done)
mlevel      .ctext "NIV."										;"NIVEAU" replaced by "NIV." (done) ("NIVEAU" is too big unfortunately)
insert      .ctext "INSfREZ DES PIeCES"
atari       .ctext "cMCMLXXXIII ATARI"        					;little c is Copyright symbol     erased: a space before "c.text" (done)

#IF (LEVEL_EDITOR > 0)
credi       .ctext "MHEDIT.ASKEY.ORG"
#ELSE          
credi       .ctext "CRfDITS"
#ENDIF

bolif       .ctext "BONUS TOUS LES "							;insertion: a space after "LES" (done)
elife       .ctext " VIE(S)sJOUEUR"    
cont        .ctext "CONDITION "
ccrit       .ctext "CRITIQUE"     
cred        .ctext "ROUGE"
cyel        .ctext "JAUNE"
cgrn        .ctext "VERTE"
garbage     .ctext "DfCHETS fJECTfS"
tesser0     .ctext "OCTACHORE ALPHA ENGAGf!"
tesser1     .ctext "OCTACHORE BgTA ENGAGf!"
tesser2     .ctext "OCTACHORE GAMMA ENGAGf!"
tesser3     .ctext "OCTACHORE DZgTA ENGAGf!"      
ahome       .ctext "ALERTE! a TOUTE LA STATION!"
conf        .ctext "CONFIRMf"
mdist       .ctext "RANG "
etyp        .ctext "TYPE D'ENNEMI"
cols        .ctext "FERMETURE"
hold        .ctext "POSSESSION"
figh        .ctext "COMBATTANTS"
sphr        .ctext "SPHfROiDES"
fort        .ctext "FORTERESSE"        
spin        .ctext "'SPINNERS'"									;""SPINNERS" replaced by "'SPINNERS'" (done)        
mainf       .ctext "V. AMIRAL"									;VAISSEAU AMIRAL" replaced by "V. AMIRAL" (done)
warpi       .ctext "---MESSAGE INTERCEPTf---"
warp0       .ctext "UTL    -> RACCOURCI ROUGE -> NIV. 4"
warp1       .ctext "UTL    -> RACCOURCI JAUNE -> NIV. 9"
warp2       .ctext "UTL    -> RACCOURCI VERT  -> NIV. 10"
warp3       .ctext "UTL    -> RACCOURCI 'EAU' -> NIV. 13"          
warp4       .ctext "UTL    -> RACCOURCI BLEU -> NIV. 17"           
warp5       .ctext "UTL    -> RACCOURCI VIOLET -> NIV. 19"
warp6       .ctext "UTL    -> RACCOURCI ROSE -> NIV. 21"                           
his         .ctext "   CLASSEMENT"								;1R (1 space before "CLASSEMENT") (done)				
enin        .ctext "ENTREZ VOS INITIALES"
getout      .ctext "SORTEZ"
pub0        .ctext "MAINTENEZ LE BOUTON"
pub1        .ctext "POUR SAUTER PLUS HAUT"						;"+" (unsupported) replaced by "PLUS" (done)
gtsc        .ctext "  RECORDS" 									;2R (added) "TOP SCORE" replaced by "RECORDS" (I know it is strange in French ;-)
ledit       .ctext "EDITEUR DE NIVEAUX"
cmodd        .ctext "MODE DfMO"
cmode       .ctext "PARTIES GRATUITES"
cmod1       .ctext "1 PIeCE 2 PARTIES"
cmod2       .ctext "1 PIeCE 1 PARTIE"
cmod3       .ctext "2 PIeCES 1 PARTIE"
mwred       .ctext "-> RACCOURCI ROUGE----- "
mwyel       .ctext "-> RACCOURCI JAUNE----- "
mwgrn       .ctext "-> RACCOURCI VERT----- "
mwaqu       .ctext "-> RACCOURCI 'EAU'----- "           
mwblue      .ctext "-> RACCOURCI BLEU----- "      
mwpurp      .ctext "-> RACCOURCI VIOLET----- "          
mwpink      .ctext "-> RACCOURCI ROSE----- "           
promise     .ctext "LA FIN PROMISE"

;MESS2
text0       .ctext "   VOUS gTES LE MAJOR HAVOC, a LA TgTE D'UNE"
text1       .ctext "INTRfPIDE HORDE DE VOS PROPRES CLONES:"
text2       .ctext "UN POUR TOUS, TOUS POUR UN, DANS VOTRE"
text3       .ctext "COMBAT POUR L'HUMANITf....."
text4       .ctext " IL Y A UNE fTERNITf, LE MALfFIQUE EMPIRE VAXXIEN A"      	;1L (1 space erased: before "IL") (done)
text5       .ctext "ENVAHI LA GALAXIE. BEAUCOUP DE VOS ANCgTRES"                ;1L (1 space erased before "ENVAHI")(done)"LA PLUPART" replaced by "BEAUCOUP"
text6       .ctext "ONT fTf ASSERVIS ET EMMENfS a LA BASE VAXXIENNE"			;sentence modified
text7       .ctext "DE MAYNARD. SEULS QUELQUES SAVANTS SE SONT"
text8       .ctext "fCHAPPfS. AUJOURD'HUI CET EMPIRE A PRESQUE DISPARU."
text9       .ctext "MAIS IL RESTE LES STATIONS SPATIALES VAXXIENNES "     		;erased: "CONTRoLfES ET" (done)
text10      .ctext "DfFENDUES PAR DES ROBOTS QUI PATROUILLENT DANS"				;erased	"," (done); insertion "DANS" (done)	
text11      .ctext "LA GALAXIE EN GARDANT VOTRE PEUPLE PRISONNIER."  	    	;modif "ET GARDENT" into "EN GARDANT" (done) ; erased "DANS" (done)
text12      .ctext "   LA PETITE fQUIPE DE SAVANTS VOUS A CLONf, VOUS,"
text13      .ctext "LE MAJOR HAVOC. AVEC VOTRE CATASTROFIGHTER VOUS"
text14      .ctext "NAVIGUEREZ DANS L'ESPACE EN DIRIGEANT VOTRE ARMfE"
text15      .ctext "DE CLONES CONTRE LES REDOUTABLES ROBOTS VAXXIENS."
text16      .ctext "DfTRUISEZ LES STATIONS SPATIALES ENNEMIES ET GAGNEZ"
text17      .ctext "LA PLANeTE VAXX POUR LIBfRER VOTRE PEUPLE."
text18      .ctext "METTEZ DES PIeCES ET FINANCEZ VOTRE ARMfE DE CLONES."   	;modif "INSfEREZ" becomes "METTEZ" (done) and "POUR" becomes "ET" (done)
text19      .ctext "VOUS POUVEZ a TOUT MOMENT GAGNER UN CLONE BONUS."  			;erased: "MgME" (done)
textend        .ctext " "

;NEW STUFF

cretx00     .ctext ".........................................."
cretx01     .ctext " "
cretx02     .ctext "       --- fQUIPE ATARI ORIGINALE ---" 
cretx03     .ctext " "
cretx04     .ctext "OWEN RUBIN   - IDfE ORIGINALE,CONCEPT.,PROG."           	;insertion : 1 space after "-"(done) ; "PROGRAMM." replaced by "PROG."(done)
cretx05     .ctext "MARK CERNY   - CONCEPT.,PROG.,OPTIM.,CRfATION"  			;"PROGRAMM." replaced by "PROG."(done) and "OPTIMISAT." by "OPTIM" (done)
cretx06     .ctext "LYLE RAINS   - ART,ANIMAT.,CRfATION"
cretx07     .ctext "MORGAN HOFF  - CHEF DE PROJET,P.I.T.R."
cretx08     .ctext "RICK MONCRIEF- SAUV.,CHEF DE PROJET"
cretx09     .ctext "DOUG SNYDER  - CONCEPT. MATfRIEL., INGfNIERIE"
cretx10     .ctext "STEVE CALFEE - CHEF DE SERVICE,CRfATION"
cretx11     .ctext "MIKE ALBAUGH - CODE DE BASE,OUTILS,SOLUTIONS"
cretx12     .ctext "DAVE SHEPPARD- SUIVI 'VAX' ,ASSEMBL.,OUTILS"
cretx13     .ctext " "
cretx14     .ctext "       --- fQUIPE DE 'LA FIN PROMISE' ---" 
cretx15     .ctext " "
cretx16     .ctext "              OWEN RUBIN" 
cretx17     .ctext "              JESS ASKEY" 
cretx18		.ctext "              BRYAN ROTH"
cretx19		.ctext "                JERKY"
cretx20		.ctext "         SCOTT451     JON K."
cretx21		.ctext "         LUKE D.      MARK S."
cretx22		.ctext "         JEROME V.    THOMAS S."
cretx23		.ctext "                HAXRUS"
cretx24     .ctext " "															;translators: not added in this file (at the discretion of Jess)
cretx25     .ctext "    MISES a JOUR: MHEDIT.ASKEY.ORG"    						;replaced "MAJ" by "MISES a JOUR:" (done)  2Letters on the left (2L) (done)
cretx26     .ctext " "
cretxend    .ctext " "


txtcmfr     .ctext "   LES CLONES VOUS SONT GRbCIEUSEMENT ATTRIBUfS."
texteot     .ctext "           -----FIN DU TEXTE-----" 


;MESS3
sco0        .ctext "LISTE DES ENNEMIS"
scoln1      .ctext "  ............................................."
sco2a       .ctext "ESPACE"														;erased "ENNEMIS/" (done)
sco3        .ctext "  FISHOiDES         1000           "        
scospc1    	.ctext "                                  "
sco4        .ctext "  FLYBOiDES          500           "
scospc2    	.ctext "                                  " 
sco5        .ctext "   MAZOiDES          500           "
scoln2      .ctext "  ............................................."
sco6        .ctext "LABYRINTHE"													;erased "ENNEMIS/" (done)
sco7        .ctext "   PYROiDES         1000           "
scospc3    	.ctext "                                  "        
sco8        .ctext "  PERKOiDES         1000           "
scospc4    	.ctext "                                  "        
sco8m       .ctext "   MAXOiDES         9000           "
scoln3      .ctext "  ............................................."
sco10       .ctext "AUTRES"
sco11       .ctext "  RfACTOiDE         5000           "
scospc5    	.ctext "                                  "        
sco12       .ctext "     OXOiDE          100           "						;"OXOiDE" 1R and "100" 1L (done)
scospc6    	.ctext "                                  "        
sco13       .ctext "     OXOiDE     VARIABLE           "						;"OXOiDE" 1R and VARIABLE" 3L (done)
scoln4      .ctext "  ............................................."
sco14       .ctext "AUTRES SCORES"
sco15       .ctext "LE BONUS D'OXYGeNE 'O2' EN SORTIE DU LABYRINTHE EST"		;sentence totally modified		
sco16       .ctext "fGALE a 100 FOIS LA QUANTITf D'O2 RESTANTE."				;sentence totally modified
sco17       .ctext "DANS L'ESPACE, LA RAPIDITf DE DESTRUCT. DES ENNEMIS"		;sentence totally modified
sco18       .ctext "EST CORRfLfE AU NOMBRE DE POINTS BONUS ATTRIBUfS."			;sentence totally modified
scoln5      .ctext "  ............................................."
sco20       .ctext "INSTRUCTIONS"
sco21       .ctext "1. UT. LE SCAN TACTILE POUR LES RACCOURCIS ET INFOS"		;"UTILISEZ" replaced by "UT." " + "(unsupport char.) replaced by "ET" (done)
sco22       .ctext "2. DfTRUISEZ LES DfFENSES DU VAISSEAU AMIRAL"
sco23       .ctext "3. AMARREZ-VOUS AU PONT DU VAISSEAU AMIRAL VAINCU"			;"POSEZ-VOUS SUR LE" replaced by "AMARREZ-VOUS AU" erased "BLANC"(done)
sco24       .ctext "4. LABYRINTHE: TROUVEZ LE RfACTOiDE ET TOUCHEZ-LE"          ;erased "DANS LE"; "," replaced by ":" (done)
sco25       .ctext "5. SORTEZ DU LABYRINTHE AVANT SON EXPLOSION"				;"L'EXPLOSION DU RfACTOiDE" replaced by "SON EXPLOSION" (done)
sco26       .ctext " "
sco27       .ctext "NB: SAUTEZ PLUS HAUT EN MANTENANT LE BOUTON 'SAUT'"			;2L ; "NOTE--" replaced by "NB:" (done); sentence totally modified 


endtx00     .ctext "MALGRf TOUTES LES EMBwCHES, VOUS, MAJOR HAVOC,"   
endtx01     .ctext "AVEC VOS CLONES, AVEZ ATTEINT" 
endtx02     .ctext "LA BASE VAXXIENNE ET PORTf UN COUP FATAL a" 				;"'" replaced by " ET" (done)
endtx03     .ctext "L'EMPIRE VAXXIEN QUE VOUS AVEZ "							;"ET LES" replaced by "QUE VOUS" (done)
endtx04     .ctext "ELIMINfS DE LA GALAXIE."
endtx05     .ctext " "															;4L before .ctext (done but verify if ok)
endtx06     .ctext "VOTRE PEUPLE A PU RETOURNER"
endtx07     .ctext "SUR SES TERRES, Y PgCHER, Y fRIGER UNE"
endtx08     .ctext "PROTECTION, SE FORMER AU COMBAT" 
endtx09     .ctext "ET SE LIBfRER DU CONTRoLE DE VAXX."							;4L (done but verify if ok)
endtx0a     .ctext "a TERME, IL POURRA PROFITER DE SA LIBERTf"
endtx0b     .ctext "RETROUVfE."
endtx0c     .ctext " "
endtx0d     .ctext "MAIS POUR VOUS, IL RESTE UNE MISSION:"
endtx0e     .ctext "QUELQUES STATIONS SPATIALES VAXXIENNES"
endtx0f     .ctext "OCCUPENT ENCORE LA GALAXIE SANS SAVOIR"
endtx10     .ctext "QUE LEUR BASE A fTf DfFAITE."
endtx11     .ctext " "
endtx12     .ctext "MAINTENANT, AVEC VOS CLONES, VOUS POUVEZ"
endtx13     .ctext "VOUS TfLfPORTER DANS LA GALAXIE"
endtx14     .ctext "POUR PRfCIPITER CES STATIONS"
endtx15     .ctext "DANS LA DfCHARGE SPATIALE."									;"DeCHARGE" replaced by "DfCHARGE" (done)
endtx16     .ctext " "
endtx17     .ctext "FfLICITATIONS MAJOR HAVOC, MISSION ACCOMPLIE."
endtx18     .ctext " "
endtx19     .ctext " "
endtx1a     .ctext " --OWEN R. RUBIN - SOUVERAIN BIENVEILLANT"
endtx1b     .ctext " "
endtx1c     .ctext " "
endtxxx     .ctext " "



		cmess(gamov,colyellow,1,$24,gamov_,0)     ;Game Over
        cmess(press,colred,1,$24,press_,0)        ;Press Start
        cmess(addm2,colcyan,1,-6,addm2_,2)        ;Rest of Message
        cmess(addm1,colcyan,1,4,addm1_,0)         ;Add Time Message
        cmess(playr,colyellow,1,$24,playr_,1)     ;Player
		mess(tmes0,colcyan,1,$48,$C7)       	;Destroy fish robotos
        mess(tmes1,colcyan,1,$48,$C7)       	;Shoot all fighters
        mess(tmes2,colcyan,1,$48,$C1)       ;Hit...    
        mess(tmes3,colcyan,1,$48,$A3)       ;Maneuver through maze
        mess(tmes4,colflash,1,$48,$C3)      ;Destroy enemy shields   
        cmess(hint0,colred,1,$46,hint0_,0)        ;Dock...		
        mess(oxyg,colgreen,1,10d,$EB)       ;Oxygen
        mess(reac,colred,1,-30d,$AA)        ;Reactor
        mess(bonus,colgreen,1,$54,$2F)      ;Bonus
        mess(mlevel,colyellow,1,-$50,$9E)   ;Level
        cmess(insert,colred,1,$24,insert_,0)       ;Insert Coins
		cmess(atari,colbluer,1,$98,atari_,0)		;MCMLXXXIII Atari


#IF (LEVEL_EDITOR > 0)
        cmess(credi,colcyan,1,$18,credi_,0)       ;MHEDIT.ASKEY.ORG
#ELSE          
		cmess(credi,colcyan,1,$18,credi_,4)       ;Credits
#ENDIF

        mess(bolif,colcyan,1,$a0,$C8)       ;Bonus Every
        mess(elife,colpurple,1,$a8,$CD)     ;Lives
        mess(cont,colwhite,1,$60,$D0)       ;Condition   
        mess(ccrit,colred,1,$60,$10)        ;Critical
        mess(cred,colred,1,$60,$10)         ;Red
        mess(cyel,colyellow,1,$60,$10)      ;Yellow
        mess(cgrn,colgreen,1,$60,$10)       ;Green
        mess(garbage,colred,1,$54,$98)      ;Garbage Ejected
		cmess(tesser0,colflash,1,$4C,tesser0_,0)	;Alpha Tesseract engaged!
		cmess(tesser1,colflash,1,$4C,tesser1_,0)	;Beta Tesseract engaged!
		cmess(tesser2,colflash,1,$4C,tesser2_,0)	;Gamma Tesseract engaged!
		cmess(tesser3,colflash,1,$4C,tesser3_,0)	;Zeta Tesseract engaged!
        mess(ahome,colflash,1,$54,$A0)      ;Approaching Homeworld!
        mess(conf,colwhite,2,$38,$42)       ;Confirmed
        mess(mdist,colcyan,2,-$38,$D4)      ;Range
        mess(etyp,colcyan,2,-$3e,$D4)       ;Enemy Type
        mess(cols,colcyan,2,-$38,$04)       ;Closing
        mess(hold,colcyan,2,-$38,$04)       ;Holding
        mess(figh,colcyan,2,-$3e,$04)       ;Fighters
        mess(sphr,colcyan,2,-$3e,$04)       ;Spheroids
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
        cmess(his,colred,0,$48,his_*2,0)          ;High Scores
        cmess(enin,colyellow,1,$34,enin_,0)      ;Enter your initials
        mess(getout,colred2,0,0,$EB)        ;Get Out!!
        mess(pub0,colwhite,0,0,$DC)         ;Push Button
        mess(pub1,colwhite,1,0,$D0)         ;For Higher Jumps
        cmess(gtsc,colgreenr,0,$48,gtsc_*2,0)      ;Great Score
		mess(ledit,colwhite,1,0,$B0)		;Major Havoc Level Editor
		cmess(cmodd,colorange,1,$2e,cmodd_,0)     ;Demo Mode
        cmess(cmode,colgreen,1,$2e,cmode_,0)      ;Free Play
        cmess(cmod1,colgreen,1,$2e,cmod1_,0)      ;1 coin 2 plays
        cmess(cmod2,colgreen,1,$2e,cmod2_,0)      ;1 coin 1 play
        cmess(cmod3,colgreen,1,$2e,cmod3_,0)      ;2 coins 1 play
        mess(mwred,colred2,1,$a2,$C4)       ;Red (For Warp Description)
        mess(mwyel,colyellow,1,$a2,$C4)     ;Yellow 
        mess(mwgrn,colgreen,1,$a2,$C4)      ;Green 
        mess(mwaqu,colcyan,1,$a2,$C7)       ;Aqua 
        mess(mwblue,colblue,1,$a2,$C7)      ;Blue     
        mess(mwpurp,colpurple,1,$a2,$C4)    ;Purple
        mess(mwpink,colpink,1,$a2,$C7)      ;Pink
		mess(promise,colwhite,1,$00,$00)	;The Promised End

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
		mess(cretx23,colblack,1,$10,$90)
		mess(cretx24,colblack,1,$10,$90)
		mess(cretx25,colwhite,1,$10,$90)
		mess(cretx26,colblack,1,$10,$90)
		mess(cretxend,colwhite,1,$10,$90)
		mess(txtcmfr,colpurple,1,$10,$90)       ;Story Cost - Free
		mess(texteot,colblue,1,$10,$90)			;End of Text 

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




