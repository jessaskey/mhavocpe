
;***********************************************
; Random Maze Strings
; 
; GERMAN
;**********************************************

mzr00a	.ctext "KLEIDUNG EBENE 2..."    	;"LINGERIE FLOOR 2..."
mzr00b 	.ctext "REAKTOR EBENE 1"			;"REACTOR FLOOR 1"
mzr01a 	.ctext "WIR STELLEN EIN..."			;"NOW HIRING..."				
mzr01b	.ctext "ANFRAGEN: P.A. AUF EBENE 3"	;"INQUIRE AT HR ON FLOOR 3"	
mzr02a 	.ctext "ABFALLLOGISTIK"				;"GARBAGE FACILITY IS"		
mzr02b	.ctext "IST AUF EBENE 2"			;"LOCATED ON FLOOR 2"			
mzr03a  .ctext "SELBSTZERSTpRUNGSSYSTEM"	;"SELF-DESTRUCT SYSTEM IS"	
mzr03b	.ctext "VORHANDEN IM KELLER"		;"LOCATED IN BASEMENT"
mzr04a	.ctext "NUR FvR AUTHORISIERTE ROBOTER"  ;"AUTHORIZED SERVICE ROBOTS ONLY"	
mzr05a 	.ctext "ALLE HOFFNUNG IST SINNLOS..."	;"ABANDON ALL HOPE..."		
mzr05b	.ctext "IHR, DIE IHR EINTRETET"		;"YE WHO ENTER HERE"			
mzr06a 	.ctext "ICH KANN DAS KAUM GLAUBEN"	;"I REALLY CANT BELIEVE YOU"
mzr06b	.ctext "SOWEIT, SOGUT!"				;"MADE IT THIS FAR!"
mzr07a 	.ctext "ES GIBT KEIN ZURvCK MEHR"	;"THERE IS NO TURNING BACK NOW"
mzr08a 	.ctext "WO SEHEN SIE DENN HIN?"		;"WHAT ARE YOU LOOKING AT?"	
mzr09a 	.ctext "DENKEN SIE DARAN, WIR"		;"REMEMBER, THIS FACILITY"
mzr09b	.ctext "SCHLIESSEN UM 22 UHR!"		;"CLOSES AT 10 PM!"
mzr0aa 	.ctext "WANN WERDEN SIE GEHEN?"		;"WHEN ARE YOU GOING TO LEAVE?"
mzr0ba 	.ctext "WEISSE ZONE IST NUR ZUM"	;"WHITE ZONE IS FOR LOADING"
mzr0bb	.ctext "LADEN UND ENTLADEN"			;"AND UNLOADING ONLY"
mzr0ca 	.ctext "SIE KpNNEN DAS SCHIFF"		;"YOU CANT PARK THAT"
mzr0cb	.ctext "DORT NICHT PARKEN!"			;"SHIP THERE!"
mzr0da 	.ctext "10 SEK. ZUR SELBSTZERSTpRUNG"	;"10 SECONDS TILL SELF-DESTRUCT"
mzr0db	.ctext "...NEE, NICHT ERNST GEMEINT!"	;"... JUST KIDDING!"
mzr0ea 	.ctext "IHRE PARKUHR IST ABGELAUFEN"	;"YOUR PARKING METER HAS EXPIRED"
mzr0fa 	.ctext "ALTER, WO IST MEIN SCHIFF?"	;"DUDE, WHERES MY SHIP?"

;********************************************************************
;  Dif 0 - Maze A - Level 1 - Level 1
;********************************************************************
mzh00a  .ctext "FOLGE DEN PFEILEN ZUM REAKTOR"
	;.ctext "FOLLOW ARROWS AND TOUCH REACTOR"
        czmess(mzh00a,$50,mzh00a_)	
		
mzh00b  .ctext "ENTKOMME BEVOR DIE ZEIT AUSLwUFT"
	;.ctext "EXIT MAZE BEFORE TIMER REACHES ZERO"
        czmess(mzh00b,$48,mzh00b_)
		
;********************************************************************
;  Dif 0 - Maze B - Level 2 - Level 2
;********************************************************************
mzh01a  .ctext "NUTZE SCANNER ZUM TRIGGERN"
	;.ctext "USE SCANNER TO TRIGGER REACTOID"
        czmess(mzh01a,$50,mzh01a_)
mzh01b  .ctext "VON REACTOIDS WENN PFAD FREI"
	;.ctext "WHEN PATH IS CLEAR"
        czmess(mzh01b,$48,mzh01b_)

;********************************************************************
;  Dif 0 - Maze C - Level 3 - Level 3
;********************************************************************
mzh02a  .ctext "NUTZE SCHILDE ZUR ZERSTpRUNG "
	;.ctext "USE SHIELDS TO DESTROY "
        czmess(mzh02a,$50,mzh02a_)
mzh02b  .ctext "EINES PERKOIDS ODER PYROIDS"
	;.ctext "1 PERKOID ROBOT OR PYROID"
        czmess(mzh02b,$48,mzh02b_)
		
;********************************************************************
;  Dif 1 - Maze D - Level 4 - Level 4
;********************************************************************
mzh03a  .ctext "FINDE DIE TvRSCHLvSSEL"
	;.ctext "FIND KEYS TO OPEN DOORS"
        czmess(mzh03a,$50,mzh03a_)
mzh03b  .ctext "GELBES O2 IST WERTVOLLER"
	;.ctext "YELLOW OXYGEN IS MORE VALUABLE    "
        czmess(mzh03b,$48,mzh03b_)
		
;********************************************************************
;  Dif 1 - Maze A - Level 5 - Level 5
;********************************************************************
mzh10a  .ctext "ROTE SCHALTER FEUERN FEUERBwLLE AB"
	;.ctext "RED TRIP PADS LAUNCH DEADLY FIREBALLS"
        czmess(mzh10a,$50,mzh10a_)
		
;********************************************************************
;  Dif 1 - Maze B - Level 6 - Level 6
;********************************************************************
mzh11a  .ctext "IST DER EINBAHNWEG DER EINZIGE WEG?"
	;.ctext "IS THAT ONE WAY THE ONLY WAY?"
        czmess(mzh11a,$50,mzh11a_)

;********************************************************************
;  Dif 1 - Maze D - Level 8 - Level 8
;********************************************************************
mzh13a  .ctext "SI VOUS ÊTES DOUf, VOUS TROUVEREZ!"
        czmess(mzh13a,$50,mzh13a_)	;,$8E)	
		
;********************************************************************
;  Dif 1 - Maze C - Level 7 - Level 7
;********************************************************************
mzh12a  .ctext "KAUM ZU GLAUBEN - SO WEIT GEKOMMEN!"
	;.ctext "I CANT BELIEVE YOU MADE IT THIS FAR"
        czmess(mzh12a,$50,mzh12a_)
		
;********************************************************************
;  Dif 2 - Maze A - Level 9 - Level 9
;********************************************************************
mzh20a  .ctext "ACHTUNG: WECHSELNDE WwNDE UND BpDEN"
	;.ctext "WATCH OUT FOR CHANGING WALLS AND FLOORS"
        czmess(mzh20a,$50,mzh20a_)
		
;********************************************************************
;  Dif 2 - Maze B - Level 10 - Level 10
;********************************************************************		
mzh21a  .ctext "DER WEG IN IST NICHT DER WEG HERAUS"
        czmess(mzh21a,$50,mzh21a_)	;,$A3)
		
;********************************************************************
;  Dif 2 - Maze C - Level 11 - Level 11
;********************************************************************
mzh22a  .ctext "FOLGE DEM PFAD DURCH WECHSELNDE WwNDE"
	;.ctext "FOLLOW THE PATH THROUGH CHANGING WALLS"
        czmess(mzh22a,$50,mzh22a_)
		
;********************************************************************
;  Dif 3 - Maze D - Level 12 - Level 12
;********************************************************************
mzh23a  .ctext "WEITER SPIELEN, DIE HEIMATWELT IST NAH"
	;.ctext "KEEP PLAYING, THE HOME WORLD IS NEAR"
        czmess(mzh23a,$50,mzh23a_)
mzh23b  .ctext "WIR MEINEN ES DIESMAL ERNST!"
	;.ctext "WE MEAN IT THIS TIME!"
        czmess(mzh23b,$48,mzh23b_)
		
;********************************************************************
;  Dif 3 - Maze A - Level 13 - Level 13
;********************************************************************
mzh30a  .ctext "NUTZEN SIE IHRE ZEIT WEISE"
	;.ctext "USE YOUR TIME WISELY"
        czmess(mzh30a,$50,mzh30a_)
		
;********************************************************************
;  Dif 3 - Maze B - Level 14 - Level 14
;********************************************************************
mzh31a  .ctext "IONENKANONENHILFE IST NICHT TpDLICH"	;.ctext "ION CANNON SUPPORT IS NOT DEADLY"
        czmess(mzh31a,$50,mzh31a_)
		
;********************************************************************
;  Dif 3 - Maze C - Level 15 - Level 15
;********************************************************************
mzh32a  .ctext "BALANCE IST DER SCHLvSSEL ZU ALLEM"
	;.ctext "BALANCE IS THE KEY TO EVERYTHING"
        czmess(mzh32a,$50,mzh32a_)
		
;********************************************************************
;  Dif 4 - Maze D - Level 16 - Level 16
;********************************************************************
mzh33a  .ctext "HABEN SIE DEN HIGH SCORE SCHON?"
	;.ctext "YOU MUST HAVE THE HIGH SCORE BY NOW"
        czmess(mzh33a,$50,mzh33a_)
		
;********************************************************************
;  Dif 4 - Maze A - Level 17 - Level 17
;********************************************************************
mzh40a  .ctext "DIESMAL KEIN HANDTRICK"
	;.ctext "NO HAND TRICK THIS TIME"
        czmess(mzh40a,$50,mzh40a_)
		
;********************************************************************
;  Dif 4 - Maze B - Level 18 - Level 18
;********************************************************************
mzh41a  .ctext "HABEN SIE ALLE VERSTECKTEN"
	;.ctext "HAVE YOU FOUND"
        czmess(mzh41a,$50,mzh41a_)
mzh41b  .ctext "ZEICHEN SCHON GEFUNDEN?"
	;.ctext "ALL THE HIDDEN TOKENS YET?"
        czmess(mzh41b,$48,mzh41b_)
		
;********************************************************************
;  Dif 4 - Maze C - Level 19 - Level 19
;********************************************************************
mzh42a  .ctext "STIEFELMEISTER ERFORDERLICH"
	;.ctext "BOOTIES MASTERY REQUIRED"
        czmess(mzh42a,$50,mzh42a_)
		
;********************************************************************
;  Dif 5 - Maze D - Level 20 - Level 20
;********************************************************************
mzh43a  .ctext "TIPP-DER LANGSAME SCHUSS IST DER BESTE"
	;.ctext "HINT - THE SLOW SHOT IS YOUR BEST SHOT"
        czmess(mzh43a,$50,mzh43a_)
		
;********************************************************************
;  Dif 5 - Maze A - Level 21 - Level 21
;********************************************************************
mzh50a  .ctext "OK KLONE, AUFWACHEN,"
	;.ctext "OK CLONES,RISE AND SHINE,"
        czmess(mzh50a,$50,mzh50a_)
mzh50b  .ctext "UND VERGESST NICHT EURE STIEFEL"
	;.ctext "AND DONT FORGET YOUR BOOTIES"
        czmess(mzh50b,$48,mzh50b_)

;********************************************************************
;  Dif 5 - Maze B - Level 22 - Level 22
;********************************************************************		
mzh51a  .ctext "SCHILDE LADEN BEIM BETRETEN VON"
	;.ctext "SHIELDS RECHARGE WHEN TRAVELING"
        czmess(mzh51a,$50,mzh51a_)
mzh51b  .ctext "BLINKENDEN TRANSPORTERN AUF"
	;.ctext "THROUGH FLASHING TRANSPORTERS"
        czmess(mzh51b,$48,mzh51b_)
		
;********************************************************************
;  Dif 5 - Maze C - Level 23 - Level 23
;********************************************************************
mzh52a  .ctext "VORSICHT BEIM 1.SCHRITT"
	;.ctext "WATCH THAT FIRST STEP"
        czmess(mzh52a,$50,mzh52a_)
mzh52b  .ctext "DER IST KNIFFLIG!"
	;.ctext "ITS A DOOZIE!"
        czmess(mzh52b,$48,mzh52b_)
		
;********************************************************************
;  Dif 6 - Maze D - Level 24 - Level 24
;********************************************************************
mzh53a  .ctext "ROTER SCHLvSSEL pFFNET"
	;.ctext "USE RED KEY TO UNLOCK"
        czmess(mzh53a,$50,mzh53a_)
mzh53b  .ctext "DEN FINALEN REACTOIDRAUM"
	;.ctext "FINAL REACTOID CHAMBER"
        czmess(mzh53b,$48,mzh53b_)
		
;********************************************************************
;  Dif 6 - Maze A - Level 25 - Level 25
;********************************************************************
mzh60a  .ctext "BRINGE MAXOIDS WEG VON SCHLvSSELN"
	;.ctext "LURE MAXOIDS AWAY FROM KEYS"
        czmess(mzh60a,$50,mzh60a_)
		
;********************************************************************
;  Dif 6 - Maze B - Level 26 - Level 26
;********************************************************************
mzh61a  .ctext "FANGE MAXOIDS HINTER WwNDEN"
	;.ctext "TRAP MAXOIDS BEHIND WALLS"
        czmess(mzh61a,$50,mzh61a_)
		
;********************************************************************
;  Dif 6 - Maze C - Level 27 - Level 27
;********************************************************************
mzh62a  .ctext "FANGEN ODER ZERSTpREN"
	;.ctext "TO TRAP OR DESTROY"
        czmess(mzh62a,$50,mzh62a_)
mzh62b  .ctext "DIES IST DIE FRAGE"
	;.ctext "THAT IS THE QUESTION"
        czmess(mzh62b,$48,mzh62b_)
		
;********************************************************************
;  Dif 7 - Maze D - Level 28 - Level 28
;********************************************************************
mzh63a  .ctext "WILLE ERREICHT ALLES"
	;.ctext "THE WILLS ACCOMPLISH EVERYTHING"
        czmess(mzh63a,$50,mzh63a_)