;******************************************************
    .sbttl "Build Sound Command Tables"
;******************************************************
;* The RPM macros etc will automatically build all    *
;* the necessary tables for each command.             *
;******************************************************
; Define the various tunetables

    STSND(g_snd_fak,REPL,7,-1,0,s_blank)    ;Blank Sound - This is required to be here?
    EXCEPT(g_geth,geths)                    ;Receive High Scores from Alpha
    EXCEPT(g_geti,getin)                    ;Receive Initials from Alpha  
    EXCEPT(g_sendh,sendhs)                  ;Send High Scores to Alpha
    EXCEPT(g_sendi,sendinit)                ;Send Initials to Alpha    	
	;EXCEPT(g_gettst,gettstr)                ;Retest GAMMA and send results
	EXCEPT(g_sstap,sendstap)                ;Send Player Game Status Info  
    EXCEPT(g_ctrl,rolgig)					;Get current Roller position
    EXCEPT(g_rand,pokran)					;Get a Random number from the Pokeys
    EXCEPT(g_swtc,switch)					;Get the GAMMA input Switch values
	EXCEPT(g_getdip0,getdip0)				;Get direct Option Switch 0 @ 13/14S
    EXCEPT(g_getdip1,getdip1)				;Get direct Option Switch 1 @ 8S
    EXCEPT(g_getset0,getset0)				;Get current settings originally stored in Option Switch 0 @ 13/14S
    EXCEPT(g_getset1,getset1)				;Get current settings originally stored in Option Switch 1 @ 8S
    EXCEPT(g_eeflgs,geteeflgs)				;Get EEROM Bad Flags
	EXCEPT(g_eedone,geteedone)				;Get EEROM Requests 
	EXCEPT(g_cnr,coinr)                     ;Pulse Coin 1
    EXCEPT(g_cnl,coinl)                     ;Pulse Coin 2
	EXCEPT(g_clro,resetop)                  ;Clear Options to DIP Settings
    EXCEPT(g_sstawx,sendstawx)              ;Send Extra Life and Warp Status Info         
    ;EXCEPT(g_sstamz,sendstamz)              ;Send Maze Level Status Info 
    EXCEPT(g_swarps,sendwarps)              ;Send All Warp Data Info
    EXCEPT(g_gwarp,getwarp)                 ;Send specified warp level data back
	EXCEPT(g_gstap,getstap)                 ;Receive Player Game Info (1P Game Count, 2P Game Count, 1P Game Time, 2P Game Time)  
    EXCEPT(g_gstawx,getstawx)               ;Receive Warp and Extra Life Status Info 
	EXCEPT(g_setopt0,setopt0)				;Set Settings Switch 0 Values
    EXCEPT(g_setopt1,setopt1)				;Set Settings Switch 1 Values
    EXCEPT(g_classwarp,reswarps)			;Set warp numbers to classics
	EXCEPT(g_genwarp,genwarps)				;Re-generate all warps
	;EXCEPT(g_clrwarp,clrwarps)				;Clear all warps (all blank and invisible)
	EXCEPT(g_getts,gettes)                  ;Receive Tesseracts from Alpha  
	EXCEPT(g_sendtes,sendtess)              ;Send Tesseracts to Alpha  
	;EXCEPT(g_sendmaxsnd,sendmaxsnd)         ;Set the Max background sound   
	EXCEPT(g_setctrlp1,setctrlp1)         	;Set the controls to Player 1 
	EXCEPT(g_setctrlp2,setctrlp2)         	;Set the controls to Player 2 (cocktail mode)  
	
	;Everythign after this is accessible via GAMMA test in SELF TEST
	EXCEPT(g_clrh,reseths)                  ;Clear High Scores
    EXCEPT(g_clrs,resetbk)                  ;Clear Game Stats
	
	KILALL(g_sndstop) 						;Stop all sounds
;* Sound Effects
;*
;* A) Game Start
    STSND(snd_coin  ,REPL,8,-1,08,s_coin)     ;Coin
    STSND(snd_launch,REPL,8,-1,08,s_launch)   ;Launch
	 CONT(				  8,-1,15,s_launch2)
    STSND(snd_passby,REPL,8,-1,08,s_passby)   ;Passby
     CONT(				  8,-1,09,s_passby2)
     
;* B) Tactical Scanner
    STSND(snd_b1a,REPL,8,-1,11,s_galert)    ;General Alert - doodley sounds in background
    STSND(snd_b1e,REPL,8,-1,11,s_redalert)  ;Red Alert - for Tesseract
    STSND(snd_b2a,REPL,8,-1,08,s_blaunch)   ;Ball Launch
    STSND(snd_b2b,REPL,8,-1,08,s_bbrick)    ;Ball Hits Brick
    STSND(snd_b2c,REPL,8,-1,08,s_bpaddle)   ;Ball Hits Paddle
    STSND(snd_b2d,REPL,8,-1,08,s_bmissp)    ;Ball Misses Paddle
    STSND(snd_b3a,REPL,8,-1,09,s_digit)     ;Digit Entered
    STSND(snd_b3b,REPL,8,-1,08,s_ccorrect)  ;Combination Correct
        
;* C) Shared Sounds for All Space and Maze Waves
    STSND(snd_c1 ,REPL,8,-1,15,s_exp)        ;Player Ship Explosion
     CONT(             8,-1,00,s_exp2)   
     CONT(             8,-1,01,s_exp3)
     CONT(             8,-1,14,s_exp3)
    STSND(snd_c2 ,REPL,8,-1,02,s_pfire)      ;Player Ship Firing
    STSND(snd_c3 ,REPL,8,-1,03,s_shotstat)   ;Shot Hits Station
    STSND(snd_c4 ,REPL,8,-1,04,s_xlife)      ;Bonus Life
     CONT(             8,-1,03,s_xlife)
    STSND(snd_c5 ,REPL,8,-1,03,s_bonus)      ;Bonus Tick
    STSND(snd_c7 ,REPL,8,-1,08,s_beeps)      ;75 Bonus Ticks
    KILID(snd_c6,snd_c7)                     ;Kill Bonus Ticks

;   
;* D) Robot Fish Space Wave
    STSND(snd_d1 ,REPL,8,-1,08,s_fishhatch)  ;Fish Hatch
    STSND(snd_d3 ,REPL,8,-1,08,s_goosefish)  ;Go.
    STSND(snd_d4 ,REPL,8,-1,09,s_blowfish)   ;Blow Up Fish
     CONT(             8,-1,10,s_blowfish2)
    STSND(snd_d5 ,REPL,8,-1,11,s_circfish)   ;Circling Fish
     CONT(             8,-1,05,s_circfish2)
     
;* E) Galaxians Space Wave
    STSND(snd_e1 ,REPL,8,-1,08,s_feject) 	;Fighters Leave Station
    STSND(snd_e2 ,REPL,8,-1,09,s_fshot)      ;Fighter Shoots
    STSND(snd_e3 ,REPL,8,-1,10,s_exp3)       ;Blow Up Fighter
     CONT(             8,-1,14,s_exp5)       
    STSND(snd_e4 ,REPL,8,-1,10,s_exp3)       ;Blow Up Fighter 2
     CONT(             8,-1,09,s_exp4)
     CONT(             8,-1,14,s_exp6)
     
;* F) Web Spinners Space Wave
    STSND(snd_f2 ,REPL,8,-1,08,s_blowspin)   ;Blow Up Spinner
    STSND(snd_f3 ,REPL,8,-1,09,s_mazehit)    ;Hit Maze Segment
    STSND(snd_f4 ,REPL,8,-1,09,s_mazekill)   ;Blow Up Maze Segment
     CONT(             8,-1,10,s_mazekill2)
     
;* G) Star Castle Space Wave

     
;* H)   Landing on the Space Station
    STSND(snd_h1 ,REPL,8,-1,15,s_exp5)       ;Successful Landing
    STSND(snd_h2 ,REPL,8,-1,15,s_exp6)       ;Crash Ship on Station
     CONT(             8,-1,08,s_exp3)
     CONT(             8,-1,09,s_exp7)
    STSND(snd_h3 ,REPL,8,-1,09,s_lshot)      ;Laser Shot
    
;* I) Maze Wave
    STSND(snd_i1a,REPL,8,-1,04,s_shield)     ;Shields in Use
    STSND(snd_i1b,REPL,8,-1,04,s_hitshield)  ;Item Hits Shield
    STSND(snd_i1c,REPL,8,-1,04,s_noshield)   ;Shields Used Up
    STSND(snd_i2a,REPL,8,-1,09,s_manhit)     ;Man Hits Object
     CONT(             8,-1,10,s_shotstat)    
    STSND(snd_i2c,REPL,8,-1,11,s_manwall) ;sp_rand_ugh) 	;Man Hits Wall/Ceiling - Triggers additional speech command
    STSND(snd_i2d,REPL,8,-1,11,s_mantrip)    ;Man Hits Trip Pad
     CONT(             8,-1,04,s_mantrip)
    STSND(snd_i2f,REPL,8,-1,00,s_spikes)     ;Man Hits Stalactite
     CONT(             8,-1,01,s_spikes2)
     CONT(             8,-1,02,s_spikes3)      
    STSND(snd_i2g,REPL,8,-1,08,s_oxygen)     ;Man Picks Up Oxygen
    STSND(snd_i2h,REPL,8,-1,00,s_key)        ;Man Picks Up Key
    STSND(snd_i2i,REPL,8,-1,00,s_door)       ;Man Opens Door
    STSND(snd_b1d,REPL,8,-1,11,s_rhum)       ;Reactor Hum
    STSND(snd_d2 ,REPL,8,-1,06,s_reactor)    ;Reactor Supercritical
     CONT(             8,-1,07,s_reactor2)
    STSND(snd_i4b,REPL,8,-1,02,s_handoff)    ;Hand Turned Off
    STSND(snd_i4c,REPL,8,-1,02,s_handon)     ;Hand Turned On
    STSND(snd_i6 ,REPL,8,-1,00,s_trans)      ;Transporter Booth
     CONT(             8,-1,02,s_trans2)

    STSND(snd_i7a,REPL,8,-1,01,s_robshot)    ;Robot Fires a Shot
	
    STSND(snd_i7b,REPL,8,-1,08,s_cann)       ;Laser Cannon Fires
     CONT(             8,-1,09,s_cann2)
     CONT(             8,-1,10,s_cann3)
     CONT(             8,-1,11,s_cann4)
	 
    STSND(snd_i7c,REPL,8,-1,01,s_ssplash)    ;Shot Splashes On Wall
    STSND(snd_i8 ,REPL,8,-1,00,s_footstep)   ;Footsteps
    STSND(snd_i9 ,REPL,8,-1,07,s_nooxy)      ;Oxygen Out
    
;* J) Leaving the Maze
    STSND(snd_j3 ,REPL,8,-1,09,s_rblow)      ;Reactor Blows Up
     CONT(             8,-1,10,s_rblow2)
     CONT(             8,-1,12,s_rblow2)
     CONT(             8,-1,13,s_rblow2)
     CONT(             8,-1,11,s_rblow3)
     CONT(             8,-1,14,s_rblow3)
     CONT(             8,-1,15,s_rblow3)
	 
    STSND(snd_j6 ,REPL,8,-1,08,s_escfall)    ;Escape Pod Fall
    
;***************************************************   
;*    Music
;***************************************************
;*  Mystery - Enter Maze
    STSND(snd_mys,REPL,6,-1,00,s_mystery)       
     CONT(             6,-1,02,s_mystery2)
     CONT(             6,-1,04,s_mystery3)
     CONT(             6,-1,12,s_mystery4)
     
;*  Breakout
    STSND(snd_brk,REPL,6,-1,00,s_breakout)      
     CONT(             6,-1,02,s_breakout2)
     CONT(             6,-1,14,s_breakout3)
     CONT(             6,-1,12,s_breakout4)

;*  Start
    STSND(snd_str,REPL,6,-1,00,s_start)
     CONT(             6,-1,02,s_start2)
     CONT(             6,-1,14,s_start3)
     CONT(             6,-1,12,s_start4)
       
;*  High Score
    STSND(snd_hsc,REPL,6,-1,00,s_highscore5)
     CONT(             6,-1,02,s_highscore6)
     CONT(             6,-1,14,s_highscore7)
     CONT(             6,-1,12,s_highscore8)
     
;*  Hero Theme - Exit Maze
    STSND(snd_hro,REPL,6,-1,00,s_escape)
     CONT(             6,-1,02,s_escape2)
     CONT(             6,-1,04,s_escape3)
     CONT(             6,-1,12,s_escape4)
    
;* Triumph - End Story
    STSND(snd_triumph ,REPL,6,-1,00,s_triumph)     
     CONT(              6,-1,02,s_triumph2)
     CONT(              6,-1,14,s_triumph3)
     CONT(              6,-1,12,s_triumph4)

; ;* Maze Entry - Tense for final Maze
    ; STSND(snd_newmus,REPL,6,-1,00,s_tense_1)       
     ; CONT(             6,-1,02,s_tense_2)
     ; CONT(             6,-1,04,s_tense_3)
     ; CONT(             6,-1,12,s_tense_4)	 
	 ; CONT(             6,-1,14,s_newmusic_5)	
	 
; ;* Victory
	; STSND(snd_victory,REPL,6,-1,00,s_vic_b1)
	 ; ;CONT(             6,-1,01,s_vic_b2)
	 ; CONT(             6,-1,02,s_vic_b3)
	 ; CONT(             6,-1,00,s_vic_mid)
	 ; CONT(             6,-1,04,s_vic_mid1)
	 ; CONT(			   6,-1,06,s_vic_mid2)
	 ; ;CONT(			   6,-1,08,s_vic_lead)
	 ; ;CONT(			   6,-1,10,s_vic_perc)

	
;* Space Bonus 
    STSND(snd_spbonus,REPL,6,-1,00,s_spbonus_1)       
	 
;* Stop Exit Maze Music
    KILID(snd_kilhro,snd_hro)

;* Star Castle Engergize	
	STSND(snd_scenergize ,REPL,8,-1,08,s_scenergize) 
     CONT(8,-1,09,s_scenergize2)
     CONT(8,-1,10,s_scenergize3)
     CONT(8,-1,11,s_scenergize4)

;* Star Castle Rotation 	
	;STSND(snd_scspin ,REPL,8,-1,05,s_scspin) 
	
;* Star Shield Hit 	
	STSND(snd_schit ,REPL,8,-1,06,s_schit) 
	
;* Star Castle Panel Drop
    STSND(snd_scdrop ,REPL,8,-1,07,s_scdrop) 
	
;* Maze Sounds - Broken Transporter Booth	
    ;STSND(snd_notrans ,REPL, 8,-1,00,s_notrans) 

;* Tact Scan Fail
	STSND(snd_fail ,REPL, 8,-1,00,s_fail) 
	 CONT(8,-1,08,s_fail2)
	 
;* Maynard fly in
	STSND(snd_maynard,REPL,8,-1,08,s_maynard)   ;Maynard fly in
	 CONT(				  8,-1,15,s_maynard2)
;* Kill Maynard fly in
	KILID(snd_kilmaynard,snd_bhend)
	
;* End of wormhole	 
	STSND(snd_bhend,REPL,8,-1,08,s_bhend)   
	
;* Bonus Build Up
	STSND(snd_bonusr,REPL,8,-1,01,s_bonusr) 
	 CONT(8,-1,09,s_bonusr2)
	 CONT(8,-1,02,s_bonusr3)
	 CONT(8,-1,10,s_bonusr4)
	 
;* Max Moving Sound
	STSND(snd_maxmv0,REPL,7,-1,15,s_maxmv0)   
;* Max Moving Pitch Increase
    STSND(snd_maxmv1,REPL,7,-1,15,s_maxmv1)  
    STSND(snd_maxmv2,REPL,7,-1,15,s_maxmv2)  
    STSND(snd_maxmv3,REPL,7,-1,15,s_maxmv3)  
    ; IMMED(snd_maxmv1,snd_maxmv,15,f_sbfreq,5)
    ; IMMED(snd_maxmv2,snd_maxmv,15,f_sbfreq,10)
    ; IMMED(snd_maxmv3,snd_maxmv,15,f_sbfreq,15)
    
    ; STSND(snd_maxmv2,REPL,8,-1,15,s_maxmv1)
    ; STSND(snd_maxmv3,REPL,8,-1,15,s_maxmv1)
    ; STSND(snd_maxmv4,REPL,8,-1,15,s_maxmv1)

;* Stop Any Max Sound
    ;KILID(snd_kilmax,snd_maxmv0)
    ;KILDEV(snd_kilmax,dev,chan)
    KILPRI(snd_kilmax,7)

;* Star Castle Background sound
	STSND(snd_hiddenbk1,REPL,6,-1,00,s_hiddenbk1)
	
;* Star Castle Background kills
	KILID(snd_kilbak,snd_hiddenbk1)

;STSND(snd_hiddenbk2,REPL,6,-1,00,s_hiddenbk2)
;STSND(snd_hiddenbk3,REPL,6,-1,00,s_hiddenbk3)
;STSND(snd_hiddenbk4,REPL,6,-1,00,s_hiddenbk4)
;***************************************************
;*  Speech Commands here (if enabled)
;***************************************************
#IF ___OKI == 1

	;Speech Chip Test
	ADPCM(sp_test,chan1,dbm0)			;Done
	
	;Mother voice
    ADPCM(sp_one,chan1,dbm0)
    ADPCM(sp_two,chan1,dbm0)
    ADPCM(sp_three,chan1,dbm0)
    ADPCM(sp_four,chan1,dbm0)
    ADPCM(sp_five,chan1,dbm0)
    ADPCM(sp_six,chan1,dbm0)
    ADPCM(sp_seven,chan1,dbm0)
    ADPCM(sp_eight,chan1,dbm0)
    ADPCM(sp_nine,chan1,dbm0)
    ADPCM(sp_ten,chan1,dbm0)
	
	;Max voice
	ADPCM(sp_maxintruder,chan2,dbm0)		;used when Max Activates
	ADPCM(sp_maxdestint,chan2,dbm0)			;used when Max Activates
	ADPCM(sp_maxdestroy,chan2,dbm0)		;used when Max kills Rex
    ADPCM(sp_maxhaha1,chan2,dbm0)			;used when Max kills Rex
	ADPCM(sp_maxhaha2,chan2,dbm0)			;used when Max kills Rex
	ADPCM(sp_maxhaha3,chan2,dbm0)			;used when Max kills Rex
	ADPCM(sp_maxwewin,chan2,dbm0)			;used when Max kills Rex
	ADPCM(sp_maxyoulose,chan2,dbm0)			;used when Max kills Rex
	ADPCM(sp_maxactivate,chan2,dbm0)		;used when Max Activates
	ADPCM(sp_maxattack,chan2,dbm0)			;used when Max Activates
	ADPCM(sp_maxcannot,chan2,dbm0)			;used at start of star castle
	ADPCM(sp_maxwillnot,chan2,dbm0)			;used at start of star castle
	ADPCM(sp_maxbeback,chan2,dbm0)			;used for Max death
	ADPCM(sp_maxahhhh,chan2,dbm0)			;used for Max injury
	ADPCM(sp_maxohhhh,chan2,dbm0)			;used for Max injury
	ADPCM(sp_maxeeeeww,chan2,dbm0)			;used for Max death
	
	;Rex voice
	ADPCM(sp_rexhurry,chan3,dbm0)			;used when Reactor is at 15
	ADPCM(sp_rexcomeon,chan3,dbm0)		;Full 1/256 random
	ADPCM(sp_rexouttahere,chan3,dbm0)	;not yet used
	ADPCM(sp_rexooof,chan3,dbm0)		;used when Rex hits wall/ceiling
	ADPCM(sp_rexooom,chan3,dbm0)		;used when Rex hits wall/ceiling
	ADPCM(sp_rexouch,chan3,dbm0)		
	ADPCM(sp_rexuggh,chan3,dbm0)		;used when Rex hits wall/ceiling - also used for suffocate
	ADPCM(sp_rexuhoh,chan3,dbm0)
	ADPCM(sp_rexumhmm,chan3,dbm0)
	ADPCM(sp_rexummm,chan3,dbm0)
	ADPCM(sp_rexwhoa,chan3,dbm0)		
	ADPCM(sp_rexwoohoo,chan3,dbm0)		;used random when Rex ship is blasting off from station

#ENDIF


#IF ___TLK == 1

    TALK(sp_test,sps_test,spx_test)
    TALK(sp_one,sps_one,spx_one)
    TALK(sp_two,sps_two,spx_two)
    TALK(sp_three,sps_three,spx_three)
    TALK(sp_four,sps_four,spx_four)
    TALK(sp_five,sps_five,spx_five)
    TALK(sp_six,sps_six,spx_six)
    TALK(sp_seven,sps_seven,spx_seven)
    TALK(sp_eight,sps_eight,spx_eight)
    TALK(sp_nine,sps_nine,spx_nine)
    TALK(sp_ten,sps_ten,spx_ten)
	;TALK(sp_xxx,sps_xxx,spx_xxx)
	
	;Max voice
	TALK(sp_maxintruder,sps_maxintruder,spx_maxintruder)		;used when Max Activates
	TALK(sp_maxdestint,sps_maxdestint,spx_maxdestint)			;used when Max Activates
	TALK(sp_maxdestroy,sps_maxdestroy,spx_maxdestroy)		;used when Max kills Rex
    TALK(sp_maxhaha1,sps_maxhaha,spx_maxhaha)			;used when Max kills Rex
	TALK(sp_maxhaha2,sps_maxhaha,spx_maxhaha)			;used when Max kills Rex
	TALK(sp_maxhaha3,sps_maxhaha,spx_maxhaha)			;used when Max kills Rex
	TALK(sp_maxwewin,sps_maxwewin,spx_maxwewin)			;used when Max kills Rex
	TALK(sp_maxyoulose,sps_maxyoulose,spx_maxyoulose)			;used when Max kills Rex
	TALK(sp_maxactivate,sps_maxactivate,spx_maxactivate)
	TALK(sp_maxattack,sps_maxattack,spx_maxattack)			;used when Max Activates
	TALK(sp_maxcannot,sps_maxcannot,spx_maxcannot)			;used at start of star castle
	TALK(sp_maxwillnot,sps_maxwillnot,spx_maxwillnot)			;used at start of star castle
	TALK(sp_maxbeback,sps_maxbeback,spx_maxbeback)			;used for Max death
	TALK(sp_maxahhhh,sps_maxuggh,spx_maxuggh)			;used for Max injury
	TALK(sp_maxohhhh,sps_maxohhhh,spx_maxohhhh)			;used for Max injury
	TALK(sp_maxeeeeww,sps_maxeeeeww,spx_maxeeeeww)			;used for Max death
	
	;Rex voice
	TALK(sp_rexhurry,sps_rexhurry,spx_rexhurry)			;used when Reactor is at 15, 1/16 chance
	TALK(sp_rexcomeon,sps_rexcomeon,spx_rexcomeon)		;Full 1/256 random
	TALK(sp_rexouttahere,sps_rexouttah,spx_rexouttah)	;not yet used
	TALK(sp_rexooof,sps_rexooof,spx_rexooof)		    ;used when Rex hits wall/ceiling
	TALK(sp_rexooom,sps_rexooom,spx_rexooom)		    ;used when Rex hits wall/ceiling
	TALK(sp_rexouch,sps_rexouch,spx_rexouch)		
	TALK(sp_rexuggh,sps_rexuggh,spx_rexuggh)		    ;used when Rex hits wall/ceiling - also used for suffocate
	TALK(sp_rexuhoh,sps_rexuhoh,spx_rexuhoh)
	TALK(sp_rexumhmm,sps_rexumhmm,spx_rexumhmm)
	TALK(sp_rexummm,sps_rexummm,spx_rexummm)
	TALK(sp_rexwhoa,sps_rexwhoa,spx_rexwhoa)		
	TALK(sp_rexwoohoo,sps_rexwoohoo,spx_rexwoohoo)		;used random when Rex ship is blasting off from station    

#ENDIF      
    
;* Keep this in order because we will limit the upper bound of sound commands to send
;* in the self test by only allowing commands up to this point.    
    SILENT(snd_off)

 
;*******************************************************************
; Easter Eggs
;*******************************************************************
#IF ___OKI != 0
	ADPCM(sp_forcebe,chan4,dbm0)	    ;Obi Wan - The force be with you always
    ADPCM(sp_looksize,chan4,dbm0)		;Wedge - Look at the size of that thing!	    
#ELSE
  #IF ___TLK != 0
    TALK(sp_forcebe,sps_sw1,spx_sw1)    ;Obi Wan - The force be with you always
    TALK(sp_looksize,sps_sw2,spx_sw2)   ;Wedge - Look at the size of that thing    
  #ENDIF
#ENDIF
;*******************************************************************
;*******************************************************************
;*******************************************************************
