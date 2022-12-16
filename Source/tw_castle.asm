;*******************************************
; Space Wave - Star Castle
;*******************************************
;* Constants
sc_state_down 		= $80
sc_state_talk		= $81
sc_state_show 		= $82
sc_state_up			= $83
sc_state_fight 		= $84
sc_state_done 		= $85

panel_hits			= $03	;Number of shield hits before being desroyed

		
enem4	;never init if ship is dead or dying
		lda	shipst
		ifne
			ifpl	
				bit scflags
				ifpl	
					;Initialization for enem4 in here
					lda #sc_state_down	;set the initialized flag since we are here
					sta scflags
					lda	#$99			;Reset bonus to 9,900 - Higher is nice
					sta	bonusa
					lda #00
					sta shiecovr		;Blank shield color override
					sta shibits			;Reset bits, these will be populated later
					sta shiroen			;No rotation yet either
					sta shirotl
					sta shiroth			;Reset rotation
					jsr setshdstat		;Reset status of each panel
					;position station
					lda #$80
					sta statxl      ;Place Base Ship
					lda #$08
					sta statyl
					lda #$01
					sta statyh
					lda #$F9        ;Station Status
					sta statst
					lda #$80
					sta stbflg		;Station will move downwards to start
					lda #$15
					sta hitpts          ;1500 Per Object
				endif
			else 
				;If shipst is minus, mark to re-init star castle when it flips positive again
				;lda #0
				;sta scflags			;Always restart Star Castle in case of death
			endif
		endif
			
		;Update rotation based upon frame
		bit	shiroen			;only if enabled tho
		ifmi
			lda frame
			and #05
			ifeq
				inc shirotl
				lda shirotl
				and #07
				sta shirotl
				ifeq
					;firstly, update the position reference
					inc shiroth
					lda shiroth
					and #07
					sta shiroth
				endif
			endif
		endif
		
		;Countdown the timer
		lda	frame
		and	#$3F
		ifeq
			lda	bonusa
			ifne
				sed					;*DECIMAL MODE
				sec
				sbc	#01				;Decrement bonus on the basis of time
				sta	bonusa
				cld					;*END DECIMAL
				ifeq
					lda #sc_state_done		;Out of time, move down off screen
					sta scflags
					lda #$80
					sta stbflg
				endif
			endif
		endif
		
		;Always draw Maynard
		jsr ?dmaynard
		
		;main running loop
		lda	scflags
		and #$07
		asl A
		tax
		lda cruntrt+1,X
		pha
		lda cruntrt,X
		pha
		rts		;we still come back here after this

cruntrt .word rundown-1		;moving down
		.word runtalk-1		;talking
		.word runrgen-1		;shield animation
		.word runup-1		;moving up
		.word runfight-1	;fighting now
		;.word runshields-1	;Star castle!!
		.word rundone-1		;wrap up


;Execution loops for each status
rundown
		;Station will move down initially
		lda statyh
		cmp #$07
		ifge
			;its where it needs to be, stop station drop motion
			lda #00				
			sta stbflg
			;sta shiroth			;Position reference is zero to start
			sta frame			;reset the frame counter for animation
			;move to next state
			lda #sc_state_talk
			sta scflags			;goto next state
			lda rands
			and #01
			ifeq
				lda #sp_maxcannot
			else
				lda #sp_maxwillnot
			endif
			jsr dosound			;Shield engergize 
			;jsr dodelay
			;lda #snd_j6			;Escape pod sound
			;jsr dosound
		endif
		rts

runtalk lda frame
		and #$3F
		ifeq
			lda #0
			sta frame
			;enable all panel bits
			lda #$ff
			sta shibits			;All segments showing now
			lda #colflash		;Flash color - no brightness
			sta shiecovr
			lda #snd_scenergize
			jsr dosound
			;move to next state
			lda #sc_state_show
			sta scflags			;goto next state
		endif
		rts
		
runrgen ;fade in the castle
		lda frame
		and #$03
		ifeq
			lda shiecovr
			and #$F0
			cmp #$F0
			ifne
				lda shiecovr
				adc #$10
				sta shiecovr
			else
				;delay a bit more
				lda frame
				and #$0f
				ifeq
					;time to transition
					lda #00
					sta shiecovr		;clear override
					sta	castsnd			;don't play sound again in fight mode
					;init next state
					lda #$C0			
					sta stbflg			;move station upwards
					lda #sc_state_up
					sta scflags			;goto next state
					lda #snd_hiddenbk1		;Cool background sound
					jsr dosound
				endif
			endif
		endif
		rts
		
runup
		;Station moving back up now...
		lda statyh
		cmp #$01		;It only goes back this far tho
		iflt
			lda #00			;its where it needs to be, stop station drop motion
			sta stbflg
			lda #$80		;set the rotation flag
			sta shiroen
			lda #sc_state_fight
			sta scflags		;set it
		endif
		rts
		
runfight
		lda shipst
		ifeq	;ifmi
			rts
		endif
		lda	castsnd			;My cue for return from player transition
		ifmi
			;start sound again
			lda #snd_hiddenbk1
			jsr dosound
			lda #$00
			sta castsnd			;flag to not start this sound again
		endif
		jsr ?dostation	
		lda nenstr		;Need to start any?
		;cmp #6d
		ifeq
			;Time to start moving
			jsr ?mvstation
			;jsr ?movefire		;update velocity, set up drawing
			;See if a fireball needs to be launched
			lda shibits			;must have panels to drop a fireball
			ifne
				lda objst+zshot+nmshot-1	;Only 1 at a time tho
				ifeq
					lda shirotl			;Only launch at postion 0 on the minor counter
					ifeq
						jsr ?rotate8
						and #02				;Is there a gap right in front (position 2)?
						ifeq
							lda shipst			;Only if player is not exploding (!=80)
							ifpl
								jsr ?dropfire
							endif
						endif
					endif
				endif
			endif	
			;do timing for the panel flashing
			lda frame
			and #$0F
			ifeq
				ldx #$07
				begin
					lda shiests,X
					ifne
						tay
						and #$07
						sta shiests,X
						tya
						and #$18
						ifne
							sec
							sbc #$08
							clc
							adc shiests,X
							sta shiests,X
						endif
					endif
					dex
				miend
			endif	
			;Are we done?
			lda shibits
			ifeq
				ldx #zshot+nmshot-1
				lda #0
				begin
					ora objst,X
					dex
					cpx #zshot
				ccend
				ora #0
				ifeq
					lda	shipst
					ifpl
						ifne
							lda #$80
							sta stbflg	
							lda #sc_state_done
							sta scflags
						endif
					endif
				endif
			endif
		endif	
		rts
	
rundone
		;We are done if we are here...
		lda statyh
		cmp #$0A
		ifge
			jsr	noneleft
		endif
		rts
	
?dropfire
		lda #snd_passby		;Fireball Shot
		jsr dosound
		ldx #+zshot+nmshot-1
		lda #$31
		sta	objst,X			;Activate fireball in slot X
		jsr shotdelta		;Sets fireball start to station start and sets velocities towards player
		
		;now set velocity based upon shotdelta calcs above
		; lda temp1+1
		; ifmi
			; clc
			; adc #01
		; endif
		; sta velxh+zshot
		; lda temp2+1
		; sta velyh+zshot
		rts
						
;*************************************************
	.sbttl "Move Station"
;*************************************************
; Moves the space station around the screen in a
; random pattern, staying within the bounds of 
; the perspective area. Also launches fighters.
;*************************************************
?dostation
        lda nenstr
        beq ?enm10
        lda frame
        tay 
        ifeq
            lda #$80
            sta lauen                   ;Force a start
        endif
        tya 
        and #$01
        ifeq
            jsr ?emem12
        else
?enm10      jsr ?coreg
        endif
        rts 


?emem12	jsr	getempty		;Find an empty slot
		ifpl
			lda	mzgame			;Still in space??
			ifeq				;yes
				bit	lauen			;Okay to launch??
				ifmi				;It's OK!
					lda	sndcue+2	;Then make a sound
					ifeq
						lda	#04
						sta	sndcue+2
						lda	#snd_e1
						jsr	dosound
					endif
					lda	statxl
					sta	sobjxl,X		;Store LSB
					lda	statyl
					sta	sobjyl,X		;LSB Y too
					lda	statxh
					sta	sobjxh,X
					lda	statyh
					sta	sobjyh,X
					lda	#$50			;Turn on & setup bit here only
					sta	sobjst,X
					lda	#00
					sta	sobjs2,X		;0 Second status byte
					inc	nenemy
					dec	nenstr			;Started another one
				endif
			endif
		endif
		;*********** Fall Through ***************	
		;*************************************************
			.sbttl "Correography"
		;*************************************************
		;* Move ship according to correography table     *
		;*                                               *
		;* Data: Each byte (1 for each ship) is broken   *
		;* up into two parts. The top nibble is the      *
		;* number of frames (x4) to do a move. The bottom*
		;* nibble is the direction to move. One of 16    *
		;* directions is picked (0=no motion). 1=22.5 deg*
		;* (clockwise) up to 15=337.5 deg. Starting from *
		;* 90 deg straight up (ie: 1=112.5 deg, 15=67.5) *
		;* Note that there is no straight up motion.     *
		;*************************************************
?coreg  jsr	next			;Start Any??
		ldx	#nmform-1
		stx	temp9
		begin
			lda	sobjs2,X
			sta	temp3+1			;Save 2nd Status
			lda	sobjst,X		;Active??
			sta	temp3			;For later use
			bne	?cor10
?cor6			jmp	?cor20
?cor10		ifmi				;Exploding??
				lda	sobjst,X
				clc	
				adc	#04
				sta	sobjst,X
				ifpl
					jsr	oneless		;One Less
				endif
				jsr	copypos		;Copy Position to vdata
				jmp	?cor15		;Just draw it
			else				;Else, must be active
				bit	temp3+1		;See if in hold place
				bvs	?cor13		;Yes, don't move it
				bit	temp3			;See if in setup mode
				ifvs
					jsr	setup			;Do setup
?cor13				jmp	?cor28
				endif
				and	#$20			;Force mode??
				ifne				;yes
					lda	toolong		;Stop moving if base is moving
					ifmi
						lda	sobjyh,X		;Already on it's way up??
						cmp	#$0A
						ifcs				;nope
							lda	#00
							sta	sobjst,X		;Just kill it then
							beq	?cor6			;And skip it
						endif
					endif
					lda	sobjyh,X
					ifeq
						jsr	stcorg		;Restart Correography
						ora	#$40
?cor8					sta	sobjst,X		;Put Back into Formation
						sta	temp3			;May be needed later
						jsr	thisent		;Get this correography entry
						sta	sobdir,X
						bne	?cor16		;And move it!
					endif
?cor7					lda	#00			;To Move Up, No X Motion
					pha				;X step is always 0 for up the screen
					ldy	#-1			;Sign extend on Y step
					lda	#-$20			
					bne	?cor11		;Always
				endif
				lda	temp3+1
				and	#04				;To 'start'??
				bne	?cor7			;Yes, get there.
				lda	temp3+1			;Check if 'following'
				and	#$20			;Followbit
				ifne				;It's follow the leader time
					jsr	followme		;Do Follow the leader
					jmp	?cor28
				endif
				lda	temp3+1
				and	#$10			;Kamakazi??
				ifne
					jsr	kama			;Check Update
					bne	?cor16
				endif
				lda	frame
				and	#03			;One every 4 frames
				ifeq
					lda	sobdir,X		;Direction
					sec	
					sbc	#$10
					cmp	#$10
					ifcc				;yes, do next
						jsr	nextent
					endif
					sta	sobdir,X		;new direction
				endif
			endif
?cor16		lda	sobdir,X
			and	#$0F			;Get Direction
			ifeq
?cor28			jsr	copypos			;Just copy position then
			else
				tax
				ldy	#00				;Sign Extend
				lda	cortlx-1,X		;X Step
				pha					;Will do this later
				lda	cortly-1,X		;Y Step
				ifmi
					dey
				endif
?cor11			clc	
				ldx	temp9
				adc	sobjyl,X
				sta	sobjyl,X
				sta	vdata+2			;Save for Output
				tya
				adc	sobjyh,X
				sta	sobjyh,X
				sta	vdata+3
				ifmi				;First check for off top
					pla				;Throw away extra push
					lda	temp3+1
					and	#04			;To 'top' of screen??
					ifne
						jsr	stcorg
						sta	temp3
						jsr	thisent
						sta	sobdir,X
					endif
					lda	sobjs2,X
					and	#$FB			;Drop 'startup' bit
					sta	sobjs2,X		;Because it must be at the top
					lda	#center
					sta	sobjxh,X
					lda	#$80
					sta	sobjxl,X
					lda	#00
					sta	sobjyh,X
					sta	sobjyl,X
					beq	?cor28		;Just xfer
				endif
				jsr	pastck		;Check for off 'bottom'
				ldy	#00
				pla
				ifmi
					dey
				endif
				clc
				adc	sobjxl,X
				sta	sobjxl,X
				sta	vdata
				tya
				adc	sobjxh,X
				sta	sobjxh,X
				sta	vdata+1		;For Output
				ifmi				;Check for off screen
					lda	#00
					sta	sobjxl,X		;Leave at 0,0
					sta	sobjxh,X
					sta	vdata
					ldy	#$8A			;If - , it went off the left side
?cor14				sta	vdata+1		;So it positions right
					lda	sobjst,X		;Forced to top???
					and	#$20			;Skip this if yes
					ifeq				;nope
						tya	
						sta	sobdir,X		;Force back on screen
					endif
				else
					cmp	#rtedge		;Did it hit the right edge
					ifcs				;yep, turn it around!!
						lda	#$FF
						sta	sobjxl,X
						sta	vdata
						lda	#rtedge-1
						sta	sobjxh,X		;Stick at Left Edge
						ldy	#$85			;Force left then
						bne	?cor14
					endif
				endif
			endif
			jsr	dropshot		;And drop a shot
			jsr	shtchk			;Hit the objects
?cor15		jsr	drawen
?cor20		dec	temp9
			ldx	temp9
		miend
		lda	#00
		ldx	#$60
		jsr	vgadd2		;Restore normal stat
		;Will see if player is stalling and bring out mother if yes
		lda	bonusa		;Out of bonus??
		ora	mzgame		;And we are still in space
		ifeq
			ldx	#nmform-1
			begin
				lda	sobjst,X		;Any left active??
				ifne
					ifpl				;Skip if exploding
						lda	sobjs2,X		;Off bottom once??
						bpl	?exit
					endif
				endif
				dex
			miend			;If we fall out...
			lda	nenemy		;At least some out?
			ifne
				lda	#$80		;We didn't find any 'good ones'
				sta	toolong		;Set too long timer
				sta	statst		;And turn on the statin
			endif
		endif
?exit	jmp mvenst
		
	
;*************************************************
; Move Station
;*************************************************
; saucvd is direction
;   00 = Down
;	01 = Left-Down
;  	02 = Left
; 	03 = Left-Up
; 	04 = Up
; 	05 = Right+Up
;	06 = Right
; 	07 = Right+Down
;*************************************************
?veltable
	.db $00,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$02,$03,$03
	.db $03,$03,$03,$03,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$16
	
?mvstation
		;Hack to position for testing
		; ldy statyh          ;Must recall position
        ; cpy #02				;Originally $03
		; ifge
			; lda #00
			; sta saucvl
		; else
			; lda #04
			; sta saucvl
			; lda #$80
			; sta stbflg
		; endif
		; rts
		;End Hack
		
        ldx saucvd          ;Velocity Direction
        ifpl                ;Add
			inx
			lda ?veltable,X
            cpx #$1F        	;Distance to travel
            ifge
                ldx #$9F
            endif
        else                    ;Else Start Slowing
            txa
			and #$7F
			tax
            lda ?veltable,X
			dex
        endif
		stx saucvd
        sta saucvl
        ifeq                    ;If 0, get a new direction
            sta saucvd          	;And start up again
            ldx stbix           	;Clear base flag index
            bit rands+2         	;Change
            ifmi                    ;No change bit clear, change ok
                dex                 	;Guess turn left
                ifvc
                    inx
                    inx                 	;Turn Right (2 to compensate for dex)
                endif
            endif
            txa
            and #07             	;Don't let wrap
        else
            lda stbix               	;Else continue same (maybe)
        endif
		
        ;*----------------- Limit Check ----------------
        ;* Y Check First 
        ;*----------------------------------------------
        ldx stbix           ;Which Direction now?
        ldy statyh          ;Check Y motion
        cpy #02             ;If above, get moving down - Original $02
        iflt                    ;It's too high
            ldy ?topok,X        ;Is it going an ok direction??
            ifpl    
                jsr ?statstop
                ifeq                    ;Change when stopped
                    lda ?topchk,X           ;Get new direction
                    ifmi                    ;It's straight up, get random direction
                        lda #01             ;Either 1 or 7
                        bit rands
                        ifmi
                            lda #07
                        endif
                    endif
                endif
            endif
        endif
        ldy bonusa          ;Bonus must be greater than zero to stop
        ifne                    ;If yes, get off screen
            ldy statyh          ;Must recall position
            cpy #04				;Originally $03
            ifge                        ;Too Low
                ldy ?botok,X            ;Going an ok direction
                ifpl                        ;no
                    jsr ?statstop                ;Stop it fast
                    ifeq                        ;if stopped, get new direction
                        lda ?botchk,X               ;Bottom check
                        ifmi
                            lda #03
                            bit rands+1
                            ifmi
                                lda #05                 ;One Random Check
                            endif
                        endif
                    endif
                endif
            endif
        endif
        ;*-------------------------------------------------*
        ;*------------ Check X now ------------------------*
        ;*-------------------------------------------------*
        ldy statxh              ;X MSB
        cpy #01                 ;To far left?
        ifcc
            ldy ?ltok,X             ;Going an ok direction?
            ifpl                        ;no
                jsr ?statstop                ;Stop it!
                lsr saucvl
                ifeq                            ;Stopped?
                    lda ?sfltchk,X                  ;Get new left direction
                    ifmi                            ;Straight in, pick new direction
                        lda #05                     ;Guess RU
                        bit rands
                        ifmi
                            lda #07                     ;Use RD
                        endif
                    endif
                endif
            endif
        endif
        ldy statxh              ;Reload position
        cpy #rtedge-1               ;To far right?
        ifcs                        ;yes
            ldy ?rtok,X                 ;Going an ok direction?
            ifpl
                jsr ?statstop
                lsr saucvl
                ifeq
                    lda ?sfrtchk,X
                    ifmi
                        lda #03                 ;Guess LU
                        bit rands+1
                        ifmi
                            lda #01                 ;Use LD
                        endif
                    endif
                endif
            endif
        endif
        sta stbix
        tax 
        lda ?nextmot,X
        sta stbflg              ;Next motion flags
		rts
		
		lda dropcnt             ;Dropping from last time?
        ifeq
            lda frame
            and #$0F                    ;Time Interval
            ifeq                        ;Time for new count
                lda ?shtnum,X               ;How many shots from here
                sta dropcnt             	;New count
            endif
        endif
		rts

;Stop the station from moving now...        
?statstop    
        ldy #$00
        sty saucvd              ;Stop it fast
		;ldy saucvl
        rts 

		
?movefire
		lda #$08
		pha					;save it for below
		ldy	#00				;MSB Velocity
		lda	shipxl
		cmp	objxl+zshot
		lda	shipxh			;See where we are
		sbc	objxh+zshot		;Which side
		ifcc
			dey					;Prop sign
			pla	
			jsr	neg
		else
			pla
		endif
		ldx #zshot
		jsr	addvel			;Update Velocity		
		;X Velocity update
		ldy	#00				;Sign Extend
		lda	velxh+zshot		;MSB of velocity
		ifmi
			dey				;Prop Sign
		endif
		adc	objxl+zshot
		sta	objxl+zshot
		sta	vdata			;Save for Draw and Collision
		tya
		adc	objxh+zshot
		sta	objxh+zshot
		sta	vdata+1
		;Y Velocity update
		ldy	#00
		lda	velyh+zshot
		ifmi	
			dey	
		endif
		adc	objyl+zshot		;MSB Y velocity
		sta	objyl+zshot
		sta	vdata+2
		tya					;Prop Sign
		adc	objyh+zshot		;Shot Speed in Y (prop carry)
		sta	objyh+zshot
		sta	vdata+3			;Check bottom for player ship collision and off bottom
		rts
		
;*************************************************
    .sbttl "Shot Delta"
;*************************************************
;* Calculates the delta X & Y from the gun to    *
;* the ship.                                     *
;*                                               *
;* Inputs:  vdata   = gun location X             *
;*          vdata+2 = gun location Y             *
;* (above are not corrected for vanishing point) *
;*          shipx = ships X position             *
;*          shipy = ships Y position             *
;*                                               *
;* Output:  temp1(2) = delta x                   *
;*          temp2(2) = delta y                   *
;*                                               *
;* Uses:    Y,A,shtdlx,shtdly                    *
;*          2 bytes stack (calls gunloc)         *
;*          temp1,temp2                          *
;*************************************************  
shotdelta   
        jsr gunloc          ;Locate gun 0 only
        lda shipxl          ;Locate gun
        sec 
        sbc statxl
        sta temp1           ;delta x lsb
        lda shipxh
        sbc statxh
        sta temp1+1         ;delta x msb
        sec 
        lda shipyl
        sbc statyl
        sta temp2           ;delta y lsb
        lda shipyh
        sbc statyh
        sta temp2+1         ;delta y msb
		rts
		;added early rts
		
        ;* Now scale up the speed for faster shots *
?spdaccx = 1
?spdaccy = 1

		inc temp1
		inc temp2

        ldy #?spdaccx            ;speed factor
        lda temp1
        begin
            asl a
            rol temp1+1
            dey
        miend
        sta temp1
		ldy #?spdaccy
        lda temp2
        begin
            asl a
            rol temp2+1
            dey
        miend
        sta temp2
        rts 
				
;*************************************************
    .sbttl "Gun Location"
;*************************************************
;* Setes the X and Y location of the fireball    
;* based on the current location of the base 
;* X contains shot slot
;*************************************************
gunloc  lda statxl
        sta vdata               ;For delta calculation
        sta objxl,X			;Save object location
        lda statxh
        sta vdata+1
        sta objxh,X
        lda statyl
        sta vdata+2
        sta objyl,X
        lda statyh
        sta vdata+3         	;vdata+2 = fireball position y
        sta objyh,X        	;for shot routine later
        rts 

barloc  lda shotxl,X
        sta vdata               ;For output of picture
        sta sobjxl,Y
        lda shotxh,X
        sta vdata+1
        sta sobjxh,Y
        lda shotyl,X
        sta vdata+2
        sta sobjyl,Y
        lda shotyh,X
        sta vdata+3         	;vdata+2 = gun's position y
        sta sobjyh,Y            ;for shot routine later
        rts 
		
;*******************************************
;Set all shield colors to the value in A
;*******************************************
;setshdcols
;		ldx #07
;		begin
;			sta shiecols,X
;			dex
;		miend
;		rts
		
;*******************************************
;Set all shield status to the value in A
;*******************************************
setshdstat
		ldx #07
		begin
			sta shiests,X
			dex
		miend
		rts
   	
;*******************************************************************
;* Drawshield - main routine for drawing the rotating
;*              shield around the homeworld. Called 
;* 				from tw_ship.
;*
;* Global		Local		Size 	Purpose
;*------------------------------------------------------------------
;* temp1		?curvec		2 		Pointer to the current vector
;* temp2					2		Not used
;* temp3		?panlen		1		Holds length of current panel vectors
;* temp3+1		?lenidx		1		Indexer into length table
;* temp4		?panrot		1		Holds the current panel rotation (0x07 masked)
;* temp4+1					1		Not used
;* temp5		?rotbits	1		Shield bits for rotating loop
;* temp5+1					1		Not Used
?curvec 	= temp1
?panlen 	= temp3
?lenidx		= temp3+1
?panrot 	= temp4
?pancnt		= temp4+1
?rotbits 	= temp5
?rotindx	= temp5+1

CASTLEC_CLR_OVERRIDE = 0

drawshield
		jsr posvec          ;place this and set scale
		lda shirotl
		;and #$07			;Can be deleted because shirotl never goes above 7
		sta ?panrot			;?panrot holds the rotation byte indexer
		asl A				;words for this one tho
		tax
		lda statshs,X
		sta ?curvec
		lda statshs+1,X
		sta ?curvec+1			;Start Vector base table in curvec

		ldy #00
		ldx #04				;Copy 4 bytes for the positioning vector
		begin
			lda (?curvec,Y)
			sta (vglist,Y)
			iny
			dex
		eqend
		dey					;fix overincremented Y
		jsr vgadd			;true up our VGINDEX
		
		;now load the panels if they are visible
		lda ?panrot
		asl A				;Words
		tax		
		lda statshe,X
		sta ?curvec
		lda statshe+1,X
		sta ?curvec+1			;Rotation base in temp1 (word)	
		
		;set up panel vector lengths
		txa
		lsr	A					;back to bytes
		tax
		lda statshl,X		
		sta ?lenidx				;indexer into the length table, we keep this around for below

		jsr ?rotate8
		sta ?rotbits			;Save our shield bits in ?rotbits for the loop
		
		lda #08
		sec
		sbc shiroth
		sta ?rotindx
		
		lda #07
		sta ?pancnt
		begin
			;color
			lda shiecovr
			ifeq
				lda ?rotindx
				and #$07
#IF CASTLEC_CLR_OVERRIDE > 0
				;hack colors are different for each panel, for troubleshooting
				clc
				adc #1
				ora #$F0
#ELSE
				;Original Code
				tax
				lda shiests,X
				and #$18
				ifne
					lda shiecf			;Flash coloring
				else
					lda shiests,X
					and #07
					tax
					lda shiecols,X		;Loads the color and brighness of the shield
				endif
				;End Original Code
#ENDIF
			endif
			ldx #bas_vpg		;Stat and page (bases) select
			jsr vgadd2			;NOTE: this destroys Y values
			
			ldx ?lenidx			;length
			lda statshl0,X
			sta ?panlen			;panlen now has the current length of the panel
			
			ldy #00				;Y base is reset
			lda ?rotbits
			clc
			lsr A				;put our bit into C
			sta ?rotbits
			ifcs
				begin
					lda (?curvec,Y)		;do until length
					sta (vglist,Y)
					iny 
					dec ?panlen
				eqend
			else
				;skip the panel vectors
				ldy ?panlen
				jsr shiadd	;this adds Y value + 1 to base pointer
				ldy #00
			endif
			;always draw the next positioning vector (not-visible)				
			lda (?curvec,Y)
			sta (vglist,Y)
			iny
			lda (?curvec,Y)
			sta (vglist,Y)
			iny
			lda (?curvec,Y)
			sta (vglist,Y)
			iny
			lda (?curvec,Y)
			sta (vglist,Y)
			jsr vgadd			;Sync up VGINDEX
			iny
			jsr shiadd			;Sync up SHINDEX
			
			inc ?lenidx			;next length pointer	
			inc ?rotindx
			dec ?pancnt
		miend
		
		rts
		;Fall through to draw fireball if active
		
		lda objst+zshot
		ifne
			jsr vgcntr
			
			lda objxl+zshot
			sta vdata
			lda objxh+zshot
			sta vdata+1
			lda objyl+zshot
			sta vdata+2
			lda objyh+zshot
			sta vdata+3
		endif
		rts
		
;***********************************
; Normalize Rotation bits
;***********************************
; we need to shift based upon the 
; current rotationthis is a clean 
; 8-bit rotate;
?rotate8	
		lda shibits				;bit definitions to draw
		ldx shiroth
		begin
			ifne
				clc
				rol A
				ifcs
					ora #$01
				endif
			endif
			dex
		miend
		rts

;***********************************
;* Update curvec with data already
;* added.
;***********************************		
shiadd  tya 
        clc 
        adc ?curvec
        sta ?curvec
        ifcs
            inc ?curvec+1
        endif
        rts 

;Draw Maynard
?dmaynard
		jsr vgcntr
		lda #01
		ldy #00
		jsr vgscal
		;static position 
		lda #$10  					;X = $012C
		sta vdata
		lda #$00					
		sta vdata+1
		lda #$F0					;Y = $012C
		sta vdata+2
		lda #$00
		sta vdata+3
		;jsr posvec
		lda #00
        sta vgbrit
		jsr vgvtr2 
		lda #($A0+colgreen)			;Color
        ldx #$60
        jsr vgadd2
		lda #07
		ldy #00
		jsr vgscal					;Tiny when static sized
		VGADD_JSR(maynard)			;Add planet
		rts
		
;*********************************************************
;* Shot to Shield Collision - this checks a specific
;* slot for a collision with the incoming panel index.
;* The calling routine will check the slots in an order
;* that allows for checking collisions around the space 
;* station properly.
;*
;* temp9 = X index to the shot
;* USES LOCAL VARS DEFINED BELOW
;*
;* Returns A = $00 - No Hit
;*         A = $80 - Hit
;*********************************************************
?targslot = temp7
?curridx = temp7+1
?panvidx = temp8
?hitindex = temp8+1
;*********************************************************	
ckshield
		lda #7				
		jsr ?chks		;Check right side
		ifpl
			;Didn't hit, continue
			lda #0
			jsr ?chks		;Check bottom-right side
			ifpl
				;Didn't hit, continue
				lda #1
				jsr ?chks		;Check bottom side
				ifpl
					;Didn't hit, continue
					lda #2
					jsr ?chks		;Check bottom-left side
					ifpl
						;Didn't hit, continue
						lda #3
						jsr ?chks		;Check left side
						ifpl
							jsr ckbase		;See if shot hits the base
							ldx temp9
							lda shotst,X    	;still alive?
							ifpl
								lda #4	
								jsr ?chks		;Check top-left side
								ifpl
									lda #6
									jsr ?chks		;Check top-right side
								endif
							endif
						endif
					endif
				endif
			endif
		endif
		rts
		
?chks	sta ?targslot
		jsr ?getslotpan 	;Will return the panel index in this slot	
		sta ?curridx
		;Not needed here, checked back in twthings.asm
		;lda shotst,X 
		;beq ?cksrts		;leave if this shot is not active
		;bmi ?cksrts		;leave if this shot is already exploding	
		jsr cnt2bitf		;destroys X
		and shibits			;See if this panel is alive
		ifne
			jsr ?chkpanel
			ifmi 
				;we hit no reason to continue, returns A = $80
				rts
			endif
		endif
		lda #0		;no hit returns A = $00
		rts

;**********************************************
; Check specific panel in ?curridx
; 
; Returns: A = $00 didn't hit
;          A = $80 hit panel
;**********************************************
?chkpanel
		lda shirotl			;(minor rotational index 0-7)
		asl A
		asl A
		asl A 
		asl A
		asl A				;x32 for each rotation
		sta ?panvidx
		lda ?targslot
		;lda ?curridx
		;jsr getpanidx		;Will return the panel slot index that this panel is in with rotation
		asl A 
		asl A				;x4 because two words per
		ora ?panvidx		;Add it in too
		sta ?panvidx
		tay
		ldx temp9			;restore shot indexer
		jsr boundck			;See if this shot(X) is in the boundaries of the panel at vector index (Y)
		ifne
			;hit on panel index in ?curridx
			lda scflags
			cmp #sc_state_fight		;can we take hits
			ifne
				lda #snd_schit			;Squishy Sound - Hit but not destroyed
				jmp dosound				;do sound and exit
			else
				ldx ?curridx
				lda shiests,X
				and #07
				cmp #panel_hits			;Use max hits defined up TOP
				ifeq
					;disable panel now... it is done
					txa
					jsr cnt2bitf			;Destroys X
					eor #$FF				;Inverse our bits
					and shibits
					sta shibits
					;also drop a red bar
					jsr dropbar				
				else
					iflt
						pha
						lda #snd_schit			;Squishy Sound - Hit but not destroyed
						jsr dosound
						pla
						clc
						adc #($18+1)			;set flash flags and also increment the hits
						and #$9F				;clear the rotation counter bits
						sta shiests,X
					endif
				endif
				ldx temp9				;restore the shot indexer
				lda #$80
				sta shotst,X    		;Kill this shot
				;rts						;EARLY EXIT - LEAVE NOW!
			endif
		endif
		rts

;**********************************************
;* Panel Bounds Checking
;**********************************************
;* Returns > 0 for panel index hit
;**********************************************
boundck	;lda #0
		;sta lasthitfg
		jsr chkybot				;check Y-Bottom first because it eliminates the most invalid positions
		ifpl		;positive results are a hit
			; lda lasthitfg
			; ora #$80
			; sta lasthitfg
			jsr chkxleft			;then check X-Bounds left side
			ifmi					;negative results are right of left bounds
				; lda lasthitfg
				; ora #$40
				; sta lasthitfg
				jsr chkxrigh			;check X-Bounds right side
				ifpl					;positive results are left of right bounds
					; lda lasthitfg
					; ora #$20
					; sta lasthitfg
					jsr chkytop				;finally Y-Top, which will be minimal
					ifmi					;negative results are below top bounds
						; lda lasthitfg
						; ora #$10
						; sta lasthitfg
						jsr chkyang				;Checks now on angled panels since we are in bounding box
						ifpl						;Will return positive or ZERO for non-angled panels)
							; lda lasthitfg
							; ora #$08
							; sta lasthitfg
							;We have a hit on ?curridx which is 0-7
							lda #-1
							rts
						endif
					endif
				endif
			endif
		endif					
		lda #0			;No hit		
		rts						
								
dropbar ldx temp9				;X is index to shot that hit the panel
		ldy #00
		sty sobjs2              ;Turn off aux stat on 'Fighter' shooting
		jsr barloc              ;Place red bar at collision point
		lda #zshot+nmshot-2 	
		ldx #00
		stx temp5               ;Flag 0 = from station
		jsr drop3               ;Drop panel
		ifpl                    ;Started one
			lda #$11
			sta objst,Y             ;Set to line from here
			dec dropcnt             ;Got one
		endif
		lda #$25
		jsr bpont2          	;And 2500 points
		lda #snd_scdrop			;Drop sound		
		jmp dosound

;*********************************************
;Normalize index based upon rotation	
;*********************************************	
getpanidx
		ldx shiroth
		ifne
			begin
				clc
				adc #01
				dex
			eqend
		endif
		and #07
		rts

;*********************************************
; Get the panel index that is currently
; in the passed slot (A)
; Return: Panel Index in (A)	
;*********************************************		
?getslotpan
		ldx shiroth
		ifne
			begin
				sec
				sbc #01
				dex
			eqend
		endif
		and #07
		rts
		
;*************************************************************************
; CNT2BITF - Will take the value in A and convert it into a bit 
;            position back in A
; 00 = 01	01 = 02		02 = 04 	03 = 08
; 04 = 10	05 = 20		06 = 40		07 = 80
;*************************************************************************
?bits	.db $01,$02,$04,$08,$10,$20,$40,$80

cnt2bitf
		and #07
		tax
		lda ?bits,X
		rts
		
		; and #07
		; ifne
			; tax
			; ;change count to bit flag
			; lda #01
			; clc
			; begin
				; rol A
				; dex
			; eqend
		; else
			; lda #01
		; endif
		; rts
		
chkxleft
		;XAxis - Add Left Bound Offset
		lda statxl
		clc
		adc shiexs,Y
		sta temp1
		lda statxh
		adc shiexs+1,Y
		sta temp1+1		;Left X bound in temp1
		ifpl			;If this went minus, then we are off the left side of the screen
						;just return N because every shot invalid
			;XAxis Left Bound - Calc difference between panel and shot back into temp1
			lda temp1
			clc				;makes zero to negative
			sbc shotxl,X
			sta temp1
			lda temp1+1
			sbc shotxh,X
			sta temp1+1
		endif
		rts
		
chkxrigh	
		;XAxis - Add Right Bound Offset
		lda statxl
		clc
		adc shiexs+2,Y
		sta temp1
		lda statxh
		adc shiexs+3,Y
		sta temp1+1		;Right X bound in temp1
		
		;XAxis Right Bound - Calc difference between panel and shot back into temp1
		lda temp1
		clc				;makes zero to negative
		sbc shotxl,X
		sta temp1
		lda temp1+1
		sbc shotxh,X
		sta temp1+1
		rts
		
chkybot	
		;YAxis - Add Bottom Bound Offset
		lda statyl
		clc
		adc shieys,Y
		sta temp2
		lda statyh
		adc shieys+1,Y
		sta temp2+1		;Bottom Y bound in temp2 now
		
		;YAxis Bottom Bound - Calc difference between panel and shot back into temp2
		lda temp2
		clc				;makes zero to negative
		sbc shotyl,X
		sta temp2
		lda temp2+1
		sbc shotyh,X
		sta temp2+1
		rts

chkytop
		;YAxis - Add Top Bound Offset
		lda statyl
		clc
		adc shieys+3,Y
		sta temp2
		lda statyh
		adc shieys+3,Y
		sta temp2+1		;Left Y bound in temp2
					
		;YAxis Top Bound - Calc difference between panel and shot back into temp2
		lda temp2
		clc				;makes zero to negative
		sbc shotyl,X
		sta temp2
		lda temp2+1
		sbc shotyh,X
		sta temp2+1
		rts
		
;special Y check for angled panes
;Returns a positive value for shot above angled surface
;Returns zero (which is still positive) for non-angled surfaces		
chkyang
		;we also need to modify the Shield Offset additionally for angled 
		;shield panels
		lda shieic,Y
		ifne
			;we need to sbc the amount in A to the current hit point for every 32 in X-Bounds
			;This is the current position >> 5
			;temp2 and temp2+1 hold our difference value
			
		endif
		lda #00
		rts
		

;***************************************
;* Color table for shied based on hits	
;***************************************
shiecols
		.db ($F0+colred2)
		.db ($D0+colred)
		.db ($B0+colorange)
		.db ($90+colyellow)
		.db ($70+colflash)
		.db ($50+colflash)
		.db ($30+colflash)
		.db ($10+colflash)
		
shiecf	.db ($F0+colflash) ;used for init and when panel is destroyed
shieic  .db 0,$20,0,-$20,0,$20,0,-$20	;Value to add to Y for each segment of X

;*********************************************************
; Station Shield Parts - Octagon
; Side Length: 165 * 3.0 = 495
;*********************************************************
statshs  .dw statshs0,statshs1,statshs2,statshs3,statshs4,statshs5,statshs6,statshs7

statshs0	vctrl(635d, -94d, hidden)
statshs1	vctrl(616d, -118d, hidden)
statshs2	vctrl(591d, -139d, hidden)
statshs3	vctrl(560d, -160d, hidden)
statshs4	vctrl(523d, -180d, hidden)
statshs5	vctrl(480d, -198d, hidden)
statshs6	vctrl(431d, -216d, hidden)
statshs7	vctrl(378d, -231d, hidden)

statshe	.dw statshe0,statshe1,statshe2,statshe3,statshe4,statshe5,statshe6,statshe7

statshe0
		;First position, front face - X:69 Y:00 Z:00
		vctrl(-14d, -230d, visible) \ 		vctrl(-338d, -174d, visible) \ 		vctrl(7d, 249d, visible) \ 		vctrl(346d, 156d, visible)
		vctrl(-346d, -156d, hidden)
		vctrl(-7d, -249d, visible) \ 		vctrl(-564d, 0d, visible) \ 		vctrl(-7d, 249d, visible) \ 		vctrl(579d, 0d, visible)
		vctrl(-579d, 0d, hidden)
		vctrl(7d, -249d, visible) \ 		vctrl(-338d, 174d, visible) \ 		vctrl(-14d, 230d, visible) \ 		vctrl(346d, -156d, visible)
		vctrl(-346d, 156d, hidden)
		vctrl(14d, -230d, visible) \ 		vctrl(69d, 199d, visible) \ 		vctrl(-11d, 208d, visible) \ 		vctrl(-72d, -178d, visible)
		vctrl(72d, 178d, hidden)
		vctrl(11d, -208d, visible) \ 		vctrl(224d, 76d, visible) \ vctrl(115d, 39d, hidden) \ 		vctrl(-4d, 195d, hidden) \ 		vctrl(-121d, -36d, hidden) \ vctrl(-225d, -67d, visible)
		vctrl(347d, 103d, hidden)
		vctrl(4d, -195d, hidden) \ 		vctrl(423d, 0d, hidden) \ 		vctrl(4d, 195d, hidden) \ 		vctrl(-432d, 0d, hidden)
		vctrl(432d, 0d, hidden)
		vctrl(-4d, -195d, hidden) \ 		vctrl(152d, -52d, hidden) \ vctrl(186d, -64d, visible) \ 		vctrl(11d, 208d, visible) \ 		vctrl(-191d, 56d, visible) \ vctrl(-156d, 46d, hidden)
		vctrl(347d, -103d, hidden)
		vctrl(-11d, -208d, visible) \ 		vctrl(69d, -199d, visible) \ 		vctrl(14d, 230d, visible) \ 		vctrl(-72d, 178d, visible)
		vctrl(0d, 0d, hidden)

statshe1
		;Second position - X:69 Y:00 Z:06
		vctrl(-14d, -233d, visible) \ 		vctrl(-386d, -159d, visible) \ 		vctrl(5d, 250d, visible) \ 		vctrl(395d, 143d, visible)
		vctrl(-395d, -143d, hidden)
		vctrl(-5d, -250d, visible) \ 		vctrl(-560d, 26d, visible) \ 		vctrl(-8d, 247d, visible) \ 		vctrl(574d, -23d, visible)
		vctrl(-574d, 23d, hidden)
		vctrl(8d, -247d, visible) \ 		vctrl(-288d, 186d, visible) \ 		vctrl(-14d, 227d, visible) \ 		vctrl(294d, -166d, visible)
		vctrl(-294d, 166d, hidden)
		vctrl(14d, -227d, visible) \ 		vctrl(113d, 193d, visible) \ 		vctrl(-10d, 206d, visible) \ 		vctrl(-117d, -172d, visible)
		vctrl(117d, 172d, hidden)
		vctrl(10d, -206d, visible) \ 		vctrl(216d, 61d, visible) \ vctrl(144d, 41d, hidden) \ 		vctrl(-3d, 194d, hidden) \ 		vctrl(-165d, -40d, hidden) \ vctrl(-202d, -50d, visible)
		vctrl(367d, 91d, hidden)
		vctrl(3d, -194d, hidden) \ 		vctrl(422d, -14d, hidden) \ 		vctrl(5d, 196d, hidden) \ 		vctrl(-430d, 13d, hidden)
		vctrl(430d, -13d, hidden)
		vctrl(-5d, -196d, hidden) \ 		vctrl(79d, -32d, hidden) \ vctrl(237d, -97d, visible) \ 		vctrl(12d, 211d, visible) \ 		vctrl(-226d, 80d, visible) \ vctrl(-97d, 34d, hidden)
		vctrl(324d, -115d, hidden)
		vctrl(-12d, -211d, visible) \ 		vctrl(22d, -204d, visible) \ 		vctrl(14d, 233d, visible) \ 		vctrl(-24d, 182d, visible)
		vctrl(0d, 0d, hidden)

statshe2
		;Third position - X:69 Y:00 Z:11
		vctrl(-14d, -236d, visible) \ 		vctrl(-427d, -143d, visible) \ 		vctrl(3d, 251d, visible) \ 		vctrl(437d, 128d, visible)
		vctrl(-437d, -128d, hidden)
		vctrl(-3d, -251d, visible) \ 		vctrl(-549d, 50d, visible) \ 		vctrl(-10d, 245d, visible) \ 		vctrl(563d, -45d, visible)
		vctrl(-563d, 45d, hidden)
		vctrl(10d, -245d, visible) \ 		vctrl(-240d, 195d, visible) \ 		vctrl(-14d, 225d, visible) \ 		vctrl(244d, -174d, visible)
		vctrl(-244d, 174d, hidden)
		vctrl(14d, -225d, visible) \ 		vctrl(152d, 186d, visible) \ 		vctrl(-10d, 204d, visible) \ 		vctrl(-157d, -165d, visible)
		vctrl(157d, 165d, hidden)
		vctrl(10d, -204d, visible) \ 		vctrl(188d, 44d, visible) \ vctrl(188d, 44d, hidden) \ 		vctrl(-2d, 194d, hidden) \ 		vctrl(-192d, -39d, hidden) \ vctrl(-192d, -39d, visible)
		vctrl(384d, 79d, hidden)
		vctrl(2d, -194d, hidden) \ 		vctrl(419d, -28d, hidden) \ 		vctrl(6d, 197d, hidden) \ 		vctrl(-427d, 25d, hidden)
		vctrl(427d, -25d, hidden)
		vctrl(-6d, -197d, hidden) \ 		vctrl(43d, -21d, hidden) \ vctrl(248d, -120d, visible) \ 		vctrl(13d, 213d, visible) \ 		vctrl(-254d, 106d, visible) \ vctrl(-44d, 18d, hidden)
		vctrl(299d, -125d, hidden)
		vctrl(-13d, -213d, visible) \ 		vctrl(-23d, -207d, visible) \ 		vctrl(14d, 236d, visible) \ 		vctrl(22d, 184d, visible)
		vctrl(0d, 0d, hidden)

statshe3
		;Fourth Position - back face - X:69 Y:00 Z:16
		vctrl(-13d, -238d, visible) \ 		vctrl(-463d, -125d, visible) \ 		vctrl(2d, 251d, visible) \ 		vctrl(475d, 112d, visible)
		vctrl(-475d, -112d, hidden)
		vctrl(-2d, -251d, visible) \ 		vctrl(-531d, 74d, visible) \ 		vctrl(-11d, 243d, visible) \ 		vctrl(545d, -66d, visible)
		vctrl(-545d, 66d, hidden)
		vctrl(11d, -243d, visible) \ 		vctrl(-190d, 201d, visible) \ 		vctrl(-14d, 222d, visible) \ 		vctrl(193d, -180d, visible)
		vctrl(-193d, 180d, hidden)
		vctrl(14d, -222d, visible) \ 		vctrl(188d, 177d, visible) \ 		vctrl(-9d, 202d, visible) \ 		vctrl(-194d, -157d, visible)
		vctrl(194d, 157d, hidden)
		vctrl(9d, -202d, visible) \ 		vctrl(136d, 26d, visible) \ vctrl(253d, 49d, hidden) \ 		vctrl(-1d, 193d, hidden) \ 		vctrl(-278d, -47d, hidden) \ vctrl(-119d, -20d, visible)
		vctrl(398d, 67d, hidden)
		vctrl(1d, -193d, hidden) \ 		vctrl(413d, -42d, hidden) \ 		vctrl(7d, 198d, hidden) \ 		vctrl(-421d, 37d, hidden)
		vctrl(421d, -37d, hidden)
		vctrl(-7d, -198d, visible) \ 		vctrl(265d, -153d, visible) \ 		vctrl(13d, 216d, visible) \ 		vctrl(-272d, 135d, visible)
		vctrl(272d, -135d, hidden)
		vctrl(-13d, -216d, visible) \ 		vctrl(-71d, -208d, visible) \ 		vctrl(13d, 238d, visible) \ 		vctrl(71d, 185d, visible)
		vctrl(0d, 0d, hidden)

statshe4
		;Fifth position - X:69 Y:00 Z:21
		vctrl(-12d, -240d, visible) \ 		vctrl(-495d, -105d, visible) \ 		vctrl(0d, 251d, visible) \ 		vctrl(508d, 94d, visible)
		vctrl(-508d, -94d, hidden)
		vctrl(0d, -251d, visible) \ 		vctrl(-507d, 96d, visible) \ 		vctrl(-12d, 241d, visible) \ 		vctrl(520d, -86d, visible)
		vctrl(-520d, 86d, hidden)
		vctrl(12d, -241d, visible) \ 		vctrl(-141d, 205d, visible) \ 		vctrl(-14d, 219d, visible) \ 		vctrl(143d, -183d, visible)
		vctrl(-143d, 183d, hidden)
		vctrl(14d, -219d, visible) \ 		vctrl(222d, 168d, visible) \ 		vctrl(-8d, 200d, visible) \ 		vctrl(-228d, -149d, visible)
		vctrl(228d, 149d, hidden)
		vctrl(8d, -200d, visible) \ 		vctrl(80d, 12d, visible) \ vctrl(321d, 50d, hidden) \ 		vctrl(0d, 193d, hidden) \ 		vctrl(-368d, -49d, hidden) \ vctrl(-40d, -5d, visible)
		vctrl(409d, 55d, hidden)
		vctrl(0d, -193d, hidden) \ 		vctrl(385d, -53d, hidden) \ vctrl(20d, -2d, visible) \ 		vctrl(7d, 200d, visible) \ 		vctrl(-20d, 2d, visible) \ vctrl(-392d, 47d, hidden)
		vctrl(413d, -50d, hidden)
		vctrl(-7d, -200d, visible) \ 		vctrl(235d, -163d, visible) \ 		vctrl(14d, 218d, visible) \ 		vctrl(-242d, 145d, visible)
		vctrl(242d, -145d, hidden)
		vctrl(-14d, -218d, visible) \ 		vctrl(-120d, -207d, visible) \ 		vctrl(12d, 240d, visible) \ 		vctrl(121d, 184d, visible)
		vctrl(0d, 0d, hidden)

statshe5
		;Sixth Position - X:69 Y:00 Z:27
		vctrl(-11d, -243d, visible) \ 		vctrl(-522d, -83d, visible) \ 		vctrl(-1d, 251d, visible) \ 		vctrl(535d, 75d, visible)
		vctrl(-535d, -75d, hidden)
		vctrl(1d, -251d, visible) \ 		vctrl(-478d, 117d, visible) \ 		vctrl(-13d, 239d, visible) \ 		vctrl(489d, -105d, visible)
		vctrl(-489d, 105d, hidden)
		vctrl(13d, -239d, visible) \ 		vctrl(-92d, 208d, visible) \ 		vctrl(-13d, 217d, visible) \ 		vctrl(92d, -185d, visible)
		vctrl(-92d, 185d, hidden)
		vctrl(13d, -217d, visible) \ 		vctrl(253d, 157d, visible) \ 		vctrl(-7d, 199d, visible) \ 		vctrl(-259d, -140d, visible)
		vctrl(259d, 140d, hidden)
		vctrl(7d, -199d, visible) \ 		vctrl(20d, 2d, visible) \ vctrl(389d, 46d, hidden) \ 		vctrl(0d, 193d, hidden) \ 		vctrl(-397d, -41d, hidden) \ vctrl(-20d, -2d, visible)
		vctrl(418d, 43d, hidden)
		vctrl(0d, -193d, hidden) \ 		vctrl(355d, -63d, hidden) \ vctrl(39d, -7d, visible) \ 		vctrl(8d, 201d, visible) \ 		vctrl(-40d, 6d, visible) \ vctrl(-362d, 56d, hidden)
		vctrl(403d, -62d, hidden)
		vctrl(-8d, -201d, visible) \ 		vctrl(203d, -173d, visible) \ 		vctrl(14d, 221d, visible) \ 		vctrl(-209d, 154d, visible)
		vctrl(209d, -154d, hidden)
		vctrl(-14d, -221d, visible) \ 		vctrl(-169d, -203d, visible) \ 		vctrl(11d, 243d, visible) \ 		vctrl(172d, 182d, visible)
		vctrl(0d, 0d, hidden)

statshe6
		;Seventh Position - X:69 Y:00 Z:32
		vctrl(-10d, -245d, visible) \ 		vctrl(-542d, -60d, visible) \ 		vctrl(-3d, 251d, visible) \ 		vctrl(556d, 54d, visible)
		vctrl(-556d, -54d, hidden)
		vctrl(3d, -251d, visible) \ 		vctrl(-443d, 136d, visible) \ 		vctrl(-13d, 237d, visible) \ 		vctrl(454d, -122d, visible)
		vctrl(-454d, 122d, hidden)
		vctrl(13d, -237d, visible) \ 		vctrl(-44d, 207d, visible) \ 		vctrl(-13d, 214d, visible) \ 		vctrl(43d, -185d, visible)
		vctrl(-43d, 185d, hidden)
		vctrl(13d, -214d, visible) \ 		vctrl(281d, 146d, visible) \ 		vctrl(-6d, 198d, visible) \ 		vctrl(-288d, -130d, visible)
		vctrl(288d, 130d, hidden)
		vctrl(6d, -198d, hidden) \ 		vctrl(416d, 34d, hidden) \ 		vctrl(1d, 194d, hidden) \ 		vctrl(-425d, -30d, hidden)
		vctrl(425d, 30d, hidden)
		vctrl(-1d, -194d, hidden) \ 		vctrl(286d, -62d, hidden) \ vctrl(95d, -20d, visible) \ 		vctrl(9d, 203d, visible) \ 		vctrl(-97d, 18d, visible) \ vctrl(-292d, 55d, hidden)
		vctrl(390d, -74d, hidden)
		vctrl(-9d, -203d, visible) \ 		vctrl(168d, -182d, visible) \ 		vctrl(14d, 223d, visible) \ 		vctrl(-173d, 162d, visible)
		vctrl(173d, -162d, hidden)
		vctrl(-14d, -223d, visible) \ 		vctrl(-219d, -198d, visible) \ 		vctrl(10d, 245d, visible) \ 		vctrl(222d, 177d, visible)
		vctrl(0d, 0d, hidden)

statshe7
		;Eighth Position - X:69 Y:00 Z:37
		vctrl(-9d, -246d, visible) \ 		vctrl(-556d, -36d, visible) \ 		vctrl(-4d, 250d, visible) \ 		vctrl(570d, 33d, visible)
		vctrl(-570d, -33d, hidden)
		vctrl(4d, -250d, visible) \ 		vctrl(-404d, 153d, visible) \ 		vctrl(-14d, 234d, visible) \ 		vctrl(413d, -137d, visible)
		vctrl(-413d, 137d, hidden)
		vctrl(14d, -234d, visible) \ 		vctrl(2d, 206d, visible) \ 		vctrl(-12d, 212d, visible) \ 		vctrl(-4d, -183d, visible)
		vctrl(4d, 183d, hidden)
		vctrl(12d, -212d, visible) \ 		vctrl(291d, 128d, visible) \ vctrl(15d, 6d, hidden) \ 		vctrl(-5d, 196d, hidden) \ 		vctrl(-31d, -11d, hidden) \ vctrl(-282d, -107d, visible)
		vctrl(313d, 119d, hidden)
		vctrl(5d, -196d, hidden) \ 		vctrl(421d, 20d, hidden) \ 		vctrl(2d, 194d, hidden) \ 		vctrl(-429d, -18d, hidden)
		vctrl(429d, 18d, hidden)
		vctrl(-2d, -194d, hidden) \ 		vctrl(202d, -53d, hidden) \ vctrl(165d, -43d, visible) \ 		vctrl(10d, 205d, visible) \ 		vctrl(-168d, 38d, visible) \ vctrl(-206d, 47d, hidden)
		vctrl(375d, -86d, hidden)
		vctrl(-10d, -205d, visible) \ 		vctrl(130d, -190d, visible) \ 		vctrl(14d, 226d, visible) \ 		vctrl(-134d, 169d, visible)
		vctrl(134d, -169d, hidden)
		vctrl(-14d, -226d, visible) \ 		vctrl(-268d, -190d, visible) \ 		vctrl(9d, 246d, visible) \ 		vctrl(273d, 170d, visible)
		vctrl(0d, 0d, hidden)


statshl	.db 0,statshl1-statshl0,statshl2-statshl0,statshl3-statshl0
		.db statshl4-statshl0,statshl5 -statshl0,statshl6-statshl0,statshl7-statshl0

statshl0	.db 16d,16d,16d,16d,24d,16d,24d,16d
statshl1	.db 16d,16d,16d,16d,24d,16d,24d,16d
statshl2	.db 16d,16d,16d,16d,24d,16d,24d,16d
statshl3	.db 16d,16d,16d,16d,24d,16d,16d,16d
statshl4	.db 16d,16d,16d,16d,24d,24d,16d,16d
statshl5	.db 16d,16d,16d,16d,24d,24d,16d,16d
statshl6	.db 16d,16d,16d,16d,16d,24d,16d,16d
statshl7	.db 16d,16d,16d,24d,16d,24d,16d,16d


;*********************************************************
; Shield Geometry:
?sidedim = 495
?halfdim = 247
?angldim = 350
;*********************************************************
?fakethik = $20  ; This is used for horizontal panes so they have a thickness
                 ; otherwise shots may skip through them

;*********************************************************
; Shield collision X - Values for bounds checking
;		LEFT,		RIGHT
shiexs
		;Major Rotation 0 - Two words for each panel position
		.dw 289	,635
		.dw -289,289
		.dw -635,-289
		.dw -635,-563
		.dw -563,-216
		.dw -216,216
		.dw 216	,563
		.dw 563	,635
		;Major Rotation 1
		.dw 220	,616
		.dw -353,220
		.dw -648,-353
		.dw -648,-531
		.dw -531,-163
		.dw -163,267
		.dw 267	,591
		.dw 591	,616
		;Rotation 2
		.dw 154	,591
		.dw -409,154
		.dw -654,-409
		.dw -654,-497
		.dw -497,-112
		.dw -112,314
		.dw 314	,614
		.dw 591	,614
		;Rotation 3
		.dw 85	,560
		.dw -460,85
		.dw -654,-460
		.dw -654,-460
		.dw -460,-62
		.dw -62	,359
		.dw 359	,631
		.dw 560	,631
		;Rotation 4
		.dw 15	,523
		.dw -505,15
		.dw -648,-505
		.dw -648,-420
		.dw -420,-10
		.dw -10	,402
		.dw 402	,644
		.dw 523	,644
		;Rotation 5
		.dw -55	,480
		.dw -545,-55
		.dw -637,-545
		.dw -637,-378
		.dw -378,40
		.dw 40	,443
		.dw 443	,652
		.dw 480	,652
		;Rotation 6
		.dw -124,431
		.dw -578,-124
		.dw -622,-578
		.dw -622,-334
		.dw -334,91
		.dw 91	,481
		.dw 481	,654
		.dw 431	,654
		;Rotation 7
		.dw -192,378
		.dw -606,-192
		.dw -606,-601
		.dw -601,-287
		.dw -287,141
		.dw 141	,516
		.dw 516	,651
		.dw 378	,651
;*********************************************************
; Shield collision Y - Values for bounds checking
; Adjustment factor: 0
;*********************************************************
;		BOTTOM,		TOP
shieys
		;Rotation 0
		.dw 250	,94
		.dw 250	,250
		.dw 250	,94
		.dw 94		,-83
		.dw -83		,-186
		.dw -186		,-186
		.dw -83		,-186
		.dw 94		,-83
		;Rotation 1
		.dw 261	,118
		.dw 261	,237
		.dw 237	,70
		.dw 70		,-101
		.dw -101		,-192
		.dw -179		,-192
		.dw -64		,-179
		.dw 118	,-64
		;Rotation 2
		.dw 268	,139
		.dw 268	,222
		.dw 222	,48
		.dw 48		,-117
		.dw -117		,-196
		.dw -171		,-196
		.dw -45		,-171
		.dw 139	,-45
		;Rotation 3
		.dw 273	,160
		.dw 273	,206
		.dw 206	,26
		.dw 26		,-131
		.dw -131		,-199
		.dw -161		,-199
		.dw -25		,-161
		.dw 160	,-25
		;Rotation 4
		.dw 274	,180
		.dw 274	,188
		.dw 188	,4
		.dw 4		,-144
		.dw -144		,-200
		.dw -150		,-200
		.dw -4		,-150
		.dw 180	,-4
		;Rotation 5
		.dw 274	,198
		.dw 274	,169
		.dw 169	,-16
		.dw -16		,-156
		.dw -156		,-199
		.dw -137		,-199
		.dw 16		,-137
		.dw 198	,16
		;Rotation 6
		.dw 270	,216
		.dw 270	,148
		.dw 148	,-36
		.dw -36		,-167
		.dw -167		,-197
		.dw -123		,-197
		.dw 38		,-123
		.dw 216	,38
		;Rotation 7
		.dw 264	,231
		.dw 264	,127
		.dw 127	,-56
		.dw -56		,-176
		.dw -176		,-194
		.dw -108		,-194
		.dw 60		,-108
		.dw 231	,60
