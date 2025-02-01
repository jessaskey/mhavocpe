;*******************************************************
    .TEXT ".TWENEMY."
;*******************************************************
	.title "TWEnemy"
	.sbttl "Space Wave Entry"
;*******************************************************		
enemy	lda	mzgame
		ifeq
            ;get the right level
			ldy maztype			;default normal station
			lda isfinal
            ifne
                ldy #04             ;Show Star Castle - Special Wave
            endif
			lda	rpage,Y		;Get page select for this guy
			sta	rompg		;Set proper page
            ;sta currpage    ;Got to save here too!
			tya				;get it back
			asl	a			;double it for words tho
			tax				;put it back in X now
			lda	enrout+1,X
			pha
			lda	enrout,X
			pha
		endif
		rts				;Do Enemy Routine	

enrout	.word enem0-1		;Space Wave 0 - Fishoids
		.word enem1-1		;Space Wave 1 - Fighters
		.word enem2-1		;Space Wave 2 - Space Maze
		.word enem3-1		;Space Wave 3 - Space Fort
        .word enem4-1       ;Final Station - Star Castle

rpage   .db $01  ;Fishoids   - Page 1
        .db $00  ;Fighters   - Not Paged
        .db $04  ;Space Maze - Page 4
        .db $03  ;Space Fort - Page 3
        .dw $03  ;Star Castle- Page 3


;********************************************
	.sbttl "Space Wave 2 - Fighters"
;********************************************
enem1	lda	#05
		sta	hitpts		;Fighters = 500
		lda	nenstr		;Need to start any?
		ifne				;yep
			lda	statyh
			cmp	#03
			ifeq				;Hold Mom at 3,80
				lda	statyl
				ifpl				;Should get us here
					lda	#00
					sta	stbflg		;Stop it here
				endif
			endif
emem12		jsr	getempty		;Find an empty slot
			ifpl				;None Available
				lda	mzgame			;Still in space??
				ifeq				;yes
					bit	lauen			;Okay to launch??
					ifmi				;It's OK!
						lda	frame
						and	#03			;Wait some time
						ifeq				;Okay to start
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
			endif
		endif
		lda	toolong			;Stalling??
		ifmi
			lda	mzgame			;And still space??
			ifeq
				tay					;Y=0
				lda	frame
				and	#03
				ifeq
					lda	frame
					lsr	a
					lsr	a
					and	#$0F				;Dive another one
					cmp	#nmform+1
					ifcc	
						tay
						lda	sobjs2,Y
						and	#$BF				;Drop any hold bits
						ora	#$10				;Kamikaze time!!
						sta	sobjs2,Y
					endif
					ldy	#$80				;Okay to move down this frame
					lda	nenstr				;Any Left??
					ifne					;Wait for all to Launch
						ldy	#00
					endif
				endif
				sty	stbflg			;Will Slowly move down screen
				jsr	skip			;See if ready to skip
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
coreg   jsr	next			;Start Any??
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
						jsr	stcorg			;Restart Correography
						ora	#$40
?cor8					sta	sobjst,X		;Put Back into Formation
						sta	temp3			;May be needed later
						jsr	thisent			;Get this correography entry
						sta	sobdir,X
						bne	?cor16			;And move it!
					endif
?cor7				lda	#00			;To Move Up, No X Motion
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
						bpl	mvenst
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
		;*************** Fall Through  ******************	
		;*****************************************************
			.sbttl "Move Active Enemy Shots"
		;*****************************************************
		;* Moves Active Enemy Shots, then calls the drawshot *
		;* routine. This routine also checks for shots going *
		;* off the bottom.                                   *
		;*****************************************************
mvenst	ldx	#zshot+nmshot-1
		stx	temp9
		begin
			lda	objst,X		;Active??
			ifeq
?mes1			jmp	?mes6
			endif
			bmi	?mes1			;Skip if blowing up too
			; and #$20			;Check to see if this is a star castle fireball
			; ifne
				; ;if so just place, movement is done in ?movefire
				; lda	objxl,X
				; sta	vdata			;Save for Draw and Collision
				; lda	objxh,X
				; sta	vdata+1
				; lda	objyl,X
				; sta	vdata+2
				; lda	objyh,X
				; sta	vdata+3
				; jmp mvst2
			; endif
			lda	velyh,X			;See if still moving up?
			ifmi				;Must get it back to normal
?mes10			tay
				lda	velyl,X
				clc
				adc	#$70			;Gravity!!!!
				sta	velyl,X
				tya
				adc	#00
?mes11			sta	velyh,X
			else				;Else, if +, see if fast enough
				ldy	objst,X		;Look at status for...
				cpy	#$10			;Is this a line?
				ifcc
					cmp	shotspeed			;See if at intended shot speed
					bcc	?mes10				;Keep accelerating then
					lda	shotspeed			;Max out at this speed then
					bne	?mes11
				else
					cpy #$31			;Is this a space station fireball?
					ifeq
						cmp	#40d			;Fireball speed fixed
						bcc	?mes10
						lda	#40d
						bne	?mes11
					else
						;line
						cmp	#12d			;Line speed fixed
						bcc	?mes10
						lda	#12d
						bne	?mes11
					endif
				endif
			endif
			;Now adjust X velocity to target player
			ldy difcty
			lda sptargx,Y
			clc
			adc gamedif		;skew in the game difficulty
			pha				;save it for below
			ldy	#00			;MSB Velocity
			lda	shipxl
			cmp	objxl,X
			lda	shipxh		;See where we are
			sbc	objxh,X		;Which side
			ifcc
				dey				;Prop sign
				pla	
				jsr	neg
			else
				pla
			endif
			jsr	addvel		;Update Velocity
			ldy	#00			;Sign Extend
			lda	velxh,X		;MSB of velocity
			ifmi
				dey				;Prop Sign
			endif
			adc	objxl,X
			sta	objxl,X
			sta	vdata			;Save for Draw and Collision
			tya
			adc	objxh,X
			sta	objxh,X
			sta	vdata+1
			pha				;Save conditions
			ifeq				;In last one Left??
				lda	velxh,X
				clc
				adc	#03
				sta	velxh,X
			else
				cmp	#rtedge-1		;In last on Right?
				ifeq
					lda	velxh,X
					sec
					sbc	#03
					sta	velxh,X
				endif
			endif
			pla			;Recall Conditions
			ifmi			;If this went -, went of left edge
?mes5			lda	#00
				sta	objst,X	    ;Just Kill it!
?mes6			jmp	loop2		;And go on to next one
			endif
			cmp	#rtedge	;Off Right Edge of plane?
			bcs	?mes5		;yep
			
			ldy	#00
			lda	velyh,X
			ifmi	
				dey	
			endif
			adc	objyl,X		;MSB Y velocity
			sta	objyl,X
			sta	vdata+2
			tya				;Prop Sign
			adc	objyh,X		;Shot Speed in Y (prop carry)
			sta	objyh,X
			sta	vdata+3		;Check bottom for player ship collision and off bottom
	
mvst2		lda objyh,X
			cmp	#$0B			;Did we go off the bottom
			ifcs				;Yep, so leave
				lda	#00
				sta	objst,X		;Kill this object
				beq	loop2
			endif
			lda	#shtsz
			sta	temp4			;X in temp4, Y in 4+1
			sta	temp4+1		;Save size of shot for collision
			lda	objst,X		;See if this is a line
			and	#$10			;Line Flag
			ifne				;yep
				lda	#$60
				sta	temp4		;Much Wider
			endif
			jsr	hitship		;See if we hit ship??
			ifmi
				lda	#00
				sta	objst,X		;Kill the shot
				beq	loop2		;And skip draw
			endif
			jsr	dfshot		;Draw a fireball shot
loop2		dec	temp9
			ldx	temp9
			cpx	#zshot		;Last one??
		ccend
exit	rts	


sptargx	.db 6		;Level 1-4
		.db 9		;Level 5-8
		.db 12		;Level 9-12
		.db 17		;Level 13-16
		.db 21		;Level 17-20
		.db 22		;Final Station
		.db 14		;Hidden Levels
		
;********************************************
	.sbttl "Get Empty Slot"
;********************************************
;* Find an empty slot.... Return minus set  *
;* if available, else return zero. If zero, *
;* X points to empty slot                   *
;********************************************		
getempty	
		ldx	#nmform		;Find an empty one
		begin	
			dex
			bmi	?ge10
			ifeq				;First Guy??
				;lda	maznum		;Doing Space Fort??
				lda maztype
				cmp	#spacefort		
				ifeq
					lda	#$80			;Don't use this one
					bmi	?ge10			;And just leave
				endif
			endif
			lda	sobjst,X
		eqend			;Got one!
		lda	#00
?ge10	rts
		
;*************************************************************
;* Tracking Speed of Ships - NOT USED?
;trkspd	.byte $07,$0A,$10,$16,$1E,$26,$2E,$36,$3E,$46,$4E		;Tracking Speed

;************************************************
	.sbttl "Shot Utilities" 
;************************************************
;* Add A to x,velxl and x,velxh                 *
;* If adding a + velocity to a - or a - to a +, *
;* then will add velocity in twice.             *
;************************************************
xvlim	= $30			;X Velocity Limit

addvel	sta	temp1		;Save Velocity	
		sty	temp1+1		;Save MSB
		asl	temp1
		rol	temp1+1
		asl	temp1
		rol	temp1+1
		lda	#00
		sta	temp2			;Guess 1 pass
		lda	velxh,X
		eor	temp1+1			;Same direction already??
		ifmi				;no
			inc	temp2			;Do this twice
		endif
		begin
			lda	temp1
			clc
			adc	velxl,X
			sta	temp3
			lda	temp1+1
			adc	velxh,X
			ifpl
				cmp	#xvlim		;Don't let get too fast
				bcs	?ave10		;Skip if too big
			else
				cmp	#-xvlim	
				bcc	?ave10		;Skip if too big
			endif
			sta	velxh,X
			lda	temp3
			sta	velxl,X
			dec	temp2			;Double add if needed
		miend
?ave10	rts

;**************************************************
	.sbttl "Draw Fire Ball Shot"              
;**************************************************
; Called above       
;**************************************************	
dfshot  ldx temp9
		lda objst,X
		and #$20
		ifne
			lda #$04
		else
			lda	#00				;Default scale facor for fighter shots and red bars
		endif
		jsr	drawshot		;This will position for us
		bmi	exit			;Skip if it did not place
		lda	#($E0+colred2)	;Fixed intensity for now
		ldx	temp9
		ldy	objst,X		    ;Line or Shot??
		cpy	#$10
		ifcc
			eor	#02
		endif
		ldx	#smtb_vpg		;Do a fireball or smart bomb
		jsr	vgadd2
		ldx	temp9
		lda	objst,X		    ;Fighter Shot??
		and	#$30			;Line bit set by Star Fort
		ifeq
?dfb10		lda	frame
			and	#06
			tay
			lda	smtb,Y
			ldx	smtb+1,Y
		else				  
			and	#$20			;Bit set from Star Castle
			ifeq
				;Space Wave 3... Angle Line
				ldy	temp1			;Save Intensity
				;lda	temp1			;Save Intensity
				;tay
				lda	objxl,X		    ;Shot's X LSB
				sec
				sbc	shipxl		    ;Will angle about player
				sta	temp1
				lda	objxh,X
				sbc	shipxh
				sta	temp1+1
				ifcc				;Need to negate??
					jsr	dblneg		;Abs
					ldx	#$64		;And Flip other direction
				else
					ldx	#$60
				endif
				ldy	temp9			;X holds data, Y is set to 0 in vgadd2
				lda	objst,Y		    ;Castle battle?
				and	#$40			;Bar during castle battle
				ifeq
					lda	#($E0+colred2)			;Fort uses Red bar
				else
					lda	#($E0+colyellow)		;Castle uses Yellow bar
				endif
				;lda	#($E0+colyellow)			;Constant Intensity
				jsr	vgadd2		    ;Add this flip
				ldy	#02
				begin
					lsr	temp1+1
					ror	temp1
					dey	
				miend
				lda	temp1
				ldx	#00
				jsr	vgadd2		    ;Position to end of line
				lda	#$80			;Above Y, now do X
				ldx	#00
				jsr	vgadd2		;Always same length
				asl	temp1			;Back twice length
				rol	temp1+1
				jsr	dblneg		;Drawing Y the other way
				lda	temp1+1
				ifeq
					tax				;0 special case
				else_eq
					and	#$1F
					tax
					lda	temp1
				endif
				jsr	vgadd2
				lda	#00
				ldx	#$3F			;X always the same again
			else
				;Star Castle Shot here
				lda #($F0+colflash)
				ldx #$60                ;Draw a fire ball
				jsr vgadd2
				laljsr(sparkb)
				lxhjsr(sparkb)          ;Draw the fire ball 
			endif
		endif
		jmp	vgadd2
		
;***********************************************
	.sbttl "Player Ship Collision Check"
;***********************************************
;* Checks an object against ship to see if it  
;* has hit the ship. Returns - In status as    
;* as setting the explosion flag in the ships  
;* status word.                                
;*                                             
;* Inputs:	vdata(4) = objects X & Y position  
;*		(X) = object's index into obj    
;*		temp4 = Size of incoming object    
;*      mzgame - skip if computer moving   
;*		shipst - skip if already exploding 
;*                                             
;* Uses:	temp1(word),temp2(word)	           
;***********************************************
hitship	lda	shipst		;Ship Exploding??
		ifne
			ifpl				;nope
				lda	mzgame		;Computer Move??
				and	#$10
				ifeq				;skip if yes
					lda	#00
					sta	temp2+1		;2 numbers to add each less than 100
					lda	#shpszy		
					clc
					adc	temp4+1		;Y Size of object
					sta	temp2			;Y Size
					ifcs
						inc	temp2+1
					endif			;temp2 has Y 'collision size'
					lda	shipyl
					sec
					sbc	vdata+2		;Shot's Y LSB
					sta	temp1
					lda	shipyh
					sbc	vdata+3		;Shot's Y MSB
					sta	temp1+1
					bmi	?hs10			;Skip if past us
					lda	temp1
					cmp	temp2
					lda	temp1+1
					sbc	temp2+1		;Did we hit??
					ifcc				;We hit in Y, now check X
						lda	#00
						sta	temp2
						lda	#shpszx		;Ship Size in X
						clc
						adc	temp4		;X Size on collision object
						sta	temp2
						ifcs
							inc	temp2+1
						endif			;Total Size in temp2
						lda	shipxl
						sec
						sbc	vdata		;Object's X LSB
						sta	temp1
						lda	shipxh
						sbc	vdata+1		;Object's X MSB
						sta	temp1+1
						ifmi
							jsr	dblneg		;Abs of Y difference
						endif
						lda	temp1
						cmp	temp2
						lda	temp1+1
						sbc	temp2+1
						ifcc				;BOOM!!!!!
#IF (DEBUG == 0)
							jsr	domzsn
							jsr	blowship
							lda	#$80
							rts
#ENDIF
						endif
					endif
				endif
			endif
		endif
?hs10   lda	#00
		rts
	
;************************************************
	.sbttl "Drop Shot From Enemy"
;************************************************
;* Check to see if time for space ship to shoot *
;*                                              *
;* Input:	(X) = space ship shooting           *
;************************************************		
dropshot	
		lda	frame
		and	#$07
		ifne
			rts
		endif
		lda	sobjyh,X
		cmp	#09			;Don't allow to shoot anymore if this is low
		ifcc
			lda	#$80
			sta	temp5			;Set 'shot from fighter' flag
drop2		jsr	shotlimit
			;---------------------------------------------------------------------
			;------ Drop3: Entry from Star Castle/ Space Fort to Start a Shot ----
			;---------------------------------------------------------------------
drop3		tay				;Get limits on how many
			iny				;Compensate for dey on entry to loop
			begin
				dey
				bit	temp5			;shooting from fighter if negative
				ifpl
					bit scflags
					ifmi
						cpy	#zshot+nmshot-6		;Castle Steals top 4+1
					else
						cpy	#zshot+nmshot-3		;Fort Steals top 2
					endif
				else
					cpy	#zshot			;Last one??
				endif
				ifcc
?ds10				lda	#$80			;Signal no shot
					rts					;None Available
				endif
				lda	objst,Y
			eqend				;Found an open one
			lda	#snd_e2
			jsr	dosound
			lda	sobjs2,X			;Moving??
			and	#$64				;Not moving, floowing, moving to top, skip
			bne	?ds10				;Skip if true
			lda	sobjxl,X			;Start at object
			sta	objxl,Y
			lda	sobjxh,X
			sta	objxh,Y
			lda	sobjyh,X
			sta	objyh,Y
			lda	sobjyl,X			
			sta	objyl,Y			;Position shot at ship
			;Shot must start out in direction fighter is pointing
			txa	
			pha					;Save this index, we will need it later
			lda	sobdir,X
			and	#$0F
			tax					;This guy's direction
			ifmi
				ldx	#08				;If 0, use straight down
			endif
			lda	cortlx-1,X		;Shot direction
			sta	velxh,Y
			lda	cortly-1,X
			asl	a				;Speed * 2
			sta	velyh,Y
			;If called by Starcastle, it will change velocities back later
			pla	
			tax					;Restore X (guy shooting)
			lda	#01
			sta	objst,Y			;Activate it!!
		endif
		rts
		
;*************************************************
	.sbttl "Shot Limit"
;*************************************************
;* Returns in A (zshot=nmshot) allowed this wave *
;* To use in most loops, this value must be      *
;* knocked down by 1.                            *
;*************************************************	
shotlimit	
		ldy	difcty				;which maze difcty are we at (every 4 levels)
		lda shtlmit,y
		clc
		adc gamedif				;Add in game difficulty (0-2)
		cmp	#nmshot+zshot-3		;Recall we may steal 3 max
		ifcs
			lda	#nmshot+zshot-3		;Shot high limit
		endif
		rts

shtlmit	.db zshot+0		;Level 1-4 - 3 Shots on Medum Difficulty
		.db zshot+1		;Level 5-8
		.db zshot+1		;Level 9-12
		.db zshot+2		;Level 13-16
		.db zshot+2		;Level 17-20
		.db zshot+3		;Final Station - +6 shots MAX
		.db zshot+2		;Hidden Levels
;******************************************
	.sbttl "Follow the Leader Routine"
;******************************************
followme	
		lda	temp3		;This is who we follow
		and	#$0F
		tay					;Index to other guy's position
		lda	sobjst,Y		;Is this guy blowing up?
		ifmi
?fm1			lda	#$10
			sta	sobjs2,X		;Turn Kamakazi
			rts
		else
			and	#$20			;Return bit
			bne	?fm1			;If yes, stop following
		endif
		sty	temp5			;See if following self
		cpx	temp5			;Following Self?
		beq	?fm1			;Shit!! How did that happen??
		lda	sobjxl,Y
		sta	temp4
		lda	sobjxh,Y		;Copy this away so we can reuse Y
		sta	temp4+1
		lda	sobjyl,Y
		sta	temp5
		lda	sobjyh,Y
		sta	temp5+1		;Now have copy of leaders position
		lda	sobdir,Y		;Leaders Direction
		and	#$0F
		tay				;Will use this as index to 'pole' table
		lda	sobjs2,X		;See if a Left or Right guy
		and	#$08			;Left/Right bit
		ifne
			tya
			clc
			adc	#$10			;Use upper half of table
			tay
		endif	
		lda	temp4			;Update X
		clc	
		adc	rodxl,Y
		sta	temp6
		lda	temp4+1
		adc	rodxh,Y
		sta	temp6+1		;New X Position Target
		lda	temp5
		clc	
		adc	rodyl,Y
		sta	temp7
		lda	temp5+1
		adc	rodyh,Y
		sta	temp7+1		;New Y Position Target
		tya	
		sta	sobdir,X		;And set picture
		lda	#$28
		sta	temp8
		sta	temp8+1		;Target Speeds
		jmp	approach
		
;*******************************************
;* Table of rod lengths for each direction 
;* Bytes 0-15 are for left side, 16-31 for 
;* right. Recall 0 is not used so a dummy  
;* byte will hold it's place.              
;*******************************************
z 	=  7		;Maximum is 7

rodxl	.byte $00,$d2,$E0,$D2,$9A,$54,$00,$AC
		.byte $66,$2E,$20,$2E,$66,$AC,$00,$54
		.byte $00,$AC,$00,$54,$9A,$D2,$E0,$D2
		.byte $9A,$54,$00,$AC,$66,$2E,$20,$2E
		
rodxh	.byte $00,$00,$00,$00,$00,$00,$00,$FF
		.byte $FF,$FF,$FF,$FF,$FF,$FF,$00,$00
		.byte $00,$FF,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$FF,$FF,$FF,$FF,$FF
		
rodyl	.byte $00,$2A,$00,$D6,$B3,$97,$90,$97
		.byte $B3,$D6,$00,$2A,$4D,$69,$70,$69
		.byte $00,$69,$70,$69,$4D,$2A,$00,$D6
		.byte $B3,$97,$90,$97,$B3,$D6,$00,$2A
		
rodyh	.byte $00,$00,$00,$FF,$FF,$FF,$FF,$FF
		.byte $FF,$FF,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$FF
		.byte $FF,$FF,$FF,$FF,$FF,$FF,$00,$00

;*************************************************
	.sbttl "Kamakazi Control"
;*************************************************
;* Called if an object is to kamakazi the player 
;*                                               
;* Inputs: 	Bit D4=1 In sobjs2                   
;*		    D3=1 If only allowed clockwise   
;*              Else 0= only allowed c-clockwise 
;*************************************************
kama	lda	frame
		and	#$03
		ifeq
?kc10		lda	shipxh
			sec	
			sbc	sobjxh,X
			clc	
			adc	#07
			tay	
			lda	sobdir,X		;Where we are pointing
			and	#$0F
			sec	
			sbc	tardir,Y		;Want to point that way?
			ifne
				ifpl
					dec	sobdir,X
				else
					inc	sobdir,X		;Point about
				endif
			endif
		endif
		lda	sobdir,X
		and	#$0F			;Don't allow to point 0
		beq	?kc10			;So bump it again
		rts	
		
tardir	.byte $04,$05,$05,$06,$06,$06,$06
		.byte $08,$0A,$0A,$0A,$0B,$0B,$0C
		
;********************************************************
	.sbttl "Line Up for Start of Coreg"
;********************************************************
;* Called from 'coreg' with X pointing at proper object 
;********************************************************
txvel	= $30
tyvel   = $30

setup	lda	f_targxl,X
		sta	temp6
		lda	f_targxh,X
		sta	temp6+1		;Put in temp6 for target routine
		lda	f_targyl,X
		sta	temp7
		lda	f_targyh,X
		sta	temp7+1		;X and Y position current
		lda	#txvel		
		sta	temp8			;Closing velocity X
		lda	#tyvel
		sta	temp8+1		;Closing velocity Y
		jsr	approach
		ifmi				;We are there
			lda	sobjst,X
			and	#$BF			;Drop Setup Bit
			sta	sobjst,X
			lda	sobjs2,X
			ora	#$40			;Turn on hold in pattern
			sta	sobjs2,X
			ldy	sobjxh,X		;Get X Position
			lda	#$10
			sta	sobdir,X		;Face Down
			dec	wtcnt
		endif
		rts				;Done
		
f_targxl		.byte $00,$00,$00,$00,$00,$00,$80,$80,$80,$00,$00,$80,$80
f_targxh		.byte $02,$03,$04,$05,$06,$07,$03,$04,$05,$04,$05,$04,$04
f_targyl		.byte $80,$80,$80,$80,$80,$80,$00,$00,$00,$80,$80,$00,$80
f_targyh		.byte $02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$04,$04

;*******************************************************
	.sbttl "Approach Target Routine"
;*******************************************************
;* Will approach the target given in temp6(X) and      
;* temp7(Y) (2 bytes each) at velocities given in      
;* temp8(X) and temp8+1(Y). If the difference to target
;* is less than the velocity, the object is simply     
;* placed at the target position.                      
;*                                                     
;* Uses:  temp1,temp2,temp6,temp7,temp8                
;*                                                     
;* Returns minus if at target, else +                  
;*******************************************************
approach	ldy	#00
		sec	
		lda	sobjxl,X		;Distance to target
		sbc	temp6
		sta	temp1
		lda	sobjxh,X
		sbc	temp6+1
		sta	temp1+1
		pha				;Save the 'real' distance
		ifmi				;To the left??
			jsr	dblneg
		endif
		ifeq				;MSB is close
			lsr	temp8			;Use half velocity
			lda	temp1
			cmp	temp8			;Less than velocity to target??
			ifcc				;If yes, just put at target
				pla			;Throw away pha
				lda	temp6
				sta	sobjxl,X
				lda	temp6+1		;Just put at target
				sta	sobjxh,X
				ldy	#$80
				bne	?ap10			;Say at target
			endif
			cmp	#$80
			ifcc				;Really close
				lsr	temp8
			endif
		endif			;Not Close
		pla				;Recall 'real' A
		ifpl
			dey
			lda	#00
			sec
			sbc	temp8			;Velocity is minus here
		else
			lda	temp8
		endif
		clc	
		adc	sobjxl,X
		sta	sobjxl,X
		tya	
		adc	sobjxh,X
		sta	sobjxh,X		;Get to X target
		iny				;Insure Y is +
?ap10		sty	temp2			;Store signal here
		ldy	#00			;Guess Y not at target
		sec	
		lda	sobjyl,X		;Distance to target
		sbc	temp7
		sta	temp1
		lda	sobjyh,X
		sbc	temp7+1
		sta	temp1+1
		pha				;Save the 'real' distance
		ifmi				;To the left?
			jsr	dblneg
		endif
		ifeq				;MSB is close
			lsr	temp8+1
			lda	temp1
			cmp	temp8+1		;Less than velocity to target??
			ifcc				;If yes, just put at target
				pla				;Throw away pha	
				lda	temp7
				sta	sobjyl,X
				lda	temp7+1		;Just put at target
				sta	sobjyh,X
				ldy	#$80
				bne	?ap20			;Say at target
			endif
			cmp	#$80
			ifcc
				lsr	temp8
			endif
		endif			;Not Close
		pla				;Recall 'real' A
		ifpl
			dey
			lda	#00
			sec
			sbc	temp8+1		;Get minus velocity
		else
			lda	temp8+1
		endif
		clc	
		adc	sobjyl,X
		sta	sobjyl,X
		tya	
		adc	sobjyh,X
		sta	sobjyh,X		;Get X target
		iny				;Insure Y is +
?ap20		tya	
		and	temp2			;Total Signal
		rts
		
;*************************************************
	.sbttl "Past Bottom Check"
;*************************************************	
pastck	ifpl	
			cmp	#$0B		;Hit the bottom??
			ifcs			;yep, at bottom
				jsr	stcorg
				ora	#$20		;Force back up bit
				sta	sobjst,X
				lda	sobjs2,X
				and	#$C0		;Drop all but set-up and bonus bits
				sta	sobjs2,X
				lda scflags		;We don't do this 
				ifeq
					sed			;*********** Caution ***************	
					sec	
					lda	bonusa	;Dec Bonus
					sbc	#04		;-400 pts
					beq	?pb10		;Count this one too
					ifmi			;Don't let wrap
?pb10					lda	sobjs2,X		
						ora	#$80		;Set once past bit
						sta	sobjs2,X
						lda	#00
					endif
					sta	bonusa
					cld			;***********************************
				endif
				lda	#-1
				sta	sobjxl,X	;Guess on right (4.FF)
				ldy	#rtedge-1	;Guess right side
				lda	sobjxh,X	;Which side are we on??
				cmp	#center
				ifcc
					ldy	#00		;Nope, put on the left edge
				endif
				tya	
				sta	sobjxh,X	;Force to Edge
				ifeq			;Is on the left, so store LSB to 0 too
					sta	sobjxl,X
				endif
				ldy	#$11		;Guess off right edge
				lda	sobjxh,X
				ifeq			;It was left
					ldy	#$1F		;Point for left
				endif
				tya	
				sta	sobdir,X	;Point Correctly
				lda	#$0A		;At bottom of screen
				sta	sobjyh,X
				lda	#-1
				sta	sobjyl,X
			endif
		endif
		rts
		
;********************************************
	.sbttl "Start (restart) Correography"
;********************************************	
stcorg	lda	sobjst,X
		ora	#$10			;Make sure it is on
		and	#$1F			;Drop all but on and coreg number
		sta	sobjst,X
		pha	
		and	#$0F			;Coreg
		tay				    ;Save copy
		lda	coegts,Y		;Table Pointers
		sta	sobcog,X		;Start this coreg
		;Want to return with A=current sobjst
		pla	
		rts	
		


;**********************************************
	.sbttl "Skip Rest of Space?"
;**********************************************
;* This routine decides if it is time to skip 
;* the rest of the fighter wave and go on to  
;* the docking wave.                          
;* Necessary Conditions: All active fighters  
;* must be either returning to the top or     
;* just getting ready to return, the base star
;* must be halfway down the screen.           
;**********************************************	
skip	lda	statyh
		bpl	?s10			;Skip if off screen
		cmp	#07
		bcs	?s10			;Still too high on screen
		ldx	#nmform-1
		ldy	#00
		begin
			lda	sobjst,X
			ifne
				ifpl				;Active and not exploding??
					and	#$20			;Waiting to go up??
					ifeq				;Here is one not ready
?s10						rts				;Not all ready
					endif
				endif
			endif
			dex
		miend
		lda	lasst			;Laser shot out??
		bne	?s10			;Then wait
		jmp	noneleft		;Force to next level

;************************************************
	.sbttl "One Less Enemy to Deal With"
;************************************************
oneless	dec	nenemy		;One less to fight
		ifeq				;We are done
			lda	nenstr		;Any left to start??
			ora	mzgame		;During space that is
			ifeq				;nope
				lda scflags			;If Star Castle is active, then allow zero ships to not trigger end
				ifeq
noneleft	    	jsr	dostop		;Stop all sounds
					;clear all space objects to make sure that station explosion is fine
					lda #00
					ldx #nmform-1
					begin
						sta sobjst,X        ;Turn all off anyway!!!
						dex
					miend
					lda	#$10
					sta	mzgame		;Next Game
					lda	#$40
					sta	lauen	    ;Skip Launch next time
					lda	#$80
					sta	stbflg		;Set Flag for down screen
					lda	bonusa		;Xfer bonus to display buffer
					sta	scbdis+1
					lda	#00
					sta	scbdis
					lda	#$0F
					sta	scbdis+3
					lda	#04
					sta	spcspd		;Speed of stars
					lda	statyh		;Already moving??
					ifeq				;nope, can restart
						lda	#$FF
						sta	statst		;Turn on the station
					endif
					lda	#$0B
					sta	shipyh
					lda	#$B8
					sta	shipyl		;Correct position for 3rd person display
					lda	manstat		;We alive??
					ifne
						ifpl				;yep!
							lda	maznum
							lsr	A			;On 1 and 3 of 0,1,2,3
							ifcs
								lda	#snd_mys
								jmp	dosound
							endif
						endif
					endif
				endif
			endif
		endif
		rts
		
;**************************************************
	.sbttl "Next Entry"
;**************************************************
;* Gets next entry in coreg table for this guy.   *
;*                                                *
;* Entry: (X) = Object Index                      *
;**************************************************	
nextent	inc	sobcog,X		;Next Entry
thisent	ldy	sobcog,X		;Entry to get current entry index
?ne10	lda	coegrt,Y		;Tabel of moves
		cmp	#$80			;End of lists??
		ifeq
			lda	sobjst,X		;Which pattern??
			and	#03
			tay	
			lda	coegts,Y		;Start of this coreg
			sta	sobcog,X		;Reset Entry
			tay				;And return with...
			lda	coegrt,Y		;A = new dance
		endif
		tay	
		lda	sobjst,X		;Check for which edge
		and	#$0F			;Top 4 are left
		cmp	#04			;Set carry for test
		tya				;Recall new direction and time
		ifcs				;This one of top 4
			sta	temp2			;Save this
			and	#$0F			;Current Entry - Time
			tay				;Get index to lfttbl
			lda	lfttbl,Y		;Get Left direction
			eor	temp2
			and	#$0F
			eor	temp2			;Replace Time
		endif
		rts	
		
;***************************************************
	.sbttl "Copy Position"
;***************************************************
;* Copies the enemy fighters quads into vdata for  *
;* the display routine. Called when the fighter is *
;* not moving (for whatever reason).               *
;***************************************************
copypos	lda	sobjxl,X
		sta	vdata
		lda	sobjxh,X
		sta	vdata+1
		lda	sobjyl,X
		sta	vdata+2
		lda	sobjyh,X
		sta	vdata+3
		rts	
		
;*****************************************************
	.sbttl "Start from Rack"
;*****************************************************
;* This routine is used to start fighters from their *
;* holding positions in the 'rack'.                  *
;*                                                   *
;* Uses:	temp1,temp2,A,X,Y,nxtptr,nxtdly          *
;* Inputs:	frame                                    *
;*****************************************************
next	jsr	snake			;Continue Snake if needed
		bit	toolong			;Stalling??
		bmi	?sr5			;No Check if stalling
		ldy	#00				;Count moving ones
		ldx	#nmform-1
		begin
			lda	sobjst,X		;Don't count active or blowing up
			ifne
				ifpl
					lda	sobjs2,X		;Holding in place
					and	#$40
					ifeq
						iny
					endif
				endif
			endif
			dex
		miend
		;Now Y tells us how many are in space
		cpy	#07
		ifcc				;Hold at 6 or 7
?sr5		bit	nxtskp		;Did the last one start??
			bmi	next2			;If no, force start
			lda	frame
			and	#01
			ifeq				;Time for next check
				dec	nxtdly		;Time for next start??
				ifmi				;yep
next2				lda	#00
					sta	nxtskp		;Guess we will start something
					ldx	nxtptr
					lda	nxttbl,X		;Get next table entry
					ifeq
						tax
						inx
						stx	nxtptr		;Restart with start of table
						lda	nxttbl,X		;Need to get the entry now
					endif
					inc	nxtptr		;Bump pointer
					pha
					and	#$0F			;Get who
					tax				;Save for routine
					pla				;Get back what
					and	#$F0			;Only top nibble
					lsr	A
					lsr	A
					lsr	A
					tay				;Do Case
					lda	nxtcse+1,Y		;Get Case
					pha
					lda	nxtcse,Y
					pha
					ldy	nxtptr		;So routine will have this too
					lda	nxttbl,Y		;And this too!
				endif
			endif
		endif
		rts				;Do Case!	


nxtcse	.word cse0-1
		.word cse1-1
		.word cse2-1
		.word cse3-1
		
nxexit	inc	nxtptr		;Look at next entry	
nxexit2	ldx	nxtptr
		inc	nxtptr
		lda	nxttbl,X		;Get Next Entry
		ifeq				;Get Next Entry and start right now
			bne	next2			;****** Always ******
		endif
		;If not X, then it's a delay till next time
		sta	nxtdly
		rts
		
;*********************************************
	.sbttl "Cases"
;*********************************************
;* Entry Y = nxttbl pointer to current entry *
;* 	   X = pointer to object to be started *
;*       A = nxttbl entry value              *
;*********************************************
;* Case 0 - Start Correography               *
;*********************************************	
cse0	jsr	movchk		;Already moving??
		ifne				;no, this is okay
			jsr	cse0r			;The routine is a subroutine so other 
								;parts of these cases may use it
		endif
		jsr	stchk			;See if started??
		jmp	nxexit		;Do Exit!
		
cse0r	jsr	putinc		;Put coreg bits in lower nibble
		lda	#04			;Turn on go to top bit
		jmp	putin			;Put in these bits and return

;*********************************
;* Case 1 - Start a Kamakazi     *
;*********************************
cse1	jsr	movchk		;Already Moving??
		ifne				;This okay to start
			lda	#$10			;Kamakazi Flag
			jsr	putin			;Turn on correct bits
		endif
		jsr	stchk			;See if started
		jmp	nxexit2		;And Leave..

;***************************		
;* Case 2 - Start a Snake  *
;***************************
cse2	bit	snakef		;Already trying to start??
		ifpl				;nope
			sta	snakec		;Save snake count and coreg
			jsr	getrand
			and	#04
			eor	snakec
			sta	snakec
			lda	#$80
			sta	snakef		;Turn of flag
			stx	snakex
		endif
		jmp	nxexit		;End of exit will start first of snake

;***************************************************
;* Utility to OR in A to sobjs2 then drop hold bit *
;***************************************************
putin	ora	sobjs2,X		;Put in new bits
		and	#$BF			;Drop hold bit
		sta	sobjs2,X
		rts	

;************************************************************** 
;* Utility to put in coreg in A into bottom nibble of sobjst  *
;**************************************************************
putinc	eor	sobjst,X		;Replace coreg
		and	#$0F			;Replace bottom nibble which is coreg bits
		eor	sobjst,X
		bmi	?pi10			;If it was blowing up, skip it
		bit	m10			;See if previously active
		ifne				;It was
			sta	sobjst,X		;Replace (but do not turn on if dead already)
		endif
?pi10	rts	

m10		.byte $10

;*********************************************
;* Utility to see if we started anybody.     *
;* Assumes X points to object just trying to *
;* start.                                    *
;*********************************************
stchk	lda sobjst,X		;Tried to start this guy
		bmi	?stc10			;If blowing up, he didn't start
		and	#$10			;Alive??
		ifeq				;No, he didn't start
?stc10		lda	#$80			;Set bad start
			sta	nxtskp
		endif
		rts
		
;***************************************
;* Check to see if already moving and  *
;* don't restart                       *
;***************************************	
movchk	lda	sobjs2,X		;Check hold bit
		and	#$40			;If ne on return, holding
		rts	
		
;***************************************
;* Case 3 - Start Fighter Squad        *
;***************************************
cse3	pha				;Save what coreg
		txa				;A is who we want to follow
		sta	temp1			;Save who to follow for routine below
		inc	nxtptr		;Will need the next entry
		iny				;And the copy of it
		jsr	movchk		;Leader waiting??
		ifeq				;No, already moving (maybe)
			pla			;Throw away this
		else				;Else, start him
			pla				;Start leader
			and	#$0F			;Pass coreg to routine 0
			jsr	cse0r			;Start the first guy as normal
			lda	sobjst,X		;See if active
			and	#$10			;Skip if dead
			ifne	
				lda	nxttbl,Y		;Who L and who R
				pha 				;Save copy
				and	#$0F			;Who on the right??
				tax				;Who we will change
				lda	temp1			;Who we will follow
				jsr	putinc		;Put this in correography as who to follow
				lda	#$20
				jsr	putin			;Add follow the leader bits
				pla	
				lsr	A
				lsr	A
				lsr	A
				lsr	A			;Now get who R
				tax				;Set current to this guy
				lda	temp1			;Who we will follow
				jsr	putinc
				lda	#$28
				jsr	putin
			endif
		endif
		jmp	nxexit		;And Leave
		
;************************************
	.sbttl "Snake Routine"
;************************************
snake	lda	snakef		;Snake trying to start??
		ifmi				;yes
			lda	frame
			and	#07			;Time??
			ifeq				;yes
				ldx	snakex		;Who we will start this time
				jsr	movchk		;Make sure he is waiting
				ifne
					lda	snakec		;Get coreg
					and	#$0F
					jsr	cse0r			;Start this guy
				endif
				inc	snakex		;Point to possible next
				lda	snakec
				sec
				sbc	#$10			;Count = Count -1
				sta	snakec
				ifmi
					lda	#00
					sta	snakef		;Done
				endif
			endif
		endif
		rts	
		
;****************************************
	.sbttl "Start From Rack Table"
;****************************************
nxttbl	.byte $01,$3C,$03,$AB		;Start Squad, Coreg 3, #0C Lead, A & B Follow
		.byte $07,$20,$52			;Start Snake using #0, 6 Guys, Coreg 2
		.byte $07,$07,$00			;Start Simple, #7, Coreg 0
		.byte $07,$08,$01			;Start Simple, #8, Coreg 1
		.byte $07,$1B			    ;Kamakazi #B (if start above)
		.byte $07,$33,$01,$24		;Squad, Coreg 1,2&4 Follow 3
		.byte $03,$19			    ;Kamakazi #7
		.byte $03,$1A			    ;Kamakazi #A
		.byte $0F,$27,$23			;Start Snake, #7, 3 Guys, Coreg 3
		.byte $03,$1B
		.byte $00,$17
		.byte $00,$16
		.byte $00,$00
		
;****************************************
	.sbttl "Correography Table"       
;****************************************
; Simple Right Sweep
coegrt
___1	.byte $50,$4B,$3A,$49,$98,$87,$C6,$F5,$80

; Right Sweep with loop back
___2	.byte $50,$7A,$79,$88,$57,$46,$35,$24	;S Loop
		.byte $13,$12,$11,$1F,$1E,$1D,$1E
		.byte $1F,$11,$12,$13,$14,$15,$16
		.byte $17,$18,$F9,$80
		
; Switch Back(Zig/Zag)
___3	.byte $50,$5B,$3A,$39,$28,$17,$26
		.byte $35,$94,$15,$16,$17,$18,$19
		.byte $1A,$1B,$BC,$1B,$1A,$19,$18
		.byte $17,$16,$15,$74,$15,$16,$17
		.byte $18,$19,$FA,$80
		
; Loop De Loop
___4	.byte $A9,$18,$17,$16,$15,$14,$13
		.byte $12,$11,$1F,$1E,$1D,$1C,$1B
		.byte $1A,$19,$18,$A7,$18,$19,$1A
		.byte $1B,$1C,$1D,$1E,$1F,$11,$12
		.byte $13,$14,$15,$16,$17,$18,$80
		
; Table of offsets to get a coreography
coegts	.byte ___1-coegrt
		.byte ___2-coegrt
		.byte ___3-coegrt
		.byte ___4-coegrt
		.byte ___1-coegrt
		.byte ___2-coegrt
		.byte ___3-coegrt
		.byte ___4-coegrt

; Table of replacment values to change from right to left side
lfttbl	.byte $00,$0F,$0E,$0D,$0C,$0B,$0A,$09,$08,$07,$06,$05,$04,$03,$02,$01

;******************************************
	.sbttl "Table of Delta X and Y"
;******************************************
;* Table of Delta X and Delta Y for a given angle.                           
;******************************************
cortlx	.byte $F4,$EA,$E2,$E0,$E2,$EA,$F4,$00,$0C,$16,$1E,$20,$1E,$16,$0C
cortly	.byte $F4,$F8,$FD,$03,$09,$0E,$12,$12 ;last one was $13 - try to fix odd fast shot
		.byte $12,$0E,$09,$03,$FD,$F8,$F4

;******************************************
	.sbttl "Draw Enemy"
;******************************************
;* Draw an Enemy Fighter                  *
;*                                        *
;* Inputs: vdata(4)= position             *
;******************************************
drawen	lda	#00
		sta	scalef			;fract
		lda	#$FE
		sta	scalef+1		;Binary Offset in Size
drawen2	;Entry from blowit
		lda	temp3			;Blowing Space Station from tw_ship?
		ifmi				;Yeah, so get larger
			asl	a
			asl	a
			sta	scalef
			ifcs
				inc	scalef+1
			endif
		endif
		lda	mzgame
		bmi	?de10			;Special for inside maze blow up
		cmp	#01
		ifeq
?de10		jsr	cor3p
			lda	#00			;Set minus
		else
			jsr	posvec		;Position and set scale of object
		endif
		ifpl				;Only do this if on screen
			ldy	temp9
			lda	sobjst,Y
			and	#03
			tax
			lda	enmcol,X		;Color is coreg
			ldx	#enm_vpg	    ;Stat, page select
			bit	temp3			;Exploding?
			ifmi
				lda	temp3			;Dim it out
				cmp	#$D0			;Bright then dim
				ifcc
					lda	#$FA			;Bright Red
				else
					eor	#$FF
					and	#$F0
					clc	
					adc	#$62
				endif
				ldx	#fexps_vpg+sparkle+xflip		;Stat, PageSel, Sparkle, Xfliip
			else
				ifvs				;Setting up?
					lda	#$E4			;Move in in Red
				endif
			endif
			pha				    ;Save color
			ldy	temp9			;Recall X
			lda	sobdir,Y		;Direction
			and	#$0F			;Drop timer
			cmp	#09
			ifcs				;Will need to flip these
				pha
				txa				;Need to flip??
				ora	#04			;Turn it on
				tax
				pla
			endif
			tay
			stx	temp1+1		;Save Xflip info
			lda	pictabl,Y	;Get the picture??
			sta	temp1		;Hold this
			pla				;Recall Color
			sta	temp2		;Save Color
			jsr	vgadd2		;Add Stat
			lda	temp3		;Blowing up??
			ifmi			;yep
				and	#$F0
				lsr	A
				lsr	A
				lsr	A
				sec
				sbc	#$10		;This number goes from 8 to F
				pha				;Save Y index
				jsr	addpic
				lda	temp1+1		;Do other half of pic
				eor	#xflip		;Flip other way
				tax
				lda	temp2
				jsr	vgadd2		;Flip and re-add color
				pla
				jsr	addpic
			else
				ldy	temp1
				lda	enemys,Y
				ldx	enemys+1,Y
				jmp	vgadd2		;Output picture
			endif
		endif
		rts
				
addpic	tay	
		lda	fexps,Y
		ldx	fexps+1,Y		;Get Figher Pic
		jmp	vgadd2
		
pictabl	.byte $0E,$00,$02,$04,$06,$08,$0A,$0C,$0E,$0C,$0A,$08,$06,$04,$02,$00

enmcol	.byte $F1,$F2,$F7,$F6

;***********************************************
	.sbttl "Fighter to Shot Collision"
;***********************************************
;* X=Object Index, will check against all shots*
;* It is assumed that vdata(4) has position.   *
;*                                             *
;* Entry hitwbs for web spinners               *
;*                                             *
;* Uses: temp1(word),temp4(word),temp6(word)   *
;*       5 bytes stack                         *
;***********************************************
shtchk	lda	#enszx		;Size of Fighter
		sta	temp4
		lda	#enszy		;Y Size
shtchk2	clc	
		adc	#04			;Shot needs a Y Size
		sta	temp4+1
		;Temp4 = Size of Object
hitwebs	lda	sobjst,X	;Make sure not exploding
		ifmi
?fs1		rts
		endif
		beq	?fs1			;Skip if dead too!
		ldy	#nmsshots-1
		begin
			lda	shotst,Y		;Shot Active
			ifne				;yep
				ifpl
					lda	vdata			;Stored XL
					sec
					sbc	shotxl,Y
					sta	temp1
					lda	vdata+1
					sbc	shotxh,Y
					sta	temp1+1
					ifmi
						jsr	dblneg		;Positive
					endif
					ifeq				;MSB is 0
						sec
						lda	temp1
						sbc	temp4			;X Difference to Size
						ifcc
							lda	shotyl,Y
							sec
							sbc	vdata+2
							sta	temp1
							lda	shotyh,Y
							sbc	vdata+3
							sta	temp1+1
							ifmi				;We are passed
								cmp	#$FF			;Skip if way passed
								ifeq				;Just passed
									lda	temp1			;See if we passed through
									sec
									sbc	#shtspd
									sta	temp6
									lda	temp1+1
									sbc	#00
									sta	temp6+1		;Old Position
									sec	
									lda	vdata+2
									sbc	temp6
									lda	vdata+3
									sbc	temp6+1		;If just passed, the will go +
									bmi	?fs5			;Skip this, already passed
									lda	vdata+2
									sta	shotyl,Y
									lda	vdata+3
									sta	shotyh,Y		;Put shot at target
									lda	#00
									sta	temp1			;Fake to say on target
								endif
							endif
							ifeq				;MSB must be 0
								sec
								lda	temp1
								sbc	temp4+1		;Size compare Y
								ifcc
									lda	#$C0
									sta	shotst,Y		;Turn off shot
									jsr	killit
								endif
							endif
						endif
					endif
				endif
			endif
?fs5		dey
		miend
;**********************************************
;* Now check object to player ship collision  *
;**********************************************	
		lda	shipst		;Only if ship active
		ifmi				;Blowing Up?
?fs10		rts
		endif
		beq	?fs10			;Skip not active
		;vdata still has this ships position
		lda	#enszy		;Y Size
		sta	temp4+1		;Recalculate - shell size above
		jsr	hitship
		ifmi				;We hit it
			jmp	killit		;Do what is needed!!
		endif
		rts
		
;**********************************************
	.sbttl "Destroy Enemy Fighter"
;**********************************************	
killit	lda	sobjst,X		;Is this one in staging??
		and	#$40
		ifne				;Must count it out then
			lda	wtcnt			;Don't let past 0
			ifne
				dec	wtcnt			;One less to wait
			endif
		endif
		lda	#$80
		;ldy	maznum		
		ldy maztype			;Make spinners wants a 4 here if hit
		cpy	#spacemaze
		ifeq
			lda	#05
		endif
		sta	sobjst,X		;And the ship we hit
		jsr	domzsn
		txa	
		pha				;Save X
		lda	hitpts		;Points
		jsr	bpont2
		pla	
		tax	
		rts	

;Make a sound for ship hits		
domzsn	;lda	maznum
		lda maztype
		ifne
			cmp	#spacemaze
			ifeq
				lda	#snd_f2
			else
				lda	rands
				and	#01
				ifeq
					lda	#snd_e3
				else
					lda	#snd_e4
				endif
			endif
			jsr	dosound
		endif
		rts
			
mror	begin
			cmp	#$80
			ror	A
			ror	temp1
			dey
		miend
		rts	
		
		
.export getempty,nmform,skip,next,oneless,copypos,setup,stcorg,thisent
.export followme,kama,cortlx,cortly,pastck,nextent,dropshot,shtchk,drawen