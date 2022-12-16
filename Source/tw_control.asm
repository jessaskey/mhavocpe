
;***************************************************
    .sbttl "Game Sequence Control"
;***************************************************
;* This controls all the different states and state
;* transitions in the game 
;***************************************************
;* Things that must happen before seqence can adv.
;* Game must be Playing : gamest = minus
;* Player must be alive : manstat = Positive but not zero
;* If player is dead (manstat = 0), score buffer must be empty : scrbuf,scrbuf+1 == 0
                                
seqcon  bit gamest          ;Playing??
        bpl ?psc5           ;Skip this Stuff
        lda manstat         ;Check Player Status
        beq ?psc80          ;He's Dead Jim!
        bpl ?psc5
        ;Dying here
        jsr ?clrjump        ;Clear Jump Status
        inc manstat         ;Continue to Kill Him
        bne ?psc5           ;Just go to scont if not yet dead
?psc80  lda scrbuf
        ora scrbuf+1        ;Wait till all score is added in
        ifne
?psc5       ;skip a bunch of stuff until score is done
			jmp ?scont          ;Wait till all score is added in
        endif
		;Where when all score is trued up
        lda lives
        ora lives+1         ;Anybody Living?? (both player check)
        ifeq
?psc10      lda #00
            sta gamest      ;Game Over
            beq ?psc5       ;Always to scont!!!!!
        endif
        lda frame
        and #$3F            ;Wait for an even frame time period
        bne ?psc5 
        ;Lost a life if here!!
        lda #$80
        sta diedyet     ;Indicate he died
; #IF ENABLE_ADAPTIVE_DIFFICULTY != 0
        ; inc incded
; #ENDIF
        ldx player
        lda lives,X     ;Don't go past 0 (Failsafe)
        ifne           
            jsr ?playertr    ;New subroutine to do all of this stuff
        endif
        lda lives
        ora lives+1     ;Game is fully over when both players have zero lives
        ifeq                ;End of Game
            ;Both players are over, send some stats
            jsr sendgstat
            jsr sendwstat
            ;Game Just over... check warp level
			sta	rampg			;Make sure RAM set
			ldx	wrplvl			;Get Player 1's level
			lda	twopl			;2 Player Game?
			ifne				;Yes
				lda	#01			;Page 1 for Player 2
				sta	rampg
				cpx	wrplvl		;See who greater
				ifcc				;Player 2 was greater
					ldx	wrplvl		;Get Player 2, it was greater
				endif
			endif
            ;X = Greater of the 2 players (or Player 1's if only 1 Player Game)
            ;We don't know what page where are currently at, so set and store
			lda	#01
			sta	rampg
			stx	wrplvl
			lda	player		;Restore to Correct Page
			sta	rampg
			stx	wrplvl		;Save both
			;now get correct current level, tesser levels go back to original level
			lda tokretr
			ifmi
				and #$7F
			else 
				lda dif4mz       ;So Back up one level
			endif
            ; sec
            ; sbc #01
            ; and #03
            ; sta addmn
            ; ifcc            ;Dropped a Difficulty Level
                ; dec adddif      ;Down 1, better not go -
                ; ifmi                ;Oooops!
                    ; lda #00         ;Back to the start sucker!
                    ; sta adddif      ;Stick at 0
                    ; sta addmn
                ; endif
            ; endif
            ;sta add4mz      ;If this is 2 or more, stick at 2,00
            cmp #20d         ;Max add-a-coin level is 20(dif=1, maz=3)
            ifcs
                lda #20d
				;sta add4mz
            endif
            sta add4mz
            ;ora adddif      ;Skip Message??
            ifne      ;Skip if wave 0, level 0
				lda demo
				ifne
					lda #0				;skip in demo
				else
					lda #$10            ;Select-a-level when not in demo
				endif
				sta addtim
            endif
            jmp ?psc10          ;And get out of here
        endif
        jsr dostop      ;Stop all sounds
        lda #00
        sta bonsnd      ;Clear Bonus Sound Flag
		lda #-1
		sta maxsnd		;Stop any max sounds happening
        lda twopl       ;2 Player Game??
        ifne            ;Yes
			;--------------------------------Change Players------------------------------------
?psc1       ldx #01         ;Guess doing player 2
            lda player      ;Which Player??
            ifne                ;Player 2
                dex             ;Set to Player 1
            endif
            stx player		;flip to other player
            stx rampg       ;Select the RAM page
            jsr plrsel      ;Select Player and Controls
            lda lives,X     ;Does other player have any lives??
            beq ?psc1           ;No, Change Back.
            txa
            ifne                ;Yes
                lda pl2int      ;Played Yet??
                ifeq                ;No, has not
                    sta sellvl      ;Guess he will want level 1
                    lda #$10
                    sta addtim      ;Give him some time
                    sta objst           ;Turn him on (so this routine skips next time)
                endif
            endif
        endif
        lda mzgms           ;Recall what he was doing
        sta mzgame      
        lda #$40
        sta chngplr     ;Put up message
        lda #00
        sta toolong     ;Clear delay flag
        sta headcol
        sta tumble      ;Clear man jump status bytes
        lda #$FF
        sta newmzr      ;Flag for next maze
        sta frame       ;So output happens next
?scont  lda mzgame      
        ifmi                ;Do only once!
            lda gamest      ;Sucessful Exit
            and #$40
            ifne                ;Yep
                lda objxl
                sec
                sbc #$3D
                lda objxh
                sbc #$08            ;Ship Enterable from Left or Right
                ifcc
                    lda #00
                    sta direct      ;Force Direction
                else
                    lda objxl
                    sec
                    sbc #$C3
                    lda objxh
                    sbc #$08
                    ifcs
                        lda #$80
                        sta direct
                    else
                        lda #00
                        sta velxh           ;Stop Running
                        lda xmot
                        ora xmot+1
                        and #$F8            ;Done Moving??
                        ifeq                ;X is done
                            lda ymot
                            clc
                            adc #$C0            ;See if Y is at 'home'
                            lda ymot+1
                            adc #00
                            ifpl                ;We are back
                                lda ymot
                                sec
                                sbc #$C0
                                lda ymot+1
                                sbc #00
                                ifmi
                                    lda #02
                                    sta mzgame      ;Zoom Out
                                    lda oxygen
                                    jsr decimal     ;Total Count Into Decimal
                                    lda temp7       ;lsb
                                    ldx temp7+1     ;MSB total time count in decimal
                                    jsr bpoint      ;Add in these too
                                    lda #08
                                    sta shipyh
                                    sta statyh
                                    lda #$D0
                                    sta shipyl
                                    sta statyl
                                    lda #center
                                    sta shipxh
                                    lda #$80
                                    sta shipxl      ;Ready for takeoff!!
                                endif
                            endif
                        endif
                    endif
                endif
            endif
        endif
		;Fall through
        ;****************************************   
            .sbttl "Score & Lives to Buffer"
        ;**************************************** 
dscore  lda scoflg      ;Change??
        ifmi
            lda #00
            sta scoflg      ;Clear Request
            ldx player      ;Which one
            ifeq
                lda # ubyte(scobufs)
                sta vglist+1
                lda # lbyte(scobufs)
                sta vglist          ;Point to Score Buffer
            else
                lda # ubyte(scobuf2s)
                sta vglist+1
                lda # lbyte(scobuf2s)
                sta vglist
            endif
            lda lives,X
            sec
            sbc #01     	;Count the one we are using
            ifpl        	;Skip this in-case minus
                ifne
                    cmp #dislive+1      ;Will display max men only
                    ifcs
                        lda #dislive
                    endif
                    sta temp1           ;Count of lives
                    begin
                        VGADD_JSR(lifech)   ;Little man pic
                        dec temp1
                    eqend
                endif
            endif
            ldx player
            lda #dislive
            sec
            sbc lives,X     ;Leave space for other lives
            ifpl                ;Was not 5
                ifne                ;In case exactly 5
                    sta temp1
                    begin
                        VGADD_JSR(char_space)   ;Spaces
                        dec temp1
                    eqend
                endif
            endif
            ldx player
            ldy #03
            begin
                txa
                ifeq
                    lda score,Y
                else
                    lda score2,Y
                endif
                sta temp1,Y
                dey
            miend           ;Move score to temp area
            lda #temp1      ;Score moved here
            ldy #04
            sec
            jsr digits
            jsr vgrtsl
        endif
        lda velxh      ;Get Velocity
        pha
        eor direct      ;New Direction??
        ifmi
            lda #00
            sta scrflg      ;Force man to center
            lda #$80            ;Yep, set change request
        else
            lda #00         ;Clear Request
        endif
        sta dcreq           ;Save Request
        pla
        ifmi
            eor #$FF
            clc
            adc #01         ;Need Absolute Here
        endif
        ;***** Assume Normal Conditions, Set Sequence Based on Control *****
        ldy #runseq     ;Guess Running
        cmp #$18            ;At Jog??
        ifcc                ;Nope
            dey
            cmp #$10            ;Walk??
            ifcc                ;Must be stop
                dey
                cmp #03         ;Stopped??
                ifcc
                    dey
                endif
            endif
        endif
        tya
        bit ground      ;On the Ground??
        ifpl                ;Nope!
            clc 
            adc #04         ;Guess Jumping.. add 4
            bit jumpst      ;Jumping??
            ifpl                ;If not, must be falling
                clc 
                adc #04
            endif
        endif
        bit dcreq           ;Want to change directions??
        ifmi
            and #~runseq        ;Force to Stop
        endif
        sta picseq      ;Next Case
        bit landflg     ;Landing??
        ifmi                ;Yes! (Y=primative state)
            cpy #jogseq     ;Skip if Running or Jogging
            ifcc
                lda #landseq
                sta picseq
            endif
            lda piccur      ;What picture are we on
            cmp #idx_pic12  ;Sitting??
            ifeq                ;Yes
                lda sittime     ;Help him along
                ifne
                    lda frame
                    and #07
                    bit face        ;Want to do it faster??
                    ifmi
                        and #03
                    endif
                    tax             ;Set eq flag
                    ifeq
                        dec sittime
                        ifeq
                            lda #00
                            sta picseq      ;Done Sitting, Force to Stop
                            sta tumble      ;clear this flag too
                            sta landflg
                            sta face        ;Clear your face smash too!!
                            lda #$80
                            sta upflg           ;Signal Getting up!
                        endif
                    endif
                endif
            endif
        endif
        rts

;**********************************************
	.sbttl "Control Select and Screen Flip"
;**********************************************
;* Player screen and controls select.         *
;*                                            *
;* Updates out1s and send command to aux      *
;* processor to switch controls               *
;**********************************************
plrsel	lda	cabsel			;Need to worry about cabinet?? 1=upright 0=cocktail
		ifeq					;Yep!!
			lda player				;Who we updating??
			cmp playrlast
			ifne
				lda player
				sta playrlast			;Save it for next time
				ifeq					;Player 0
					;Tell GAMMA
					lda #g_setctrlp1
					jsr dosound
					lda	out1s				;Make sure screen faces correctly
					and	#$3F
				else
					;TODO: Tell GAMMA
					lda #g_setctrlp2
					jsr dosound
					lda	out1s
					ora	#$C0
				endif
				sta	out1s
				sta outputs			;Book em danno!
			endif
		endif
		rts	
		
; ;*************************************************
; ; Get highest diff level between the two players
; ; Returns value in A
; ; Destroys X,Y 
; ;*************************************************
; ?getdiflvl
		; lda #0				;Player 1 first
		; sta rampg           ;Make sure RAM set
		; ;are they on a tesser level?
		; jsr ?getpdiff
		; tay
		; ;wrplvl contains P1 either TokReturnDiff or normal diff
		; ldx twopl           ;2 Player Game?
		; ifne                ;Yes
			; lda #01         ;Page 1 for Player 2
			; sta rampg
			; lda wrplvl
			; jsr ?getpdiff
			; cpy wrplvl      ;No tesster.. see who greater
			; ifcc				;Player 2 was greater
				; ldy	wrplvl		;Get Player 2, it was greater
			; endif
		; endif
		; tya
		; rts

; ;********************************************************************
; ;Get the current players (RAM page already set) correct diff value 
; ;understanding that they may be in a tesser (diff 0,4,8,C)
; ; returns diff in A
; ?getpdiff	
		; lda wrplvl			;Get Player 1's normal level
		; ldx tokretr
		; ifne
			; txa
			; lsr A 
			; lsr A 			;tokretr value should now be right for difcty     
			; sta wrplvl
		; endif
		; rts
;****************************************
;* Transisition a player
;****************************************
?playertr
		;stop any max sounds
		lda maxsnd
		ifpl
			lda #-1
			sta maxsnd			;set it
			jsr setmaxv			;send it
		endif
		lda #-1
		sta castsnd			;Flag Star Castle to play background sound again (restart after dying)
		lda #0
        tay                 ;Default, died in space
        lda mzgame
        sta mzgms  
        ifmi                ;Another maze related death
            lda #02
            tay 
            lda #$A0
            bit objst+zreactor      ;Reactor set off??
            ifne
                lda #03         ;Point to Maze death after reactor
                tay
            endif
            tya             ;I told you not to ask
        else
            ifne                ;Died Landing??
                lda #01
                tay
            endif
        endif  
        lda st_plrmzs,Y     ;Y will contain either 0,1,2,3 depending on death location
        clc
        adc #01
        sta st_plrmzs,Y     ;Another Maze related death
        lda st_plrmzs+1,Y
        adc #00
        ifcc
            sta st_plrmzs+1,Y 	;Don't let wrap
        endif
        ;End of Deaths recording
		ldx player
        dec lives,X     	;1 Less Life
        ifeq                ;Game Over this player
            ;save maze stats
            jsr sendmstat
            lda twopl           
            ifne                ;2 Player Game
                lda #$7F
                sta gmov,X      ;Put up message 1 sec
            else                ;1 Player Game
                ldy wrplvl      ;xfer to Player 2 incase...
                lda #01         ;.. 2 player game is next
                sta rampg           ;RAM Page 1
                sty wrplvl
                lda #00
                sta rampg           ;RAM Page 0
            endif
            
            ;lda adddif      ;See if better
            ;cmp difcty
			lda add4mz
			cmp dif4mz
            ifcc                ;Yes, Better
?psc90          lda dif4mz
				sta add4mz
				;lda difcty
                ;sta adddif      ;This guy was higher.. xfer
                ;lda maznum
                ;sta addmn
                ;HACK: Removed from Select-A-Level 
				;lda mzgame
				;sta addmz           ;Save this to see where we were
				;END HACK
            else
                ifeq                    ;Same difficulty level
                    lda add4mz
					cmp dif4mz
					;lda addmn
                    ;cmp maznum          ;at a different level
                    bcc ?psc90          ;Yep.... set higher
                    ifeq                    ;If same, set this guy in the maze
                        bit mzgame          ;In Maze??
                        bmi ?psc90          ;Yep, made it to this level
                    endif
                endif
            endif
            lda demo        ;Demo Mode??
            ;cmp #03         ;Demo Mode
            ifeq                ;nope
                ;jsr stpg0      ;removed because this is on the same page now
                jsr update      ;Check for High Scores
            endif
        else
			;Rex died, but there are more clones for this player
			lda #$A0
			bit objst+zreactor  ;Reactor set off??  (warning Z flag usage of BIT instruction)
			ifne                ;Yep
				lda #00
				sta mzgame      	;Fake Restart routine
				sta mzgms       	;Store to Mask Too
				jsr spcini      	;This will make it look like he did good.
				lda isfinal			;final station?
				ifne
					jsr prephw			;get ready for sequence
				endif
#IF TOURNAMENT_EDITION = 1
			else				;Nope
				lda isfinal			;final station, special case!
				ifne
					lda maznum
					ifne				;for levels 22,23,24, start back at Level 21 (and reset everything, sorry Rex!)
						lda #0
						;sta isfinal
						sta maznum
						sta maztype
						sta holmz       	;Makes sure all are re-initialized
						lda difcty
						asl A  
						asl A 
						sta dif4mz			;Fix dif4mz too
					endif
				endif
#ENDIF
			endif
			;endif
        endif
        rts        

        
;*****************************************
    .sbttl "Display Lives and Bonus"
;*****************************************
distlibo   
		ldx #melife         ;Extra Life Message
        jsr mesg

#IF LANGUAGE = 2
		VGADD_VCTRL(-576,0,hidden)		;French
#ELSE
#IF LANGUAGE = 1
		VGADD_VCTRL(-256,0,hidden)		;German
#ELSE
		VGADD_VCTRL(-608,0,hidden)		;English
#ENDIF
#ENDIF
		
		;VGADD_VCTRS(0,-84,hidden)
        lda slives          ;Number of Lives per game
        sta temp1
        sec 
        lda #temp1
        ldy #01
        jsr digits          ;How many lives
		

		
        ldx nxtbonus        ;Switch Settings
        lda ?bontbl,X       ;Get K Settings
        ifne
            pha
            ldx #mbolif         ;Bonus every ...
            jsr mesg
            pla
            sta temp1+2         ;How Much??
            lda #00
            sta temp1
            sta temp1+1         ;0000
            lda #temp1
            ldy #03
            sec
            jsr digits          ;Display 100,000
        endif
        ; lda twocoin         ;Two Coin Minumum
        ; ifpl
            ; lda c_tcmflg        ;Waiting
            ; ifmi                    ;Yep, not satisfied
                ; lda c_crdt              ;Any Credit
                ; ifne                    ;1 Credit waiting for second credit here
                    ; lda frame
                    ; and #$10                    ;Flash and Make noise
                    ; ifne
                        ; lda #snd_b2b                ;Make a beep
                        ; jsr dosnd2
                        ; lda #$10
                    ; endif
                ; else
                    ; lda #$10                    ;Else always On
                ; endif
                ; ifne
                    ; ldx #mcmod4
                    ; jsr mesg
                ; endif
            ; endif
        ; endif
        rts

?bontbl	.byte   $00,$05,$10,$20
        
;*************************************************
    .sbttl "Read Jump and Shield Buttons"
;*************************************************
rdjmp   lda gamest
        ifmi
            and #$10            ;Count Hold??
            ifeq                ;No
                lda manstat
                ifne
                    ifpl
                        and #$DF            ;Guess Cleared
                        bit shldok          ;Shields Allowed
                        ifmi
                            bit lastswitch          ;Button pressed
                            ifvc
                                bit mzgame
                                ifmi
                                    pha
                                    lda #snd_i1a
                                    jsr dosound
                                    pla
                                endif
                                ora #$20
                            endif
                        else
                            bit lastswitch          ;Pushed this frame?
                            ifvc
                                bit jboswt
                                ifvc                    ;Not pushed last frame
                                    bit mzgame
                                    ifmi
                                        pha
                                        lda #snd_i1c
                                        jsr dosound
                                        pla
                                    endif
                                endif
                            endif
                        endif
                        sta manstat
jmp3                    lda lastswitch          ;Jump Button
                        jsr neg
                        tay
                        lda jboswt          ;Old Switch Status
                        sty jboswt          ;Save Read Status
                        tay
                        and jboswt          ;1's where twice 1's
                        ora jbstat
                        sta jbstat          ;Save for Main Line
                        tya
                        ora jboswt          ;0 where twice 0
                        and jbstat
                        sta jbstat
                        ;swstat will contain last 'Solid-State' case
                    endif
                endif
            endif
        endif
        rts
        
        
        
;*****************************************
    .sbttl "Jump Control Status"
;*****************************************
jumpr   bit gamest      ;Leaving Maze??
        ifmi
            ifvc                ;Nope, make check
                lda mazeyh      ;Above Maze??
                cmp #$FC
                bcs ?clrjump    ;No jumps above maze
            endif
?jmpr2      lda mzgame      ;Landing or Just Landed??
            and #02
            bne ?clrjump
            bit tumble      ;Hit Head??
            bmi ?clrjump
            lda objst+zstuf+1   ;Are the Magic Boots on?
            cmp #02
            ifeq                ;Yes
                lda objst           ;Man Still Around??
                beq ?clrjump         ;Don't Jump if he isn't
                bmi ?clrjump
                bpl ?jcs10          ;Branch
            endif
            bit mzgrnd      ;On Ground??
            ifmi                ;Yes
?jcs10          bit jbstat          ;Button Pressed
                ifmi
                    lda #00
                    sta landflg         ;Stop Landing
                    lda #$80            ;Start Jumping
                else
                    lda #00
                endif
                sta jumpst          ;Clear Jump Bit
            else
                bit jbstat          ;Button Still Pressed??
                ifpl
?clrjump            lda #00
                    sta jumpst          ;Clear Jump Flag
                endif
            endif
        else
            bit markgm      ;Find out if control needed??
            ifmi
                lda markpt      ;number of times to do this motion
                ifeq
                    lda markls
                    clc
                    adc #02
                    sta markls
                    tax
                    lda ?mazecor1+1,X
                    sta markpt
                endif
                dec markpt
                ldx markls
                lda ?mazecor1,X
                sta temp1
                and #$80
                sta jbstat      ;Jump status in D7
                lda temp1
                and #$40
                lsr A
                sta temp1+1
                lda manstat
                and #$DF
                bit shldok      ;If shields left
                ifmi
                    ora temp1+1     ;Shield Status in D6
                endif
                sta manstat
                lda temp1
                asl a
                asl a
                sta rgdd            ;Rolly-gig data in D0-D5, Signed
                jmp ?jmpr2
            endif
        endif
        rts 

;************************************************************
;* Maze Correography - For Attract Mode Rex
;************************************************************
; Bit 80 = 1 = Jump
; Bit 40 = 1 = Shield
; Bits 3F = Direction (D5 is sign)
;************************************************************
#IF TOURNAMENT_EDITION = 0
?mazecor1   .db $00,$03
			.db $07,$02
			.db $08,$13
			.db $81,$18
            .db $88,$04
			.db $0C,$24
			.db $03,$30
			.db $08,$0A
            .db $03,$4C   ;here, was $48
			.db $3C,$20
			.db $00,$40
			.db $B8,$0A
            .db $34,$08
			.db $00,$38
			.db $40,$08
			.db $48,$18
            .db $BF,$40
			.db $B8,$18
			.db $BF,$12
			.db $38,$3C
            .db $B8,$0A
			.db $3C,$40
			.db $08,$0C
			.db $3F,$10
            .db $38,$24
			.db $81,$80
			
?mazecor2   .db $00,$16
			.db $07,$38
			.db $02,$12
			.db $00,$10
			.db $40,$20
			.db $3E,$18
			.db $00,$08
            .db $BE,$18
			.db $3C,$48
			.db $82,$18
			.db $88,$04
            .db $0C,$24
			.db $04,$0A
			.db $02,$10
			.db $00,$60
            ;.db $3E,$10
			;.db $04,$20
			;.db $84,$40
			.db $02,$10
            .db $05,$28 
			.db $00,$20	;land by reactor
			.db $03,$28
			.db $3D,$20
			.db $38,$3C
            .db $00,$80
			.db $00,$80
#ELSE  
            ;Tournament Edition Rex 
?mazecor1   .db $00,$16
			.db $07,$38
			.db $02,$12
			.db $00,$10
			.db $40,$20         ;Shield
			.db $3E,$38
            .db $BE,$18         ;Jump
			.db $3C,$8D
            .db $00,$80
            .db $3E,$10
            .db $BE,$18         ;Jump
            .db $3C,$40
            .db $00,$FF         ;Rex leans


?mazecor2   .db $00,$16
			.db $07,$38
			.db $02,$12
			.db $00,$10
			.db $40,$20         ;Shield
			.db $3E,$38
            .db $BE,$18         ;Jump
			.db $3C,$8D
            .db $00,$70
            .db $06,$80         ;Rex dies
            .db $00,$FF
            .db $00,$FF
            
#ENDIF

;************************************************************
    .sbttl "Markjump - Mec control of spaceman motion"
;************************************************************
markjump    
        lda #00
        sta temp1
        bit atflag      ;In attract mode??
        ifmi
            bit mzgame      ;Inside Maze??
            ifmi
                bit markgm      ;Outside of control last time??
                ifpl
                    bit openflg
                    ifpl                ;Now inside control??
                        lda #$80
                        sta temp1
                        lda #00
                        sta markpt
                        lda #-2			;Start 2 down, will increment to zero
                        sta markls
						lda rands
						and #$07
						cmp #$05
                        ifcs
							lda #?mazecor2-?mazecor1-2
							sta markls
						endif
                    endif
                else
                    lda mazeyh
                    cmp #$FC
                    ifcc                ;If inside maze
                        lda #$80
                        sta temp1
                    endif
                endif
            endif
        endif
        lda temp1
        sta markgm
        rts

        
            