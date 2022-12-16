;********************************************************
    .TEXT ".TWSHIP."
;********************************************************
    .title "TWShip - Ship Control"
    .sbttl "Game States"
;********************************************************
;* Game States and the numbers that go with them..      
;*                                                      
;*     STATE                   MZGAME  GAMEST  X FOR    
;*       #                     VALUE   BIT 7   TARGSHIP 
;*------------------------------------------------------
;*                                                      
;* 1). In Tube                 40         0    0/2      
;* 2). Tact Dsp, Space Trans.  20         0     1       
;* 3). Space Play              00         0     2       
;* 4). Left/Right Move(trans)  10         0    3/4     
;* 5). Upwards trans to center 08         0     5      
;* 6). Landing on Maze         04         0     2       
;* 7). Zoom in after land      02         0     6       
;* 8). Game Play in Maze       80        0/1   ***      
;* 9). Zoom out after Exit     02         1     7       
;*10). Take off from Maze      04         1     2       
;*11). Blow Maze Sequence      01         1    9/10     
;*                                                      
;* Note:    Gamest bit 7 represents a succesful exit    
;*          from the maze. X is the index to the        
;*          motion/speed used by that section of game   
;*          play.                                       
;* Note:    If tact is minus, then tactile scanner mode 
;*          is on; and X is always set to 8 until that  
;*          mode ends. No change of gamest will take    
;*          place in the tact routine.                  
;********************************************************
__numtrg 		= 13d
__curtrg		= 0d
__tmptrg		= 0

#DEFINE deftarget(ptrgname,ptrgx,ptrgxl,ptrgy,ptrgyl,pspeedx,pspeedy,pdscal,ptrscl,ptrsch)		\__tmptrg .set *
#DEFCONT																						\ptrgname = __curtrg
#DEFCONT																						\ .org mvspdy+__curtrg
#DEFCONT																						\ .byte pspeedy 
#DEFCONT																						\ .org mvspdx+__curtrg
#DEFCONT																						\ .byte pspeedx
#DEFCONT																						\ .org targx+__curtrg
#DEFCONT																						\ .byte ptrgx
#DEFCONT																						\ .org targxl+__curtrg
#DEFCONT																						\ .byte ptrgxl
#DEFCONT																						\ .org targy+__curtrg
#DEFCONT																						\ .byte ptrgy
#DEFCONT																						\ .org targyl+__curtrg
#DEFCONT																						\ .byte ptrgyl
#DEFCONT																						\ .org mvscal+__curtrg
#DEFCONT																						\ .byte pdscal
#DEFCONT																						\ .org tarscl+__curtrg
#DEFCONT																						\ .byte ptrscl
#DEFCONT																						\ .org tarsch+__curtrg
#DEFCONT																						\ .byte ptrsch
#DEFCONT																						\__curtrg .set __curtrg+1
#DEFCONT																						\ .org __tmptrg
#DEFCONT                        																\#IF __curtrg > (__numtrg)
#DEFCONT			            																\	.error "TARGSHIP: Too many targets defined. Increase __numtrg!"
#DEFCONT				        																\#ENDIF 

;*************************************************************************************************************************************************
;         Name       Target X		 Target Y		 XSpeed  YSpeed  ScalCh	ScaleLi	ScaleBi
;*************************************************************************************************************************************************
;  Coord 0,0 is top left of screen for targeting
;*************************************************************************************************************************************************
;         ptrgname,  ptrgx,   ptrgxl,ptrgy	,ptrgyl	,pspeedx,pspeedy,pdscal	,ptrscl	,ptrsch
deftarget(trg_tubem	,center-1,$00	,$00	,$00	,$02	,$06	,$01	,$70	,$71)		;Tube with motire towards center
deftarget(trg_downc	,center  ,$70	,$0B	,$B8	,$04	,$16	,$04	,$16	,$72)		;Transition down towards center (start space)
deftarget(trg_hold	,0  	 ,$00	,$00	,$00	,$00	,$00	,$00	,$10	,$71)		;Hold
deftarget(trg_right ,rtedge-3,$00	,$0B	,$B8	,$08	,$28	,$03	,$16	,$72)		;Right side space ending
deftarget(trg_left	,center-1,$00	,$0B	,$B8	,$08	,$28	,$03	,$16	,$72)		;Left side space ending
deftarget(trg_up	,center  ,$80	,$01	,$00	,$08	,$28	,$03	,$28	,$72)		;Frome Right/Left into landing prep
deftarget(trg_zoomi	,center+1,$33	,$05	,$40	,$08	,$28	,$06	,$10	,$70)		;Zoom in for maze
deftarget(trg_zoomo	,center  ,$80	,$08	,$D0	,$04	,$18	,$04	,$28	,$72)		;Zoom out from maze
deftarget(trg_tact	,center  ,$80	,$08	,$20	,$08	,$18	,$08	,$00	,$73)		;To tact position (typically from trg_iretr)
deftarget(trg_infin	,center  ,$80	,$04	,$00	,$01	,$04	,$05	,$10	,$76)		;After maze, zoom into infinity
deftarget(trg_iretr	,center  ,$80	,$06	,$00	,$01	,$10	,$08	,$16	,$70)		;Return from Infinity
deftarget(trg_down	,center  ,$00	,$0B	,$B8	,$00	,$16	,$04	,$16	,$72)		;Transition down to bottom, but no X movement
deftarget(trg_tiptop,center  ,$80	,$04	,$00	,$04	,$10	,$03	,$10	,$76)		;Maynard fly in
;deftarget(trg_tactb	,center  ,$80,$0B,$20,$08,$18,$08,$00,$73)		;To tact position but lower so we are out of the way of the Vaxx bonus

targx   .block 	__numtrg	;.byte center-1,center,0,rtedge-3                ;X Target Position
							;.byte center-1,center,center+1
							;.byte center,center,center,center
							;        1   2   3   4   5   6   7   8   9   A   B
targxl  .block	__numtrg	;.byte $00,$70,$00,$00,$00,$80,$33,$80,$80,$80,$80   ;X Position LSB Target
targy   .block	__numtrg	;.byte $00,$0B,$00,$0B,$0B,$01,$05,$08,$08,$04,$06   ;Y Target Position
targyl  .block	__numtrg	;.byte $00,$B8,$00,$B8,$B8,$00,$40,$D0,$20,$00,$00   ;Y Position LSB Target
mvspdx  .block 	__numtrg	;.byte $02,$04,$00,$08,$08,$08,$08,$04,$08,$01,$01   ;X Speed
mvspdy	.block 	__numtrg   	;.byte $06,$16,$00,$28,$28,$28,$28,$18,$18,$04,$10   ;Y Speed
mvscal  .block	__numtrg	;.byte $01,$04,$00,$03,$03,$03,$06,$04,$08,$05,$08   ;Scale Change Amount
tarscl  .block	__numtrg	;.byte $70,$16,$10,$16,$16,$28,$10,$28,$00,$10,$16   ;Target Scale Linear
tarsch  .block	__numtrg	;.byte $71,$72,$71,$72,$72,$72,$70,$72,$73,$76,$70   ;Target Scale Binary

;********************************************
    .sbttl "Transitions Routine"
;********************************************
;* Entry from Mainline only if mzgame is    *
;* not minus                                *
;********************************************
shipout ;Do Ship Routine
        ;Select Target position, scale and speed index
        ldy #00         	;If 0 at end, we do targship
        ldx #trg_hold		;If we fall out bottom, we are in space
        lda mzgame      	;Which Play Level
        ifeq                	;In space
?ssp        ldx spcspd      	;Slow Down Space
            cpx #06
            ifcc
                lda frame
                and #$1F
                ifeq
                    inc spcspd      ;Slow Down
                endif
            endif
            dey         ;Skip targship
        else
            asl a       ;40 into -
            ifmi                ;Game Status 2 (40=In Tube)
                ldx #trg_tubem    	;Tube Motion
                bit tstat           ;Tube Motion on Hold??
                ifpl                ;Yes
                    ldx #trg_hold		;So do nothing now
                endif
            else                ;Not 40
                asl a   ;20 into -
                ifmi            ;Game State 3 (20 =Transition Down)
                    jsr pictran     ;Need to do transition (maybe)
                    ldx #trg_downc
					
					;bit animate			;For wormhole, no targship, allows ship to move freely
					;ifmi
					;	dey
					;endif
					
					bit tact            ;Doing Tact Scanner?
					ifmi
						ldx #trg_tact		;Hold at center then
						lda vxstat
						ifne
							dey					;no targship for vaxx stuff
						endif
					else
						lda animate			;Wormhole?
						ifmi
							ldx #trg_down
						endif
					endif
                else            ;Not 40 or 20
#IF DISABLE_FASTTRANSITIONS = 0
                    ldx #$80
                    stx gofast      ;Do this fast
#ENDIF
                    asl a           ;10 into -
                    ifmi                ;Game State 4 (10 = Left/Right Move, Maynard too!)	
						lda vxstat
						ifne
							ldx #trg_tiptop
						else
							ldx #trg_right  	;Will Guess Transition Right
							lda shipxl
							cmp #$80
							lda shipxh      ;Which Side are we on??
							sbc #center     ;Greater than 3,80??
							ifcc                ;The Left!!
								ldx #trg_left   	;Move Left instead
							endif
						endif
                    else                Not 40,20 or 10
                        asl a           ;8 into -
                        ifmi                ;Game State 5 (08 = Upwards Transition)
							jsr pictran     ;Change Pics!
							ldx #trg_up
                        else                ;Not 40,20,10 or 08
                            asl a           ;4 into -
                            ifmi                ;Game State 6 or 10
                                ldx #trg_hold
                                dey             ;Neither do anything
                            else                ;Not 40,20,10,08 or 04
                                asl a           ;2 into -
                                ifmi                ;Game State 8 or 10 (Zooming!)
                                    bit gamest      ;In or Out?
                                    ifvc                ;Zoom in
                                        ldx #trg_zoomi
                                    else
                                        ldx #trg_zoomo
                                    endif
                                else                ;Else must be State 1 (Blow Station)
                                    jsr blowit      	;Do Motion Control
                                    ldx #trg_infin  	;Do Zoom stuff - Zoom to infinity
                                    lda blowst      	;Start moving forward
                                    ifeq
                                        ldx #trg_iretr
                                    endif
                                endif
                            endif
                        endif
                    endif
                endif
            endif
        endif
        ;******************* Fall Through to Next **********************
        tya             ;Do Targship??
        ifpl
            jsr targship
        endif
        jsr moveship        ;Move and Draw Ship
        ;Above calls shipdis
        lda shipst      ;Skip if the ship died
        ifne
            ifpl
                lda targf           ;At target above??
                ifmi                ;yes
                    lda mzgame      ;First time in??
                    ifpl                ;Might have gone minus
                        cmp #$20
                        ifcs                ;20 or 40??
                            bit tact            ;But wait for tact to go away
                            ifpl
								lda animate			;Don't start space if this is set tho
								ifpl
									;******************************************************
									;* Re-init our 'isfinal' flag
									;******************************************************
									jsr stpg6			;PAGE CHANGED!!!!!!!
									ldx dif4mz
									lda mscstl,X
									sta isfinal
									jsr stpg0			;PAGE CHANGED!!!!!!!
									;******************************************************
									;******************************************************
									jsr startspace      ;Start Space Wave
									lda #$3F
									sta mtim            ;Display Message (If Needed)
									lda #00
									sta tact            ;Clear Tact Display
									;was it finished?
									lda bronce
									ifmi
										and #$03
										clc
										adc #01
										cmp #nmballs
										ifcs
											lda #nmballs-1
										endif
										sta bronce			;If breakout was done, reset it here always so it will come back
										jsr newbrk          ;Restore breakout bricks
									endif
									jsr initcolram		;Reset Colors
								endif
                            endif
                        else
                            cmp #08     ;From Transition Mode??
                            ifeq            ;yep
								jsr dospace
                            else
								cmp #01     ;Blowship/Animation??
								ifeq
									jsr bumpini
								endif
                            endif
                        endif
                    endif
                endif
            endif
        endif
		jsr award
        rts 
        
bumpini lda blowst      ;End Maze Sequence?
		ifeq
			jsr stpg0
			jsr sendmstat   ;Send this level's Maze stats now..
			lda isfinal
			ifne   
				jmp prephw		;Set Homeworld as completed! YAY!!   		
				;Does not continue here - Short circuit
			endif
			lda #00
			sta seqst
			sta seqx        ;Reset!
			lda #$20
			sta mzgame      ;Back to tact (downward)
			lda #$80
			sta gamest      ;Clear Exit Bit
			sta tact        ;And turn this back on now
#IF (LEVEL_EDITOR == 0)
			sta tactde      ;Still Want tact next time
#ENDIF
			jsr nextmz      ;Set up next maze
			lda maznum
			sec
			sbc #01
			and #03
			sta seqp        ;Start 1 pic back
		else                ;Could be moving back
			cmp #$40            
			ifeq
				lda #$50        ;Set Move Station Bit
				sta blowst
				lda #$80
				sta statst      ;And Turn on Station
			endif
		endif
        rts

;******************************************************
; Prep for Final Animation sequences
;******************************************************
prephw	;Reset back to first level
		lda #0
		sta difcty
		sta maznum
		sta dif4mz
		;reset frame - HACK: can we do this?
		sta frame
		sta frame+1
		;hide text for bonus to start
		sta perm5+1
		;sta newmzr			;Flag to not load new maze objects
		;reset fireworks
		jsr resfirew
		;set vaxx flags
		lda #$80 			
		sta vxstat			;Start Vaxx Bonus
		lda #1
		sta manstat			;Turn off rex for now, but not dead
		lda tokpock
		ora #$10
		sta tokpock			;Give him a medal
		lda #$20
		sta mzgame      	;Back to tact (downward)	
		lda gamest
		and #$40
		ora #$80			;Keep sucessful exit set!
		sta gamest      	;Clear Exit Bit
		lda #$80
		sta tact        	;And turn this back on now
		sta tactde      	;Still Want tact next time
		lda #snd_spbonus	;Do the score background music
		jmp dosound
		
;******************************************************
    .sbttl "Start Space Play Wave from Transition"
;******************************************************
dospace lda #04         ;Set to Third person play level
        sta mzgame      ;Must be at 3rd play point
        sta spcspd      ;Stars on take off speed
        lda #$3F
        sta mtim        ;Display Message if needed
		lda #$73
        sta stsclh		;station scale
		;***************************************************************
		; Landing speed is based upon two different parts
		; Ship - starts @ 5 and increases one for each level
		; Station - starts @ -7 an decreases one for each level
        ;lda #-7     
        ;sec 
        ;sbc dif4mz      ;-7 to -12
        ;sta statst      ;Turn on space station and start stars
		ldx dif4mz
		lda statspeed,X 
		sta statst
		;***************************************************************
		; Now deal with the X nudging to start the landing process
        lda dif4mz
        clc 
        adc gamedif
        adc #04
        cmp #07			;THIS USED TO BE $08, we are slowing down max nudge from side to side here
        ifcs
            lda #07			;THIS USED TO BE $08
        endif           ;Get Speed of Landing
        lsr A
;*********************************************************
;* Hack for Developing and Level Edtior, the ship will
;* always land straight down. :-)
;*********************************************************        
#IF ((DEBUG > 0) | (LEVEL_EDITOR > 0))
        lda #00
#ELSE
        ldy rands
#ENDIF
;*********************************************************
        ifmi
            jsr neg
        endif
        sta shpvel+1
        lda #00
        sta statxl
        sta stscll
        sta gunctl
        sta gunctl+1    ;Close the guns
        lda #$0C
        sta statyh      ;Initial Position of Space Ship
        lda #$80
        sta statyl
        sta statxl      ;LSB's at 80
        lda #center
        sta statxh      ;Center of Screen X
        rts 
        
;***********************************************
    .sbttl "Blow Base Station Sequence Control" 
;***********************************************
;* Used to control action of base star and     
;* player ship for the blow up base star       
;* sequence                                    
;***********************************************
blowit  lda frame
        and #07
        tay             ;Save for anybody who needs this below
        lda #$20        ;Set NE test for below
        bit blowst      ;Already Blown??
        ifvs                ;Keep Moving Backward
            jsr pictran     ;Change to end pic
            lda spcspd      ;Get Star speed to 5
            cmp #06
            ifcs                ;Star Speed at 5
                tya
                ifeq
                    dec spcspd
                endif
            endif
            lda stroyh      ;Would like stars near center
            ifne
                lda stroyl
                sec
                sbc #$20            ;Move down star origin
                sta stroyl
                ifcc
                    dec stroyh
                endif
            endif
        else
            ifne                ;We wish to move forward...
                lda spcspd      ;Start decel move
                cmp #09
                ifcc
                    tya
                    ifeq
                        inc spcspd      ;Slow down first
                    endif
                    
                else
                    lda #00
                    sta blowst      ;Set flag for accel again
                    sta rearview        ;Set to forward again
                endif
            else                ;Want to move forward now
                lda spcspd
                cmp #03
                ifcs                ;Speed up stars again
                    tya
                    ifeq
                        dec spcspd
                    endif
                endif
            endif
        endif
        ;**************** End Star Control ***************
        lda blowst
        ifpl                ;Skip if we already started this
            and #$10            ;Moving Base Ship??
            ifne                ;yes
                lda statyl      ;Move up from bottom
                sec
                sbc #$40
                sta statyl
                lda statyh
                sbc #00
                sta statyh      ;Move up ship
                lda stscll
                clc
                adc #$0C            ;Move away speed
                ifmi
                    sec
                    sbc #$80
                    inc stsclh
                endif
                sta stscll
                lda stsclh
                cmp #$75			;Has enemy station reached the explosion point?
                ifeq
                    lda #$C8
                    sta blowst      ;BOOM!!!
                    sta frame       ;Force Frame for Flash effect
                    jsr dostop
					;jsr do3del
                    lda #snd_kilhro ;Stop Music if playing already
                    jsr dosnd2      ;Always!
                    lda #$76
                    sta blsclh
                    sta blscll      ;Start Scale of pieces
                    lda shipst      ;Must save this
                    pha
                    jsr blowship    ;And use his peices to do explosion
                    pla
                    sta shipst      ;Restore (clobbered by blowship)
                endif
            endif
        else
            ;**************  End Station Control **************
            ;************ Now add explosions if needed ********
            ldx #$nmexp-1       ;A few more peices
            jsr piece2      	;Do Explosion
            lda blsclh      	;Big Already??
            cmp #$71            ;Max Size
            ifcs                ;Not there yet
                lda blscll
                sec
                sbc #05
                ifmi
                    sec
                    sbc #$80            ;Keep in bottom 80
                    dec blsclh
                endif
                sta blscll      ;Save back LSB
                
            else                ;Are there
                lda #$20
                sta blowst      ;Next Wave
            endif
            lda frame
            cmp #$D8
            ifcs                ;Still white explosion pic
                ldy #$C0
                sty blowst      ;Drop white bit
            endif
            and #03         ;Time for another explosion?
            ifeq                ;yes
                ldx nxtexp      ;Pointer to next guy
                cpx #nmform     ;Any left??
                ifne
                    ldy #00
                    jsr getrand
                    and #$3F
                    bit rands           ;Bit Random Number
                    ifmi
                        jsr neg         ;Negative Number
                        dey
                    endif
                    clc
                    adc statyl
                    sta sobjyl,X
                    tya
                    adc statyh
                    clc
                    adc #05         	;Compensate for base being not offset by cr3p
                    sta sobjyh,X
                    lda #$80
                    clc
                    adc sobjyl,X
                    sta sobjyl,X
                    ifcs
                        inc sobjyh,X        ;Rest of Compensation
                    endif
                    ldy #00
                    jsr getrand
                    and #$3F
                    bit rands+1
                    ifmi
                        jsr neg         ;Negative Number
                        dey
                    endif
                    clc
                    adc statxl
                    sta sobjxl,X
                    tya
                    adc statxh
                    sta sobjxh,X
                    lda #$80
                    sta sobjst,X        ;Explode it!
                    lda #snd_j3
                    jsr dosound
                    inc nxtexp
                endif
            endif
        endif
moveexp ;Entry from below in inblow
        ldx #nmform-1
        begin
            stx temp9
            lda sobjst,X        ;Exploding this one?
            ifmi
                sta temp3           ;drawen wants this to change scale of explosion
				clc
				bit isfinal			;test this
				ifne
					;Final Station - more explosions
					adc #01				
                else 
					adc #04
				endif
				;rollover
                ifpl
                    lda #00
                endif
                sta sobjst,X
                jsr copypos     ;Copy position to vdata
                jsr drawen2     ;Draw this guy
            endif
            ldx temp9
            dex
        miend
        rts 
        
inblow  ldx nxtexp
        cpx #nmform
        ifne
            lda frame
            and #01
            ifeq
                lda inblotx,X
                sta sobjxh,X
                lda inbloty,X
                sta sobjyh,X
                lda #$80
                sta sobjyl,X
                sta sobjxl,X
                sta sobjst,X
                lda #snd_j3
                jsr dosound
                inc nxtexp
            endif
        endif
        jmp moveexp
                    
inblotx .byte $04,$04,$04,$05,$05,$05,$03,$03,$03,$02,$08,$04,$04,$05,$06,$04
inbloty .byte $06,$07,$08,$07,$08,$06,$08,$06,$07,$08,$07,$07,$09,$05,$04,$05

;********************************************
    .sbttl "Base Ship Control"
;********************************************
;* Start Space Game (Move ship upscreen and *
;* away). If skip flag (lauen=40), then     *
;* just go to passby wave.                  *
;********************************************
startspace  
        ldx maznum      ;Get Start Positions
		
#IF (LEVEL_EDITOR != 0)
		;set overflow flag
		lda #$7F ;   +127
		adc #$01 ; +   +1
#ELSE
		bit lauen       ;Skip Space Fight??
#ENDIF
        ifvs            ;yes... use X=4
            lda #$10        ;Value to store to lauen
            ldx #00         ;Will use item 0 for skip mode
        else            ;Do this if not skip
            lda #09     
            sta shipyh
            lda #$C0
            sta shipyl
            lda #00
        endif
        sta mzgame      ;Store Game Play
        lda #$80
        sta statst      ;Also turn on Station
        lda s_statm,X   ;Station Motion
        sta stbflg      ;Motion Up Flag
        lda s_statx,X   ;Station XH
        sta statxh
        lda s_statxl,X
        sta statxl
        lda s_staty,X
        sta statyl      ;Set Y
        sta statyh      ;*** Temp Start Position ***
        lda #00
        sta saucvl      ;Stop Saucer Motion
        sta saucvd
        rts 
        
s_statm     .byte $80,$C0,$00,$80 			;Station Motion
s_statx     .byte center,0,center,center  	;Position MSB
s_statxl    .byte $80,$10,$80,$80          	;Position LSB
s_staty     .byte $00,$07,$00,$00          	;Y MSB Position

;********************************************
    .sbttl "Approach Target"
;********************************************
;* This routine is used to allow the ship to
;* approach a destination position for auto 
;* ship control.                            
;*                                          
;* Input: X = Select of Target Destination  
;********************************************
targship    
        ldy #00         ;Signal not there yet
        sty temp1+1     ;Clear Flag for zooming out, LSB force mode
        lda shipxh      ;Do X First
        pha 
        lda mzgame      ;Special Target for Zoom
        cmp #02         ;Zooming?
        bne ?at5        ;No, Do normal.
        bit gamest      ;Zooming Out??
        ifvs                ;yep!
            dec temp1+1     ;Set flag
            sec
            lda shipxl
            sbc landsl      ;Back to landing spot
            sta temp1
            pla
            sbc landsh      ;Move Back to original Landing Sight
        else
?at5        lda shipxl          ;Zooming in if here
            sec
            sbc targxl,X
            sta temp1           ;Save for LSB check
            pla
            sbc targx,X     	;X Target
        endif
        ifne                ;Not there yet
?at6        ifvs
                eor #$80
            endif
            ifpl                ;To Right of Target
                dey
                lda mvspdx,X
                jsr neg2
            else
                lda mvspdx,X
            endif
            sta shpvel+1
            clc
            adc shipxl
            sta shipxl
            tya
            adc shipxh
            sta shipxh
            iny             ;Insure positive number here
        else
            ;Could approach from right side of target, MSB would be 0
            lda temp1           ;Any LSB's
            and #$F8            ;Drop 0-7
            ifne                ;Not at target
                lda #00         ;Make MSB 0
                beq ?at6
            endif
            bit temp1+1     ;Zooming Out??
            ifmi                ;yep
                lda shipxl      ;Just force back, skip approach
                sta landsl      ;Force LSB's to be the same
            endif
            ldy #$80            ;At target X
        endif           	;End of X
        sty targf           ;Stor X at Target Part
        ;--------------------------------------------------------------
        ;------------- Now Check Y Part -------------------------------
        ;--------------------------------------------------------------
        ldy #00         	;Signal Not at Target
        lda shipyl
        cmp targyl,X        ;LSB change too
        lda shipyh
        sbc targy,X     	;At Y Target
        ifne                ;Not there
            ifvs
                eor #$80
            endif
            ifpl                ;To Right of Target
                dey             ;Sign prop
                lda mvspdy,X        ;Move Speed
                jsr neg2
            else                ;Move in from Right
                lda mvspdy,X
            endif
            clc 
            adc shipyl
            sta shipyl
            tya 
            adc shipyh
            sta shipyh
            iny             ;Insure + Number
        else
            ldy #$80
        endif           ;End of Y
        tya 
        and targf
        sta targf           ;Both must be 80 for done
        ;----------------------------------------------------------------
        ldy #00         ;Guess scale not at a target
        lda shpsch      ;Want to shrink down ship
        cmp tarsch,X        ;Down to target??
        ifne
            ifcs
?at10           lda shpscl      ;Change Scale Down
                sec
                sbc mvscal,X        ;Change Scale
                ifmi                ;Went past 0
                    sbc #$80            ;Stay within lin < 80
                    dec shpsch      ;Linear down by 1
                endif
            else
?at20           lda shpscl
                clc
                adc mvscal,X        ;Change up
                ifmi                ;Went past 80
                    sec
                    sbc #$80
                    inc shpsch      ;Bin up by 1
                endif
            endif
?at25       sta shpscl
        else                ;Bin is the same
            sec
            lda shpscl
            sbc tarscl,X        ;Check Linear
            ifne
                ifcs                    ;Target is smaller
                    cmp mvscal,X            ;Smaller diff than change??
                    ifcc                    ;yes
?at28                   lda tarscl,X            ;Then just set at target
                        ;************** Caution, never set a target at 0 ************
                        jmp ?at25               ;Go and Store
                    endif
                    jmp ?at10               ;Else do normal
                endif
                ;Not CS... Must be CC, that is target is greater!
                jsr neg             ;Get + Difference
                cmp mvscal,X            ;Diff less than Scale??
                bcc ?at28               ;Just store it then
                bcs ?at20               ;Else do normal
            endif
            ldy #$80                ;Must be at target
        endif
        tya 
        and targf
        sta targf           ;Put in with Targets!
        ;******************* Now Check for Base Scale Change if needed *********************
        lda mzgame
        and #02
        ifne                ;Game 2 needs it!
            bit gamest      ;Want to grow??
            ifvc                ;yes
                lda stsclh      ;Grow to target size
                cmp #sttarh     ;Up to target
                ifne                ;Not there yet
?at30               lda stscll      ;Change scale down
                    sec
                    sbc mvscal,X        ;Change Scale
                    ifmi                ;Went past 0
                        sbc #$80            ;Stay with lin < 80
                        dec stsclh      ;Linear down by 1
                    endif
                    sta stscll
                else                ;High byte at target, check low
                    lda stscll
                    cmp #$10            ;... check linear
                    bcs ?at30
                    ;********************* Zoom in Achieved ****************************
                    jsr bumpgame        ;Start next game
                endif
            else                ;Flag says shrink down again
                lda stsclh      ;Shrink down to target size
                cmp #sttarh+2       ;At size??
                ifne                ;Not there yet
?at40               lda stscll
                    clc
                    adc mvscal,X        ;Change up
                    ifmi                ;Went past 80
                        sbc #$80
                        inc stsclh      ;Bin up by one
                    endif
                    sta stscll
                else                ;Bin is the same
                    lda stscll
                    cmp #$10
                    bcs ?at40           ;Add till wraps 0 to 5
                    lda #07
                    sta mzgame      	;We are out, take off
                    lda #$8F            ;Start stars and station moving
                    sta statst
                    lda #01
                    sta manstat     	;In case off in attract
                    lda #snd_passby
                    jmp dosound
                endif
            endif
        endif
        rts
            
;*******************************************
    .sbttl "Pick Picture"
;*******************************************
; Sets correct ship index in shppic
;*******************************************
picpic  ldx #plane_vpg         ;Stat, Page Select (Might be needed)
        lda shipst
        ifeq
?pp1        jmp pic4
        endif
        ifmi                    ;Blowing Up??
            ldx #plane_vpg+sparkle     ;Stat, Page Select, Sparkle
            bne ?pp1                ;And skip the rest
        endif
		lda vxstat				;exception for Maynard flyby 
		ifeq
			lda tact
			bne ?pic3rd     	;3rd person for Tact Display (mostly)
		endif
        lda mzgame
        tay                 ;Save Copy
        and #$C4            ;In 3rd Person Play??
        ;Above also is an escape for tunnel pic as nothing is needed for that
        ifne
?pic3rd    	lda animate				;If we are animating (wormhole, then show 3rd in tact (when $80)
			ifmi
				bit tact
				bpl pic2			;If animation is in wormhole, then we can tilt the ship
			endif
			lda #third          ;Always third person here
            sta shppic
            bne ?pp1            ;Skip it!
        else
			lda shipxh
            cmp #center
            ifcc                    ;It's on right
                txa
                ora #04             ;Flip Bit (60 to 61 or 68 to 69)
                tax
            endif
            tya                 ;Recall mzgame
            and #$0F                ;All these don't do this routine!!
            bne ?pp1
        endif
pic2    ldy #00             ;Guess Flat
        lda shpvel+1            ;Ship Velocity X
        ifmi
            bit lastflip            ;Already flipped
            ifmi                    ;If yes, hold til down to -5
                cmp #-3
                ifcc
pic3                ldy #picpos2-picpos ;Move to tilt left
                endif
            else                    ;Else, don't flip till at 10
                cmp #-10d               
                bcc pic3                ;Okay to flip
            endif
        else
            bit lastflip            ;Already flipped
            ifmi                    ;If yes, hold till under 5
                cmp #03
                ifcs
?pp10           	ldy #picpos3-picpos     ;Move to tilt right sequence
                endif
            else                    ;Else, wait till at 15
                cmp #10d
                bcs ?pp10               ;Time to flip
            endif
        endif
        tya                 ;Pic Base
        sta lastflip            ;If 0, we just cleared this flag
        ifne                    ;We flipped
            ldx #$80
            stx lastflip            ;so set flag
        endif
        lda shipxl
        sta temp2
        lda shipxh
        sta temp2+1         ;T
        asl temp2
        rol temp2+1         ;2T
        clc 
        lda shipxl
        adc temp2
        sta temp2
        lda shipxh
        adc temp2+1         ;2T+T
        sta temp2+1
        sec 
        lda temp2
        sbc #$80
        sta temp2
        lda temp2+1
        sbc #07             ;-((rtedge-4)*3/2)
        sta temp2+1         ;(2T+T)-180
        sta shppbs          ;Save for fire shot routine
        tya 
        clc                 ;Add in position select
        adc temp2+1         ;Add position select to tilt info
        tay 
        lda picpos,Y            ;Get proper picture from position
        sta shppic
        ldx pictil,Y            ;Get tilt info
pic4    lda #($E0+colwhite)     ;Ship color!
        ldy shipst          	;Blowing Up??
        beq ?pp20               ;Or dead??
        ifmi
?pp20       tya
            eor #$70                ;Dim out as blowing up
        endif
        sta temp8           ;Save color info here
        stx temp8+1         ;Save flip info here
        jsr vgadd2          ;Add Stat Instruction
        rts 

;Flat Ship Rotations         
picpos      .byte $01,$02,$03,$04,$05,$06,$06,$05,$04,$03,$02,$01


picpos2     .byte $0D,$0E,$0F,$10,$11,$0C,$0C,$0B,$0A,$09,$08,$07       ;Tilt Left
picpos3     .byte $07,$08,$09,$0A,$0B,$0C,$0C,$11,$10,$0F,$0E,$0D       ;Tilt Right

pictil      .byte plane_vpg,plane_vpg,plane_vpg,plane_vpg,plane_vpg,plane_vpg
            .byte plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip
			
            .byte plane_vpg,plane_vpg,plane_vpg,plane_vpg,plane_vpg
			.byte plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip
			
            .byte plane_vpg,plane_vpg,plane_vpg,plane_vpg,plane_vpg,plane_vpg
            .byte plane_vpg,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip,plane_vpg+xflip

;*****************************************
;* Used by 'trans' routine to do upward  
;* /downward transition on pictures from 
;* 1st to 3rd person and back.           
;*****************************************
pictran lda frame
        and #07         ;Change Time
        ifeq
            lda shppic      ;Will need to check for ends
            bit gamest      ;Up or Down??
            ifvc
                cmp #third      ;Made it to third yet??
                ifne
                    inc shppic
                endif
            else
                cmp #stthird        ;Make it back to first yet??
                ifne
                    dec shppic
                endif
            endif
        endif
        rts 
        
;***************************************************
    .sbttl "Ship Piece Explosions"
;***************************************************
;* If ship is exploding, used to move and output 
;* ship pieces.            
;***************************************************
pieces  bit shipst          ;Blowing up??
        ifpl                    ;Nope
            rts
        endif
        ldx #(nmexp/2)-1        ;Explosion Pieces
piece2  ;Entry for Base Ship Explosion
        begin
            stx temp9               ;Save a copy
            lda sxst,X          ;See if this peice active
            ifeq
                jmp ?spe10
            endif
            lda frame
            and rspeed,X            ;How fast to roll
            ifeq
                inc sxst,X          ;Next Pic
                lda sxst,X
                and #$17
                sta sxst,X          ;One of 8 pics and active bits
            endif
            ldy #00
            lda sxyv,X
            ifmi
                dey
            endif
            clc
            adc sxyl,X
            sta sxyl,X
            sta vdata+2
            tya 
            adc sxyh,X
            sta sxyh,X
            sta vdata+3
            ldy #00
            lda sxxv,X
            ifmi
                dey
            endif
            clc 
            adc sxxl,X
            sta sxxl,X
            sta vdata
            tya 
            adc sxxh,X
            sta sxxh,X
            sta vdata+1
            ;----------------------------------------------
            ;---------- Now Draw this peice ---------------
            ;----------------------------------------------
            jsr cor3p               ;Position and correct for 3rd person
            lda shpscl
            ldx shpsch          	;Current Ship Scale
            bit blowst          	;Station Blowing??
            ifmi                    ;Yep, Different scale
                lda blscll
                ldx blsclh
            endif
            inx 
            jsr vgadd2          	;Proper Scale Added
            lda shipst          	;We know this to be 80 or off
            eor #$FF
            and #$78
            asl a
            ora #colwhite           ;White
            bit blowst          	;Station (Again!!)??
            ifmi                    ;Yep!
				ldx isfinal				;Last station blows up White + Red
				ifeq
					eor #05             	;Normal stations blow up Green + Red
				endif
                bit rands+2         	;Bit a Random
                ifpl
                    ifvs                    ;Random !?!?!?!?!
                        lda #($F0+colred2)  	;Bright Red!
                    endif
                endif
            endif
            ldx #sexps_vpg         ;Stat Instruction
            jsr vgadd2          ;Add Color
            ;-------------- Which Piece is This? ---------------------
            ldx temp9               ;Recall X
            lda sxst,X
            and #07
            asl a
            clc 
            bit blowst          ;Doing Base Explosion??
            ifmi
                adc basepic,X
                tay 
                lda bxp0s,Y
                ldx bxp0s+1,Y
            else
                adc wpiece,X            ;Which Piece?
                tay                 	;Now points to the jsrl
                lda sxp0s,Y
                ldx sxp0s+1,Y           ;Have jsrl
            endif
            jsr vgadd2          ;Add Pic
?spe10      ldx temp9               ;Recall X
            dex
        miend               ;We are Done
        rts
        
;Rotation Speed of Each Piece   
rspeed  .byte $03,$01,$07,$03,$01,$07,$03,$01,$00,$01,$07,$03

;Which Picture
wpiece  .byte sxp0s-sxp0s       ;Piece 0 jsrl
        .byte sxp1s-sxp0s   
        .byte sxp2s-sxp0s
        .byte sxp3s-sxp0s
        .byte sxp1s-sxp0s
        .byte sxp3s-sxp0s
		
basepic .byte bxp0s-bxp0s
        .byte bxp1s-bxp0s
        .byte bxp1s-bxp0s
        .byte bxp0s-bxp0s
        .byte bxp1s-bxp0s
        .byte bxp0s-bxp0s
        .byte bxp0s-bxp0s
        .byte bxp1s-bxp0s
        .byte bxp0s-bxp0s
        .byte bxp0s-bxp0s
        .byte bxp1s-bxp0s
        .byte bxp1s-bxp0s
        

;************************************************
    .sbttl "Move Ship"
;************************************************
;* This Routine is for moving the ship from the *
;* rolly-gig. It also controls any special Y    *
;* Motion that may be needed.                   *
;************************************************       
moveship    
        bit shipst          ;Skip if Exploding
        ifmi
?ms49   	jmp ?msexit
		endif
		bit gamest
        bpl ?ms51
        ifvs
?ms51       jmp ?ms1                ;Take Off, Skip this
        endif
        ldy #00
		lda animate			;allow movement during wormhole
		ifpl
			lda mzgame
			and #$FA                ;So far games 4 and 0
			bne ?ms49
		else
			lda tact		;but not while still on tact screen
			bmi ?ms49
		endif

        lda mzgame
        cmp #04             ;Landing??
        ifeq                    ;Might be
            ;ldy #00
            lda rgdd                ;Any motion??
            ifne                    ;Skip this if not moving
                ifmi
                    dey
                endif
                clc 
                adc shpvel
                sta shpvel
                tya 
                adc shpvel+1
                sta shpvel+1
            endif
            ldy #00
            tax                 ;Save new velocity
            lda frame
            and #$1F
            ifeq
                txa
                ifne
                    ifmi
                        inc shpvel+1
                    else
                        dec shpvel+1
                    endif
                endif
            endif
            ;lda shpvel+1            ;Recall for below position add
        else
            lda rgdd                ;Rolly Gig data
            sta shpvel+1            ;For Tilt Info
        endif
		bit animate				;Slower limits for wormhole
		ifmi
			lda shpvel+1
			ifmi
				dey             ;Propagate Sign 
				cmp #-$11
				ifcc
					lda #-$11           ;Limit - Velocity
				endif
			else
				cmp #$11
				ifcs
					lda #$11            ;Limit + Velocity
				endif
			endif
		else
			lda shpvel+1
			ifmi
				dey             ;Propagate Sign 
				cmp #-$40
				ifcc
					lda #-$40           ;Limit - Velocity
				endif
			else
				cmp #$40
				ifcs
					lda #$40            ;Limit + Velocity
				endif
			endif
		endif
        sta shpvel+1            ;Resave velocity (in case limited)
        bit lasst               ;Laser Active??
        bmi ?ms2                ;yes, so don't move in X
        clc 
        adc shipxl          ;Add to position
        sta temp2          	;Save LSB
        tax                 ;Save LSB for later
        tya 
        adc shipxh
        tay                 ;Save MSB for real store later
        sta temp2+1         ;Hold LSB
		;Set player ship limits
		bit animate
		ifmi
			lda #3				;left limit
			sta temp3
			lda #2
			sta temp3+1			;right range limit
		else
			lda #2				;left limit
			sta temp3
			lda #4
			sta temp3+1			;right range limit
		endif
        ;------------------ Now Do Limit Check --------------------
		sec 
		lda temp2               ;(rtedge/2)-2
		sbc #$80
		lda temp2+1
		sbc temp3				;Left limit from above
		ifmi
?ms2        lda #00
			sta shpvel          ;Stop Velocity on edges
			sta shpvel+1
			beq ?ms1            ;And Skip this mess
		endif
		cmp temp3+1         ;Right Range Check from above
		bcs ?ms2            ;Don't go to 4 (3.FF is max)
		;Save actual X ship position here
        tya                 ;Recall 'real' Position
        sta shipxh
        txa                 ;Recall LSB
        sta shipxl
		;Ship Y motion now
?ms1    lda mzgame
        and #04
        ifne                    ;Game 4, move down slowly
			ldx dif4mz
			lda landspeed,X
            sta temp3           ;Save this
            lda shipyl
            bit gamest          ;Take off or land??
            ifvc                ;Land
#IF DISABLE_FASTTRANSITIONS = 0
                ldx #00
                stx gofast          ;Clear Fast Flag too
#ENDIF
                clc
                adc temp3               ;Add in velocity
                tax
                lda shipyh
                adc #00
                ifpl
?ms5                stx shipyl
                    sta shipyh          ;Move Y a little
					jsr checkwoo
                endif
            else                ;Taking Off!
                sec 
                sbc #$10
                tax 
                lda shipyh
                sbc #00
                cmp #03         ;At top??
                bne ?ms5        ;Okay to store
                lda #01
                sta mzgame      ;Blow Ship Time
                lda #08
                sta spcspd      ;Start with slow stars
                lda #$40
                sta blowst
                lda #$80
                sta rearview    ;Start Stars moving backwards
                lda #$0C
                sta statyl      ;Position of Station
                sta statyh      ;Init Position of Station
                lda #05
                sta stroyh
                sta stroyl
                lda #$70
                sta stsclh      ;Initial Station Scale
                ldx #maxstr-1
                lda #00
                sta statst      ;Station off for now
                sta stbflg      ;And no motion
                sta stscll      ;Initial Scale of Station
                sta nxtexp      ;Init exp Index
                begin
                    sta strflg,X
                    sta strxh,X
                    sta stryh,X     ;Clear old stars
                    dex
                miend
            endif
        endif
?msexit jsr rgdr                ;Scroll down rgdd data
        ;********** Fall Through *********************
		;*********************************************
			.sbttl "Output Ship to Screen"
		;*********************************************
		;* Add ship picture at proper size to vglist 
		;*********************************************
shipdis lda blowst
        and #$10            ;Station if in front of us
        bne ?oss1           ;Just leave then
        lda shipst          ;Ship active??
        ifeq                
?oss1       rts                 ;Nope!
        endif
        ;************** Place Ship Now! **************
        ldx #03
		begin
			lda shipxl,X
			sta vdata,X
			dex 
        miend
        lda mzgame
        ifne
            jsr cor3p		;Correct for 3rd Persion view
        else
            lda #00
            sta scalef
            sta scalef+1
            jsr posvec
            ifmi
                rts                 ;Not Drawn
            endif
        endif
        lda shipst          ;Blowing up?
        ifmi
            cmp #$D0                ;Do First part Fast
            iflt
                lda shipst
                adc #03
                sta shipst
            else
                lda frame
                and #01
                ifeq
                    inc shipst
                    inc shipst      ;Slow down at end
                endif
            endif
            lda shipst          ;Set Sign just in case
            ifpl                    ;It's Done!
                lda #00
                sta manstat         ;For now, you die!
                sta shipst
            endif
        endif
shipdis2    
        ldx shpsch
        lda shpscl          ;Get Ship's Scale
        inx 
        jsr vgadd2          ;Scale for ship
        lda #00
        sta vdata
        sta vdata+1
        sta vdata+3         ;Correction vector for bottom of ship
        lda #$70
        sta vdata+2
        jsr vgvtr2
		;*************************************************
		lda vxstat				;Ship hidden for all vxstat minus states except for when it is at $40 even 						
		ifmi					;              which is for Maynard landing and vxinit only (maynard flyby)
			and #$40
			cmp #$40
			bne ?noship
			lda hwstat
			bne ?noship
		endif
		;cmp #$60			;Ship hidden for  $40 - Vaxx Bonus = 1
							;                 $20 - Homeworld Animation = 1
							;                 $20 - End Story = 0
							;       		  $60 - Hidden
		;beq ?noship			;no ship when the final story is scrolling (bonus countup is done)
		;*************************************************
		jsr picpic          ;Pick correct picture 
		bit mzgame
        ifvs                ;In tube_vpg??
            ldy #(tubepic*2)	;Tube pic only
        else
            lda shppic
            asl a
            tay
        endif   
        lda shipst          ;Blowing up??
        ifmi
            cmp #$D0                ;Only 5 pics
            ifcc
                and #$F0
                lsr A
                lsr A
                lsr A
                sec 
                sbc #$10
                pha 
                jsr addsexp         ;Left Half
                lda temp8+1
                eor #04
                tax 
                lda temp8
                jsr vgadd2
                pla 
                jsr addsexp
            endif
        else
            lda planes-2,Y          ;-2 -- No Plane 0
            ldx planes+1-2,Y        ;From Source
            jsr vgadd2          	;Add ship picture
        endif
        lda shpscl
        ldx shpsch
        cpx #$71                ;Too Large
        ifcc                    ;yes
            rts                 ;Just Skip it!
        endif
        jsr vgadd2
?noship lda tact                ;Tact display on??
        ifne                    ;yes, either $80 or $40
			;cmp #$40
			;beq ?dtact				;tact has control still
			;ifne
				lda isfinal
				ifne
					bit vxstat
					bpl ?dtact					;Final level but not at end if this is positive, just do tact then
					ifvc
						;Bonus not done yet
						jsr stpg5
						jsr dovxbonus			;special stuff 
						jsr stpg0
					else
						;Bonus is done, is animation done?
						lda vxstat
						and #$20
						ifeq
							;Do homeworld animation sequence
							jsr stpg5
							jsr dohwani
							jsr stpg0
						else
							;Animation done, is story done?
							; lda vxstat
							; and #$10
							; ifne
								; ;set ship to center
								; lda #$84
								; sta shipxl      ;Postion Ship
								; lda #$04
								; sta shipxh
								; lda #$24
								; sta shipyl      ;Place Y Position
								; lda #$08
								; sta shipyh 
								; jmp ?dtact		
							; endif
							jsr endstory			;Do final story
						endif
					endif
				else
?dtact				jsr stpg5
					jsr disptact              ;ON PAGED ROM!!
					jsr stpg0
				endif
			;endif
        endif
        lda #00             ;Just restore normal STAT
        ldx #$60
        jsr vgadd2
        jmp pieces          ;Add pieces if necessary

checkwoo
		lda shipyl
		ifeq			;see if time for a random woo-hoo
			lda shipyh
			cmp #08
			ifeq
				lda retime
				cmp #$10			;Reactor time must be less than 10 count
				ifcc
					lda rands			;Only does woo-hoo chance 1 in 2
					ifpl
						lda #sp_rexwoohoo
						jsr dosound
					endif
				endif
			endif
		endif
		rts
;*********************************************************
;Landing on Space Station downward speed.... 
;These two tables are inherently linked so keep them
;in sync unless you want landing sequences to start 
;looking wonky.
;
; These are called from different code areas too which
; makes it even more difficult to track their interop.
;*********************************************************
; landspeed base is 5 and increases
landspeed
		.db 5,6,7,8			;Level 1-4
		.db 8,9,10,11		;Level 5-8
		.db 11,12,13,13		;Level 9-12
		.db 14,14,14,15		;Level 13-16
		.db 15,15,16,16		;Level 17-20
		.db 17,17,17,17		;Final Station
		.db 9,11,14,14		;Hidden levels
	
; station speed is -7 and decreases	
statspeed
		.db -7,-8,-9,-10 		;Level 1-4
		.db -10,-11,-12,-13		;Level 5-8
		.db -13,-14,-15,-16		;Level 9-12
		.db -16,-17,-18,-19		;Level 13-16
		.db -19,-19,-19,-19		;Level 17-20
		.db -19,-19,-19,-19		;Final Station
		.db -11,-13,-16,-16		;Hidden levels
		
;*********************************************
;* Ship Explosion Pictures (only 5)
;*********************************************        
addsexp tay                
        lda sexps,Y
        ldx sexps+1,Y
        jmp vgadd2       
        
;***********************************************
    .sbttl "Blow Up Ship (Start Sequence)"
;***********************************************
blowship
        txa 
        pha             ;Save X Reg
        jsr dodelay
        lda #snd_c1
        jsr dosound
        lda #$80
        sta shipst      ;Start Explosion
        ldx #nmexp-1
        begin
            jsr getrand     ;Random Start Spin
            and #07
            ora #$10
            sta sxst,X      ;Turn on a piece
            lda shipxl
            sta sxxl,X
            lda shipxh
            sta sxxh,X
            lda shipyl
            sta sxyl,X
            lda shipyh
            sta sxyh,X      ;Position Pieces
            jsr getrand
            and #07
            jsr signc           ;Get Sign
            sta sxxv,X  
            jsr getrand
            and #$1F
            bit blowst
            ifpl
                and #07
            endif
            jsr signc           ;Get Sign
            sta sxyv,X      ;Start it moving
            dex
        miend
        pla 
        tax             ;Restore X
        rts 
        
signc   pha             ;Save this A
        jsr getrand     ;Random Direction
        ifmi
            pla
            jmp neg
            pha         ;***************** Remove this later *************** JA
        endif
        pla
        rts
            
;*********************************************************************************************
    .sbttl "Shoot Out Tube"
;*********************************************************************************************
;* The routine will sequence the tube to allow the player to shoot down it! Uses the table 
;* of JSRL's in TWROM module & tcount as which of 32 sequences to use.                     
;*********************************************************************************************
tube    bit tstat
        ifpl                ;Launch on hold
            bit mzgame
            ifvs                ;Only check this if in lauch mode
                ;Want the man to stand for a bit, then run to ship
                lda velxh
                bne ?tu10           ;This is already done
                lda frame
                and #$3F
                ifeq
                    lda #$20			;Man will move up and to the right like this
                    sta velxh
                    lda #05
                    sta velyh
                endif
            endif
			;Tube sequence stuff
			lda frame
			and #$07
			ifeq
				inc tcolsq
			endif
            jmp ?tu10           ;Always
        endif
        bit tcount
        ifpl
            dec tcount
            ifmi                ;Done here
#IF (LEVEL_EDITOR != 0)
				lda #0
#ELSE
                lda tactde      ;Will tell us if....
#ENDIF
                sta tact        ;Must do tact display
                ifmi
                    lda maznum
                    sec
                    sbc #01
                    and #03
                    sta seqp            ;Init Pic
                endif
                lda #00
                sta seqst           ;Clear Status
                sta seqx            ;Start Over
                lda #$20            ;Out of tube_vpg, into transition
                sta mzgame
                lda #$3F
                sta mtim
                lda #snd_passby
                jmp dosound
            endif
?tu10       lda tcount
            cmp #$20            ;In Extension??
            ifcs
                and #$03
                adc #$1A            ;Add 1B (carry is set)
            endif
            tay             ;Set Sign bit
            ifpl                ;- is inactive
				pha             ;Sequence number of 4 - Save this
				bit tstat
				ifpl
					bit gamest
					bpl ?norm
					pla 			;don't need this, throw it away
					jsr ?specialtube
				else
?norm				lda #00
					ldx #$72
					jsr vgadd2      ;Set Scale
					lda #($F0+colblue)
					ldx #tube_vpg   ;Stat, page select
					jsr vgadd2      ;Add Color
					pla             ;Sequence #
					asl a           ;For words
					tay
					lda movet,Y
					ldx movet+1,Y
					jsr vgadd2
				endif
            endif
        endif
        bit tstat           ;Skip if moving
        ifpl
            ldx player
            lda lives,X     ;Number of lives
            sec
            sbc #01         ;Count the one we have
            ifne
                ifpl
                    cmp #05
                    ifcs
                        lda #04         ;Show Only 4 Please
                    endif
                    ifne
                        sta temp8
                        lda #-4
                        sta vdata+1
                        lda #$15
                        sta vdata           ;Initial Position
                        lda #-3
                        sta vdata+3
                        lda #$49
                        sta vdata+2
                        lda #00
                        sta vgbrit
                        lda #mancol+$E0     ;Set up Color for all these
                        ldx player          ;Player 2??
                        ifne                ;Yes, change color
                            lda #mancol2+$A0       
                        endif
                        ldx #mpic_vpg      	;Stat, page select
                        jsr vgadd2
                        lda #$98            ;Add Scale Instruction
                        ldx #$72
                        jsr vgadd2      	;Man's Scale
                        begin           	;Output Lives
                            jsr vgcntr
                            jsr vgvtr2      	;Position for this one
                            lda #idx_pic27     	;Arms crossed pic
                            jsr picout      	;Do this one
                            lda vdata
                            sec
                            sbc #$58
                            sta vdata
                            ifcc
                                dec vdata+1
                            endif           	;Position for possible next one
                            lda vdata+2
                            sec
                            sbc #$0D
                            sta vdata+2
                            ifcc
                                dec vdata+3
                            endif
                            dec temp8
                        eqend
                    endif
                endif
            endif
        endif
        rts

?specialtube
		lda #00
		ldx #$72
		jsr vgadd2      ;Set Scale
		lda #07
		sta perm1
		begin
			lda tcolsq
			and #07
			cmp perm1
			ifeq
				lda #($B0+colgreen)
				ldx #tube_vpg   ;Stat, page select
				jsr vgadd2      ;Add Color
			else
				lda #($F0+colblue)
				ldx #tube_vpg   ;Stat, page select
				jsr vgadd2      ;Add Color
			endif

			lda perm1
			asl a           ;For words
			tay	
			lda tritable,Y
			ldx tritable+1,Y
			jsr vgadd2
			dec perm1
		eqend
		rts
		
;---------------------------------------
; Launch Tube Triangle pics
;---------------------------------------
tritable
	jsrl(trix7)
	jsrl(trix6)
	jsrl(trix5)
	jsrl(trix4)
	jsrl(trix3)
	jsrl(trix2)
	jsrl(trix1)
	jsrl(trix0)


        
;***********************************************
    .sbttl "Space Station Picture and Motion"
;***********************************************
;* The routine moves and places the space      *
;* station. One of 4 space station pics are    *
;* used.                                       *
;*                                             *
;* Inputs: stat[xl,xh,yl,yh,st]                *
;*         mzgame                              *
;***********************************************
statot  lda statst          ;Station Active??
        ifpl                    ;nope
?ssp1       rts                 ;Just skip this then
        endif
        lda mzgame
        cmp #08             ;Skip on transition
		beq ?ssp1
        and #$EF            ;Other case is 3rd person views
        ifeq                ;Third person??
            jmp statfb          ;Do first person views
        endif
        ;Is Third Person View, Set Landing Sight Pic
		lda statst          ;Recall Station Status
        ldy #00             ;Sign Extend
        and #$7F                ;Drop Active, pick up speed
        cmp #$40                ;7 bit signed number
        ifcs
            ora #$80                ;Prop Sign
            dey
        endif
        clc 
        adc statyl
        sta statyl
        sta vdata+2         ;For Output
        tya 
        adc statyh
        sta statyh          ;New Position
        sta vdata+3         ;For Output
        cmp #$0C
        ifcs
            lda blowst          ;Blowing Up?
            ifeq                    ;Nope
                tya                 ;Moving Down??
                ifpl                    ;yep
                    lda #$80
                    sta statst          ;Station Status to top
                endif
                jmp ?ssp1
            endif
        endif
        tay                 ;Save MSB
        ;----------------- Set Landing Sight Position ----------------------------
        lda statxl
        sta landsl
        lda statxh
        sta landsh
        ;----------------- Sight Uses Same Y as Base Ship ------------------------
        lda statxl
        sta vdata               ;Save for output
        sec 
        sbc shipxl          ;Get ship relative to station
        sta temp1           ;Save in temp for possible negate
        lda statxh
        sta vdata+1         ;For station output later
        sbc shipxh
        sta temp1+1
        ifmi                    ;Want absolute value
            jsr dblneg          ;Negative temp1
        endif
        ;Returns A = temp1+1 = MSB of difference
        ldx mzgame          ;Skip this if not needed
        cpx #04
        ifeq
            bit shipst          ;We can skip this if blowing up
            ifpl
                ;ldx maznum          
				ldx maztype			;Which Maze??
                cmp mazxsh,X        ;Chech MSB of maze size
                bcc ?ssp10          ;Okay, we are above the maze....
                ifeq                ;If it's the same here
                    lda temp1           ;...else check LSB amount over
                    cmp mazxsl,X        ;Additional LSB size
                    bcs ?ssp30          ;Not above here either
                    ; We are above the maze here... Now check Y level for landing
?ssp10              jsr ychk
                    ; Above returns A= MSB difference, temp1 = LSB difference
                    ; Also, temmp1+1 = MSB
                    ifmi                    ;Landing Level achieved?
						;Now must recheck X position to see if we landing good or bad
                        jsr landit
                    endif
                else                    ;Not above base ship
?ssp30              jsr ychk                ;Do we need to shoot him?
                    ifmi                    ;He is past!, Shoot the sucker.
                        lda shipst          ;Open only if ship active
                        ifne
                            ifpl
                                lda #02
                                ldx #00         ;Guess Gun 0
                                ldy shipxh      ;Which Side??
                                dey
                                dey 
                                dey             ;Cheap way to see which side of 3   
                                ifpl    
                                    inx
                                endif
                                ldy gunctl,X            ;Already opening?
                                ifeq                    ;no, okay to open
                                    sta gunctl,X            ;Open this gun
                                    lda #snd_h3
                                    jsr dosound
                                endif
                            endif
                        endif
                    endif
                endif
            endif
        endif
        lda mzgame          ;Blow up sequence?
        cmp #01
        ifeq
            lda blowst          ;Blowing Up?
            and #$18
            ifne
                lda #00
                sec 
                sbc vdata+2
                sta vdata+2
                lda #00
                sbc vdata+3
                sta vdata+3         ;Negate, Y runs wrong direction
                jsr cor33           ;Complete Correction
                jsr cor             ;Place (Output)
                lda stscll          ;Set new size
                ldx stsclh
                inx 
                jsr vgadd2
                jmp ?fpbase          ;And draw first person
            endif
            rts
        endif
		lda isfinal
        ifne
			;Final Station - White
            lda #$F0+colwhite
        else
            ;Normal color - Green
            lda #$F0+colgreen
        endif
        ldx #st_vpg             ;Stat, page select
        jsr vgadd2              ;Add color
        lda stscll
        sec 
        sbc #$40
        lda stsclh
        sbc #$71                ;Is it under71 40??
        ifmi                    ;yep
            lda #-1
            sta vdata+1         ;Position Line!
            jsr cor3p
            lda #00
            ldx #$71
            jsr vgadd2          ;Always scale 0
            laljsr(longline)
            lxhjsr(longline)            ;Just draw a long line
        else
            jsr cor3p               ;Correct for 3rd person and draw
			lda stscll
			ldx stsclh
			inx 
            jsr vgadd2          ;Scale of base
			;For final Cube station, we need to change scale a little for landing pics
			lda isfinal
			ifne
				;Rescale to largest
				lda #00
				ldx #$71
				jsr vgadd2          ;Always scale 0
				lda #04
				bne ?fss
			endif
            ;lda maznum
			lda maztype
?fss        asl a
            tay 
            lda bases,Y
            ldx bases+1,Y
        endif
        jsr vgadd2          ;Draw pic and return
        ;******************* Now Draw Landing Sight *************************8
        ldy #00
        lda mzgame          ;Flag for zooming check
        cmp #02             ;Zoom?
        ifeq
            dey
        endif
        sty temp3               ;Save Flag
        lda widthl
        sta temp1
        lda widthh
        lsr A               ;Need half of width later
        sta temp1+1         ;Will be used later
        ror temp1
        lda landsl
        ldx landsh          ;Use normal sight
        ldy mzgame          ;Zooming??
        cpy #02
        ifeq                    ;yes.... so ...
            lda shipxl
            ldx shipxh          ;Use ships position
        endif
        sta vdata               
        stx vdata+1         ;Store X Position
        jsr cor3p2          ;Third person correction and draw (xonly)
        jsr sclst3          ;Restore scale to platform size
        lda #00
        sta vdata+2
        sta vdata+3         ;Now draw to left of beacon
        jsr dblneg          ;Want vector to the left
        sta vdata+1         ;MSB 1/2 width
        lda temp1
        sta vdata           ;1/2 width
        jsr vgvtr2          ;Position to left beacon
        jsr sclst2          ;Scale of beacon set
        ldy #00             ;Beacon offset amount
        lda #colred2        ;Red color
        jsr beacon
        jsr sclst3          ;Reset Scale
        lda widthl
        sta vdata
        lda widthh
        sta vdata+1         ;Now draw a line to other beacon
        lda frame               
        rol A
        rol A
        rol A
        and #$F0
        ora #whtlin         ;Color of landing platform
        ldx #$60
        jsr vgadd2
        lda #$20            ;Vector intensity
        sta vgbrit
        jsr vgvtr2
        jsr sclst2          ;Scale of this beacon
        ldy #$18            ;Beacon Number
        lda #colred2        ;Red Color
        jsr beacon
        jmp bguns
        
;*************************************************
    .sbttl "First Person Motion and Picture"
;*************************************************
;* Space Station 1st Person Routine              
;*                                               
;* Uses:    A,X,Y                                
;* Stack:   2 bytes                              
;*************************************************
statfb  lda lasst               ;Laser On?
        ifeq                    ;Skip if laser on
            lda #$20
            bit stbflg          ;H Motion
            ifne
                lda #00
                sta temp1+1         ;H Speed
                lda #$1F
                ;ldy maznum
				ldy maztype
                cpy #spacefort
                ifeq                    ;Space Fort uses it's own speed
?ownv               lda saucvl
				else
					lda scflags			;So does Star Castle
					bne ?ownv
                endif
                sta temp1
                lda #$10                ;Check this bit
                bit stbflg          ;Left or Right?
                ifeq
                    jsr dblneg          ;Left
                endif
                clc
                lda temp1               ;LSB Speed
                adc statxl
                sta statxl
                tay                 ;Save copy
                lda statxh
                adc temp1+1
                sta statxh          ;MSB Add
            endif
        endif
        bit stbflg          ;Vertical Motion Hold?
        ifmi_                    ;No... Do motion
            ldx mzgame          ;If not space, skip the hold motion
            ifeq
				ldx lasst           ;Laser Active??
				bne ?stb5           ;If yes, skip motion
			endif
			ldy #$18            ;Fly by speed
            ;lda maznum          ;Space fort game??
			lda maztype
            cmp #spacefort
            ifeq
                lda mzgame          ;But only in space
                ifeq
?ownv2              lda saucvl
                    lsr A
                    tay
                endif
			else
				lda mzgame          ;But only in space
                ifeq
					lda scflags			;So does Star Castle
					cmp #sc_state_fight
					beq ?ownv2
				endif
            endif
            sty temp1
            ldy #00         ;Guess this speed (moving down screen)
            sty temp1+1
            ifvs                ;VC is move
                jsr dblneg      ;Moving the other way
            endif
            clc
            lda statyl
            adc temp1
            sta statyl
            tax             ;Save copy LSB
            lda statyh
            adc temp1+1
            sta statyh
?stb5       ifmi                ;If - , went off top of screen
                lda #00
                sta statst      ;Turn it off for now
                sta stbflg      ;Also clear flag too
                sta statyh
                sta statyl      ;Stick at 0
                rts             ;No need to draw it either
            else
				lda vxstat
				ifeq
					lda statyh      ;In case we lost it
					cmp #$0B        ;See if it hit bottom
					ifcs
						lda #08
						ldy shipst      ;We alive?
						ifeq
							lda #$10            ;If not go here
						endif
						sta mzgame      ;Next game
						lda #stthird
						sta shppic      ;First trans pic
	;----------------- 3rd Person Conversion ------------------------------------
						lda #$B8
						sta shipyl
						lda #$0B
						sta shipyh
					endif
				else
					lda statyh
					cmp #08
					ifcs
						lda #$08
						sta statyh
						lda #$00
						sta statyl
					endif
				endif
            endif
        endif
        bit stbflg          ;Moving up screen?
        ifvs                    ;Yes
            cmp #04             ;Launch Fighter Point
            ifeq                    ;yep
                ldy #$80                ;Okay to launch fighters
                sty lauen
            endif
        endif
        ldx #03
        begin
            lda statxl,X
            sta vdata,X         ;Put position into vdata
            dex
        miend
        ;ldx maznum          ;Scale Control
		ldx maztype
		lda vxstat
		ifne
			ldx #4			;Force scale correct for Maynard
		endif
        lda sclsb,X         ;Correction LSB
        sta scalef
        lda scmsb,X
        sta scalef+1        ;Size offset
        jsr posvec          ;Ok mark, Place this and set scale
        ifmi
            rts                 ;Skip if not drawn
        endif
        ;Fall through
?fpbase ;Entry from 3rd person special view for blowship!!
        ldy #($F0+colgreen)   ;Default Green
        lda isfinal
        ifne
			ldy #($F0+colwhite)   ;Second default: Final Space Station WHITE
			lda vxstat
			ifne
				ldy #($F0+colgreen) 
			endif
        endif
        lda blowst
        and #08             ;Blowing up?
        ifne
            ldy #($F0+colwhiter)     ;then White!
        endif
        tya
        ldx #bas_vpg        ;Stat, page select (other base stations)
        jsr vgadd2          ;Add stat
        lda isfinal
        ifne
			lda vxstat
			ifne
				lda #05				;Maynard it!
			else
				lda #04				;Cube it!
			endif
        else  
			lda maztype			;Show correct station
        endif
        asl A
        tay 
        lda fbase,Y         ;Get the pic
        ldx fbase+1,Y
        jsr vgadd2          ;Draw it!!
		;if in star castle mode, draw the castle...
		lda scflags
		ifne
			jsr stpg3
			jsr drawshield			;Draw the shield for the station if it is enabled
			jsr stpg0
		endif
		rts

;*********************************************************
;* ychk - Used to calculate differnce between ship and   *
;*        station. Used more than once.                  *
;*********************************************************
    .sbttl "Y Difference Check"
;*********************************************************
ychk    lda statyl          ;Y LSB saved above (statyl)
        sec 
        sbc shipyl
        sta temp1               ;For abs possibility
        tya 
        sbc shipyh
        sta temp1+1
        rts 
        
;*********************************************************
;*  Landit - Called above to check for a good landing    *
;*                                                       *
;*  (width/2)-(shipsize/2)-ABS(difference) > 0 ??        *
;*  (width-size-2*dif) > 0 ??                            *
;*  shipsize+(2*diff)-width < 0 ??                       *
;*********************************************************

shpszl  =   $38
shpszh  =   $0

landit  lda shipst          
        ifne
            ifpl                    ;Not blowing up or dead
                lda #$80
                sta statst          ;Stop Motion
                lda shipxl          ;Ship/Station Diff saved above
                sec
                sbc landsl          ;Landing point
                sta temp1
                lda shipxh
                sbc landsh          ;MSB Landing Point
                sta temp1+1
                ifmi
                    jsr dblneg          ;Abs value
                endif
                
            ; (A) = MSB Difference
            ; Above checks with center, so must check against half the
            ; width... To do this I will multiply the difference by 2
            ; (instead of dividing the width by 2).
                asl temp1               ;LSB * 2
                rol A               ;MSB * 2 prop carry
                sta temp1+1         ;2* difference (MSB)
                clc
                lda temp1
                adc #shpszl         ;LSB of ship size
                sta temp1
                lda temp1+1
                adc #shpszh         ;MSB of size
                sta temp1+1
            ; (2 * ABS(diff))+shipsize in temp1(2)
                lda temp1
                sec 
                sbc widthl          ;Compare to the width
                lda temp1+1
                sbc widthh          ;Check MSB width
                ifcc
                    lda #02
                    sta mzgame          ;Set game play to zoom
                    lda #snd_h1         
                    jsr dosound
                    lda #$8A
                    sta mtim
                else                    ;Bad landing... BOOM!
?ldi30              lda #tiltpic
                    sta shppic
                    jsr blowship            ;Into peices
                    ldx #$0F                ;Fill color RAM with all white
                    lda #white          ;White
                    begin
                        sta colram+$10,X
                        dex
                    miend                   ;So ship stays white
                    jsr dostop          ;Stop and let it be done
                    lda #snd_kilhro     ;Stop any exit music too         
                    jsr dosound
                    lda #snd_h2
                    jsr dosound
                endif
            endif
        endif
        rts
        
    .sbttl "Third Person View Vector Correction"
;Places 0,0 in the upper left corner and scale 0 (I hope)   
cor3p   jsr cor3
cor     lda #00
        sta vgbrit
        jsr vgcntr          ;Center
        jsr sclset          ;Set 3rd Person Scale
        jmp vgvtr2          ;And draw it!
cor3p2  jsr cor32               ;Do X only
        jmp cor
cor3        lda #$80
        sec 
        sbc vdata+2         ;Y runs wrong direction
        sta vdata+2
        lda #05             ;Neg(x-5)
        sbc vdata+3
        sta vdata+3
        ;---------------- temp correction to put x to a.ff on screen ----------------------
cor33   cmp #$80
        ror A
        ror vdata+2
        cmp #$80
        ror A
        ror vdata+2
        sta vdata+3         ;y/4
cor32   lda vdata
        sec 
        sbc #$80
        sta vdata
        lda vdata+1
        sbc #center         
        sta vdata+1         ;Corrected X
        rts 
        
;***************************************************************
    .sbttl "Set Scales for Position"
;***************************************************************
;* Set scale routines for above.. saves 4 bytes each routine.
;***************************************************************
sclst2  lda #$60            ;Beacon Size
        ldx #$72            ;Scale for beacon
        jmp vgadd2
        
sclst3  ldx mzgame          ;Position during zoom
        cpx #02             ;zoom?
        ifeq                    ;Special scale for zoom in/out
            lda shpscl
            sec 
            sbc #$70                ;It to get larger
            ifmi
                sec 
                sbc #$80
                clc
            endif   
            tay 
            lda shpsch
            sbc #00         ;Gets larger
            cmp #$70
            ifcc                ;Can't do this!!
                ldy #00
                lda #$70            ;just stick at large
            endif
            tax 
            tya 
            inx 
            jmp vgadd2
        endif                   
sclset  lda #00             ;Else use normal draw
        ldx #$72
        jmp vgadd2          ;Scale for position
        
;****************************************************************
    .sbttl "Maze Guns"
;****************************************************************
;* This is used to display and move the guns on the top of the  *
;* mazes. They are used when a ship tries to pass off the edge  *
;* of the maze.                                                 *
;****************************************************************
bguns       lda mzgame
        cmp #02
        ifeq                    ;Skip during zoom
            rts 
        endif
        lda #01             ;Do 2 guns
        sta temp3
        begin
            lda statyl
            sta vdata+2         ;Gun Y position
            lda statyh
            sta vdata+3
            ldx temp3
            lda frame               ;Open close rate
            and #03
            ifeq
                lda gunctl,X            ;If 0, leave alone
                ifne
                    ifpl                    ;- but says close
                        clc 
                        adc #02
                        cmp #$0B                ;Open?
                        bcs ?bgu10          ;If yes, just skip this
                    else                    ;Else, close the thing
                        sec 
                        sbc #02
                        ifpl                    ;Closed already?
                            lda #00             ;Set back to normal
                        endif
                    endif
                    sta gunctl,X            ;Restore control
?bgu10          endif
            endif
            ;lda maznum
			lda maztype
            asl a               ;2 entries per maze
            tax 
            lda temp3
            ifne
                inx                 ;Use second entry
            endif
            lda statxl
            clc 
            adc gunxl,X
            sta vdata
            lda statxh
            adc gunxh,X
            sta vdata+1
            ldx temp3               ;Recall index
            ldy gunctl,X
            ifne                    ;This one is on
                cpx #01             ;Test Y for which gun
                ifeq
                    ldy #00             ;Move laser start over right
                    lda #$20
                else
                    ldy #-1             ;Else move it left
                    lda #-$20
                endif
                clc 
                adc vdata
                sta lsdsxl
                tya 
                adc vdata+1
                sta lsdsxh
                lda statyl          ;Origin correction
                sec 
                sbc #$30
                sta lsdsyl
                lda statyh
                sbc #00
                sta lsdsyh
            endif
            lda #00
            ldx #gun_vpg        ;Stat, page select
            jsr vgadd2          ;place gun normal stat
            jsr cor3p           ;correct for 3rd person
            lda #00
            ldx #$71
            jsr vgadd2          ;Scale of gun
            lda #$F1
            ldx #gun_vpg        ;stat, page select
            ldy temp3
            ifeq
                tay                 ;Save A
                txa 
                ora #xflip          ;Add flip
                tax
                tya                 ;restore A
            endif
            stx temp3+1         ;Save stat X for later
            jsr vgadd2          ;Add stat
            ldx temp3           ;Which gun
            lda gunctl,X        ;Gun control for this gun
            and #$7F            ;Drop control bit
            tay 
            pha                 ;Save copy of index
            lda heads,Y
            ldx heads+1,Y       ;Get proper head pic
            jsr vgadd2          
            ;lda #02
            ;ldx #00
            ;ldy #00
            ;jsr vgvtr5
			VGADD_VCTRS(0,2,hidden)
            lda #$FA
            ldx temp3+1         ;Recall flip for new color
            jsr vgadd2
            lda #$40
            ldx #$71
            jsr vgadd2          ;Scale of gun
            pla 
            sec 
            sbc #04             ;Delay open
            ifpl
                cmp #06
                ifcs                    ;If greater, always use 6
                    lda lasst               ;Laser On??
                    ifeq                    ;no
                        ldx #03
                        begin                   ;Fire laser
                            lda shipxl,X
                            sta lstsxl,X            ;destination
                            dex
                        miend
                        lda lstsyl
                        sec 
                        sbc #$80
                        sta lstsyl
                        lda lstsyh
                        sbc #00
                        sta lstsyh          ;Offset to center of ship
                        lda shipst          ;Ship Active??
                        ifne
                            ifpl
                                lda gunctl
                                ora gunctl+1            ;Assumed only one working
                                cmp #$0A                ;Open and ready to fire
                                ifeq
                                    lda #$EF                ;Fire!!!
                                    sta lasst               ;Turn it on
                                    lda #07
                                    sta laspd               ;Fast Laser
                                endif
                            endif
                        endif
                    endif
                    lda #06
                endif
                tay
                lda guns,Y
                ldx guns+1,Y
                jsr vgadd2
            endif
            dec temp3
        miend
        rts
        
;********************************************
    .sbttl "Bump Game"
;********************************************
;* Used to advance and set up the game for  *
;* the maze play portion from the space     *
;* portion.                                 *
;********************************************   
bumpgame    
        lda #$80
        sta vgrestart       ;Hold this picture
        sta ymot            ;Set man's postion at ship
        sta objyl
        jsr domaze          ;Draw whole maze 
        lda #-1             
        sta xmot+1
        lda #00
        sta ymot+1
        sta piccur          ;Make him run
        sta tspark
        sta nodraw          ;Clear these just in case
#IF DISABLE_HCOUNT = 0		
		sta hcount			;Clear our IRQ counter so the maze can load still too
#ENDIF
		ldx #nmdisc-1		;Clear any previous oxygen animations
		begin
			sta oxystat,X
			dex
		miend
        lda #$DA
        sta xmot
        lda #$FC
        sta mazeyh          ;Place man/maze on screen
        ;ldx maznum          ;Different vel for different mazes
		ldx maztype
        lda tossvel,X
        sta velxh           ;'throw' him out of space ship
        lda #$80
        sta objxl
        sta direct          ;Jump out of ship right direction
        lda #07
        sta objxh           ;Set his position to this spot
        
        lda #$82
        sta mzgame          ;Set to normal maze play 
        ; The 2 is so maze does not scroll right away (flag to motion)
        lda gamest
        ora #$18                ;Add just entering bit
        sta gamest
        lda #mazcol+$E0
        sta mapbuf+2            ;Turn on map
        lda #oxycol+$E0
        sta oxybuf+2            ;Turn on oxygen counter       
        bit atflag              ;Attract flag??
        ifmi
            lda #01
            sta manstat         ;Make sure on
        endif
        rts
            
tossvel .byte -$40,-$30,-$28,-$28  

        
;****************************
;* Table of base Ship Sizes 
;****************************
mazxsh  .byte 1,1,0,0           ;Maze Size MSB
mazxsl  .byte 4,2,$E2,$D8       ;LSB Size

;*****************************
;* Scale correction for each 
;* ship draw on 1st person.  
;*****************************
sclsb   .byte $7F,$40,$00,$00,$00
scmsb   .byte $00,$01,$00,$01,$03

;*****************************
;* Gun positions for bases.  
;*****************************
gunxl   .byte $48,$B8,$4A,$B6,$68,$98,$78,$88
gunxh   .byte -1,0,-1,0,-1,0,-1,0


;***********************************************
    .sbttl "Position a ship on the grid"
;***********************************************
;* Input:   vdata  = x lsb  (         )        
;*      	vdata+1= x msb  (unchanged)        
;*          vdata+2= y lsb  (         )        
;*          vdata+3= y msb  (         )        
;*                                             
;* Output:  Adds up to 20d bytes to vglist     
;*      z=1,m=0 Object Displayed on Grid   
;*          m=1,z=0 Object off grid            
;*                                             
;* Uses:    temp1,temp2,temp3,temp4,temp5      
;***********************************************
posvec  lda vdata+3
        cmp #$0B
        bcs offscreen
        lda vdata+1
        cmp #09
        bcs offscreen
        lda vdata+3
        cmp #$0A
        ifeq
            lda vdata+1
            cmp #02
            bcc offscreen
            cmp #07
            ifcs    
offscreen       lda #$FF            ;Vector out of bounds
                rts                 ;Return -1
            endif
        endif
posvc2  ldy #00
        lda #$40
        sta (vglist,Y)
        iny 
        lda #$80
        sta (vglist,Y)			;vcntr
        iny 
        lda #00
        sta (vglist,Y)
        iny 
        lda #$71
        sta (vglist,Y)			;scale
        iny 
        lda #00
        sta (vglist,Y)
        iny 
        lda #$60
        sta (vglist,Y)			;color black, page 0
        jsr vgadd               ;scal 0,0
        lda vdata+3
        asl a
        tay 
        sta temp5
        lda gridy,Y
        ldx gridy+1,Y
        jsr vgadd2          	;vctr, delta Y and opcode
        lda vdata+1
        asl a
        tay 
        lda vdata               ;Twice as above
        ifmi                    ;X units as Y
            iny 
        endif
        sty temp1               ;X bin
        asl temp1
        lda vdata+3
        asl a
        asl a
        sta temp2
        asl a
        asl a
        adc temp2
        ldx temp1
        cpx #$14
        ifcc
            adc temp1
            tay 
            lda gridx2,Y
            sbc gridx,Y
            sta temp4
            lda gridx2+1,Y
            sbc gridx+1,Y
            and #$1F
            sta temp4+1
            lda gridx,Y
            ldx gridx+1,Y
        else
            adc #$23
            sec 
            sbc temp1
            tay 
            sec 
            lda gridx,Y
            sbc gridx2,Y
            sta temp4
            lda gridx+1,Y
            sbc gridx2+1,Y
            and #$1F
            sta temp4+1
            lda #00
            sec 
            sbc gridx,Y
            pha 
            lda #00
            sbc gridx+1,Y
            and #$1F
            tax 
            pla 
        endif
        jsr vgadd2
        lda vdata+2
        lsr A
        lsr A
        ifeq
            lda vdata
            asl a
            beq ?psv10
            ldy #00
            sta temp1
            lda #00
            sec 
            sbc temp1
            sta (vglist,Y)
            iny 
            lda #$71
            sta (vglist,Y)
            iny 
            ldx vdata+3
            jmp ?psv5
        endif
        tax 
        ldy #00
        lda qrtlog-1,X
        sta (vglist,Y)
        iny 
        lda #00
        sec 
        sbc fullog2-1,X
        sta temp1
        lda #$71
        sta (vglist,Y)
        iny 
        ldx temp5
        lda disty,X
        sta (vglist,Y)
        iny 
        lda disty+1,X
        sta (vglist,Y)
        iny 
        lda temp4
        sta (vglist,Y)
        iny 
        lda temp4+1
        sta (vglist,Y)
        jsr vgadd
        lda vdata
        and #$7F
        ifne
            jsr multiply
            asl a
            rol temp2+1
            ldx #$71
            lda #00
            sec 
            sbc temp2+1
            ifne
                jsr vgadd2
                ldy #00
                ldx vdata+3
                inx 
?psv5               lda #00
                sta (vglist,Y)
                iny 
                lda #00
                sta (vglist,Y)
                iny 
                lda distx,X
                sta (vglist,Y)
                iny 
                lda #00
                sta (vglist,Y)
                jsr vgadd
            endif
        endif
?psv10  lda scalef+1
        cmp #$80
        ifeq
            lda vdata+3
            cmp #$0A
            ifcc
                sta temp1
                lda vdata+2
                lsr temp1
                ror A
                lsr A
                tay 
                lda #05
                sec 
                sbc temp1
                ora #$70
                tax 
                lda fullog,Y
            else
                lda #00
                ldx #$71
            endif
            jsr vgadd2
        else
            cmp #$40
            ifne
                clc 
                lda vdata+2
                adc scalef
                tay 
                lda vdata+3
                adc scalef+1
                clc 
                sta temp1
                ifmi
                    sec
                endif   
                tya 
                ror temp1
                ror A
                lsr A
                tax 
                ldy fullog,X
                lda #05
                sec 
                sbc temp1
                ifmi
                    lda #00
                    tay 
                endif
                cmp #06
                ifcs
                    lda #06
                endif
                clc 
                adc #01
                jsr vgscal				;a=binary scaling value, y = linear scaling value 
            endif
        endif
        lda #00
        rts 
        
distx   .byte $02,$03,$04,$06,$08,$0B,$10,$17,$20,$2D,$40,$5B
disty   .word $1FFA,$1FF7,$1FF4,$1FEE,$1FE7,$1FDD,$1FCE,$1FBA,$1F9D,$1F73,$1F39

gridx   .word $1FEE,$1FF0,$1FF2,$1FF4,$1FF6,$1FF8,$1FFA,$1FFC,$1FFE,$00
gridx2  .word $1FE7,$1FE9,$1FEC,$1FEF,$1FF2,$1FF5,$1FF8,$1FFA,$1FFD,$00
        .word $1FDC,$1FE0,$1FE4,$1FE8,$1FEC,$1FF0,$1FF4,$1FF8,$1FFC,$00
        .word $1FCD,$1FD3,$1FD8,$1FDE,$1FE4,$1FE9,$1FEF,$1FF5,$1FFA,$00
        .word $1FB8,$1FC0,$1FC8,$1FD0,$1FD8,$1FE0,$1FE8,$1FF0,$1FF8,$00
        .word $1F9A,$1FA5,$1FB1,$1FBC,$1FC7,$1FD3,$1FDE,$1FE9,$1FF5,$00
        .word $1F70,$1F80,$1F90,$1FA0,$1FB0,$1FC0,$1FD0,$1FE0,$1FF0,$00
        .word $1F34,$1F4B,$1F62,$1F78,$1F8F,$1FA5,$1FBC,$1FD3,$1FE9,$00
        .byte $E0,$1E,$00,$1F,$20,$1F,$40,$1F,$60,$1F,$80,$1F,$A0,$1F,$C0,$1F,$E0,$1F,$00,$00
        .byte $69,$1E,$96,$1E,$C3,$1E,$F0,$1E,$1E,$1F,$4B,$1F,$78,$1F,$A5,$1F,$D3,$1F,$00,$00
        .byte $C0,$1D,$00,$1E,$40,$1E,$80,$1E,$C0,$1E,$00,$1F,$40,$1F,$80,$1F,$C0,$1F,$00,$00       
        .byte $D1,$1C,$2C,$1D,$86,$1D,$E1,$1D,$3B,$1E,$96,$1E,$F0,$1E,$4B,$1F,$A5,$1F,$00,$00
        
gridy   vctrly(225) ;$00E1
        vctrly(219) ;$00DB
        vctrly(210) ;$00D2
        vctrly(198) ;$00C6
        vctrly(180) ;$00B4
        vctrly(155) ;$009B
        vctrly(120) ;$0078
        vctrly(70)  ;$0046
        vctrly(0)   ;$0000
        vctrly(-99) ;$1F9D
        vctrly(-240);$1F10
        vctrly(-439);$1E49





