;**************************************************************
    .TEXT ".TWMAZEO."
;**************************************************************
    .title "Reactor Routines"
    .sbttl "Reactor Color Control"
;**************************************************************
;* If reactor status is + (safe) then color pulses just       
;* yellow. If reactor status is - (ready to blow) then color  
;* is red/yellow pulses.                                      
;*                                                            
;* Uses: temp1          
;*
;* objst+zreactor status notes:
;*
;* Bit 80: Minus if touched and counting + if safe
;*     40: On Screen Bit
;*     20: Used with 80 above to signify touched and ready to blow up
;*                                 
;**************************************************************
react   lda frame           ;Continue to move intensity
        asl A
        asl A
        asl A
        ora #$20            ;Min brightness
        and #$F0
        sta reintn          ;Save intensity
        bit objst+zreactor      ;Status?
        ifne_
            ifpl                    ;Time countdown
                ;Stay yellow if OK
                lda #00
                sta rodstat         ;Rods stay in if safe
            else
                lda frame
                and #07
                ifeq                    ;Rod pull out time
                    lda rodstat         ;Need to move out rods?
                    cmp #06
                    ifcc                    ;Under 3, will move
                        inc rodstat         ;Next pic
                        inc rodstat         ;Next pic
                    endif
                endif
                bit gamest          ;Exit??
                ifvc                    ;Nope, Ok to count
                    lda manstat         ;Skip if dead or dying
                    ifpl
                        ifne
                            lda gamest
                            and #$10                ;Count hold??
                            ifeq                    ;Nope
                                lda sndcue+2            ;See if the last sounds was actually played yet
                                ifeq                    ;If zero, then it was played, do another, this keeps sounds from stacking up
                                    lda #$22                ;this is a time factor until this sound can be played again
                                    sta sndcue+2
                                    lda #snd_d2         ;Normal sound
                                    jsr dosound         ;Sound this thing off
                                endif
                                lda #06					;Reactor Tick Timer (original value was 7 ticks)
                                clc
                                adc gamedif
                                sta temp1               ;Amount to subtract from reactor LSB each frame
                                lda retime+1
                                sec
                                sbc temp1
                                sta retime+1            ;Reactor time LSB
                                ifcc
                                    sed                 ;DECIMAL MODE
                                    lda retime
                                    sec
                                    sbc #01
                                    sta retime          ;Reactor time
                                    cld                 ;END DECIMAL MODE
                                    ifeq                    ;Time to blow?
                                        lda #00
                                        sta retime+1        ;Reactor time LSB
                                        sta nxtexp          ;For in maze explosion
                                        lda #$80
                                        sta manstat
                                        sta tspark          ;Blow up so turn off drawing
                                        sta nodraw          ;Not sparkle draw either
                                        lda #$20
                                        sta objst+zreactor  ;Boom
                                    else
										;cmp #$15
										;ifeq
											;lda rands
											;and #0F				;Rex hurry only 1 in 16 chance
											;ifeq
											;	lda #sp_rexhurry
											;	jsr dosound         ;Rex says hurry
											;endif
										;else
											cmp #$10
											ifeq
												lda #sp_ten
												jsr dosound         ;Sound this thing off
											else
												ifcc
													clc
													adc #sp_one-1
													jsr dosound         ;Sound this thing off
												endif
											endif
										;endif
                                    endif
                                endif
                            endif
                        endif
                    endif
                endif
            endif
        endif
        lda vglist
        pha                 ;Save current vglist    
        lda vglist+1
        pha 
        lda # ubyte(retbufs)
        sta vglist+1
        lda # lbyte(retbufs)
        sta vglist
        lda #$A0                ;Counting or dead?
        bit objst+zreactor
        ifeq
            VGADD_JSR(char_space)   ;<space>
            VGADD_JSR(char_space)   ;<space>
            VGADD_JSR(char_o)       ;O
            VGADD_JSR(char_k)       ;K
            VGADD_JSR(char_space)   ;<space>
        else
            lda retime
            sta temp1
            lda #00
            sta temp1+1
            lda #temp1
            sec 
            ldy #02
            jsr digits
        endif
        pla 
        sta vglist+1
        pla 
        sta vglist          ;restore original vglist
        ;Fall Through to next routine
		;****************************************************
			.sbttl "Reactor Draw Routine"
		;****************************************************
		;* Will draw the reactor in the proper color if it  *
		;* is visible on the screen. Uses routine 'locate'  *
		;* to place the object.                             *
		;****************************************************
		lda tspark          ;Don't draw reactor while beaming
        bne ?rac1
        ldx #zreactor       ;Reactor number
        jsr locate          ;Put it on screen if ready
        tya 
        ifpl                    ;Ready
?rac1       jmp endre1
        endif
        lda objst+zreactor
        ifpl
            lda sndcue+1
            ifeq
                lda #$40
                sta sndcue+1
                lda #snd_b1d                ;Hmmmmmmmmmmmm
                jsr dosound
            endif
        endif
        lda #($F0+reacol)       ;Purple reactor??
        bit objst+zreactor
        ifmi
            lda #($C0+colyellow)    ;Yellow if set off
        endif
        ldx #body_vpg+xflip     ;Stat, page select, flip
        jsr vgadd2
		;Body Scale
		ldy #01
		lda #01
        jsr vgscal          	;Size for this object
		;**********************************************************
		; Lots of duplicate code below, but it will execute faster
		; this way, be sure to change in both places if modifying
		; reactor drawing routines. First section is for regular
		; reactor and second is for Mega-reactor
		;**********************************************************
		lda limbo+zreactor		;Check special flag for Reactor size
		ifeq					;Small Reactor
			VGADD_JSR(body)     	;Use Robot body for reactor
			;Reactor Rods
			lda #$0F
			bit objst+zreactor
			ifmi
				lda #rodcol
			endif
			ora reintn          	;Add intensity
			ldx #rods_vpg+xflip 	;Stat, page select, xflip on
			jsr vgadd2          	;Color for rods and button
			ldy rodstat         	;Rod status
			lda rods,Y
			ldx rods+1,Y
			jsr vgadd2          	;Add rods and button
			;Pyroid in center
			lda #re2col         	;Center pyroid color (orange)
			ora reintn          	;Color and intensity
			ldx #$60+sparkle+xflip  ;Stat, sparkle and flip
			bit objst+zreactor      ;Counting??
			ifmi
				ldx #$60+xflip          ;Turn on sparkle
			endif
			jsr vgadd2          	;Add 2 bytes to list
			;Pyroid Scaling
			ldy retime          	;Ball grows with time
			ifeq
				ldy #$B0                ;If not couting, hold at small
			endif
			lda #02					;Normal Binary Scaling		
			jsr vgscal	
			VGADD_JSR(sparkb)
		else
			VGADD_JSR(bigbody)		;Big Robot Body
			;Reactor Rods
			lda #$0F
			bit objst+zreactor
			ifmi
				lda #rodcol
			endif
			ora reintn          	;Add intensity
			ldx #rods_vpg+xflip 	;Stat, page select, xflip on
			jsr vgadd2          	;Color for rods and button
			ldy rodstat         	;Rod status
			lda bigrods,Y
			ldx bigrods+1,Y
			jsr vgadd2          	;Add rods and button
			;Pyroid in center
			lda #re2col         	;Center pyroid color (orange)
			ora reintn          	;Color and intensity
			ldx #$60+sparkle+xflip  ;Stat, sparkle and flip
			bit objst+zreactor      ;Counting??
			ifmi
				ldx #$60+xflip          ;Turn on sparkle
			endif
			jsr vgadd2          	;Add 2 bytes to list
			;Pyroid Scaling
			ldy retime          	;Ball grows with time
			ifeq
				ldy #$B0                ;If not couting, hold at small
			endif
			lda #00					;a bit bigger now
			jsr vgscal	
			VGADD_JSR(sparkb)
			;Draw something else when BIG so it is cooler
			;lda frame
			;and #07
			;tay
			;lda bigrods,Y
			;ldx bigrods+1,Y
			;jsr vgadd2          	;Add rods and button
		endif
        lda #colblack
        ldx #$60
        jsr vgadd2          ;Restore normal stat
        lda #$A3
        bit gamest          ;Game over??? (Attract?)
        ifpl                    ;yep
            lda #00
        endif
        ifne
            bit objst+zreactor	;reactor set off?
            ifmi
                lda dif4mz          ;wave 0
                ifeq
					VGADD_VCTRS(-40,-40,hidden)
                    ldx #mgetout
                    jsr msgnop_rtp2            ;Get out message
                endif
            endif
        endif
		;****************************************************
			.sbttl "Blow up from Within Check and Call"
		;****************************************************
endre1  lda objst+zreactor      ;Make sure reactor is not alive
        and #$BF                ;Drop screen bit
        cmp #01
        ifne
            lda retime          ;And time=0
            ifeq
                lda manstat         ;Man dying??
                and mzgame          ;And still in maze
                ifmi
                    jsr inblow          ;Blow this thing!
                endif
            endif
        endif
        rts

;****************************************************************
;* Max Background sounds
;****************************************************************
;* We will evaluate the max robots distance from rex
;* but only if there is at least one max defined on this level
;****************************************************************
maxsound
		lda gamest
		ifmi
			lda mzgame
			ifmi
				lda #-1
				sta temp2				;No max sounds by default
                ;-----------------------------------------------
                ;BUG #215 - Level 27 Maxoid Trip Pad Shots
                ;-----------------------------------------------
				;lda objst+zmax
				;ifne
					;loop through each max and get the lowest value
					ldx #zmax+nmmax-1
					begin					
						lda objst,X
						ifne
							ifpl				;max can actually die, so make sure he 'aint dead
								and #$10			;is this max active?
								ifne
									jsr maxpy			;check max/pyroid collision
									ifeq				;zero if max is still alive
										;X is max index, will be checked against Rex
										;*************************************************
										;I think these are irrelevant, take them out?
										;txa
										;tay
										;*************************************************
										jsr colchkx			;temp1 will have abs value of X distance between Rex and max 
										lda temp1
										sta temp3
										lda temp1+1
										sta temp3+1			;Save X distance in temp3
										jsr colchky			;temp1 will have abs value of Y distance between Rex and max 
										lda temp1
										adc temp3
										sta temp3
										lda temp1+1
										adc temp3+1
										sta temp3+1			;Sum of X and Y distances are now in temp3... 
										;now which is smaller, our running total or this max robot?
										lda temp2			;Has the highest 
										cmp temp3+1
										ifcs
											lda temp3+1
											sta temp2
										endif
									endif
								endif
							endif
						endif
						dex
						cpx #zmax-1
					eqend
				;endif
				lda temp2
				;temp2 has the value we need to send if it is different than last time
				cmp maxsnd			;is it different than what we sent last?
				ifne
					sta maxsnd
					;send it
					jsr setmaxv 		;Send A as Max sound param
				endif

			endif
		endif
		rts


;*******************************************************
; Max + Trip Pad Pyroid Collisions
; 
; When Max hits a Trip Pad Pyroid, he will take a hit
; 4 hits will kill max, pyroid always dies on hitting
; max tho.
;
; X contains current Max index
; Will return -1 in A if Max dies, zero otherwise
; Y will be used for Pyroid index
;*******************************************************
maxpy	;check each pyroid
		ldy #zfire+nmfire-1     ;Look at top 8 only (these are the only slots that TPP's will occupy)
		begin    
			lda objst,Y				;Fireball status
			ifpl
				and #$10				;Check to see if this is a trip pad pyroid (bit $10 is set if so)
				ifne
					jsr cmpxyx				;compare X
					lda temp1+1				
					ifeq					;MSB must be zero
						lda #$28				;Hardcoded X width of Max robots
						cmp temp1
						ifcs
							;Hit on X, now do Y
							jsr cmpxyy				;Compare Y
							lda temp1+1				;MSB must be zero
							ifeq
								lda #$48				;Hardcoded Y height of Max robots
								cmp temp1
								ifcs					;In same Y block as pyroid?
									;we need to save X in case we destroy it below 
									txa
									pha
									;max will take a hit
									lda maxdata-zmax,X
									and #$C0
									cmp #$C0			;is max at 3 hits already?
									ifeq					
										lda #$80			;Max will die
										sta objst,X
										lda rands+1
										and #1
										tax 				;We can destroy X here because we will restore before leaving
										lda _maxsnds2,X		;Max is unhappy/died
										jsr dosound
										lda #$90
										jsr bpont2          ;And 9000 points, this also destroys A and X
									else
										lda maxdata-zmax,X
										clc					;Add (1) to the hit count
										adc #$40
										sta maxdata-zmax,X
										lda rands+2
										and #1
										tax 				;We can destroy X here because we will restore before leaving
										lda _maxsnds1,X		;Max is injured
										jsr dosound
									endif
									
									;kill the pyroid, it makes a splash animation
									lda #$80
									sta objst,Y
									pla 				;Restore X
									tax
									jmp _maxex			;Exit now
								endif
							endif
						endif
					endif
				endif
			endif
			dey 
            cpy #zfire+8
		miend
_maxex	lda #0
		rts	

;max gets hit
_maxsnds1 	.db sp_maxahhhh,sp_maxohhhh
;max dies	
_maxsnds2	.db sp_maxbeback,sp_maxeeeeww

cmpxyx	sec
		lda objxl,X
		sbc objxl,Y				;Same X block as pyroid?
		sta temp1
		lda objxh,X
		sbc objxh,Y	
		sta temp1+1
		ifmi
			jsr dblneg          	;ABS of temp1
		endif
		rts
		
cmpxyy	sec
		lda objyl,X
		sbc objyl,Y 				;Same X block as pyroid?
		sta temp1
		lda objyh,X
		sbc objyh,Y
		sta temp1+1
		ifmi
			jsr dblneg          ;ABS of temp1
		endif
		rts
;************************************************
    .sbttl "Animate Discs"
;************************************************
anio2  	ldx #zdisc+nmdisc-1
		lda frame
		and #01
		ifne
			begin
				lda oxystat-zdisc,X
				ifne
					clc
					adc #01
					cmp #$08
					ifge
						lda #$80
						sta objxh,X     	;Kill this one
						lda #0				;For below save
					endif
					sta oxystat-zdisc,X		;Save new value for animation
				endif
				dex
				cpx #zdisc
			ccend
		endif
		rts
        
;************************************************
    .sbttl "Sparks Routine"
;************************************************
sparks  lda # lbyte(sparkb)
        sta vglist
        lda # ubyte(sparkb)
        sta vglist+1                ;Set up output sparkler
        ldy #nmsparks-1             ;Do all sparks
        sty temp3+1
        begin
            ldy temp3+1             ;Sparks left to do
            lda frame
            clc 
            asl A
            adc ofsets,Y                ;Group offsets, so that sparklets start at different time
            and #$0E                    ;Longest length possible
            sta temp1
            ifeq                        ;New vector
                ;jsr getrand
				lda rands,Y
                sta sparkangle,Y
            endif
            lda sparkangle,Y
            jsr sin                 ;Now use angle, get X and Y components
            jsr multiply
            sta temp6                   ;Save Y
            ldy temp3+1                 ;Recall Y
            lda sparkangle,Y            ;Recall angle
            jsr cos
            jsr multiply
            sta temp5               
            ;Now prepare to write vector
            ldx temp6
            ldy #$20                    ;Brightness
            jsr vgvtr
            ;Now reverse the vector
            lda #00
            sec 
            sbc temp6
            tax 
            lda #00
            sec 
            sbc temp5
            ldy #$20                    ;Brightness
            jsr vgvtr
            dec temp3+1
        miend
        jsr vgrtsl              ;Add an RTSL to the buffer
        rts
            
ofsets  .byte 0d,8d,16d,24d,36d,48d

;******************************************************************
    .sbttl "Position All Maze Objects"
;******************************************************************
;* Moves and controls all objects in
;* the maze.
;*                                     
;* Inputs: Object X,Y, Status, VelX & Y 
;*                                      
;* Output: Update position and status.  
;*                                      
;* Uses:   (see movthg)                 
;*         +2 additional bytes stack    
;*******************************************************************
;* objst notes:
;* 
;* objst is the main object status byte. There are some general
;* rules about this and then each object may use the status byte
;* in specific ways for that object. Each object objst is described
;* below with the code to move that object.
;*******************************************************************
;* General objst usage:
;*
;*  00 = Object is dead/not-active or otherwise ignored
;*  80 = Object is in the process of dying, it does not affect Rex
;*  40 = Object should be drawn if this is set, this I believe
;*       is a flag to save cpu time on drawing an object each time.
;*       It is called the 'Screen Flag' in other areas
;*  10 = Means that this is a 'temp object' which when checked against
;*       wall collisions, it will be set to destroyed objst = $80
;*
;*  01 or Greater = Active for drawing and collision detection
;*******************************************************************
;* Fireball/Spark objst Notes:
;*
;* Bit $10 
;*     Set: Fireball was launched from a Trip Pad (red dude) or IonCannon shot
;*     Clr: Just a normal old spark (white)
;*******************************************************************
posmo   lda tspark              ;Beaming, move nothing... woo hoo!
        ifne
            jmp ?drtran         	;Skip all drawing routines transporter
        endif                       
        ldx #zfire+nmfire+nmlsht-1      ;Number of fireballs and cannon shots
        stx temp9
        begin
			;******************************took color code from here ***********************************************
            lda objst,X
            ifne_
                ifmi                    ;Exploding??
                    lda objst,X
                    clc 
                    adc #08             ;Explosion speed
                    sta objst,X
                    ifpl
                        lda #00
                        sta objst,X         ;Kill this one and kill rest on this one
                        beq ?fir10
                    endif
                else                    ;Not exploding if in here
                    cpx #zfire+nmfire       ;Is it a fireball?
                    ifcc
                        jsr movtng          ;Yes, move it very slow
                    else
                        jsr movtng2         ;Laser shots are unaffected
                    endif
                endif
                ldx temp9
                jsr bounce			;right/left collisions - also destroys 'temp objects' like IonCannon shots and TripPadPyroids
                jsr bump			;ceiling/floor collisions
                jsr locate          ;Place it!
                tya                 ;Did it place?
                ifmi                    ;yep
					;***************************************************************************************************
					;******************************Moved color code here does it work!??********************************
					ldx temp9
					cpx #zfire+nmfire
					ifcc                        ;Fireball
						ldy #($F0+fircol)       	;Guess normal
						lda objst,X             	;Launched
						and #$10
						ifne                    	;yep
							ldy #($F0+colred2)  		;They are redr
						endif
						tya                     	;put color in A
						ldx #shtex_vpg
					else                        ;Laser Cannon Shots
						lda #($F0+colcyan)
						ldx #lshot_vpg
					endif
					jsr vgadd2          ;Add color
					;***************************************************************************************************
					;***************************************************************************************************
                    ldx temp9
                    lda objst,X         ;Explode?
                    ifpl                    ;no!
                        cpx #zfire+nmfire
                        ifcs
                            lda #00             ;Scale to full size if laser cannon shot
                            ldx #$72
                            jsr vgadd2
                            lda temp9
                            asl A
                            clc
                            adc frame
                            asl A
                            and #$0E
                            tay
                            lda newshot,Y
                            ldx newshot+1,Y
                        else
                            laljsr(sparkb)
                            lxhjsr(sparkb)          ;Add JSR to sparkbuffer
                        endif
                    else
                        jsr splash
                    endif
                    jsr vgadd2
                endif
            endif
?fir10      dec temp9
            ldx temp9
            cpx #zfire          ;Last one
        ccend
		;****************** Fall Through *********************
			.sbttl "Laser Cannon in Maze"
		;*****************************************************
		;* Cannon objst notes:
		;*
		;* Cannon is either inactive (=0) or active (!=0)
		;*
canndr  ldx #zcann+nmcann-1
        stx temp9
        begin
            lda objst,X             ;Make sure cannon is on
            ifne
                jsr cannon				;Get cannon stuff from main ROM as it will read page 6/7 maze data
				jsr drcann				;Do the final cannon rendering 
            endif
            dec temp9
            ldx temp9
            cpx #zcann
        ccend
		;****************** Fall Through *********************
			.sbttl "Discs in Maze"
		;*****************************************************
discs   lda	#($F0+colpurple)		;They are all purple if reactor is subcritical
		bit	objst+zreactor
		ifmi
			lda	#($F0+colyellow)		;And yellow if it is supercritical
		endif
		ldx	#shld_vpg		;Stat and page select
		jsr	vgadd2
		ldx #zdisc+nmdisc-1
        stx temp9
        begin
            lda objxh,X     ;Defined
            ifne
				ifpl
					jsr locate
					tya
					ifmi			;On Screen?
						lda #$80		;default linear scale
						sta temp8		;save for later
						ldx temp9		;Get back working X object index
						lda oxystat-zdisc,X
						ifne
							;This O2 is animating
							and #$0F
							tax
							dex
							ldy discscal,X
							sty temp8			;adjusted scale
							asl A
							asl A
							asl A
							asl A
							ldx #0
							jsr vgadd2
							ldx #0
							lda #0				
							jsr vgadd2
						endif
						;draw chars
						lda temp8	;#$80		;0 ORIGINALLY
						ldx #$74				;75 originally
						jsr vgadd2              ;Scale down
						VGADD_JSR(shield)
						lda temp8	;#00
						ldx #$72                ;Small 2 (was 73 originally)
						jsr vgadd2
						VGADD_VCTRS(-12,8,hidden)
						lda vgjch2              ;vgmsga+6   ;Get JSRL into 2
						ldx vgjch2+1            ;vgmsga+7   
						jsr vgadd2
					endif
				endif
            endif
            dec temp9
            ldx temp9
            cpx #zdisc
        ccend
		;********** Fall Through ********************
			.sbttl "Slow Down Clock"
		;********************************************
		;* Slow down clock - Concentric boxes       *
		;* growing inside a larger one.             *
		;********************************************
drclock lda objst+zstuf
        ifeq
?drc5       jmp drboot
        endif
        lda objst+zreactor      ;Moves fast before touch
        ifpl
?drc10      dec sldnfr
        else
            lda frame
            and #$0F                ;Move slow
            beq ?drc10
        endif
        ldx #zstuf          	;Now use sldnfr to generate a picture
        jsr locate
        tya 
        bpl ?drc5
        lda #$E6
        ldy objst+zstuf         ;Set off?
        cpy #02             	;Yes if 2
        ifeq
            lda #$FB                ;Yellow becomes red!!
        endif
        ldx #clock_vpg
        jsr vgadd2          	;Same color as exploding fish
        lda #$60
        ldx #$71
        jsr vgadd2
        lda gclock
        ldx gclock+1
        jsr vgadd2          	;Draw clock body
        lda #$F7
        ldx #clock_vpg
        jsr vgadd2          	;White
        lda gclock+2
        ldx gclock+3
        jsr vgadd2          	;For the tick marks on the clock
        lda #$60
        ldx #$75
        jsr vgadd2          ;Now do hands
        lda #$FB
        ldy objst+zstuf     ;Set off??
        cpy #02
        ifeq                    ;yes
            lda #($E0+colyellow)	;Set the red to yellow!!! 
        endif                   ;Unconnected events
        ldx #clock_vpg          ;Hands color
        jsr vgadd2
        lda sldnfr
        pha 
        pha 
        asl A
        asl A
        asl A
        pha 
        jsr sin
        sta perm2               ;Y + coordinate of minute hand
        jsr neg
        sta perm2+1         ;Y - coordinate of minute hand
        pla 
        jsr cos
        pha                 ;X + coordinate of minute hand
        jsr neg
        sta perm1+1         ;X - coordinate of minute hand
        pla                 ;X coordinate
        ldx perm2               ;Y coordinate
        ldy #$20                ;Intensity
        jsr vgvtr
        lda perm1+1         ;X coordinate back
        ldx perm2+1         ;Y coordinate back
        ldy #00             ;not visible
        jsr vgvtr
        lda #$60
        ldx #$76
        jsr vgadd2          ;Hour hand is half as large (yup! cheap way!)
        pla 
        jsr sin
        sta perm2           ;Y + coordinate
        pla 
        jsr cos             ;X coordinate
        ldx perm2           ;Y coordinate
        ldy #$20
        jsr vgvtr
		;********** Fall Through ***********************
			.sbttl "Magic Boots"
		;***********************************************
		;* Bootie objst notes:
		;*
		;* 00 = Not Active
		;* 01 = Active and not on Rex
		;***********************************************
drboot  lda objst+zstuf+1
        cmp #01
        ifeq                    ;1 means they are out there but not picked up
            ldx #zstuf+1
            jsr locate
            tya
            ifmi
                lda #($A0+colred2)
                ldx #boot_vpg
                jsr vgadd2          ;Put on your red shoes and dance the blues.... (I see your David Bowie reference - jma)
                lda #00
                ldx #$71
                jsr vgadd2          ;Set scale
                lda gboot
                ldx gboot+1
                jsr vgadd2          ;Draw them
            endif
        endif
		;******** Fall Through ******************
		;* Key Pouch objst notes:
		;*
		;* 00 = Not Active
		;* 01 = Active and not on Rex
		;* 02 = Picked up already
		;****************************************
drkeyp	ldx #zstuf+4
		lda objst,X
		ifne
		    cmp #$01               ;In possesion??
            ifeq
				jsr locate
				tya
				ifmi          			;Don't draw it if not on screen
					lda #30
					ldx #$72
					jsr vgadd2          	;Set scale
					lda #($F0+colpurple)
					ldx #lock_vpg
					jsr vgadd2          ;Set color and page
					lda gkeyp
					ldx gkeyp+1
					jsr vgadd2
				endif
			endif
		endif   
		;********** Fall Through ***********************
			.sbttl "Hand that turns off the reactor"
		;***********************************************
		;* Hand objst notes:
		;*
		;* 00 = Inactive
		;* 01 = Resting
		;* 02 = Extending
		;* 03 = Retracting
		;***********************************************
drhand  lda objst+zstuf+3
        ifeq
            jmp drtite
        endif
		sta temp1
        cmp #01             ;At or going to rest point
        ifeq
            lda daccx
            ifne
                jsr subx                ;Retract to corner
            else
                lda daccy
                cmp raccy               ;Rest point
                ifne
                    ifcc
                        jsr addy
                    else
                        jsr suby                ;Retract past corner
                    endif
                else
                    lda objst+zreactor
                    ifmi                    ;Reactor going to explode
                        lda #02
                        sta objst+zstuf+3       ;So turn it back off
                    endif
                endif
            endif
        endif
        lda temp1
        cmp #02             ;Extending to reactor
        ifeq
            lda daccy
            cmp maccy
            ifcc
                jsr addy                ;Extend to corner
            else
                lda daccx
                cmp maccx
                ifcc
                    jsr addx                ;Extend to reactor
                else
                    lda #01             	;Return to rest position
                    sta objst+zstuf+3
                    lda retime
                    ora #$80
                    sta lastreac            ;Set flag so don't award 5000 points again
                    lda #01
                    sta objst+zreactor
                    lda #02
                    sta nxtdisc             ;And reset oxygen value
                endif
            endif
        endif
        lda temp1
        cmp #03             ;Returning to start position
        ifeq
            lda daccx               ;Retract to corner
            ifne
                jsr subx
            else
                lda daccy
                ifne
                    jsr suby            ;Retract up to box
                endif
            endif
        endif
        ldx #zstuf+3
        jsr locate
        tya 
        ifpl
            jmp drtite
        endif
        lda vglist
        pha 
        lda vglist+1
        pha 
        and #08
        asl A               ;0 if 4000, 40 if 4800
        asl A   
        asl A
        asl A
        sta perm3           ;0 or 80
        sta vglist
        ora #$40
        sta perm3+1         ;40 or C0
        lda # ubyte(accbuf)
        sta vglist+1        ;Accordian buffer
        lda daccy
        jsr angcal
        lda #$F2
        sta perm4           ;One accordian color
        lda #$E5
        sta perm4+1         ;Other color
        jsr abuf1
        jsr swcol
        jsr abuf1
        jsr vgrtsl
        lda perm3+1
        sta vglist          ;Other buffer
        lda daccx
        jsr angcal
        jsr abuf2
        jsr swcol
        jsr abuf2
        jsr vgrtsl
        pla 
        sta vglist+1
        pla 
        sta vglist          ;Restore vglist
        lda #$F5
        ldx #hand_vpg
        jsr vgadd2          ;Purple
        lda #$30
        ldx #$71
        jsr vgadd2          ;Scale it
        lda ghand+2
        ldx ghand+3
        jsr vgadd2          ;Draw box
        lda #$F4
        ldx #hand_vpg
        jsr vgadd2          ;Red
        ldy #00             ;Switch is off
        lda objst+zstuf+3
        cmp #03
        ifeq
            ldy #02             ;On
        endif
        lda ghand+4,Y
        ldx ghand+5,Y
        jsr vgadd2          ;Draw switch
        lda #$30
        ldx #$76
        jsr vgadd2          ;Accordian time!!
        lda daccy
        ifne
            lda naccy
            sta perm1
            begin
                ldx perm3               ;0 or 80
                lda # ubyte(accbuf)
                jsr vgjsrl
                dec perm1
            eqend
        endif
        lda daccx
        ifne
            lda naccx
            sta perm1
            begin
                ldx perm3+1
                lda # ubyte(accbuf)
                jsr vgjsrl
                dec perm1
            eqend
        endif
        lda #$30
        ldx #$71
        jsr vgadd2          ;Now draw hand
        lda #$F5                ;Purple
        ldx #$63
        jsr vgadd2
        lda ghand
        ldx ghand+1
        jsr vgadd2
        ;************* Fall Through *****************
		.sbttl "Stalactites in the Maze"
		;********************************************
		;* They hang above trip pads and force rex
		;* to run over the pad. If hit, they vibrate
		;********************************************
		;* Tites objst notes:
		;*
		;* 00 = Inactive
		;* 01 = Normal, just hanging out you know
		;* 02-40 = You hit my Battleship!
		;*
		;********************************************
drtite  lda #$C6                ;They are all green
        ldx #tite_vpg
        jsr vgadd2
        ldx #ztite+nmtite-1
        stx temp9
        begin
            lda objst,X
            ifne
                jsr locate
                tya
                ifmi
                    lda #00
                    ldx #$71
                    jsr vgadd2          ;Scale appropriately
                    lda gtite
                    ldx gtite+1
                    jsr vgadd2
                endif
                ldx temp9
                lda objst,X         ;Stalactite Status
                cmp #02
                ifcs
                    adc #00
                    cmp #$40
                    ifcs
                        lda #01
                    endif
                    sta objst,X         ;Repositioning is done in locate
                endif
            endif
            dec temp9
            ldx temp9
            cpx #ztite
        ccend
		;********** Fall Through ************************
			.sbttl "Locked Walls in the Maze"
		;************************************************
		ldx #zlock+nmlock-1
        stx temp9
        begin
            lda objst,X
            ifne
                jsr locate
                tya
                ifmi
					ldx temp9
					lda objxh,X
					cmp #1				;Lock min value is 1 (all the way to left side), this is hidden
					ifne
						jsr color           ;Set color and page
						lda #00
						ldx #$71
						jsr vgadd2          ;Set scale
						lda glock
						ldx glock+1
						jsr vgadd2          ;Draw it!
					endif
                endif
            endif
            dec temp9
            ldx temp9
            cpx #zlock
        ccend
		;********** Fall Through ************************
			.sbttl "Keys to fit Those Locks"
		;************************************************
		ldx #zkeys+nmkeys-1
        stx temp9
		;*********************************************************************
        ;I Can't see how this is actually used anywhere, will take it out
		;lda #$FF
        ;sta temp9+1         ;Number of keys in Rex's pocket
		;*********************************************************************
        begin
            lda objst,X
            ifne
                cmp #$10                ;In possesion??
                ifcc
                    jsr locate
                    tya
                    ifmi          			;Don't draw it if not on screen
						lda #00
						ldx #$71
						jsr vgadd2          	;Set scale
						jsr color           	;Set color and page
						lda gkey
						ldx gkey+1
						jsr vgadd2
					endif
                endif
            endif
			dec temp9
            ldx temp9
            cpx #zkeys
        ccend
        ;************* Fall Through *********************
		.sbttl "Escape Pod"
		;************************************************
		;* Entry drpod drawn as static pod.
		;* Entry drpod2 drawn instead on man picture!
		;************************************************
		lda objst+zstuf+2
        ifne
            bmi ?pod10
            cmp #01
            ifeq
?pod10          ldx #zstuf+2            ;If status is 1 or 80, draw it as static
                jsr locate
                tya
                ifmi
drpod2              lda #$68                ;Called from Man draw routine in TWMot
                    ldx #$72
                    jsr vgadd2          	;Scale down
                    lda objst+zstuf+2       ;If in position before blastoff
                    cmp #01
                    ifeq
                        lda #($F0+colwhite)
                        ldx #pod_vpg
                        jsr vgadd2          	;White
                        lda frame               ;Get set for sequencing letters on ship side
                        and #$3C
                        cmp #$14
                        ifcs
                            lda #$14
                        endif
                        lsr A
                        tay
                        lda gpod,Y
                        ldx gpod+1,Y
                        jsr vgadd2
                    endif
                    lda #($D0+colpink)
                    ldx #pod_vpg
                    jsr vgadd2          	;Pink Ship
                    lda epodgr
                    asl A
                    tay
                    lda gpod+$0c,Y
                    ldx gpod+$0d,Y
                    jsr vgadd2
                    lda objst+zstuf+2       ;Flames??
                    cmp #02
                    ifcs
                        lda epodgr
                        cmp #04
                        ifcc                    ;Flames!!!
                            asl A
                            asl A
                            asl A
                            sta perm1
                            lda #$F3
                            ldx #pod_vpg
                            jsr vgadd2          ;Green Flames
                            lda frame
                            and #06
                            clc
                            adc perm1
                            tay
                            lda gpod+$1e,Y
                            ldx gpod+$1f,Y
                            jsr vgadd2
                        endif
                    endif
                endif
            endif
        endif
		;************** Fall Through ************************
			.sbttl "One Way Walls in the Maze"
		;****************************************************
		;*  He's no fun - He fell right over!
		;****************************************************
		ldx #zonew+nmonew-1
        stx temp9
        begin
            ldy #02
            lda onewst-zonew,X
            ifne
                ifmi
                    ldy #00             ;0 points to left, 2 to right
                endif
                sty temp9+1
                jsr locate
                tya 
                ifmi
                    ldx #lshot_vpg          ;Set sign color (Blue Sign, Yellow Lettering)
                    lda temp9+1
                    ifne
                        ldx #lshot_vpg+4
                    endif
                    lda #$F1
                    jsr vgadd2          ;Also page and xflip
                    ldy temp9+1
                    lda onesign,Y
                    ldx onesign+1,Y
                    jsr vgadd2          ;JSRL data in 5000 ROM
                    lda frame               ;Now add lettering
                    and #$20
                    ifne
                        lda #$C6
                    endif
                    ldx #lshot_vpg
                    jsr vgadd2
                    lda onesign+4
                    ldx onesign+5
                    jsr vgadd2
                endif
            endif
            dec temp9
            ldx temp9
            cpx #zonew
        ccend
		;********** Fall Through **********************
			.sbttl "Arrows in the Maze"
		;**********************************************     
		lda frame
        and #$0F
        ifeq                    ;Next in sequence
            sed                 ;************* Caution *****************
            lda thisarw         ;One to hilight
            clc 
            adc #01             ;next
            cmp #$10
            ifcs                    ;Thats all of them
                lda #00
            endif
            sta thisarw
            cld                 ;***************************************
        endif
        ldx #zarow+nmarow-1
        stx temp9
        begin
            lda objxh,X         ;Hit??
            ifne
                ifpl
                    jsr locate
                    tya
                    ifmi
                        ldy #$A0+colred2        ;default Red2
                        sec 
                        lda temp9
                        sbc #zarow          	;Get which of the arrows
                        cmp thisarw         	;One to hilight
                        ifeq
                            ldy #$E0+colflash            ;To flash color
                        endif
						tya 
						ldx #arrow_vpg
						jsr vgadd2          	;Vector Page + flippity bit
                        ldx temp9
                        lda ardir-zarow,X
                        pha 
						
                        cmp #09
                        ifcc
                            lda #$60
                            ldx #$71
                            jsr vgadd2          ;Scale down
                            pla 
                            asl A
                            tay 
                            lda mazarw,Y
                            ldx mazarw+1,Y
                        else				;OUT Arrows instead
                            lda #$60
                            ldx #$72
                            jsr vgadd2			;bigger than the other arrows
                            pla 
                            asl A
                            tay 
                            lda mazarw,Y
                            ldx mazarw+1,Y
                            jsr vgadd2
                            lda #$F0+colwhite	;OUT in brite white
                            ldx #arrow_vpg
                            jsr vgadd2
                            lda mazoutwrd
                            ldx mazoutwrd+1
                        endif
                        jsr vgadd2
                    endif
                endif
            endif
            dec temp9
            ldx temp9
            cpx #zarow
        ccend
		;********** Fall Through *********************
			.sbttl "Lightning in the Maze"
		;*********************************************
		lda #$EC                ;They are flash color
        ldx #ltg_vpg            ;Stat and page select
        jsr vgadd2
        ldx #zligh+nmligh+nmfrfl-1
        stx temp9
        begin
            lda objxh,X         ;Active??
            ifne
                ifpl
                    jsr locate
                    tya
                    ifmi
                        lda #$38
                        ldx #$72
                        jsr vgadd2          ;Scale down
                        ldx temp9
                        lda frame
                        lsr A
                        lsr A
                        and #$0E
                        tay 
                        cpx #zfrfl
                        ifcc
                            tya                 ;Offset for Horiz
                            clc
                            adc #$12                ;To H light pics
                            tay
                        endif
                        lda lightning,Y
                        ldx lightning+1,Y
                        jsr vgadd2
                    endif
                endif
            endif
            dec temp9
            ldx temp9
            cpx #zligh
        ccend
		;********** Fall Through ***********************
			.sbttl "Trip Plates in the Maze"
		;***********************************************
		lda frame
        rol A
        rol A
        rol A
        and #$F0
        ora #$0B            ;Red line
        ldx #$60
        jsr vgadd2          ;Add color
        ldx #ztrpp+nmtrpp-1
        stx temp9
        begin
            lda objxh,X         ;Hit??
            ifne
                ifpl
                    jsr locate
                    tya 
                    ifmi
                        lda #00
                        ldx #$73
                        jsr vgadd2      ;Scale down
                        ;lda #-$35
                        ;ldx #00
                        ;jsr vgvtr5      ;Draw blank vector right
						VGADD_VCTRS(0,-53,hidden)
                        ldy #$20
                        lda #$66
                        ldx #00
                        jsr vgvtr       ;Draw a line back, brightness 20
                    endif
                endif
            endif
            dec temp9
            ldx temp9
            cpx #ztrpp
        ccend
		;********** Fall Through ************************
			.sbttl "Ship on Maze"
		;************************************************
		;* Special Routine to draw the ship on the maze *
		;* so that the man comes from somewhere.        *
		;* It will be assumed that the ship 'always'    *
		;* lands at the same place (I zoom it in that   *
		;* way!!) So this may be a constant position.   *
		;************************************************
		lda dif4mz				;Dont show ship on these two levels
		cmp #22-1
		ifne
			cmp #23-1
			ifne
				ldx #zspecial           ;Specials location
				lda #$80                ;New position object
				sta obssxl
				lda #08
				sta obssxh
				lda #00
				sta obssyl
				lda #$FC
				sta obssyh
				lda #01
				sta obsst               ;Object active??
				jsr locate              ;Place it!!
				tya                     ;Did it place??
				ifmi
					jsr shipdis2            ;Draw ship out there!!
				endif  
			endif
		endif
		;********** Fall Through *************************
			.sbttl "Shots from Robots"
		;*************************************************
        ldx #zshot+nmshot-1 ;Number of shots
        stx temp9
        lda #shtcol+$F0
        ldx #shtex_vpg      ;Stat page select
        jsr vgadd2          ;Color shot
        ldx temp9
        begin
            lda objst,X         ;Active??
            ifne
                ifpl
                    inc objst,X         	;Move along!!
                    lda #01
                    sta temp2               ;Will move it twice, half distance
                    begin                   ;To ease fast shot collisions
                        jsr movtng          	;Moveit!!
                        ldx temp9
                        lda rtcol,X
                        ora ltcol,X          	;Hit wall?
                        ifmi                    ;yes
                            lda #$80
                            sta objst,X         ;Remove it
                        endif
                        dec temp2
                    miend
                    jsr locate          ;On screen??
                    tya
                    ifmi                ;Yes!
                        lda #$58
                        ldx #$72
                        jsr vgadd2			;Set scale	
                        lda vglist+1
                        and #08
                        asl A
                        asl A
                        ora #$80
                        ldx #$A0+(trnbuf&$fff/$200)
                        jsr vgadd2
                    endif
                else
                    clc
                    adc #08             ;Shot explodes
                    ifcs
                        lda #00             ;Done
                    endif
                    sta objst,X
                    ifne
                        jsr locate
                        tya
                        ifmi
                            lda #$FB
                            ldx #shtex_vpg
                            jsr vgadd2
                            ldx temp9
                            jsr splash
                            jsr vgadd2
                        endif
                    endif
                endif
            endif
            dec temp9
            ldx temp9
            cpx #zshot              ;Done??
        ccend
		;********** Fall Through *********************
			.sbttl "Perkoid Robots in the Maze"
		;*********************************************
		;* Robots objst notes:
		;* 
		;* Bit 40 - ?
		;* Bit 20 - Lid Opening(set) or Closing(clr) 
		;*          This will count up $A steps... so
		;*          $00->$0A->SetFlag->$2A->$20->ClearFlag->$00(repeat)
		;* Bit 10 - 
		;* Bit 0X - Used for the top status
		;*
		;*********************************************
		ldx #zrobot+nmrob-1
        stx temp9
        begin
            lda objst,X
            bne ?rob5
?rob1       jmp ?rob8               ;If 0 or Minus, skip
?rob5       bmi ?rob1
            lda objst-zrobot+zshot,X    ;Just shoot??
            and #$3F                ;Drop screen bit
            beq ?rob6               ;Dead shot, skip it
            cmp #04
            ifcc
                jsr colidit         ;Just do collisions
            else
?rob6           jsr movtng          ;Move it (this also tests for collisions)
            endif
            ldx temp9           ;We will need X again
            lda velxh,X         ;What he did this frame
            sta temp8+1         ;Save for later uses here and in 'drawrob'
			ifmi                    ;Moving Left
				lda #00             ;If yes, use 'real' vel negated
                sec
                sbc robvel-zrobot,X
            else
				lda robvel-zrobot,X     ;Real velocity
            endif
			sta velxh,X         ;Guess will move
            lda ltcol,X
            and robdir-zrobot,X     ;Moving left and hit left wall??
            ifmi                    ;yes
?rob7           lda temp8+1         ;First time found
                ifne                    ;yep, we were moving before
                    lda objst+zreactor      ;Reactor supercritical??
                    ifpl                    ;no
                        lda objst,X
                        and #$DF                ;Set top to close
                        sta objst,X
                    endif
                endif
            else
                lda robdir-zrobot,X     
                eor #$80
                and rtcol,X          ;Moving right and hit right wall?
                bmi ?rob7
            endif
            jsr rturns               ;Check for special change
            lda frame
            and #03
            ifeq                    ;Time to change pic
                lda objst,X
                and #$BF                ;Drop drawn bit
                bit mask20
                ifeq                ;closing
                    sec
                    sbc #01             ;Close top
                    ifeq
                        lda objst+zreactor
                        ifmi                    ;Insert planned random robot turnaround after reactor is set off!
                            jsr getrand
                            sta robdir-zrobot,X     ;Creates random direction
                        endif
                        lda #$21
                    endif
                else                    ;Counting up/Opening
                    ldy objst-zrobot+zshot,X
                    ifeq
                        ldy limbo-zrobot+zshot,X
                        ifeq
                            clc                     ;Else open top
                            adc #01
                            cmp #$2A                ;Check for open all the way?
                            ifeq
                                jsr fireshot            ;Robot can shoot
                                lda #$0A                ;Clear count up flag
                            endif
                        endif
                    endif
                endif
                sta objst,X         ;Restore stat and new pic info
            endif
            lda rtcol,X
            ifmi
                lda #00
                sec
                sbc velxh,X         ;Negate
                tay                 ;Velocity
                ifpl
                    ldy velxh,X         ;Use correct signed velocity
                endif
                jsr obst
            endif
            lda ltcol,X
            ifmi
                lda #00
                sec 
                sbc velxh,X
                tay
                ifmi
                    ldy velxh,X         ;Use correct signed velocity
                endif
                jsr obst
            endif
            jsr bump                ;Bump head
            jsr locate              ;Place it!
            tya                     ;Did it place?
            ifmi                    ;yep
                jsr drawrob
            endif
?rob8       dec temp9
            ldx temp9
            cpx #zrobot
        ccend
		;********** Fall Through *************************************
			.sbttl "Max Robots in the Maze"
		;*************************************************************
		;* Max objst notes:
		;* 
		;* Bit 40 - Draw Bit, if this is clear, then the object will not draw
		;* Bit 10 - Active bit, if Max is active, then this is set, otherwise he is inactive
		;* Bit 08 - XFlip for controlling direction Max is pointing
		;* Bit 07 - 7 Frames of data for Max rotation
		;*************************************************************
		ldx #zmax+nmmax-1
        stx temp9
        begin
            lda objst,X
            bne ?max5
?max1       jmp ?max8               ;If 0 or Minus, skip
?max5       bmi ?max1           
            and #$10				;Check active flag
            ifeq
                ;max will only activate if Rex is alive
                lda objst+zman
                ifpl
                    ;max is not 'activated' yet, no motion but check for trigger
                    lda maxdata-zmax,X
                    and #$0F
                    ifeq                	;Zero activates right away on maze start
?maxact                 lda objst,X
                        ora #$10
                        sta objst,X
						lda rands+1
						and #3
						tay
						lda _maxactiv,Y				;Randome Max 'WakeUp/Alert/Intruder' voice
                        ;lda #sp_maxactivate       ;Max 'Alert' voice
                        jsr dosound
                    else
                        ;tay             	;put the trigger distance into Y for below
                        ;dey             	;need to subtract one so we can bounds check properly 
						sta temp9+1			;Save our distance in temp9+1
                        lda mazexl
                        sec
                        sbc objxl,X     	;X LSB
                        sta temp1       	;Save this
                        lda mazexh
                        sbc objxh,X
                        sta temp1+1
                        ifmi
                            jsr dblneg          ;ABS of temp1(word)... temp1+1 is in A 
                        endif
                        cmp temp9+1			;Subtract our trigger distance-1
                        ifcc				;X distance is within trigger, now check Y
							lda mazeyl
							sec
							sbc objyl,X     ;Y LSB
							sta temp1       ;Save this
							lda mazeyh
							sbc objyh,X
							sta temp1+1
							ifmi
								jsr dblneg          ;ABS of temp1
							endif
							cmp temp9+1
							bcc ?maxact			;Y is close enough too... Branch to Activate
						endif
                    endif
                endif
            else
				;Check for collisions to stop
                jsr movtng2  	;move object, check all wall collisions, skips clock slowdown
				
				;Set the X velocity we think Max should have
				ldx temp9
				jsr colchkx         ;this sets position diff in temp1,temp1+1 and side between Max and Rex     
				jsr getmaxv   
				sta velxh,X
				
				;Max Animation steps, has to happen after X checking due to temp2 usage below
                lda objst,X
                tay             ;save for below
                and #$0f
                ;now we do animation based upon x motion
                tax
                lda temp2       ;Get back Z status for movement or not
                ifeq
                    ;no motion, use the non-motion table          
                    lda mnonm,X
                else
                    lda mmot,X
                    tax             ;X now has the new frame data that needs split based upon direction
                    lda side
                    ifmi
                        txa
                        ;positive motion, use MSB
                        lsr A
                        lsr A
                        lsr A
                        lsr A
                    else
                        ;negative motion, use LSB
                        txa
                    endif
                endif
                and #$0f
                sta temp1       		;This is our new animation frame
                tya             		;get back original status
                and #$f0
                ora temp1   
                ldx temp9				;must restore X
                sta objst,X     		;save the new status with updated animation data
									
				;Set Y velocity
				jsr colchky			;Y between Max and Rex 
				jsr getmaxv       	;Will return velocity
				sta velyh,X  
				
				;Stop Max if he has hit any walls
				jsr bumpmax	
            endif
			
            jsr locate              ;Place it!
            tya                     ;Did it place?
            ifmi                    ;yep
                jsr drawmax
            endif

?max8       dec temp9
            ldx temp9
            cpx #zmax
        ccend
		;*************** Fall Through *************
		; Maze Tokens
		;******************************************
		lda obsst+1	
		ifpl
			and #$10
			ifeq			;not yet in pocket, do some checks first before drawing
				ldx #ztoken
				jsr locate		;Is it on screen at least?
				tya 
				ifmi   
					ldx #ztoken
					jsr colchky
					lda temp1
					sta temp2				;Save MSB distance in Y for later
					lda temp1+1
					ifeq
						jsr colchkx
						ifeq
							;Now the two MSB distances exist in temp1:x and temp2:y, we need the smaller of the two
							lda	temp2     		;Load second number into accumulator
							cmp	temp1     		;Compare the numbers
							ifcc    			;DONE if first is less than or equal to second.
								lda temp1
							endif
							and #$F0
							eor #$F0
							sta temp1			;for hex color below
							lda obsst+1	
							and #3
							sta temp2
							jsr drawtok					;in TW_MAP
						endif
					endif
				endif
			; else
				; ;position to place based upon index
				; lda obsst+1	
				; and #3
				; sta temp2
				; lda #$F0			;full brite for top
				; sta temp1
				; jsr drawtok			;In TW_Map
			endif
		endif
        ;************ Fall Through ******************
		.sbttl "Transporters in the Maze"
		;********************************************
		; IMPORTANT: The transporter is at the end
		;            because if transporting, we only
		;            draw these guys and the routine
		;            ends/returns. So no other objects
		; 			 should be drawn after transporters.
		;********************************************
?drtran	lda objst+zreactor
        cmp #$20
        ifeq
            rts                     ;If reactor is blowing up, then skip drawing Transporters
        endif 
		;This is all Transport buffer management right at first
        lda vglist+1            ;See which buffer we are drawing in currently
        and #08
        asl A
        asl A
        asl A
        tay                     ;Y = Position in buffer, 0 or 40
        ldx #ntrans-1
        stx temp9                   ;8 sparkles
        begin
            ldx temp9
            lda ttran,X
            clc 
            adc #01                 ;Grow and shrink stars first
            cmp #07
            ifeq                        ;Need to start a new star
                jsr getrand
				pha
                ora #$B0                    ;Get sparkle color
                sta ctran,X
                ;jsr getrand
				pla
				pha
                begin
                    sec 
                    sbc #$19
                    cmp #$19                    ;Get number 0-18
                ccend
                sbc #$0B                    ;Get number -C to +C
                lsr A
                and #$1F
                sta xtran,X             ;1A to 6 signed
                ;jsr getrand
                pla
				eor frame
				begin
                    sec 
                    sbc #$2D
                    cmp #$2D                    ;Get number 0 - 2C
                ccend
                sbc #$15                    ;-16 to 16
                lsr A
                and #$1F
                ora #$40
                sta ytran,X
                lda #00
            endif
            pha 
            sta ttran,X
            lda xtran,X             	;Now draw it in buffer
            sta trnbuf,Y
            lda ytran,X
            sta trnbuf+1,Y              ;Goto sparkle position
            lda ctran,X
            sta trnbuf+2,Y
            lda #tran_vpg               ;Color and page select
            sta trnbuf+3,Y
            pla 
            tax 
            lda tranjsrl,X              ;Get JSRL location
            tax 
            lda gtransp,X
            sta trnbuf+4,Y
            lda gtransp+1,X
            sta trnbuf+5,Y              ;JSRL to sparkle
            ldx temp9                   ;Unless last one, return to center
            ifeq
                sta trnbuf+6,Y
                lda #$C0                    ;RTSL
            else
                lda #00
                sec 
                sbc xtran,X             	;Return to center
                and #$1F
                sta trnbuf+6,Y
                lda #$A0
                sec 
                sbc ytran,X             	;CS
                and #$5F
            endif
            sta trnbuf+7,Y
            tya 
            clc 
            adc #08
            tay 
            dec temp9
        miend
        ;Draw Actual Transporter now...
        ldx #ztran+nmtran-1
        stx temp9
        begin
            lda objst,X
            ifeq
?trn20          jmp ?trnnxt
            endif
            lda objst,X
            ;and #~tr_match
            ;sta objst,X             ;Clear it if it was set, so that we don't do this again
            and #tr_match           ;This flag marks a transporter to go to
            ifne
                lda tspark				;This is set to #01 in dotran (transporter collision routine)
                ifne
					lda temp9
					pha
                    jsr coltran			;X is the index of the match
					pla
					sta temp9
					;***************************************************
					; BUGFIX: Issue #172 Transporter Graphic Glitch
					;***************************************************
					; Need to put value actually in X too!
					;***************************************************
					tax
					;***************************************************
					;***************************************************
                endif
            endif
			;If this one is hidden, can move to next one
            lda objst,X
            bmi ?trn20              ;hidden, skip it
			;Sparkle management starts here...
            lda tspark
            ifne
                lda tranhi-ztran,X
                bpl ?trn30
            endif
            lda frame
            and #03
            ifne
                lda tranhi-ztran,X
                and #$7F				;Flip positive
                jmp ?trn40
            endif
            lda tranhi-ztran,X
            ifpl
                ifne
                    sec 
                    sbc #01
                endif
                jmp ?trn40
            endif
?trn30      and #$7F
            clc 
            adc #02
            cmp #08
            ifcs
                lda #08			;max 8
            endif
?trn40      ldx temp9
			sta tranhi-ztran,X
            ;draw it here
            lda #00
            ldx #$61
            jsr vgadd2
            ldx temp9
            jsr locate
            tya 
            bpl ?trn20              ;Branch if not on screen
            lda #00
            ldx #$71
            jsr vgadd2              ;Scale it
            ldy temp9               
            lda objst,Y
            and #tr_right           ;Also get xflip information
            sta perm1+1
            ifeq
                ldx #tran_vpg
            else
                ldx #tran_vpg+v_xflip
            endif
            stx perm1
            lda objst,Y
            and #$0F
            ora #$E0
            jsr vgadd2  			;Set transporter color
            VGADD_JSR(booth3)		;Main Transporter Body
			;lda gtran
            ;ldx gtran+1
            ;jsr vgadd2				;Tranporter Body here
            ldx temp9
            lda tranhi-ztran,X
            cmp #08					;Pulse transporter base color if activated
            ifeq
                lda #07                 ;Default is full brighness all the time
            endif
            asl A
            asl A
            asl A
            asl A
            asl A
            ora #colflash
            ldx perm1
            jsr vgadd2
            VGADD_JSR(booth4)			;Top and Bottom inside plates
			;lda gtran+2
            ;ldx gtran+3            ;Draw base and top inside plates of transporter
            ;jsr vgadd2
            lda #($E0+colflash)     ;Draw bolts flash color
            ldx perm1
            jsr vgadd2
			VGADD_JSR(booth5)			;Booth nuts
            ;lda gtran+4
            ;ldx gtran+5
            ;jsr vgadd2              ;Draw tranporter bolts
            lda perm1+1
            ifne
                lda #00
                ldx #$60
                jsr vgadd2
            endif
            ldx temp9
            lda tranhi-ztran,X			;Cycles through 80,00,02,04,06,08,07,06,05,04,03,02,01
            ifne
                asl A
                asl A
                sta temp9+1				;04,08,0C,10,14,18,1C,20,24
                lda vglist+1            ;Choose sparkles from appropriated buffer
                and #08
                asl A
                asl A                   ;00 or 20
                adc #$A0                ;A0 or C0
                sec 
                sbc temp9+1				;9C,98,94,90,8C,88,84,80,7C
										;BC,B8,B4,B0,AC,A8,A4,A0,9C
										;4D38,4D30,4D28,4D20,4D18,4D10,4D08,4D00,4CF8
										;4D78,4D70,4F68,4D60,4D58,4D50,4D48,4D40,4D38
                ldx #$A0+(trnbuf&$fff/$200)	;=$A6
                jsr vgadd2              ;JSRL to star buffer
            endif
?trnnxt     dec temp9
            ldx temp9
            cpx #ztran
        ccend
        rts 
;*********************************************
;This is the actual end of the posmo routine
;*********************************************
;*********************************************
_maxactiv .db sp_maxactivate,sp_maxattack,sp_maxintruder,sp_maxdestint

;Scale factor for Oxygen Animation (if enabled)
discscal
		.db $80,$78,$70,$68,$60,$58,$50,$45
		
angcal  pha                 ;Get ready to draw accordian pieces in buffer   
        jsr sin             ;Entry value is an angle
        sta temp1           ;Y,+
        jsr neg
        sta temp1+1         ;Y,-
        pla 
        jsr cos
        sta temp2           ;X,+
        jsr neg
        sta temp2+1         ;X,-
        rts 
        
abuf1   lda perm4           ;Draw the vertical accordian buffer
        ldx #hand_vpg
        jsr vgadd2          ;Set color
        lda temp2+1
        ldx temp1+1
        ldy #$20
        jsr vgvtr
        lda temp2
        ldx temp1+1
        ldy #$20
        jsr vgvtr
        lda perm4+1
        ldx #hand_vpg
        jsr vgadd2
        lda temp2
        ldx temp1
        ldy #$20
        jsr vgvtr
        lda temp2+1
        ldx temp1
        ldy #$20
        jsr vgvtr
        lda #00
        ldx temp1+1
        tay 
        jsr vgvtr
        lda #00
        ldx temp1+1
        tay 
        jmp vgvtr
        
swcol   lda perm4               ;Switch perm4 and perm4+1
        ldx perm4+1
        stx perm4
        sta perm4+1
        rts 
        
abuf2   lda perm4               ;Assemble horizontal buffer
        ldx #hand_vpg
        jsr vgadd2
        lda temp1
        ldx temp2+1
        ldy #$20
        jsr vgvtr
        lda temp1
        ldx temp2
        ldy #$20
        jsr vgvtr
        lda perm4+1
        ldx #hand_vpg
        jsr vgadd2
        lda temp1+1
        ldx temp2
        ldy #$20
        jsr vgvtr
        lda temp1+1
        ldx temp2+1
        ldy #$20
        jsr vgvtr
        lda temp1
        ldx #00
        ldy #00
        jsr vgvtr
        lda temp1
        ldx #00
        ldy #00
        jmp vgvtr
        
subx    ldx #00             ;Subtract angle delta from Y angle
subx2   dec daccx,X
        ifmi
            inc daccx,X
        endif
        rts
            
suby    ldx #01
        jmp subx2
        
addx    ldx #00
addx2   lda daccx,X
        clc 
        adc #01
        cmp maccx,X
        ifcs
            lda maccx,X
        endif
        sta daccx,X
        rts
            
addy    ldx #01
        jmp addx2
        
;***************************************************
    .sbttl "Splash for Fireball, Cannon and Robot"
;***************************************************
splash  lda objst,X
        lsr A
        lsr A
        lsr A
        and #$1E
        tay 
        lda shtexp-$10,Y		;Subtraction is because of the X offset of fireballs from objst
        ldx shtexp-$10+1,Y      ;Use a shot explosion
        rts    
        
color   ldx temp9
        lda objst,X         	;Get color
		and #$0F                ;Entry point for TWMaze key display
        ora #$E0
        ldx #lock_vpg
        jmp vgadd2          	;Set color and page

;********************************************************
; Check to see which way Max should move by using 
; temp1 (set to either X or Y position) if so, get the velocity
;
; Returns Velocity in A
;********************************************************
getmaxv
		lda temp1           ;LSB Difference
        and #$F0
        ora temp1+1         ;temp1+1 is only in low nybble
        sta temp2           ;Save comparison for later
        ifne
            lda side        ;which side defines sign of velocity
			php
			lda maxdata-zmax,X
			and #$30		;Get Max robot speed depending on enum value
			lsr a
			lsr a
			lsr a
			lsr a
			tay
			lda maxspd,Y
			plp
			ifmi
				jsr neg
			endif
        endif
        rts

        
bit7    .db $07
     
maxmax  = $07
	 
maxspd	.db $02	;Slowest
		.db $03 ;Slow
		.db $05 ;Medium
		.db maxmax ;Aggressive
        
;************************************************************
;* Max animation tables - used for the next state based upon
;*                        the X movement results in relation
;*                        to Rex.
;************************************************************
mnonm   .db $08,$08,$01,$02,$03,$04,$05,$06
        .db $08,$08,$09,$0A,$0B,$0C,$0D,$0E
        
mmot    .db $18,$28,$31,$42,$53,$64,$75,$76
        .db $19,$8A,$9B,$AC,$BD,$CE,$DF,$EF
                
;************************************
; Special Check for Perkoid Turns 
;************************************
rturns  lda frame
        and #01
        ifeq
            lda objst,X
            and #$3F
            cmp #$21                ;Opening
            ifeq
                lda temp8+1         ;Standing still??
                ifeq
                    lda objst+zreactor
                    ifpl
                        lda robdir-zrobot,X
                        eor #$80
                        sta robdir-zrobot,X
                    endif
                endif
            endif
        endif
        rts

;*********************************************
; Override velocity based on status of robot
; Call only from wall collisions
;*********************************************	
; Y = current velocity	
; Returns overridden velocity in Y and flips
; direction 
obst    lda objst,X
        bit mask20          ;Is it opening??
        ifne                    ;yes
            and #$0F                ;Check only pic
            cmp #02
            ifcc                    ;Hold velocity until turned
                ldy #00
            endif
        else
            ldy #00             ;Else always hold while closing
        endif
        tya 
        sta velxh,X
        sta temp8+1         ;Indicate cleared
        ifne                ;Velocity change (indicated by tya above)
            bit objst+zreactor
            ifpl
                sta robdir-zrobot,X     ;Save direction here
            endif
        endif
        rts
            
mask20  .byte $20
;mask40  .byte $40

;*******************************************
    .sbttl "Fire a Shot from the Robot"
;*******************************************
fireshot    
        lda #01
        sta objst-zrobot+zshot,X    ;Fire a shot
        lda #$12                    ;Add some velocity
        ldy dif4mz
        cpy #01                 	;Second maze
        ifeq
            lda #06                 ;Yes, slow moving shots
        endif
        ldy robdir-zrobot,X
        ifmi
            jsr neg
        endif
        clc 
        bit objst+zreactor
        ifpl
            adc velxh,X
        endif
        cmp #$80
        ror A                   	;Moves twice so cut in half
        sta velxh-zrobot+zshot,X
        lda #00
        sta velyh-zrobot+zshot,X
        lda objxh,X
        sta objxh-zrobot+zshot,X
        lda objxl,X
        sta objxl-zrobot+zshot,X
        clc 
        lda objyl,X
        adc #$30                    ;Offset up to gun
        sta objyl-zrobot+zshot,X
        lda objyh,X
        adc #00
        sta objyh-zrobot+zshot,X
        lda objst,X             	;Robot on screen??
        asl A
        ifmi                        ;yep
            lda #snd_i7a
            jmp dosound
        endif
        rts
        
tranjsrl    
		.byte 0,2,4,6,4,2,0

;**************************************************
; Transporter Collision - X index is destination
;**************************************************
coltran cmp #01         ;tspark is loaded here
        ifeq
            ldy #$0F
            lda #green          ;Change sparkle RAM to green if starting to beam
            begin
                sta colram+$10,Y
                dey
            miend
        endif
        lda tspark      ;get back tspark
        sec 
        sbc #01
        pha             ;Push tspark-1 for later
        and #$0F
        tay             ;(tspark-1&$0f) into Y
        pla 
        and #$F0
        ifeq
?ctr10      lda stcolr,Y                ;Sparkle in if 00
        else
            cmp #$20
            beq ?ctr10              ;Sparkle in if 20
            ifcs
                lda #green              ;Goto green if 30
            else
                lda #black              ;Goto black if 00
            endif
        endif
        sta colram+$10,Y
        inc tspark              ;tspark++
        lda tspark            
        cmp #$41
        ifge
            lda objst,X         ;No longer being warped to, cause we are there
            and #(~tr_match)    ;Clear our 'warp to' flag
            sta objst,X
            lda #00
            sta tspark
            jmp initcolram      ;Restore all colors to normal
			;EARLY RETURN
        endif
        sta tspark
        cmp #$21
        ifeq_
			;If we are in final maze, do the level 'skip' trick if it is 'special'
			lda tmatch 
			;lda objst,X
			;and #tr_special
			ifne
				;secretly increment level to next maze, shhhhhh!
				jsr nxtmaz_rtpg2		;This destroys X and Y
				;now we have to find our matching transporter
				ldx #ztran+nmtran-1
				begin
					lda objst,X
					ifne
						cmp tmatch			;tmatch is the special transporter code we are looking for
						ifeq
							ora #tr_match
							sta objst,X
							bne ?foundt			;Always
						endif
					endif
					dex
					cpx #ztran
				ccend					;If nothing matches, it will match tranporter @ index 0 which is no good but it will work
			endif
?foundt		lda objxh,X
            sta objxh                   ;Move man at midpoint
            lda mazeyh,X
            sta mazeyh
            lda #$50                    ;Need to raise man so he doesn't fall through floor
            sec
            sbc objyl
            ifcs
                clc
                adc ymot                    ;Move man and screen center so no jump
                sta ymot
                ifcs
                    inc ymot+1
                endif
                lda #$50
                sta objyl
            endif
            lda #$B0
            sec 
            sbc objyl
            ifcc
                adc ymot
                sta ymot
                ifcc
                    dec ymot+1
                endif
                lda #$B0
                sta objyl
            endif
            ldy #00                 ;Now adjust Y coordinate
            lda objst,X
            and #tr_right
            ifeq
                lda #$58
            else
                lda #$A8
            endif
            pha                     ;Move to edge of transporter on way out
            sec 
            sbc objxl
            sta perm1
            ifcc
                dey
            endif
            lda xmot
            clc 
            adc perm1
            sta xmot
            tya 
            adc xmot+1
            sta xmot+1              ;Adjust screen position of man so transporter stays in the same place visually
            pla 
            sta objxl            	;Move man
        endif
        rts
        
;********************************************
    .sbttl "Bounce Utilities"
;********************************************
;* Bounce off wall... Only changes velocity *
;* if change is needed. else no velocity    *
;* change.                                  *
;*                                          *
;* Inputs:  velxh,ltcol,rtcol,(x)=obj index *
;*                                          *
;* Output:  update velocity (if needed)     *
;*                                          *
;* Stack:   0                               *
;*                                          *
;* Uses:    A,X(unchanged),temp8            *
;********************************************
bounce  lda #00
        sec 
        sbc velxh,X             ;Negated velocity
        sta temp8
        ifmi                        ;Negated wants to move left
            lda rtcol,X              ;If -,then want - velocity
            ifmi
?bou10          lda objst,X
                ifpl                        ;And not blowing up??
                    and #$10                    ;temp object?
                    ifne
                        lda #$80
                        sta objst,X             ;Blow it up!
                    endif
                endif
                lda temp8
                sta velxh,X
            endif
        else                        ;Negated wants to go right
            lda ltcol,X              ;Only if left collision
            bmi ?bou10
        endif
        rts 

;sees if object is hitting a wall, if so stop it        
bump    lda #00
        sec 
        sbc velyh,X
        tay
        ifmi                        ;Want to move down??
            lda headcol,X               ;Hit head?
            ifmi
?bmp10          lda objst,X
                ifpl
                    and #$10                ;Temp object??
                    ifne                    ;yes
                        lda #$80
                        sta objst,X         ;Blow it up
                    endif
                endif
                tya
                sta velyh,X
            endif
        else                        ;Want to move up??
            lda ground,X                ;Hit ground??
            bmi ?bmp10
        endif
        rts 

bumpmax lda velyh,X
		ifne
			ifmi                        ;Want to move down??
				lda ground,X               ;Hit head?
				ifmi
?bmpmy		        lda #00
					sta velyh,X
				endif
			else                        ;Want to move up??
				lda headcol,X                ;Hit ground??
				bmi ?bmpmy
			endif
		endif
		;also check left and right...
		lda velxh,X
		ifne
			ifmi	
				;moving left
				lda ltcol,X					;check left
				ifmi
?bmpmx				lda #00
					sta velxh,X
				endif
			else
				;moving right
				lda rtcol,X
				bmi ?bmpmx
			endif
		endif
        rts 
        
;**********************************************
    .sbttl "Perkoid Draw Routine"
;**********************************************
;* This routine draws the robots in 4 stages. *
;*                                            *
;* 1). The body is always the same.           *
;* 2). The head is next. objst is used to     *
;*     determine which of the 7 head pics is  *
;*     drawn.                                 *
;* 3). The tail is then added. velxh is used  *
;*     to determine which fo 3 tails is added.*
;*     If moving left, tail goes right, etc.  *
;* 4). Then the gun is drawn. this is only    *
;*     drawn if the head is open. Again, objst*
;*     will determine which of 4 gun pics is  *
;*     drawn.                                 *
;* 5). Eyes are last.                         *
;*                                            *
;* Inputs: objst(x),velxh(x), (temp9) = (x)   *
;*         robdir(x-zrobot),(temp8+1)=velxh(x)*
;*                                            *
;* Outputs: vectors to vglist                 *
;*                                            *
;* Uses: A,X,Y,temp7,temp8,temp9              *
;**********************************************
;* NOTE : drawrobm is below
;**********************************************
drawrob lda #$30
        ldx #$72                ;scale
        jsr vgadd2
        ldx temp9               ;Object x
        lda objst-zrobot+zshot,X    ;A shot with this robot
        pha                     ;Save for body decision
        lda robdir-zrobot,X     ;Velocity is flip
        ifpl
            ldx #body_vpg           ;Stat and page select
        else
            ldx #body_vpg+xflip     ;Else flip
        endif
        stx temp8           ;Save this too
        lda #robcol+$E0     ;Add color
        jsr vgadd2          ;Color of body
        lda temp8+1         ;Moving??
        ifeq
            pla                 ;Recall shot with this robot and toss for now
?dro10      laljsr(body)
            lxhjsr(body)
        else
            pla                 ;Recall shot with this robot
            and #$3F            ;Drop screen bit
            beq ?dro11          ;Skip if not active
            cmp #$0C            ;Just shot??
            bcc ?dro10          ;yes, so straighten him up
?dro11      laljsr(bodyt)
            lxhjsr(bodyt)       ;Else used tipped body
        endif
        jsr vgadd2
        jsr getstat
        cmp #08             ;Top always open
        ifcs
            lda #08             ;Stay open
        endif
        asl A               ;*2 for words
        tay 
        lda heads-2,Y           ;-2 corrects for no 0
        ldx heads-2+1,Y
        jsr vgadd2          ;Draw head
        lda temp8+1         ;velxh is here
        ifeq
            lda #00
        else
            lda #01
        endif
        asl A               ;*2 for words
        tay 
        lda tails,Y
        ldx tails+1,Y
        jsr vgadd2          ;Add Tail
        lda #mancol2+$E0    ;Player 2 color for now
        ldx temp8
        jsr vgadd2          ;Yellow gun
        jsr getstat
        sec 
        sbc #04             ;Gun out??
        ifpl                    ;It's out!!
            cmp #04             ;All the way out?
            ifcs
                lda #03
            endif
            asl A
            tay 
            lda guns,Y
            ldx guns+1,Y
            jsr vgadd2          ;Draw gun
        endif
        jsr getstat
        cmp #04
        ifcs
            lda #02
        endif
        asl A
        tay
        lda eyes-2,Y            ;-2 as no stat=0
        ldx eyes-2+1,Y
        jsr vgadd2
        lda #colblack
        ldx #$60                ;Restore normal stat
        jmp vgadd2
        
;************* Get object stat and and with 0F ********************
getstat ldx temp9           ;Recall object index
        lda objst,X         ;Get object status
        and #$0F
        rts 
        
 ;********************************************
    .sbttl "Max Robot Routines"
;*********************************************
;* This routine draws the robots in 3 stages. 
;*                                            
;* 1). The body is first. objst is used to     
;*     determine which of the 7 body pics is  
;*     drawn.            
;* 2). The head is next. objst is used to     
;*     determine which of the 7 head pics is  
;*     drawn.                                 
;* 3). The eye is then added 
;*                                            
;* Inputs: objst(x),velxh(x), (temp9) = (x)   
;*         robdir(x-zrobot),(temp8+1)=velxh(x)
;*                                            
;* Outputs: vectors to vglist                 
;*                                            
;* Uses: A,X,Y,temp7,temp8,temp9              
;**********************************************       
drawmax
        lda #$30
        ldx #$72            ;scale
        jsr vgadd2
        ldx temp9           ;Object x
        jsr gmaxstat
        stx temp1+1         ;Save Maxstat here
        ldx temp9
        lda objst,X         ;lda maxdata-zmax,X
        and #$10
        ifne
            lda #colred+$E0    ;Red. Active.
        else
            lda #colred+$A0    ;Dim Red. Inactive (was $60 before BLR feedback)
        endif
        ldx temp1+1
        jsr vgadd2          ;Color of body, maxpage and flip

        ldx temp9           ;Object x
        lda objst,X
        and #07
        asl A
        sta temp1           ;Save our Max vector index
        tay
        lda maxbods,Y           
        ldx maxbods+1,Y
        jsr vgadd2          ;Draw body    
        ldy temp1           ;Get back our index
        lda maxheads,Y           
        ldx maxheads+1,Y
        jsr vgadd2          ;Draw head
		
        ldx temp9
        lda objst,X         ;lda maxdata-zmax,X
        and #$10
        ifne
#IF MAX_LEVITATIONS != 0  	   
			;draw levitations
			lda #$F0+colyellow
			ldx temp1+1               
			jsr vgadd2
			lda frame
			and #$0C
			lsr A					;Words
			tay
			lda lvts,Y           
			ldx lvts+1,Y
			jsr vgadd2          	;Draw Levitations	
#ENDIF

            lda frame
            asl A
            asl A
            asl A
            ifmi                ;positive number in frame
                eor #$FF 
            endif                               
            and #$70
            ora #$80 + colyellow
            ldx temp1+1         ;Get back our stat X value
            jsr vgadd2          ;Color of eyes, maxpage and flip
            ldy temp1           ;back back our index
            lda maxeyes,Y           
            ldx maxeyes+1,Y
            jsr vgadd2          ;Draw Eyes
            
            lda #colblack
            ldx #$60                ;Restore normal stat
            jsr vgadd2
         endif 
         rts
         
gmaxstat
        lda objst,X
        ;sta temp9+1
        and #08
        ifeq
            ldx #maxrob_vpg           ;Stat and page select
        else
            ldx #maxrob_vpg+xflip     ;Else flip
        endif
        rts

;********************************************
; Finish Drawing Cannon
;********************************************
drcann	jsr movtng2         ;Move it, even if clock hit
        lda #00
        ldx #$60
        jsr vgadd2
        ldx temp9
        jsr locate
        tya 
        ifmi_               ;If offscreen, don't draw it!
            ;ifpl
            ;    jmp ?cm100      
            ;endif
            ldx temp9
            lda objyh,X
            cmp perm1           ;If NE, Roof is far away
                                ;If EQ, Roof is Close
            lda objyl,X
            ror A
            clc 
            adc #08
            sta temp5           ;Distance to roof
            lda canngr-zcann,X
            sta temp5+1         ;Store this since we lose X now
            lda #$30
            ldx #$71
            jsr vgadd2
            lda #$A7
            ldx #mzls_vpg
            jsr vgadd2          ;White
            lda #00
            sec 
            sbc temp5
            ldx #00
            jsr vgadd2          ;Up To Roof
            lda #00
            ldx #$20            ;Bright White Line
            jsr vgadd2
            lda #$F1
            ldx #mzls_vpg
            jsr vgadd2          ;Dark Blue
            lda cann
            ldx cann+1
            jsr vgadd2          ;Draw Cannon Mount
            lda temp5
            ldx #$1F            
            jsr vgadd2          ;Back down from roof
            lda #00
            tax 
            jsr vgadd2          ;Invisible Line
            ldy #mzls_vpg
            lda temp5+1
            and #$0E
            cmp #08
            ifcs
                ldy #mzls_vpg+4     ;Need Xflip if angle is 8-0
                eor #$FF
                adc #$0C
            endif
            sty tempa           ;Xflip
            sta tempa+1     ;Rotation Info, 0 2 4 6
            lda #$C6
            ldx tempa
            jsr vgadd2      ;Yellow
            lda #$58
            ldx #$71
            jsr vgadd2      ;Scale down cannon picture
            ldy tempa+1
            lda cann+2,Y
            ldx cann+3,Y
            jsr vgadd2      ;Cannon Body
            lda #$AB
            ldx tempa
            jsr vgadd2      ;Red
            lda temp5+1
            and #$C0
            lsr A
            lsr A
            lsr A
            adc tempa+1
            tay 
            lda cann+$0a,Y
            ldx cann+$0b,Y
            jsr vgadd2      ;Barrel
            lda perm1+1
            ldx tempa
            jsr vgadd2      ;Cyan (usually)
            lda temp5+1
            and #$30
            lsr A
            adc tempa+1
            tay 
            lda cann+$22,Y
            ldx cann+$23,Y
            jsr vgadd2      ;Tubing
            lda perm5
            ifne                ;It shot this frame
                lda #snd_i7b
                jsr dosound
            endif
        endif
        rts
        

        
;********************************************
    .sbttl "Draw and Place Things"
    .sbttl "Locate in the Maze"
;***********************************************************
;* Ths routine decides if it's time to draw 
;* a 'thing' on the screen or not. This is  
;* done by checking the 'things' position   
;* with the current screen location. If     
;* within certain limits, the object is     
;* placed on the screen at its location -   
;* the screen location.                     
;* Note: The upper left corner is 0,0 and   
;* the maze is built in the fourth quadrant 
;* so all objects should have X positive    
;* and Y negative.                          
;*                                          
;* Inputs: (x) = object number              
;*         objxl,objxh,objyl,objyh=location 
;*  If X > ztop then xlsb from table lsb    
;*  in maze.                                
;*                                          
;* Output: (y)= 80 if object placed, else 0 
;*                                          
;* Uses:   temp1, xmot                      
;***********************************************************
locate  ldy #00                 ;Will set it to 80 if placed
		;***************************************************
		; ISSUE #90 - Objects visible on screen
		;***************************************************
		lda objxh,X
		ifeq
			rts			;If object X H value is 00, then never place
		endif
		;***************************************************
        cpx #ztop               ;Is Special object?
        ifcs
            lda lsbsy-ztop,X        ;Get lsb, skip on screen bit
        else
            lda objst,X
            ifpl                    ;Skip clear if exploding
                and #$BF            ;Clear displayed bit
                sta objst,X
            endif
            ifeq
                rts                 ;Skip if dead
            endif
            lda objyl,X         ;Y low position
        endif
        sec 
        sbc mazeyl          ;Mans Y low position
        sta temp1           ;Difference Y LSB
        lda objyh,X
        sbc mazeyh          
        sta temp1+1         ;Difference Y MSB
        lda temp1           
        clc 
        adc ymot			;Add in any pan amount
        sta vdata+2         ;Store for possible position vector
        sta temp1
        lda temp1+1
        adc ymot+1
        sta vdata+3         ;Store for possible position vector
        sta temp1+1
        ifmi
            jsr dblneg          ;Object is below man
            sec
            lda #$70
            cpx #zspecial
            ifeq
                lda #$FF            ;Set to max for ship!!
            endif
            cpx #zcann          ;Determine if laser cannon
            ifcs
                cpx #zcann+nmcann
                ifcc                    ;Cannon has mount suspended above it
                    lda objyl,X         ;We must determine if mount is visible
                    eor #$FF                ;So we add in distance to mount to determine the seeable distance
                    adc #$70
                    sta perm2
                    lda #02
                    adc #00
                    sta perm2+1
                    lda perm1               ;Starting location Y MSB
                    cmp objyh,X         ;If Y MSB is different then it is far away
                    ifne
                        inc perm2+1
                    endif
                    lda perm2
                    sec
                    sbc temp1
                    lda perm2+1
                    sbc temp1+1
?2loc5              jmp ?loc5
                endif
            endif
            sbc temp1
			;Check to see if this is a big reactor, it needs to stay on the screen 3 below the man instead of just two
			php
			cpx #zreactor
			ifeq
				lda limbo,X 		;Is this a mega reactor?
				beq ?nore			;yes
				lda #03
			else 
?nore			lda #02
			endif
			plp
            sbc temp1+1         ;See if less than 320???
			;Exit with sign + if on screen below man
        else                    ;Check above
            lda #$30
            sbc temp1               ;See if above 120
            lda #01
            php 
            cpx #zstuf+3            ;Is it the hand?
            ifeq
                lda hytend
            endif
            plp 
            sbc temp1+1
        endif
        
        ;One of either of the above sections will return with
        ;sign + if the object is on the screen
?loc5   ifmi
            rts
        endif                   ;On screen Y, now do X
        cpx #ztop               ;Fixed LSB??
        ifcc                    ;Nope
            lda objxl,X         ;Use given LSB, else...
        else
            lda lsbsx-ztop,X
            cpx #ztran          ;Check if it is the stalactite
            ifcc                    ;Yup!
                sta perm3               ;Save the LSB
                lda objst,X
                cmp #01
                ifeq
                    lda #00
                endif
                cmp #$21
                ifcs
                    eor #$3F                ;1 to 20, 1E to 1
                endif
                sta perm3+1
                lda frame               ;Vibrate perm3+1 in an alternating direction
                and #02
                ifne
                    lda #00
                    sec
                    sbc perm3+1
                    sta perm3+1
                endif
                lda perm3+1
                clc
                adc perm3               ;Yes, thats the LSB
            endif
        endif
        sec 
        sbc mazexl
        sta temp1
        ldy #00             ;Reset this in case changed
        lda objxh,X
        sbc mazexh
        sta temp1+1
        clc 
        lda temp1
        adc xmot
        sta vdata           ;Store for possible position vector
        lda temp1+1
        adc xmot+1
        sta vdata+1         ;Store for possible position vector
        ifmi                    ;Need absolute value
            cpx #zstuf+3
            ifeq                    ;hand
                eor #$FF
                cmp hxtend
                bcc ?loc15          ;Special check
            else
                eor #$FF                ;Check on + side
            endif
        endif
        cpx #zspecial           ;Special object
        ifeq
            cmp #04         ;If greater than 4, not on screen
        else
            cmp #03             ;If greater than 3, not on screen
        endif
        ifcc                    ;On screen, add to list
            cpx #zfire
            ifge
                cpx #zfire+nmfire+nmlsht
                iflt
                    lda objst,X
                    cmp #$80
                    ifeq
                        lda #snd_i7c
                        jsr dosound
                    endif
                    jmp ?loc14
                endif
            endif
            cpx #ztop
            ifcc
            ;bcc ?loc14          ;Not special
            ;cpx #ztran
            ;ifge
            ;    cpx #ztran+nmtran
            ;    iflt
?loc14              lda objst,X         ;Add in display bit
                    ifpl                    ;Skip if exploding (is on screen)
                        ora #$40
                        sta objst,X
                    endif
            ;    endif
            endif
?loc15      lda #00
            sta vgbrit      ;Draw it in black
            lda #$30
            ldx #$72
            jsr vgadd2      ;Set scale, qqqscalqqq
            jsr vgcntr
            jsr vgvtr2      ;Move beam to object location here
            ldy #$80
        endif
        rts
        
;************************************************
    .sbttl "Move Thing"
;************************************************
;* Will add an objects velocity to its position 
;* and then set collision case flags.           
;*                                              
;* Inputs: X = object index                     
;*         velx(x),vely(x),objx(x),objy(x)      
;*                                              
;* Output: Update position, collision flags set 
;*                                              
;* Uses:   A,X,Y,temp4, 5 bytes stack           
;************************************************   
movtng  lda objst+zstuf         ;Check clock
        cmp #02
        ifeq
            lda frame               ;Slow down if set off
            and #03
            bne colidit				;Dont move, next time maybe, just check collisions
        endif
movtng2 lda objst+zstuf+1   ;Boots
        bmi colidit         ;Freeze action for man discovering shoes
        lda objst+zreactor      ;If reactor set off?
        ifmi
            cpx #zfire
            ifcs
                cpx #zfire+nmfire
                ifcc                    ;Yes, it's a fireball
                    lda objst,X
                    and #$10            ;Yes it's a permanent one
                    beq colidit         ;Don't move it
                else
                    cpx #zrobot
                    ifcs
                        cpx #zrobot+nmrob           ;Of if is a robot
                        bcc colidit
                    endif
                endif
            endif
        endif
        ldy #00
        lda velyh,X
        ifmi
            dey
        endif
        clc
        adc objyl,X
        sta objyl,X         ;Save shot Y LSB
        tya 
        adc objyh,X
        sta objyh,X
mtngx   lda objxl,X
        sta oldxl,X         ;Save for collisions
        ldy #00             ;For sign extend
        lda velxh,X
        ifmi
            dey
        endif
        clc 
        adc objxl,X
        sta objxl,X
        tya                 ;Recall MSB add amount  
        adc objxh,X
        sta objxh,X         ;Move X position
        ;Fall through
colidit stx zindex
        ;******************************************************************************************************
		; JMA - 06042020 - This does not seem to be needed because objhcse does nothing with Y and it is loaded again into Y directly below
		;ldy zindex
		;******************************************************************************************************
		lda objtypetab,X
		cmp #obj_max
		ifeq
			;max only
			jsr maxstloc        ;Locates maze stamp of current object, also sets headcol and ground flags
			jsr maxbcse        	;Checks bottom half collisions for Max robot only
			jsr maxhcse			;Special head checks for Max robot only
		else
			jsr stloc           ;Locates maze stamp of current object, also sets headcol and ground flags
			jsr botcase         ;Checks bottom half collisions
			jsr objhcse         ;Check for top half collisions (non Rex objects only)
		endif
        ldy zindex
		cpy #zshot+nmshot
		ifcc
			lda cktran,Y        ;Does it need to be checked against transporters?
			ifmi
				lda objst+ztran
				ifne                    ;Are there transporters on this maze?
					ldx #nmtran-1
					begin    
						lda objst+ztran,X		;Is there a valid transporter in this slot?
						ifpl						;yes if positive
							ifne						;AND not zero
								lda objxh+ztran,X
								cmp objxh,Y
								ifeq
									lda objyh+ztran,X
									cmp objyh,Y
									ifeq                    ;In same square as transporter
										lda #$80
										sec
										sbc objxl,Y
										ifmi
											jsr neg             ;Distance from transporter
										endif
										cmp #$28
										ifcc                    ;Hit transporter
											lda objxl,Y
											eor velxh,Y         ;Moving towards transporter
											ifpl
												lda objst+ztran,X
												asl A
												asl A
												asl A
												eor velxh,Y
												ifpl                    ;Is touching 
													jsr tranac              ;Warp away!
												else
?colreb			                                 	lda velxh,Y         ;Rebounds off
													ifpl
														lda #$80
														sta rtcol,Y
													else
														lda #$80
														sta ltcol,Y
													endif
												endif
											endif
										endif
									endif
								endif
							endif
						endif
						dex
					miend
				endif
			endif
		endif
        rts 

;******************************************************
;* Will move an object to the correct place after
;* transporting and also adjust velocity
;******************************************************        
tranac  lda objst+ztran,X       ;Hit transporter X
        and #$10
        sta tempa
        txa 
        eor #01                 ;Get other transporter info
        tax 
        lda objst+ztran,X
        and #$10
        eor tempa
        ifeq                    ;Must reverse object direction
            sec
            sbc velxh,Y
            sta velxh,Y
            cpy #zrobot
            ifcs
                cpy #zrobot+nmrob
                ifcc
                    sta robdir-zrobot,Y         ;Store away direction too if robot
                endif
            endif
        endif
        lda objst+ztran,X
        pha 
        and #$40
        ifne
            lda #snd_i6					;Transport sound
            jsr dosound
        endif
        pla 
        and #$10
        ifeq
            lda #$28
        else
            lda #$D8
        endif
        sta objxl,Y
        cpy #zshot
        ifcs
            cpy #zshot+nmshot
            ifcc
                lda objst,Y
                clc
                adc #$10                    ;Shorter up shot distance if going through transporter
                ifmi
                    lda #00
                    sta objst,Y
                    rts
                endif
                sta objst,Y
            endif
        endif
        txa 
        ora #08
        sta limbo,Y
        lda objst,Y
        sta stasav,Y
        lda #00
        sta objst,Y
        lda objxh+ztran,X
        sta objxh,Y
        lda objyh+ztran,X
        sta objyh,Y
        txa 
        eor #01
        tax 
        rts 

;************************************************************
; Max Only Stamp Location and Ground/Ceiling checks
;************************************************************
maxstloc   
		lda objxl,X
        sta temp3           ;Location of this stamp
        lda objxh,X         ;Stamp Position
        sta temp3+1
        lda objyl,X
        sta temp4           ;Place in this stamp
        lda objyh,X
        sta temp4+1         ;Line On
		jsr sttype          ;Set current stamp type
        sta curstmp         ;Current Stamp in
        inc temp4+1
        jsr sttype
        and #$0F            ;Don't need flags here
        sta abvstmp         ;Stamp above my head
        dec temp4+1	
		;******************************************************************************
			.sbttl "Ground Check - Non Max Only"
		;******************************************************************************
		;* Enter from above to set proper 0 page variables and X must point to posit    
		;*                                      
		;* Exit: (A)= tempgrnd value, updates abvg and undg flags too         
		;******************************************************************************    
		lda #00             ;Clear Just past bit
        tay                 ;Guess no head collision
        sta abvg            ;Not above ground as a guess
        ;sta pastbit
        sta undg            ;Also guess above ground
        lda temp4           ;objyl from above
        ifpl                ;Want to check ground
            cmp #$10            ;At least above the stamp
            ifcs
                sec
                sbc gndvt+zmax  	;constant - Ground Table
                ifmi                ;Might have gone by it
                    lda velyh,X     ;Moving down
                    ifmi
                        lda temp4   	;Get back old position
                        sec
                        sbc velyh,X     ;Subtract back in vel and check
                        cmp gndvt+zmax	;constant
                        ;ifcs            ;Yep, it went by
                        ;    lda #$80
                        ;    sta pastbit     ;Signal it just went past
                        ;else                ;Otherwise, we are underground
                            lda #$80
                            sta undg        ;Set this for later
                            lda #00
                        ;endif
                    else            ;Underground moving up!!
                        lda #$80
                        sta undg        ;Set this for later
                        lda #00
                    endif
                else                ;Not past, just above?
                    cmp #maxmax    	;Slope amount
                    ifcc                ;On ground
                        lda #$80			;Yep, set tempground = true
                    else
                        lda #00
                    endif
                endif
            else                ;Not near ground
                lda #$80
                sta undg            ;But for sure under it!
                lda #00
            endif
        else                ;Not near ground, near ceiling??
            sec 
            sbc celt+zmax   ;constant - Can he hit his head??
            ifmi                ;Is below, but is T close?
                cmp #(-1*maxmax)   	;How close?
                ifcs                ;Close enough
                    ldy #$80            ;Will hit his head!
                endif
            else
                lda #$80            ;Is above, did it just pass?
                sta abvg            ;His head is above ceiling
                lda temp4
                sec
                sbc velyh,X     	;Back out velocity
                cmp celt+zmax      	;constant - Did it pass this time?
                ifcc
                    ldy #$80
                endif
            endif
            tya
			sta headcol,X       ;Possible Head collision
            lda #00
        endif
        sta tempgrnd
        ;lda ground,X            ;Save if change state
        ;sta lastgnd,X
        rts 

;*******************************************
    .sbttl "Max Bottom/Ground Case Table"
;*******************************************
;* Collision case routine for ground    
;* collisions. Requires 'curstmp' to    
;* contain current stamp type the man   
;* is standing in. Also requires that   
;* temp3 & temp4 contain X and Y        
;* positions.                           
;* Will return as a subroutine          
;*                                      
;* Caution: (Y) = Object Index          
;*          (Also in zindex)            
;*******************************************
maxbcs 	.word maxb0-1
        .word maxb1-1      ;Stamp 1 (H Line)
        .word maxb2-1      ;Stamp 2 (H end, right down)
        .word maxb3-1      ;Stamp 3 (H end, right up)
        .word maxb4-1      ;Stamp 4 (H end, left up)
        .word maxb5-1      ;Stamp 5 (H end, left down)
        .word maxb6-1      ;Stamp 6 (V Line)
        .word maxb7-1      ;Stamp 7 (Black)
maxbce 	;end of table marker


maxbcse 
		ldy zindex      	;Index of object we are colliding with
		lda #00
        sta rtcol,Y
        sta ltcol,Y
        lda curstmp     	;Current Stamp
        and #$07
        asl a           	;words
        tax             	;into X
        ;cpx #maxbce-maxbcs  ;Valid stamp?
        ;ifcs                ;not valid (NOTE: JMA - This should never happen, maybe check and this can come out?)
        ;    ldx #02         ;Set to line for now
        ;endif
        lda maxbcs+1,X
        pha 
        lda maxbcs,X
        pha 
        rts             ;Do case (jmp equiv)
        
;****************************************
    .sbttl "Stamp calls for Ground"
;****************************************
;* Stamp 0 - No Stamp 0, this is an     *
;* end of line marker, should not ever  *
;* happen                               *
;****************************************   
maxb0  	rts         ;No action!

;****************************************
;* Stamp 1 - H line, set ground bit if  *
;* at ground level always               *
;****************************************   
maxb1  	lda tempgrnd
        sta ground,Y    ;Ok to stop here
        rts
        
;****************************************
;* Stamp 2 - H Line, Right edge down.   *
;* If H position is greater than 80     *
;* then will clear ground bit, else     *
;* use tempgrnd from above.                *
;****************************************       
maxb2  	lda tempgrnd
        ldx temp3           ;X LSB from way above
        ifmi
            cpx #($A0-$18)&$FF     ;lftwal On Edge, time to fall:
            ifcs
                jsr setlft      ;Set at left edge
                lda #00
            endif
        endif
        sta ground,Y
        tax             ;Set Zero Flag
        ifeq                ;If not on ground
            bit undg            ;Under Ground??
            ifmi
                jsr rtchk           ;See if skewered
                jmp maxb6x      	;Don't allow to pass wall
            endif
        endif
        rts 

;****************************************
;* Stamp 3 - H Line, Right edge up.     *
;* Should be no way to pass this edge   *
;* if X position > edge. set rtcol      *
;* if on ground                         *
;****************************************       
maxb3  	ldx temp3           ;LSB X position
        ifpl
            lda tempgrnd
        else
            lda #00
        endif
        sta ground,Y
        jsr maxb6x          ;Do wall check as vertical wall
        bit undg            ;Under Ground Level??
        ifmi                ;yes
            jmp rtchk           ;check for possible skewer
        endif
        rts
        
;****************************************
;* Stamp 4 - H Line, Left edge up.      *
;* Should be no way to pass this edge.  *
;* Set ltcol if at or passed this edge  *
;****************************************               
maxb4  	ldx temp3           ;LSB X from way above
        ifmi                ;On Right edge
            lda tempgrnd           ;Get ground status
        else
            lda #00
        endif
        sta ground,Y
        jsr maxb6x        	;Do wall check as vertical wall
        bit undg          	;Under ground level??
        ifmi              	;yes
            jmp ltchk           ;check left for possible skewer
        endif
        rts

;****************************************
;* Stamp 5 - H Line, Left edge down.    *
;* If over left edge, will clear ground *
;* Else will use tempgrnd from above       *
;****************************************               
maxb5   lda tempgrnd
        ldx temp3           ;LSB X position from above
        ifpl                ;On ledge??
            cpx #$48+$10      ;yep, time to fall??
            ifcc
                jsr setrt           ;Set right wall
                lda #00
            endif
        endif
        sta ground,Y
        tax             ;Test A for 0 (possibly not set!)
        ifeq
            bit undg            ;Under ground??
            ifmi
                jsr ltchk           ;check for skewer
                jmp maxb6x
            endif
        endif
        rts

;****************************************
;* Stamp 6 - Vertical Wall              *
;* Do not allow to pass if hitting wall *
;* may pass if above of below if room.  *
;* Ground is always cleared unless      *
;* standing on edge of wall.            *
;****************************************       
maxb6  	jsr maxb7      	;Do check for side stamps
maxb6x  ldx abvstmp     	;Whats above??
        lda abvchk,X        ;Can we pass this on
        bmi ?maxbad7        ;No need to check here
        lda temp4           ;Y LSB
        cmp #$48+$75       	;gndv Top of this wall here
        ;At this point it is almost in the next stamp!!!!!!
        ifcs                ;We are above
            cmp #$48+$80       ;gndv May need to fall a bit
            ifcs
                rts					;EARLY RTS! No ground here!!
            endif
            lda temp3
            cmp #$48+$10		;rtwal
            ifcs
                cmp #($A0-$18)&$FF	;lftwal = $A0 so this is = -$88
                ifcc                ;We are standing on top!
                    lda #$80
                    sta ground,Y
                else                ;Else, falling, make sure outside edge
                    jsr setlft
                endif
            else                ;Else falling, make sure outside edge
                jsr setrt
            endif
        else                ;Not above wall
?maxbad7    ldx #$80
            ;check for possible wall pass through
            lda velxh,Y     ;Left check left, right check right
            ifmi			;Moving left, check left wall
                lda temp3           ;X LSB
                cmp #$20            ;Max velocity and stop stamp crossover
                ifcs                ;no need to check from 20 to 0
                    cmp lcolv,Y     ;At left wall (size include)
                    ifcc                ;We know we are left, did we just pass through??
                        lda oldxl,Y     ;See if old pos was right of wall
                        bmi ?maxb10      ;Go set collision left wall
                    endif
                endif
            else            ;Moving right, check right wall
                lda temp3       ;XLSB from motion routine
                cmp #-$20       
                ifcc            ;Same as above for other wall
                    cmp rcolv,Y ;At right wall (size table)
                    ifcs                ;We know we are right of right wall, did we just pass through??
                        lda oldxl,Y     ;See if old pos was left of wall
                        bpl ?maxb15      ;Go Set collision
                    endif
                endif
            endif
            lda temp3           ;X LSB
            ifmi                ;Could approach from left
                cmp lcolv,Y     ;At left wall (size include)
                ifcc
?maxb10             lda lcolv,Y     ;Set at left wall (slightly in)
                    sec
                    sbc #01
                    sta objxl,Y
                    txa
                    sta ltcol,Y     ;Stop left motion
                endif
            else
                cmp rcolv,Y         ;At right wall (size table)
                ifcs
?maxb15             lda rcolv,Y         ;Set at right wall (slightly in)
                    clc
                    adc #01
                    sta objxl,Y
                    txa
                    sta rtcol,Y
                endif
            endif
        endif
		rts 

;****************************************
;* Stamp 7 - Blank                      *
;* No H action, must check stamps on    *
;* either side to see if it's time      *
;* to fall                              *
;****************************************   
maxb7  	ldx #00     	;Will guess no ground
        lda temp3       ;LSB X position
        ifmi            ;On right edge of blank
            jsr rtchk       ;Check right side
        else
            jsr ltchk       ;Check left side
        endif
        txa 
        sta ground,Y
        rts
        

;*******************************************
    .sbttl  "Max Head Case Table"  
;*******************************************
;* Max Head collision case routine. Uses info  
;* in temp3 and temp4 (X & Y Man Position) 
;* and headcol to determine velocity action
;*******************************************
maxhcs  .word mxhc0-1		;No Action
        .word mxhc1-1 		;Horizontal
        .word mxhc2-1		;Left Down
        .word mxhc3-1		;Left Up
        .word mxhc4-1		;Right Up
        .word mxhc5-1		;Right Down
        .word mxhc6-1		;Verical
        .word mxhc7-1		;Blank - No head case collision
maxhce  ;End of table marker

maxhcse lda abvstmp     ;Stamp above head
		and #07
		asl a           ;Words
		tax 
		;cpx #maxhce-maxhcs
		;ifcs
		;	ldx #02
		;endif
		lda maxhcs+1,X
		pha
		lda maxhcs,X
		pha
        rts
        
;Stamp 0,1  
mxhc0 
mxhc1   rts         ;No Action!!!

;Stamp 2 - Left Down
mxhc2
		jsr mxhc62        	;Must check part that turns down first
		;Fall Through
;Stamp 3 - Left Up
mxhc3   lda temp3           ;X LSB value
        cmp #(($A0-$10)&$FF) 	;lftwal See if we can pass
        ifcs					;right side
            lda #00
            sta headcol,Y
            bit abvg                ;Possible skewer
            ifmi
                jsr rthdck
            endif
		else					;left side
			lda abvg
			ifmi
				sta ltcol,Y
			endif
        endif
        rts

;Stamp 5 - Right Down		
mxhc5   ;* Stamp 5 - H Line, Left Edge Down           *
        ;* Okay to pass above ground here.            *       
        jsr mxhc62        	;Must check part that turns down first
;Stamp 4 - Right Up
mxhc4   lda temp3           ;X LSB value
        cmp #$4A
		ifcc				
			lda #00
            sta headcol,Y       ;Else pass on this side
            ;bit abvg            ;Above ceiling??
            ;ifmi
            ;    jsr lthdck      ;Check next stamp over
            ;endif
		else
			lda abvg
			ifmi
				sta rtcol,Y
			endif
        endif
        rts
        
;**********************************************************
;* Stamp 6 - Vertical Wall                                *
;* Must check to see if we hit it from the bottom as well *
;* as no horizontal motion through it.                    *
;********************************************************** 
mxhc6   jsr mxhc7         ;Check sides as in 7
mxhc62  lda ground,Y    ;This is entry for any stamp that has an edge pointing down
        ifpl    
            lda temp3
            ifmi
                cmp lcolv,Y     ;Hit Wall?
                ifcc
                    lda #$80
                    jsr sltcol
                endif
            else
                cmp rcolv,Y
                ifcs
                    lda #$80
                    jsr srtcol
                endif
            endif
            ; lda temp4           ;Y LSB
            ; cmp #gndv+$10       ;Ground value
            ; ifcs
                ; lda temp3           ;Hit head check
                ; cmp #lftwal-$18
                ; ifcc
                    ; cmp #rtwal+$10
                    ; ifcs
						; lda #00
						; sta jumpst  	;Clear Jump
                        ; jmp hd12        ;Hit head
                    ; endif
                ; endif
            ; endif
        endif
        rts
        
;**********************************************************
;* Stamp 7 - Blank                                        *
;* Must check the stamp to the closest side to see if a   *
;* possible collision might have occured. This is similar *
;* to st_vpg for ground.                                     *
;********************************************************** 
mxhc7   ldx #00     ;Guess no head collision
        lda temp3       ;LSB X position
        ifmi
            jsr bitrt       ;skip if already in collision
            ifpl
                jsr rthdck  ;check next stamp right
                jsr bitrt
                ifmi
                    lda headcol,Y   ;Will need to hit if we did
                    tax
                    lda abvg        ;Set this back if set
                    jsr srtcol
                endif
            endif
        else
            jsr bitlt       ;skip if already in collision
            ifpl
                jsr lthdck
                jsr bitlt
                ifmi
                    lda headcol,Y
                    tax
                    lda abvg
                    jsr sltcol
                endif
            endif
        endif
        txa 
        sta headcol,Y       ;Set collisions
        rts

;***************************************************
    .sbttl "Top Line collisions other than Rex/Max"
;***************************************************
;* This routine is for objects moving in    
;* the maze other than the man. All ground  
;* and bottom wall collisions for objects   
;* are handled as if the man, but above     
;* collisions are not the same.             
;*                                          
;* Input:  Y = object index pointer         
;*         Y,headocl = possible head col    
;*                                          
;* Output: Will set proper collision bits   
;****************************************************   
; Top Line case table       
objhcs  .word objhc0-1
        .word objhc1-1
        .word objhc2-1
        .word objhc3-1
        .word objhc4-1
        .word objhc5-1
        .word objhc6-1
        .word objhc7-1
objhce  ;End of table marker

objhcse lda abvstmp     ;Stamp we might hit
        asl a
        tax 
        cpx #objhce-objhcs
        ifcs
            ldx #02
        endif
        lda objhcs+1,X
        pha 
        lda objhcs,X
        pha 
        rts             ;Do case
    
objhc0
objhc1  rts             ;Stamp 0 & 1 always collide so leave headcol alone
    
objhc2
objhc3  lda temp3           ;Still contains position
        cmp #($A0-$10)&$FF 	;lftwal See if we can pass
        ifcs
            lda #00
            sta headcol,Y
            bit abvg                ;Possible skewer
            ifmi
                jsr rthdck
            endif
        endif
        rts
            
objhc4
objhc5  lda temp3
        cmp #$48+$10
        ifcc
            lda #00
            sta headcol,Y       ;Else pass on this side
            bit abvg
            ifmi
                jsr lthdck
            endif
        endif
        rts

objhc6  ldx #00     ;No collision??
        lda temp4       ;Y LSB, above center??
        ifmi            ;yes
            lda temp3       ;which side
            ifpl            ;Left or center
                cmp #$58        ;At right wall
                ifcs            ;yes
                    ldx #$80
                    stx rtcol,Y ;Stop right
                endif
            else            ;Right of center
                cmp #$98        ;At left wall
                ifcc            ;yes
                    ldx #$80
                    stx ltcol,Y ;Stop left
                endif
            endif
		endif
        txa 
        sta headcol,Y
        rts
		
; td6     ldx #00     	;No collision??
        ; lda temp4       ;Y LSB, above center??
        ; ifmi            ;yes
            ; lda temp3       ;which side
            ; ifpl            ;Left or center
                ; cmp rcolv,Y	;#$58        ;At right wall
                ; ifcs            ;yes
; ?6sr                ldx #$80
                    ; stx rtcol,Y ;Stop right
                ; endif
            ; else            ;Right of center
				; ;Max might be higher here...
                ; cmp lcolv,Y ;#$9A ;$98        ;At left wall
                ; ifcc            ;yes
; ?6sl                ldx #$80
                    ; stx ltcol,Y ;Stop left
                ; endif
            ; endif
		; else
			; ;above center, check head collision
			; sec 
            ; sbc celt,Y      ;Can he hit his head??
            ; ifmi                ;Is below, but is T close?
                ; cmp #-4         	;How close?
                ; ifcs                ;Close enough
                    ; ldx #$80            ;Will hit his head!
                ; endif
            ; else
                ; lda #$80            ;Is above, did it just pass?
                ; sta abvg            ;His head is above ceiling
                ; lda temp4
                ; sec
                ; sbc velyh,Y     	;Back out velocity
                ; cmp celt,Y      	;Did it pass this time?
                ; ifcc
                    ; ldx #$80
                ; endif
            ; endif
        ; endif
        ; txa 
        ; sta headcol,Y
        ; rts
            
objhc7  ldx #00     ;Guess no head collision
        lda temp3       ;LSB X position
        ifmi
            jsr bitrt       ;skip if already in collision
            ifpl
                jsr rthdck  ;check next stamp right
                jsr bitrt
                ifmi
                    lda headcol,Y   ;Will need to hit if we did
                    tax
                    lda abvg        ;Set this back if set
                    jsr srtcol
                endif
            endif
        else
            jsr bitlt       ;skip if already in collision
            ifpl
                jsr lthdck
                jsr bitlt
                ifmi
                    lda headcol,Y
                    tax
                    lda abvg
                    jsr sltcol
                endif
            endif
        endif
        txa 
        sta headcol,Y       ;Set collisions
        rts

;********************************************************
; Stash/Untash Keys - used for final level to help us
; keep player keys for tricky maze transitions.
;********************************************************
stashkeys
		;see if Rex has any keys, we will save only 1 tho!!
		lda #0
		sta wrpdat				;Number of found keys *HACK
		ldx #zkeys+nmkeys-1
		begin
			lda objst,X
			ifne
				and #$10			;In possesion??
				ifne
					lda objst,X
					and #$0F
					sta wrpdat			;Save the color of the first key found in pocket
					rts					;leave early!!!!!
				endif
			endif
			dex
			cpx #zkeys
		ccend
		rts
		
unstashkeys
		lda wrpdat
		ifne
			ldx #zkeys+nmkeys-1
			begin
				lda objst,X
				and #$0f
				cmp wrpdat
				ifeq
					;match, put it in pocket
					lda objst,X
					ora #$10
					sta objst,X
					rts				;early exit!
				endif
				dex
				cpx #zkeys
			ccend
		endif
		rts
		