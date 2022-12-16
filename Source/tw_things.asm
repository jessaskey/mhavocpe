;***********************************************        
    .TEXT ".TWTHINGS."
;*********************************************************
    .title "TWThings: Things in the Maze"
    .sbttl "Cannon Routines!"
;*********************************************************
;* ROM Page Notes:
;*
;* This routine is not in paged ROM because it leverages
;* stuff in a few different ROM pages and then eventually
;* ends up back in tw_mazeo on Page 2, so it doesn't 
;* really work like typical routines.

cannon  lda cannp1-zcann,X
        sta temp5
        lda cannp2-zcann,X
        sta temp5+1         ;Set up pointer to action table
        lda #00
        sta perm5           ;Set to one if cannon fires this frame
        lda #06     
        sta rompg           ;ROM PAGE CHANGE!!!!! PAGE 6
        ldy #03
        lda (temp5,Y)
        sta perm1           ;Store Y MSB of laser cannon
        lda objst+zstuf+1
        ifmi                ;Boots being put on?
            lda cannss-zcann,X
            jmp ?cm20
        endif
        lda cannss-zcann,X
        ifne
            cmp #$11            ;Getting ready to fire, don't fetch
            ifcc
                jmp ?cm10
            endif
        endif
        lda cannfr-zcann,X  ;Frame wait timer
        ifeq                ;0 means back to start of table
            ldy cannin-zcann,X
            lda (temp5,Y)
            ifeq
                ldy #04         ;Since first four bytes are not actions
                lda (temp5,Y)
            endif
            ifpl                ;Turn a gun to a new orientation or start over
				
                jsr cannturn
            else
				;Either Move Location or Pause 
                and #$40
                ifeq                ;Move gun
                    lda (temp5,Y)
                    asl a
                    asl a
                    sta cannfr-zcann,X      ;Set frame timer
                    ifeq
                        sta velxh,X         ;If no frames then stop velocity
                        sta velyh,X
                    else
                        iny
                        lda (temp5,Y)		;Get X Velocity
                        sta velxh,X
                        iny
                        lda (temp5,Y)		;Get Y Velocity
                        sta velyh,X         ;Set cannon velocities
                    endif
                else
					;pause cannon
                    lda (temp5,Y)
                    asl a
                    asl a
                    sta cannfr-zcann,X    
                endif
                iny
            endif
            tya
            sta cannin-zcann,X      ;Set forward the index an appropriate amount
        else
            dec cannfr-zcann,X      ;Else just decrement wait timer
        endif
?cm10   lda cannss-zcann,X      ;If spinning or firing continue that as well
        ifne
            pha
            ldy canndf-zcann,X
            clc
            adc ?cm110,Y            ;Shot sequence occurs faster
            and ?cm115,Y
            sta cannss-zcann,X
            pla
            cmp #$10
            ifeq    
                jsr cannshot            ;Shoot a bolt(routine must return an 8 in A)
            else
                cmp #$30                ;Done spinning
                ifeq
                    lda #00
                    sta cannss-zcann,X
                endif
            endif
        endif
?cm20   lsr a           ;If not shots fired,then this is zero
        tay
        lda cannseq,Y
        and #$C0            ;Barrel Data
        sta tempa
        lda canngr-zcann,X
        and #$0F            ;Preserve Angle
        ora tempa
        sta tempa           ;Angle and Barrel
        lda frame
        and #06
        asl a
        asl a
        asl a
        ora tempa           ;Angle & Barrel and Tubing
        sta canngr-zcann,X
        lda cannseq,Y
        and #$0F
        ora #$F0            ;Tubing Color
        sta perm1+1
        lda #02
        sta rompg           ;ROM PAGE CHANGE!!!!! PAGE 2
		;now we can rts
		rts
		
?cm110  .byte $01,$02,$04
?cm115  .byte $FF,$FE,$FC

;*********************************************
    .sbttl "Turn Cannon to new Orientation"
;*********************************************
cannturn    
		sty tempa       ;Save Index
        sta tempa+1     ;Save Data
        pha 
        and #06         ;Get Position Speed
        lsr A
        tay 
        sta canndf-zcann,X  ;Store it away
        lda ?ct90,Y
        sta perm4
        lda ?ct91,Y
        sta perm2
        lda ?ct92,Y
        sta perm3
        pla 
        and #$38
        lsr A
        lsr A
        lsr A
        tay             	;New Orientation Goal.. 0 to 6
        lda cannss-zcann,X
        bne ?ct70           ;Don't do this if spinning
        lda canngr-zcann,X
        and #$0E
        cmp ?ct100,Y        ;Current Cannon angle versus final angle
        ifne
            php             ;Wrong direction
            lda objyl,X
            and perm4
            cmp #$A0
            ifne                ;Wrong Height to rotate at
                ifcs
                    sbc perm2           ;Too high so lower it
                else
                    adc perm2           ;Too low so raise it
                endif
                sta objyl,X
                plp
            else
                plp         ;Right height, so rotate it
                ifcs            
                    lda #$FE        ;Too large a number so lower it 
                    clc
                else
                    lda #02     ;Too small a number so raise it
                endif
                tay
                lda frame       ;Turn more slowly
                and perm3
                ifne
                    ldy #00
                endif
                tya
                adc canngr-zcann,X
                sta canngr-zcann,X
            endif
        else
            lda objyl,X     ;Correct direction, wroing height
            and perm4
            cmp ?ct110,Y
            beq ?ct80           ;Made it!
            ifcs
                sbc perm2
            else
                adc perm2
            endif
            sta objyl,X
            cmp ?ct110,Y
            beq ?ct80
        endif
?ct70  	ldy tempa           ;Restore Pointer
?ct75   rts

    
?ct80   lda tempa+1     ;Made it to destination
        and #01
        ifne
            lda cannss-zcann,X      ;Cannon spinning?
            ifne
                lda #$10                ;Fire Immediately
            else
                lda #01             ;Warm up and fire
            endif
            sta cannss-zcann,X
        endif
        inc tempa       ;Move pointer forward one, either to shot
                        ;velocity or new instruction bra
        bne ?ct70
        
?ct90   .byte $fc,$f8,$f0
?ct91   .byte $04,$08,$10
?ct92   .byte $03,$01,$00
?ct100  .byte $00,$00,$00,$06,$0C,$0C,$0C       ;Angles
?ct110  .byte $C0,$80,$40,$80,$C0,$80,$40       ;Heights

cannseq .byte $03,$04,$03,$06,$04,$06,$0B,$06   ;Warm up to fire
        .byte $8B,$8B,$46,$4B,$0B,$06,$04,$06   ;Fire and retract Muzzle
        .byte $04,$06,$04,$06,$03,$04,$03,$06   ;Re-extend Muzzle


cannshot    
		ldy #zlsht+nmlsht-1     ;Find a place to put shot
        begin   
            lda objst,Y
            ora limbo,Y
            beq ?cs10			;found a slot, use it
            dey 
            cpy #zlsht
        ccend
        inc cannin-zcann,X      ;Nowhere to put it
        lda #$10
        rts 
        
?cs10   sty tempa
        inc perm5
        lda canngr-zcann,X      ;Shot this frame
        and #$0E
        lsr A
        cmp #03
        ifeq
            lda #08
        endif
        lsr A
        lsr A
        sta tempa+1
        tay 
        lda objxl,X
        clc 
        adc ?cs100,Y
        ldy tempa
        sta objxl,Y
        lda objxh,X
        ldy tempa+1
        adc ?cs101,Y
        ldy tempa
        sta objxh,Y
        lda objyl,X
        clc 
        ldy tempa+1
        adc ?cs102,Y
        ldy tempa
        sta objyl,Y
        lda objyh,X
        ldy tempa+1
        adc ?cs103,Y
        ldy tempa
        sta objyh,Y
        ldy cannin-zcann,X      ;Get Velocity
        inc cannin-zcann,X
        lda (temp5,Y)
        sta temp9+1
        lda #00
        ldy tempa
        sta velxh,Y
        sta velyh,Y
        lda #$10
        sta objst,Y         ;Temporary Object
        lda tempa+1
        ifeq
            lda temp9+1
            sta velxh,Y     ;Apply velocity in correct direction
        else
            cmp #01
            ifeq
                lda #00
                sbc temp9+1
                sta velxh,Y
            else
                lda #00
                sbc temp9+1
                sta velyh,Y
            endif
        endif
        lda #$10
        rts
        
?cs100  .byte $68,$98,$00           ;Offsets of shots from gun center
?cs101  .byte $00,$FF,$00
?cs102  .byte $00,$00,$80
?cs103  .byte $00,$00,$FF

;*************************************************************
    .sbttl "Get Scaled Speed"
;*************************************************************
;* The routine calculates the speed an object will move on   *
;* the screen based on it's Y position.(starspeed,etc)       *
;*                                                           *
;* Inputs:  A = Y position MSB                               *
;*          Y = Y Position LSB                   *
;*   (spcspd) = Speed Factor, larger numbers are slower      *
;*      carry = set for up screen                            *
;*                                                           *
;* Output:  A & temp1  = speed amount LSB                    *
;*          A & temp+1 = speed amoutn MSB                  *
;*                                                           *
;* Uses:    A, Y, temp1, 2 bytes stack                       *
;*                                                           *
;* Note: Entry at gss2 assumes:                              *
;*      1) LSB already stored in temp1                   *
;*      2) Input Y = speed factor                        *
;*                                                           *
;* Note: Entry at gss1 is for Star Motion. The Amount of     *
;*       offset is added so stars move at different speeds   *
;************************************************************* 
gss1    sty temp1
        php 
        pha 
        lda strflg,X        ;Save A and Carry
        and #03
        eor #$FF            ;Negate this
        sec 
        adc spcspd
        tay 
        pla 
        jmp gss3
gss     sty temp1           ;Save for later
        ldy spcspd
gss2    php             	;Save entry carry
gss3    jsr mror            ;Get scaled speed
        sta temp1+1     	;Save MSB
        tay             	;Also save in Y
        lda temp1           ;MSB=0?? (in case LSB is!!)
        ifeq                ;Don't get stuck at 0
            lda #01
            sta temp1
        endif
        plp             	;Check for negate
        ifcs                ;Negate
            jsr dblneg
            tay         		;Save MSB in Y
            lda temp1
        endif
        rts
        
;*************************************************************
    .sbttl "Get Intensity .vs Distance"
;*************************************************************
;* Gets intensity vs distance 'up' the screen so far away    *
;* objects don't look as bright as objects which are near.   *
;*                                                           *
;* Inputs:  A = Distance up Screen, not corrected for star   *
;*              origin offset.                   *
;*                                                           *
;* Output:  A = Intensity byte for Stat Instruction          *
;*                                                           *
;* Uses:    A                                                *
;*************************************************************          
getint  clc 
        adc #04     ;Add 4 to give range of 4-f
        asl a   
        cmp #$10
        ifcs
            lda #$0F
        endif
        asl a
        asl a
        asl a
        asl a
        rts         ;Intensity in top nibble!
            
;*****************************************
 .sbttl "Stars"
;*****************************************
escape  jmp strmov  ;Will just move them this time

;*****************************************
    .sbttl "Generate Stars"
;*****************************************
strgen  lda mzgame  
		cmp #04     	;Possible Landing?
		ifeq            ;Could be, check direction
			bit gamest  	;Up or Down??
			bvc escape  	;Just draw them
		endif
		cmp #08     	;Transistion to up or down
		beq escape  	;Just draw
		and #02+$80 	;Skip on Zoom or maze
		ora tact        ;Skip if tactical
		ifne
			rts         	;Skip on these games
		endif   
        lda numstr
        cmp #maxstr
        bcs escape  	;No start up's needed
        ldx #maxstr
        begin
            dex
            bmi escape  	;None available now
            lda strflg,X
        eqend           ;Found a dead one
        lda mzgame  
        and #04     	;3rd person view?
        ifne
            jsr getrand
            sta strxl,X
            jsr getrand
            and #07
            bit rands+2
            ifmi
                jsr neg
            endif
            sta strxh,X
            lda #00
            sta stryl,X
            sta stryh,X 	;Start at top
            jmp ?gs9        ;*** Always!! ***
        endif
;****** Normal First Person View Stars *******
        jsr getrand
        sta stryl,X
        jsr getrand
        sta strxl,X     	;Got X
        lda rearview        ;Moving Backwards?
        ifpl                ;nope
            lda #00
            sta stryh,X
            sta strxh,X
            jsr getrand
            ifmi
                dec strxh,X 		;Upper byte minus 1
                lda strxl,X
                jsr neg
                sta strxl,X
            endif
            lda stroyh      ;Where is the origin??
            cmp #05         ;Off top of screen??
            bcs ?gs8            ;If yes, alway start down
            jsr getrand
            ifmi
?gs8            dec stryh,X
                lda stryl,X
                jsr neg
                sta stryl,X
            endif
        else            ;Moving Backwards, start at edges
            bit rands
            ifmi
                lda stroyh  ;Get Origin
                cmp #05     ;If at top, always start at bottom
                bcs   ?gs11     ;Always start at bottom
                bit rands+1
                ifmi
                    lda #06
                else            ;Else, start at bottom
?gs11                   lda #-6
                    sec
                    sbc stroyh
                endif
                sta stryh,X ;Place object Y
                jsr getrand
                and #$0F
                sec
                sbc #07     ;+/- 7
                sta strxh,X
            else
                lda #07     ;Guess right edge
                bit rands
                ifmi
                    lda #-7
                endif
                sta strxh,X
                jsr getrand
                and #07     
                sec
                sbc #01     ;0-6
                bit rands+2
                ifmi
                    jsr neg
                endif
                sta stryh,X
            endif
        endif
?gs9    jsr getrand
        and #03     	;Extra Speed Flags
        ora #$80        ;Flag Active
        sta strflg,X
        inc numstr
        ;********* Fall Through *************************
		;************************************************
			.sbttl "Move Stars"
		;************************************************
		;* Will move the stars in a quasi 3 dimensional *
		;* pattern, or will move them from a third      *
		;* person view if mzgame, bit 4 is set. In this *
		;* case, the speed of the stars in Y must be    *
		;* passed in ?????                              *
		;*                                              *
		;* Inputs: mzgame, temp1                        *
		;*                                              *
		;* Uses:   temp1, temp2, A, X, Y                *
		;************************************************
strmov  lda mzgame
        and #$5F        ;Not during these plays
        ifeq            ;Only during space game
            lda stroyh
            cmp #06     ;Have to move origin
            ifne
                lda stroyl
                clc
                adc #$14
                sta stroyl
                ifcs
                    inc stroyh
                endif
            endif
        endif
        ldx #maxstr-1       ;Total Stars
        begin
            lda strflg,X
            ifpl
                jmp nxtstr
            endif
            and #03
            sta temp2       ;Used in gss1 later
            lda mzgame
            tay         ;Save this
            and #08     ;transition??
            ifne
                jmp ?mst5       ;Just draw it
            endif
            tya
            and #06     ;Fixed speed
            ifne
                ldy #00
                lda statst  ;Move at station speed
                eor #$FF        ;stars are old way, move oppisite
                and #$7F        ;Drop active bit
                cmp #$40        ;Up or down??
                ifcs            ;Negative
                    dey
                    ora #$80
                endif
                jmp ?mst1       ;Move Y only
            endif
            lda strxh,X
            ldy strxl,X
            clc         ;Down Screen
            jsr gss1        ;Get scaled speed
            bit rearview    ;Backwards??
            ifpl            ;Nope
?mst30          clc         ;Above returns A=LSB Y=MSB
                adc strxl,X
                sta strxl,X
                tya
                adc strxh,X
                sta strxh,X
                jsr chkstar
                beq nxtstr  ;It died
            else            ;Else, moving backwards
                sty temp1+1
                sta temp1
                jsr dblneg  ;Move inwards
                ldy temp1+1
                lda temp1
                bne ?mst30
            endif
            lda stryh,X ;Y Distance
            ldy stryl,X
            clc         ;Down Screen
            jsr gss1        ;Get Scaled Speed
            bit rearview    ;Backwards??
            ifpl            ;nope
?mst1               clc
                adc stryl,X
                sta stryl,X
                tya
                adc stryh,X
                sta stryh,X
                clc
                adc stroyh      ;Correct for star offset
                jsr chkstar      ;Off Edge
                beq nxtstr      ;Skip if dead
            else
                sty temp1+1
                sta temp1
                jsr dblneg
                ldy temp1+1
                lda temp1
                bne ?mst1           ;Reverse and move
            endif
?mst5       stx temp1           ;Save X
            jsr drawstr
            ldx temp1
nxtstr      dex
        miend
        jmp shpshts     ;Now move ship shots
		
;*****************************************		
; Check star for boundary
;*****************************************
chkstar php             ;Save signs 
        bit rearview        ;Moving forward??
        ifpl
            plp             ;Recall Signs
            ifmi
                cmp #-6
                bcc ?mst15
            else
                cmp #06
                bcs ?mst15
            endif
            lda #01     ;Set NE flag
            rts 
?mst15      dec numstr
            lda #00     ;Another star novas
            sta strflg,X    ;Return with 0
            rts 
        else
            plp         ;Toss away
            lda strxh,X
            ora stryh,X ;When both MSB's are 0...
            beq ?mst15  ;... Kill it!!
        endif
        rts 
  
;**************************************
; Draw a Star
;************************************** 
drawstr lda strxl,X
        sta vdata
        lda strxh,X
        sta vdata+1     ;Save X vector value
        lda stryl,X
        clc 
        adc stroyl
        sta vdata+2
        lda stryh,X
        pha             ;Save for intensity
        adc stroyh      ;Center point offset
        sta vdata+3     ;Transfer position
        jsr vgcntr      
        lda #$58        ;Use same scale as for ship position
        ldx #$73
        jsr vgadd2      ;Set Scale
        lda #00
        sta vgbrit      
        jsr vgvtr2
        pla             ;Recall Y posit
        ifmi
            eor #$FF
        endif
        asl a
        asl a
        asl a
        asl a           ;Stars Intensity
        adc #$47
        ldx #$60+sparkle ;mapdt_vpg+sparkle  ;Stat, Sparkle
        jsr vgadd2
        lda #00
        ldx #$72            ;Scale for dot
        jsr vgadd2
        laljsr(mapdot)
        lxhjsr(mapdot)
        jmp vgadd2
        
;**********************************************
    .sbttl "Ships Shots Routine"
;**********************************************
;* Shots wil move just opposite of the stars  *
;**********************************************
shpshts ldx #nmsshots-1     ;Ship shots
        stx temp9
        begin
            lda shotst,X        ;Shot Active??
            bne ?ssr10      	;This one is active
            jmp ?ssr20
?ssr10      ifpl                ;Not blowing up!
                lda shotxh,X
                sta vdata+1     	;Transfer X position
                lda shotxl,X
                sta vdata
                lda #shtspd 
                ldy #-1         	;Constant speed up screen
                clc
                adc shotyl,X        
                sta shotyl,X        ;Save shot Y LSB
                sta vdata+2
                tya
                adc shotyh,X
                sta shotyh,X
                sta vdata+3
                tay             	;Save LSB
                iny
                lda mzgame
                and #04
                ifeq
                    dey
                    dey
                    dey
                endif
                tya 
                ifmi
                    lda #00
                    sta shotst,X        ;Stop this show now
                else                ;Else, draw this one
					;shot to collision checks 
					lda scflags
					cmp #sc_state_fight
					ifeq
						;*************************************************
						; For Star Castle, check for shot to panel
						; collisions.
						;*************************************************
						jsr stpg3
						jsr ckshield
						jsr stpg0
					else
						;Normal fighter level
						jsr ckbase		;See if shot hits the base
					endif
					lda #02         	;Size factor
					jsr drawshot        ;Draw the shot (does the position)
                endif
?ssr20      else
				;blowing up
                lda shotxl,X
                sta vdata
                lda shotxh,X
                sta vdata+1
                lda shotyl,X
                sta vdata+2
                lda shotyh,X
                sta vdata+3     	;Xfer Location
                lda #02         	;Scale Factor
                jsr drawshot        ;Will only position
                ldx #shtex_vpg
                jsr vgadd2      	;Page select for shot
                ldx temp9
                lda shotst,X        ;Continue Explosion
                lsr A
                lsr A
                lsr A
                lsr A
                tay             	;Save this
                clc
                adc shotst,X        ;Move along explosion
                ifpl
                    lda #00
                endif
                sta shotst,X
                tya
                asl A
                tay
                lda shtexp-$10,Y    ;Shot Explosion
                ldx shtexp-$10+1,Y  ;MSB of JSR
                ;-10 as top nibble for explosion goes 80 to off
                jsr vgadd2
            endif
            dec temp9
            ldx temp9
        miend
		;**********************************************
			.sbttl "Check Fire Switch"
		;**********************************************
		;* Start a shot from space ship. Will start 1 *
		;* shot every time the fire button is pressed *
		;* If the button is held, it will fire every  *
		;* 1/4 second.(As long as shots are available)*
		;**********************************************     
		;strtshot    
        lda shipst
        ifne
            ifpl
                lda mzgame
                ifeq                ;Space only
                    lda gamest      ;Attract??
                    ifpl                ;Yes
                        jsr getrand
                    else                ;Game Mode
                        lda jbstat      ;Button pressed
                    endif
                    ifmi            ;Yes!
                        lda shotdb      ;Pressed Last time
                        ifpl                ;no
                            lda #$80
                            sta shotdb      ;Set last flag
                            jsr stone       ;Toss a stone
                        else
                            lda frame
                            and #03         ;Time lag between shots
                            ifeq
                                jsr stone           ;Toss one out
                            endif
                        endif
                    else            ;Button not pressed
                        sta shotdb  ;Clear last flag
                    endif
                endif
            endif
        endif
        jsr enemy       ;Do Enemy Routines
        jmp laser       ;Do Enemy Ships		
        
;***************************************************
    .sbttl "Start a Shot at the Ship"
;***************************************************
stone   ldx #nmsshots-1
        begin               ;Find an empty slot
            lda shotst,X
            ora shotst-1,X      ;Need 2 slots
            beq ?ss10
            jmp ?ss20           ;Long Jump
?ss10       lda #01         
            sta temp4
            ldy shppbs      ;Picture base (0-5)
            begin
                lda shipxl
                sta shotxl,X
                lda shipxh
                sta shotxh,X        ;Save X Position
                lda shipyl
                sta shotyl,X
                lda shipyh
                sta shotyh,X        ;Position Shot
                lda #00
                sta temp1           ;Guess + Number 
                lda nxtsid      ;Alt sides
                eor #$80
                sta nxtsid
                ifmi    
                    lda strlfx,Y        ;Start Left
                else
                    lda strrtx,Y        ;Start right
                endif
                ifmi                ;Negative Number??
                    dec temp1           ;Prop carry
                endif
                asl a           ;Number *2
                rol temp1
                clc
                adc shotxl,X
                sta shotxl,X
                lda temp1
                adc shotxh,X
                sta shotxh,X        ;Correct X
                lda #-1
                sta temp1           ;The number is -
                lda #$E4            ;Y always from same place ($f2*2)
                clc
                adc shotyl,X
                sta shotyl,X
                lda temp1           ;MSB (sign prop)
                adc shotyh,X
                sta shotyh,X        ;Finish corrected position
                lda #01
                sta shotst,X        ;Active
                dex             ;Start alt shot 
                dec temp4
                lda #$80            ;No-op out for pair fire
            miend
            lda #snd_c2
            jmp dosound
?ss20           dex
            dex
        miend
        rts
        
        
            
strlfx  .byte $E4,$E6,$EA,$EB,$EE,$F0,$F1,$F4,$F6,$F8,$FA,$FC
strrtx  .byte $08,$08,$0C,$0E,$10,$10,$12,$15,$16,$18,$1A,$1C

;**********************************************
    .sbttl "Draw Shot Picture"             
;**********************************************
;* Entry A= LSB Scale Factor                  *
;**********************************************
drawshot    
		sta scalef+1        ;Size offset
        lda #00
        sta scalef
        jsr posvec      	;OK Mark, position and set scale
        ifmi
            rts             	;Skip if offscreen
        endif
        ldx temp9           ;Recall X
        lda shotyh,X        ;For intensity
        jsr getint      	;Get shot's intensity
        sta temp1           ;Might want this later
        ldx temp9           ;Skip if this shot is exploding
        cpx #nmsshots       ;Player Ship Shot??
        ifcc                ;yes
            ldy shotst,X
            ifmi
                rts
            endif
        endif
        ldx #shpsh_vpg      ;Add color and page select
        ora #$0A            ;Add color of shot
        jsr vgadd2
        VGADD_JSR(shipsh)   ;Add picture
        lda #00             ;Make sure not -
        rts 
        
;**********************************************
    .sbttl "Check Base"
;**********************************************
;* Check shots against enemy base station for *
;* possible hit. Called from mvenst as each   *
;* shot is moved                              *
;*                                            *
;* Inputs: X = Index of this shot             *
;**********************************************
ckbase  lda mzgame      ;Landing or Takeoff
        and #04
        ifne
            rts
        endif
        lda #00
        sta temp4+1     ;LSB 1 in case used
        ;ldy maznum      ;Which base star
		ldy maztype
        lda boszy,Y     ;Base 0 Y size
        sta temp5       ;Save Y size
        lda boszx,Y     ;Base 0 X size
        asl a
        sta temp4       ;X is size over 2
        rol temp4+1     ;Make 1 if overflow
		;X - Calc difference between station and shot into temp1
        lda statxl
        sec 
        sbc vdata       ;Shot position left in vdata
        sta temp1		;XL diff in temp1
        lda statxh
        sbc vdata+1
        sta temp1+1		;XH diff in temp1+1
        lda temp1+1     ;Reset Sign
        ifmi
            jsr dblneg      ;Abs of X difference affects temp1 value
        endif
		;See if X hits
        lda temp4
        sec 
        sbc temp1
        lda temp4+1
        sbc temp1+1     ;Compare to size
        ifcs                ;We hit in X, check Y
            lda statyl
            sec
            sbc vdata+2
            sta temp1
            lda statyh
            sbc vdata+3
            sta temp1+1
            ifmi
                jsr dblneg
            endif
            ifeq            ;MSB is 0
                sec
                lda temp1
                sbc temp5
                ifcc            ;We hit it
hitbas				lda #$80
					sta shotst,X    ;Kill this shot
					;*********************************************
					;* Removed as doesn't seem
					;* to be used.
					;*********************************************
					;sta stbcol      ;Indicate collision at base
					;*********************************************
					lda #snd_c3
					jmp dosound
                endif
            endif
        endif
        rts
        
;****************************************
;* Table of Base Star Sizes             
;****************************************   
boszx       .byte $51,$78,$FF,$7F
boszy       .byte $80,$90,$90,$A0

;****************************************
;* Rotating Beacon utility. Position at 
;* place of draw and pass color in reg A
;* Pass beacon number in Y              
;****************************************
beacon  sta temp1
        tya             ;Y has beacon offset
        clc 
        adc frame       ;Pick Intensity
        and #$1F
        lsr a
        lsr a
        tax 
        pha             ;Save this for below
        lda beaintn,X   ;Get Intensity
        ora temp1       ;Put back color
        ldx #becn_vpg   ;Stat, Page Select
        jsr vgadd2      ;Add Color Instruction
        pla             ;Recall above
        asl a           ;Words entry
        tay 
        lda beapic,Y
        ldx beapic+1,Y  ;JSRL to beacon picture
        jmp vgadd2      ;Bye
        
beaintn .db $40,$70,$A0,$F0,$A0,$70,$40

;****************************************
    .sbttl "Laser Shot"
;****************************************
;* Base gun (and whoever else wants it) 
;* laser. Will draw a 'slow' vector     
;* between the 2 points passed in lsdsxx
;* and lsstxx.                          
;****************************************
laser   lda mzgame
        and #$18            ;Get rid of it here
        ifne
            lda #00
            sta lasst
?ls1        rts
        endif
        lda #00
        sta vgbrit
        lda lasst           ;Active??
        beq ?ls1
        ldx #07         ;Copy laser vals into temps
        begin
            lda lstsxl,X
            sta temp4,X
            dex
        miend
        bit mzgame      ;3rd person play??
        ifpl
            jsr laser3      ;Adjust points for 3rd person
        endif
        jsr sclset      ;Position Scale
        jsr vgcntr      ;Center for first point
        lda #$FA
        ldx #$60+sparkle    ;Stat, Sparkle on
        jsr vgadd2
        lda lasst
        sec 
        sbc laspd           ;Laser Speed
        sta lasst
        ifmi                ;Drawing Outward
            jsr pt1         ;Start at Origin
            jsr vgvtr2      ;Position Origin
            jsr lasz            ;At this poing A= 8-F
            sec
            sbc #08         ;Now 2-9
            cmp #01         ;Don't go below 2
            ifcc
                lda #$6F            ;Set beam to move out
                sta lasst           ;Set Status
                lda #01         ;And hold this at 2
            endif
        else                ;Moving in
            jsr pt2         ;Point 2 is Origin
            jsr vgvtr2      
            bit lascol      ;Collision??
            ifmi
                lda #00
                ldx #$72
                jsr vgadd2      ;Add a little fire ball
                VGADD_JSR(sparkb)
            endif
            jsr vgcntr      ;Recenter
            jsr sclset      ;Restore Scale
            jsr pt2
            jsr vgvtr2      ;Reposition for draw back
            jsr lasz            ;Get bin scale figure
            ;At this point A = 0-7
            ifeq                ;Done
                sta lasst           ;Turn it off
                ldy #$8A            ;Close the gun
                ldx shipxh      ;Look at side of ship
                dex
                dex
                dex
                ifpl
                    sty gunctl+1
                else
                    sty gunctl
                endif
                rts             ;And leave
            endif
            eor #$FF
            and #07         ;Now have 0-7
            cmp #01         ;Don't get too large
            ifcc
                lda #01
            endif
            cmp #03         ;It's long enough to hit now
            ifcc
                ldx shipst
                ifne
                    ifpl                ;Only if not blowing up
                        pha             ;Save A
                        jsr lascl2      ;Always die for now
                        pla
                    endif
                endif
            endif
        endif
        jsr addscale
        jsr dif         ;Point back to other point
        lda #$20
        sta vgbrit      ;Draw this line
        jsr vgvtr2
        rts

;****************************************
    .sbttl "3rd Person Setup"
;****************************************
;* Corrects lsds** and lsts** for 3rd   *
;* person quad system                   *
;****************************************   
laser3  jsr pt1         ;Just move int vdata
        jsr cor3        ;Correct this point
        ldx #03
        begin
            lda vdata,x
            sta temp6,x
            dex
        miend
        jsr pt2
        jsr cor3
        ldx #03
        begin
            lda vdata,x
            sta temp4,x
            dex
        miend
        rts
        
;****************************************
    .sbttl "Laser/Ship Collision"
;****************************************
;* Check end of laser with ship's       *
;* current position                     *
;****************************************   
lascl2  lda #$80
        sta lascol      ;Signal Collision
        sta statst      ;Stop any station motion
        jsr blowship        ;Blow into Peices
        lda #g_sndstop
        jsr dosound
        jsr dodelay
        lda #snd_c1
        jmp dosound
        
;****************************************
    .sbttl "Add Proper Scale"
;****************************************
;* Add scale instruction generated above*
;* and check for a scale greater        *
;* (smaller in size) than 6 as the fast *
;* monitor does not allow scale 7.      *
;*                                      *
;* Input:   A = scale value (2 thru 7)  *
;*                                      *
;* Output:  Scale inst to VGRAM         *
;****************************************
addscale    clc 
        adc #01
        cmp #07
        ifcs            ;6 or greater stays 6
            lda #07
        endif
        ora #$70        ;Make it a scale instruction
        tax 
        cpx #$70        ;At positioning scale??
        ifeq    
            lda #00     ;Stick at proper position
        else
            lda lasst       ;Rest of scale
            ifpl                ;Closing
                eor #$FF
            endif
            asl a
            asl a
            asl a
            and #$70        ;Use bottom for bin scale
        endif
        jmp vgadd2
        
;*************************************
    .sbttl "Shift lasst Down 4"
lasz    lda lasst
        lsr A
        lsr A
        lsr A
        lsr A
        rts 

;*************************************
    .sbttl "Copy Point 1 to vdata"
pt1     ldx #03         ;Point 1 (origin)
        begin
            lda temp6,X
            sta vdata,X
            lda temp4,X
            sta temp2,X     ;In here for diff
            dex
        miend
        rts
        
;*************************************
    .sbttl "Copy Point 2 to vdata"  
;*************************************
pt2     ldx #03         ;Point 2 (target)
        begin
            lda temp4,X
            sta vdata,X
            lda temp6,X
            sta temp2,X
            dex
        miend
        rts
        
;*************************************
    .sbttl "Difference to Point 1"
;************************************
;* Calculates the difference from   *
;* the value in vdata to point and  *
;* places the results in vdata      *
;************************************   
dif     sec 
        lda temp2
        sbc vdata
        sta vdata
        lda temp2+1
        sbc vdata+1
        sta vdata+1
        lda temp3
        sec 
        sbc vdata+2
        sta vdata+2
        lda temp2+3
        sbc vdata+3
        sta vdata+3
        rts 



;***************************************
; Outtesser - Draws appropriate
;             tokens for saved values
;***************************************
; temp4+1 contains the index into the 
; tesseract table
;***************************************
outtesser
		;a position vector first
		lda #8
		ldx #3
		jsr vgvtr5		;spacing
		lda #$D0
		sta temp1
		ldx temp4+1
		lda gtesser,X
		;LS bits contain the tesseract flags
		; 01 - Token A
		; 02 - Token B
		; 04 - Token C 
		; 08 - Token D
outtes2	ifne
			;pha
			;vgadd_scale(ywin_off,binscal2,80)	;slightly smaller for the tokens			
			;pla
			;Do 4 tokens + Final Station Badge
			ldy #0
			sty temp2
			begin
				lsr A
				ifcs
					pha 
					;temp2 contains the Token index 0-4
					;temp1 contains the brightness
					jsr drawtok
					lda #6
					ldx #0
					jsr vgvtr5		;spacing
					pla
				endif			
				inc temp2
				ldy temp2
				cpy #4
			eqend
			;pha
			;vgadd_scale(ywin_off,binscal2,0)		;set scale back to normal
			;pla
		endif
		rts
		
outbadge
		;lda #$D0
		;sta temp1
		ldx temp4+1
		lda gtesser,X
		and #$10
		ifne
			ldy #4
			sty temp2
			;temp2 contains the Token index 0-4
			;temp1 contains the brightness
			jsr drawtok
			lda #6
			ldx #0
			jsr vgvtr5		;spacing
		endif
		rts
		
		
;******************************************
; Draw the token on the screen
; temp2 contains the Token index 0-4
; temp1 contains the brightness
; 
; The last token(4) is the homeworld token
; for high score table only.
;******************************************
drawtok ldy temp2
		lda temp1
		ora tokcolor,Y 
		ldx #tok_vpg     
		jsr vgadd2
		lda temp2
		asl A 
		tay
		lda tokens,Y
		ldx tokens+1,Y
		jsr vgadd2
		ldy temp2
		cpy #4
		ifne
			lda temp1			;MSB brite value
			ora #colred2
			ldx #tok_vpg
			jsr vgadd2
			VGADD_JSR(tokhex)	
		endif
		rts
		
.export outtesser,outbadge,outtes2