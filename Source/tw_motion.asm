;*****************************************************
    .TEXT ".TWMOT."
;*****************************************************
    .title "TWMot - TWEB Motion"
;*****************************************************

offpos  = -6        ;Man's off screen position

;***********************************************************
    .sbttl "Place and Output Picture"
;***********************************************************
;* This routine sets up and adds the man picture to the    *
;* display list. It also times when the next picture       *
;* change should take place based upon the current         *
;* picture sequence in progress, and calls nextpic         *
;*                                                         *
;* Inputs:  scal,frame,piccur,sittime,direct,picseq,picdely*
;*                                                         *
;* On Exit: Adds JSRL to VGLIST, updates picdely,sittime   *
;*                                                         *
;* Stack:   2 bytes (JSRL call)                            *
;***********************************************************
manpic  lda objst
        ifeq
?mpr	    rts			
        endif
        ldy #$30            ;Linear Part
        bit mzgame  
        ifpl                ;Not in mazegame if +
			ifvs				;In Tube??
				ldy	#$94
				;allow draw
			else
				lda vxstat
				beq ?mpr
				lda vxstat
				and #$70			;Allow Rex on planet surface
				cmp #$40
				bne ?mpr			;leave for all except planet surface 
				lda hwstat
				cmp #_strex+1
				bcc ?mpr
				cmp #_stelevatordn
				beq ?mpr
				lda picseq
				ifmi
					jsr stpg3
					jsr emote			;if seq is minus, then it specifies a dance move on Maynard
					jmp stpg0
					;Exits here!!!!
				endif	
			endif
        endif
		lda #02         ;Scale 1 for position
        jsr vgscal      ;Set Scale
        jsr vgcntr      ;Center
        ldy #00
        sty vgbrit      ;Blank Vector
        ldx #xmot       ;Vector from xmot
        jsr vgvctr      ;Place this object
        lda piccur
        cmp #idx_pic24        ;Stopped??
        ifeq
            lda picseq      ;Stopped??
            ifeq                ;Yes
                sta upflg           ;Standing, clear flag
                lda frame
                and #01
                ifeq
                    inc waitim      ;Still waiting??
                    ifeq
                        dec waitim      ;Don't allow to pass back to 0
                    endif
                endif
                lda waitim
                ifmi                ;Long Enough??
                    lda rtcol
                    ora ltcol       ;Near Wall??
                    ifmi
                        lda #idx_pic28
                    else
                        lda #idx_pic27     ;We shall wait
                    endif
                    sta piccur      ;Use this picture now
                endif
            else
                lda #$40
                sta waitim      ;Reset Wait Time
            endif
        endif
        bit mzgame        ;No Shield in Space!!
        ifmi
			lda manstat
			ifpl                ;No Shield if dying
				bit msk20_2     ;Shields On??
				ifne                ;Yes
					lda #snd_i1a        
					jsr dosound     	;Shield Sound
					lda #$FF            ;Brightness
					ldx #shld_vpg+sparkle   ;Sparkle too
					jsr vgadd2
					lda frame
					ifmi
						eor #$FF
						and #$7F
					endif
					lsr A
					lsr A
					clc
					adc #$50            ;Min Size
					tay
					lda #02
					jsr vgscal
					VGADD_JSR(shield)
				endif
			endif
        endif
        lda mestim
        ifne
			cmp #02				;First Count (actually starts @ 2)
			ifeq
				lda rands
				cmp #4d
				ifeq
					lda #sp_rexcomeon		;Come On!
					jsr dosound
					lda #0
					sta mestim				;Reset whole thang!
				endif
			else
				;Display Idiot Message If needed
				lda #00         			;Display the "hold button for higher jumps" message
				ldx #$70
				jsr vgadd2
				VGADD_VCTRS(-2,-16,hidden)
				ldx #mpub0
				stx temp4
				jsr msgnop
				VGADD_VCTRS(-10,-80,hidden)
				ldx #mpub1
				stx temp4
				jsr msgnop
				VGADD_VCTRS(32,-81,hidden)
			endif
        endif
        ldy #$68            ;Set Linear Size
        lda mzgame          ;In Tube??
        ifpl                ;If +, Not in Maze
			ldy #$98            ;Tube Scale
			lda vxstat
			ifne
				ldy #$88			;Homeworld scale for rex
			endif
        endif
        lda #02         ;At Scale 2
        jsr vgscal      ;Add Scale
        lda piccur
        cmp #idx_pic27     ;Waiting??
        ifeq                ;Yep
            lda teeter      ;In a teeter position?
            ifne                ;Could be
                ora #$80            ;Make it look that way
                sta teeter
            endif
        endif
        lda objst+zstuf+1
        ifmi
            lda #$FB            ;Show Boots on Man
            ldx #boot_vpg
            jsr vgadd2
            lda gboot+$0a
            ldx gboot+$0b
            jsr vgadd2
            jmp boots2
        endif
        lda objst+zstuf+1       ;Maybe also show man levitating on force field
        cmp #02
        ifeq
            lda jumpst          ;Check button pressed
            ifmi
boots2          lda #($A0+colred2)
                ldx #boot_vpg
                jsr vgadd2
                lda frame
                and #03
                asl a
                tay
                lda gboot+2,Y       ;Four Frame Sequence
                ldx gboot+3,Y
                jsr vgadd2
            endif
        endif
        bit face                ;Slamming Face into Wall??
        bmi bad4                ;Yes, so skip below
        lda frame
        ldy jumpst          	;Jumping??
        ifmi                    ;Yep
            ldy #00
            sty upflg               ;Obviously not getting up'
            ldy piccur
            cpy #idx_pic15            ;Jumping??
            ifeq                    ;yep
                and #$10                ;Flip Bit select
                lsr A
                lsr A
                ora #mpic_vpg           ;Page Select
            else
                jmp bad4                ;(Bad Habit) Do Normal Pic
            endif
        else                    ;No Jumping, Sitting?
            bit teeter          ;Or maybe Teetering
            ifpl                    ;nope, not teetering
                ldy sittime
                ifne                    ;Sitting on Floor??
                    and #xflip
                    ora #mpic_vpg
                else
bad4                lda #mpic_vpg       ;Stat base and page select
                    ldy direct
                    ifmi
                        ora #xflip      ;Add X Flip
                    endif
                endif
            else
                and #$3C            ;Mask to alt teeter and not teeter
                lsr A
                lsr A
                tay
                lda teettbl,Y        ;Change direction every 4 frames
            endif
        endif
        ldx piccur
        cpx #idx_pic28         ;Waiting??
        ifeq                    ;yep
            ldx rtcol              ;Right Collision??
            ifpl                   ;No, lean other way
                ora #v_xflip
            endif
        endif
        bit manstat         ;Is he dying??
msk10_2 ifmi    
            ldy oxygen          ;Death due to lack of air??
            beq ?npi15
            ora #sparkle        ;Add Sparkle to old stat
            tax
            lda manstat
            and #$F0
            eor #$F0
        else
?npi15      tax                 ;Just save A as is
            lda #mancol+$e0     ;Add color and intensity
            ldy player          ;Player 2??
            ifne                ;yes change color
                lda #mancol2+$a0       
            endif
        endif
		;***************************************************************
		;JMA - Removed this because I suspect it isn't used anymore but 
		;      putting this comment just in case
        ;sta temp5           ;Save for Later (Maybe)
		;***************************************************************
msk20_2 jsr vgadd2          ;Add Page Select
        ldx picseq          ;Get Sequence
        dec picdely         ;Picture Delay
        ifmi                    ;Ready for next pic??
            lda delaytb,X            ;New Delay Amount
            sta picdely
            jsr nextpic
        endif
        lda manstat         ;Dying??
        ifmi
            ldy oxygen          ;Due to Lack of Oxygen??
            ifeq                ;Yep!
                pha                 ;Save this
                sec
                sbc #$80
                lsr A
                lsr A
                lsr A
                lsr A
                tax
                lda choktbl,X       ;Get Choking Pics
                tax                 ;Save pic
                pla                 ;Recall manstat
                cpx #idx_pic36
                ifne                ;Last one is fixed
                    and #08             ;Flip Code
                    ifne
                        inx
                    endif
                endif
                stx piccur
            else
                ldy #idx_pic24
                sty piccur
                and #$F0
                eor #$F0            ;Want him to fade out
                eor frame
                and #$F0
                eor frame           ;Flash Color
                ldx #mpic_vpg
                jsr vgadd2          ;Add Color and Stat
                VGADD_JSR(pic37)    ;Add Skeleton pic
                rts                 ;and we are out of here
            endif
        endif
        ;*************** Now output picture to list *************************
        lda piccur          ;Current Pic
picout  ldy objst+zstuf+2       ;Escape pod out there?
        ifpl
            cpy #02             ;Climbing??
            ifcs
                lda #02
                sta rompg           ;PAGE CHANGE!!!!! PAGE 2
                jmp drpod2          ;Draw Escape Pod instead of man
            endif
        endif
        ldy objst+zstuf+1       ;Magic Boots
        ifmi                    ;Freeze is on, Man w boots on is displayed
            iny
            cpy #$B0
            ifcs
                ldy #02         ;Leave this mode after 20 frames
                lda #$A0
                sta gamest      ;Return to normal play (non-freeze)
            endif
            sty objst+zstuf+1	;save new boots status
            lda #$15 ;idx_pic15        ;Show a specific man picture (jumping pic)
        endif
        bit tumble      ;Clear tumble bit in strange situations
        ifmi
            cmp #idx_pic27 ;Is he tapping his foot??
            beq ?op10
            cmp #idx_pic24        ;Just standing there
            ifeq
?op10           inc unstik          ;Another Frame Stuck
                ldx unstik
                cpx #07             ;After a certain number of frames, clear tumble
                ifcs
                    ldx #00
                    stx tumble
                endif
                jmp ?op20           ;Don't Clear Unstick
            endif
        endif
        ldx #00         ;No lockup condition this frame, clear unstik
        stx unstik
?op20   cmp #idx_pic27         ;Prevent man on side of screen on tube_vpg
        ifne
            bit mzgame
            ifvs                    ;Make sure a man in attract
                tay
                lda ymot
                sec
                sbc #$55
                lda ymot+1
                sbc #$FE
                bpl ?op30           ;Make sure a man above a certain height
                tya 
            endif
        endif
        pha 
        asl a           ;Times 2 for index use
        tay 
        bit teeter
        ifmi
            ldy #(idx_pic38*2)           ;Teeter picture if teetering
        endif
        lda mansrc,Y        ;Get Source
        ldx mansrc+1,Y
        jsr vgadd2      ;Add man's pic
        pla
        cmp #idx_pic27     ;Tapping Foot??
        ifeq                ;Yes
            bit teeter      
            ifpl                ;not teetering
                bit mzgame          ;Maze playing?
                ifmi                
?tippytap			lda frame
					and #$0F
					ifeq     
						lda #snd_i8     ;'tap' foot
						jsr dosound
					endif
				else				;in tube here
					lda vxstat
					ifne
						lda nenemy
						cmp #_stscroll		;Planet surface only
						beq ?tippytap
					endif
                endif
                lda frame
                and #08
                ifeq
                    laljsr(leg1)
                    lxhjsr(leg1)
                else
                    laljsr(leg2)
                    lxhjsr(leg2)
                endif
                jmp vgadd2
            endif
        endif
?op30   rts

;***********************************************************
    .sbttl "Next Picture"
;***********************************************************
;* This routine gets the next picture in the sequence      *
;* and handles and special case hold requests.             *
;*                                                         *
;* Entry:   Current pic number (piccur)                    *
;*                                                         *
;* Exit:    New pic code updated                           *
;***********************************************************    
nextpic bit landflg
        ifmi
            lda piccur
            cmp #idx_pic24        ;Stop pic??
            ifeq                ;yes
                bit stopflg     ;Force to stop seq (from Land)
                ifmi
                    lda #stopseq
                    sta picseq      ;Sets to #stopseq
                    sta stopflg     ;Clear flag
                    sta landflg     ;Not landing either
                endif
            else
                cmp #00         ;Stride
                ifeq
bad8                sta landflg
                    lda #runseq
                    sta picseq      ;Force to run
                else
                    cmp #idx_pic4        ;Other stride
                    beq bad8
                endif
            endif
        endif
        lda picseq      ;Current Sequence
        asl a           ;*2 for words
        tax             ;into X
        lda pictbl,X    ;Get Low byte of Table Address
        clc 
        adc piccur      ;Add Current Pic
        sta picind      
        lda pictbl+1,X
        adc #00         ;Prop carry if one (High Byte)
        sta picind+1        ;Create Indirect Pointer for this seqs table
        ldy #00
        lda (picind,Y)      ;Get Next Pic
        ifmi                ;Possible Foot Sound
            and #$7F            ;Drop foot bit
            cmp piccur      	;Already at this pic?
            ifne                ;Not the same
                pha             	;Save pic code
                lda #snd_i8     
                jsr dosound
                pla
            endif
        endif
        sta piccur  
		bit gamest      ;Skip this if in tube_vpg
		ifvc
			;bit vxstat			;No player control in homeworld
			;ifvc
				cmp #idx_pic24        ;Stopped??
				ifeq
bad6                lda #00
					sta dcreq
					lda rgdd            ;Get new direction
					and #$80            ;Only direction please
					sta direct          ;Update direction
				else
					cmp #idx_pic27         ;Waiting??
					beq bad6
					cmp #idx_pic28
					beq bad6
				endif
			;endif           ;Fall through....
        endif
		;***********************************************************
			.sbttl "Update Velocity"
		;***********************************************************
		;* Updates X and Y velocities using Rolly-Gig data         *
		;* and jump button flags                                   *
		;***********************************************************        
upvel   lda mzgame      ;Doing Maze??
        and #$4D
        ifne            ;Doing Tube, skip this
?uv1        rts
        endif 
		;lda vxstat			;removed
		;ifeq
			lda gamest      ;Skip this on completed maze
			and #$48        ;Succesful exit or just entered??
			bne ?uv15       ;Will skip X velocity if yes
		;endif
        lda tumble      ;Did he hit his head??
        ifne                ;yep
?uv10       lda #00         
            sta velxh           ;Stop X Motion
?uv15       jmp noact           ;And do nothing else
        endif
        bit upflg       ;Getting up??
        bmi ?uv10       ;Yep, don't allow to move
        lda manstat     ;Only when active
        bmi ?uv10
        beq ?uv10      	;2 cases of no action
        ;X Update
        lda velxh      ;Are we moving??
        ifmi
            clc
            adc #01         ;-1 check
        endif
        ifne
            lda #$60        ;Reset Wait Timer
            sta waitim      ;He moved
            lda piccur
            cmp #idx_pic27     ;Waiting??
            ifcs                ;yes
                lda #idx_pic24
                sta piccur
            endif
        endif
        lda rgdd
        bit mzgrnd      ;On ground??
        ifpl                ;nope, must go down
            lda velxh
            ldx #04
            jsr div2x           ;1/8 of velocity
            sta temp1           ;Save
            lda velxh
            sec
            sbc temp1           ;A = 7/8 of old velocity
            sta temp1           ;Hold this
            lda rgdd            ;Control Velocity
            ldx #03             ;Use 1/6 Control
            jsr div2x
            clc 
            adc temp1           ;velxh(new)=7/8(velxh(old)+1/8(Control_velocity)
            ifvc                ;No Overflow??
                sta velxh      ;Store new
            endif
        else
            clc					
            adc velxh
            bvs ?uv20           ;Already wrapped, max out
            clc
            adc rgdd            ;else, add data in again
            ifvc                ;No overflow in speed
                cmp #$80
                ror A
            else                ;Stick at max
?uv20               ifmi                ;If -, was + so max at +
                    lda #$60            
                else
                    lda #-$60           ;Else max out at minus
                endif
            endif
            sta velxh      ;Update X
        endif
        lda velxh      ;Check for possible wall hits
        ifmi                ;Moving left
            bit ltcol
            ifmi                ;And Left wall Collision
                lda #00         ;Will stop X left motion
            endif
        else            ;Moving Right
            bit rtcol       ;Right Collision??
            ifmi                ;yep
                lda #00         ;Stop X Motion
            endif
        endif
        sta velxh      ;0 or return velxh
        ;Y Update
noact   bit rtcol       ;If in frozen counter mode shouldn't hit anything
        ifmi                ;Right??
            lda gamest
            and #$EF
            sta gamest      ;If do, start up counters again
        endif
        bit jumpst      ;Jumping??
        ifmi                ;yes
            lda objst+zstuf+1
            cmp #02
            ifeq
                lda velyh
                ifmi
                    bit mzgrnd
                    ifmi
                        lda #00
                    endif
                endif
                clc
                adc #02
                cmp #maxvup
                ifpl
                    lda #maxvup
                endif
                sta velyh  
            else
                lda #maxvup         ;Max Speed Up
                sec
                sbc velyh          ;Approach maxvup if jump
                ldx #03
                jsr div2x
                sec                 ;Always add something
                adc velyh
                sta velyh          ;velyh(new)=velyh(ol)+1/32(maxvup-velyh(old))
            endif
        else
            bit mzgrnd      ;On Ground??
            ifpl                ;No, and not jump, must be falling
                bit tumble      ;If hit head, fall faster (so don't fall up to next floor!!!!)
                ifmi
                    lda velyh
                    ifpl
                        sec
                        sbc #02
                        sta velyh
                    endif
                endif
                lda #maxvdn     ;Max down velocity
                sec 
                sbc velyh
                ldx #06
                jsr div2x
                clc
                adc velyh
                sta velyh      ;velyh = velyh-1/64(maxvdn-velyh)
            else
                lda #00
                sta velyh      ;Bring to a stop
            endif
        endif
        ;************* Entry from TWShip *****************
rgdr    lda rgdd            ;Slow down read speed
        cmp #$80
        ror A
        cmp #$FF            ;Damn, -1 problem not going to 0
        ifeq
            lda #00
        endif
        sta rgdd
        rts
        
;***********************************************************
    .sbttl "Man's Left/Right Position"
;***********************************************************
;* Using Man's speed, move man left or right and hold till *
;* proper slow down. If 'scrflg',D7=1 then we are holding  *
;* at one edge location. If D6=1, then left hold           *
;***********************************************************            
m_targx lda #02         ;Bit 2 indicates just in, wait for ground
        bit mzgame      ;Doing other than maze??
        ifpl
?mp5        rts
        endif
        bne ?mp5            ;Wait awhile
        lda objst+zstuf+2   ;Force man motion if in escape pod
        bmi ?mp7
        cmp #02
        ifcs
            dec epodfr
            ldy epodfr      ;Need new velocity??
            ifmi
                adc #00
                cmp #$0D            ;Time to Land??
                ifeq
                    lda #00
                    sta velyh
                    lda #$80
                    sta objst+zstuf+2
                    ;This is needed until table is correct
                    lda #$0B
                    sta objxh+zstuf+2
                    lda #$FC
                    sta objyh+zstuf+2
                    lda #$08
                    sta epodgr
                    lda #snd_d4         ;Splash
                    jsr dosound
                    lda #-$28
                    sta velxh
                    lda #runseq
                    sta picseq          ;He runs from crashing pod
                    ;End stuff needed until table is correct
                    bpl ?mp7
                endif
                sta objst+zstuf+2
                tax
                lda pode-3,X
                sta epodfr
            endif
            ldx objst+zstuf+2
            lda podx-3,X
            sta velxh
            lda pody-3,X
            sta velyh
            lda podg-3,X
            sta epodgr
        endif
?mp7    ldx #02         ;Will do Y at same time
        begin
            lda scrflg,X        ;Already Holding??
            ifmi                ;yes, must look for slow down
                txa             ;is X=0
                ifne
                    lda velyh-2,X
                else
                    lda velxh,X
                endif
                ifmi
                    jsr neg         ;Need + Numbers Only
                endif
                cmp #08
                ifcc
                    lda #00
                    sta scrflg,X        ;Re-Center Now
                endif
            else                ;Not on Edge, check for possible re-center
                txa             ;Which vel to use
                ifne
                    lda velyh-2,X      ;X is 2 here
                else
                    lda velxh,X        ;Check for change of direction
                endif
                ifpl                    ;Guess want Left
                    ldy #$C0
                else
                    ldy #$80            ;Else guess Right
                    jsr neg         ;+ Velocity
                endif
                cmp velmax,X        ;Velocity to start move
                ifcc                ;nope
                    ldy #00         ;Will Shut Down
                endif
                tya
                sta scrflg,X        ;Save Flag
            endif
            ;**********************************************
            ;* Now check flag and add offsets as needed   *
            ;* This routine will do all approx scrolling  *
            ;**********************************************
            lda scrflg,X        ;Need to Scroll??
            asl a           ;Set C and M from M and V
            ifcs                ;yep
                ifmi                ;and wish to go left
                    lda xmot+1,X
                    cmp limits,X        ;Read limit for this direction ifne
                    ifne
                        txa             ;Is this X
                        ifeq                ;This is X!
                            bit gamest      ;Exit Special
                            bvs ?mp15
                        endif
                        jsr sub2
                    endif
                else                ;Want to move right
                    lda xmot+1,X
                    cmp limits+1,X      ;Check right limit now
                    ifne
                        jsr add2
                    endif
                endif
            else                ;Wish to move to park postion
                sec 
                lda xmot,X      ;LSB
                sbc xstopl,X
                sta temp7       ;Save LSB
                lda xmot+1,X
                sbc xstop,X     ;Stop Value
                ifmi                ;At Left(known not 0)
                    jsr add2
                else                ;Approach from Right
                    ifne
?mp10                   jsr sub2
                    else                ;Close in to 0
                        lda temp7       ;If it is on the - side, it will go + next time!!
                        and #$F8            ;0 (+/-) 7
                        bne ?mp10
                    endif
                endif
            endif
?mp15           dex
            dex
        miend
        rts
        
podx        .byte $00,$00,$F7,$E6,$E0,$E8,$EE,$EE,$EE,$EC
pody        .byte $10,$20,$20,$18,$10,$10,$04,$00,$F8,$F8
pode        .byte $10,$50,$24,$2C,$40,$10,$10,$10,$10,$10
podg        .byte $00,$00,$01,$02,$03,$03,$04,$05,$06,$07

;*************************************************
;* Subroutines for above... add and subtract to  *
;* current position of scroll.                   *
;*************************************************
add2    lda xmot,X
        clc 
        adc scrolv,X
        sta xmot,X
        lda xmot+1,X
        adc #00
        sta xmot+1,X
        rts 
        
sub2    lda xmot,X
        sec 
        sbc scrolv,X
        sta xmot,X
        lda xmot+1,X
        sbc #00
        sta xmot+1,X
        rts 
        
;Scroll Velocities..... X=0 for H vel and X=2 for Y vel
scrolv  .byte 6,0,4

;Stop Limits for Motion
xstop   .byte 0,0,-1
xstopl  .byte 0,0,$40

;****************************************************
;* Limits for X and Y scroll (high byte check)      *
;* NOTE: targx will scroll until the high byte      *
;* of the scroll value is equal to the value in     *
;* this table. Therefore, to scroll to -2 the       *
;* value in the table must be -3 to compensate      *
;* for the 2's complement number.                   *
;*                                                  *
;* The values are: xleft, xright, yleft, yright     *
;****************************************************
limits  .byte -2,1,-3,1

;Start Scroll Min speed 
velmax  .byte $18,$00,$20

;**************************************************
    .sbttl "Maze Scroll Calculations"
;**************************************************
;* This routine moves the man according to his    
;* velocities as set above. This routine updates  
;* the 'maze' position, and then calls manpic     
;* It falls through to colobj as well.           
;**************************************************
posit   lda tspark          ;When transporting, maze does not scroll for a bit
        ifne
            jmp manpic
        endif
        lda #00
        sta temp1
        sta temp2           ;(Y)
        bit mzgame          ;Doing Tube??
        ifvs
			;Normal Tube Motion
			ldy #00
			lda velyh
			ifmi
				dey
			endif           ;Sign Extend 16 bits
			clc
			adc ymot
			sta ymot
			tya
			adc ymot+1
			sta ymot+1      ;Update Y postion
			lda velxh      ;Do simple motion here
			clc
			adc xmot
			sta xmot
			lda #00
			adc xmot+1
			sta xmot+1      ;Move Man
			ifmi
				cmp #offpos
				beq ?msc10          ;Man is holding off screen
			else
				cmp #02         ;Plane Position MSB
				ifcs                ;Almost there??
					lda xmot            ;Check for LSB
					cmp #$60
					ifcs                ;There??
						lda xmot+1      ;Man Active??
						cmp #offpos         ;Off screen hold place
						ifne
							lda #00             ;Turn off man
							sta velxh          ;Set Speed to 0
							lda #$10
							sta frame           ;0 Frame from constant wait time
							lda #offpos         ;Store man off screen
							sta xmot+1          ;Hold off screen
							lda #snd_launch            
							jsr dosound         ;Start Launch Sound
						else
?msc10                      lda frame
							and #$3F
							ifeq                    ;Time to Launch
								lda #$80
								sta tstat               ;Launch
							endif
						endif
					endif
				endif
			endif
			jmp ?posy
        endif
		lda vxstat
		ifne
			;homeworld animation, Rex moves directly no maze scrolling so we control him
			ldy #00
			lda velyh
			ifmi
				dey
			endif           ;Sign Extend 16 bits
			ldx #02         ;Divide by 2^x
			jsr div2x
			clc
			adc ymot
			sta ymot
			tya
			adc ymot+1
			sta ymot+1      ;Update Y postion
			;ground check @ $FE80
			ifmi
				cmp #$FE
				ifeq
					lda ymot
					cmp #$81
					ifcc
						lda #$80
						sta mzgrnd
					endif
				endif
			endif
			
			lda velxh
			ifne
				cmp #-1         ;Skip -1 too, as it stays here if minus
				ifne
					ldx picseq      ;Update Position??
					ifpl            ;Only if not in emote
						ldy #00             ;Sign Extend
						lda velxh           ;Add in X Velocity
						ifmi
							clc
							adc #01         ;Must be at least -1
							ifmi                ;In case we went back to 0
								dey             ;Sign Extend
							endif   
						endif
						lda velxh      ;Do simple motion here
						ldx #02         ;Divide by 2^x
						jsr div2x
						clc
						adc xmot
						sta xmot
						tya
						adc xmot+1
						sta xmot+1      ;Move Man
					endif
				endif
			endif
			jsr manpic		
			rts
			;***********************************
			;EARLY EXIT FROM HERE
			;***********************************
		endif
        lda velxh
        ifne            ;Skip if no X Velocity
            cmp #-1         ;Skip -1 too, as it stays here if minus
            ifne
                ifmi                ;Moving Left
                    bit ltcol           ;Left Colision
                    bmi ?posy           ;Skip if left collision moving left
                else                ;Moving Right
                    bit rtcol           ;Right Collision
                    bmi ?posy
                endif
                ldx picseq      ;Update Position??
                ifne            ;Nope!!
                    lda mazexl
                    sta oldxl           ;Save old position
                    ldy #00             ;Sign Extend
                    lda velxh           ;Add in X Velocity
                    ifmi
                        clc
                        adc #01         ;Must be at least -1
                        ifmi                ;In case we went back to 0
                            dey             ;Sign Extend
                        endif   
                    endif
                    ldx #02
                    jsr div2x           ;2X cmp#80 ror
                    clc 
                    adc mazexl          ;Move X Low
                    sta mazexl          ;Move Maze
                    tya                 ;High Byte
                    adc mazexh
                    sta mazexh
                endif
            endif
        endif
?posy   jsr m_targx     ;Offset Routine
        ldy #00         ;Now do Y
        lda velyh      ;Add in Y Velocity
        ifmi
            dey             ;Sign Extend
        endif
        ldx #02         ;Divide by 2^x
        jsr div2x
        clc 
        adc mazeyl
        sta mazeyl
        tya 
        adc mazeyh      ;Prop to High Byte
        sta mazeyh
        bit mzgame      ;Are we in tube? Homeworld doesn't make it this far, exits above.
        ifpl
            jsr manpic
            lda #$80
            sta mzgrnd      ;Always on ground
            rts
        endif
        ldy objst+zstuf+2 
        cpy #02
        bcs ?msc20      	;Skip if in Pod
        cmp #$FC            ;Top Line Position
        ifcs                ;At least on top line
            bit mzgrnd      ;On the ground??
            bmi ?msc15      ;If on the ground, skip V checking
            lda mazeyl      ;LSB check
            cmp #gndv+08        ;Above Ground
            ifcs                ;Yes
?msc15          lda gamest      ;In Maze Already
                bit msk20_2     ;Already in Maze??
                ifne            ;yes
                    and #$DF            ;Clear 'already in' bit
                    ora #$40            ;Set 'Exit' bit, this also stops any new max sounds from starting
                    sta gamest          ;New Status
                    lda #00
                    sta jbstat
                    sta jumpst          ;No more jump
					;***********************************************************************
					lda #-1
					sta maxsnd			;Stop any max sounds happening
					;***********************************************************************
					;BUG #215: Max sound still playing after sucessful maze exit
                    lda #snd_kilmax                    
                    jsr dosound         ;Hero Theme
                    jsr do3del
                    ;***********************************************************************
                    lda #snd_hro
                    jsr dosound         ;Hero Theme
					lda #$28
                    sta velxh           ;Push him toward ship
                    lda velyh
                    clc
                    adc #$10
                    sta velyh           ;Give him a bit of boost upward
                    lda gamest
                    ora #$10            ;Stop Clocks
                    sta gamest
                endif
            else                ;Could be falling in from first entry
                cmp #gndv-08        ;At least below line
                ifcc
                    lda gamest
                    bit msk08           ;Just Entering??
                    ifne                ;yep
                        and #$E7            ;Start clocks, drop entering bit
                        sta gamest
                        lda #00
                        sta velxh           ;Stop X Motion
                    endif
                endif
            endif
        else                ;Not above so set in flag 
            bit outflg      ;In a border stamp? check for maze entrance here
            ifpl                ;no
                lda objst+zstuf+1   ;If man is frozen with boots on, leave him alone
                ifpl
                    lda gamest      ;Status
                    ora #$20        ;Set Entry Flag, Close Doors
                    and #$BF        ;Clear Exit bit
                    sta gamest      ;Save new status for maze entrance
                endif
            endif
        endif
?msc20  jsr manpic      ;Place and output picture
        lda #00
        sta teeter      ;Zero teeter for next frame
        ldx #zman       ;Collision index for the man
        stx zindex
        jsr stloc       ;Go do Collision routine
        ; JMA - 06042020 - These do not seem to be needed since botcase is going to load Y from zindex and zindex
		;                  is already set above
		;ldy #zman       ;Set for botcase
        ;sty zindex
        jsr botcase     ;Maybe set teeter to 40
        jsr manact		;Reacts to walls
		;**********************************************
        ;Fall Through to do collisions too!!! 
		;**********************************************
		.sbttl "Man, Object Collision Routine"
		;**********************************************
colobj  lda manstat     ;Is man already dead or dying??
        ifeq
?moc1       rts             ;Then just leave
        endif
        bmi ?moc1       ;He is dying, so leave
        lda tspark      ;Stop collisions if beaming
        bne ?moc1
        ;bit vxstat      ;No collisions on animation routines
        ;bvs ?moc1
        lda #01			;PAGE CHANGE!!!!! PAGE 1 
        sta rompg
        ldx #ztop2-1    ;Top of Collision Loop
coloop  stx zindex      ;Save index
        cpx #zreactor   ;Reactor
		ifeq
			lda limbo,X
			ifne
				;we have a mega-reactor here
				jsr colchkx
				ifeq
					lda temp1		;Get X MSB
					cmp #$D2		;mega left-side
					bcs moskip
					lda mazeyl
					clc
					adc #$34		;fudge factor to correct from mega to standard position for collisions (drop down by $34)
					sec
					sbc objyl,X
					sta temp1
					lda mazeyh
					sbc objyh,X
					sta temp1+1			;0 or 1 is a hit
					beq ?tocol			;collision
					ifpl
						dec temp1+1
						ifeq
?tocol						jmp ?moc12
						endif
					endif
				endif
				jmp nxto
			endif			;Fall out to normal checking for collision
		else			;Only non-reactor stuff
			cpx #zspecial   ;Skip the ship here
			beq moskip
			cpx #zdisc      ;Special Objects
			ifcc
				lda objst,X     ;0 if inactive(kept in position)
				bne ?moc3       ;Check not exploding if active
				cpx #ztop
				bcs moskip
				lda limbo,X
				beq moskip       ;If not in limbo either, skip it
				pha             ;Find which transporter to flash
				and #07
				tay
				pla
				ifpl
					tya
					eor #01
					tay
					lda limbo,X
				endif
				clc
				adc #08         ;Else advance Limbo timer
				ifcs
					lda stasav,X
					sta objst,X     ;Restore Status
					lda #00
					beq ?moc2
				endif
				ifvs
					pha
					lda #snd_i6
					jsr dosound
					pla
				endif
?moc2       	sta limbo,X     ;In any event, set the active bit for the transporter
				lda tranhi,Y
				ora #$80
				sta tranhi,Y
				jmp nxto            ;Get it back from the transporter after 16 frames
			endif
			
			lda objxh,X         ;Active??
			beq moskip          ;0 is inactive, so skip it!
?moc3   	ifmi                ;Is dying, skip this one too
moskip      	jmp nxto
			endif
		endif
bad9    jsr colchkx
        ;returns zero if in same stamp
        bne moskip
        lda temp1		;Get X MSB
        cpx #zcann      ;Different collision if laser cannon 
        bcc ?moc5           
        cpx #zcann+nmcann 
        ifcc                ;Yes, it is a cannon 
            lda canngr-zcann,X
            and #$0E
            tay
            lda temp1
            cmp cannsize,Y
        else
?moc5       cmp xsize-1,X       ;See if we can touch
        endif
        ;-1 as no table entry for the man vs. man
        bcs moskip
        lda mazeyl
        cpx #ztrpp      ;Trip plates are on floor
        ifcs                ;Is a trip plate
            cpx #zonew      ;One way walls are above floor, bypass this
            ifcc
                lda mazeyh      ;Man on same line?
                cmp objyh,X     ;Same line as trip plate?
                bne ?moc10      ;No
                lda ground      ;Man on Ground??
                bmi ?moc12      ;If on ground we hit it!
                bpl ?moc10
            endif
        endif
        cpx #ztop
        ifcs                ;Specials (Non-Motion Objects)
            sbc lsbsy-ztop,X
        else
            sec
            sbc objyl,X
        endif
        sta temp1
        lda mazeyh
        sbc objyh,X
        sta temp1+1
        ifmi
            lda side
            ora #$40            ;Set to Below hit
            sta side
            jsr dblneg
        endif
        ifne
?moc10      jmp nxto
        endif
        lda temp1
        cpx #zcann      ;Difference Collision if Laser Cannon
        bcc ?moc11
        cpx #zcann+nmcann
        ifcc                    ;Yes it is a cannon
            lda canngr-zcann,X
            and #$0E
            tay
            lda temp1
            cmp cannsize+1,Y
        else
?moc11      cmp ysize-1,X       ;Check size of object
        endif
        ;-1 as no table entry for man vs. man
        bcs ?moc10      ;No Collision Here
		;*********************************************************
		;*****  Collision Occured.. Do Correct Action!!!!!!  *****
		;*********************************************************
?moc12  ldy colact-1,X      ;Collision Action Number
        lda coltbl+1,Y
        pha 
        lda coltbl,Y
        pha 
        rts 
        
cannsize    .byte $58,$41,$36,$41,$36,$41,$36,$9F,$36,$41,$36,$41,$58,$41

				
;************************************************************
;* X Axis Collision check against man
;* X contains object index
;* Return stamp distance in A
;* temp1 will have abs value of distance between Rex and object
;* side will also be set 00:Left 80:Right
;************************************************************
colchkx lda #00
        sta side            ;For possible collision bounces
        ;Set to Left/Above as initial guess
        lda mazexl
        cpx #ztop
        ifcs
            sbc lsbsx-ztop,X	;X LSB from table in TWMaze
        else
            sec
            sbc objxl,X     ;X LSB
        endif
        sta temp1           ;Save this
        lda mazexh
        sbc objxh,X
        sta temp1+1
        ifmi
            lda #$80
            sta side            ;Set to Right
            jsr dblneg          ;ABS of temp1
        endif
        rts

colchky lda #00
        sta side        ;Set above default
        lda mazeyl
        sec
        sbc objyl,X     ;Y LSB
        sta temp1       ;Save this
        lda mazeyh
        sbc objyh,X
        sta temp1+1
        ifmi
            lda #$80
            sta side            ;Rex must be below Max
            jsr dblneg          ;ABS of temp1
        endif
        rts        


;*************************************
; Hit Reactor Action
;*************************************
; X is always == 1 for reactor
;*************************************
reahit  lda objst,X     ;Already Set off?
        ifpl                ;Not yet
            ;lda objyh+zreactor      ;Make sure man and reactor are on same line
            ;cmp objyh+zman
            ;ifne
            ;    jmp nxto        ;Not on same line, skip collision
            ;endif
            lda lastreac      	;Reactor time left
            and #$7F
            sta retime
            lda #$FF
            sta retime+1
            ldx #$08
            stx nxtdisc     	;So next one will be worth 1000 (see above)
            ldx #00
            stx lauen           ;Ready for Launch
            stx rompg           ;PAGE CHANGE!!!!! PAGE 0
            bit lastreac
            ifpl
				lda #$50            ;5000 for reactor (pass BCD)
                jsr bpoint      	;Don't call each time you hit the reactor
            endif
            jsr stpg6			;PAGE CHANGE!!!!! PAGE 6
            jsr newarrow        ;Out Maze arrows
            lda #$E4
            sta retbuf+2        ;Turn on Clock
            ;ldx zindex      	;Recall old X
        endif
		;*********************************************************
        ;ISSUE #54 - Rector has strange obstacles around it
		;ISSUE #123 - Put it back! :-)
		lda #$80
        sta rtcol          ;Don't walk through it!
		;*********************************************************
        ldx zindex
        bne bad10			;always
		
        ;Shot Hit
shthit  lda #$20
        bit manstat         ;Shields on??
        beq ?killrx         ;No... bye bye baby!
        lda #snd_i1b            
        jsr dosound
        dec shldht
        bpl bad10               ;Had One Left
        lda #00
        sta shldok          ;Else lose shields, 1 hit gone
        beq bad10
        
        ;Robot or Fireball Hit
rfhit   lda #00
        sta rompg			;PAGE CHANGE!!!!! PAGE 0
        stx tempa+1         ;I hope it's open!
		lda #01
        sta rompg			;PAGE CHANGE!!!!! PAGE 1 
        lda #$20
        bit manstat         ;Shields on??
        ifne                    ;yep!
            lda #$10
            jsr bpont2          ;And 1000 points
            ;lda #01
			;sta rompg			;Moved up to above
            ldx tempa+1
            lda #00
            sta shldok          ;Shields gone now!
            lda #snd_i1b
            jsr dosound
        else                ;No Shields, he dies!
            lda #$10
            jsr bpont2      	;And 1000 Points
            ;lda #01
            ;sta rompg			;Moved up to above
            ldx tempa+1
            ;Player Dies... Enter at ?killrx
?killrx     
#IF (DEBUG = 0)			
			lda #$80
            sta manstat
            lda #snd_i2a
            jsr dosound
            lda #00
            sta jumpst      ;Clear Jump
            sta jbstat      ;Button Clear also
#ENDIF
        endif
        ;Object Dies... Enter at bad10      
bad10   lda #$80
        sta objst,X     ;This object will die
nxto    dex 
        ifne
            jmp coloop      ;Continue Loop
        endif 
		;X is zero here
		stx rompg           ;PAGE CHANGE BACK!!!!! PAGE 0
		;Idiot message main routine gets call each collision loop      
        lda dif4mz
        beq ?idiot
		cmp #4d					;Are we on first level of homeworld
		beq ?idiot 
        rts 
		
?idiot	lda manstat
		ifne
			jsr idiotm          ;Check to see if idiot message needed
		endif
		rts
		
;****************************************************
        .sbttl "Trip Plate Action Routine"
;****************************************************
;* This routine is called from 'colobj' in TWMot    
;* and is used to set off the fire balls when a     
;* trip plate is stepped on.                        
;* Each maze may have up to 8 trip plates. The      
;* plate index, through the maze number, through    
;* the 'difcty' level will obtain the trip plate    
;* table entry.                                     
;* Each Entry Contains the Following:              
;*  XH,YH,XV,YV                                   
;* One or Two fireballs will be started (if available)     
;* at the X,Y location specified. They will be      
;* offset from each other depending on their        
;* velocities.                                      
;*                                                  
;* Inputs: X=ztrpp+index (to current plate hit)     
;*                                                  
;* Note: This routine must not use temp9!!!         
;****************************************************
trpoint jsr chkall      ;Make sure this guy didn't fire any
        ifne_           ;Already fired (NOTE FAR BRANCH)
            jsr getfb           ;Any empty, A = 0 if fireball available, OBJ Index in Y
            ifpl_               ;(NOTE FAR BRANCH)
				;We require Page 6 for Maze data in here
				jsr stpg6
                sty tempa       ;Save Y index to fireball
				lda #0
                sta objyl,Y     ;Turn this off
                sta objxl,Y
                lda #snd_i2d    ;launch it
                jsr dosound
                txa             ;This is the trip point index here...
                sec 
                sbc #ztrpp      ;Trip point 0-7
                sta tempa+1
                asl a
                clc 
                adc tempa+1     ;((maznum*8)+trp #)*3
                
				tay             ;As indirect index
                ldx tempa       ;Get Index
                lda (trindptr,Y);Get XH Position
                pha             ;Save for possible flag
                and #$1F        ;Drop Flags
                sta objxh,X
                sta temp4+1     ;Save for possible other
                iny 
                lda (trindptr,Y)
                sta objyh,X     ;Set YH Position
                sta temp5+1     ;Save for possible other
                iny 
                lda (trindptr,Y);X velocity
                sta velxh,X
                sta temp6       ;For Possible other
                ifpl
                    lda #$80
                else
                    lda #$7F
                endif
                sta objxl,X
                sta temp4       ;Save for possible other
                lda gamedif     ;0-4
                asl a
                asl a
                asl a			;x8 = 0-32
                clc 
                adc temp6		;Add Speed indexer
                and #$7F        ;Drop High Bit 
                tay 
                lda ispeed,Y
                bit temp6       ;Add back in direction
                ifmi
                    jsr neg
                endif
                sta temp6
                sta velxh,X
                lda #00         ;Always 0 in Y direction
                sta velyh,X
                sta temp6+1     ;Save for possible other
                ifeq            ;If Y vel 0, then space Y
                    lda #$48
                    sta objyl,X
                    lda #$D7
                    sta temp5           ;Space Y apart
                endif
                lda temp3           ;The saved status from chkall
                sta objst,X
                pla             ;Recall Flag(s)
                ifpl            ;Need two
                    jsr getfb           ;Get us another one
                    bmi ?tp10           ;Sorry None available
                    lda temp4           ;XLSB from above
                    sta objxl,Y
                    lda temp4+1
                    sta objxh,Y     	;XMSB from above
                    lda temp5
                    sta objyl,Y     	;YLSB from above
                    lda temp5+1
                    sta objyh,Y     	;YMSB from above
                    lda temp6
                    sta velxh,Y     	;Velocity from above
                    lda temp6+1
                    sta velyh,Y 
                    lda temp3       	;Saved status
                    sta objst,Y     	;Turn on other too
                endif
				jsr stpg0		;Put it back
            endif
        endif
?tp10   ldx zindex      ;Restore X
        jmp nxto        ;And Continue

;*********************************************************************      
;* Make sure this trip pad does not have a fire ball out there
;*********************************************************************
chkall  txa 
        sec 
        sbc #ztrpp      	;Just the index of this pad
        ora #$10            ;So compare works with status bit
        sta temp3           ;Save for store in Main Routine
        ;A=index of this trip point ! 10(for status on bit)
        ldy #07
        begin
            lda objst+zfire+8,Y     ;Look at top 8
            and #$BF                ;Drop Screen Bit
            cmp temp3               ;Same as out??
            beq ?ca10               ;Shit... found one
            dey
        miend
        ;Returns with - flag, bu A=what we shall store to status
        rts 
        
?ca10   ldy #00
        rts             ;0 is bad return
        
;*******************************************************************    
;* Get-A-Fireball - see if there are any unused Fireballs on this
;*                  level that we can use. If it finds one, then
;*                  A will be zero and Y will be the index
;*                  to the fireball to use. If there are no 
;*                  available fireballs, A is minus.
;*******************************************************************
getfb   ldy #zfire+nmfire-1     ;Look at top 8
        begin
            lda objst,Y     ;check if active (status)
            ora limbo,Y     ;could also be in limbo (transporting)
            beq fbret       ;if neither, then return ZERO
            dey 
            cpy #zfire+8
        ccend               ;None available
        lda #$80
fbret   rts

;********* Adds 1 in Decimal to Disc Count *********    
updiscs 
		ldx zindex
		lda oxystat-zdisc,X
		ifeq				;Ignore this if it is currently animating
			jsr stpg6				;PAGE CHANGE: PAGE 6
			lda dif4mz
			tax
			lda oxybonus,X
			clc 
			adc oxygen				;Add oxygen amounts, higher levels get more 02 for sanity
			ifcs
				lda #$FF            ;Don't allow to wrap
			endif
			sta oxygen
			lda #snd_i2g
			jsr dosound     ;Sound for it!
			lda nxtdisc     ;Points for this one
			bit objst+zreactor  ;Reactor set off??
			ifmi
				sed             ;*********** Decimal Mode **************    
				lda nxtdisc
				clc 
				adc #02
				sta nxtdisc
				cld             ;*********** End Decimal  **************
			endif
			;;ldx #00
			;;stx rompg          ;Set to page 0 for a bit
			;ldx #0
			jsr bpont2
			ldx zindex      	;Restore X
#IF ANIMATE_OXYGEN != 0
			lda #01
			sta oxystat-zdisc,X		;Flag to animate this one
#ELSE
			lda #$80
			sta objxh,X     	;Kill this one
#ENDIF
		endif
        jmp nxto
		
;*************************************
; Hit Vertical/Horiz Lightning Action
;     Laser Cannon 
;*************************************
dofrfl  lda mazeyh
        cmp objyh,X     ;Make sure same line
        ifeq
docann      lda objst+zstuf+2
            cmp #02         ;Skip collisions if in escape pod
            ifcc         
				lda #snd_i2a
                jsr dosound			;Rex dies!
				;Random Ayeeeee, but only 1 in 16 times
				lda frame
				and #$1F	
				ifeq
					txa
					pha
					jsr do3del
					lda #sp_rexwhoa
					jsr dosound
					pla
					tax
				endif
#IF (DEBUG = 0)	
                lda #$80
                sta manstat     ;Do this here, can't jump to ?killrx
                lda #00
                sta jumpst      ;it will try and kill the lightning too
                sta jbstat      ;and that clobbers a robot velocity
#ENDIF
            endif
        endif
        jmp nxto            ;Next!!!    

;*************************************
; Hit Max Action
;*************************************
domaxx	lda objst+zstuf+2
		cmp #02         ;Skip collisions if in escape pod
		ifcc  		
			txa
			pha
			lda rands 
			and #07
			tax					;https://github.com/jessaskey/mhavocpe/issues/197
			lda _maxha,X
			jsr dosound			;Max+Rex death speech 
			jsr do3del			
			lda #snd_i2a
			jsr dosound			;Rex dies!
			pla
			tax
#IF (DEBUG = 0)	
			lda #$80
			sta manstat     	;Do this here, can't jump to ?killrx
			lda #00
			sta jumpst      	;it will try and kill the lightning too
			sta jbstat      	;and that clobbers a robot velocity
#ENDIF
		endif
        jmp nxto            ;Next!!!  

;Max kills Rex.. random sounds to play
_maxha	.db sp_maxhaha1,sp_maxhaha2,sp_maxhaha3,sp_maxwewin
		.db sp_maxyoulose,sp_maxhaha1,sp_maxhaha2,sp_maxdestroy		
        
;*************************************
; Hit One Way Action
;*************************************
doonew  lda mazeyh
        cmp objyh,X     ;Make sure same line
        ifeq
#IF (DEBUG = 0)	
            ldy onewst-zonew,X  ;Status of this wall: - One way to left... + One way to Right
            lda side
            ifmi
                tya             ;See if this is one way right
                ifmi                ;if -, no right allowed
                    lda #$80
                    sta rtcol
                endif
            else
                tya             ;See if one way left
                ifpl                ;If +, no left allowed
                    lda #$80
                    sta ltcol
                endif
            endif
#ENDIF
        endif
        jmp nxto            ;Next!!!!
        
dstop   lda #$80            ;Stop man from moving through an object
        ldy side
        ifmi
            sta rtcol
        else
            sta ltcol
        endif
        rts 
        
dunder  lda objyh,X     ;Check if same height MSB as man
        cmp objyh
        rts 
 
;*************************************
; Hit Clock Action
;************************************* 
doclock jsr dunder      ;Check is on same level
        ifeq
            lda objst,X     ;Pick up an object
			cmp #01
			ifeq
				inc objst,X     ;Activated
				lda #$25
				jsr bpont2		;Award 2500 points, only once tho
			endif
            jsr dstop           ;But don't let him through it
        endif
        jmp nxto

;****************************************************
; Hit Boots Action
;
; objst for Boots are as follows:
; $80 Flag - When initially picked up (freeze)
; $00 = Not in this maze
; $01 = Visible in Maze
; $02 = Rex has boots
;
; $01 -> $80 -> +++++ -> $B0 -> $02
;****************************************************
doboot  jsr dunder      ;Check if on same level
        ifeq     
            lda objst+zstuf+1   ;Pick it up
            cmp #01
            ifeq
                lda #$80
                sta objst+zstuf+1
                lda #$D0            ;Freeze Timers, Cut out controls
                sta gamest
                lda #00         ;Zero Velocity
                sta velxh
                sta velyh
				;sta jumpst      ;Clear Jump
				;sta jbstat      ;Button Clear also
            endif
        endif
        jmp nxto

;*************************************
; Hit Pouch Action
;*************************************
dokeyp	jsr dunder      ;Check if on same level
        ifeq     
            lda objst+zstuf+4   ;Pick it up
            cmp #01
            ifeq
                lda #$80
                sta objst+zstuf+4
            endif
        endif
        jmp nxto

;*************************************
; Hit Stalactite Action
;*************************************
dotite  lda velyh          ;Do Stalactite
        ifpl
            cmp #$10
            ifcs
                jsr hd12            ;Hit Head on Tite
            endif
        endif
        lda velxh
        jsr lf12                ;Check if horizontal velocity high
        lda #00
        sta jumpst          ;Make him fall down in any event
        sta velxh          ;Also stop his sideways progress
        bit tumble
        ifmi
            lda #snd_i2f
            jsr dosound
            lda #02             ;Ring it like a bell
            sta objst,X         ;I'll believe X is correct when I see it!!
        endif
        jmp nxto                ;Next Object Please

;*************************************
; Hit Lock Action
;*************************************        
dolock  jsr dstop               ;Don't let him move through it
        lda objst+nmkeys,X      ;Look at corresponding key
        and #$10
        ifne                    ;He has it
            lda #00
            sta objst+nmkeys,X      ;Take away the key
            sta objst,X         ;Take away the lock
            lda #snd_i2i
            jsr dosound
        else
            jsr lf11
        endif
        jmp nxto            ;Next Object...

;*************************************
; Hit Key Action
;*************************************        
dokeys  jsr dunder          ;Check to see if on same level
        ifeq
            lda objst,X
            cmp #$10
            ifcc
                lda objst,X
                ora #$10
                sta objst,X         ;He picked it up
                lda #snd_i2h
                jsr dosound
                stx temp4
                lda #$10                ;1000 Points
                jsr bpont2
                ldx temp4
            endif
        endif
        jmp nxto

;*************************************
; Hit Tesseract/Token Action
;*************************************
dotok  	jsr dunder          ;Check to see if on same level
        ifeq
            lda objst,X
            and #$10
            ifeq
                lda objst,X
                ora #$10
                sta objst,X         ;He picked it up
				;put in pocket
				and #03
				sta lasttok			;Save this for the upcoming tact animation
				tay
				lda bitflags,Y
				ora tokpock
				sta tokpock
				;Now make these minus, which means they are active
				lda toktarg
				ora #$80
				sta toktarg
				lda tokretr
				ora #$80
				sta tokretr
				;make sound, award points
                lda #snd_i2h
                jsr dosound
                stx temp4
                lda #$10                ;1000 Points
                jsr bpont2
                ldx temp4
            endif
        endif
        jmp nxto

;*************************************
; Hit Escape Pod Action
;*************************************
dopod   lda objst+zreactor      ;Escape Pod
        bpl ?dp10               ;Nothing happens if reactor not blowing up
        lda objst+zstuf+2
        cmp #01
        bne ?dp10               ;If already set, do nothing
        lda #snd_passby           ;Take off sound
        jsr dosound
        jsr dodelay
        lda #snd_j6
        jsr dosound
        lda #00             ;Set man to position of pod without screen jump
        tay 
        sec 
        sbc objxl
        sta temp1
        lda objxh+zstuf+2
        sbc objxh
        sta temp1+1         ;Man's position change must be added to xmot
        jsr spod                ;Y is 0
        lda #$80                ;Now handle Y coordinate change
        sec 
        sbc objyl
        sta temp1
        lda objyh+zstuf+2
        sbc objyh
        sta temp1+1         ;Man's position change must be added to xmot
        ldy #02
        sty objst+zstuf+2
        jsr spod                ;2 in Y
        lda #00
        sta objxl
        sta objst+zstuf+1       ;Turn off Boots (just in case)
        sta jbstat
        sta jumpst
        sta rtcol              ;*** Make sure Oxygen count stops!! ****
        ;Above is because a collision with right wall in pod restarts oxygen time
        lda #$80
        sta objyl
        lda objxh+zstuf+2
        sta objxh
        lda objyh+zstuf+2
        sta objyh
        lda #$D0
        sta gamest          ;Run him off toward large ship
?dp10   jmp nxto            ;Next Object....

spod    lda xmot,Y          ;Support routine for dopod
        clc 
        adc temp1           ;Y is 0 or 2
        sta xmot,Y
        lda xmot+1,Y
        adc temp1+1
        sta xmot+1,Y
        rts 

;*************************************
; Hit Hand Action
;*************************************
dohand  bit ground          ;Do De Hand
        ifmi                    ;Can't hit switchbox unless on the ground
            lda #01
            ldy velxh
            ifne
				ifmi
					lda #03         ;Make it retract
				endif
				cmp objst+zstuf+3   ;Already in this state?
				ifne
					sta objst+zstuf+3   ;Store the new state
					cmp #01             ;Turning on??
					ifeq
						lda #snd_i4c        ;Sound 'On'
					else_ne
						lda #snd_i4b        ;Sound 'Off'
					endif
					
					jsr dosound
					lda #$18
					sta sndcue+1
					sta sndcue+2
				endif
			endif
            ;No need to store it away if already the same!!
        endif
		jmp nxto                ;Next please!!

;*************************************
; Hit Transporter Action
;*************************************
dotran  lda tspark          ;Not currently transporting
        ifeq
            lda objst,X         ;HERE
            ifpl
                and #tr_right
                ifeq                    ;Enter from Left
                    lda objxl
                    ifpl                    ;Moving to Right
                        lda velxh
                        ifpl
?dtyes                      lda objxl
                            ifmi
                                jsr neg
                            endif
                            cmp #$60
                            ifcs
								lda #0
								sta tmatch		;Clear this by default unless our transporter is 'special'
								;******************************************
								; Original matching method was simple
								; Doesn't work now that we have tokens tho
								;******************************************
								;txa				;Entered transporter
								;eor	#01			;Find it's partner
								;tay
								;lda	objst,Y
								;*******************************************************
								; New matching code solves issue with base transporter
								; being an odd index, so we have to do actual math now
								;*******************************************************
                                txa					;Entered transporter
                                lsr A
								txa					;Original value back in A, Carry not affected by this
                                ifcs                     
                                    adc #0				;Carry is set so +1
                                else
                                    sbc #0				;Carry is clear, so -1
                                endif
								tay
								;*******************************************************
								lda	objst,Y
								bit ?bit40			;Hack for $40 since there is no BIT,imm on the 6502
								ifne
									ora #(tr_special+tr_hidden)
									sta tmatch			;tmatch contains the special objst that we want to match 
													;(in next level transports, same level doesn't use it)
								endif
								ora #tr_match
                                sta objst,Y 		;Booth to travel to, marked with tr_match
                                lda #01
                                sta tspark  		;Start Phase out effect
                                lda #snd_i6
								jsr dosound
                            endif
                        endif
                    else                    ;Moving to Left
?dtno                   jsr dstop               ;Hit side
                        jsr lf11                ;Maybe hit hard
                        lda objxl
                        ifpl
                            lda #$24
                        else
                            lda #$DC
                        endif
                        sta objxl
                    endif
                else                    ;Enter from Right
                    lda objxl
                    bmi ?dtyes              ;Hit Transporter moving to right
                    lda velxh
                    bpl ?dtno
                endif
            endif
        endif
        jmp nxto

?bit40	.db tr_special


;*******************************************
    .sbttl "Idiot Message Routines"
;*******************************************        
idiotm  bit jumpst
        ifmi
            bit jumprv
            ifpl
                lda objyh
                asl a
                asl a
                asl a
                asl a
                ora objxh           ;Compact MSB location to one byte
                ldx #estmps-jstmps-1
                begin				;check to see if we are in a valid idiot message jumping location
                    cmp jstmps,X
                    beq ?im10
                    dex
                miend
                lda #00         ;Not on one of the idiot stamp
                sta jmptim
                beq ?im20           ;bra
?im10           lda #01
                sta jmptim      ;Starting jump in correct stamp
            else
                lda jmptim      ;Jump in progress??
                ifne
                    cmp #$18
                    ifcc
                        clc
                        adc #01
                    endif
                    sta jmptim
                endif
            endif
        else
            bit jumprv
            ifmi                ;Starting to fall
                bit tumble
                ifpl
                    lda jmptim
                    ifne
                        cmp #$18
                        ifne
                            inc fldcnt
                            lda fldcnt
                            cmp #03
                            ifeq
                                lda #00
                                sta fldcnt
                                lda #01
                                sta mestim
                            endif
                        endif
                    endif
                endif
            endif
        endif
?im20   lda jumpst
        sta jumprv
        lda mestim
        ifne
            clc
            adc #01
            cmp #$80
            ifcs
                lda #00
            endif
        endif
        sta mestim
        rts

;*********************************************        
;* Can only be an idiot in these locations...
;* again, like New Jersey    
;*********************************************    
jstmps  .db $A3,$A4,$A5,$A6
		.db $A7,$A8,$96,$97
		.db $88,$89,$8A,$79,$7A
estmps


;*****************************************
; Object Type Tables
;*****************************************
tmpptr 			= $
objtypetab	.fill	nmobjs,$FF
_lastobjtype	= 0;		
_lastobjidx		= 0;

#define 	DEFOBJ(name,cnt)		\tmpptr .set *
#defcont							\ .org (objtypetab+_lastobjidx)
#defcont							\ .fill cnt,_lastobjtype
#defcont							\obj_+name = _lastobjtype
#defcont							\_lastobjtype .set (_lastobjtype+1)
#defcont							\_lastobjidx .set (_lastobjidx+cnt)
#defcont                        	\ .org tmpptr
#defcont    						\#if (_lastobjidx > nmobjs) 
#defcont    						\ .error "DEFOBJ: Number of objects exceeds nmobjs!"
#defcont    						\#endif 

		;This will define our objecttypetab enums for quick reference on a zindex object type rather than bounds checking z
		DEFOBJ(rex,nmman)
		DEFOBJ(reactor,nmreactor)
		DEFOBJ(fireball,nmfire)
		DEFOBJ(lshot,nmlsht)   
		DEFOBJ(cann,nmcann)  
		DEFOBJ(robot,nmrob)   
		DEFOBJ(rshot,nmshot)
		DEFOBJ(max,nmmax)
		DEFOBJ(rshp,nmrshp)
		DEFOBJ(toke,nmtoke)
		DEFOBJ(tite,nmtite)
		DEFOBJ(tran,nmtran)
		DEFOBJ(lock,nmlock)
		DEFOBJ(key,nmkeys)
		DEFOBJ(clock,1)
		DEFOBJ(boots,1)
		DEFOBJ(pod,1)
		DEFOBJ(hand,1)
		DEFOBJ(pouch,1)
		DEFOBJ(disc,nmdisc)
		DEFOBJ(ligh,nmligh)
		DEFOBJ(frfl,nmfrfl)
		DEFOBJ(trpp,nmtrpp)
		DEFOBJ(onew,nmonew)
		DEFOBJ(arow,nmarow)

; ;******************************************************       
; ;* Collision Action Table for any touchable object
; ;******************************************************
; colact  .dw $0			;Rex touches himself, not supported
		; .dw reahit-1   	;Reactor
        ; .dw rfhit-1     ;Sparkoids
        ; .dw rfhit-1     ;Laser Cannon Shots
        ; .dw docann-1	;Laser Cannons
        ; .dw rfhit-1     ;Perkoids
        ; .dw shthit-1    ;Perkoid Shots
        ; .dw domaxx-1    ;Max Robots 
        ; .dw nxto-1      ;Rex's Ship 
		; .dw dotok-1		;Tokens
        ; .dw dotite-1    ;Stalactites
        ; .dw dotran-1    ;Transporters
        ; .dw dolock-1    ;Locks
        ; .dw dokeys-1    ;Keys
        ; .dw doclock-1   ;Clock
        ; .dw doboot-1    ;Boots
        ; .dw dopod-1     ;Escape Pod
        ; .dw dohand-1    ;De Hand
		; .dw dokeyp-1	;Keypouch
        ; .dw updiscs-1   ;Oxygen
        ; .dw docann-1    ;Lightning - Horizontal (uses the Laser Cannon handler)
        ; .dw dofrfl-1    ;Force Fields - Vertical
        ; .dw trpoint-1   ;Trip Pads
        ; .dw doonew-1    ;One Way

;******************************************************       
;* Collision Action Table for any touchable object
;******************************************************
colact  .fill nmreactor,colreahit   ;Reactor
        .fill nmfire,colrfhit       ;Sparkoids
        .fill nmlsht,colrfhit       ;Laser Cannon Shots
        .fill nmcann,coldocann      ;Laser Cannons
        .fill nmrob,colrfhit        ;Perkoids
        .fill nmshot,colshthit      ;Perkoid Shots
        .fill nmmax,coldomaxx       ;Max Robots 
        .fill 1,colnxto        		;Rex's Ship 
		.fill 1,coldotok			;Tokens
        .fill nmtite,coldotite      ;Stalactites
        .fill nmtran,coldotran      ;Transporters
        .fill nmlock,coldolock      ;Locks
        .fill nmkeys,coldokeys      ;Keys
        .fill 1,coldoclock          ;Clock
        .fill 1,coldoboot           ;Boots
        .fill 1,coldopod            ;Escape Pod
        .fill 1,coldohand           ;De Hand
		.fill 1,coldokeyp			;Keypouch
        .fill nmdisc,colupdiscs     ;Oxygen
        .fill nmligh,coldocann      ;Lightning - Horizontal (uses the Laser Cannon handler)
        .fill nmfrfl,coldofrfl      ;Force Fields - Vertical
        .fill nmtrpp,coltrpoint     ;Trip Pads
        .fill nmonew,coldoonew      ;One Way
		
o_collactnum    = 0       
#define DEFCOLL(location)           \col+location = o_collactnum
#defcont                            \ .dw location-1
#defcont                            \o_collactnum .set o_collactnum+2
;Address of routines based on index from above      
coltbl 	DEFCOLL(reahit)
        DEFCOLL(rfhit)
        DEFCOLL(shthit)
        DEFCOLL(updiscs)
        DEFCOLL(docann)
        DEFCOLL(dofrfl)
        DEFCOLL(trpoint)
        DEFCOLL(doboot)
        DEFCOLL(nxto)
        DEFCOLL(doonew)
        DEFCOLL(dotite)
        DEFCOLL(dotran)
        DEFCOLL(dolock)
        DEFCOLL(dokeys)
        DEFCOLL(doclock)
        DEFCOLL(dopod)
        DEFCOLL(dohand)
		DEFCOLL(dokeyp)
		DEFCOLL(dotok)
		DEFCOLL(domaxx)

;*****************************************
    .sbttl "Size Tables"
;*****************************************   
tmpptr2     = $
o_objsz     = 0
nmcollobj   = ztop2-zreactor
xsize       .block nmcollobj
ysize       .block nmcollobj

#define SETCOLL(cnum,xval,yval)  \tmpptr2 .set *
#defcont                        \ .org xsize+o_objsz
#defcont                        \ .fill cnum,xval
#defcont                        \ .org ysize+o_objsz
#defcont                        \ .fill cnum,yval
#defcont                        \o_objsz .set o_objsz+cnum
#defcont                        \ .org tmpptr2
#defcont                        \#IF o_objsz > (nmcollobj)
#defcont			            \	.error "RPM: Too collision objects defined. Increase limit or fix."
#defcont				        \#ENDIF 
   
        SETCOLL(nmreactor,$60,$60)  ;Reactor
        SETCOLL(nmfire,$28,$48)     ;Fireballs
        SETCOLL(nmlsht,$28,$48)     ;Laser Cannon Shots
        SETCOLL(nmcann,$60,$40)     ;Laser Cannons
        SETCOLL(nmrob,$28,$48)      ;Perkoids
        SETCOLL(nmshot,$18,$40)     ;Perkoid Shots   
        SETCOLL(nmmax,$28,$48)      ;Max Robots
        SETCOLL(1,$80,$40)     		;Rex's Ship on Maze + Elevator - Not Used
		SETCOLL(1,$38,$38)     		;Tokens
        SETCOLL(nmtite,$58,$58)     ;Stalactites
        SETCOLL(nmtran,$5C,$80)     ;Transporters
        SETCOLL(nmlock,$30,$80)     ;Locks
        SETCOLL(nmkeys,$34,$68)     ;Keys
        SETCOLL(1,$54,$60)          ;Clock
        SETCOLL(1,$30,$60)          ;Boots
        SETCOLL(1,$80,$80)          ;Escape Pod
        SETCOLL(1,$20,$50)          ;De Hand
		SETCOLL(1,$30,$60)          ;Key Pouch
        SETCOLL(nmdisc,$30,$3F)     ;Oxygen
        SETCOLL(nmligh,$80,$30)     ;Lightning (Horizontal)
        SETCOLL(nmfrfl,$30,$98)     ;Force Fields (Vertical)
        SETCOLL(nmtrpp,$88,$88)     ;Trip Pads
        SETCOLL(nmonew,$40,$98)     ;One Way Walls
            
;************************************************
;* These are the LSB positions for non-motion 
;* objects where their maze stamp is their 
;* locations. These LSB's position within
;* the stamp.    
;************************************************
o_lsbs      = 0
nmstatobj   = (ztop2+nmarow-ztop)

lsbsx       .block nmstatobj
lsbsy       .block nmstatobj

#define SETLSB(lnum,xval,yval)  \tmpptr .set *
#defcont                        \ .org lsbsx+o_lsbs
#defcont                        \ .fill lnum,xval
#defcont                        \ .org lsbsy+o_lsbs
#defcont                        \ .fill lnum,yval
#defcont                        \o_lsbs .set o_lsbs+lnum
#defcont                        \ .org tmpptr
#defcont                        \#IF o_lsbs > (nmstatobj)
#defcont			            \	.error "RPM: Too static object LSBs defined. Increase limit or fix."
#defcont				        \#ENDIF 

    ;SETLSB(number,xlsb,ylsb)
    SETLSB(nmtite,$80,$B0)  ;Stalactites
    SETLSB(nmtran,$80,$80)  ;Transporters
    SETLSB(nmlock,$80,$80)  ;Locks
    SETLSB(nmkeys,$00,$40)  ;Keys
    SETLSB(1,$00,$40)       ;Clock
    SETLSB(1,$00,$34)       ;Boots
    SETLSB(1,$00,$80)       ;Escape Pod
    SETLSB(1,$3C,$01)       ;De Hand
	SETLSB(1,$00,$34)       ;Key Pouch
    SETLSB(nmdisc,$90,$40)  ;Oxygen
    SETLSB(nmligh,$00,$80)  ;Lightning (Horizontal)
    SETLSB(nmfrfl,$80,$80)  ;Force Field (Vertical)
    SETLSB(nmtrpp,$80,$08)  ;Trip Pads
    SETLSB(nmonew,$80,$80)  ;One Way 
    SETLSB(nmarow,$C0,$40)  ;Maze Arrows

;Unit pointer offset tables
maz0u   .db m0ua-maz0,m0ub-maz0,m0uc-maz0,m0ud-maz0,m0ue-maz0,m0uf-maz0 
		.db m0u1-maz0,m0u2-maz0,m0u3-maz0,m0u4-maz0,m0u5-maz0,m0u6-maz0,m0u7-maz0,m0u8-maz0 
		
maz1u   .db m1ua-maz1,m1ub-maz1,m1uc-maz1,m1ud-maz1,m1ue-maz1,m1uf-maz1 
		.db m1u1-maz1,m1u2-maz1,m1u3-maz1,m1u4-maz1,m1u5-maz1,m1u6-maz1,m1u7-maz1,m1u8-maz1,m1u9-maz1 
		
maz2u   .db m2ua-maz2,m2ub-maz2,m2uc-maz2,m2ud-maz2,m2ue-maz2,m2uf-maz2 
		.db m2u1-maz2,m2u2-maz2,m2u3-maz2,m2u4-maz2,m2u5-maz2,m2u6-maz2,m2u7-maz2,m2u8-maz2,m2u9-maz2,m2u10-maz2
		
maz3u   .db m3ua-maz3,m3ub-maz3,m3uc-maz3,m3ud-maz3,m3ue-maz3,m3uf-maz3 
		.db m3u1-maz3,m3u2-maz3,m3u3-maz3,m3u4-maz3,m3u5-maz3,m3u6-maz3,m3u7-maz3,m3u8-maz3,m3u9-maz3,m3u10-maz3,m3u11-maz3,m3u12-maz3
        
;Unit Pointers - Point to the table of unit pointers for each maze
mazunt  .word	maz0u,maz1u,maz2u,maz3u 

;*******************************************
    .sbttl "Get Pointer"
;*******************************************
unitp	lda maztype
        asl a
        tay             	;Offset into unit pointer offset table
        lda mazunt+1,Y      ;Pointers to offsets
        sta mazpt+1
        lda mazunt,Y
        sta mazpt
        lda linen           ;Line Number
        clc 
        adc mazpt           ;Add Line to unit address
        sta mazpt
        ifcs
            inc mazpt+1
        endif           	;Now points to offset into data for this line
        ldy #00
        lda (mazpt,Y)
        clc 
        adc # lbyte(mazebuf);Add to low byte
        sta mazpt
        lda #00
        adc # ubyte(mazebuf);Prop carry to high byte
        sta mazpt+1     	;Now points to data for this line
        rts 

;****************************************************
;*     Collision Routines!!!!!                      
;****************************************************
    .sbttl "Stamp Locator - Non Max Only"
;****************************************************
;* Determine which stamp the guy is in and also save 
;* his position in that stamp for possible later use.        
;*                                      
;* Input:   (X)=Offsets into positions  
;*              obj*l and obj*h         
;*                                      
;* Uses:    temp3,temp4,X           
;****************************************************
stloc   lda objxl,X
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
			.sbttl "Ground Check - Non-Max Only"
		;******************************************************************************
		;* Enter from above to set proper 0 page variables and X must point to posit    
		;*                                      
		;* Exit: (A)= tempgrnd value, updates abvg and undg flags too         
		;******************************************************************************    
		lda #00             ;Clear Just past bit
        tay                 ;Guess no head collision
        sta abvg            ;Not above ground as a guess
        sta pastbit			;Not past
        sta undg            ;Also guess above ground
        lda temp4           ;objyl from above
        ifpl                ;Want to check ground
            cmp #$10            ;At least above the stamp
            ifcs
                sec
                sbc gndvt,X     ;Ground Table
                ifmi                ;Might have gone by it
                    lda velyh,X     ;Moving down
                    ifmi
                        lda temp4   	;Get back old position
                        sec
                        sbc velyh,X     ;Subtract back in vel and check
                        cmp gndvt,X
						ifcs            ;Yep, it went by
                            lda #$80
                            sta pastbit     ;Signal it just went past
                        else                ;Otherwise, we are underground
                            lda #$80
                            sta undg        ;Set this for later
                            lda #00
                        endif
                    else            ;Underground moving up!!
                        lda #$80
                        sta undg        ;Set this for later
                        lda #00
                    endif
                else                ;Not past, just above?
                    cmp #04         ;Slope amount
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
            sbc celt,X      ;Can he hit his head??
            ifmi                ;Is below, but is T close?
                cmp #-4         	;How close?
                ifcs                ;Close enough
                    ldy #$80            ;Will hit his head!
                endif
            else
                lda #$80            ;Is above, did it just pass?
                sta abvg            ;His head is above ceiling
                lda temp4
                sec
                sbc velyh,X     	;Back out velocity
                cmp celt,X      	;Did it pass this time?
                ifcc
                    ldy #$80
                endif
            endif
            txa                 ;X=0 for man
            bne ?gc10           ;Only skip for man
            lda #02
            and mzgame          ;Waiting for game to start
            ifeq
?gc10           tya
                sta headcol,X       ;Possible Head collision
            endif
            lda #00
        endif
        sta tempgrnd
		cpx #0
		ifeq
			lda ground          ;Save if change state
			sta lastgnd
		endif
        rts 

;*********************************************************
;* All motion objects need to check for vertical ground 
;* and ceiling collisions... but they use a different
;* offset due to their vertical size.
;*********************************************************        
gndvt   .fill 1,gndv            ;Rex
        .fill nmreactor,$40     ;Reactor
        .fill nmfire,$28        ;Fireballs
        .fill nmlsht,$28        ;Laser Cannon Shots
        .fill nmcann,$10        ;Laser Cannons
        .fill nmrob,$18         ;Perkoids
        .fill nmshot,gndv       ;Perkoid Shots
        .fill nmmax,$38         ;Max Robots
    
celt    .fill 1,celing          ;Rex
        .fill nmreactor,$C8     ;Reactor
        .fill nmfire,$D8        ;Fireballs
        .fill nmlsht,$D8        ;Laser Cannon Shots
        .fill nmcann,$F0        ;Laser Cannons
        .fill nmrob,$D8         ;Perkoids
        .fill nmshot,$E8        ;Perkoid Shots
        .fill nmmax,$B8         ;Max Robots
        
;*******************************************************************************
    .sbttl "Man's Action on Collisions"
;*******************************************************************************
;* This routine is called after calling stloc and grchk to effect the mans   
;* velocities and picture sequences based on bits set by those routines  
;*******************************************************************************
manact  lda curstmp
        and #$0F            ;Only stamp info
        sta manstmp     ;Save this for later use
        bit curstmp 
        ifvs
            lda #$80            ;An outside stamp
        else
            lda #00
        endif
        sta outflg      ;Set outflg
        bit mzgrnd
        ifmi                ;On ground??
            bit lastgnd     ;Just Land?
            ifpl                ;yep
                lda picseq      ;Only if at stop
                and #08         ;Was falling??
msk08 = $-1                         ;A bit #08 mask!!!
                ifne                ;Was a fall sequence
                    lda picseq
                    and #02         ;Look for jog or run
                    ifeq                ;Was stop of walk
                        ldy #04         ;Don't change pics to fast(stored below)
                        lda #$80
                        sta landflg     ;Landing
                        lda #landseq
                        sta picseq      ;Set to land sequence
                        lda #$10        ;Set to squat pic to start bounce seq
                        bit tumble      ;Only if tumbling
                        ifmi
                            lda #02         ;So sit there for a while
                            sta sittime     
                            bit face            ;Smash face??
                            ifpl                ;no, do normal stuff for hit head
                                lda #idx_pic13     ;force to squat
                                ldy #08         	;and some time til next sequence
                            else
                                lda piccur      	;Else, continue sequence in table
                                ldy #06         	;Do it slowly
                            endif
                        endif
                        sta piccur      ;Start new sequence
                        sty picdely
                        lda #$80
                        sta stopflg     ;To indicate a change to stop sequence
                    endif
                endif
                lda #$80
                sta mzgame          ;This may clear any hold bit left on
            endif
        endif
        lda mzgrnd
        and pastbit         ;On Ground and just past it?
        ifmi
            sec
            lda #gndv
            sbc temp4           ;Amount under 50
            clc
            adc mazeyl      ;Add back to position
            sta mazeyl      ;Hold on the ground
        endif
        jsr docse2      ;Do head check also and return
lf11    lda velxh      ;But was he moving very fast
        ifpl
            bit rtcol   
            bmi lf13
        else
            bit ltcol   
lf12        ifmi
                jsr neg     ;Look at + velocity
lf13            cmp #$50        ;At least running??
                ifcs            ;yep
                    lda #00
                    sta jumpst      ;Stop any jump also
                    jsr hd12        ;Tumble from face smash
                    bit mzgrnd      ;Did he do it on the ground
                    ifmi                ;yep
                        lda #idx_pic29        ;For hitting a wall
                        sta piccur          ;Smack the wall face first
                        lda #$0C            
                        sta lastgnd         ;Fool it next time and say just landed
                        sta mzgrnd          ;A bit more fooling
                        sta picdely
                        lda #$80
                        sta face            ;Tell it we smashed our face
                    endif
                endif
            endif
        endif
        rts

;****************************************
    .sbttl "Stamp Lookup Routine"
;****************************************
;* Uses info from stamp locator and     
;* returns the type of the stamp        
;* that he is currently standing in.    
;* Expects that X & Y position info     
;* will be in temp3(XL & XH) and        
;* temp4 (YL & YH )                     
;*                                      
;* Uses: temp5,temp1,temp3,temp4        
;****************************************               
sttype  ;********** Out of bounds check first **********
		; JMA - 06052020 - Removed because it doesn't matter if you collide outside, lets save some time.
        ; lda temp4+1     ;Y MSB (above maze check)
        ; cmp #$FD           ;This is above top line
        ; ifcs
; ?sl10       lda #$47            ;Always blank and outside
            ; rts
        ; endif
        ; lda temp3+1     ;X MSB (Left of maze)
        ; beq ?sl10
        ; bmi ?sl10
        sty temp7       ;Save Y
        lda #02         ;Fudge factor that works always
        sec 
        sbc temp4+1     ;Y Position (negated)
        sta linen       ;This line for stamp
        jsr unitp       ;Get unit pointers
        lda mazpt
        clc 
        adc temp3+1     ;Current stamp in this line
        sta mazpt
        ifcs
            inc mazpt+1
        endif
        clc 
        adc #03         ;Another weird offset factor
        sta mazpt
        ifcs
            inc mazpt+1
        endif
        lda (mazpt,Y)       ;Get stamp
        ldy temp7
        rts 
        
;****************************************
    .sbttl "Ground Case Table"
;****************************************
;* Collision case routine for ground    *
;* collisions. Requires 'curstmp' to    *
;* contain current stamp type the man   *
;* is standing in. Also requires that   *
;* temp3 & temp4 contain X and Y        *
;* positions.                           *
;* Will return as a subroutine          *
;*                                      *
;* Caution: (Y) = Object Index          *
;*          (Also in zindex)            *
;****************************************   
coljsr  .word stamp0-1
        .word stamp1-1      ;Stamp 1 (H Line)
        .word stamp2-1      ;Stamp 2 (H end, right down)
        .word stamp3-1      ;Stamp 3 (H end, right up)
        .word stamp4-1      ;Stamp 4 (H end, left up)
        .word stamp5-1      ;Stamp 5 (H end, left down)
        .word stamp6-1      ;Stamp 6 (V Line)
        .word stamp7-1      ;Stamp 7 (Black)
coljse  ;end of table marker


botcase ldy zindex      ;Index of object we are colliding with
		lda #00
        sta rtcol,Y
        sta ltcol,Y
        lda curstmp     ;Current Stamp
        and #$0F
        asl a           ;words
        tax             ;into X
        cpx #coljse-coljsr  ;Valid stamp?
        ifcs                ;not valid (NOTE: JMA - This should never happen, maybe check and this can come out?)
            ldx #02         ;Set to line for now
        endif
        lda coljsr+1,X
        pha 
        lda coljsr,X
        pha 
        rts             ;Do case (jmp equiv)
        
;****************************************
    .sbttl "Stamp calls for Ground"
;****************************************
;* Stamp 0 - No Stamp 0, this is an     *
;* end of line marker, should not ever  *
;* happen                               *
;****************************************   
stamp0  rts         ;No action!

;****************************************
;* Stamp 1 - H line, set ground bit if  *
;* at ground level always               *
;****************************************   
stamp1  lda tempgrnd
        sta ground,Y    ;Ok to stop here
        rts
        
;****************************************
;* Stamp 2 - H Line, Right edge down.   *
;* If H position is greater than 80     *
;* then will clear ground bit, else     *
;* use tempgrnd from above.                *
;****************************************       
stamp2  lda tempgrnd
        ldx temp3           ;X LSB from way above
        ifmi
            cpx #lftwal-$18     ;On Edge, time to fall::
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
                jmp stamp6x        ;Don't allow to pass wall
            endif
        endif
        rts 

;****************************************
;* Stamp 3 - H Line, Right edge up.     *
;* Should be no way to pass this edge   *
;* if X position > edge. set rtcol      *
;* if on ground                         *
;****************************************       
stamp3  ldx temp3           ;LSB X position
        ifpl
            lda tempgrnd
        else
            lda #00
        endif
        sta ground,Y
        jsr stamp6x            ;Do wall check as vertical wall
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
stamp4  ldx temp3           ;LSB X from way above
        ifmi                ;On Right edge
            lda tempgrnd           ;Get ground status
        else
            lda #00
        endif
        sta ground,Y
        jsr stamp6x        ;Do wall check as vertical wall
        bit undg            ;Under ground level??
        ifmi                ;yes
            jmp ltchk           ;check left for possible skewer
        endif
        rts

;****************************************
;* Stamp 5 - H Line, Left edge down.    *
;* If over left edge, will clear ground *
;* Else will use tempgrnd from above       *
;****************************************               
stamp5  lda tempgrnd
        ldx temp3           ;LSB X position from above
        ifpl                ;On ledge??
            cpx #rtwal+$10      ;yep, time to fall??
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
                jmp stamp6x
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
stamp6  jsr stamp7      	;Do check for side stamps
stamp6x ldx abvstmp     	;Whats above??
        lda abvchk,X        ;Can we pass this on
        bmi bad7            ;No need to check here
        lda temp4           ;Y LSB
        cmp #gndv+$75       ;Top of this wall here
        ;At this point it is almost in the next stamp!!!!!!
        ifcs                ;We are above
            cmp #gndv+$80       ;May need to fall a bit
            ifcs
                rts          		;EARLY RTS! No ground here!!
            endif
            lda temp3
            cmp #rtwal+$10
            ifcs
                cmp #lftwal-$18		;lftwal = $A0 so this is = -$88
                ifcc                ;We are standing on top!
                    tya
                    ifeq
                        lda #$40
                        sta teeter      ;Teetering
                    endif
                    lda #$80
                    sta ground,Y
                else                ;Else, falling, make sure outside edge
                    jsr setlft
                endif
            else                ;Else falling, make sure outside edge
                jsr setrt
            endif
        else                ;Not above wall
bad7        ldx #$80
            ;check for possible wall pass through
            lda velxh,Y     ;Left check left, right check right
            ifmi			;Moving left, check left wall
                lda temp3           ;X LSB
                cmp #$20            ;Max velocity and stop stamp crossover
                ifcs                ;no need to check from 20 to 0
                    cmp lcolv,Y     ;At left wall (size include)
                    ifcc                ;We know we are left, did we just pass through??
                        lda oldxl,Y     ;See if old pos was right of wall
                        bmi ?stm10      ;Go set collision left wall
                    endif
                endif
            else            ;Moving right, check right wall
                lda temp3       ;XLSB from motion routine
                cmp #-$20       
                ifcc            ;Same as above for other wall
                    cmp rcolv,Y ;At right wall (size table)
                    ifcs                ;We know we are right of right wall, did we just pass through??
                        lda oldxl,Y     ;See if old pos was left of wall
                        bpl ?stm15      ;Go Set collision
                    endif
                endif
            endif
            lda temp3           ;X LSB
            ifmi                ;Could approach from left
                cmp lcolv,Y     ;At left wall (size include)
                ifcc
?stm10              lda lcolv,Y     ;Set at left wall (slightly in)
                    sec
                    sbc #01
                    sta objxl,Y
                    txa
                    sta ltcol,Y     ;Stop left motion
                endif
            else
                cmp rcolv,Y         ;At right wall (size table)
                ifcs
?stm15              lda rcolv,Y         ;Set at right wall (slightly in)
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
stamp7  ldx #00     	;Will guess no ground
        lda temp3       ;LSB X position
        ifmi            ;On right edge of blank
            jsr rtchk       ;Check right side
        else
            jsr ltchk       ;Check left side
        endif
        txa 
        sta ground,Y
        rts
        
;*********************************************************
;* Left and Right wall check values. The following table *
;* is used to compensate for the different size of object*
;*********************************************************
lcolv   .fill 1,lftwal+4        ;Rex
        .fill nmreactor,lftwal  ;Reactor - Not Used
        .fill nmfire,$88        ;Fireballs
        .fill nmlsht,$88        ;Laser Cannon Shots
        .fill nmcann,$90        ;Laser Cannons
        .fill nmrob,$98         ;Perkoids
        .fill nmshot,$90        ;Perkoid Shots
        .fill nmmax,$A8         ;Max Robots

rcolv   .fill 1,rtwal           ;Rex
        .fill nmreactor,$48     ;Reactor - Not Used
        .fill nmfire,$58        ;Fireballs
        .fill nmlsht,$58        ;Laser Cannon Shots
        .fill nmcann,$58        ;Laser Cannons
        .fill nmrob,$58         ;Perkoids
        .fill nmshot,$68        ;Perkoid Shots
        .fill nmmax,$48         ;Max Robots
        
;*************************************************************
;* Using above stamp, check to see if it is possible to pass *
;* above a stamp 6. This is possible if there is a space but *
;* certain stamps leave no space here as say another 6.      *
;* If below table has an 80, then there is no space to pass  *
;* above.                                                    *
;*************************************************************
abvchk  .db $00 ; Not Valid Stamp
		.db $00 ; Horizontal 
		.db $80 ; LeftDown
		.db $00 ; LeftUp
		.db $00 ; RighUp
		.db $80 ; RightDown
		.db $80 ; Vertical
		.db $00 ; Blank


;****************************************************
    .sbttl "Right Edge Check"
;****************************************************
;* Check stamps to the right for possible motion    *
;* restriction when the man is 'underground' level  *
;*                                                  *
;* Inputs:  undg = 80 if under level                *
;*      temp3= X position (word)                *   
;*          temp4= Y position (word)                *
;*          x = 0                                   *
;*                                                  *
;* Output:  rtcol = 80 if not right motion allowed  *
;*          X = (tempgrnd) or 0 depending on           *
;*              conditons of collisions             *
;*                                                  *
;* Uses:    temp5(word), 2 bytes of stack           *
;****************************************************
rtchk   inc temp3+1     ;Look 1 stamp right
        jsr sttype
        dec temp3+1     ;Restore original value
        cmp #04         ;Look for 0,1,2,3 as bad (0=dont'care)
        ifcc
            lda temp3           ;Where are we::
            cmp #lftwal+$3e
            ifcs
                bit undg            ;Underground?
                ifmi
                    lda #$80
                    jsr srtcol
                endif
                ldx tempgrnd           ;This may be used if in 'st_vpg'
            endif
        endif	
        rts
        
;****************************************************
    .sbttl "Left Edge Check"
;****************************************************
;* Check stamp to the left for possible motion      
;* restriction when the man is 'underground' level  
;*                                                  
;* Inputs:  undg = 80 if under level                
;*      	temp3= X position (word)                   
;*          temp4= Y position (word)                
;*          x = 0                                   
;*                                                  
;* Output:  rtcol = 80 if no left motion allowed    
;*          X = (tempgrnd) or 0 depending on        
;*              conditons of collisions             
;*                                                  
;* Uses:    temp5(word), 2 bytes of stack           
;****************************************************   
ltchk   dec temp3+1
        jsr sttype      ;What stamp is to the left
        inc temp3+1     ;Restore
        cmp #01
        ifeq    
bad3        lda temp3
            cmp #rtwal-$30      ;Was a floor to the left??
            ifcc                ;yep
                bit undg            ;under floor level?
                ifmi                ;yep
                    lda #$80
                    jsr sltcol      ;Store to proper ltcol
                endif
                ldx tempgrnd
            endif
        else                ;Not stamp 1, check other bad ones
            cmp #04         ;>=4??
            ifcs
                cmp #06
                ifcc        
                    jmp bad3            ;Was 4 or 5
                endif
            endif
        endif
        rts
        
;************* Store Proper ltcol ******************    
sltcol  ldy zindex
        sta ltcol,Y
        rts 
;************* Store Proper rtcol ******************
srtcol  ldy zindex
        sta rtcol,Y
        rts 
;************* Bit Proper ltcol ******************
bitlt   ldy zindex
        lda ltcol,Y
        rts 
;************* Bit Proper rtcol ******************
bitrt   ldy zindex
        lda rtcol,Y
        rts 
;*************   Set Left Edge  ******************
setlft  lda lcolv,Y     ;Get Left edge for this guy
        clc 
        adc #$1         ;Move in 1
        sta oldxl,Y     ;Fake to show right of left edge
        rts 
;*************  Set Right Edge  ******************
setrt   lda rcolv,Y     ;Get Right edge for this guy
        sec 
        sbc #$1         ;Move in 1
        sta oldxl,Y     ;Fake to show left of right edge
        rts 
        
;*******************************************
    .sbttl  "Head Case Table"  
;*******************************************
;* Head collision case routine. Uses info  *
;* in temp3 and temp4 (X & Y Man Position) *
;* and headcol to determine velocity action*
;*******************************************
coljs2  .word hd0-1		;No Action
        .word hd1-1 	;Horizontal
        .word hd2-1		;Left Down
        .word hd3-1		;Left Up
        .word hd4-1		;Right Up
        .word hd5-1		;Right Down
        .word hd6-1		;Verical
        .word hd7-1		;Blank - No head case collision
colje2  ;End of table marker

docse2  lda mzgame
        and #02
        ifeq                ;Not in hold
            lda abvstmp     ;Stamp above head
            asl a           ;Words
            tax 
            cpx #colje2-coljs2
            ifcs
                ldx #02
            endif
            lda coljs2+1,X
            pha
            lda coljs2,X
            pha
        endif
        rts
        
;*************************************************
    .sbttl "Special Stamp Collisions for Head"
;*************************************************
; Stamp 0   
hd0     rts         ;No Action!!!

; Stamp 1
hd1     bit headcol
        ifmi                ;Possible Head Collision?
            lda #00         ;Collision clears jump bit
            sta jumpst
            bit tumble      ;First time check on head?
            ifpl                ;yep, do this once
                lda velyh      ;Slow down 'up' velocity
                ifpl                ;Only slow 'up' velocities
                    cmp #$80
                    ror A
                    sta velyh      ;Will cut it in half
                endif
            endif
            lda picseq
            and #04         ;Jump??
            ifne                ;yes
hd12            lda #idx_pic16
                sta piccur      ;Hit head, crouch!!
                lda #snd_i2c
                jsr dosound     ;Klunk Noise
				;*********************************
				bit tumble
				ifpl
					lda rands		;Check a random
					and #03
					ifeq
						tax
						jsr dodelay     ;Thanks peter!
						lda ughtbl,X
						jsr dosound     ;Speech
					endif
				endif
                ;**********************************
				lda #$80
                sta tumble      ;He's going to tumble
				lda #fallseq    ;And make him fall
                sta picseq      ;Down!!!!!!!
            endif
        endif
        rts 

ughtbl	.db sp_rexooof,sp_rexooom,sp_rexummm,sp_rexuggh
;**********************************************************
;* Stamp 3 - H Line, Right Edge Up.                       *
;* Hit head on the left, allow to pass on the right       *
;**********************************************************
hd2     ;* Stamp 2 - H Line, Right Edge Down          *
        ;* Same as 3 except for no left skewer check  *
        ;********************************************** 
        jsr hd62        ;Must check turn down part first
hd3     lda temp3       ;X LSB Value
        cmp #lftwal-$10 
        ifcc
            bit mazhed
            ifmi
                jsr hd1     ;Hit Head
            endif
            bit abvg
            ifmi
                lda abvstmp
                cmp #03
                ifeq                ;Stamp 3 differs
                    lda #$80            ;No left motion allowed
                    jsr sltcol
                endif
            endif
        else
            bit abvg            ;Above ground??
            ifmi
                jsr rthdck          ;Check stamp to right
            endif
        endif
        rts

;**********************************************************
;* Stamp 4 - H Line, Left Edge Up.                        *
;* Hit head on the right, allow to pass on the left       *
;**********************************************************
hd5     ;* Stamp 5 - H Line, Left Edge Down           *
        ;* Okay to pass above ground here.            *
        ;**********************************************             
        jsr hd62            ;Must check part that turns down first
hd4     lda temp3           ;X LSB value
        cmp #rtwal+$10
        ifcs
            bit mazhed
            ifmi
                jsr hd1     ;Hit head
            endif
            bit abvg                ;Above Ceiling??
            ifmi
                lda abvstmp
                cmp #04         ;No right motion on 4
                ifeq
                    lda #$80
                    jsr srtcol
                endif
            endif
        else                ;On left edge of stamp
            bit abvg            ;Above ceiling??
            ifmi
                jsr lthdck      ;Check next stamp over
            endif
        endif
        rts
        
;**********************************************************
;* Stamp 6 - Vertical Wall                                *
;* Must check to see if we hit it from the bottom as well *
;* as no horizontal motion through it.                    *
;********************************************************** 
hd6     jsr hd7         ;Check sides as in 7
hd62    lda ground,Y    ;This is entry for any stamp that has an edge pointing down
        ifpl    
            lda temp3
            ifmi
                cmp #lftwal     ;Hit Wall?
                ifcc
                    lda #$80
                    jsr sltcol
                endif
            else
                cmp #$rtwal
                ifcs
                    lda #$80
                    jsr srtcol
                endif
            endif
            lda temp4           ;Y LSB
            cmp #gndv+$10       ;Ground value
            ifcs
                lda temp3           ;Hit head check
                cmp #lftwal-$18
                ifcc
                    cmp #rtwal+$10
                    ifcs
						lda #00
						sta jumpst  	;Clear Jump
                        jmp hd12        ;Hit head
                    endif
                endif
            endif
        endif
        rts
        
;**********************************************************
;* Stamp 7 - Blank                                        *
;* Must check the stamp to the closest side to see if a   *
;* possible collision might have occured. This is similar *
;* to st_vpg for ground.                                     *
;********************************************************** 
hd7     ldx #00     	;Will guess no head collision
        lda temp3       ;LSB X Position
        ifmi            ;left edge
            jsr bitrt       ;Already have a collision
            ifpl            ;no
                jsr rthdck  ;check right side
                jsr bitrt
                ifmi
                    jsr hd1     ;yes
                    lda abvg    ;Set back to 0 if needed (was a flag here)
                    jsr srtcol
                endif
            endif
        else
            jsr bitlt       ;Already have a collision?
            ifpl            ;if yes, skip this
                jsr lthdck  ;Check left side
                jsr bitlt
                ifmi            ;sure did
                    jsr hd1
                    lda abvg
                    jsr sltcol
                endif
            endif
        endif
        rts

;********************************************
    .sbttl "Right Edge Check"
;********************************************
;* Checks one line up and one stamp right   *
;* for possible head and or body collision  *
;*                                          *
;* Inputs: temp3 = X position (word)        *
;*         temp4 = Y position (word)        *
;*                                          *
;* Output: trcol = 80 if no right motion    *
;*                 allowed                  *
;*                                          *
;* Uses:   temp5, 2 bytes stack             *
;********************************************   
rthdck  inc temp4+1     ;One line up
        inc temp3+1     ;One stamp right
        jsr sttype      ;A = What stamp is to the right??
        dec temp3+1     ;Restore
        cmp #04
        ifcc                ;0,1,2,3 collide ( 0 is don't care)
            lda temp3
            cmp #lftwal+$3e     ;Might be touching next wall
            ifcs                ;yep
                lda #$80
                jsr srtcol      ;right collision
            endif
        endif
        rts
        
;********************************************
    .sbttl "Left Edge Check"
;********************************************
;* Checks one line up and one stamp left    *
;* for possible head and or body collision  *
;*                                          *
;* Inputs: temp3 = X position (word)        *
;*         temp4 = Y position (word)        *
;*                                          *
;* Output: trcol = 80 if no left motion     *
;*                 allowed                  *
;*                                          *
;* Uses:   temp5, 2 bytes stack             *
;********************************************   
lthdck  inc temp4+1     ;One line up
        dec temp3+1     ;One stamp left
        jsr sttype      ;A = stamp to the left
        inc temp3+1     ;restore
        cmp #01
        ifeq
bad2        lda temp3
            cmp #rtwal-$30      ;Was floor to left
            ifcc                ;yep
                lda #$80
                jsr sltcol      ;Don't allow to pass left
            endif
        else                ;Not 1, check for 4 or 5
            cmp #04
            ifcs                ;>= 4
                cmp #06         ;<6?
                ifcc                ;yes
                    jmp bad2            ;Was 4 or 5
                endif
            endif
        endif
        rts
        
        
;********************************************************
    .sbttl "Picture Tables"
;********************************************************
; Define each major sequence  
stopseq = 0
walkseq = 1
jogseq  = 2
runseq  = 3     
stjmseq = 4     ;Jump from Stop sequence
wajmseq = 5     ;Jump from Walk sequence
jojmseq = 6     ;Jump from Jog sequence
rujmseq = 7     ;Jump from Run sequence
fallseq = 8     ;Fall from Stop
waflseq = 9     ;Fall from Walk
joflseq = 10d   ;Fall from Jog
ruflseq = 11d   ;Fall from Run
landseq = 12d   ;Land from Stop or Walk
pshbseq = 13d   ;Button press for Elevator
emotseq = $80	;Floss Sequence (minus flags linear)

;* The entry tells the number of frames to pass
;* before stepping to the next picture in that seq.               
delaytb .byte 03	;stop
		.byte 04	;walk
		.byte 03	;jog
		.byte 02  	;run
        .byte 03	;jump from stop
		.byte 03	;jump from walk
		.byte 03	;jump from jog
		.byte 03  	;jump from run
        .byte 05	;fall from stop
		.byte 05	;fall from walk
		.byte 05	;fall from jog
		.byte 05  	;fall from run
        .byte 04	;land from stop
		.byte 04	;button press

;Pointer to sequences
pictbl  .word stop
        .word walk
        .word jog
        .word run
        .word jump1
        .word jump1
        .word jump2
        .word jump2
        .word fall
        .word fall
        .word fall2
        .word fall2
        .word land
        .word pushb

;**************************************************************        
;Sequence tables - These are state tables for each sequence 
;                  enumeration. For each current index the 
;                  data byte is the index to the next state
;                  in the sequence
;
;       If 80 is added to a number that means
;       we may wish to add a foot tap sound here.
;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
stop    .db 01d             ;(pic00) JW Up Left             ->
		.db 02d             ;(pic01) JW Contact Left        -> 
		.db 25d             ;(pic02) JW Pass Left           -> 
		.db 02d             ;(pic03) JW Kickoff Left        -> 
		.db 05d             ;(pic04) JW Up Right            ->
		.db 23d             ;(pic05) JW Contact Right       -> 
		.db 23d             ;(pic06) JW Pass Right          -> 
		.db 06d             ;(pic07) JW Kickoff Right       -> 
		.db 25d             ;(pic08) Squash Left            ->
		.db 25d             ;(pic09) Stretch Right          ->
        .db 23d             ;(pic10) Squash Right           ->
		.db 23d             ;(pic11) Stretch Left           ->
		.db 13d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 15d             ;(pic14) Squat Middle           ->
		.db 24d             ;(pic15) Stretch Up             ->
		.db 24d             ;(pic16) = (pic14)
		.db 24d             ;(pic17) = (pic13)
		.db 24d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         -> 
        .db 19d             ;(pic20) Slight Turn Left       -> 
		.db 24d             ;(pic21) Standing               -> 
		.db 24d             ;(pic22) = (pic20)              ->
		.db 22d             ;(pic23) Slight Turn Right      ->
		.db 24d             ;(pic24) = (pic21)
		.db (26d+$80)       ;(pic25) = (pic19)
		.db 24d             ;(pic26) = (pic20)
		.db 27d             ;(pic27) Waiting                ->
		.db 28d             ;(pic28) Leaning                ->     
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->

;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
walk    .db 01d             ;(pic00) JW Up Left             ->
		.db 02d             ;(pic01) JW Contact Left        -> 
		.db 09d             ;(pic02) JW Pass Left           -> 
		.db 05d             ;(pic03) JW Kickoff Left        -> 
		.db 05d             ;(pic04) JW Up Right            ->
		.db 06d             ;(pic05) JW Contact Right       -> 
		.db 11d             ;(pic06) JW Pass Right          -> 
		.db 01d             ;(pic07) JW Kickoff Right       -> 
		.db 02d             ;(pic08) Squash Left            ->
		.db (10d+$80)       ;(pic09) Stretch Right          ->
        .db 06d             ;(pic10) Squash Right           ->
		.db (08d+$80)       ;(pic11) Stretch Left           ->
		.db 09d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 21d             ;(pic14) Squat Middle           ->
		.db 14d             ;(pic15) Stretch Up             ->
		.db 21d             ;(pic16) = (pic14)
		.db 18d             ;(pic17) = (pic13)
		.db 21d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         ->
        .db 19d             ;(pic20) Slight Turn Left       ->
		.db 20d             ;(pic21) Standing               ->
		.db 20d             ;(pic22) = (pic20)              ->
		.db 22d             ;(pic23) Slight Turn Right      ->
		.db 20d             ;(pic24) = (pic21)
		.db 26d             ;(pic25) = (pic19)
		.db 24d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->
        

;**************************************************************************************
; Jogging is an abbreviated 4-Step Run cycle with 'Up' removed from the animation
; Contact -> Pass -> Kickoff
;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
jog     .db (01d+$80)       ;(pic00) JW Up Left             -> JW Contact Left
		.db 02d             ;(pic01) JW Contact Left        -> JW Pass Left
		.db 03d             ;(pic02) JW Pass Left           -> JW Kickoff Left
		.db (05d+$80)       ;(pic03) JW Kickoff Left        -> JW Contact Right
		.db (05d+$80)       ;(pic04) JW Up Right            -> JW Contact Right
		.db 06d             ;(pic05) JW Contact Right       -> JW Pass Right
		.db 07d             ;(pic06) JW Pass Right          -> JW Kickoff Right 
		.db (01d+$80)       ;(pic07) JW Kickoff Right       -> JW Contact Left
		.db 02d             ;(pic08) Squash Left            ->
		.db 10d             ;(pic09) Stretch Right          ->
        .db 06d             ;(pic10) Squash Right           ->
		.db 08d             ;(pic11) Stretch Left           ->
		.db 09d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 19d             ;(pic14) Squat Middle           ->
		.db 19d             ;(pic15) Stretch Up             ->
		.db 19d             ;(pic16) = (pic14)
		.db 16d             ;(pic17) = (pic13)
		.db 19d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         -> JW Pass Left
		.db 19d             ;(pic20) Slight Turn Left       -> Turn/Pass Left
		.db 19d             ;(pic21) Standing               -> Turn/Pass Left
		.db 19d             ;(pic22) = (pic20)              ->
		.db 02d             ;(pic23) Slight Turn Right      ->
		.db 19d             ;(pic24) = (pic21)
		.db 02d             ;(pic25) = (pic19)
        .db 19d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->
        


;**************************************
; Run is a 4-Cycle step
; Contact -> Pass -> Kickoff -> Up
;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
run     .db (01d+$80)       ;(pic00) JW Up Left             -> JW Contact Left
        .db 02d             ;(pic01) JW Contact Left        -> JW Pass Left
        .db 03d             ;(pic02) JW Pass Left           -> JW Kickoff Left
        .db 04d             ;(pic03) JW Kickoff Left        -> JW Up Right
		.db (05d+$80)       ;(pic04) JW Up Right            -> JW Contact Right
		.db 06d             ;(pic05) JW Contact Right       -> JW Pass Right
		.db 07d             ;(pic06) JW Pass Right          -> JW Kickoff Right 
		.db 00d             ;(pic07) JW Kickoff Right       -> JW Up Left
		.db 02d             ;(pic08) Squash Left            ->
		.db 10d             ;(pic09) Stretch Right          ->
        .db 06d             ;(pic10) Squash Right           ->
		.db 07d             ;(pic11) Stretch Left           ->
		.db 13d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 19d             ;(pic14) Squat Middle           ->
		.db 19d             ;(pic15) Stretch Up             ->
		.db 19d             ;(pic16) = (pic14)
		.db 16d             ;(pic17) = (pic13)
		.db 19d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         ->
        .db 19d             ;(pic20) Slight Turn Left       ->
		.db 19d             ;(pic21) Standing               ->
		.db 19d             ;(pic22) = (pic20)              ->
		.db 02d             ;(pic23) Slight Turn Right      ->
		.db 19d             ;(pic24) = (pic21)
		.db 02d             ;(pic25) = (pic19)
		.db 19d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->

;Jump Table 1 for jump from stop or walk
;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
jump1   .db 01d             ;(pic00) JW Up Left             ->
		.db 02d             ;(pic01) JW Contact Left        -> 
		.db 25d             ;(pic02) JW Pass Left           -> 
		.db 02d             ;(pic03) JW Kickoff Left        -> 
		.db 05d             ;(pic04) JW Up Right            ->
		.db 06d             ;(pic05) JW Contact Right       -> 
		.db 11d             ;(pic06) JW Pass Right          -> 
		.db 11d             ;(pic07) JW Kickoff Right       -> 
		.db 09d             ;(pic08) Squash Left            ->
		.db 02d             ;(pic09) Stretch Right          ->
        .db 25d             ;(pic10) Squash Right           ->
		.db 25d             ;(pic11) Stretch Left           ->
		.db 17d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 15d             ;(pic14) Squat Middle           ->
		.db 15d             ;(pic15) Stretch Up             ->
		.db 15d             ;(pic16) = (pic14)
		.db 14d             ;(pic17) = (pic13)
		.db 15d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         ->
        .db 19d             ;(pic20) Slight Turn Left       ->
		.db 20d             ;(pic21) Standing               ->
		.db 24d             ;(pic22) = (pic20)              ->
		.db 22d             ;(pic23) Slight Turn Right      ->
		.db 13d             ;(pic24) = (pic21)
		.db 26d             ;(pic25) = (pic19)
		.db 24d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->

;Jump Table 2 for jump from jog or run
;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
jump2   .db 00d             ;(pic00) JW Up Left             ->
		.db 02d             ;(pic01) JW Contact Left        -> 
		.db 03d             ;(pic02) JW Pass Left           -> 
		.db 04d             ;(pic03) JW Kickoff Left        -> 
		.db 04d             ;(pic04) JW Up Right            ->
		.db 06d             ;(pic05) JW Contact Right       -> 
		.db 07d             ;(pic06) JW Pass Right          -> 
		.db 00d             ;(pic07) JW Kickoff Right       -> 
		.db 02d             ;(pic08) Squash Left            ->
		.db 10d             ;(pic09) Stretch Right          ->
        .db 06d             ;(pic10) Squash Right           ->
		.db 07d             ;(pic11) Stretch Left           ->
		.db 13d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 21d             ;(pic14) Squat Middle           ->
		.db 21d             ;(pic15) Stretch Up             ->
		.db 16d             ;(pic16) = (pic14)
		.db 16d             ;(pic17) = (pic13)
		.db 21d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         ->
        .db 19d             ;(pic20) Slight Turn Left       ->
		.db 20d             ;(pic21) Standing               ->
		.db 20d             ;(pic22) = (pic20)              ->
		.db 22d             ;(pic23) Slight Turn Right      ->
		.db 20d             ;(pic24) = (pic21)
		.db 26d             ;(pic25) = (pic19)
		.db 24d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->
        
;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
fall    .db 01d             ;(pic00) JW Up Left             ->
		.db 02d             ;(pic01) JW Contact Left        -> 
		.db 25d             ;(pic02) JW Pass Left           -> 
		.db 02d             ;(pic03) JW Kickoff Left        -> 
		.db 05d             ;(pic04) JW Up Right            ->
		.db 23d             ;(pic05) JW Contact Right       -> 
		.db 23d             ;(pic06) JW Pass Right          -> 
		.db 06d             ;(pic07) JW Kickoff Right       -> 
		.db 25d             ;(pic08) Squash Left            ->
		.db 25d             ;(pic09) Stretch Right          ->
        .db 23d             ;(pic10) Squash Right           ->
		.db 23d             ;(pic11) Stretch Left           ->
		.db 13d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 15d             ;(pic14) Squat Middle           ->
		.db 24d             ;(pic15) Stretch Up             ->
		.db 16d             ;(pic16) = (pic14)
		.db 24d             ;(pic17) = (pic13)
		.db 24d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         ->
        .db 19d             ;(pic20) Slight Turn Left       ->
		.db 24d             ;(pic21) Standing               ->
		.db 24d             ;(pic22) = (pic20)              ->
		.db 22d             ;(pic23) Slight Turn Right      ->
		.db 24d             ;(pic24) = (pic21)
		.db 26d             ;(pic25) = (pic19)
		.db 24d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->  
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->

;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
fall2   .db 00d             ;(pic00) JW Up Left             ->
		.db 02d             ;(pic01) JW Contact Left        -> 
		.db 03d             ;(pic02) JW Pass Left           -> 
		.db 04d             ;(pic03) JW Kickoff Left        -> 
		.db 04d             ;(pic04) JW Up Right            ->
		.db 06d             ;(pic05) JW Contact Right       -> 
		.db 07d             ;(pic06) JW Pass Right          -> 
		.db 00d             ;(pic07) JW Kickoff Right       -> 
		.db 02d             ;(pic08) Squash Left            ->
		.db 10d             ;(pic09) Stretch Right          ->
        .db 06d             ;(pic10) Squash Right           ->
		.db 07d             ;(pic11) Stretch Left           ->
		.db 13d             ;(pic12) Sitting                ->
		.db 14d             ;(pic13) Squat Low              ->
		.db 21d             ;(pic14) Squat Middle           ->
		.db 21d             ;(pic15) Stretch Up             ->
		.db 16d             ;(pic16) = (pic14)
		.db 16d             ;(pic17) = (pic13)
		.db 21d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         ->
        .db 19d             ;(pic20) Slight Turn Left       ->
		.db 20d             ;(pic21) Standing               ->
		.db 20d             ;(pic22) = (pic20)              ->
		.db 22d             ;(pic23) Slight Turn Right      ->
		.db 20d             ;(pic24) = (pic21)
		.db 26d             ;(pic25) = (pic19)
		.db 24d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->
        
;**************************************************************************************
                            ;THIS PIC                               NEXT PIC
land    .db 01d             ;(pic00) JW Up Left             ->
		.db 02d             ;(pic01) JW Contact Left        -> 
		.db 25d             ;(pic02) JW Pass Left           -> 
		.db 02d             ;(pic03) JW Kickoff Left        -> 
		.db 05d             ;(pic04) JW Up Right            ->
		.db 23d             ;(pic05) JW Contact Right       -> 
		.db 23d             ;(pic06) JW Pass Right          -> 
		.db 06d             ;(pic07) JW Kickoff Right       -> 
		.db 25d             ;(pic08) Squash Left            ->
		.db 25d             ;(pic09) Stretch Right          ->
        .db 23d             ;(pic10) Squash Right           ->
		.db 23d             ;(pic11) Stretch Left           ->
		.db 12d             ;(pic12) Sitting                ->
		.db 12d             ;(pic13) Squat Low              ->
		.db 15d             ;(pic14) Squat Middle           ->
		.db 16d             ;(pic15) Stretch Up             ->
		.db 17d             ;(pic16) = (pic14)
		.db 18d             ;(pic17) = (pic13)
		.db 24d             ;(pic18) = (pic14)
		.db 02d             ;(pic19) Turn/Pass Left         ->
        .db 19d             ;(pic20) Slight Turn Left       ->
		.db 24d             ;(pic21) Standing               ->
		.db 24d             ;(pic22) = (pic20)              ->
		.db 22d             ;(pic23) Slight Turn Right      ->
		.db 24d             ;(pic24) = (pic21)
		.db 26d             ;(pic25) = (pic19)
		.db 24d             ;(pic26) = (pic20)
		.db 24d             ;(pic27) Waiting                ->
		.db 24d             ;(pic28) Leaning                ->  
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 08d             ;(pic32) Arm Extending          ->
        .db 32d             ;(pic33) Arm Extended           ->

;**************************************************************************************
                            ;THIS PIC                               NEXT PIC        
pushb   .db 01d             ;(pic00) JW Up Left             ->             
        .db 02d             ;(pic01) JW Contact Left        -> 
        .db 09d             ;(pic02) JW Pass Left           -> 
        .db 05d             ;(pic03) JW Kickoff Left        -> 
        .db 05d             ;(pic04) JW Up Right            ->
        .db 06d             ;(pic05) JW Contact Right       -> 
        .db 11d             ;(pic06) JW Pass Right          -> 
        .db 01d             ;(pic07) JW Kickoff Right       -> 
        .db 32d             ;(pic08) Squash Left            ->      Arm Extending
        .db (10d+$80)       ;(pic09) Stretch Right          ->
        .db 06d             ;(pic10) Squash Right           ->
        .db (08d+$80)       ;(pic11) Stretch Left           ->
        .db 09d             ;(pic12) Sitting                ->
        .db 14d             ;(pic13) Squat Low              ->
        .db 21d             ;(pic14) Squat Middle           ->
        .db 14d             ;(pic15) Stretch Up             ->
        .db 21d             ;(pic16) = (pic14)
        .db 18d             ;(pic17) = (pic13)
        .db 21d             ;(pic18) = (pic14)
        .db 11d             ;(pic19) Turn/Pass Left         ->  was 02d
        .db 19d             ;(pic20) Slight Turn Left       ->
        .db 20d             ;(pic21) Standing               ->
        .db 20d             ;(pic22) = (pic20)              ->
        .db 22d             ;(pic23) Slight Turn Right      ->
        .db 20d             ;(pic24) = (pic21)
        .db 26d             ;(pic25) = (pic19)
        .db 24d             ;(pic26) = (pic20)
        .db 24d             ;(pic27) Waiting                ->
        .db 24d             ;(pic28) Leaning                ->
        .db 30d             ;(pic29) Smack Wall             ->
		.db 31d             ;(pic30) Fall Off Wall          ->
		.db 12d             ;(pic31) Sitting After Wall     ->
        .db 33d             ;(pic32) Arm Extending          ->
        .db 33d             ;(pic33) Arm Extended           -> Stays here
       
;Teetering Pictures
teettbl .byte mpic_vpg,         mpic_vpg,           mpic_vpg,           mpic_vpg+v_xflip
        .byte mpic_vpg,         mpic_vpg+v_xflip,   mpic_vpg+v_xflip,   mpic_vpg+v_xflip
        .byte mpic_vpg,         mpic_vpg+v_xflip,   mpic_vpg,           mpic_vpg
        .byte mpic_vpg+v_xflip, mpic_vpg+v_xflip,   mpic_vpg+v_xflip,   mpic_vpg

;Choking Pictures
choktbl .db idx_pic32		;Choking High
        .db idx_pic32
        .db idx_pic32
        .db idx_pic34       ;Choking Lower
        .db idx_pic34
        .db idx_pic34
        .db idx_pic36       ;Dead on ground
        .db idx_pic36 
        
		