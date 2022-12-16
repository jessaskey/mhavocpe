;*****************************************
    .TEXT ".TWTACT."
;*****************************************
    .title "Tactical Display Routines"
;*****************************************
;* Entry just after ship is drawn, scale *
;* previously set.                       *
;*****************************************
;* Message levels (places for text message)
;*(Recall these are -1 from displayed level)

;?mesl1   =   4
;?mesl2   =   10
;?mesl3   =   13
;?mesl4   =   15

disptact
		lda animate
		ifmi
			jmp dowormh				;Do the wormhole animation
		else
			;Normal Tact Scan here
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colblue)
			VGADD_JSR(tactd0)    ;Outer border - blue
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colwhite)
			VGADD_JSR(tactd1)    ;Inner border - white
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colgreen)
			VGADD_JSR(tactd2)    ;Long Range Scanner - green
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$D,colred2)
			VGADD_JSR(tactd3)    ;Homeworld Bottle - redr - don't draw this for wormhole
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$D,colcyan)
			VGADD_JSR(tactd4)    ;Bottom Planet - cyan - don't draw this for wormhole
			lda difcty
			cmp #maxdif
			ifne
				VGADD_JSR(tactd5)    ;Long Range Scanner Distance Dots - colors embedded - don't draw these for wormhole
			endif
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colcyan)
			VGADD_JSR(tactd6)    ;Confirmation Box - cyan
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colred)
			VGADD_JSR(tactd7)    ;Heart Rate Scanner - red
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colpurple)
			VGADD_JSR(tactd8)    ;Breakout Box - purple
			VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colyellow)
			VGADD_JSR(tactd9)    ;Center Scanner - yellow			
		endif

		lda shpsch          ;Scale of this display
		cmp #$73            ;Hold rest until out
		ifne
			rts             ;Skip the rest until small
		endif
		
		;Main Timing Logic here
        lda frame
        and #$0F                ;Seconds
        ifeq
            inc ttime
        endif
        lda ttime
		cmp #03
		ifeq
			;Tesser time
#IF (FORCE_TESSERACT_ANIMATION > 0)
			lda #$80
			sta tokretr
#ENDIF
			lda tokretr
			ifmi
				;Need to start the animation, this will cause the animation control below to run
				lda #$80
				sta animate
				lda #snd_fail
				jsr dosound
			endif
		endif	
		;********************************************
		; Tact Timing Checks
		;********************************************
		; jsr ?eexit			;should be do an early exit?
		; ifcs
			; lda ttime
			; cmp #28d				;only do this once
			; ifcc
				; lda #28d
				; sta ttime				;Flag as time out for ending sound, 2 ticks remain
				; ;lda #$80
				; ;sta brstat				;don't launch any more now!
			; endif
		; endif
		lda ttime
		cmp #30d                ;30 half seconds - Max time allowed
		bcs ?tdone				;More than 30d always exits
		cmp #12d                ;Longer enough??
		ifcs                    ;yes!
			ldx perm1               ;Is this showing a warp message? 
			ifmi                    ;nope, can leave early
				bit brstat          	;Playing breakout?
				ifpl                    ;no
?tdone           	lda #$40
					sta tact            	;End Wave
					lda #$80        		;Re-open and outside
					sta rmazenum			;Clear any random maze messages (they will be re-randomized on maze show)
					lda #00
					sta tactde          	;Turn off enable for next time
					sta targf           	;Tell mainline not at target anymore (kludge)
					jsr dostop              ;stop all sounds
					lda ttime
					cmp #13d                ;If leaving because of breakout
					ifcs
						lda #snd_b2d
						jsr dosound         ;Must have used breakout to get here
					endif
				endif
			endif
		endif
		
		;Sound Stuff
        lda sndcue
        ifeq
			lda #$80
            sta sndcue
            lda #snd_b1a
            jsr dosound
        endif
        lda condition
        cmp #04             ;Red alert?
        ifeq
            lda frame
            and #$7F
            ifeq
                lda #snd_b1e
                jsr dosound
            endif
        endif
		
		;Do we need to display warp info?
        ldx dif4mz
        lda ?teltab,X            ;What to tell
        cmp wrplvl
        ifcc
            lda #$FF            ;Don't tell player about warp if he used it already
        endif
        sta perm1	
		;*****************************************************
        ;If this is a message level, make sure we have the 
        ;data in RAM for the current warp level 
		;This is a potential GAMMA call
		;*****************************************************
		ifpl
			jsr getwarp
		endif
	
        ;Now add storage screen object
		jsr vgcntr
		lda seqst               ;Object Status
		and #$20                ;Erase bit??
		ifne
			lda frame
			and #07
			ifeq
				lda #00
				sta seqx
				sta seqst               ;Done with erase
				beq ?heart              ;Skip here and do next
			endif
			lda #$F0+colwhite           ;Bright to erase
		else
			lda #$A0+colwhite           ;In 'light white'
		endif
		ldx #$60                
		jsr vgadd2              ;Write Color
		VGADD_VCTRS(68,127,hidden)
		VGADD_VCTRS(0,34,hidden)	;At this scale, 2 vectors to get there
		lda isfinal
		ifne
			lda #04             ;Show cube
		else
			lda seqp            ;Which picture
			and #03
		endif
		asl a               ;Word entries
		tax 
		lda ?dislist,X      ;Get address of source
		sta temp2
		lda ?dislist+1,X    ;Get MSB
		sta temp2+1
		jsr copypic         ;Do fancy display of vector

		;* Time for Heart Scanner *
?heart	jsr vgcntr
        ldx player
        lda lives,X 
        cmp #4
        ifcs
            lda #($A0+colgreen)
        else
            cmp #1
            ifcs
                lda #($A0+colyellow)
            else
                lda #($A0+colred)
            endif
        endif
        ldx #$60
		jsr vgadd2          ;Color
		lda #00
		ldx #$72            ;This may be Scale 2
		jsr vgadd2
		VGADD_VCTRS(-6,64,hidden)
		lda condition       ;Which one?
		tax                 ;Save in X
		lsr A
		tay 
		lda ?hsbox,Y        ;Get color for this
		sta colram+4        ;In this location
		jsr ?hscand         ;Display it

		;Long Range Scanner
		jsr ?lrscan      
		
		ldx #mcont
		jsr mesg_rtp5       ;Condition Message

		lda isfinal
		ifne
			ldx #mahome
			jsr mesg_rtp5
		else
			lda brstat          ;Playing??
			ifne                ;Button was pressed
				bit perm1               ;Warp Display Wave?
				ifmi                    ;Yes if +, (else go to hell!)
					ldx #mgarbage
					jsr mesg_rtp5
				endif
			endif
		endif
		
		lda condition
		cmp #04             ;Don't go past 4
		iflt
			ldx ttime
			lda ?condtbl,X      ;What condition here
			sta condition
		endif
		lsr A
		tax 
		lda ?tmess,X
		tax 
		jsr mesg_rtp5       ;Condition message
		
		;NOTE: Vector Timing Logic was here
		
		;Long Range Scanner Update Logic - used to switch between different confirmed enemies
		lda seqst
		ifeq                ;If zero, then this is a fresh start, get everything initialized
			lda frame
			and #$0F
			ifeq
				lda seqp
				cmp maznum          ;Same pic?
				ifne
					inc seqp
					lda seqp
					and #03
					sta seqp
					lda #0
					sta seqx		;reset back to start
				endif
				inc seqst
			endif
			lda seqst               ;Restore signs
		endif
		ifmi				;All is drawn now, add text
			lda seqp
			cmp maznum          ;Same pic??
			ifeq
				lda isfinal
				ifne
					ldx #04             ;Show cube
				else
					ldx maztype
				endif
				lda ?etypm,X         ;Enemy Type message
				tax 
				jsr mesg_rtp5
				bit frame
				ifvs
					ldx #mconf          ;Confirmed message
					jsr mesg_rtp5
				endif
			else
				lda frame
				and #$1F
				cmp #$1C                ;Diff is erase flash time
				ifeq
					lda #$A0
					sta seqst
				endif
			endif
		endif
		
		ldx #mmdist
		jsr mesg_rtp5       ;Distance Message
		lda #$C0
		sec 
		sbc lroff           ;Get Distance
		jsr decimal
		ldy #02
		lda #temp7
		sec 
		jsr digits          ;Display distance
		
		lda frame
		and #08
		ifne
			ldx #mcols          ;Guess Closing
			lda lroff
			cmp #$A0
			ifcs                    ;Nope, Holding
				ldx #mhold
			endif
			jsr mesg_rtp5
		endif
		ldx #metyp
		jsr mesg_rtp5		;Tell Enemy Type
		
		;Only show the 'Level' number if not in the special levels and not a wormhole level
		lda difcty
		cmp #maxdif
		ifne
			lda tokretr
			ifpl
				ldx #mmlevel        ;Tell Level
				jsr mesg_rtp5
				lda difcty          ;Level is difcty*4+maznum
				asl a
				asl a
				sec 
				adc maznum
				jsr decimal         ;In decimal
				ldy #01
				lda #temp7
				jsr digits
			endif
		endif
		
		;Timer Line
        jsr vgcntr
        lda #($F0+colorange)
        ldx #$60
        jsr vgadd2          ;Color
        lda #00
        ldx #$72
        jsr vgadd2          ;Size
		VGADD_VCTRS(08,64,hidden)
        lda frame
        and #$0F
        ifeq
            lda rands
            sta tactln
        endif
        lda tactln
        and #$17
        ldy #$20                ;Use Stat
        ldx #00                 ;No Y deflection
        jsr vgvtr               ;Display time left line (above heart scan)
        ldx perm1               ;Display Wave??
        ifpl
            lda frame
            lsr A
            lsr A
            ora #$E0
            sta colram+$0F          ;Flash color for this
            ldx #mwarpi
            jsr mesg_rtp5
            ldy perm1
            ldx ?whathint,Y
            jsr mesg_rtp5           ;Put up warp info
            jsr vgcntr
			VGADD_VCTRS(89,-100,hidden)
            ldy perm1  
            lda wdigits,Y
            sta perm4           ;Number of digits (2 or 3)
            tya
            asl A               ;x2
            tax                 ;X is index into warpcodes data  
            lda warpcodes,X
            sta perm5
            lda warpcodes+1,X
            sta perm5+1
            clc
            ldy perm4
            lda #perm5
            jsr sdigits
        else
			ldy maztype
            lda isfinal
            ifne
                ldy #04             ;Show cube message
            endif
            ldx ?tmesg,Y         ;Get message this wave
            jsr mesg_rtp5
        endif
		
        ;***************************************
        ;* .sbttl "Center Scanner Pic Control"
        ;***************************************
		;lda #00
		;ldx #$73            ;Scale for window position vector
		;jsr vgadd2
		VGADD_SCALE(ywin_off,binscal3,0)		;Scale for window position vector
		jsr vgcntr      
		VGADD_VCTRS(127,0,hidden)	;Position for center pic
		;lda #00
		;ldx #$79
		;jsr vgadd2          ;Turn on Y-Window
		VGADD_SCALE(ywin_on,binscal1,0)		;Turn on Y-Window
		jsr vgcntr          ;Recenter
		lda #$C0
		sec                 ;Position is 0C0-lroff (0A0 Max)
		sbc lroff           ;Get position
		sta vdata+2
		lda #00
		sta vdata
		sta vdata+1
		sta vdata+3         ;zero X and Y MSB
		jsr vgvtr2          ;Draw vector
		;lda #00
		;ldx #$73            ;Scale of center pic
		;jsr vgadd2
		VGADD_SCALE(ywin_off,binscal3,0)

		;set up stuff different for final maze
		lda #($E0+colgreen) ;Color and page select of pic
		ldx isfinal
		ifne
			lda #($E0+colwhite) 	;CUBE
		endif
		ldx #tacct_vpg
		jsr vgadd2

		lda maztype			;Get correct pic
		ldx isfinal
		ifne
			lda #04             ;Show the vax cube
		endif
		asl a
		tay                 ;*2 in Y for table lookup
		lda cstations,Y
		ldx cstations+1,Y   ;Get center pic
		jsr vgadd2            
		VGADD_SCALE(ywin_off,binscal1,0)		;Reset Scale			
		VGADD_VCTRS(127,0,hidden)
		VGADD_SCALE(ywin_on,binscal1,0)		;Put beam killer way at top again
		
		;* Get Rolly-Gig data for warp code.....
        lda wrpwh           ;Skip it, bad digits entered??
        ora diedyet         ;Or player died or too high a wave??
        ifmi
            rts                 ;yep!
        endif
        ldx dif4mz
        ldy ?whichw,X       ;which warp are we on?
        ifmi
            rts                 ;If table contains -1, then no warp available (Homeworld only)
        endif
		
		;Gather warp info, set Y in temp9+1 which is used down many places
		sty temp9+1
		lda wdigits,Y
		sta perm2           ;Number of digits (2 or 3)
		tya
		asl A               ;x2
		tax                 ;X is index into warpcodes data      
		;lda ?warpc0,Y
		lda #00             ;First Digit
		jsr getdigit
		sta perm2+1         ;Warp code in perm2+1 to perm3+1
		;lda ?warpc1,Y
		lda #01             ;Second Digit
		jsr getdigit
		sta perm3
		;lda ?warpc2,Y       ;If no third digit, code must be 0
		lda #02             ;Third Digit
		jsr getdigit
		sta perm3+1
			
		ldx whatmes,Y
		jsr mesg_rtp5       ;Display 'color' warp message
		
		lda #00
		sta perm4               ;Draw 2 or 3 digit code
		begin
			cmp wrpwh
			ifcs
				lda #($F0+colwhite)	;White before entered
			else
				ldy temp9+1         ;Otherwise, warp code color instead
				lda warpcol,Y
			endif
			ldx #$60
			jsr vgadd2          ;Alphanumerics in 5000 page
			ldy perm4
			lda wrpdat,Y
			jsr vghex               ;And the digit
			inc perm4
			lda perm4
			cmp perm2
		eqend
		
		ldx wrpwh
        cpx perm2               ;All digits entered?
        ifeq                    ;Yes
            jsr ?compcod        ;Compare codes
            bne ?rgw10          ;If they don't match, leave
            ldy temp9+1
            jsr warpstat
            iny 
            sty wrplvl          ;Store away warp level
            dey 
            lda wherel,Y        ;Get the new Level warped too            
            sta sellvl
            dec sellvl          ;Make it zero based 
            jsr thisone_rtpg5   ;Set up new level, call the special one to keep this page tho
            jsr newscore        ;Give them points
            lda #snd_b3b
            jsr dosound
; #IF ENABLE_ADAPTIVE_DIFFICULTY != 0
            ; ldx dif4mz
            ; lda expdth,X
            ; sta incded
; #ENDIF
            sed                 ;*** DECIMAL MODE ***
            lda st_warps
            clc 
            adc #01             ;Another warp taken
            sta st_warps
            lda st_warps+1
            adc #00
            ifcc
                sta st_warps+1         ;Don't wrap
            endif
            cld                 ;*** END DECIMAL MODE ***
            
            ;NO POINTS FOR WARPING ANYMORE!!!!!
            ;ldx #00
            ;lda player              ;Who is this?
            ;ifne                    ;It's player 2
            ;    ldx #score2-score       ;Index to score 2
            ;endif
            ;ldy temp9+1
            ;tya
            ;asl a                   ;x2
            ;tay                     ;back to Y
            ;lda ?warpts,Y
            ;sta score+2,X           ;Bonus points
            ;lda ?warpts,Y
            ;sta score+2,X           ;Bonus points
            ;lda #00
            ;sta score+1,X
            ;sta score,X
            
            ldy temp9+1
            lda whatliv,Y
            ifne
                jsr ?addlif          ;Player gets another life
            endif
            lda #$80
            sta scoflg          ;Indicate change
?rgw10  else				;no match here
            ldy #00
            lda rgdd
            asl a
            asl a
            ifmi
                ldy #$99
            endif
            clc 
            adc wrpdl
            sta wrpdl
            sed 
            tya 
            adc wrpdat,X
            and #$0F
            sta wrpdat,X
            cld 
            bit jblast          ;Last on or off?
            ifmi                    ;Was off
                bit jbstat          ;Jump to lock
                ifpl
                    ldx wrpwh
                    lda perm2+1,X
                    cmp wrpdat,X
                    ifeq
                        lda #00
                        sta ttime           ;Give Him/her more time to enter second digit
                    endif
                    inc wrpwh
                    ldx wrpwh
                    cpx perm2
                    ifne
?rgw20                  lda #snd_b3a            ;Digit Entered
                    else
                        jsr ?compcod         ;All digits entered, correct code?
                        bne ?rgw20
                        lda #snd_b3b
                    endif
                    jsr dosound
                endif
            endif
        endif
		
		;Update Jump button status/last
        lda jbstat
        sta jblast          ;Store last
        rts 

;****************************************************
; Do Wormhole Animation
; Used when jumping to different levels
;****************************************************
; Some space variables for this section
whlevels 	= 8
whtrigger   = $07			;How many counts before we start the next level (bitmask)
whlevel 	= sobjst
whcount		= sobjst+1
whending 	= sobjst+2
whcolor		= sobjst+3
whzoom		= sobjxl
whcolors	= sobjxh

dowormh ;Animation Control First
		lda animate
		and #$70			;Get Animation stage
		lsr A 
		lsr A
		lsr A
		tax
		lda whctrlt+1,X
		pha
		lda whctrlt,X 
		pha
		rts
		
whctrlt
		.dw wh0-1		;80 - Wiggly
		.dw wh1-1		;90 - Zooming
		.dw wh2-1		;A0
		
;**************************************
;Stage 0 - Tact and start
;**************************************
wh0		lda animate
		and #$08
		ifeq
			;Animation Stage 0 - Wavering tact
			lda shpscl
			ifeq
				lda frame
				lsr A 
				and #$1F
				ifeq
					inc animate
				endif
			endif
			lda shpscl
			ldx shpsch
			jsr vgadd2
			jsr minitact

			jsr vgcntr			
			lda #0             ;X to tok position
			ldx #$27           ;Y to tok position
			jsr vgvtr5
			
			lda #00
			ldx #$70
			jsr vgadd2
			lda lasttok				;Which tesseract?
			sta temp2
			lda frame
			lsr A 
			and #$1F
			cmp #$10
			ifcs
				eor #$1F
			endif
			asl A
			asl A 
			asl A 
			asl A
			sta temp1
			jsr drawtok

			;Black hole level, show special message
			ldx temp2
			lda tessermsg,X
			tax
			jsr mesg_rtp5
		else 
			;wavering over, move to next stage
			lda #$90
			sta animate
			;Reset all levels to $00 which disables them
			ldy #whlevels-1
			lda #00
			begin
				sta whzoom,Y
				dey 
			miend
			sta whcount
			sta whending
			lda #$F0+(colbluer)
			sta whcolor
			lda #1
			sta whlevel
			lda #snd_launch
			jsr dosound
			
			lda #$40
			sta tact            ;End Wave
			;lda #00
			;sta tactde          ;Turn off enable for next time
			;sta targf           ;Tell mainline not at target anymore (kludge)
		endif
		rts

tessermsg
		.db mtesser0,mtesser1,mtesser2,mtesser3
;**************************************
;Stage 1 - warp in
;**************************************		
wh1		;Show existing tact scan so it can zoom out
		lda shpscl
		cmp #$16
		ifne
			ldx shpsch
			jsr vgadd2
			jsr minitact
		endif
		;lda frame
		;and #$00
		;ifeq
		;time to move, increase each level which is enabled (!= 0)
		ldx #whlevels-1
		begin
			lda whzoom,X
			ifne
				and #$F0
				lsr A 
				lsr A 
				lsr A 
				lsr A
				tay 
				lda whzoom,X
				sec
				sbc bhamount,Y
				sta whzoom,X
				;bcs ?bhcancel			;If we overflowed, then stop it
				cmp #$30
				ifcc
					;offscreen, cancel it
					lda #0
					sta whzoom,X
				endif
			else
				;this hasn't started, should it?
				lda whending			;not if we are attempting to end tho
				ifeq
					cpx whlevel
					ifmi
						dec whzoom,X		;this starts it...
						;set color too
						lda whcolor
						sta whcolors,X
					endif
				endif
			endif
			dex
		miend
		
		;See if it is time to start the next level
		inc whcount
		lda whcount
		and #whtrigger
		ifeq
			inc whlevel
			lda whlevel
			cmp #$10			;When to trigger the next sound
			ifeq
				lda #snd_bhend		;Long splash sound
				jsr dosound
			endif
			;set up colors
			lda whlevel
			lsr A 
			lsr A 
			lsr A
			tax
			lda whclrs,X
			sta whcolor
		endif
		;endif
		
		lda whlevel
		cmp #$20
		ifcs
			and #$0F
			tax
			lda #0
			sta whzoom,X
			lda #-1
			sta whending
			;see if they are all done now
			lda whzoom
			ora whzoom+1
			ora whzoom+2
			ora whzoom+3
			ora whzoom+4
			ora whzoom+5
			ora whzoom+6
			ora whzoom+7
			ifeq
				;all done!
				lda #$00
				sta tact            ;End Tact
				lda #00
				sta tactde          ;Turn off enable for next time
				sta targf           ;Tell mainline not at target anymore (kludge)
				sta animate			;Stop the animation
				;reset the RAM we used for this special animation
				ldx #3
				begin
					sta	sobjst,X
					dex
				miend
				sta sobjxl
				sta sobjxh
			endif
		endif
		
		
		lda #whlevels-1
		sta temp2
		begin
			ldx temp2
			;VGADD_STAT(sparkle_off,xflip_off,tact_vpg,$A,colbluer)
			;Load the color for this line
			lda whcolors,X
			ldx #tact_vpg
			jsr vgadd2
			ldx temp2
			lda whzoom,X
			ifne
				;We can draw this one
				ldx #$71				;binary scaling into X
				jsr vgadd2
				VGADD_JSR(tactd9)    ;Center Scanner
			endif
			dec temp2
		miend
		rts
		
;Last animation step, end tact scan now		
wh2	
		rts

;our special colors for the wormhole warp sequence	
whclrs	.db $F0+colbluer
		.db $E0+colpurple
		.db $E0+colred2
		.db $E0+colorange
		
;The amount to take away from each level to make it look better		
bhamount	
		.db $10,$0F,$0E,$0D,$0C,$0B,$0A,$09
		.db $08,$07,$06,$05,$04,$03,$02,$01
		
;bhvalue .db $FF,$FD,$FA,$F6,$EF,$E5,$D9,$CB
;		.db $BA,$A6,$8F,$75,$58,$38,$00,$00

minitact 	
		VGADD_STAT(sparkle_on,xflip_off,tact_vpg,$A,colblue)
		VGADD_JSR(tactd0)    ;Outer border - blue
		VGADD_JSR(tactd1)    ;Inner border - white
		VGADD_JSR(tactd2)    ;Long Range Scanner - green
		VGADD_JSR(tactd3)    ;Homeworld Bottle - redr - don't draw this for wormhole
		VGADD_JSR(tactd4)    ;Bottom Planet - cyan - don't draw this for wormhole
		;VGADD_JSR(tactd5)    ;Long Range Scanner Distance Dots - colors embedded - don't draw these for wormhole
		VGADD_JSR(tactd6)    ;Confirmation Box - cyan
		VGADD_JSR(tactd7)    ;Heart Rate Scanner - red
		VGADD_JSR(tactd8)    ;Breakout Box - purple
		VGADD_JSR(tactd9)    ;Center Scanner - yellow			
		rts

whtransform
		;Shape will start as a 512x1024 rectangle
		;Each rendering will control the number of vertex's as they can be different (from 6-12 sides).
		
whanit
		.dw whani0
		.dw whani1
		.dw whani2
		.dw whani3

whanis  .db 6
		.db 6
		.db 12
		.db 12

;Initial vertex locations, absolute position, we will calculate the vectors between them		
whani0 	.dw -256, 512
		.dw 256, 512
		.dw 256, 0
		.dw 256, -512
		.dw -256, -512
		.dw -256, 0

whani1
whani2
whani3

		
;****************************************************
;* Get Warp Code for this level.
;* If we don't have it, then ask GAMMA for it.
;* A = Warp Index (Zero Based)
;*
;* Returns two bytes (4-digits) in temp1.
;****************************************************
getwarp tay
        asl A     ;x2
        tax
        lda warpcodes,X
        cmp #-1
        ifeq
            ;We don't have the Warp code for this level yet
            ;Have GAMMA generate it.
?genw       tya
            jsr wrp1in
        else
            lda warpcodes+1,X       ;Check both, if either are minus, they are not defined
            cmp #-1
            beq ?genw
        endif
        ;we should have the warp codes now
        lda warpcodes,X
        sta temp1
        lda warpcodes+1,X
        sta temp1+1
        rts
        
;***********************************
;* Get Warp Digit
;* Will shift through target warp
;* index to get the zero padded 
;* digit into the LSB of A0
;* Uses:
;* A = Digit (Zero based)
;* X = Word index into warpcodes (warp level)
;* 
;* Returns:
;*  Zero padded digit in A
;***********************************
getdigit
        lsr A
        php
        lsr A
        ifcc
            ;Low Byte
            lda warpcodes,X
			plp
			ifcc
                ;Low Nybble
?lownyb         and #$0F
            else
                ;High Nybble
                lsr A 
                lsr A
                lsr A 
                lsr A
            endif
        else
			plp
            lda warpcodes+1,X
            bne ?lownyb				;Always low nybble on second byte
        endif
        rts

;***********************************
;* Update Stat for Warp Usage
;* Y contains the warp index
;***********************************
warpstat
        tya
        asl A 
        tax                 ;x2
        sed                 ;*** Decimal Mode ***
        lda st_warps,X
        clc
        adc #01
        sta st_warps,X
        lda st_warps+1,X
        adc #00
        ifcc
            sta st_warps+1,X
        endif
        cld
        rts
        
;At what level is the warp code shown to the player        
?teltab .db -1  ;Level 1
        .db -1  ;Level 2
        .db -1  ;Level 3
        .db 00  ;Level 4  = Red
        .db -1  ;Level 5
        .db -1  ;Level 6
        .db -1  ;Level 7
        .db -1  ;Level 8
        .db 01  ;Level 9  = Yellow
        .db 02  ;Level 10 = Green
        .db -1  ;Level 11
        .db -1  ;Level 12
        .db 03  ;Level 13 = Aqua
        .db -1  ;Level 14
        .db -1  ;Level 15
        .db -1  ;Level 16  
        .db 04  ;Level 17 = Blue
        .db -1  ;Level 18 
        .db 05  ;Level 19 = Purple
        .db -1  ;Level 20 
        .db -1  ;Level 21 = Final Maze
        .db -1  ;Level 22 = Final Maze 
        .db -1  ;Level 23 = Final Maze
        .db -1  ;Level 24 = Final Maze
        .db -1  ;Level 25 = Hidden Level no hints
		.db -1  ;Level 26 = Hidden Level no hints
		.db -1  ;Level 27 = Hidden Level no hints
		.db -1  ;Level 28 = Hidden Level no hints
        ;We won't show the final warp code until after 
        ;the homeworld is completed this is done in the 
        ;winner sequence
        ; 06  ;Homeworld = Pink


;For each level, if the player has not died too 
;many times, which warp is available for the 
;player to enter via the tactical scanner.
?whichw .db 0   ;Level 1
        .db 0   ;Level 2
        .db 0   ;Level 3
        .db 1   ;Level 4
        .db 1   ;Level 5
        .db 1   ;Level 6
        .db 1   ;Level 7
        .db 1   ;Level 8
        .db 2   ;Level 9
        .db 3   ;Level 10
        .db 3   ;Level 11
        .db 3   ;Level 12
        .db 4   ;Level 13
        .db 4   ;Level 14
        .db 4   ;Level 15
        .db 4   ;Level 16
        .db 5   ;Level 17
        .db 5   ;Level 18
        .db 6   ;Level 19
        .db 6   ;Level 20
        .db -1  ;Level 21 - Final Maze
        .db -1  ;Level 22 - Final Maze
        .db -1  ;Level 23 - Final Maze
        .db -1  ;Level 24 - Final Maze
        .db -1  ;Level 25 - Hidden Level
		.db -1	;Level 26 - Hidden Level
		.db -1	;Level 27 - Hidden Level
		.db -1 	;Level 28 - Hidden Level


___vbwrpnum = 0
___vbnumwrp = 7			;Allocate ROM space here, must match GAMMA def tho

#IF ___vbnumwrp != g_numwarps
	.error "The number of Warps defined in ALPHA does not match the warp definition in GAMMA."
#ENDIF 

;Macro builder for Warp Codes
#define  	DWARP(wrp_dig,wrp_lvl,wrp_col,wrp_liv,wrp_mes)	\ .org (wdigits+___vbwrpnum)
#defcont													\ .byte wrp_dig
#defcont													\ .org (wherel+___vbwrpnum)
#defcont													\ .byte wrp_lvl
#defcont													\ .org (warpcol+___vbwrpnum)
#defcont													\ .byte wrp_col
#defcont													\ .org (whatliv+___vbwrpnum)
#defcont													\ .byte wrp_liv
#defcont													\ .org (whatmes+___vbwrpnum)
#defcont													\ .byte wrp_mes
#defcont    												\___vbwrpnum .set ___vbwrpnum+1
#defcont    												\#if (___vbnumwrp < ___vbwrpnum) 
#defcont    												\ .error "MSG: Number of Warp Data definitions exceeds the defined limit!"
#defcont    												\#endif 

wdigits 	.block	___vbnumwrp
wherel		.block	___vbnumwrp
warpcol		.block	___vbnumwrp
whatliv		.block	___vbnumwrp
whatmes		.block	___vbnumwrp

		 ;Digits	;Target ;TxtColor		;Exlife	;Message
	DWARP(2,		04d,	$C0+colred2,	0,		mmwred)	;Red
	DWARP(2,		09d,	$C0+colyellow,	1,		mmwyel)	;Yellow
	DWARP(3,		10d,	$C0+colgreen,	1,		mmwgrn)	;Green
	DWARP(3,		13d,	$E0+colcyan,	1,		mmwaqu)	;Aqua
	DWARP(3,		17d,	$F0+colblue,	1,		mmwblue);Blue
	DWARP(3,		19d,	$E0+colpurple,	1,		mmwpurp);Purple
	DWARP(3,		21d,	$E0+colpink,	1,		mmwpink);Pink
	
; ;How many digits for each warp
; wdigits     .db 2   ;Red
            ; .db 2   ;Yellow
            ; .db 3   ;Green
            ; .db 3   ;Aqua
            ; .db 3   ;Blue
            ; .db 3   ;Purple
            ; .db 3   ;Pink
       

; ;These are 1 Based Level Numbers
; ?wherel     .db 04d     ;Red Warp
            ; .db 09d     ;Yellow Warp
            ; .db 10d     ;Green Warp
            ; .db 13d     ;Aqua Warp
            ; .db 17d     ;Blue Warp
            ; .db 19d     ;Purple
            ; .db 21d     ;Pink

; ;Color of numbers of warp
; warpcol     .db $80+colred2
            ; .db $C0+colyellow
            ; .db $C0+colgreen
            ; .db $F0+colcyan
            ; .db $F0+colblue
            ; .db $F0+colpurple
            ; .db $F0+colpink
        
; ;Extra Lives to Award for each warp level   
; ?whatliv    .db 0,1,1,1,1,1,1,1

; ;Which Message 
; ?whatmes    .db mmwred
            ; .db mmwyel
            ; .db mmwgrn
            ; .db mmwaqu           ;Warp message
            ; .db mmwblue
            ; .db mmwpurp
            ; .db mmwpink    

;******************************************************
;* Compare Warp Codes
;* If they match, will return $00 in A
;* X contains number of Digits
;******************************************************
?compcod 
        ldy #00
        begin
            lda perm2+1,Y
            cmp wrpdat,Y        ;Compare entered data to warp code
            bne ?cod10          ;Bad digit, no action
            iny
            dex
        eqend                   ;All digits good
        lda #00
?cod10  rts

;************************************************
    .sbttl "Long Range Scanner"
;************************************************
?lrsx = -$166

?lrscan lda	lroff
		cmp	#$A0
		ifcc
			inc	lroff
		endif
		
		jsr vgcntr
		lda difcty				;dont show this on tesster levels
		cmp #maxdif
		ifne
			lda tokretr
			ifpl
				lda # lbyte(?lrsx)
				sta vdata
				lda # ubyte(?lrsx)
				sta vdata+1
				lda dif4mz			;Offset table is based on maze level
				asl a				;words
				tay                 ;Input Place
				lda ?lrvctr,Y       ;copy base position to vdata
				sta vdata+2         ;store Y LSB position
				lda ?lrvctr+1,Y     ;copy base position to vdata
				sta vdata+3         ;store Y MSB position
				jsr vgvtr2          ;Position for draw
				
				lda #00
				sta vdata
				sta vdata+1
				sta vdata+3
				lda lroff
				lsr A
				lsr A
				sta vdata+2		
				jsr vgvtr2          ;Position for draw
				
				;draw ship 
				lda #$A0+colyellow  ;yellow
				ldx #tact_vpg
				jsr vgadd2          ;Color of box
				lda #$40
				ldx #$72            ;Size of box
				jsr vgadd2
				laljsr(lrsrbx)
				lxhjsr(lrsrbx)      ;Add box
				jsr vgadd2          ;Add picture				
			endif
		else
			VGADD_VCTRS(0,-87,hidden)
			lda #$B0+colflash  ;yellow
			ldx #tact_vpg
			jsr vgadd2          ;Color
			lda #$00
			ldx #$71            ;Size
			jsr vgadd2
			laljsr(qmark)
			lxhjsr(qmark)      	;Add qmark
			jsr vgadd2          ;Add picture	
		endif
		rts
        
?lrbase = (-248)
?pdist = 92
#DEFINE pdistance(lrbase,pdist,level) \ .dw (lrbase)+((((level)-1)/4)*(pdist))+((((level)-1)%4)*((pdist)/4)) 

?lrvctr 
		pdistance(?lrbase,?pdist,1)		;Level 1
		pdistance(?lrbase,?pdist,2)
		pdistance(?lrbase,?pdist,3)
		pdistance(?lrbase,?pdist,4)	
		pdistance(?lrbase,?pdist,5)		;Level 5
		pdistance(?lrbase,?pdist,6)	
		pdistance(?lrbase,?pdist,7)
		pdistance(?lrbase,?pdist,8)
		pdistance(?lrbase,?pdist,9)		;Level 9
		pdistance(?lrbase,?pdist,10)	
		pdistance(?lrbase,?pdist,11)
		pdistance(?lrbase,?pdist,12)
		pdistance(?lrbase,?pdist,13)	;Level 13
		pdistance(?lrbase,?pdist,14)
		pdistance(?lrbase,?pdist,15)
		pdistance(?lrbase,?pdist,16)	
		pdistance(?lrbase,?pdist,17)	;Level 17
		pdistance(?lrbase,?pdist,18)
		pdistance(?lrbase,?pdist,19)
		pdistance(?lrbase,?pdist,20)
		pdistance(?lrbase,?pdist,21)	;Level 21
		;technically these are never hit
		;pdistance(?lrbase,?pdist,22)	
		;pdistance(?lrbase,?pdist,23)	
		;pdistance(?lrbase,?pdist,24)	



?etypm   .db msphr,mfigh,mspin,mfort,mmainf

?condtbl .byte 0,0,0,0,2,2,2,2,2,4

        
;************************************************
    .sbttl "Heart Scanner"
;************************************************
;* Heart Scan Monitor                           *
;*                                              *
;* Inputs: X    = which (1 of 4) to display     *
;*         hscan= how many svecs to draw blank  *
;************************************************
?hscand lda frame
        and #03
        ifeq
            inc hscan
            inc hscan
        endif
        lda ?hlist,X
        sta temp2
        lda ?hlist+1,X
        sta temp2+1         ;Get input list of vectors
        ldy #00
        begin
            lda (temp2,Y)
            and #$1F                ;Turn off vector
            sta (vglist,Y)
            iny 
            lda (temp2,Y)
            cmp #$C0                ;An RTSL here?
            ifeq
                dey                 ;Back out the RTSL
                lda #00
                sta hscan               ;Start over
                beq ?hsc10              ;We hit end of list
            endif
            sta (vglist,Y)          ;Another short vector
            iny
            cpy hscan               ;Done enough?
        csend                   ;yep!
        ldx #06             ;Guess use 20
        lda hscan
        cmp #$0E                ;Use all?
        ifcc
            lsr A
            tax
        endif
        lda ?stint,X         ;Start Intensity
        sta temp3               ;Store intensity
        begin
            ldx #01
            begin
                lda (temp2,Y)
                ora temp3
                sta (vglist,Y)
                iny 
                lda (temp2,Y)
                cmp #$C0
                ifeq
                    dey                 ;Back up before this one    
                    jmp ?hsc10          ;Force done
                endif
                sta (vglist,Y)
                iny 
                dex                 ;Do 2 at each intensity
            miend
            lda temp3
            clc 
            adc #$20
            sta temp3               ;Continue until we wrap intensity
        csend                   ;We are done
?hsc10  dey 
        jsr vgadd
        ; FALL THROUGH
;*************************************************
	.sbttl "Breakout Ball Movement and Display"
;*************************************************
?balltimer = $80

?bdis   jsr vgcntr
        lda #00
        ldx #$72
        jsr vgadd2          ;Working scale
        bit bronce          ;Did you get a life here??
        ifmi                    ;yep!
            lda #($F0+colblue)
            ldx #mpic_vpg       
            jsr vgadd2
			lda breakx
			and #3
			asl A
			sta temp3
			tay
			lda ?whline+1,Y
			tax
			lda ?whline,Y
			jsr vgvtr5			
			;VGADD_VCTRS(-40,81,hidden)	;Position for rex
            lda #$70
            ldx #$71
            jsr vgadd2          ;Change scale
			;which pic to show
			ldy temp3
			lda ?whpic+1,Y
			tax
			lda ?whpic,Y
			jsr vgadd2
            ;VGADD_JSR(pic28)    ;Draw Rex   
			;extra pic for last index
			lda temp3
			cmp #(3<<1)
			ifeq
				VGADD_JSR(leg1)    ;Draw Rex  
			endif		
			ldy temp3
			lda ?whlin2+1,Y
			tax
			lda ?whlin2,Y
			jsr vgvtr5
			;VGADD_VCTRS(-42,-14,hidden)	;Position for WOW
            lda #$50
            ldx #$71
            jsr vgadd2          ;Scale for letters
            lda #($F0+colflash) ;Flash color
            ;bit perm5
            ;ifpl
            ;    lda #($F0+colgreen)	;Unless this happend in the distant past
            ;endif
            ldx #$60
            jsr vgadd2
            VGADD_JSR(char_w)   ;W
            VGADD_JSR(char_o)   ;O        
			VGADD_VCTRS(0,-1,hidden)	;Compensate for Funky W
            VGADD_JSR(char_w)   ;Last W
            lda #00
            ldx #$72
            jsr vgadd2          ;Restore Scale
            rts 
        endif                   ;So much for breakout
        lda #02
        sta temp3               ;3 rows
        begin
            jsr vgcntr
            ldy temp3
            lda #$41                ;x place for all 3 rows
            ldx ?yline,Y
            jsr vgvtr5
            lda #05                 ;5 bricks
            sta temp3+1
			lda bronce				;bronce masked
			and #03
			asl A
			asl A
			clc 
			adc temp3
            tax
            lda ?lincol,X
            ldx #brick_vpg         ;Color and stat and page
            jsr vgadd2
            ldx temp3
            lda brick,X         	;1 byte each brick
            sta temp2               ;On or Off flags
            begin
                laljsr(brickp)
                lxhjsr(brickp)          ;Guess will draw brick
                asl temp2               ;This one on?
                ifcc
                    laljsr(brline)
                    lxhjsr(brline)          ;Skip over this one
                endif
                jsr vgadd2
                dec temp3+1         ;Done all 6
            miend                   ;yes
            dec temp3               ;All 3 lines?
        miend     
		;* Fall through to ball Motion 
		;*************************************
			.sbttl "Ball Checks + Motion"
		;*************************************
		lda brstat          	;Ball Moving, yes if minus
        ifeq ;ifpl                	;Waiting to serve
			jsr getswitch
			ifpl            		;He pushed it
				lda #?balltimer
				sta balltime
				lda #0
				sta ballsout
				tax
				jsr ?startball
			endif
        endif
		;Loop through each ball
		ldx #nmballs-1
		begin
			stx temp3
			lda ballvx,X		;Skip ball if it has no velocity
			ora ballvy,X
			ifne
				jsr ?ballmvdr		;Move and draw active ball	
				jsr ?ballcoll		;Ball Collisions
			endif
			ldx temp3
			dex
		miend
		
		;Timer for launching another ball
		dec balltime
		lda balltime
		ifeq
			lda brstat
			ifmi
				and #$03
				sec
				sbc ballsout
				ifcs
					;we can do another ball now
					ldx ballsout		;this is actually the index of the next ball to start
					jsr ?startball
				endif
			endif
		endif

		;* Move and output paddle *
        lda rgdd
        cmp #$80
        ror A
        clc 
        adc paddle
        ifmi
            lda #00
        endif
        cmp #$70
        ifcs
            lda #$70
        endif
        bit brstat          ;Large paddle?
        ifvc                    ;yes
            cmp #$68
            ifcs
                lda #$68
            endif
        endif
        sta paddle          ;Save new paddle position
        clc 
        adc #04
        sta vdata
        lda #01
        sta vdata+1
        lda #$E0
        sta vdata+2
        lda #$FE
        sta vdata+3
        jsr vgcntr
        jsr vgvtr2          ;Position for paddle
        lda #$D2
        ldx #brick_vpg
        jsr vgadd2          ;Color and page select
        laljsr(padlep)      ;Paddle pic
        lxhjsr(padlep)      
        bit brstat          ;Check status
        ifvs
            laljsr(brickp)
            lxhjsr(brickp)          ;Small paddle
        endif
        jsr vgadd2          ;Draw paddle
        rts

?yline  .db -$20,-$1C,-$18            ;Position for Bricks	
?whpic	.dw ((((pic28)&$7fff)>>1)|$a000)	;leaning
		.dw ((((pic38)&$7fff)>>1)|$a000)	;teetering
		.dw ((((dab06)&$7fff)>>1)|$a000)	;dabbing
		.dw ((((pic27)&$7fff)>>1)|$a000)	;waiting 
;initial position line for rex
?whline .db 81,-40
		.db 81,-40
		.db 74,-40
		.db 78,-40
;position line for WOW
?whlin2	.db -14,-42
		.db -12,-42
		.db -10,-42
		.db -20,-20


;***************************************
; Start Ball in X
;***************************************
?startball
		inc ballsout
		lda #?balltimer
		sta balltime
		lda bronce
		and #03
		ora #$80
		sta brstat          ;Start play, ball is slow, two low bits signify how many balls can launch
		lda #-3
		sta ballvx,X
		sta ballvy,X        ;Start moving down
		;get random start
		lda rands
		and #$03
		tay
		lda ?ballxl,Y		;random angle to start ball
		;lda #$77
		sta ballxl,X
		lda #$B0
		sta ballyl,X
		lda #00
		sta ballxh,X
		sta ballyh,X   		;MSB's 0
		lda #snd_b2a
		jmp dosound
		;This is the end, my only friend the end

?ballxl	.db $77,$57,$37,$17
;?balvy 	.db -3,-3,-3

		
;****************************************
; Move and draw the breakout ball
; temp3 contains the current ball index
;****************************************
?ballmvdr 
		ldx temp3
		ldy #00         	;Guess + velocity
        lda ballvx,X         ;Velocity
        ifmi
            dey                 ;Prop sign
        endif
        clc 
        adc ballxl,X       	;Add to position LSB
        ;tax                ;Save LSB
		sta temp3+1			;Save LSB
        tya 
        adc ballxh,X       	;MSB
        ifmi                ;Don't let it go off left edge
            jsr ?chbx                ;Change Velocity
            lda #00
            ;tax                 ;Set ball at 0,0
			sta temp3+1			;Set ball at 0,0
        else    
			ldy temp3+1				;Not negative
            cpy #$78                ;Off right??
            ifcs                    ;yep
                jsr ?chbx
                lda #$77
				sta temp3+1
                lda #00             ;Set at right line
            endif
        endif
		sta ballxh,X
		lda temp3+1
        sta ballxl,X
        
        ldy #00             ;Guess + Velocity
        lda ballvy,X         ;Velocity
        ifmi
            dey                 ;Prop Sign
        endif
        clc 
        adc ballyl,X        ;Add to Position LSB
        sta temp3+1         ;Save LSB
        tya     
        adc ballyh,X       	;MSB
        ifmi       
			;Bottom edge passed
            lda ttime
            cmp #$0B
            ifcc
                lda #snd_b2d
                jsr dosound			;this destroys X
				ldx temp3			;get it back
            endif
            lda brstat
            ora #$80 ;+$20)            ;This rack is over, brstat = 80 will not allow launching any more balls even if avail.
            sta brstat
            lda #00
			sta ballxl,X			;Mark ball as done
			sta temp3+1
			lda #$FF				;get back YH to store
        else
			ldy temp3+1
            cpy #$F0            ;Off top?
            ifcs                ;yes
                lda #$C0
                ora brstat      ;Save old bits too
                sta brstat      ;Small Paddle
                jsr ?chby
                lda #$EF
				ldx temp3
                sta colflg,X    ;Allow another brick hit
                lda #00         ;Set at the top line
            endif
        endif
		sta ballyh,X
		lda temp3+1
        sta ballyl,X
        
		;Time to Draw it
		;Now Draw Ball
        ;lda brstat
        ;and #$20                ;Hold play?
        ;ifeq
		ldx temp3
		lda ballxl,X
		ifne
            jsr vgcntr       
			VGADD_VCTRS(-78,64,hidden)	;Position to bottom left corner
			ldx temp3				;get back X
			lda ballxl,X
			sta vdata
			lda ballxh,X
			sta vdata+1
			lda ballyl,X
			sta vdata+2
			lda ballyh,X
			sta vdata+3
            jsr vgvtr2      	;Position
            lda #($F0+colwhite) ;White ball
            ldx #brick_vpg
            jsr vgadd2
            VGADD_JSR(mapdot)
        endif   
		rts

;************************************
; Ball Collision
; temp3 is ball index
;************************************		
?bline1  	= $b0
?bline2  	= $c0
?bline3  	= $d0
?bline4  	= $e8
?paddline	= $10

?ballcoll
		;bricks first
		ldx temp3
        lda colflg,X          ;Brick collisions allowed??
        ifmi                    ;yes!
            ldy #00             ;Line 1 to start
            lda ballyl,X             ;Look to see if it is at a line
            cmp #?bline1
            ifcs                    ;At least at line 1
                cmp #?bline2         ;And not at line 2
                ifcc                    ;At line 1
?pdl10              sty temp3+1				;Save line index for next routine
					jsr ?chklin         	;Check for collision
                else                    ;Above line 2
                    iny                 ;Guess line 2 for now (X=1)
                    cmp #?bline3        ;Between line 2 and 3?
                    bcc ?pdl10          ;Check line
                    iny                 ;Guess line 3 now (X=2)
                    cmp #?bline4        ;At line 4 (Above line 3?)
                    bcc ?pdl10          ;Yep, check this line
                endif
            endif
        endif	
		;* Now check paddle *
		ldx temp3
        lda ballvy,X        ;Only if ball moving down
        ifmi
            lda ballyl,X
            cmp #?paddline           ;At paddle line?
            ifcs                    ;Above bottom
                cmp #?paddline+$0C
                ifcc
                    lda #$80
                    sta colflg,X        ;Set ok for another hit flag
                    jsr ?chkpdl
                endif
            endif
        endif
		rts

;*******************************		
;* Paddle Collision Routine 
;* X contains ball index     
;*******************************
?chkpdl lda ballxl,X
        sec 
        sbc paddle          ;See if left of left edge
        ifcs                    ;Right of left edge
            ldy #00             ;Guess small
            bit brstat          ;Large paddle?
            ifvc                ;It's large
                iny
            endif
            cmp ?middle,Y       ;Left of middle?
            ifcc                    ;Is left of middle
                lda ballvx,X        ;Want moving left here
                ifpl
?cpd10              jsr ?chbx                ;Get moving left
                endif
?cpd11          jsr ?chby
				jsr ?dopadhit
            else
                cmp ?redge,Y         ;Past right edge??
                ifcc                 ;yep
                    lda ballvx,X        ;Want moving right here
                    bmi ?cpd10          ;Turn it about
                    bpl ?cpd11          ;Else just change Y
                endif
            endif
        endif
        rts
            
?middle     .db $0A,$0F       ;Half Size
?redge      .db $14,$1E       ;Size

;***************************************
;* Check for Brick hit in a Line 
;* temp3 contains ball index
;* temp3+1 contains line index
;***************************************
?chklin  
		ldx temp3
        lda ballxl,X        ;Check ball X
		ldx temp3+1
		ldy #-1             ;Start at left, brick 0
        begin
            iny                 ;Next entry
            cmp ?brickx,Y       ;Look up in table
        ccend                   :Hit this brick (the last one actually!)
        ;* Y contains Brick Hit - See if there *
        sty temp1           ;Must have this if needed to turn off brick
        lda brick,X         ;Get brick status
        begin
            rol A               ;Brick status into carry
            dey
        miend
        ;* Carry is 1 if brick exists *
        ifcs
            lda brick,X
            ldy temp1           ;Recall Brick Index
            and ?brout,Y        ;Drop this bit
            sta brick,X
            cpx #02             ;Top Line?
            ifeq                    ;Speed things up!
				ldx temp3
                lda ballsp,X    	;See if already fast
                ifeq                    ;No not fast
                    inc ballsp,X		;faster now
                    asl ballvy,X        ;Heheheheheheheheheh
                    asl ballvx,X			
                endif
				ldx temp3+1			;get back line index
            endif
            lda $?linscr,X      ;Add in brick score
            jsr ?brscor
			ldx temp3
            lda #$80
            sta scoflg          ;Score change
            lda #00
            sta colflg,X        ;Don't allow another hit
            jsr ?chby           ;Turn the ball about
            lda #snd_b2b
            jsr dosound         ;Hit a brick
        endif
        rts

;******************************        
;* Change the ball X/Y velocity 
;* temp3 has ball index
;******************************
?chbx   ldx temp3
		lda ballvx,X
        jsr neg                 ;Change X velocity
        sta ballvx,X
        rts
            
?chby   ldx temp3
		lda ballvy,X
        jsr neg                 ;change Y velocity
        sta ballvy,X
        rts

;*********************************************
; Early Exit - Yes if all are true 
; Is this a warp level with a message?
; Are all balls 'inactive'=missed or not launched
; Can the player currently enter warp numbers?
;*********************************************
?eexit	lda perm1		;warp message being shown?
		ifmi			;nope
			lda wrpwh
			cmp perm2           ;All digits entered?
			bne ?bdfls			;branch when not all entered
			lda brstat			;initial ball launched?
			bpl ?bdfls			;branch for false
			;check launched balls
			lda brstat
			and #3
			tax
			begin
				lda ballyh,X 
				bpl ?bdfls			;ball Y positive = false
				dex
			miend
			sec				;TRUE, all tests passed
		else
?bdfls		clc			;false return
		endif
?bdexit rts
			
;******************************
; Score the ball!
;******************************		
?brscor ldy #00                 ;Guess Player 1
        ldx player              ;Who gets this score
        ifne
            ldy #score2-score
        endif
        sed 
        clc 
        adc score,Y
        sta score,Y
        lda score+1,Y
        adc #00
        sta score+1,Y
        lda score+2,Y
        adc #00
        sta score+2,Y
        lda score+3,Y
        adc #00
        sta score+3,Y
        cld 
        rts

?dopadhit
		lda #snd_b2c
		jsr dosound
		; ;Another paddle hit
		; inc colcnt
		; ;Update velocities based upon collision count
		; lda brstat
		; and #$10                ;Moving fast??
		; ifeq                    ;Not yet
			; lda colcnt          ;How many paddle hits??
			; cmp #05             ;Time for speed up?
			; ifeq
				; inc colcnt          ;Do it once only
				; ldx temp3
				; lda ballvx,X
				; ifmi
					; dec ballvx,X
				; else
					; inc ballvx,X
				; endif
			; endif
		; endif	
		;See if all bricks gone!
		lda brick
		ora brick+1
		ora brick+2         ;All Gone?
		ifeq                    ;yes
			sta ttime
			ldx player
			inc lives,X			;Extra Man!
			lda #$50            ;5000 points
			jsr bpont2          ;Bonus!!!!
			jsr dodelay         ;Thanks peter!
			lda #snd_brk
			jsr dosound         ;Make sound too!
			lda bronce
			ora #$80
			sta bronce
			inc breakx			;mark one more completed!
			;sta perm5               ;Only thing that perm5 is used for
		endif
		rts
				
;* Add a Life Here! *
?addlif ldx player
        lda slives
        clc 
        adc #01
        sta lives,X
        lda #snd_c4
        jmp dosound       

;* Message for normal Warp Hints *
?whathint    .byte mwarp0,mwarp1,mwarp2,mwarp3
             .byte mwarp4,mwarp5;,mwarp6

;* Name Display Color Table *
?alcol       .byte $8B,$C6,$CF,$C3,$F1

;* Tact Display Hint Table *
?tmesg      .byte mtmes0,mtmes1,mtmes2,mtmes3,mtmes4
?tmess      .byte mcgrn,mcyel,mcred,mccrit
?linscr     .byte 1,4,8

;Top Right Scanner Pictures
?dislist    .word tact0,tact1,tact2,tact3,tactv

;Heart Rate Scanner Patterns
?hlist      .word scan1,scan2,scan3,scan0

;* Start Intensity For Heart Scan Monitor *
?stint      .byte $E0,$C0,$A0,$80,$60,$40,$20

;* Heart Scan Box Color Selection *
?hsbox      .byte green,yellow,redr,redr
?brout      .byte $7F,$BF,$DF,$EF,$F7,$FB       ;Which bit to drop if brick hit
?brickx     .byte $13,$29,$3F,$55,$6B,$80       ;Brick X Left Edge

;Brick Colors	
?lincol     .byte ($D0+colblue),($D0+colgreen),($80+colred2),0
			.byte ($80+colred2),($D0+colorange),($D0+colblue),0
			.byte ($D0+colorange),($D0+colblue),($D0+colgreen),0
			.byte ($D0+colpink),($D0+colgreen),($D0+colorange),0

;Tact Scan Space Station pics 
cstations	jsrl(tactc0)
			jsrl(tactc1)
			jsrl(tactc2)
			jsrl(tactc3)
            jsrl(tactc4)
;***************************************************************
;* Some vector data here for copypic routines here in tw_tact
;* We don't need to do anything special with paging since
;* this data will always be called from disptact above
;***************************************************************

tact0		vctr(-32d,0d,hidden)
            vctr(9d,24d,visible)
            vctr(10d,8d,visible)
            vctr(25d,0d,visible)
            vctr(9d,-8d,visible)
            vctr(11d,-24d,visible)
            vctr(-16d,-20d,visible)
            vctr(-32d,0d,visible)
            vctr(-16d,20d,visible)
            vctr(64d,0d,visible)
            vctr(-16d,20d,visible)
            vctr(-32d,0d,visible)
            vctr(-16d,-20d,visible)
            vctr(16d,19d,hidden)
            vctr(3d,13d,visible)
            vctr(29d,-12d,hidden)
            vctr(-4d,12d,visible)
            vctr(-44d,40d,hidden)
            vctr(6d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-6d,-4d,visible)
            vctr(-8d,4d,visible)
            vctr(0d,8d,visible)
            vctr(8d,4d,visible)
            vctr(64d,0d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-8d,4d,visible)
            vctr(0d,8d,visible)
            vctr(8d,4d,visible)
            vctr(64d,-56d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-7d,3d,visible)
            vctr(0d,9d,visible)
            vctr(7d,4d,visible)
            vctr(-48d,-73d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-7d,4d,visible)
            vctr(0d,8d,visible)
            vctr(7d,4d,visible)
            vctr(-96d,0d,hidden)
            vctr(6d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-7d,4d,visible)
            vctr(0d,8d,visible)
            vctr(8d,4d,visible)
            vctr(-49d,73d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-9d,visible)
            vctr(-7d,-3d,visible)
            vctr(-8d,3d,visible)
            vctr(1d,9d,visible)
            vctr(7d,4d,visible)
            vctr(97d,-16d,hidden)
            rtsl
         
			
tact1		vctr(24d,6d,visible)
            vctr(10d,-6d,visible)
            vctr(52d,14d,visible)
            vctr(0d,50d,visible)
            vctr(-20d,18d,visible)
            vctr(-42d,8d,visible)
            vctr(-48d,0d,visible)
            vctr(-42d,-8d,visible)
            vctr(-20d,-18d,visible)
            vctr(0d,-50d,visible)
            vctr(52d,-14d,visible)
            vctr(10d,6d,visible)
            vctr(24d,-6d,visible)
            vctr(0d,42d,visible)
            vctr(86d,22d,visible)
            vctr(-54d,18d,visible)
            vctr(-32d,-8d,visible)
            vctr(-32d,8d,visible)
            vctr(-54d,-18d,visible)
            vctr(86d,-22d,visible)
            vctr(0d,20d,hidden)
            vctr(18d,4d,visible)
            vctr(6d,8d,visible)
            vctr(-24d,-6d,visible)
            vctr(-24d,6d,visible)
            vctr(6d,-8d,visible)
            vctr(18d,-4d,visible)
            vctr(32d,20d,hidden)
            vctr(-8d,8d,visible)
            vctr(-48d,0d,hidden)
            vctr(-8d,-8d,visible)
            vctr(16d,-104d,hidden)
            vctr(-32d,0d,visible)
            vctr(14d,-12d,visible)
            vctr(18d,12d,visible)
            vctr(32d,0d,hidden)
            vctr(32d,0d,visible)
            vctr(-14d,-12d,visible)
            vctr(-18d,12d,visible)
            vctr(0d,-32d,hidden)
            vctr(-32d,0d,visible)
            vctr(14d,-12d,visible)
            vctr(18d,12d,visible)
            vctr(-16d,54d,hidden)
            rtsl
			
tact2		vctr(-42d,0d,hidden)
            vctr(-6d,-32d,visible)
            vctr(-64d,0d,visible)
            vctr(80d,144d,visible)
            vctr(18d,0d,visible)
            vctr(-10d,-40d,visible)
            vctr(42d,0d,visible)
            vctr(6d,-32d,visible)
            vctr(-24d,0d,visible)
            vctr(0d,32d,visible)
            vctr(30d,0d,hidden)
            vctr(16d,0d,visible)
            vctr(12d,-32d,visible)
            vctr(-18d,0d,visible)
            vctr(-10d,32d,visible)
            vctr(-6d,14d,hidden)
            vctr(-24d,0d,visible)
            vctr(0d,26d,visible)
            vctr(32d,0d,visible)
            vctr(80d,-144d,visible)
            vctr(-128d,0d,visible)
            vctr(4d,26d,hidden)
            vctr(2d,28d,visible)
            vctr(-26d,0d,visible)
            vctr(4d,18d,visible)
            vctr(-40d,0d,visible)
            vctr(84d,-46d,hidden)
            vctr(-2d,28d,visible)
            vctr(72d,0d,visible)
            vctr(-24d,-10d,hidden)
            vctr(16d,-44d,visible)
            vctr(-36d,132d,hidden)
            vctr(-24d,0d,visible)
            vctr(-14d,-100d,hidden)
            rtsl

tact3	vctr(-57d,83d,hidden)
		vctr(38d,6d,visible)
		vctr(38d,0d,visible)
		vctr(38d,-6d,visible)
		vctr(38d,-19d,visible)
		vctr(0d,-22d,visible)
		vctr(-38d,-28d,visible)
		vctr(-32d,-12d,visible)
		vctr(-51d,0d,visible)
		vctr(-32d,12d,visible)
		vctr(-38d,28d,visible)
		vctr(0d,22d,visible)
		vctr(38d,19d,visible)
		vctr(-38d,-32d,hidden)
		vctr(51d,-25d,visible)
		vctr(89d,0d,visible)
		vctr(51d,25d,visible)
		vctr(-153d,-38d,hidden)
		vctr(0d,-19d,visible)
		vctr(0d,-6d,visible)
		vctr(32d,-12d,visible)
		vctr(51d,0d,visible)
		vctr(32d,12d,visible)
		vctr(0d,6d,visible)
		vctr(0d,19d,visible)
		vctr(0d,-19d,hidden)
		vctr(38d,-19d,visible)
		vctr(0d,-22d,visible)
		vctr(-38d,-28d,visible)
		vctr(-32d,-12d,visible)
		vctr(-51d,0d,visible)
		vctr(-32d,12d,visible)
		vctr(-38d,28d,visible)
		vctr(0d,22d,visible)
		vctr(38d,19d,visible)
		vctr(-38d,-32d,hidden)
		vctr(51d,-25d,visible)
		vctr(89d,0d,visible)
		vctr(51d,25d,visible)
		rtsl

;This was the old/original version of tact3 which looked like the star castle (incorrect)
; tact3		vctr(-32d,-22d,hidden)
            ; vctr(-21d,24d,visible)
            ; vctr(0d,30d,visible)
            ; vctr(30d,18d,visible)
            ; vctr(45d,0d,visible)
            ; vctr(30d,-18d,visible)
            ; vctr(0d,-30d,visible)
            ; vctr(-20d,-24d,visible)
            ; vctr(-64d,0d,visible)
            ; vctr(16d,-4d,visible)
            ; vctr(32d,0d,visible)
            ; vctr(16d,4d,visible)
            ; vctr(0d,34d,visible)
            ; vctr(20d,20d,visible)
            ; vctr(-30d,12d,visible)
            ; vctr(-6d,-6d,visible)
            ; vctr(16d,-26d,visible)
            ; vctr(-65d,0d,visible)
            ; vctr(1d,-34d,visible)
            ; vctr(17d,72d,hidden)
            ; vctr(-8d,-7d,visible)
            ; vctr(-30d,-11d,visible)
            ; vctr(20d,-20d,visible)
            ; vctr(17d,26d,visible)
            ; vctr(-7d,5d,visible)
            ; vctr(6d,-5d,hidden)
            ; vctr(33d,0d,visible)
            ; vctr(6d,6d,hidden)
            ; vctr(-8d,6d,visible)
            ; vctr(-14d,-51d,hidden)
            ; vctr(4d,-5d,visible)
            ; vctr(-4d,-3d,visible)
            ; vctr(-4d,4d,visible)
            ; vctr(4d,4d,visible)
            ; vctr(0d,1d,hidden)
            ; rtsl
			
tactv  
	
		vctr(79d, 43d, hidden)
		vctr(-9d, -92d, visible)
		vctr(-82d, -41d, visible)
		vctr(-3d, 101d, visible)
		vctr(95d, 32d, visible)
		vctr(-68d, 30d, visible)
		vctr(-86d, -21d, visible)
		vctr(59d, -40d, hidden)
		vctr(-59d, 40d, visible)
		vctr(10d, -88d, visible)
		vctr(52d, -53d, visible)
		;vctr(90d, 120d, hidden)
		;vctr(-93d, -33d, visible)
		;vctr(50d, 18d, hidden)
		;vctr(-3d, -82d, visible)
		rtsl
		
		vctr(40d, 90d, hidden)
		vctr(0d, -9d, visible)
		vctr(-4d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-5d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-5d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-5d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-5d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-13d, 4d, hidden)
		vctr(0d, -9d, visible)
		vctr(-6d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-6d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-6d, 7d, hidden)
		vctr(0d, -9d, visible)
		vctr(-7d, 7d, hidden)
		vctr(0d, -10d, visible)
		vctr(-7d, 7d, hidden)
		vctr(0d, -10d, visible)
		vctr(-9d, 13d, hidden)
		vctr(-58d, 42d, visible)
		vctr(26d, -19d, hidden)
		vctr(6d, -81d, visible)
		vctr(21d, 54d, hidden)
		vctr(0d, -10d, visible)
		vctr(-5d, 13d, hidden)
		vctr(0d, -9d, visible)
		vctr(-5d, 13d, hidden)
		vctr(0d, -9d, visible)
		vctr(-5d, 13d, hidden)
		vctr(0d, -9d, visible)
		vctr(-4d, 13d, hidden)
		vctr(0d, -9d, visible)
		vctr(-4d, 12d, hidden)
		vctr(0d, -9d, visible)
		vctr(-9d, 16d, hidden)
		vctr(0d, -9d, visible)
		vctr(-4d, 12d, hidden)
		vctr(0d, -9d, visible)
		vctr(-4d, 12d, hidden)
		vctr(0d, -9d, visible)
		vctr(-4d, 11d, hidden)
		vctr(0d, -9d, visible)
		vctr(-4d, 11d, hidden)
		vctr(0d, -8d, visible)
		vctr(-4d, 11d, hidden)
		vctr(0d, -8d, visible)
		rtsl





scan0		vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            rtsl
			
scan1		vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            rtsl
			
scan2		vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            rtsl
			
scan3		vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            rtsl

