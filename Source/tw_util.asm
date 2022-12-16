
;***********************************************
    .TEXT ".TWUTIL."         
;***********************************************
    .title "Utility & Math Routines"
	.sbttl "Cosine Routine"
;* cos(a)=sin(A+PI/2)                    *
;*****************************************
cos		clc	
		adc	#$40		;Fall through to Sin now
		
;******************************************
	.sbttl "Sine Routine"
;******************************************
;* (A)=Angle (0 to FF represents 0 to 360 *
;* (CC+=Minus Plus Flag Set Correctly     *
;*                                        *
;* Exit: (A)=sine (-127 to +127)          *
;*                                        *
;* Uses: (A),(X)                          *
;******************************************
sin		ifmi				;if pi>(A)>0
			and	#$7F
			jsr	sin1			;sin(a) when pi>a>0
			jmp	neg
		endif
		
sin1	cmp	#$41
		ifcs				;pi/2>a>0
			eor	#$7F			;sin(pi/2+a)=sin(pi/2-a)
			adc	#00
		endif
		tax	
		lda	sincos,X
		rts
			
sincos	.byte 000,003,006,009,012,016,019,022
		.byte 025,028,031,034,037,040,043,046
		.byte 049,051,054,057,060,063,065,068
		.byte 071,073,076,078,081,083,085,088
		.byte 090,092,094,096,098,100,102,104
		.byte 106,107,109,111,112,113,115,116
		.byte 117,118,120,121,122,122,123,124
		.byte 125,125,126,126,126,127,127,127
		.byte 127
		
;**********************************************
	.sbttl "Multiply"
;**********************************************
;* Multiply - Unsigned. Need to conditionally 
;* fix the result for the signed operations.  
;* Uses Nibble method from 4 sums.            
;* (A4*B4*100)+((AH*BL+BL*AH)*10)+(AL*BL)     
;*                                            
;* Input	(A)=signed                        
;*          (temp1)=unsigned (preserved) (=B) 
;*                                            
;* Output	(temp2,temp2+1), High byte in A   
;*                                            
;* Uses	X,Y,A,temp4,temp2                 
;**********************************************
multiply	
        tax	
		php			;Reset Status
		;
		;Form AH*BH*100
		;
		sta	temp4
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		eor	temp1
		and	#$0F
		eor	temp1
		tax	
		lda	multbl,X
		sta	temp2+1
		;
		;Form AL*BL
		;
		lda	temp4
		asl	a
		asl	a
		asl	a
		asl	a
		eor	temp1
		and	#$F0
		eor	temp1
		tax	
		lda	multbl,X
		sta	temp2
		;
		;Form AL*B4 Index and save in Y
		;
		lda	temp4
		eor	temp1
		and	#$0F
		eor	temp1
		tay	
		;
		;Form AH*BL, Save in X
		;
		lda	temp4
		eor	temp1
		and	#$F0
		eor	temp1
		tax	
		lda	multbl,X
		clc	
		adc	multbl,Y
		tax	
		ror	a
		lsr	a
		lsr	a
		lsr	a
		sta	temp4
		txa	
		asl	a
		asl	a
		asl	a
		asl	a
		clc	
		adc	temp2
		sta	temp2
		lda	temp2+1
		adc	temp4
		plp		;If +, ok as it is.	
		ifmi		;If -, subtract the unsigned value from the high byte of results
			sec
			sbc	temp1
		endif
		sta	temp2+1
		rts

;*********************************************
    .sbttl "Math Routines Used All Over"         
;*********************************************
;*                                           *
;* Complement                                *
;*                                           *
;* Entry neg2: Special case for 'targship    *
;*             in module 'twship'            *
;* Entry neg : Complement                    *
;*                                           *
;* Inputs: (A) = Input                       *
;*         (PS)= Set +/- Correctly for (A)   *
;* Exit:   (A) = Output                      *
;*                                           *
;*********************************************
neg2    ifeq
            iny         ;Return FF to 0 for 0
        endif
neg     eor #$FF
        clc
        adc #01
        rts

;*********************************************
    .sbttl "Count bits"      
;*********************************************             
;* Counts the number if bits set in A
;* returns count in A, destroys X                 
;********************************************* 
howmbits
		ldx #0                  ; Clear bit count
		begin
			asl A                     ; Shift a bit
			ifcs		        ; Did a one shift out?
				inx                     ; Add one to count
				ora #0                  ; Retest for zero
			endif
        eqend
        txa                     ; Move count to A
        rts
;*********************************************
    .sbttl "Double Negate"         
;*********************************************
;* Will Negate the 2 byte value stored in    *
;* temp1 and temp1+1                         *
;*********************************************  
dblneg  lda #00
        sec 
        sbc temp1
        sta temp1
        lda #00
        sbc temp1+1
        sta temp1+1     ;Want a positive integer
        rts 
        
;***********************************************
    .sbttl "Divide by 2*x"         
;***********************************************
;* Does and arithmetic divide by 2 for x times *
;*                                             *
;* CAUTION: Enter with 0 will do 256 times!!   *
;***********************************************    
div2x   begin
            cmp #$80
            ror A
            dex
        eqend
        rts
		
;*********************************************
	.sbttl "Full Log Table to Convert Ship Sizes"	
;*********************************************
fullog	.byte $7F,$7F,$7E,$7D,$7C,$7C,$7B,$7A,$7A,$79,$78,$77,$77,$76,$75,$74
		.byte $74,$73,$72,$71,$71,$70,$6F,$6E,$6D,$6D,$6C,$6B,$6A,$69,$69,$68
		.byte $67,$66,$65,$64,$64,$63,$62,$61,$60,$5F,$5E,$5E,$5D,$5C,$5B,$5A
		.byte $59,$58,$57,$56,$55,$55,$54,$53,$52,$51,$50,$4F,$4E,$4D,$4C,$4B
fullog2	.byte $4A,$49,$48,$47,$46,$45,$44,$43,$42,$41,$40,$3F,$3E,$3D,$3C,$3B
		.byte $3A,$38,$37,$36,$35,$34,$33,$32,$31,$30,$2E,$2D,$2C,$2B,$2A,$29
		.byte $28,$26,$25,$24,$23,$22,$20,$1F,$1E,$1D,$1C,$1A,$19,$18,$17,$15
		.byte $14,$13,$11,$10,$0F,$0D,$0C,$0B,$0A,$08,$07,$05,$04,$03,$01,$00
		
;*****************************************************
	.sbttl "Quarter Log Table to Draw Unfinish Web Lines"	
;*****************************************************
qrtlog	.byte $FD,$F9,$F6,$F2,$EF,$EC,$E8,$E5,$E1,$DE,$DA,$D7,$D3,$CF,$CC,$C8
		.byte $C4,$C1,$BD,$B9,$B6,$B2,$AE,$AA,$A6,$A3,$9F,$9B,$97,$93,$8F,$8B
		.byte $87,$83,$7F,$7B,$77,$73,$6F,$6B,$66,$62,$5E,$5A,$55,$51,$4D,$49
		.byte $44,$40,$3B,$37,$33,$2E,$2A,$25,$21,$1C,$17,$13,$0E,$09,$05,$00
		
;*****************************************************
	.sbttl "Multiply Table"	
;*****************************************************		
multbl	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
		.byte $00,$02,$04,$06,$08,$0A,$0C,$0E,$10,$12,$14,$16,$18,$1A,$1C,$1E
		.byte $00,$03,$06,$09,$0C,$0F,$12,$15,$18,$1B,$1E,$21,$24,$27,$2A,$2D
		.byte $00,$04,$08,$0C,$10,$14,$18,$1C,$20,$24,$28,$2C,$30,$34,$38,$3C
		.byte $00,$05,$0A,$0F,$14,$19,$1E,$23,$28,$2D,$32,$37,$3C,$41,$46,$4B
		.byte $00,$06,$0C,$12,$18,$1E,$24,$2A,$30,$36,$3C,$42,$48,$4E,$54,$5A
		.byte $00,$07,$0E,$15,$1C,$23,$2A,$31,$38,$3F,$46,$4D,$54,$5B,$62,$69
		.byte $00,$08,$10,$18,$20,$28,$30,$38,$40,$48,$50,$58,$60,$68,$70,$78
		.byte $00,$09,$12,$1B,$24,$2D,$36,$3F,$48,$51,$5A,$63,$6C,$75,$7E,$87
		.byte $00,$0A,$14,$1E,$28,$32,$3C,$46,$50,$5A,$64,$6E,$78,$82,$8C,$96
		.byte $00,$0B,$16,$21,$2C,$37,$42,$4D,$58,$63,$6E,$79,$84,$8F,$9A,$A5
		.byte $00,$0C,$18,$24,$30,$3C,$48,$54,$60,$6C,$78,$84,$90,$9C,$A8,$B4
		.byte $00,$0D,$1A,$27,$34,$41,$4E,$5B,$68,$75,$82,$8F,$9C,$A9,$B6,$C3
		.byte $00,$0E,$1C,$2A,$38,$46,$54,$62,$70,$7E,$8C,$9A,$A8,$B6,$C4,$D2
		.byte $00,$0F,$1E,$2D,$3C,$4B,$5A,$69,$78,$87,$96,$A5,$B4,$C3,$D2,$E1

        
;***********************************************
    .sbttl "Set ROM Page Routines"         
;***********************************************        
stpg0   lda #00
        beq ?stpg
stpg1   lda #01
        bne ?stpg
stpg2   lda #02
        bne ?stpg
stpg3   lda #03
        bne ?stpg
stpg4   lda #04
        bne ?stpg
stpg5   lda #05
        bne ?stpg
stpg6   lda #06
        bne ?stpg
stpg7   lda #07
?stpg   sta rompg
        ;pha
        ;lda currpage
        ;sta lastpage
        ;pla
        ;sta currpage
        rts
        
;*****************************************************
	.sbttl "Score Routines"	
;*****************************************************
;* Add Points Passed in to a buffer that will be     *
;* 'scrolled' into the total score                   *
;*                                                   *
;* Entry: (A)= LSB Amount, (X)=MSB Amount            *
;*****************************************************
bpont2	ldx	#00			;For places where X=0
bpoint	sta	scbdis+1
		stx	scbdis+2    ;Will Display this amount
bpont3	bit	gamest		;Enter here to skip display of this amount
		ifmi
			sed
			clc
			adc	scrbuf
			sta	scrbuf
			txa					;Add MSB
			adc	scrbuf+1
			sta	scrbuf+1
			cld
		endif
		lda	#$0F			;3 1/2 Seconds Approx
		sta	scbdis+3		;Display Time
		rts

;*****************************************************
	.sbttl "Add Points to Score"	
;*****************************************************
;* Add Points to players score                       
;*                                                   
;* Entry: (scrbuf(2))= hundreds to add               
;*****************************************************		
points	txa	
		pha	
		tya	
		pha					;Save X
		ldx	#00				;Guess Player 0
		stx	temp5			;0 Amount
		lda	player		
		ifne
			ldx	#score2-score	;Point to Player 2 Score
		endif
		lda	scrbuf+1		;Alot of Points
		ifne
			inc	temp5
		endif
		lda	scrbuf
		ifne
			inc	temp5
		endif
		lda	temp5			;Any Score??
		ifne
			dec	scoflg		;X, score change
			sed				;**** decimal mode ****
			clc
			adc	score+1,X
			sta	score+1,X		;Add in Thousands
			ifcs				;A Carry!
				lda	#00
				adc	score+2,X
				sta	score+2,X
				php				;Save any Carry
				ldy	nxtbonus    ;0 is no free lives
				ifne
					cpy	#01
					ifeq
						and	#$0F		;1 is one every 50K
						beq	?aps10
						cmp	#05
						beq	?aps10
					endif
					cpy	#02
					ifeq
						and	#$0F
						beq	?aps10
					endif
					cpy	#03
					ifeq
						and	#$1F
						ifeq
?aps10						cld				;Skip Decimal for a bit
							txa
							pha				;Save X
							ldx	player
							inc	lives,X		;Another Life
							lda	#snd_c4
							jsr	dosound		;Extra Life sound
							sed				;Reset Decimal
							lda	st_extlie
							clc
							adc	#01
							sta	st_extlie
							lda	st_extlie+1
							adc	#00
							ifcc
								sta	st_extlie+1
							endif
							pla
							tax				;Restore Index
						endif
					endif
				endif
				plp				;Restore any carry
				lda	#00
				adc	score+3,X
				sta	score+3,X
			endif
			lda	scrbuf
			sec
			sbc	temp5			;How Much we added
			sta	scrbuf
			lda	scrbuf+1
			sbc	#00
			sta	scrbuf+1
		endif
		cld					;Clear Decimal
		ldx	scbdis+3		;Any Display time??
		ifne
			lda	frame
			and	#03
			ifeq
				dec	scbdis+3
			endif
			txa	
			asl	a
			asl	a
			asl	a
			asl	a
			ora	#colpurple		;purple to dim out
			ldx	#$60
			jsr	vgadd2			;Add Stat
			lda	#00
			ldx	#$72
			jsr	vgadd2			;Scale
			jsr	vgcntr
			lda	#-$58			;Guess Player 1
			ldx	player
			ifne
				lda	#$4C			;Nope, on other side (player 2)
			endif
			ldx	#$60
			jsr	vgvtr5			;Position
			lda	#00
			sta	scbdis			;Fake the 2 extra 0's
			lda	#scbdis
			sec
			ldy	#03
			jsr	digits			;Display this
		endif
		pla
		tay
		pla	
		tax	
		rts
		
;***********************************************
	.sbttl "Award Bonus Points"
;***********************************************
award   bit atflag          ;Never show bonusin Attract mode
        ifpl
            lda bonusa
            ifne                ;Skip if 0
                lda shipst      ;Skip if Dying or Dead
                ifne
                    ifpl    
                        lda mzgame
                        and #$1C            ;Only during these times
                        ifne
                            lda bonsnd      ;Bonus Sound Started??
                            ifeq
                                lda #$snd_c7        ;Do Bonus Sound
                                sta bonsnd
                                jsr dosound
                            endif
                            lda frame
                            and #03         ;Add in slowly
                            ifeq
                                sed             ;********** Caution **********
                                lda bonusa
                                sec
                                sbc #01
                                sta bonusa      ;Add another 100
                                cld             ;*****************************
                                ifeq                ;Down to 0??
                                    sta bonsnd
                                    lda #snd_c6     ;Stop Bonus Sound
                                    jsr dosound
                                endif
                                lda #01
                                ldx #00
                                jsr bpont3			;Score but don't show it underneath
                            endif
                        endif
                        lda mzgame      ;Skip message only in tact scan
                        and #$60            ;In Tact??
                        ifeq                ;not tact or tube
                            ldx #mbonus
                            jsr mesg          ;Bonus message
                            lda #00
                            sta temp1           ;LSB fake 0
                            lda bonusa
                            sta temp1+1
                            ldy #02
                            lda #temp1
                            sec
                            jsr digits
                        endif
                    endif
                endif
            endif
        endif
		rts

;*****************************************************
	.sbttl "Sdigits - Display Single Digits"	
;*****************************************************
;* Display Y digit numbers                          
;* Input	(C) = carry set for zero supression      
;*          (A) = base address of zero page digits          
;*          (Y) = number of single digits to display 
;*                                                   
;* Uses	A,X,Y,temp5                              
;*****************************************************	
sdigits	dey	
        sty temp5+1     ;Number of digits
		sta	temp5		;LSB of Digits
		begin
            ldx temp5
			lda	$0000,X
			jsr	vghexz	;Odd Digit
            dec temp5+1
            ifpl
                ldx	temp5
                lda	$0000,X
                lsr	A
                lsr	A
                lsr	A
                lsr	A
                jsr	vghexz	;Even Digit
                inc	temp5
                dec temp5+1
            endif 
		miend
		rts
        
;*****************************************************
	.sbttl "Digits - Display 2 Digits"	
;*****************************************************
;* Display 2Y digit numbers                          
;* Input	(C) = carry set for zero supression      
;*          (A) = address of (Y) zero page   
;*          (Y) = number of digit to show
;*                (2-digits per byte)  
;*                                                   
;* Uses	A,X,Y,temp5                              
;*****************************************************	
; temp5+1 = digit counter
; temp5   = current byte being worked on
;*****************************************************
digits	php				;Save input parameters
		dey	
		sty	temp5+1		;Y is now zero based digits to show (0 = 1 digit)
		clc	
		adc	temp5+1		
		sta	temp5		;MSB of Digits
		plp	
		tax				;x is current pointer
		begin
			php	
			lda	0000,X	
			lsr	A
			lsr	A
			lsr	A
			lsr	A
			plp	
			jsr	vghexz		;First Digit, if carry is set here, then this will be a 'blank' if zero
			lda	temp5+1
			ifeq
				clc			;Display Last Digit Even if 0
			endif
			ldx	temp5
			lda	0000,X
			jsr	vghexz		;Second Digit
			dec	temp5
			ldx	temp5
			dec	temp5+1		;One less on digit counter
		miend
		rts
		
;*****************************************************
	.sbttl "Decimal Conversion"	
;*****************************************************
;* Hex to BCD decimal conversion                     
;* Input:	A                                        
;* Output:	temp7    = MSB                           
;*          temp7+1  = LSB                           
;* Uses: 	A,Y,temp5                                
;*****************************************************			
decimal	sta	tempa		;Save this
		ldy	#07		;Bit Count
		lda	#00
		sta	temp7		;Clear Results
		sta	temp7+1
		sed			;***** Warning *****
		begin
			asl	tempa
			lda	temp7
			adc	temp7
			sta	temp7
			lda	temp7+1
			adc	temp7+1
			sta	temp7+1		;An 'inc' to Decimal
			dey
		miend
		cld			;***** Okay Now *****
		rts
		
;******************************************************
;* JSRL to Vector Character
;******************************************************
;* Entry:   Y = word index of character (0=blank.... )
;* Uses:    A,X,Y                                
;******************************************************  
;vgchar  ldx vgmsga+1,Y
;        lda vgmsga,Y
;        jmp vgadd2          ;Add JSRL to this char
        
;******************************************************
;* Copy Memory Block from One address to another
;* 
;*  temp2 - Source Address
;*  temp3 - Destination Address
;*  Y     - Length 
;******************************************************
copymem
    dey
    begin
        lda (temp2,Y)
        sta (temp3,Y)
        dey
        cpy #-1
    eqend
    rts

;******************************************************
;* Reset Frame counter
;******************************************************
resframe
    lda #0
    sta frame
    sta frame+1
    rts
