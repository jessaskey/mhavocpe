;*****************************************************
    .TEXT ".TWCOIN."
;*****************************************************
    .title "TWCoin"
    .sbttl "Coin Routine"   
;*****************************************************
;* Coin65 - 650X Universal Coin Routine              *
;*                                                   *
;* Programmers: Downend & Albaugh                    *
;* Revised for Havoc 10-27-83                        *
;* Removed all macro references 6-13-00              *
;*****************************************************
    
;include     = 1
;bonadd      = 1
;coin67      = 1 ;High 3 bits
;slam       = 0 ;Assembled Low
;cntint     = 0 ;ill count interrupts myself
;gama       = 1 ;Tell my coin routine gamma is here
    

moolah  ldx #02         ;X is used to index from right to left coin mech
		begin
			lda inputs      ;Get Coin Switches
			cpx #01         ;Which mech are we doing
			ifne            ;Middle Shift Twice
				bcs ?c12        ;Right Shift Once
				asl a           ;Else left, shift Thrice
			endif
			asl a
?c12	    asl a
			lda c_cnstt,x
			and #$1F        ;Shared Inst
			bcs ?c5         ;Branch if input high (Coin Absent)
			ifne            ;Stick at 0(Terminal Count)
				cmp #$1B        ;In First 5 Samples
				bcs ?c10        ;Yes, run fast
				tay             ;Else, save status
				lda c_intct     ;Check Interrupt Counter
				and #07         ;Are D0-D2 all 1's??
				cmp #07         ;Set Carry if so
				tya             ;Status back into A 
				ifcs            ;Skip if not all 1's
?c10	            sbc #01         ;Carry Set
				endif
			endif
?c1			sta c_cnstt,x   ;Save updated Status
			lda inputs      ;Check Slam Switch
			and #$10        
			ifeq            ;Branch if bit hi (switch off)
				lda #$F0        ;Else set pre-coin slam timer
				sta c_lmtim     ;Dec 8 times/frame = prst frames
			endif
			lda c_lmtim     ;Check pre-coin slam timer  
			ifne            ;O.K.
				dec c_lmtim     ;else run timer
				lda #00     
				sta c_cnstt,x   ;Clear Coin Status
				sta c_pstsl,x   ;Clear Post-Coin Slam Timer
			endif
			clc             ;Default 'no coin detected'     
			lda c_pstsl,x   ;check post-coin slam timer
			ifne            ;empty, proceed
				dec c_pstsl,x       ;run timer
				ifeq                ;not done, prceed
					sec                 ;when it becomes zero, indicate a coin
					ifcc                ;(always)
?c5		                cmp #$1B            ;Is coin valid yet?? (On for >4 samples)
						ifcc                ;no,reset it
							lda c_cnstt,x       ;Get status again
							adc #$20            ;Bump coin-off up counter
							bcc ?c1             ;If it didn't wrap, just store status
							ifne             ;It wrapped but coin was on too long, just reset
								clc                 ;Set validity again
							endif
						endif
						lda #$1F            ;Reset Down Counter
						bcs ?c1             ;Branch if coin too long or too short
						sta c_cnstt,x       ;Save reset status
						lda c_pstsl,x       ;Check Howie's assumption
						ifne                ;Branch if $pstsl vacant
							sec                 ;Else give credit a little early    
						endif
						lda #$78            ;Post Frames
						sta c_pstsl,x       ;Delay acceptance for post/60 sec.
					endif
				endif
			endif
			ifcs
				;*****************************
				.sbttl "Mech-Multipliers"
				;*****************************
				lda #00         ;Start with 0
				; cpx #01         ;Check which mech
				; ifcs            ;If left, always add 1
					; ifne        ;If center, check half mult
						; lda c_cmode  
						; and #$0C
						; lsr A
						; lsr A
						; beq ?c85        ;00- Add 1
						; adc #02         ;Else map 1,2,3 to 3,4,5
						; bne ?c85        ;(always)
					; endif
					; lda c_cmode     ;Get Coin Mode from Zero Page
					; and #$10        ;Isolate half multiplier
					; ifne
						; lda #01
					; endif
				; endif
	;?c85        
				;sec 
				;pha 
				;adc c_bccnt     ;Update Bonus Adder Counter
				;sta c_bccnt
				;pla 
				sec 
				adc c_cnct
				sta c_cnct
				inc cntrs,X     ;Shadow Counter
			endif
			dex 
        miend
        ;;jmp _detct

;********************************************      
    .sbttl "Bonus Adder"
;********************************************
; _bonus  lda c_cmode
        ; lsr A
        ; lsr A
        ; lsr A
        ; lsr A
        ; lsr A           ;Isolate Bonus-Adder mode in bits 0-2
        ; tay 
        ; lda c_bccnt
        ; sec 
        ; sbc _modlo,Y    ;See if enough unit-coins have accumulated
        ; ifpl      ;Branch if not
            ; sta c_bccnt     ;Else update Bonus Adder and...
            ; inc c_bonusc    ;Give 1 or 2 Bonus Coins
            ; cpy #03     
            ; ifeq 
                ; inc c_bonusc    ;Mode 3 yields 2 bonus coins for 4 inserted
            ; endif
        ; endif
   
;********************************************
.sbttl "Convert Coins to Credits"
;********************************************
; 00 = Free Play
; 01 = 1 Coin 2 Plays
; 02 = 1 Coin 1 Play
; 03 = 2 Coin 1 Play
_cnvrt  lda c_cmode     ;Get Coin Mode
        ;and #03         ;Isolate Coins-Credits Option
        tay 
        ifne            ;If free play, cmode=0, do nothing
            lsr A           ;Else for price (0,1,1,2)
            adc #00
            eor #$FF        
            sec 
            adc c_cnct      ;Coinct-Price -> A
            ifcc            ;Branch if no borrow
                ;adc c_bonusc    ;Add in bonus coins, see if they help
                bmi ext         ;Branch if coinct+bonus coins < price
                ;sta c_bonusc    ;Else A = Unused bonus coins
                lda #00
            endif
            cpy #02         ;Y=coin mode - Coin mode 2 or 3??
            ifcc            ;Branch if mode 2 or 3, give 1 credit
                inc c_crdt      ;Else give 2 for mode 1
            endif
            inc c_crdt   
        endif
        sta c_cnct      ;Update coinct
		;Moved in from Mainloop
		lda c_cmode         ;Free Play??
		;and #03             ;Look at Coin Bits
		ifeq
			lda #02
			sta c_crdt         ;Free Play
			sta c_oldc			;Also save here so there isn't a coin sound on start
			;sta c_tcmflg       ;Skip This nonsense
		endif
		;*************************************
		;Fall Through to handle EM counters
		;*************************************
ext     lda c_intct  
        lsr A           ;Use LSB for pulse
        ifcc  
            ldy #00         ;Start with flag of 0
            ldx #02
            begin
                lda c_cctim,X   ;Check Timer(X)
                ifne            ;Neither running nor pending
                    cmp #$10        ;Is it running
                    ifcs            ;No, skip
                        adc #$EF        ;else dec 4 MSB
                        iny             ;Set on Flag
                        sta c_cctim,X  
                    endif
                endif
                dex         
            miend
            tya             ;Check "on" Flag
            ifeq            ;Skip if any on
            ;If any of the counters are currently on, we check to see if any can be started
                ldx #02
                begin
                    lda c_cctim,X    ;We need to start this one
                    ifne            ;No, no counts pending
                        clc 
                        adc #$EF        ;Set 4 MSB, Dec 4 LSB
                        sta c_cctim,X    ;Start Timer
                        bmi ?ccrts        ;Exit, so we don't start more
                    endif
                    dex 
                miend
            endif
        endif
?ccrts  rts 

;_modlo  .byte $7f,$02,$04,$04,$05,$03,$7f,$7f   ;7f generates 0 bonus coins
;*****************************************************
; End of Coin Routine
;*****************************************************

