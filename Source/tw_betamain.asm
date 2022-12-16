;**********************************
;**********************************
    .title "TWBeta"
    .sbttl "MainLine"
;**********************************
;**********************************
    .TEXT ".TWBETAMAIN."
;**********************************
 
b_main	sei 
        cld 
        ldx #$FF         ;Set Stack Pointer 
        txs 
        jsr b_test         ;Test the Gamma system	
		;jsr init3d
?spin	lda #comlimt
		sta curcommand
		begin
			;A x 8 bytes for each command
			lda curcommand
			asl A 
			asl A
			asl A 
			tax 
			lda commandtable,X
			ifmi
				;command is ready to process for BETA
				ifvc
					;but only if BETA isn't already done!!!
					lda curcommand
					and #$1F			;limit to 32 objects
					asl A				;words
					lda objtab,X
					sta curobjptr 
					lda objtab+1,X 
					sta curobjptr+1			
					lda curcommand+1
					sta thetax
					lda curcommand+2
					sta thetay
					lda curcommand+3
					sta thetaz
					lda curcommand+4
					sta curscal
					jsr do3dobj
				endif
			endif
			dec curcommand
		miend
		jmp ?spin
		
b_irq	rti 			;shouldn't happen ever


;********************************************
; Translate an entire object based upon 
; the addres currently in curobjptr
;********************************************
do3dobj	jsr calcmat				;Calculate the rotation matrix first
		;loop through all vertices
		begin
			ldy #0
			lda (curobjptr),Y			;is first vertex $80
			cmp #$80
			ifne
				sta curvtx
				;iny
				;lda (curobjptr),Y
				;sta curvtx+1
				iny
				lda (curobjptr),Y
				sta curvty
				;iny
				;lda (curobjptr),Y
				;sta curvty+1
				iny
				lda (curobjptr),Y
				sta curvtz
				;iny
				;lda (curobjptr),Y
				;sta curvtz+1
				clc
				lda #3
				adc curobjptr
				ifcs
					inc curobjptr+1
				endif
				lda #1				;flag to continue loop
			endif
		eqend
		;all vertices are done
		rts
		

; ;********************************
; ;*           3D-MATH           
; ;*                              
; ;*      BY MARC GOLOMBECK       
; ;*                              
; ;*   VERSION 1.61 / 07.05.2017  
; ;*                              
; ;* NEEDS THE FOLLOWING DATA     
; ;* ALREADY LOADED INTO MEMORY:  
; ;*                              
; ;* $7000: SINE/COSINE-TABLE     
; ;* $7200: POINT AND LINE DATA   
; ;* $7400: SHAPE TABLE           
; ;* $7A00: MULTIPLICATION TABLE  
; ;* $8200: PROJECTION TABLE      
; ;*                              
; ;* ADAPTED FROM CODE FOR AN APPLE 
; ;* IIE FOR 3D TRANSFORMATIONS.
; ;********************************


; ;********************************
; ;* Translate and Scale a single
; ;* 3D point.
; ;********************************
; dovtx     	lda	#SSQLO/256 ; RESET MULT-TABLE
			; sta	PSLO+1
			; lda	#SSQHI/256
			; sta	PSHI+1
			; lda	#DSQLO/256
			; sta	PDLO+1
			; lda	#DSQHI/256
			; sta	PDHI+1
			; ;* EVAL SIN/COS-TAB
			; ldx currotx
			; LDA	SINTAB,X	; GET SINUS FOR X
			; STA	SINX
			; LDA	COSTAB,X	; GET COSINUS FOR X
			; STA	COSX
			; ldy curroty
			; LDA	SINTAB,X
			; STA	SINY
			; LDA	COSTAB,X
			; STA	COSY
			; ldx currotz
			; LDA	SINTAB,X
			; STA	SINZ
			; LDA	COSTAB,X
			; STA	COSZ
			
			; ldy #0
			; lda (curvtx),Y 		;x
			; sta XPOS
			; iny 
			; lda (curvtx),Y  	;y
			; sta YPOS 
			; iny 
			; lda (curvtx),Y		;z
			; sta ZPOS
			; jsr STRTROT
			; rts

; ;*
; ;*
; ;********************************
; ;* INIT POINT POSITIONS		 
; ;********************************
; STRTROT		LDA	#$00
			; STA	PNTCNT

			; LDA	XPOS       ; RX = X * COS(Y) + Z * SIN(Y)
			; STA	MATOR		; X * COS(Y)
			; LDA	COSY
			; STA	MKAND
			; JSR	MULT
			; STA	PX1
			; STY	PX1+1

			; LDA	SINY		 ; XPOS STILL IN MATOR!
			; STA	MKAND		; X * SIN(Y)
			; JSR	MULT
			; STA	PX4
			; STY	PX4+1

			; LDA	ZPOS		 ; Z * SIN(Y)
			; STA	MATOR
			; LDA	SINY
			; STA	MKAND
			; JSR	MULT
			; STA	PX2
			; STY	PX2+1

			; LDA	COSY		 ; RZ = Z * COS(Y) - X * SIN(Y)
			; STA	MKAND		; ZPOS STILL IN MATOR
			; JSR	MULT		 ; Z * COS(Y)
			; STA	PX3
			; STY	PX3+1

			; CLC				; X-CCORD:  ADD
			; LDA	PX1
			; ADC	PX2
			; STA	RX
			; LDA	PX1+1
			; ADC	PX2+1
			; STA	RX+1		 ; SAVE HI-BYTE AS XPOS
			; ;*
			; SEC				; Z-COORD: SUB
			; LDA	PX3
			; SBC	PX4
			; STA	RZ
			; LDA	PX3+1
			; SBC	PX4+1
			; STA	RZ+1
			; ;*
			; LDA	YPOS		 ; RY = YPOS * 1 (=$7F)
			; STA	MATOR
			; LDA	#$7F
			; STA	MKAND
			; JSR	MULT
			; STA	RY
			; STY	RY+1
; ;*
; ;* PERFORM X-ROTATION
; ;*
			; LDA	RX+1		 ; RXNEW = RX+1 * 1 (=$7F)
			; STA	MATOR
			; LDA	#$7F
			; STA	MKAND
			; JSR	MULT
			; STA	RX
			; STY	RX+1
			; ;*
			; LDA	RY+1		 ; RY = Y * COS(X) - Z * SIN(X)
			; STA	MATOR		; Y * COS(X)
			; LDA	COSX
			; STA	MKAND
			; JSR	MULT
			; STA	PX1
			; STY	PX1+1
			; ;*
			; LDA	SINX		 ; CALC Y * SIN(X) FOR Z-COORD
			; STA	MKAND		; Y * SIN(X)
			; JSR	MULT
			; STA	PX3
			; STY	PX3+1
			; ;*
			; LDA	RZ+1		 ; Z * SIN(X)
			; STA	MATOR
			; LDA	SINX
			; STA	MKAND
			; JSR	MULT
			; STA	PX2
			; STY	PX2+1
			; ;*
			; LDA	COSX		 ; CALC Z * COS(X) FOR Z-CCORD
			; STA	MKAND		; Z * COS(X)
			; JSR	MULT
			; STA	PX4
			; STY	PX4+1
			; ;
			; SEC				; Y-COORD: SUB
			; LDA	PX1
			; SBC	PX2
			; STA	RY
			; LDA	PX1+1
			; SBC	PX2+1
			; STA	RY+1
			; ;
			; CLC				; Z-COORD: ADD + TRANSLATE
			; LDA	PX3
			; ADC	PX4
			; STA	RZ
			; LDA	PX3+1
			; ADC	PX4+1
			; CLC
			; ADC	ZTRANS	  ; Z-TRANSLATION AND SCALE DOWN
			; LSR A
			; LSR A
			; STA	RZ+1
; ;*
; ;* PERFORM Z-ROTATION
; ;*
; ;* DO NOT TOUCH RZ ANYMORE!
; ;*
			; LDA	RX+1		 ; RX = X * COSZ - Y * SINZ
			; STA	MATOR		; X * COS(Z)
			; LDA	COSZ
			; STA	MKAND
			; JSR	MULT
			; STA	PX1
			; STY	PX1+1
			; ;
			; LDA	SINZ		 ; X * SIN(Z)
			; STA	MKAND
			; JSR	MULT
			; STA	PX3
			; STY	PX3+1
			; ;
			; LDA	RY+1		 ; Y * SIN(Z)
			; STA	MATOR
			; LDA	SINZ
			; STA	MKAND
			; JSR	MULT
			; STA	PX2
			; STY	PX2+1
			; ;
			; LDA	COSZ		 ; Y * COS(Z)
			; STA	MKAND
			; JSR	MULT
			; STA	PX4
			; STY	PX4+1
			; ;
			; SEC				; NOW CALCULATE NEW RX
			; LDA	PX1		; SINCE OLD RX IS NEEDED FOR
			; SBC	PX2		; RY-CACLULUS BEFORE!
			; STA	RX
			; LDA	PX1+1
			; SBC	PX2+1
			; STA	RX+1
			; ;
			; CLC				; Y-COORD: ADD
			; LDA	PX3
			; ADC	PX4
			; STA	RY
			; LDA	PX3+1
			; ADC	PX4+1
			; STA	RY+1
; ;*
; ;* PERFORM SCALING AND TRANSLATION
; ;*
			; ; ASL	RX
			; ; ROL	RX+1
			; ; ASL	RX
			; ; ROL	RX+1
			; ; ASL	RY
			; ; ROL	RY+1
			; ; ASL	RY
			; ; ROL	RY+1
			; ; ;
			; ; LDX	RZ+1		 ; LOAD PROJECTION
			; ; LDA	PROJTAB,X
			; ; STA	MATOR
			; ; LDA	RX+1		 ; ONLY HI-BYTE!
			; ; STA	MKAND
			; ; JSR	UMULT
			; ; STY	QX
			; ; ;
			; ; LDA	RY+1		 ; ONLY HI-BYTE!
			; ; STA	MKAND
			; ; JSR	UMULT
			; ; STY	QY
			; ; ;
			; ; CLC				; XD = QX + XT
			; ; LDA	QX
			; ; ADC	#XTRANS
			; ; STA	XD
			; ; LDA	#$00
			; ; STA	XD+1
			; ; LDA	QX
			; ; BMI	CALCYD	  ; IF QX < 0 THEN DO NOT ADD CARRY!
			; ; LDA	#$00
			; ; ADC	#$00		 ; ADD CARRY BIT
			; ; STA	XD+1
; ; CALCYD		CLC				; YD = QY + YT
			; ; LDA	QY
			; ; ADC	#YTRANS
			; ; STA	YD		 ; ONLY 1 BYTE HERE, NO CARRY!

; ;*
; ;* WRITE DATA BACK TO TABLE
; ;*
; WRTPNT      
			; LDA	XD			; SAVE XDRAW POSITION
			; STA	OUTTABLE,X
			; INX
			; LDA	XD+1
			; STA	OUTTABLE,X
			; INX
			; LDA	YD			; SAVE YDRAW POSITION
			; STA	OUTTABLE,X

			; INC	PNTCNT
			; LDA	PNTCNT
			; CMP	NUMPNT
			; BCS	INITEND	 ; ALL DONE
			; JMP	GETPNT
; INITEND     RTS
				
; ;********************************
; ;*SIGNED  DIVISION 16 BIT/8 BIT *
; ;********************************

; DIVI		STY	DEND	    ;
			; STA	DEND+1
			; STX	DOR
; ;*
		    ; LDX	#$00
; DECHK		LDA	DEND+1	  ; DIVIDEND NEG?
			; BPL	DORCHK
			; INX				; INC X-REG FOR NEG SIGN
			; LDA	DEND
			; SEC				; TWO'S COMPLEMENT
			; SBC	#$01
			; EOR	#$FF
			; STA	DEND
			; LDA	DEND+1
			; SBC	#$00
			; EOR	#$FF
			; STA	DEND+1
; DORCHK		LDA	DOR		; DIVISOR NEG?
			; BPL	DIVIGO
			; INX				; INC X-REG FOR NEG SIGN
			; SEC				; TWO'S COMPLEMENT
			; SBC	#$01
			; EOR	#$FF
			; STA	DOR
; ;*
; DIVIGO		LDA	DEND+1	  ; TOO LARGE OR ZERO?
			; CMP	DOR		; CMP HI-BYTE WITH DOR!
			; BCC	DLOOP		; NO -> NO ERROR!
			; JMP	DIVERR     ; YES -> ERROR!
; DLOOP		ASL	DEND		 ; DOUBLE SHIFT DIVIDEND
			; ROL		      ; DEND+1 STILL IN ACCU!
			; BCS	DSUBTR	  ; SUBTRACTION WHEN CARRY IS SET
			; CMP	DOR
			; BCC	DLOOP2
; DSUBTR		SBC DOR
		    ; INC DEND
; DLOOP2		ASL DEND
			; ROL
			; BCS	DSUBTR2
			; CMP	DOR
			; BCC	DLOOP3
; DSUBTR2		SBC	DOR
			; INC DEND
; DLOOP3		ASL DEND
			; ROL
			; BCS	DSUBTR3
			; CMP	DOR
			; BCC	DLOOP4
; DSUBTR3		SBC	DOR
			; INC	DEND
; DLOOP4		ASL	DEND
			; ROL
			; BCS	DSUBTR4
			; CMP	DOR
			; BCC	DLOOP5
; DSUBTR4		SBC	DOR
			; INC	DEND
; DLOOP5		ASL	DEND
			; ROL
			; BCS	DSUBTR5
			; CMP	DOR
			; BCC	DLOOP6
; DSUBTR5		SBC	DOR
			; INC	DEND
; DLOOP6		ASL	DEND
			; ROL
			; BCS	DSUBTR6
			; CMP	DOR
			; BCC	DLOOP7
; DSUBTR6		SBC	DOR
			; INC	DEND
; DLOOP7		ASL	DEND
			; ROL
			; BCS	DSUBTR7
			; CMP	DOR
			; BCC	DLOOP8
; DSUBTR7		SBC	DOR
			; INC	DEND
; DLOOP8		ASL	DEND
			; ROL
			; BCS	DSUBTR8
			; CMP	DOR
			; BCC	DCONT
; DSUBTR8		SBC	DOR
			; INC	DEND

; ;*
; ;* STORE RESULTS
; ;*
; DCONT		STA	DOR		; MOVE REMAINDER IN DOR
			; CLC				; NO ERROR -> CLEAR CARRY
			; CPX	#$01		 ; NEG SIGN FOR RESULT?
			; BNE	DIVEND	  ; NO, SIGN IS POSITIVE -> END
			; EOR	#$FF		 ; TWO'S COMPLEMENT  OF ACCU
			; CLC
			; ADC	#$01
			; STA	DOR
			; LDA	DEND
			; EOR	#$FF
			; ADC	#$01
			; STA	DEND
			; LDA	DOR		; DOR = REMAINDER
; DIVEND		LDY	DEND       ; DEND = QUOTIENT
			; BNE	DIVRTS
			; INY
; DIVRTS		RTS
; DIVERR		LDY	#$00       ; RETURN 0 AS RESULT
		    ; RTS


; ;*
; ;********************************
; ;* 8-BIT UNSIGNED FAST MULTIPLY *
; ;********************************
; UMULT		LDY	MKAND
			; STY	TEMP1		; SAVE FOR LATER USE
; UMTCHK		LDA	MATOR
			; STA	TEMP2		; SAVE FOR LATER USE
			; STA	PSHI
			; EOR	#$FF
			; STA	PDHI
			; SEC
			; LDA	(PSHI),Y	; GET (A+Y)^2/4 (HI-BYTE)
			; SBC	(PDHI),Y   ; SUBTRACT (-A+Y)^2/4 (HI-BYTE)
; ;*
; ;* STORE RESULTS
; ;*
			; LDY	TEMP1		; CHECK IF MKAND < 0
			; BPL	UMDONE	  ; NO -> ALL DONE
			; SEC				; MATOR STILL IN ACCU!
			; SBC	TEMP2      ; SUBTRACT TEMP2 FROM MATOR
; ;
; UMDONE		TAY		      ; MOVE ACCU TO Y-REG AS RESULT
		    ; RTS

; ;********************************
; ;* 8-BIT SIGNED COMPL MULTIPLY  *
; ;********************************
; MULT		LDY	MKAND
		    ; STY	TEMP1		; SAVE FOR LATER USE
; MTCHK		LDA	MATOR
			; STA	TEMP2		; SAVE FOR LATER USE
			; STA	PSLO		 ; INDEX INTO SUM TABLE BY A
			; STA	PSHI
			; EOR	#$FF
			; STA	PDLO		 ; INDEX INTO DIFF TABLE BY -A-1
			; STA	PDHI
			; LDA	(PSLO),Y	; GET (A+Y)^2/4 (LO BYTE)
			; SEC
			; SBC	(PDLO),Y	; SUBTRACT (-A+Y)^2/4 (LO BYTE)
			; STA	MKAND		; SAVE IT
			; LDA	(PSHI),Y	; GET (A+Y)^2/4 (HI-BYTE)
			; SBC	(PDHI),Y   ; SUBTRACT (-A+Y)^2/4 (HI-BYTE)
; ;*
; ;* STORE RESULTS
; ;*
			; LDY	TEMP1      ; CHECK IF MKAND < 0
			; BPL	CHKT2      ; NO CHECK MATOR
			; SEC			   ; MATOR STILL IN ACCU!
			; SBC	TEMP2      ; SUBTRACT TEMP2 FROM MATOR
; ;
; CHKT2		LDY	TEMP2		; CHECK IF MATOR < 0
			; BPL	MDONE		; NO, RTS
			; SEC				; MATOR STILL IN ACCU
			; SBC	TEMP1      ; SUBTRACT TEMP1 FROM MATOR
; ;*
; MDONE		TAY		      ; MOVE ACCU TO Y-REG AS RESULT
		    ; LDA	MKAND      ; LOAD LO BYTE
		    ; RTS

; ;********************************
; ;* 8-BIT SIGNED FAST MULTIPLY   *
; ;********************************

; FMULT		LDY	MKAND
			; STY	TEMP1		; SAVE FOR LATER USE
; FMTCHK		LDA	MATOR
			; STA	TEMP2		; SAVE FOR LATER USE
			; STA	PSHI
			; EOR	#$FF
			; STA	PDHI
			; SEC
			; LDA	(PSHI),Y	; GET (A+Y)^2/4 (HI-BYTE)
			; SBC	(PDHI),Y   ; SUBTRACT (-A+Y)^2/4 (HI-BYTE)
; ;*
; ;* STORE RESULTS
; ;*
			; LDY	TEMP1		; CHECK IF MKAND < 0
			; BPL	FCHKT2	  ; NO CHECK MATOR
			; SEC				; MATOR STILL IN ACCU!
			; SBC	TEMP2      ; SUBTRACT TEMP2 FROM MATOR
; ;
; FCHKT2		LDY	TEMP2		; CHECK IF MATOR < 0
			; BPL	FMDONE	  ; NO, RTS
			; SEC				; MATOR STILL IN ACCU
			; SBC	TEMP1		; SUBTRACT TEMP1 FROM MATOR
; ;
; FMDONE		TAY		      ; MOVE ACCU TO Y-REG AS RESULT
		    ; RTS

SINTAB
		.db $00,$03,$06,$09,$0C,$10,$13,$16
		.db $19,$1C,$1F,$22,$25,$28,$2B,$2E
		.db $31,$33,$36,$39,$3C,$3F,$41,$44
		.db $47,$49,$4C,$4E,$51,$53,$55,$58
		.db $5A,$5C,$5E,$60,$62,$64,$66,$68
		.db $6A,$6B,$6D,$6F,$70,$71,$73,$74
		.db $75,$76,$78,$79,$7A,$7A,$7B,$7C
		.db $7D,$7D,$7E,$7E,$7E,$7F,$7F,$7F
COSTAB
		.db $7F,$7F,$7F,$7F,$7E,$7E,$7E,$7D
		.db $7D,$7C,$7B,$7A,$7A,$79,$78,$76
		.db $75,$74,$73,$71,$70,$6F,$6D,$6B
		.db $6A,$68,$66,$64,$62,$60,$5E,$5C
		.db $5A,$58,$55,$53,$51,$4E,$4C,$49
		.db $47,$44,$41,$3F,$3C,$39,$36,$33
		.db $31,$2E,$2B,$28,$25,$22,$1F,$1C
		.db $19,$16,$13,$10,$0C,$09,$06,$03
		.db $00,$FD,$FA,$F7,$F4,$F0,$ED,$EA
		.db $E7,$E4,$E1,$DE,$DB,$D8,$D5,$D2
		.db $CF,$CD,$CA,$C7,$C4,$C1,$BF,$BC
		.db $B9,$B7,$B4,$B2,$AF,$AD,$AB,$A8
		.db $A6,$A4,$A2,$A0,$9E,$9C,$9A,$98
		.db $96,$95,$93,$91,$90,$8F,$8D,$8C
		.db $8B,$8A,$88,$87,$86,$86,$85,$84
		.db $83,$83,$82,$82,$82,$81,$81,$81
		.db $81,$81,$81,$81,$82,$82,$82,$83
		.db $83,$84,$85,$86,$86,$87,$88,$8A
		.db $8B,$8C,$8D,$8F,$90,$91,$93,$95
		.db $96,$98,$9A,$9C,$9E,$A0,$A2,$A4
		.db $A6,$A8,$AB,$AD,$AF,$B2,$B4,$B7
		.db $B9,$BC,$BF,$C1,$C4,$C7,$CA,$CD
		.db $CF,$D2,$D5,$D8,$DB,$DE,$E1,$E4
		.db $E7,$EA,$ED,$F0,$F4,$F7,$FA,$FD
		.db $00,$03,$06,$09,$0C,$10,$13,$16
		.db $19,$1C,$1F,$22,$25,$28,$2B,$2E
		.db $31,$33,$36,$39,$3C,$3F,$41,$44
		.db $47,$49,$4C,$4E,$51,$53,$55,$58
		.db $5A,$5C,$5E,$60,$62,$64,$66,$68
		.db $6A,$6B,$6D,$6F,$70,$71,$73,$74
		.db $75,$76,$78,$79,$7A,$7A,$7B,$7C
		.db $7D,$7D,$7E,$7E,$7E,$7F,$7F,$7F


SSQLO   .db $00,$00,$01,$02,$04,$06,$09,$0C
        .db $10,$14,$19,$1E,$24,$2A,$31,$38
        .db $40,$48,$51,$5A,$64,$6E,$79,$84
        .db $90,$9C,$A9,$B6,$C4,$D2,$E1,$F0
        .db $00,$10,$21,$32,$44,$56,$69,$7C
        .db $90,$A4,$B9,$CE,$E4,$FA,$11,$28
        .db $40,$58,$71,$8A,$A4,$BE,$D9,$F4
        .db $10,$2C,$49,$66,$84,$A2,$C1,$E0
        .db $00,$20,$41,$62,$84,$A6,$C9,$EC
        .db $10,$34,$59,$7E,$A4,$CA,$F1,$18
        .db $40,$68,$91,$BA,$E4,$0E,$39,$64
        .db $90,$BC,$E9,$16,$44,$72,$A1,$D0
        .db $00,$30,$61,$92,$C4,$F6,$29,$5C
        .db $90,$C4,$F9,$2E,$64,$9A,$D1,$08
        .db $40,$78,$B1,$EA,$24,$5E,$99,$D4
        .db $10,$4C,$89,$C6,$04,$42,$81,$C0
        .db $00,$40,$81,$C2,$04,$46,$89,$CC
        .db $10,$54,$99,$DE,$24,$6A,$B1,$F8
        .db $40,$88,$D1,$1A,$64,$AE,$F9,$44
        .db $90,$DC,$29,$76,$C4,$12,$61,$B0
        .db $00,$50,$A1,$F2,$44,$96,$E9,$3C
        .db $90,$E4,$39,$8E,$E4,$3A,$91,$E8
        .db $40,$98,$F1,$4A,$A4,$FE,$59,$B4
        .db $10,$6C,$C9,$26,$84,$E2,$41,$A0
        .db $00,$60,$C1,$22,$84,$E6,$49,$AC
        .db $10,$74,$D9,$3E,$A4,$0A,$71,$D8
        .db $40,$A8,$11,$7A,$E4,$4E,$B9,$24
        .db $90,$FC,$69,$D6,$44,$B2,$21,$90
        .db $00,$70,$E1,$52,$C4,$36,$A9,$1C
        .db $90,$04,$79,$EE,$64,$DA,$51,$C8
        .db $40,$B8,$31,$AA,$24,$9E,$19,$94
        .db $10,$8C,$09,$86,$04,$82,$01,$80
        .db $00,$80,$01,$82,$04,$86,$09,$8C
        .db $10,$94,$19,$9E,$24,$AA,$31,$B8
        .db $40,$C8,$51,$DA,$64,$EE,$79,$04
        .db $90,$1C,$A9,$36,$C4,$52,$E1,$70
        .db $00,$90,$21,$B2,$44,$D6,$69,$FC
        .db $90,$24,$B9,$4E,$E4,$7A,$11,$A8
        .db $40,$D8,$71,$0A,$A4,$3E,$D9,$74
        .db $10,$AC,$49,$E6,$84,$22,$C1,$60
        .db $00,$A0,$41,$E2,$84,$26,$C9,$6C
        .db $10,$B4,$59,$FE,$A4,$4A,$F1,$98
        .db $40,$E8,$91,$3A,$E4,$8E,$39,$E4
        .db $90,$3C,$E9,$96,$44,$F2,$A1,$50
        .db $00,$B0,$61,$12,$C4,$76,$29,$DC
        .db $90,$44,$F9,$AE,$64,$1A,$D1,$88
        .db $40,$F8,$B1,$6A,$24,$DE,$99,$54
        .db $10,$CC,$89,$46,$04,$C2,$81,$40
        .db $00,$C0,$81,$42,$04,$C6,$89,$4C
        .db $10,$D4,$99,$5E,$24,$EA,$B1,$78
        .db $40,$08,$D1,$9A,$64,$2E,$F9,$C4
        .db $90,$5C,$29,$F6,$C4,$92,$61,$30
        .db $00,$D0,$A1,$72,$44,$16,$E9,$BC
        .db $90,$64,$39,$0E,$E4,$BA,$91,$68
        .db $40,$18,$F1,$CA,$A4,$7E,$59,$34
        .db $10,$EC,$C9,$A6,$84,$62,$41,$20
        .db $00,$E0,$C1,$A2,$84,$66,$49,$2C
        .db $10,$F4,$D9,$BE,$A4,$8A,$71,$58
        .db $40,$28,$11,$FA,$E4,$CE,$B9,$A4
        .db $90,$7C,$69,$56,$44,$32,$21,$10
        .db $00,$F0,$E1,$D2,$C4,$B6,$A9,$9C
        .db $90,$84,$79,$6E,$64,$5A,$51,$48
        .db $40,$38,$31,$2A,$24,$1E,$19,$14
        .db $10,$0C,$09,$06,$04,$02,$01,$00

SSQHI   .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $01,$01,$01,$01,$01,$01,$01,$01
        .db $01,$01,$01,$01,$01,$01,$02,$02
        .db $02,$02,$02,$02,$02,$02,$02,$02
        .db $03,$03,$03,$03,$03,$03,$03,$03
        .db $04,$04,$04,$04,$04,$04,$04,$04
        .db $05,$05,$05,$05,$05,$05,$05,$06
        .db $06,$06,$06,$06,$06,$07,$07,$07
        .db $07,$07,$07,$08,$08,$08,$08,$08
        .db $09,$09,$09,$09,$09,$09,$0A,$0A
        .db $0A,$0A,$0A,$0B,$0B,$0B,$0B,$0C
        .db $0C,$0C,$0C,$0C,$0D,$0D,$0D,$0D
        .db $0E,$0E,$0E,$0E,$0F,$0F,$0F,$0F
        .db $10,$10,$10,$10,$11,$11,$11,$11
        .db $12,$12,$12,$12,$13,$13,$13,$13
        .db $14,$14,$14,$15,$15,$15,$15,$16
        .db $16,$16,$17,$17,$17,$18,$18,$18
        .db $19,$19,$19,$19,$1A,$1A,$1A,$1B
        .db $1B,$1B,$1C,$1C,$1C,$1D,$1D,$1D
        .db $1E,$1E,$1E,$1F,$1F,$1F,$20,$20
        .db $21,$21,$21,$22,$22,$22,$23,$23
        .db $24,$24,$24,$25,$25,$25,$26,$26
        .db $27,$27,$27,$28,$28,$29,$29,$29
        .db $2A,$2A,$2B,$2B,$2B,$2C,$2C,$2D
        .db $2D,$2D,$2E,$2E,$2F,$2F,$30,$30
        .db $31,$31,$31,$32,$32,$33,$33,$34
        .db $34,$35,$35,$35,$36,$36,$37,$37
        .db $38,$38,$39,$39,$3A,$3A,$3B,$3B
        .db $3C,$3C,$3D,$3D,$3E,$3E,$3F,$3F
        .db $40,$40,$41,$41,$42,$42,$43,$43
        .db $44,$44,$45,$45,$46,$46,$47,$47
        .db $48,$48,$49,$49,$4A,$4A,$4B,$4C
        .db $4C,$4D,$4D,$4E,$4E,$4F,$4F,$50
        .db $51,$51,$52,$52,$53,$53,$54,$54
        .db $55,$56,$56,$57,$57,$58,$59,$59
        .db $5A,$5A,$5B,$5C,$5C,$5D,$5D,$5E
        .db $5F,$5F,$60,$60,$61,$62,$62,$63
        .db $64,$64,$65,$65,$66,$67,$67,$68
        .db $69,$69,$6A,$6A,$6B,$6C,$6C,$6D
        .db $6E,$6E,$6F,$70,$70,$71,$72,$72
        .db $73,$74,$74,$75,$76,$76,$77,$78
        .db $79,$79,$7A,$7B,$7B,$7C,$7D,$7D
        .db $7E,$7F,$7F,$80,$81,$82,$82,$83
        .db $84,$84,$85,$86,$87,$87,$88,$89
        .db $8A,$8A,$8B,$8C,$8D,$8D,$8E,$8F
        .db $90,$90,$91,$92,$93,$93,$94,$95
        .db $96,$96,$97,$98,$99,$99,$9A,$9B
        .db $9C,$9D,$9D,$9E,$9F,$A0,$A0,$A1
        .db $A2,$A3,$A4,$A4,$A5,$A6,$A7,$A8
        .db $A9,$A9,$AA,$AB,$AC,$AD,$AD,$AE
        .db $AF,$B0,$B1,$B2,$B2,$B3,$B4,$B5
        .db $B6,$B7,$B7,$B8,$B9,$BA,$BB,$BC
        .db $BD,$BD,$BE,$BF,$C0,$C1,$C2,$C3
        .db $C4,$C4,$C5,$C6,$C7,$C8,$C9,$CA
        .db $CB,$CB,$CC,$CD,$CE,$CF,$D0,$D1
        .db $D2,$D3,$D4,$D4,$D5,$D6,$D7,$D8
        .db $D9,$DA,$DB,$DC,$DD,$DE,$DF,$E0
        .db $E1,$E1,$E2,$E3,$E4,$E5,$E6,$E7
        .db $E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF
        .db $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7
        .db $F8,$F9,$FA,$FB,$FC,$FD,$FE,$00

DSQLO   .db $80,$01,$82,$04,$86,$09,$8C,$10
        .db $94,$19,$9E,$24,$AA,$31,$B8,$40
        .db $C8,$51,$DA,$64,$EE,$79,$04,$90
        .db $1C,$A9,$36,$C4,$52,$E1,$70,$00
        .db $90,$21,$B2,$44,$D6,$69,$FC,$90
        .db $24,$B9,$4E,$E4,$7A,$11,$A8,$40
        .db $D8,$71,$0A,$A4,$3E,$D9,$74,$10
        .db $AC,$49,$E6,$84,$22,$C1,$60,$00
        .db $A0,$41,$E2,$84,$26,$C9,$6C,$10
        .db $B4,$59,$FE,$A4,$4A,$F1,$98,$40
        .db $E8,$91,$3A,$E4,$8E,$39,$E4,$90
        .db $3C,$E9,$96,$44,$F2,$A1,$50,$00
        .db $B0,$61,$12,$C4,$76,$29,$DC,$90
        .db $44,$F9,$AE,$64,$1A,$D1,$88,$40
        .db $F8,$B1,$6A,$24,$DE,$99,$54,$10
        .db $CC,$89,$46,$04,$C2,$81,$40,$00
        .db $C0,$81,$42,$04,$C6,$89,$4C,$10
        .db $D4,$99,$5E,$24,$EA,$B1,$78,$40
        .db $08,$D1,$9A,$64,$2E,$F9,$C4,$90
        .db $5C,$29,$F6,$C4,$92,$61,$30,$00
        .db $D0,$A1,$72,$44,$16,$E9,$BC,$90
        .db $64,$39,$0E,$E4,$BA,$91,$68,$40
        .db $18,$F1,$CA,$A4,$7E,$59,$34,$10
        .db $EC,$C9,$A6,$84,$62,$41,$20,$00
        .db $E0,$C1,$A2,$84,$66,$49,$2C,$10
        .db $F4,$D9,$BE,$A4,$8A,$71,$58,$40
        .db $28,$11,$FA,$E4,$CE,$B9,$A4,$90
        .db $7C,$69,$56,$44,$32,$21,$10,$00
        .db $F0,$E1,$D2,$C4,$B6,$A9,$9C,$90
        .db $84,$79,$6E,$64,$5A,$51,$48,$40
        .db $38,$31,$2A,$24,$1E,$19,$14,$10
        .db $0C,$09,$06,$04,$02,$01,$00,$00
        .db $00,$01,$02,$04,$06,$09,$0C,$10
        .db $14,$19,$1E,$24,$2A,$31,$38,$40
        .db $48,$51,$5A,$64,$6E,$79,$84,$90
        .db $9C,$A9,$B6,$C4,$D2,$E1,$F0,$00
        .db $10,$21,$32,$44,$56,$69,$7C,$90
        .db $A4,$B9,$CE,$E4,$FA,$11,$28,$40
        .db $58,$71,$8A,$A4,$BE,$D9,$F4,$10
        .db $2C,$49,$66,$84,$A2,$C1,$E0,$00
        .db $20,$41,$62,$84,$A6,$C9,$EC,$10
        .db $34,$59,$7E,$A4,$CA,$F1,$18,$40
        .db $68,$91,$BA,$E4,$0E,$39,$64,$90
        .db $BC,$E9,$16,$44,$72,$A1,$D0,$00
        .db $30,$61,$92,$C4,$F6,$29,$5C,$90
        .db $C4,$F9,$2E,$64,$9A,$D1,$08,$40
        .db $78,$B1,$EA,$24,$5E,$99,$D4,$10
        .db $4C,$89,$C6,$04,$42,$81,$C0,$00
        .db $40,$81,$C2,$04,$46,$89,$CC,$10
        .db $54,$99,$DE,$24,$6A,$B1,$F8,$40
        .db $88,$D1,$1A,$64,$AE,$F9,$44,$90
        .db $DC,$29,$76,$C4,$12,$61,$B0,$00
        .db $50,$A1,$F2,$44,$96,$E9,$3C,$90
        .db $E4,$39,$8E,$E4,$3A,$91,$E8,$40
        .db $98,$F1,$4A,$A4,$FE,$59,$B4,$10
        .db $6C,$C9,$26,$84,$E2,$41,$A0,$00
        .db $60,$C1,$22,$84,$E6,$49,$AC,$10
        .db $74,$D9,$3E,$A4,$0A,$71,$D8,$40
        .db $A8,$11,$7A,$E4,$4E,$B9,$24,$90
        .db $FC,$69,$D6,$44,$B2,$21,$90,$00
        .db $70,$E1,$52,$C4,$36,$A9,$1C,$90
        .db $04,$79,$EE,$64,$DA,$51,$C8,$40
        .db $B8,$31,$AA,$24,$9E,$19,$94,$10
        .db $8C,$09,$86,$04,$82,$01,$80,$00

DSQHI   .db $3F,$3F,$3E,$3E,$3D,$3D,$3C,$3C
        .db $3B,$3B,$3A,$3A,$39,$39,$38,$38
        .db $37,$37,$36,$36,$35,$35,$35,$34
        .db $34,$33,$33,$32,$32,$31,$31,$31
        .db $30,$30,$2F,$2F,$2E,$2E,$2D,$2D
        .db $2D,$2C,$2C,$2B,$2B,$2B,$2A,$2A
        .db $29,$29,$29,$28,$28,$27,$27,$27
        .db $26,$26,$25,$25,$25,$24,$24,$24
        .db $23,$23,$22,$22,$22,$21,$21,$21
        .db $20,$20,$1F,$1F,$1F,$1E,$1E,$1E
        .db $1D,$1D,$1D,$1C,$1C,$1C,$1B,$1B
        .db $1B,$1A,$1A,$1A,$19,$19,$19,$19
        .db $18,$18,$18,$17,$17,$17,$16,$16
        .db $16,$15,$15,$15,$15,$14,$14,$14
        .db $13,$13,$13,$13,$12,$12,$12,$12
        .db $11,$11,$11,$11,$10,$10,$10,$10
        .db $0F,$0F,$0F,$0F,$0E,$0E,$0E,$0E
        .db $0D,$0D,$0D,$0D,$0C,$0C,$0C,$0C
        .db $0C,$0B,$0B,$0B,$0B,$0A,$0A,$0A
        .db $0A,$0A,$09,$09,$09,$09,$09,$09
        .db $08,$08,$08,$08,$08,$07,$07,$07
        .db $07,$07,$07,$06,$06,$06,$06,$06
        .db $06,$05,$05,$05,$05,$05,$05,$05
        .db $04,$04,$04,$04,$04,$04,$04,$04
        .db $03,$03,$03,$03,$03,$03,$03,$03
        .db $02,$02,$02,$02,$02,$02,$02,$02
        .db $02,$02,$01,$01,$01,$01,$01,$01
        .db $01,$01,$01,$01,$01,$01,$01,$01
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$00
        .db $00,$00,$00,$00,$00,$00,$00,$01
        .db $01,$01,$01,$01,$01,$01,$01,$01
        .db $01,$01,$01,$01,$01,$02,$02,$02
        .db $02,$02,$02,$02,$02,$02,$02,$03
        .db $03,$03,$03,$03,$03,$03,$03,$04
        .db $04,$04,$04,$04,$04,$04,$04,$05
        .db $05,$05,$05,$05,$05,$05,$06,$06
        .db $06,$06,$06,$06,$07,$07,$07,$07
        .db $07,$07,$08,$08,$08,$08,$08,$09
        .db $09,$09,$09,$09,$09,$0A,$0A,$0A
        .db $0A,$0A,$0B,$0B,$0B,$0B,$0C,$0C
        .db $0C,$0C,$0C,$0D,$0D,$0D,$0D,$0E
        .db $0E,$0E,$0E,$0F,$0F,$0F,$0F,$10
        .db $10,$10,$10,$11,$11,$11,$11,$12
        .db $12,$12,$12,$13,$13,$13,$13,$14
        .db $14,$14,$15,$15,$15,$15,$16,$16
        .db $16,$17,$17,$17,$18,$18,$18,$19
        .db $19,$19,$19,$1A,$1A,$1A,$1B,$1B
        .db $1B,$1C,$1C,$1C,$1D,$1D,$1D,$1E
        .db $1E,$1E,$1F,$1F,$1F,$20,$20,$21
        .db $21,$21,$22,$22,$22,$23,$23,$24
        .db $24,$24,$25,$25,$25,$26,$26,$27
        .db $27,$27,$28,$28,$29,$29,$29,$2A
        .db $2A,$2B,$2B,$2B,$2C,$2C,$2D,$2D
        .db $2D,$2E,$2E,$2F,$2F,$30,$30,$31
        .db $31,$31,$32,$32,$33,$33,$34,$34
        .db $35,$35,$35,$36,$36,$37,$37,$38
        .db $38,$39,$39,$3A,$3A,$3B,$3B,$3C
        .db $3C,$3D,$3D,$3E,$3E,$3F,$3F,$00




		
;*******************************************
		.sbttl "Poweron Self Test"
;*******************************************
b_test 	;set default test flags - which is 'all failed' + $0A which is what Alpha looks for to know BETA is running
		lda #$0A
		sta tstflag
										
;********************************************
    .sbttl "Page 7 Test"
;********************************************
?stest0 lda #$FF
        ldx #00
        begin
            sta $0700,X            ;First set RAM to $FF
            dex
        eqend
		;X is now $00
        begin
        lda $0700,X
            eor #$FF
            ifeq            ;Got a bad RAM here
                lda #01             ;RAM is 2K x 8, use 1,2,4,8,10,20,40,80 test
?rnxtpat        sta $0700,X         ;Write out test pattern
                tay 
                eor $0700,X
                ifeq    ;Not the same, oh shit! An error!
                    tya 
                    beq ?patdone        ;Stop when RAM is 0
                    eor #$FF            ;Write more than one bit
                    sta $0700,X         ;Ed says this causes some RAM's to fail
                    eor $0700,X
                    ifeq            ;Get an Error?
                        tya 
                        asl a
                        jmp ?rnxtpat
                    endif
                endif
            endif
            ldy #01
            bne ?rambad         ;Report top RAM bad!
?patdone    inx 
        eqend                   ;Next location
		
		;At this point we know 7 page is okay
        stx tstart      	;Do the rest now
        lda #06
        sta tend
        stx tstart+1
        dex 
        stx tend+1      	;Test 0-6ff
        jsr ?tst2k          ;Test 2K
?zpgtst	ifcs
?rambad     lda #$7A			;Have a RAM error
        else
			;Main RAM is good here
			lda tstflag
			ora #$80
		endif
		sta tstflag       
		;Now test COM Ram
		lda #$40			;COM RAM High
		sta tstart      	;Do the rest now
		lda #0
		sta tstart+1
        lda #07
        sta tend
        lda #$ff
        stx tend+1      	;Test 0-7ff
		jsr ?tst2k           ;Test 2K
		ifcs
            lda tstflag
			and #$BF 			;COM RAM Error
		else
			;Main RAM is good here
			lda tstflag
			ora #$40
        endif
		sta tstflag
        
;********************************************
;* RAM Done... Now checksum ROM and make sure
;* it's okay(this is dumb as how would we be 
;* running if it was nfg!!)
;********************************************
        .sbttl "ROM Test"
;********************************************   
        lda #00
        tay 
        tax 
        sta temp1           	;Bottom is always 0
        sta temp3           	;Seed starts here
        
        lda # ubyte(program)    ;Do main page program now
        sta temp1+1
        ldx #64d                ;Lots of pages here
        jsr ?dosum
        lda # ubyte(program2) 	;Second ROM  
        sta temp1+1
        ldx #64d
        jsr ?dosum              ;Do C000-FFFF
        ;csums are at pgcstbl, check the now and set flags appropriately
		lda tstflag
		ldx pgcstbl
		ifne
			;Low ROM bad
			and #$DF 
		else
			ora #$20
		endif
		sta tstflag
		ldx pgcstbl+1
		ifne
			;High ROM bad
			and #$EF 
		else
			ora #$10
		endif 
		sta tstflag
		rts
		
;*********************************************
    .sbttl "Checksum Utilities"
;*********************************************
;* dosum - used to add check sum of a ROM.   
;*                                           
;* Entry:   temp1 = ptr to ROM to csum       
;*          x     = number of pages to use   
;*          temp3 = starting value         
;*                                           
;* Exit:    temp3 updated                    
;*          x,pgcstbl checksum stored here    
;*********************************************
?dosum  stx temp2           ;Save page count
		;set identifier
		ldx temp3
		lda (temp1,Y)		;Get the ROM Identifier
		sta pgidtbl,X			;Save identifier
		txa
		asl A 
		tax
		ldy #2
		lda (temp1,Y)		;Get the Major Version number
		sta pgvetbl,X		;Save 
		ldy #3
		lda (temp1,Y)		;Get the Minor Version number
		sta pgvetbl+1,X		;Save 
		ldy #0
        lda temp3           ;Use number as seed
		clc
		adc #IDENTIFIER_BL
        begin
            begin   
                eor (temp1,Y)
                iny
            eqend
            inc temp1+1
            ;sta watchdog        ;No Watchdog on BETA
            dec temp2
        eqend
        ldx temp3
        sta pgcstbl,X        ;Checksums
        inc temp3           ;Indicated 1 more
        rts 
		
;**********************************************************
    .sbttl "RAM Test Routines"
;**********************************************************
;* Assumes Page 7 has been tested and is good! 
;**********************************************************
;* Entry: (A)  = MSB of starting address                  
;*                                                        
;* Exit:  (CC) = Carry set if error occured               
;*        (A)  = Difference between expected and recieved 
;**********************************************************

?tstram 
		sta tstart         ;Save MSB of address
        clc 
        adc #$0F            ;Add for end of 4K x 8
?tst2k  sta tend
		;**********************************************************
			.sbttl "Generic RAM Test Code"
		;**********************************************************
?ramtst ldx #00         	;Enter here to test RAM
?nxtpat ldy #vtend-?voltbl
?grt10  lda ?voltbl,Y        ;Move volitile code into RAM
        sta vram,Y
        dey 
        bpl ?grt10
        lda tstart
        sta vadh1
        sta vadh2
        sta vadh3
        lda tend
        sta vadh4           ;Set up 'soft' starting address
        txa 
        bne ?grt20
        stx vpat            ;No comparison on first pass
        ldx #patend-pats+1
        bne ?grt30      ;Always
?grt20  dex 
        txa             ;Return with zero if done   
        beq endtst
?grt30  lda pats-1,X
        ldy pats,X
        jmp vram
        
;************************************************************
    .sbttl "Crashable Code"
;************************************************************

?voltbl  sta $100

vadl1       =   *-?voltbl-2+vram
vadh1       =   *-?voltbl-1+vram

        cpy $101
        
vadl2       =   *-?voltbl-2+vram
vadh2       =   *-?voltbl-1+vram

        bne ?cc50
    
vpat        =   *-?voltbl-1+vram       ;Set to zero for the inital pass

        cmp $100
        
vadl3       =   *-?voltbl-2+vram
vadh3       =   *-?voltbl-1+vram

        bne ?cc50               ;RAM Failure
        inc vadl1
        bne ?cc10
        inc vadh1
?cc10   inc vadl2
        bne ?cc30
        inc vadh2
        ldy tend
        cpy vadh2
        bcs ?cc20
        sta $01FF               ;Set last byte for next pass
        
vadh4   =   *-?voltbl-1+vram

        jmp ?nxtpat
?cc20   ldy pats,X
?cc30   inc vadl3
        bne ?cc40
        inc vadh3
?cc40   jmp vram
?cc50   sec 
        jmp ramerr

vtend
pats    .byte $00,$FE,$01,$FD,$02,$FB,$04,$F7,$08
        .byte $EF,$10,$DF,$20,$BF,$40,$7F,$80
        
patend  .byte $ff               ;Start with ff and end with RAM set to 00

endtst  clc

ramerr  tsx 
        inx
        ifne                ;Can't use rts if page 1xx as we jumped here
            rts
        endif
        ; We are doing 1 page, skip rts and jump back to rest of test
        jmp ?zpgtst