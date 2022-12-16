;*********************************************
    .TEXT ".TWMESS."
;*********************************************
    .title "Message Routine"
;*******************************************************
;* Will output specified message to specified location 
;* on screen.                                          
;*      
;*  Inputs: Y = message # 
;*
;* mesg   - Normal Message routine, uses all defaults                       
;*  Inputs: X = message # 
;*
;* mesgv  - Override Y Position
;*  Inputs: X = message # 
;*          A = Y position value
;*
;* msgent - Manual Text display (assumes position set)
;*       
;*******************************************************
;Local Vars
;msg_x 	= temp1
;msg_y 	= temp2
;msg_idx = temp4
		
msgnop_rtp2
        jsr msgnop
        jmp stpg2
		
mesg_rtp1
        jsr mesg
        jmp stpg1

mesg_rtp5
        jsr mesg
        jmp stpg5
		
msgent	stx msg_idx
		jsr stpg0
		jmp ?msgent
		
msgnop	stx msg_idx
		jsr stpg0
		jmp ?msgnop
		
mesgonly 
		stx msg_idx
		jsr stpg0
		jmp ?mesgonly 
		
mesg    stx msg_idx
		jsr stpg0				;Page to where tw_messd.asm is
        ;***********************************************************
        ;* For reference, the following variables contain
        ;* the following data, you can call msgent directly
        ;* to print text this way. Scaling is set to 2B/0L.
        ;* Use calls farther down to roll your own
        ;*
        ;*  vdata: (word) Pointer to Text
        ;*  msg_x: X Position
        ;*  msg_y: Y Position
        ;*  msg_idx: Message Index
        ;***********************************************************
		;Position for text
?msgent jsr vgcntr
		ldx msg_idx
		lda messypos,X          ;This is Y position
        sta msg_y
		lda messxpos,X          ;This is X position
        sta msg_x
?msgss	lda #00
        sta vgbrit              ;black line for positioning
        lda #02					;Scale for positioning is always 2
        jsr vgscalz             ;a=binary scaling value, linear scaling value = 0
        lda msg_x
        ldx msg_y
        jsr vgvtr1              ;Position Beam (Use vgbrit)
		;Fall through
		;Set Color and Scale Next
?msgnop ldx msg_idx
		lda messscol,X            ;Color value and scale of text
        pha 
        lsr A
        lsr A
        lsr A
        lsr A
        ora #$F0
		tay 
        lda #00
        jsr vgstat              ;Set color
		pla
        and #$0F
		clc
        adc #01
        ldy #$30
        jsr vgscal              ;Set scale  
		;Fall through
?mesgonly 
		ldx msg_idx 
		lda messptrl,X
		sta vdata
		lda messptrh,X
		sta vdata+1
		;***************************************************************************
?msgx   lda #00                 ;Init vglist offset
		ldy #00
        sta temp3+1
        begin
            lda (vdata,Y)               ;Get next character
            sta msg_x
            and #$7F                    ;Mask neg bit for end check
            tax 
            iny                         ;Next byte index
            sty temp3                   ;Save Y value
            lda vgmsga,X                ;Get correct vector JSRL from char index
            ldy temp3+1
            sta (vglist,Y)
            iny 
            lda vgmsga+1,X
            sta (vglist,Y)              ;Write it to VGRAM
            iny 
            sty temp3+1                   ;Save y
            ldy temp3                   ;Get character ptr
            bit msg_x                   ;if not end of string
        miend
        ldy temp3+1
        dey 
        jmp vgadd

;*****************************************
;* Random Maze Messages are a special case  
;* stored on a different ROM page (7)
;*
;* A - Random Message Number
;*****************************************
mzmesgr and #$0F
		asl A
		pha
		tay
		jsr stpg7
        ldx mazehintsr,Y     ;Get first message
		ifpl
			jsr ?domzm			;show message
		endif
		pla
		tay
		ldx mazehintsr+1,Y	;Get second message
		ifpl
			jsr ?domzm			;show message
		endif
		jmp stpg0			;back to page 0 and return
		
;*****************************************
;* Maze Messages are a special case and 
;* stored on a different ROM page (7)
;*
;* A - Maze Level Number
;*****************************************        
mzmesg  asl A 				;two bytes per level
		tay
		pha					;save for later
		jsr stpg7
        ldx mazehints,Y     ;Get first message
		ifpl
			jsr ?domzm			;show message
		endif
		pla
		tay			
		ldx mazehints+1,Y     ;Next maze level message
		ifpl
			jsr ?domzm			;show message
		endif
		jmp stpg0			;back to page 0 and return
		
		;Position for text
?domzm  stx msg_idx
		jsr vgcntr
		ldx msg_idx
		lda zmessypos,X          ;This is Y position
        sta msg_y
		lda zmessxpos,X          ;This is X position
        sta msg_x
		lda #00
        sta vgbrit              ;black line for positioning
        lda #02					;Scale for positioning is always 2
        jsr vgscalz             ;a=binary scaling value, linear scaling value = 0
        lda msg_x
        ldx msg_y
        jsr vgvtr1              ;Position Beam (Use vgbrit)        
		;Color and Message scale
		lda #00
		ldy #(colcyan+$F0)			;Color always Aqua
        jsr vgstat              
		lda #02					;Scale always 2
        ldy #$30
        jsr vgscal              ;Set scale  
		ldx msg_idx
		lda zmessptrl,X
		sta vdata
		lda zmessptrh,X
		sta vdata+1
		jmp ?msgx
 
 
;**********************************************
    .sbttl "Maze Header Time Info"
;**********************************************
; This shows the top header text band
; Oxygen + Reactor
;**********************************************
header  jsr vgcntr
        lda #00
        ldx #$71
        jsr vgadd2
		VGADD_VCTRS(43,18,hidden)	
        ldx #moxyg
        jsr msgnop
        jsr vgcntr
        lda #00
        ldx #$71
        jsr vgadd2
		VGADD_VCTRS(43,49,hidden)	
        ldx #mreac
        jmp msgnop 
				

; Subroutines
.export mesg, msgnop, mesgonly, header
;.export robonly, welvaxx, 
.export msgnop_rtp2,mesg_rtp1,mesg_rtp5
