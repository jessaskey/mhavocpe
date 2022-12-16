;*********************************************
    .TEXT ".TWMSG."
;*********************************************
    .title "Message Routine"
;*******************************************************
;* Will output specified message to specified location 
;* on screen.                                          
;*      
;* mzmesg - For displaying Maze Messages only, this will
;*          change the ROM page to the Maze Data Pages
;*          (currently PAGE 6) to get the actual text.
;*          This is so the level editor has the ability
;*          to edit maze messages. :-)
;*  Inputs: Y = message # * 2
;*
;* mesg   - Normal Message routine, uses all defaults                       
;*  Inputs: X = message # * 2  
;*
;* mesgv  - Override Y Position
;*  Inputs: X = message # * 2 
;*          A = Y position value
;*
;* msgent - Manual Text display (assumes position set)
;*  Inputs: vdata: Pointer to base of text string
;*          temp1: X Position
;*          temp2: Y Position
;*          temp4: X index to msg tables         
;*******************************************************
mzmesg  jsr stpg6
        ldx mazehint,Y      ;Get Hint Message - these are in mazed      
        bne mesg

?mesgv  sta temp2
        lda msgcsy+1,X          ;Y Position
        stx temp4
        jmp ?mesg_
        
mesg    jsr stpg0
        lda msgcsy+1,X          ;This is Y position
        stx temp4
        sta temp2
?mesg_  ldy temp4
        lda (litral,Y)          ;Get Literal ptr
        sta vdata
        iny 
        lda (litral,Y)
        sta vdata+1
        ;make sure ROM page is correct for TEXT data, Maze is on different page
        ; Page 0: All TEXT
        ; Page 7: Maze Hint TEXT
        ldx temp4
        cpx #mmzh1-1
        ifcs
            cpx #mmzhhw4
            ifcc
                ;maze text, change page
                jsr stpg7
            endif
        endif
        ldy #00
        lda (vdata,Y)              ;Get X position from literal
        sta temp1
        lda msgcsy+1,X             ;This is Y position
        sta temp2
        
        ;***********************************************************
        ;* For reference, the following variables contain
        ;* the following data, you can call msgent directly
        ;* to print text this way. Scaling is set to 2B/0L.
        ;* Use calls farther down to roll your own
        ;*
        ;*  vdata: (word) Pointer to Text
        ;*  temp1: X Position
        ;*  temp2: Y Position
        ;*  temp4: Pointer to base of text string
        ;***********************************************************
        
msgent  jsr vgcntr
?msgen2 lda #00
        sta vgbrit              ;black line for positioning
        lda #02
        jsr vgscalz              ;a=binary scaling value, linear scaling value = 0
        lda temp1
        ldx temp2
        jsr vgvtr1              ;Position Beam (Use vgbrit)
msgnop  ldy temp4               ;Set up ptr to literal
        lda (litral,Y)          ;Get X (horizontal) position
        sta vdata
        iny 
        lda (litral,Y)
        sta vdata+1
        ldx temp4
        lda msgcsy,X            ;Color value of text
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
        ldy #01             
mesgpe  ;this is only for THE PROMISED END text zooming call
        lda #00                 ;Init vglist offset
        sta temp1
        begin
            lda (vdata,Y)               ;Get next character
            sta temp2
            and #$7F                    ;Mask neg bit for end check
            tax 
            iny                         ;Next byte index
            sty temp3                   ;Save Y value
            lda vgmsga,X                ;Get correct vector JSRL from char index
            ldy temp1
            sta (vglist,Y)
            iny 
            lda vgmsga+1,X
            sta (vglist,Y)              ;Write it to VGRAM
            iny 
            sty temp1                   ;Save y
            ldy temp3                   ;Get character ptr
            bit temp2                   ;if not end of string
        miend
        ldy temp1
        dey 
        jmp vgadd
        
;* No vertical positioning 
;* This may not be used     
msgfol  stx temp4               ;Input: x = msg#*2
        sta temp1               ;A = x offset
        lda #00
        sta temp2
        beq ?msgen2             ;Always
        
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
        jsr ?msgfak
        jsr vgcntr
        lda #00
        ldx #$71
        jsr vgadd2
		VGADD_VCTRS(43,49,hidden)	
        ldx #mreac
        jsr ?msgfak
        rts

;Message to show on Homeworld
robonly ldx #mhwrobs
        jsr ?msgfak
        VGADD_VCTRL(-150,-60, hidden)
        ldx #mhwonly
        jmp ?msgfak
        
welvaxx ldx #mhwvaxx
        jmp ?msgfak
        
?msgfak stx temp4
        jmp msgnop
        
       

; Subroutines
.export mesg, mesgpe, header, robonly, welvaxx
;mesgv