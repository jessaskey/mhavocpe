;*****************************************************
    .TEXT ".TWVECTOR."
;*****************************************************
    .title "TWVECTOR - Vector Routines" 
    .sbttl "Vector List Sequencer"
;*****************************************************
;* This routine will be used to copy vectors from    *
;* ROM to VGRAM, 1 at a time, making the last      *
;* drawn vector brighter.                            *
;* Limits: Ths source pics must be in the fixed      *
;*         vector page. No more than 3 of these      *
;*         allowed.                                  *
;*                                                   *
;* Inputs: X       = display select (see list below) *
;*         seqx(x) = how far already in list         *
;*         seqst(x)= 80 if whole pic achieved        *
;*                   40 if last vec was long         *
;*                   20 if erase mode on             *
;*****************************************************
copypic ldy seqx                ;Current vector working on
		beq ?cpalw
        lda seqst               ;Do this one??
        ifpl                    ;Yep, not all there
            lda frame   
            and #01
            ifeq                ;Time to add another to list
?cpalw          iny                 
                lda (temp2,Y)           ;Get high byte of vector
                cmp #$C0                ;RTSL?
                ifeq
                    dec seqx                ;And skip this one next time
                    dec seqx
                    lda #$80
                    sta seqst               ;No more adds for this one
                endif
                jsr add2_b              ;Bump pointer by 2
                lda seqst
                and #$BF
                sta seqst               ;Guess short vector
                lda (temp2,Y)           ;Recall this opcode
                and #$E0                ;Look at opcode
                ifeq                    ;Must be long
                    jsr add2_b
                    lda seqst
                    ora #$40                ;Set Long Vector Bit
                    sta seqst
                endif
            ;Now point at next vector
            endif
        endif
        ldy #00             ;Start at start of list
        begin
            lda (temp2,Y)
            sta (vglist,Y)
            iny 
            cpy seqx            ;End of list
        csend
        dey 				;Fix Y to point at end again
		bit seqst               ;Skip bright if 'done'
		ifpl
			tya 
			pha                 	;Save this Y for later
			;Now point at last byte of opcode
			bit seqst               ;Long or short?
			ifvc                    ;Short
				dey
			endif
			lda (vglist,Y)          ;Get the vector 
			ora #$E0                ;make it bright
			sta (vglist,Y)          ;put it back
			pla                 	;restore old Y
			tay 
		endif

        jmp vgadd               ;Add Y to List
        
        
add2_b  lda seqx
        clc 
        adc #02                 ;Next Vector (Guess Short)
        sta seqx
        rts 
        
;*****************************************************
    .title "Common Vector Routines"
;*****************************************************
    
vgrtsl  lda #$C0
        ifeq
vghalt      lda #$20
        endif
        ldy #00
        sta (vglist,Y)
        jmp vgvctr2
        
vghexz  ifcs
            and #$0F
            beq ?vgj10
        endif
vghex   and #$0F
        clc 
        adc #01
?vgj10  php 
        asl a
        ldy #00
        tax 
        lda vgmsga,X
        sta (vglist,Y)
        lda vgmsga+1,X
        iny 
        sta (vglist,Y)
        jsr vgadd
        plp 
        rts
            
vgjmpl  lsr A
        and #$1F
        ora #$E0
vgjsr1  ldy #01
        sta (vglist,Y)
        dey
        txa
        ror A
        sta (vglist,Y)
        iny
        bne vgadd
vgjsrl  lsr A
        and #$1F
        ora #$A0
        bne vgjsr1
        ldy vgbrit
vgstat  ldx #$60
        begin
            tya
            jmp vgadd2
            ldx #$60
        eqend
        ;Fall Through
;****************************************************
; Add VGCENTER instruction to VG
vgcntr  lda #$40
        ldx #$80
        ;Fall Through
;****************************************************
; Push write two bytes to VGRAM at current pointer
; Location
; A = First Byte   X = Second Byte
vgadd2  ldy #00
        sta (vglist,Y)
        iny 
        txa 
        sta (vglist,Y)
        ;Fall Through
;****************************************************
; Update increment Vector Pointer Y+1
vgadd   tya 
        sec 
        adc vglist
        sta vglist
        ifcs
            inc vglist+1
        endif
        rts 
;*****************************************************
;* vgscalz - set scaling
;* a=binary scaling value, linear scaling value = 0 
vgscalz ldy   #$00
;*****************************************************
;* vgscal - set scaling
;* a=binary scaling value, y = linear scaling value 
vgscal  ora #$70
        tax 
        tya 
        jmp vgadd2
 
;****************************************************** 
;Draw vector utility methods
;        
; vgvtr5 - transparent vector
; vgvtr  - vector with brightness = Y
; vgvtr1 - vgbrit as is
;
; Common parameters:
;    A: Vector Y value (Signed value +/-127d max)
;    X: Vector X value (Signed value +/-127d max)
;******************************************************
vgvtr5  ldy #00
vgvtr   sty vgbrit
vgvtr1  ldy #00
        asl a
        ifcs
            dey
        endif
        sty vdata+1
        asl a
        rol vdata+1
        sta vdata
        txa 
        asl a
        ldy #00
        ifcs
            dey
        endif
        sty vdata+3
        asl a
        rol vdata+3
        sta vdata+2
        ;Fall Through
;************************************************
; Draw Vector as defined in vdata to vdata+3
vgvtr2  ldx #vdata   ;$27
        ;Fall Through
;************************************************
; Draw Vector as defined in address offset in X
vgvctr  ldy #00
        lda 2,X				;offset X+2
        sta (vglist,Y)
        lda 3,X				;offset X+3
        and #$1F
        iny 
        sta (vglist,Y)
        lda 0,X				;offset X+0
        iny 
        sta (vglist,Y)
        lda 1,X				;offset X+1
        eor vgbrit
        and #$1F
        eor vgbrit
vgvctr2 iny 
        sta (vglist,Y)
        bne vgadd
    