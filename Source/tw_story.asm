;**********************************************
    .TEXT ".TWSTORY."
;**********************************************
    .sbttl "Scoring and Details"
;***********************************************
scormsg lda strtst          ;Story status on??
        ifmi                    ;yep!
            lda #$40
			sta tempa			;line spacing
			jsr	?setupln
            ldx #06             ;Will do 6 lines
            stx temp9
            lda strtln          ;First line to output
            cmp #msco27+1        ;Done all?
            ifcs
				;Scoring Summary is done... do backstory
				lda #mtext0
				sta strtln
				lda strtst 
				ora #$40			;Change story modes    		
                sta strtst    		;Clear status
                lda #-2
                sta strtyl
                sta strtyh			;Reset scroll
                rts                 ;Return
            endif
            sta temp8
            begin
                ldx temp8
                cpx #msco27+1           ;All on screen??
                bcs ?done1
                jsr ?drawwmsg            ;Do this line
                lda temp8
                tax 
                lda messypos,X         ;YPos = Picture with this one??
                ifne                    ;yep
                    jsr dopic               ;Do this pic
                endif
                jsr ?nextline
            miend
        endif
?done1  rts

?setupln	
		lda #$00
		sec 
		adc strtyl
		tay                 ;Save LSB   
		lda strtyh
		adc #$00
		ifpl                    ;This line is gone
			inc strtln
			tax                 ;Save this too  
			tya 
			sec 
			sbc tempa           ;Line spacing set from caller
			tay
			txa
			sbc #$00
		endif
		sta strtyh
		sta vdata+3         ;Will be Y MSB
		sty strtyl
		sty vdata+2         ;Is LSB
		rts

?setupln2	
		lda #$00
		sec 
		adc strtyl
		tay                 ;Save LSB   
		lda strtyh
		adc #$00
		ifpl
			cmp #01				;Is it in the 1 zone
			ifcs                    
				ldx strtyl
				cpx #$70			;Check msb now, is it above this too?
				ifcs				;This line is gone
					inc strtln
					tax                 ;Save this too  
					tya 
					sec 
					sbc tempa           ;Line spacing set from caller
					tay
					txa
					sbc #$00
				endif
			endif
		endif
		sta strtyh
		sta vdata+3         ;Will be Y MSB
		sty strtyl
		sty vdata+2         ;Is LSB
		rts


?nextline
		inc temp8           ;To next line
		lda vdata+2         ;Move down some space
		sec
		sbc tempa			;line spacing
		sta vdata+2
		ifcc
			dec vdata+3         ;To next line
		endif
		dec temp9               ;Any more lines?
		rts

        
;***********************************************
;* Will output message to specified location   
;* on screen.       
;*
;* X contains message index, also saved in temp4
;* 
;***********************************************
?drawwmsg    
		stx temp4
        lda messptrl,X
        sta temp5
        lda messptrh,X
        sta temp5+1         ;Set literal pointer
        ldy #$00
        sty vgbrit          ;Position dark
        lda messxpos,X      ;Get H position 
        sta vdata           ;X LSB/4 position
        lda #-1
        sta vdata+1
        asl vdata
        rol vdata+1         ;Multiply by 4
        asl vdata
        rol vdata+1         ;X position
        lda #$02
        jsr vgscalz         ;Set position scale
        jsr vgcntr
        ldx #vdata
        jsr vgvctr          ;Position
        ldx temp4
        lda messscol,X
        pha 
        lsr A
        lsr A
        lsr A
        lsr A
        ora #$F0
        tay 
        lda #$00
        jsr vgstat          ;Set color
        pla 
        and #$0F
        clc 
        adc #$01
        ldy #$5C
        jsr vgscal          ;Set scale
        ldy #$00
        lda #$00            ;C init vglist offset
        sta temp1
        begin
            lda (temp5,Y)       ;C Get character representation
            sta temp2
            and #$7F
            iny 
            sty temp3           ;Save Y
            tax 
            lda vgmsga,X        ;C Get correct JSRL
            ; bit foreign         ;D Want 'foreign' letters?
            ; ifmi
                ; lda formsg,X    ;C Get alternate char set
            ; endif
            ldy temp1
            sta (vglist,Y)
            iny 
            lda vgmsga+1,X
            ; bit foreign         ;D Want 'foreign' letters?
            ; ifmi
                ; lda formsg+1,X          ;C Get alternate char set
            ; endif
            sta (vglist,Y)
            iny 
            sty temp1               ;Save Y
            ldy temp3               ;C Get character ptr
            bit temp2               ;D If not end of string
        miend
        ldy temp1               ;C Update List
        dey 
        jmp vgadd

		
;*********************************************
    .sbttl "Picture Draw Routines"
;*********************************************  
picjsr  .word picfish-1       ;Dummy call
        .word picfish-1
        .word picfight-1
        .word picmaz-1
        .word picfire-1
        .word picperk-1
        .word picmax-1
        .word picreac-1
        .word pixoxy-1
        .word picoxy2-1

picjse  ;End of table


dopic   asl A
        tax                 ;Get the routine
        cpx #picjse-picjsr      ;Off end of table??
        ifcc                    ;nope!
            lda picjsr+1,X
            pha
            lda picjsr,X
            pha
        endif
        rts
        
;**********************************************
    .sbttl "Pic - Fish Bombs"
;********************************************** 
picfish   
        lda #$40
        ldx #$73
        jsr vgadd2          ;Scale
        lda #$F7
        ldx #$63                ;Stat instruction
        jsr vgadd2
        lda cerpup
        ldx cerpup+1
        jsr vgadd2
        lda #$F4
        ldx #$63
        jsr vgadd2
        lda frame
        and #06
        tay 
        lda cerwng,Y
        ldx cerwng+1,Y          ;Add wings
        jmp vgadd2
        
;**********************************************
    .sbttl "Pic - Fighters"
;********************************************** 
picfight
        lda #00
        ldx #$74
        jsr vgadd2          	;Scale
        lda #($F0+colgreen) 	;Color
        ldx #enm_vpg			;Stat for fighter
        jsr vgadd2
        lda frame
        lsr A
        lsr A
        and #$0F
        tax 
        lda fightlist,X         ;Get the pic
        tay 
        lda enemys,Y
        ldx enemys+1,Y          ;Get enemy pic
        jmp vgadd2
        
fightlist   .byte 0,2,4,6,8,$A,$C,$E,$E,$C,$A,8,6,4,2,0

;**********************************************
    .sbttl "Pic - Spinner"
;**********************************************
picmaz  lda #$40
        ldx #$74
        jsr vgadd2
        lda #$F3
        ldx #spinnr_vpg
        jsr vgadd2
        lda frame
        lsr A
        lsr A
        and #06
        tay 
        lda spinlist,Y
        ldx spinlist+1,Y
        jmp vgadd2
    
;*** VROM Table for Spinners *** 
spinlist    jsrl(spinner0)
            jsrl(spinner1)
            jsrl(spinner2)
            jsrl(spinner3)

;**********************************************
    .sbttl "Pic - Fireballs"
;**********************************************
picfire lda #$F7
        ldx #$60                ;Draw a fire ball
        jsr vgadd2
        lda #$40
        ldx #$72
        jsr vgadd2
        laljsr(sparkb)
        lxhjsr(sparkb)          ;Draw the fire ball 
        jmp vgadd2
        
;**********************************************
    .sbttl "Pic - Robots"
;**********************************************
picperk lda #$40
        ldx #$72
        jsr vgadd2
        lda #robcol+$E0
        ldx #body_vpg          ;Stat and color
        jsr vgadd2
        laljsr(body)
        lxhjsr(body)
        jsr vgadd2          ;Output body
        lda frame
        lsr A
        lsr A
        and #$0F
        ifeq
            lda #01             ;Don't let go 0
        endif
        sta temp3               ;Save for below
        cmp #08
        ifcs
            lda #08
        endif
        asl A
        tay 
        lda heads-2,Y
        ldx heads-2+1,Y         ;Put a head on it
        jsr vgadd2
        lda tails
        ldx tails+1
        jsr vgadd2          ;Add a tail
        lda #mancol2+$E0
        ldx #body_vpg
        jsr vgadd2          ;Color for gun
        lda temp3               ;Recall status
        sec 
        sbc #04
        ifpl
            cmp #04
            ifcs
                lda #03
            endif
            asl A
            tay
            lda guns,Y
            ldx guns+1,Y
            jsr vgadd2
        endif
        lda temp3
        cmp #03
        ifcs
            lda #02
        endif
        asl A
        tay 
        lda eyes-2,Y
        ldx eyes-2+1,Y
        jmp vgadd2
        
;**********************************************
    .sbttl "Pic - Reactoid"
;**********************************************
picreac lda #00
        ldx #$72
        jsr vgadd2
        lda #$AA
        ldx #body_vpg+xflip
        jsr vgadd2
        VGADD_JSR(body)
        lda #$AA
        ldx #rods_vpg+xflip
        jsr vgadd2
        lda rods
        ldx rods+1
        jsr vgadd2
        lda #$F6
        ldx #$60
        jsr vgadd2
        lda #00
        ldx #$73
        jsr vgadd2
        laljsr(sparkb)
        lxhjsr(sparkb)
        jmp vgadd2

;**********************************************
    .sbttl "Pic - Oxoids"
;**********************************************
pixoxy  lda #$F5
?pico2  ldx #shld_vpg
        jsr vgadd2
        lda #00
        ldx #$75
        jsr vgadd2
        VGADD_JSR(shield)
        lda #00
        ldx #$73
        jsr vgadd2
        lda #$D0
        ldx #$1F
        jsr vgadd2              ;1st half of a vector
        lda #$20
        ldx #00                 ;Second half of vector
        jsr vgadd2
        lda vgjch2              ;Get JSRL into 2
        ldx vgjch2+1             
        ;lda vgmsga+6
        ;ldx vgmsga+7
        jmp vgadd2
        
;**********************************************
    .sbttl "Pic - Oxoids after Reactor"
;**********************************************
picoxy2 lda #$F6
        jmp ?pico2
        
;**********************************************
    .sbttl "Pic - Maxoid"
;**********************************************        
picmax  
        lda #colred+$E0
        ldx #maxrob_vpg          ;Stat and color
        jsr vgadd2
        VGADD_JSR(maxbody0)
        VGADD_JSR(maxhead0)
        lda frame
        asl A
        asl A
        asl A
        ifmi                    ;positive number in frame
            eor #$FF 
        endif                               
        and #$70
        ora #$80 + colyellow
        ldx #maxrob_vpg         ;Stat and color
        jsr vgadd2
        laljsr(maxeye0)
        lxhjsr(maxeye0)
        jmp vgadd2
   
    
;*******************************************
    .sbttl "Backstory"
;*******************************************
backstory   
		lda strtst          ;Story status on??
        ifmi
			lda #$80
			sta tempa			;line spacing
			jsr	?setupln
			ldx #$03            ;Will do 4 lines
			stx temp9
			lda strtln          ;First line to output
			cmp #mtextend   	;Done all??
			ifcs
				lda strtst
#IF FORCE_DESIGNERS != 0
				and #0
#ELSE
				and #03
#ENDIF
				ifeq
					lda #mcretx00		;Set to first message for designers
					sta strtln
					lda strtst
					ora #$20			;Show designers please    
				else
					;clear story status, leave counter intact
					lda strtst
					and #$0F
				endif
				sta strtst 			;store new status        
				rts                 ;Exit Please
			endif
			sta temp8
			begin
				ldx temp8
				cpx #mtextend+1     	;include the actual last line so +1
				bcs ?done2
				;do replacement texts
				;first for coin mode
				cpx #mtext18
				ifeq
					ldy slives          ;Save this for check
					lda c_cmode         ;Which coin mode??
					ifeq
						ldx #mtxtcmfr                ;Do free play message
					endif
				endif
				;second, if not doing designer credits, then do end of text message last
				lda strtst
				and #$03
				ifne
					cpx #mtextend
					ifeq
						ldx #mtexteot
					endif
				endif
				;*** All others default to message 24, Insert Coin Message
				jsr ?drawwmsg                ;Do this line
				jsr ?nextline
			miend
		endif
?done2	rts   

;*******************************************
    .sbttl "Designers"
;*******************************************
storydes   
		lda strtst          ;Story status on??
        ifmi
			lda #$40
			sta tempa			;line spacing
			jsr	?setupln
			ldx #$06            ;Will do 6 lines
			stx temp9
			lda strtln          ;First line to output
			cmp #mcretxend     	;Done all (+2 delays the exit by two lines)
			ifge
				lda strtst
				and #$0F
				sta strtst          ;Dbne.. change status, keep counter
				rts                 ;Exit please!
			endif
			sta temp8
			begin
				ldx temp8
				cpx #mcretxend+1			;include the actual last line so +1
				bcs ?done3
				cpx #mcretxend
				ifeq
					ldx #mtexteot
				endif
				jsr ?drawwmsg                ;Do this line
				jsr ?nextline
			miend
		endif
?done3	rts

;*******************************************
    .sbttl "End Story"
;*******************************************
endstory   
		lda #$40
		sta tempa			;line spacing
		jsr	?setupln2
		ldx #$08           	;Will do 8 lines
		stx temp9
		lda strtln          ;First line to output
		cmp #mendtxxx     	;Done all??
		ifcs
            lda #0
            sta shpscl
            lda #$84
            sta shipxl      ;Postion Ship for new tact
            lda #$04
            sta shipxh
            lda #$24
            sta shipyl      ;Place Y Position
            lda #$08
            sta shipyh 
            
			;lda vxstat
			;ora #$10
			lda #0
			sta objst			;Kill off Rex finally I think
			sta vxstat			;Mark closing sequence completed
			sta strtst          ;Change status
			sta isfinal			;End final sequence
			sta sellvl			;Restrt on Level 1
			jmp thisone			;Init Tact and maze
			;Short Exit - must be a jmp above because the routine above will change ROM page
			;             so we cannot come back here.
		endif
		cmp #mendtxxx-12
		ifcs
			cmp #mendtxxx-3     ;End fireworks launching a little early
			jsr fanfare			;Launching of new fireworks is dependent on the Carry Flag
								; Unset = Launch, Set = NoLaunch
		endif
		lda strtln
		sta temp8
		begin
			ldx temp8
			cpx #mendtxxx+1
			beq ?done4
			jsr ?drawwmsg                ;Do this line
			jsr ?nextline
		miend
?done4	rts
		
.export scormsg,backstory,storydes,endstory