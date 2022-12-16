;********************************************
    .TEXT ".TWSELECT."
;********************************************
    .sbttl "SELECT-A-LEVEL"
;***********************************************
    .sbttl "Add-a-coin(and such) Display"         
;***********************************************
;*  Used to display add-a-coin messages, and   
;*  also used to set up for select-a-level for 
;*  player 2 start (player 1 at end of game too
;***********************************************
adddis  ldx #maddm2
        jsr mesg_rtp1
        lda sellvl
        clc 
        adc #01         ;Display select Level
        jsr decimal     ;Convert
        lda frame
        lsr A
        ora #$A0        ;Flash Color
        pha             ;Save for below
        ldx #$60
        jsr vgadd2
        lda #temp7
        sec 
        ldy #01
        jsr digits      ;Which level
        ldx #maddm1     ;Press start within...
        jsr mesg_rtp1
		VGADD_VCTRS(0,-80,hidden)	;Back up to time space
        pla 
        sta temp3           ;Save for select routine
        ldx #$60            ;Get back Flash color
        jsr vgadd2
        clc 
        lda addtim
        sta temp1
        lda #temp1      ;For digit display
        ldy #01
        jsr digits      ;Display Time
        lda frame
        and #$3F            ;Seconds (approx)
        ifeq
            sed 
            sec 
            lda addtim
            sbc #01
            sta addtim
            cld
            ifeq
                lda #00
                bit gamest          ;Game On???
                ifmi                ;If yes, this is player 2 select, not restart
                    sta chngplr         ;Just do same as pushing start
                else
					sta adden		;Disabled 
                    ;sta adddif
                    ;sta addmn       ;Clear these now
					sta add4mz
                    sta wrplvl      ;Start back at 0
                    lda #01
                    sta rampg
                    lda #00
                    sta wrplvl      ;Clear both players
                    lda player      ;Restore page to correct one
                    sta rampg
                endif
            endif
        endif
        ;Fall Through      
        ;***********************************************
            .sbttl "Select-a-Level Start"         
        ;***********************************************
        ;* Used to select a level up to the allowable  
        ;* Start level.                                
        ;*                                             
        ;* Temp3 = Flash color from main line          
        ;***********************************************
        ;* Assumption: This is only called if we can   
        ;*             start at more than just level 1 
        ;***********************************************
        jsr vgcntr
		VGADD_VCTRS(-64,-128,hidden)
		VGADD_VCTRS(0,-8,hidden)
        ; lda adddif      	;Allowable start level
        ; asl a
        ; asl a
; ;#IF (DEBUG = 1)
		; clc
; ;#ELSE
; ;        sec 
; ;#ENDIF
        ; adc addmn           ;Max level to start (+1)
		lda add4mz
        sta temp8           ;Save this
        tay             	;Save for compare
		lda #01         	;Guess start with 1
        ldy sellvl      	;Select level determines this
        cpy #08
        ifcs
            tya
            sbc #06         	;Start with amount greater than 7
        endif
        sta temp2           ;Number to start with
        lda #08
        sta temp2+1     ;Do 9 max
        begin           ;We are positioned at a place to draw number
            lda #($F0+colorange)	;Default Orange
            ldy temp2
			cpy #22d				;Skip Final Station intra-levels
			ifeq
?dblk			lda #colblack
				beq ?ddig
			endif
			cpy #23d
			beq ?dblk	
			cpy #24d
			beq ?dblk
            dey             		;It's 1+
            cpy sellvl      		;Number pointed to??
            ifeq
                lda temp3           	;Select Digit.. Flash
            endif
?ddig       ldx #$63            ;Flash Digit
            jsr vgadd2
			;Draw the level number now..
            lda temp2
			cmp #23d
			ifcs
				;Special Levels Change lables
				vgadd_jsrl(char_x)
				vgadd_jsrl(char_x)
			else
				jsr decimal     	;Convert
				lda #temp7
				sec
				ldy #01         	;Display This
				jsr digits
			endif
			VGADD_VCTRS(0,17,hidden)
            dec temp2+1     	;Do all Max??
            bmi ?sal10
            inc temp2
            lda temp8
            cmp temp2           ;Reached max??
        ccend
?sal10  lda temp3
        ldx #$63            ;Always flash arrow
        jsr vgadd2
        dec temp8
        jsr vgcntr
        lda sellvl      ;Which level selected
        cmp #08
        ifcs
            lda #07         ;Decrease for Offset
        endif
        asl a
        asl a
        asl a
        asl a
        pha             ;Save for double position
        clc 
        adc #-$7a           ;Offset to char at left
        ldx #-$58           ;Position for arrow
        jsr vgvtr5
        pla 
        sec 
        sbc #01         	;In case it is 80!
        ldx #00         	;No Y motion
        jsr vgvtr5      	;Complete Postion
        lda #00
        ldx #$71
        jsr vgadd2      	;Size of ship
        lda mazarw+4
        ldx mazarw+5
        jsr vgadd2
        ;Now Read in the Roller
        ldy #00
        jsr rgdr            ;Get and move data
        asl a
        asl a
        ifmi
            dey             	;Prop sign
        endif
        clc 
        adc wrpdl           ;Can use these here even tho they are not for this purpose
        sta wrpdl
        tya 
        adc sellvl      	;Move Level
        ifmi
            lda #00
        endif
        cmp temp8           ;Max side
        ifcs
            lda temp8
        endif
        sta sellvl
		;Special Case Checks
		lda rgdd
		ifne
			ifmi
				lda sellvl
				cmp #23d
				ifeq
					lda #20d
					sta sellvl
				endif
			else
				lda sellvl
				cmp #21d
				ifeq
					lda #24d
					sta sellvl
				endif
			endif
		endif
        rts
          