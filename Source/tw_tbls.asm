;********************************************
    .TEXT ".TWTBLS."
;********************************************
    .title "TWTBLS - Table to list scores!"
;********************************************
    .sbttl "Get Initial for Table"
;********************************************

;* Entry only if updflg is minus!
geths   lda updcur			;Are we doing top 3... if so, then show some fireworks too!
		cmp #03
		ifcc
			lda frame+1
			cmp #$EE
			ifeq
				clc					;set to launch
				jsr fanfare
				rts
			endif
			cmp #$EF
			ifeq
				;fireworks have been running for one major frame, now
				;go to actual high score screen
				lda #snd_hsc
				jsr dosnd2
				lda #$F0
				sta frame+1		;Force to HS table now
			endif
			jsr fanfare
		endif
		ldx #mplayr         ;Player Message
        jsr mesg          	;Message
        ldx updwho          ;0 or 1
		jsr	plrsel			;Select and save X
        inx 
        inx                 ;1 or 2 now
        txa 
        asl A               ;2 or 4 now
        tay                 ;Y for vgchar
        JSR_VGCHARY()       ;2 or 4 = 1 or 2 output to screen
        ldx #mgtsc          ;Great Score
        jsr mesg
        ldx #menin
        jsr mesg          	;Enter your initials
        lda updcur          ;Entry where this inital will go
        clc 
        adc updint          ;Initial working on
        sta temp7           ;Save index
        tax 
        ldy #00
        jsr rgdr            ;Use Rolly Gig to change data   
        asl A
        asl A
        ifmi
            dey
        endif
        clc 
        adc wrpdl           ;Use this byte, not being used
        sta wrpdl               
        tya 
        adc initl,X         ;Change Initial
        cmp #$26
        ifcs
            lda #$0A            ;Wrap back about
        endif
        cmp #$0A
        ifcc
            lda #$25
        endif
        sta initl,X
        bit jblast          ;Last button on or off?
        ifmi                ;Was down, wait for up
            bit jbstat
            ifpl
                cmp #$25                ;Back space?
                ifeq                    ;yep!
                    ldy updint          ;Make sure not doing 0
                    ifne                    ;nope!
                        dec updint
                        lda #09
                        sta initl,X         ;Set current to space
                    endif
                else
                    inc updint          ;Next Initial
                    lda updint
                    cmp #03
                    ifeq                    ;Last Entry
                        lda #00
                        sta updint          ;Got last, Entry over!!
                        sta updflg
                        sta frame+1         ;Clear frame entry
                        lda updcur          ;Where entered in this table
                        cmp #(3*3)          ;In Top 3?
                        ifcc                    ;Yes, okay to write into EEROM
                            lda #-1
                            sta wrhflg          ;Set flag to write
                        endif
                        ldy #02             ;Move 3 initials
                        sty temp4
                        begin
                            lda initl,X
                            ldy updwho          ;Where to save
                            ifeq
                                ldy temp4
                                sta pl1last,Y
                            else
                                ldy temp4
                                sta pl2last,Y
                            endif
                            dex                 ;Go back trough last letters entered
                            dec temp4
                        miend
                    else                    ;Not last initial entry
                        ldx updint          ;Which initial (1 or 2?)
                        lda pl1last,X           ;Guess player 1
                        ldy updwho          ;Could be player 2
                        ifne
                            lda pl2last,X           ;Is player 2
                        endif
                        ldy temp7               ;Storage index
                        iny                 ;Will do next
                        sta initl,Y         ;Set next initial
                    endif
                endif
            endif
        endif
        lda jbstat
        sta jblast
        jmp ?nohs           ;When doing initials, don't show the 'high score' message
		;* Fall Through *
		;***************************************************
			.sbttl "Display High Scores"
		;***************************************************
hstbl   ldx #mhis              	;High Scores
        jsr mesg              
		
?nohs   jsr vgcntr
        vgadd_scale(ywin_off,binscal2,0)
        lda #$C0+colyellow
        sta temp8               ;Color to restore to after a flash color
        ldx #$60                ;Green for Table
        jsr vgadd2              ;Add stat
        lda #09
        sta temp9               ;Show 10d scores
        lda #$10
        sta temp3+1             ;Starting Y for each line
        lda #00
        sta temp4               ;Index into high score table
		sta temp4+1
        sta temp6               ;Index for initials
        lda #01
        sta temp3
        begin
            jsr vgcntr
            lda temp9               ;Change color after first 3 scores
            cmp #07                 ;Different color??
            ifcc                    ;Yes, bottom are a different color
                lda #$F0+colcyan        ;In cyan
                sta temp8               ;Save for possible flash
            endif
			lda temp8
			ldx #$60
            jsr vgadd2          ;Change to appropriate color
            lda #-$40
            ldx temp3+1         ;Position of this line
            jsr vgvtr5
            bit temp9+1         ;Need to set back to green after flash??
            ifmi
                lda temp8               ;Recall last color
                ldx #$60
                jsr vgadd2
                lda #00
                sta temp9+1         	;Clear flag
            endif
            lda temp6
            cmp updcur          ;Need to flash this one?
            ifeq
                lda frame
                asl A
                ora #$F3            ;Flash color
                sta temp9+1         ;Set flag for setting back to green
                ldx #$60
                jsr vgadd2          ;Add Flash Color
            endif
            lda #temp3
            sec 
            ldy #01
            jsr digits          ;Put out place temp3
            JSR_VGCHAR(idxchar_period)	;Put a period
            JSR_VGCHAR(idxchar_space) ;Put out a space
            ldy temp4
            ldx #-4             ;(Zero page will wrap about)
            begin
                lda hscore,Y            ;Copy to zero page for display
                sta vdata+4,X           ;A good place to put them
                iny
                inx
            plend               ;Move all 3
            lda #vdata
            sec 
            ldy #04
            jsr digits          ;Display them
            JSR_VGCHAR(idxchar_space)  ;Add a space
            jsr inital
            jsr inital
            jsr inital          ;Write out 3 initials
			jsr outtesser
			jsr outbadge
            lda temp3+1
            sec 
            sbc #$0C            ;Get next starting place
            sta temp3+1
            inc temp4
            inc temp4
            inc temp4
            inc temp4           ;Next index
			inc temp4+1
            lda temp3
            sed 
            clc 
            adc #01
            cld 
            sta temp3           ;Next line number
            dec temp9           ;Another one done
        miend
		rts
		
;***************************************
; Fireworks code from Space Duel
;
; If Carry Flag is clear, then enable 
; launching of new fireworks otherwise, 
; no new launches.
;***************************************
numfirew = $10		

?localpc = $
	.org nmmotobj	

;vars for fireworks
fwstat	.block	numfirew
fwxh	.block	numfirew
fwxl	.block 	numfirew
fwyh	.block 	numfirew
fwyl	.block 	numfirew
fwtemp	.block	1

	.org ?localpc

fanfare	ifcc
			lda frame+1			
			cmp #$F1		;Don't launch after High Scores are showing for a sec
			iflt
				lda frame
				and #$0F
				ifeq
					jsr ?launch
				endif
			endif
		endif
		jsr ?updfan
		
		ldx #numfirew-1	
		stx fwtemp
		begin
			ldx fwtemp
			lda fwstat,X
			ifne
				jsr vgcntr
				lda #02					;Scale for positioning is always 2
				jsr vgscalz             ;a=binary scaling value, linear scaling value = 0
		
				ldx fwtemp
				lda fwxh,X
				tay
				lda fwyh,X
				tax
				tya
				jsr _vgvtr5
				
				ldx fwtemp
				lda fwstat,X
				cmp #$78
				iflt
					cmp #$7F
					bcs ?exp
					;vstats
					lda #($F0+colwhite)
					ldx #flare_vpg
					jsr vgadd2
					laljsr(flare)
					lxhjsr(flare)
					jsr vgadd2
				else
?exp				lda fwtemp			;Get back index (which is color basis)
					and #07
					ifeq				;NO BLACK
						lda #colpink		;DEFAULT RED
					endif
					ora #$F0			;Full brightness
					ldx #shtex_vpg
					jsr vgadd2
					;Get back original value
					ldx fwtemp
					lda fwstat,X
					and #07				;Only this many animation steps
					asl A				;Words
					tay
					lda shtexpnc,Y
					ldx shtexpnc+1,Y								
					jsr vgadd2			;Add JSRL
				endif
			endif	
			dec fwtemp
		miend
		rts

;special local version which does not use vdata, uses maze data base @ ltcol
_vgvtr5 ldy #00
		sty vgbrit
		ldy #00
        asl a
        ifcs
            dey
        endif
        sty ltcol+1
        asl a
        rol ltcol+1
        sta ltcol
        txa 
        asl a
        ldy #00
        ifcs
            dey
        endif
        sty ltcol+3
        asl a
        rol ltcol+3
        sta ltcol+2
		ldx #ltcol
		jmp vgvctr


;BELLS AND WHISTLES
?launch	
		ldx lastfire
		lda fwstat,X		;DONE?
		ifeq				;YEP
			lda rands
			and #$3F
			adc #$38
			sta fwstat,X		;Save distance traveled up
			lda rands+1
			sta fwxh,X		;Starting X position
			lda rands+2
			sta fwxl,X
			lda rands+3
			sta fwyl,X
			lda #$80
			sta fwyh,X
			lda #0
			sta velxl+1,X			;NO X MOTION
			;sta velyh+1,X			;NO Y MSB MOTION
			lda rands+4
			and #$03			;RANDOM VELOCITY 
			adc #$03			;Min Velocity
			sta velyh+1,X
		endif
		inx
		cpx #numfirew-1
		ifcs 
			ldx #0			;START OVER
		endif
		stx lastfire
        rts
		
?updfan	ldx #numfirew-1
		begin
			lda fwstat,X
			ifne
				ifpl
					clc
					lda velyh+1,X
					adc fwyh,X
					sta fwyh,X
					;Is it ready to explode?
					ifpl
						cmp fwstat,X		;OBJST HOLDS DEST Y
						ifne
							ifge
								lda #0
								sta velyh+1,X		;STOP MOTION
								lda #$90
								sta fwstat,X		;EXPLODE
								txa
								lsr A
								ifcs
									lda	#snd_e3
								else
									lda	#snd_e4
								endif
								jsr dosnd2		;Always
								jsr dodelay 	;just in case another explosion comes soon
							endif
						endif
					endif
				else
					inc fwstat,X
					lda fwstat,X		;Explosion increment
					cmp #$97
					ifge
						lda #0
						sta fwstat,X
					endif
				endif
			endif
			dex
		miend
		rts	
		
resfirew
		ldy #numfirew-1
		lda #0
		begin
			sta fwstat,Y
			dey
		miend
		lda #0
		sta lastfire
		rts
		
;********************************************
    .sbttl "Check for High Score"
;********************************************
hscend  =   36d     ;Top of high scores
hinend  =   27d     ;Top of initials
    
update
		lda #-1
        sta updcur      ;Will guess none available
        ldx #04
        lda player      ;Player 2
        ifeq
            ldx #00         ;Nope, player 1
        endif
        stx temp4
        ldy #00
        sty temp3           ;Place to save initial index
        sty frame
        sty frame+1         ;Clear in case no high score, select-a-level
upda20  ldx temp4           ;Which score to check
        begin
            lda hscore,Y
            cmp score,X
            lda hscore+1,Y
            sbc score+1,X
            lda hscore+2,Y
            sbc score+2,X
            lda hscore+3,Y
            sbc score+3,X
            bcc upda30          ;Looks like we have a WINNER, do it now..
            inc temp3
            inc temp3
            inc temp3           ;Keep an index for initals also
            iny 
            iny 
            iny 
            iny                 ;check next
            cpy #hscend+4       ;Past last one??
        csend
        rts 
        
upda30  stx temp2               ;Save which player?
        sty temp2+1             ;Save score index
        ldy temp3               ;Which inital index
        sty updcur              ;Save which entry
?upd35  ldy #hscend             ;Need to move everybody down
        ldx #hinend             ;High initial end
        begin
            cpx updcur          ;Loop until done or end
            beq ?upd45          ;Done, get out
            lda initl-3,X
            sta initl,X
            lda initl-2,X
            sta initl+1,X
            lda initl-1,X
            sta initl+2,X
            lda hscore-4,Y
            sta hscore,Y
            lda hscore-3,Y
            sta hscore+1,Y
            lda hscore-2,Y
            sta hscore+2,Y
            lda hscore-1,Y
            sta hscore+3,Y
            dex 
            dex 
            dex 
            dey 
            dey 
            dey 
            dey 
        eqend                   ;Loop until end or done
?upd45  ;* Set up for new initials
        ldy temp4               ;Get 0 or 4
        ifne                    ;We need 0 or 3 here
            dey
        endif
        lda pl1last,Y           ;Letter to start with
        sta initl,X             ;Start letters at A
        lda #00
        sta initl+1,X
        sta initl+2,X
		sta frame			;reset LSB too
        lda #$EE			;EE will allow one major frame (4 sec) 
							;of Fireworks and then 16 major frames of initial time
        sta frame+1         ;64 seconds at 60Hz (Timeout)
        ldx temp2           ;Current Score
        ldy temp2+1         ;Where it goes
        lda score+3,X
        sta hscore+3,Y
        lda score+2,X
        sta hscore+2,Y
        lda score+1,X
        sta hscore+1,Y
        lda score,X
        sta hscore,Y        ;Move in new score
		tya					;Fix index tokens are straight up
		lsr A 
		lsr A
		tay
		lda tokpock
		sta gtesser,Y
        lda player          ;Save who this is
        sta updwho          ;Save who for display
        lda #-1
        sta updflg          ;Set flag for update
        lda #00
        sta updint          ;Start with first initial
        sta jblast          ;Fake last pushed was not
        jsr dostop          ;Stop allother songs
		jsr resfirew		;Reset fireworks always, even if not being done
		lda updcur			;If not top 3, then start the music here, otherwise music will start up top later
		cmp #03
		ifcs
			jsr dodelay         ;And wait before doing this
			lda #snd_hsc
			jsr dosound
		endif
		rts
        
;********************************************
    .sbttl "Output Initial"
;********************************************
;* Initial Display Initial at current place *
;* on screen.                               *
;*                                          *
;* Entry:   Y = idex into initl table       *
;********************************************       
inital  ldy temp6
        inc temp6               ;Why?
        lda initl,Y             ;Get this initial
        cmp #$39                ;Bad indicator?
        ifne                    ;Nope
            cmp #idxchar_excla     ;Back space?
            ifeq
                lda #idxchar_ltri       ;Left Pointing Arrow
                bne ?oi11               ;Skip rest of check
            endif
            cmp #(idxchar_a)        ;Below letter A??
            bcc ?oi10               ;Yep... turn to blank
            cmp #idxchar_excla
            ifcs                    ;Too high, bad char
?oi10           lda #00             ;Restore with a space
            endif
        endif
?oi11   asl A               ;*2 for words
        tay                 ;For vgchar
        JSR_VGCHARY()
		rts
          

;*********************
; EXTERNALS
;*********************	
.export geths,hstbl,update,resfirew