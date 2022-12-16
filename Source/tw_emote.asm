

;***********************************************************
    .TEXT ".TWEMOTE."
;***********************************************************
	.sbttl "Emote Moves"
;***********************************************************
; Routine to make Rex do a linear pic sequence based upon
; the roller direction
;***********************************************************
emomax  = temp1
emospd  = temp1+1
emotbl  = temp2

emote	;set up our params for below
		lda picseq
		and #$3					;only 4 emotes supported
		tax
		sta temp3				;save emote index here for below
		lda emoixmx,X			;get the max emote index
		sta emomax
		lda emospds,X
		sta emospd
		lda temp3
		asl A 					;words
		tax
		lda emotbls,X
		sta emotbl 
		lda emotbls+1,X
		sta emotbl+1
		ldy emoteix				;set the current pic based upon the current index
		lda (emotbl,Y)
		sta piccur
		;see if the roller has rolled enough to change, this will changed the index
		lda rgdd
		ifmi
			eor #$FF
		endif
		and #$18
		ifne
			lsr A 
			lsr A 
			;lsr A			;$18 to $03
			tax
			begin
				dec picdely
				dex
			miend
			lda picdely
			ifmi
				;we have enough to move to the next emote pic
				;reset picdely
				lda emospd
				sta picdely
				;which direction?
				bit rgdd
				ifmi
					;left, needs to go down
					ldy emoteix
					ifeq
						;will underflow
						ldx temp3
						lda emosty,X		;Which style (0=loop,1=stop)
						ifeq			
							ldy emomax			;set to max
						endif
					else
						dey
					endif
				else
					;right, needs to go up
					ldy emoteix
					cpy emomax 
					ifeq 
						ldx temp3
						lda emosty,X		;Which style
						ifeq
							ldy #0				;Loop
						endif
					else
						iny
					endif
				endif
				sty emoteix
				lda (emotbl,Y)
				sta piccur
			endif
		endif	
		;draw rex for emote
		ldy #$30        ;Linear Part
		lda #02         ;Scale 1 for position
        jsr vgscal      ;Set Scale
        jsr vgcntr      ;Center
        ldy #00
        sty vgbrit      ;Blank Vector
        ldx #xmot       ;Vector from xmot
        jsr vgvctr      ;Place this object		
		;Emotes always happen on planet surface, tiny scale
		ldy #$88		;Homeworld scale for rex
        lda #02         ;At Scale 2
        jsr vgscal      ;Add Scale
		
		;Color
		lda #mancol+$e0     ;Add color and intensity
		ldy player          ;Player 2??
		ifne                ;yes change color
			lda #mancol2+$a0       
		endif
		ldx #mpic_vpg
		jsr vgadd2          ;Add Page Select

		;draw rex now finally
		lda piccur
		asl a           ;Times 2 for index use
        tay 
        lda mansrc,Y        ;Get Source
        ldx mansrc+1,Y
        jsr vgadd2      ;Add man's pic

		rts

;Style for the emote, does it go in a loop or stop once it hits the end
emote_loop 	= 0
emote_stop 	= 1

emosty	.db emote_loop
		.db emote_stop

;Number of indexes for each sequence
emoixmx	.db flo_end-flo_str-1	
		.db dab_end-dab_str-1

emospds	.db 8d		;Speed
		.db 8d

emotbls	.dw flo_str
		.dw dab_str
		
dab_str	.db idx_pic21
		.db idx_dab02
		.db idx_dab04
		.db idx_dab06
dab_end

flo_str	.db idx_flossb      ;00:(flossa) floss Neutral     	   -> (flossb)         
        .db idx_flossc      ;01:(flossb) left both front 1     -> (flossc)
        .db idx_flossd      ;02:(flossc) left both front 2     -> (flossd)
        .db idx_flossc      ;03:(flossd) left both front 3     -> (flossc)
        .db idx_flossb      ;04:(flossc) left both front 2     -> (flossb)
        .db idx_flossa      ;05:(flossb) left both front 1     -> (flossa) neutral
        .db idx_flosse      ;06:(flossa) floss Neutral         -> (flosse) right, left behind
        .db idx_flossf      ;07:(flosse) right, left behind 1  -> (flossf)
        .db idx_flossg      ;08:(flossf) right, left behind 2  -> (flossg)
        .db idx_flossf      ;09:(flossg) right, left behind 3  -> (flossf)
        .db idx_flosse      ;10:(flossf) right, left behind 2  -> (flosse)
        .db idx_flossa      ;11:(flosse) right, left behind 1  -> (flossa) neutral
        .db idx_flossb      ;12:(flossa) floss neutral		   -> (flossb)
        .db idx_flossc      ;13:(flossb) left both front 1	   -> (flossc)
        .db idx_flossd      ;14:(flossc) left both front 2	   -> (flossd)
        .db idx_flossc      ;15:(flossd) left both front 3	   -> (flossc)
        .db idx_flossb      ;16:(flossc) left both front 2 	   -> (flossb)
        .db idx_flossa      ;17:(flossb) left both front 1	   -> (flossa)
        .db idx_flossh      ;18:(flossa) floss neutral		   -> (flossh)
        .db idx_flossi      ;19:(flossh) right both front 1	   -> (flossi)
        .db idx_flossj      ;20:(flossi) right both front 2    -> (flossj)
        .db idx_flossi      ;21:(flossj) right both front 3	   -> (flossi)
        .db idx_flossh      ;22:(flossi) right both front 2	   -> (flossh)
        .db idx_flossa      ;23:(flossh) right both front 1    -> (flossa)
        .db idx_flossk      ;24:(flossa) floss neutral         -> (flossk)
        .db idx_flossl      ;25:(flossk) left, right behind 1  -> (flossl)
        .db idx_flossm      ;26:(flossl) left, right behind 2  -> (flossm)
        .db idx_flossl      ;27:(flossm) left, right behind 3  -> (flossl)
        .db idx_flossk      ;28:(flossl) left, right behind 2  -> (flossk)
        .db idx_flossa      ;29:(flossk) left, right behind 1  -> (flossa)
		.db idx_flossh      ;30:(flossa) floss neutral         -> (flossh)
		.db idx_flossi      ;31:(flossh) right both front 1	   -> (flossi)
        .db idx_flossj      ;32:(flossi) right both front 2    -> (flossj)
        .db idx_flossi      ;33:(flossj) right both front 3    -> (flossi)
		.db idx_flossh      ;34:(flossi) right both front 2    -> (flossh)
flo_end
