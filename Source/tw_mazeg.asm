;************************************************
    .TEXT ".TWMAZEG."
;************************************************
    .title "TWMAZE - Maze Routines"
    .sbttl "Globals"

scbinit ldx #scoin1x-scoin1      ;Init score buffer data
        begin
            lda scoin1,X
            sta scobuf,X
			lda scoin2,X
            sta scobuf2,X           ;Player 2 score buffer
            cpx #retint-retini+1    ;Score area
            ifcc                    ;Time to do it!!
                lda retini,X
                sta retbuf,X            ;Reactor time buffer
                lda oxyini,X
                sta oxybuf,X            ;Oxygen counter buffer
            endif
            dex
        miend
        ;lda #mancol2+$a0        ;Init Score headings
        ;sta scobuf2+2           ;Change color on player 2
        ;lda #$10        
        ;sta scobuf2+8           ;Position slightly different
        ;lda #01
        ;sta scobuf2+9           ;To right of screen
init2   bit gamest              ;Attract??
        ifpl                    ;No, wrong aftershave
            jsr resframe            ;Clear Frame
        endif
		;***********************************************
		;* Fall Through
		;***********************************************
			.sbttl "Game Play Start Init!"
		;***********************************************        
gminit  lda #$50
        sta bonusa          ;Start with 5000 bonus
        lda #$18
        sta nxtdly          ;Wait until first launch
        lda #$C0
        sta olmznm          ;So it will start new
        sta target          ;Shots target
        sta tactde          ;Want tact first time
        lda #01
        sta nxtptr          ;First entry again!
        jsr newbrk          ;Restore breakout bricks
        lda #00             
		sta bronce          ;Clear breakout data
		lda #00
        ;sta colcnt          ;Reset collision count
        sta statst          ;Make sure station is off
        sta lauen               
        sta holmz           ;Don't hold old objects
        sta tact            ;Make sure display is off
        sta maznum          ;0 = sphere
		sta maztype
        sta rearview        ;Stars forward please
        sta tstat           ;Restart tunnel first time only
        sta difcty          ;And start difcty
        sta scbdis+3        ;Clear score display
        sta updflg
        sta updint          ;Clear table
		sta toktarg
		sta tokretr
		sta tokpock
		sta vxstat
		sta vxcount
        ldx #nmmotobj-1		;Motion objects
        begin
            sta sobjst,X        ;Clear old enemy shots
            sta objst,X         ;Clear old ship shots
            ;* And all other objects as well
            cpx #hitpts-scrbuf
            ifcc
                bit gamest          ;Playing??
                ifpl                    ;Do only if new game
                    sta scrbuf,X            ;Clear old scores
                endif
            endif
            dex
        miend                   ;Clear score and score buffers
        lda #$32
        sta tcount          ;Next tunnel init
		lda #00
		sta tcolsq			;Sequence counter for tube
        lda #-3             ;For man running to ship
        sta xmot+3          ;Also man's initial Y MSB
        lda #-4
        sta xmot+1          ;Start at Left (X MSB)
        lda #$90
        sta xmot            ;X LSB
        lda #$6A
        sta xmot+2          ;Man's Y LSB
        lda #idx_pic27      ;Standing with arms crossed
        sta piccur
        lda #$40
        sta mzgame          ;Start in tube_vpg
        sta mzgms           ;Save shadow too!
		lda #-1
		sta breakx			;Reset breakout complete counter
        jmp initshp         ;Do initshp and end
 
;************************************************
;* Vector Buffer Init Data                         
;************************************************
;Initial Score Buffer Data
scoin1  ;vscal(ywin_off,2,$50)		
		vstat(sparkle_off,xflip_off,live_vpg,$e,mancol)				;.db mancol+$e0,live_vpg
		vcntr	
		vctrl(-704,608,hidden)	
		;.db $60,$02
		;.db $40,$1D
		rtsl				
scoin1x

scoin2  ;vscal(ywin_off,2,$50)		
		vstat(sparkle_off,xflip_off,live_vpg,$a,mancol2)				;.db mancol+$e0,live_vpg
		vcntr	
		vctrl(272,608,hidden)	  ;X:272 Y:608
		rtsl				
scoin2x

;Initial Reactor Counter Buffer Data
retini  vscal(ywin_off,1,$40)		
		vstat(sparkle_off,xflip_off,0,$f,colcyan)	
		vcntr				
		.db $C0,$00
		.db $E0,$00
        jsrl(char_space)

retint

;Initial Oxygen Counter Buffer Data
oxyini  vscal(ywin_off,1,$40)		
		vstat(sparkle_off,xflip_off,0,$0,colblack)	
		vcntr				
		.db $C0,$00
		vctrs(0,64d,voff)	
        jsrl(char_space)                
        