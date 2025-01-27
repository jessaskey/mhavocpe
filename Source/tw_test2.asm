;*************************************************
;*  TWTEST2 - Accessory routines for Self Test
;*************************************************
	.TEXT ".TWTEST."
;*************************************************
    .sbttl "BETA CPU Test"
;*************************************************
betat	ldx #stmbtest     
        jsr stmesg          ;'--- BETA CPU ---'
		ldx #stmtestra
		jsr stmesg			;RAM TESTS
		ldx #stmtestro
		jsr stmesg			;ROM TESTS
		;Hardware labels
		ldx #stmbcomm    
        jsr stmesg          ;BETA COMM     
        ldx #stmbromlow
        jsr stmesg          ;BETA MAIN LOW
        ldx #stmbromhig
        jsr stmesg          ;BETA MAIN HIGH
		ldx #stmbrammain
		jsr stmesg			;BETA MAIN RAM
		
		;ROM versions
		lda #($F0 + colpurple)
		ldx #$60
        jsr vgadd2  
		;ROM versions
		lda pgvetbl
		sta temp9			;Save base version in temp9 for below
		lda pgvetbl+1
		sta temp9+1
		;Show the base version 
		jsr vgcntr
		VGADD_VCTRS($28,$30,hidden)
		lda #0		;Alpha Low ROM version is 'master'
		sta temp8
		jsr showversion
		
		;ROM CSUM Displays
		lda #08
		sta temp8			;Line height
		lda #$1C
        sta temp8+1         ;Starting Y screen pos 
        ldx #IDENTIFIER_BL   
		ldy #IDENTIFIER_BH+1
		jsr shromsum
		
		;RAM Display
		ldx #01             ;Test this many RAM results
        stx temp8
        begin
            ldx temp8
            ldy ?msgpos,X
            lda comram+comtestbase        ;Any bad news?
			begin
				asl A
				dex
			miend
            ifcc
				ldx #stmmfail 
            else
                ldx #stmmpass
            endif
            ;jsr stpg4
            tya
            jsr stmesgv
            dec temp8
        miend 
		rts

?msgpos .db $44,$3C		
;*************************************************
    .sbttl "Statistics Display #1"
;*************************************************
;* This display shows the data specifics for the 
;* game.                    
;*************************************************    
teststat1 
        lda gotdata
        ifeq
            jsr ?getstats       ;Get the GAMMA Stats only once per self test
            lda #$80
            sta gotdata
        endif
        ;static text
        lda #(?ststat1&$ff)
        sta stextp
        lda #(?ststat1/256d)
        sta stextp+1
        jsr statict
        jsr setcolwhite
        ldy #04
        lda # ubyte(stb_game1)
        sta temp2+1
        lda # lbyte(stb_game1)
        sta temp2
        jsr ?copyd
        ;lda #$20
        ;ldx #$00
        ;jsr vgvtr5
		VGADD_VCTRS(0,32,hidden)
        ldy #04
        lda # ubyte(stb_atime1)
        sta temp2+1
        lda # lbyte(stb_atime1)
        sta temp2
        jsr ?copyd           ;Total Time 1 player games

        ldx #stmact2pg         
        jsr stmesg          ;'2P GAMES:'
        jsr setcolwhite
        ldy #04
        lda # ubyte(stb_game2)
        sta temp2+1
        lda # lbyte(stb_game2)
        sta temp2
        jsr ?copyd
        ;lda #$20      
        ;ldx #$00
        ;jsr vgvtr5          ;Position for atime2
		VGADD_VCTRS(0,32,hidden)
        ldy #04
        lda # ubyte(stb_atime2)
        sta temp2+1
        lda # lbyte(stb_atime2)
        sta temp2
        jsr ?copyd           ;Total Time 2 player games
		
        ldx #stmactwrps
        jsr stmesg          ;'WARPS:' 
        ;lda #$20
        ;ldx #$00
        ;jsr vgvtr5
		VGADD_VCTRS(0,32,hidden)
        jsr setcolwhite
        lda # ubyte(stb_xwarps)
        sta temp2+1
        lda # lbyte(stb_xwarps)
        sta temp2 
        ;Loop each warp
        lda #00
        sta temp4
        begin
			sta watchdog        ;kick the doggie
            pha
            cmp #04
            ifeq
                ;move back and down some
                jsr vgcntr
				VGADD_VCTRS(57,-36,hidden)
            endif
            ;change color
			pla
			tax
			jsr warpcolrtp1
            jsr scol
            ldy #02
            jsr ?copyd           ;Show Warps       
			VGADD_VCTRS(0,8,hidden)	;A little space over
            inc temp2
            inc temp2
            inc temp4
            lda temp4
            cmp #08
        eqend    
		VGADD_VCTRS(0,10,hidden)	;Position to start of XLIVES text
        ldx #stmactxliv
        jsr stmesg          ;'XLIVES:' 
        lda #$20
        ldx #$00
        jsr vgvtr5          ;Position from XLIVES text to Number position
        jsr setcolwhite
        ldy #02
        lda # ubyte(stb_extlie)
        sta temp2+1
        lda # lbyte(stb_extlie)
        sta temp2
        jsr ?copyd           ;Extra Lives Digits here
		rts

;****************************************
; Second Statistics Screen
;****************************************
?mzstaty = $26		;Y position where maze stats start
?mzstahy = $30		;Y position where maze stat header is located (typically +4 from actual stats)

teststat2
		;static text
        lda #(?ststat2&$ff)
        sta stextp
        lda #(?ststat2/256d)
        sta stextp+1
        jsr statict
        ;maze stat header
        jsr setcolyellow
        jsr vgcntr
        lda #$8F
        ldx #?mzstahy
        jsr vgvtr5 
        jsr ?stathead
        jsr vgcntr
        lda #$38
        ldx #?mzstahy+10d
        jsr vgvtr5 
        jsr ?stathead

        lda #$D4
        ldx #$70
        jsr vgadd2          ;Scale down a bit to make fit
        lda #27d            ;28 mazes
        sta temp9
        lda # ubyte(stb_mzstats)
        sta temp2+1
        lda # lbyte(stb_mzstats)
        sta temp2
        lda #?mzstaty
        sta temp6+1     ;Starting Y
        lda #01
        sta temp6       ;Store the first level number
        begin
			sta watchdog        ;kick the doggie
            jsr vgcntr
            lda temp9
            cmp #13d
            ifge
                ifeq
                    ;reset Y position for next list
                    lda #?mzstaty
                    sta temp6+1     ;Starting Y
                    jmp ?col2
                else
                    lda #$80       ;Starting X
                    ldx temp6+1
                    jsr vgvtr5      ;Position
                    ;lda #$C8       ;More X to the left
                    ;ldx #00
                    ;jsr vgvtr5      ;Position
					VGADD_VCTRS(0,-56,hidden)	;More X to the left
                endif
            else
?col2           lda #$08       ;Starting X for Column #2
                ldx temp6+1
                jsr vgvtr5      ;Position
            endif
            lda #($F0+colgreen)
            ldx #$60
            jsr vgadd2 
            ;ldy #vgjchl-vgmsga
            JSR_VGCHAR(idxchar_l)      ;Letter 'L' 
            lda temp6            
            jsr decimal         ;In decimal
            ldy #01
            lda #temp7
            clc
            jsr digits
        
            lda #($F0 + colwhite)
            ldx #$60
            jsr vgadd2 
            
            lda #03         ;Do sets of 4 statistics
            sta temp9+1
            begin     
                ldy #02
                jsr ?copyd           ;Output 2 digits
                lda temp2
                clc
                adc #02
                sta temp2
                lda temp2+1
                adc #00
                sta temp2+1     ;Move to next stat
                ;ldy #00
                JSR_VGCHAR(idxchar_space)      ;Space
                dec temp9+1     ;Next Set 
            miend
            inc temp6       ;Increment displayed L#
            lda temp6+1
            sec 
            sbc #$0A
            sta temp6+1     ;Move down next line
            dec temp9
        miend
        rts 


;***********************************************
;* Gamma Routines for Stats
;***********************************************   
?getstats    
        sei 
        lda #$80
        sta nogamma     ;Stop Gamma requests
        cli
        lda #g_sstawx           ;Request Xlife+Warp Data
        jsr xmit                ;Send it away
        ldx #g_numstwx-1        ;This many bytes
        begin
            jsr ?rcvst              ;Get Input
            sta stb_extlie,X        ;Save it
            dex
            cpx #-1
        eqend
        lda #g_sstap        ;Now Game Stats
        jsr xmit
        ldx #g_numstgs-1    ;Number Game Stats
        begin
            jsr ?rcvst
            sta stb_game1,X
            dex
            cpx #-1
        eqend
        sta watchdog
        lda #g_sstamz        ;Now Maze Stats
        jsr xmit
        ldx #g_numstmz-1     ;Number of Maze Stats
        begin
            jsr ?rcvst
            sta stb_mzstats,X
            dex
            cpx #-1
        eqend
        lda #00
        sta nogamma     ;Gamma Stuff allowed again
        rts

;special rcv for stats        
?rcvst  lda #i_xmigama
        sta watchdog
        ldy #00
        begin
            bit inputs
            bne ?rstat
            dey
        eqend
        ;Failed if here..
		lda #00
        rts
              
?rstat  lda portrd      ;Get Data
        rts   
		
;**********************************************
    .sbttl "Copy and Display"
;**********************************************
;* Copies Y bytes from temp2 to $0100 and 
;* displays them.
;**********************************************
?copyd  tya                 ;Store number of digits (2-digits per byte)
        pha
        lda #statbuff       ;Dest Buffer LSB
        sta temp3
        lda #$00            ;Dest Buffer MSB
        sta temp3+1
        jsr copymem           ;Copies bytes to statbuff
  
        ;(C) = carry set for zero supression      
        ;(A) = address of (Y) zero page           
        ;(Y) = number of zero page locs to use
        pla             ;How many bytes (2-digits per byte)
        tay         
        sec 
        lda #statbuff 
        jmp digits

?ststat1 
		.db stmgmstat,stmact1pg,-1
?ststat2
		.db stmgmstat,-1
        
?stathead
        lda #$D4
        ldx #$70
        jsr vgadd2          ;Scale down a bit to make fit
        JSR_VGCHAR(idxchar_s)	;S
		VGADD_VCTRS(0,32,hidden)
        JSR_VGCHAR(idxchar_l)	;L
		VGADD_VCTRS(0,32,hidden)
        JSR_VGCHAR(idxchar_b)	;B
		VGADD_VCTRS(0,32,hidden)
        JSR_VGCHAR(idxchar_a) 	;A 
		rts

;**********************************************
    .sbttl "BIPS Check"
;**********************************************
testbip lda #$A4                ;A few red ones
        ldx #spot_vpg
        jsr vgadd2          ;Set color red
        lda #00
        ldx #$73                ;This scale
        jsr vgadd2
        jsr vgcntr
        VGADD_JSR(frbox)    ;Box with center and position
        lda #05
        sta temp3
        begin
            VGADD_JSR(frbx2)    ;Box without center and scale
            dec temp3
        miend
        lda #$A2
        ldx #spot_vpg
        jsr vgadd2          ;Change color on last one
        laljsr(frbx2)
        lxhjsr(frbx2)
        jmp vgadd2          ;Add last box
             
;***********************************************
    .sbttl "Pattern Test and Alphabet (Tell a Friend)"
;***********************************************
testcr 
		laljsr(crosshatch)
        lxhjsr(crosshatch)
        jmp vgadd2
		
;***********************************************
    .sbttl "Hysterisis"
;***********************************************
testhy 	VGADD_JSR(hystr)
        jsr vgcntr
        lda frame+1
        sta temp3               ;Hold here to use
        lda frame
        rol A
        rol temp3               ;Use D7,D1,D0
        lda temp3
        and #07             	;Binary part
        ora #$70                ;Make look like a stat
        cmp #$71                
        ifcc                    ;Skip if too big
            rts
        endif
        tax 
        lda frame
        and #$7F
        jsr vgadd2          ;Write stat instruction
        laljsr(frbox)
        lxhjsr(frbox)
        jmp vgadd2
        
;***********************************************
    .sbttl "Color Bars Test 1 (Old Test)"
;***********************************************
testco	lda #02             ;Set scale 2
        jsr vgscalz          ;Set scale
        ldy #06
        sty temp3
        begin
            jsr vgcntr          ;Center
            ldy temp3
            lda ?posix,Y         ;Position this group
            ldx ?posiy,Y
            jsr vgvtr5
            lda temp3
            eor #$FF
            and #07
            ldx #spot_vpg
            jsr vgadd2
            lda temp3               ;White group?
            ifeq
                laljsr(clpt2)
                lxhjsr(clpt2)           ;Special White group
            else
                laljsr(clpat)
                lxhjsr(clpat)
            endif
            jsr vgadd2
            dec temp3
        miend
        laljsr(cl73)                ;Last on is white
        lxhjsr(cl73)
        jmp vgadd2

;**************************************************
    .sbttl "Color Bars Test 2 (New Test)"
;**************************************************  
barvram = vecram+$400		;Where we will put our vector data
   
testcs  jsr vgcntr          ;Center
        lda #02             ;Set scale to 2
        jsr vgscalz         ;Set scale
        bit temp2           ;Already written pattern to buffer?
        ifpl
            jsr initcolram      ;Init color RAM
            lda vglist
            pha 
            lda vglist+1
            pha                 ;Save current pattern
            lda # lbyte(barvram) 
            sta vglist          ;Use upper half of RAM
            lda # ubyte(barvram)
            sta vglist+1        ;Set up buffer pointer
            ldy #16d-1          ;Number of patterns
            sty temp3
            lda #$82
            ldx #$20           	;Move to left to draw a pattern
            jsr vgvtr5
            begin
                lda #$F0            ;One line of each intensity
                sta temp3+1         ;Save intensity
                sta watchdog
                begin                   ;Draw a group
                    lda temp3+1         ;Intensity of this line
                    ora temp3               ;Add in color of this line
                    ldx #$60                ;Stat instruction
                    jsr vgadd2
                    ldy #$20
                    lda #$0C
                    ldx #00
                    jsr vgvtr               ;Small line    
					VGADD_VCTRS(-4,-12,hidden)	;Position for next line
                    lda temp3+1
                    sec 
                    sbc #$10                ;Next Intensity
                    sta temp3+1
                ccend                   ;Done with that group
                lda #$11                ;Move over for next group
                ldx #$40                ;Move up to beginning
                jsr vgvtr5
                dec temp3               ;Next group??
            miend
            jsr vgrtsl          ;Add an RTSL to buffer
            pla 
            sta vglist+1            ;Restore old vglist
            pla 
            sta vglist
            lda #$80
            sta temp2               ;Set flag
        else
            VGADD_JSR(vecram+$400)  ;Add JSRL to list
        endif
        rts 
        
;*******************************
;* X Position for bars
?posix  .byte $DE,$9D,$1F,$9D,$DE,$1F,$DE

;*******************************
;* Y Position
?posiy  .byte $F4,$D8,$D8,$10,$D8,$10,$10

;******************************************
    .sbttl "Checker/Grid Board"
;******************************************
testgr  
		jsr vgcntr
        lda #02
        jsr vgscalz
        ldx #09             ;Nine bars horizontal
        stx temp2
        jsr vgcntr
        begin
            ldy temp2
            lda #$80
            ldx ?hlpos,Y
            jsr vgvtr5          ;Postion for this line
            VGADD_JSR(hline)    ;Draw line and recenter
            dec temp2
        miend
        ldx #11d                ;Number of Bars
        stx temp2
        begin
            ldy temp2
            lda ?vlpos,Y
            ldx #$6C
            jsr vgvtr5
            VGADD_JSR(vline)
            dec temp2
        miend
        lda gammaerr            ;Gamma Running?
        ifpl                    ;Nope, use aux coin switch
            jsr ?getc                ;Returns A=X and sign set of A
            ifmi
                asl tempa               ;Debounce color switch
                ifcs
                    inc temp8               ;Next color
                endif
            else
                lda #$20                ;Reset not pushed
                sta tempa
            endif
        else                    ;Gamma OK, use gamma
            jsr getleta         ;Get LETA value
            lsr A
            lsr A
            lsr A
            lsr A               ;Use top 4 bits
            sta temp8           ;Sav for color
        endif
        rts 
        
?getc   lda #(o_resetg+o_resetb)                ;Deselect player 1 for coins, but keep the other procs running
        sta outputs                 
        lda inputs                              ;get them now
        ldx #o_swtchmux+o_resetg+o_resetb       ;Put Player 1 back on to player inputs
        stx outputs                 
        tax                                     ;coin switches, test sign of A to see if right coin was triggered
        rts 
        
?hlpos  .byte $A1,$B6,$CB,$E0,$F6,$0A,$20,$35,$4A,$5F
?vlpos  .byte $94,$A8,$BB,$CF,$E3,$F6,$0A,$1D,$31,$45,$58,$6C

;*************************************************
; Inputs and Outputs Display
;*************************************************
iodisp	ldx #stminpout
        jsr stmesg          ;'--- INPUTS/OUTPUTS ---'  
        ;Show Roller Value
        jsr vgcntr
		lda #($F0+colgreen)
        ldx #$60        	;Color GREEN
        jsr vgadd2          ;Add stat
        jsr getleta         ;Display direct
        sta thisleta
        lda #thisleta       ;So Display can use it
        ldy #01
        jsr digits          ;Display the roller value
        ;Position for options
        jsr vgcntr
		VGADD_VCTRS(-104,-64,hidden)
        lda #01             ;Scale 1
        jsr vgscalz          ;Big Numbers
        jsr ?optn2          ;Output option switches
		jsr ?cpuid
        rts

;********************************************
; Settings Management
;********************************************
gsettings
		jsr doconfirm		;Show confirm if needed
		lda gammawait
		ifne
			;Waiting for Gamma to return clear
			jsr getgdon
			ifeq
				;yes, it is done
				lda #g_sndstop
				jsr xmit
				lda #0				;Clear the data flag so it is fetched again
				sta gotdata
				sta gammawait
			else
				;Waste some time
				jsr do3del
			endif
			rts
		endif
		lda gotdata
		ifeq
			;first time, get settings from Gamma
			jsr initoption      ;Get Option data
			lda c_cmode
			sta set_cmode
			lda rawgamedif
			sta set_diff
			lda slives
			sta set_lives
			lda nxtbonus
			sta set_bonus
			lda sndatt
			sta set_attsd
			;Roller Multiplier
			lda rollmult
			sta set_rollmult
			lda #$80
            sta gotdata
		endif
		sta watchdog
		ldx #stmgsetts
        jsr stmesg      
		ldx #stmpaux
		jsr stmesg
		ldx #stmfiresel
		jsr stmesg
		lda setsel
		cmp #07
		ifne
			ldx #stmrollsel
			jsr stmesg
		endif
		;Only show 'save' message if on 'load from dips' or data has changed
		jsr chkchngs
		ifne
			ldx #stmpshelds
			jsr stmesg
		endif

		;Pricing
		ldy set_cmode		;Load from base copy
        ldx stcmodz,Y       ;Message from Mainline
		lda #$01			;Color Flag
		jsr getcol
        jsr stmesgc         ;Display cost
		;Difficulty Level
        ldy set_diff		;Which difficulty level?
        ldx diftbl,Y
		lda #$02			;Color Flag
		jsr getcol
        jsr stmesgc
		;Lives per game
		jsr vgcntr
		vgadd_scale(ywin_off,binscal2,$00)
		lda #-86
		ldx #$30
        jsr vgvtr5          ;Hidden position
		lda #$03			;Color Flag
		jsr getcol			;Get Color
		pha
		lsr A 
		lsr A
		lsr A 
		lsr A
		ora #$F0
		ldx #$60
		jsr vgadd2			;Set Color
		vgadd_scale(ywin_off,binscal2,$30)
		lda set_lives
		jsr decimal     	;lives in decimal to temp7
		lda #temp7
		ldy #1				;1 digit
		sec 				;hide leading zeros
		;clc
		jsr digits      	;Display Lives
		ldx #stmlivem
		pla 				;Get back color
		jsr stmesgc
		
		ldx #stmlddips
		jsr stmesg
		
		sta watchdog
		
		;Bonus life
        ldy set_bonus
		ldx bonusmg,Y
		lda #$04			;Color Flag
		jsr getcol
		jsr stmesgc
		
		;Roller Multiplier
        ldx #stmrollx1      ;Guess x 1
        lda set_rollmult               
        ifne
            ldx #stmrollx2
        endif
		lda #$06			;Color Flag
		jsr getcol
        jsr stmesgc

		;Attract Mode Sounds
        ldx #stmsndon       ;Guess sounds in attract
        lda set_attsd
        ifeq
            ldx #stmsndoff
        endif
		lda #$05			;Color Flag
		jsr getcol
        jsr stmesgc

		;Draw the selector triangle
		lda setsel
		ifne
			vgadd_scale(ywin_off,binscal2,$00)
			lda #$28
			ldx setsel
			begin
				cpx #7
				ifge			;Extra for 'Reset to DIPS' message
					sec
					sbc #$08
				endif
				sec
				sbc #$08
				dex
			eqend
			tax
			lda #-90
			jsr vgvtr5          ;Hidden position
			vgadd_scale(ywin_off,binscal2,$30)
			vgadd_stat(sparkle_off,xflip_off,0,$F,colyellow) 
			VGADD_JSR(char_ltri)	
		endif
		
        sta watchdog
		;Done with messages
		;Fire Buton
		lda #2
		jsr chkbut
		ifcs
			;move to next
			inc setsel
			lda setsel
			cmp #08
			ifge
				lda #$01
				sta setsel
			endif
		endif
		
		sta watchdog
		jsr updroller
		
		ldx setsel
		ifne
			;check roller status now..
			lda rollcnt     ;-1, 0, 1 valid values. (Bumps one option each time)
			ifne
				dex				;Zero based
				ldy whatram,X
				clc
				adc 0,Y     ;Bump option up/down 1
				cmp settop,X
				ifpl
					lda settop,X
                else
				    cmp setlow,X
				    ifmi
					    lda setlow,X
                    endif               
				endif
				sta 0,Y     ;Save new option setting
			endif
			lda #0				;Clear our accum counter
			sta rollcnt		
			;look for shield button press
			jsr chkchngs
			ifne
				lda #3
				jsr chkbut
				ifcs
					lda bconfirm		;bconfirm goes from 0 to -1 and then back to 0
					ifeq
						lda #$FF
						sta	bconfirm
					else
						lda #-1
						sta nobut
						lda setsel
						cmp #07
						ifeq
							;reset from DIPS
							lda #snd_b1d
							jsr xmit
							jsr do3del
							lda #g_clro
							jsr xmit
							jsr do3del
							lda #-1
							sta gammawait
						else
							jsr savesets
						endif
						lda #0
						sta nobut
						sta bconfirm
					endif
				endif	
			endif
		endif	
		rts

;See if the currrent settings on screen are different from what is
;stored in memory settings. Returns 0 if equal, 1 if not equal.
chkchngs
		lda c_cmode
		cmp set_cmode
		bne ?notequal
		lda rawgamedif
		cmp set_diff
		bne ?notequal
		lda slives
		cmp set_lives
		bne ?notequal
		lda nxtbonus
		cmp set_bonus
		bne ?notequal
		;Upright/Cocktail
		lda rollmult
		cmp set_rollmult
		bne ?notequal
		lda sndatt
		cmp set_attsd
		bne ?notequal
		lda setsel
		cmp #07
		ifeq
?notequal	lda #1
		else
			lda #0
		endif
		rts
		
;for each value of setsel, which RAM location should we increment?		
whatram .db set_cmode,set_diff,set_lives,set_bonus,set_attsd,set_rollmult
setlow	.db $00  	 ,$00  	  ,$03      ,$00      ,$00      ,$00
settop	.db $03 	 ,$03     ,$06      ,$03      ,$01      ,$01

savesets
		sta watchdog
		lda #0					
		sta setsel				;Clear this so that roller and shield are no longer checked
		;Start sounds
		lda #snd_passby         ;Take off sound
		jsr xmit
		jsr do3del
		lda #snd_b1d
		jsr xmit
		jsr do3del
		;Build up a single byte (in temp1)
		lda #0
		sta temp1
		lda set_attsd
		asl A 
		and #02
		ora temp1
		sta temp1
		;Roller Multiplier
		lda set_rollmult
		and #01
		ora temp1
		sta temp1

		lda set_bonus
		asl A 
		asl A
		and #$0C
		ora temp1
		sta temp1
		lda set_diff
		asl A 
		asl A
		asl A 
		asl A
		and #$30
		ora temp1
		sta temp1
		lda set_lives
		tax 
		lda gamlvsx,X
		ror A
		ror A
		ror A
		and #$C0
		ora temp1			;Now we have the final value to send
		jsr setoptn0		;Save Play options
		sta watchdog
		jsr do3del			;3 delay loops
		lda set_cmode
		jsr setoptn1		;Pricing options
		lda #-1
		sta gammawait
		rts

;returns color and scale in Y based upon bit
getcol	cmp setsel
		ifeq
			lda #((colred<<4)|(1)) 
		else
			lda #((colgreen<<4)|(1))
		endif
		rts
		
;********************************************
    .sbttl "Display Option Switches"
;********************************************
?optn2  jsr vgcntr
		VGADD_VCTRS(-40,-28,hidden)
		lda #($F0+colwhite)
        ldx #$60        	;Color WHITE
        jsr vgadd2          ;Add stat
		lda #07             ;8 Switches
        sta temp2
		sei                 ;No interrupts while reading switch
        ; lda #o_resetg+o_resetb          ;Drop player select for these switches, keep procs running tho
        ; sta outputs
        ; lda inputs
        ; jsr dis01
		; VGADD_VCTRS(8,-64,hidden)
        ; ldx #07
        ; stx temp2
        
        ; lda #o_swtchmux+o_resetg+o_resetb       ;Reselect self test stuff, keep procs running too
        ; sta outputs
        ; lda inputs          ;Remaining switches
        ; cli 
        ; jmp dis01

        lda #g_getdip0  ;GAME Options @ 13/14S - POKEY DIPS (inverted) 
        jsr xmitr
		jsr dis01
		VGADD_VCTRS(8,-64,hidden)
		ldx #07
        stx temp2
        jsr dodelay     
        lda #g_getdip1  ;COIN Options @ 8S - Straight DIPS
        jsr xmitr
        cli 
		jmp dis01

;********************************************
    .sbttl "CPU IDENTIFIER"
;********************************************
?cpuid	lda #($F0+colblue)
        ldx #$60        	;Color BLUE
        jsr vgadd2          ;Add stat
		VGADD_VCTRS(16,-64,hidden)
		ldx #07
        stx temp2
		lda d6502			;If the CPU ID made it through all the self test code, it is here
        jmp dis01
		
.export teststat1,teststat2,iodisp,testgr,testcs,testco,testcr,testbip,testhy,gsettings,betat