;*****************************************************
    .TEXT ".TWTEST."
;*****************************************************
    .title "Self Test Routines"
    .sbttl "Reset Memory"
;*****************************************************

;***********************************************
;* Define Memory Blocks for Gamma stats data
;* These are all index based, so we have to add
;* back on the local Alpha address offset to
;* make them right for over here.
;***********************************************
_stptr  = *

;Zero Page Self Test Stats, safe to reuse alpha RAM after 
 .org       ltcolm
 
btn_a_time  .block 1		;debouncing Aux Coin switch
btn_f_time  .block 1		;debouncing Fire switch
btn_s_time  .block 1		;debouncing Sheild switch
litrast     .block 2    	;Text Message Pointer for Self Test (2 Bytes)
bconfirm    .block 1    	;Set to $80 when confirm message is shown
eebad       .block 1
goteebad    .block 1    	;Flag to know if we have already requested EEBAD from GAMMA
stexti      .block 1    	;Index Variable for looping through static text tables
stextp      .block 2    	;(2 BYTES) Pointer location for static text
testnm      .block 1    	;Need a byte here
laststnm	.block 1		;If current test is not last test, then some flags are always cleared
statbuff    .block 16d  	;16 Bytes for Buffering of Digits
pgcstbl     .block IDCOUNT  	;15 checksums go here
pgidtbl		.block IDCOUNT		;ROM Identifier Byte
pgvetbl		.block IDCOUNT*2	;ROM Version Words
erplc       .block 5    	;RAM and such error regs
							;$F0 - Low RAM Bad: Page 0,1 and 0 Swap RAM
							;$F1 - High RAM Bad: 1 Swap RAM and page 8,9
							;$F2 - Vector RAM?
							;$F3 - Vector RAM?
							;$F4 - COMM RAM (BETA)
gammaerr    .block 1    	;Gamma Errors
stgdead   	.block 1 		;0=normal, otherwise, Gamma has failed

nxtcmd      .block 1    	;Next command number
thisleta    .block 1    	;The value of the roller this time
lastleta    .block 1    	;Last value of LETA for roller scrolling
lastdir     .block 1    	;List direction the spinner was going
gotdata    	.block 1
setsel		.block 1		;Setting selector for game settings screen
set_cmode	.block 1
set_diff	.block 1
set_lives	.block 1
set_bonus	.block 1
set_rollmult .block 1
set_attsd	.block 1
rollcnt		.block 1
nobut		.block 1		;When negative Self-Test Fire/Shield checks always return false
gammawait	.block 1		;When negative, need to wait for GAMMA Writes to finish

#if $ >= frame \ .error "SELF TEST RAM has intruded upon Alpha RAM boundary." \ #endif

 .org $0600

stb_opts     .block g_numstop
stb_extlie   .block g_numstxl       ;Extra Lives Earned - This is after the warp data, so add offset
stb_xwarps   .block g_numstwx       ;Number of Warps Done - 2 bytes per warp
stb_game1    .block g_numstgs/4     ;1 Player Games
stb_game2    .block g_numstgs/4     ;2 Player Games
stb_atime1   .block g_numstgs/4     ;Accum time for 1 Player Games
stb_atime2   .block g_numstgs/4     ;Accum time for 2 Player Games
stb_mzstats  .block g_numstmz       ;Maze Stats 

;*****************************************************************
;* Return to original ROM address here
;*****************************************************************
 .org _stptr
 
;********************************************************
;* Self Test Entry from Main Program
;* Y contains CPU ID value
;********************************************************
selftest   
        sei 
        ldx #$FF
        sta vgreset         ;Reset the VG
        cld 
        lda #o_swtchmux     ;$20
        sta outputs
        ;lda #o_swtchmux     ;$2C
        ;sta outputs
        lda #00             ;Clear ALL RAM
        tax 
        begin
            sta $0000,X
            sta $0100,X
            sta $0200,X
            sta $0300,X
            sta $0400,X
            sta $0500,X
            sta $0600,X
            sta $0700,X
            sta $0800,X
            sta $0900,X
            sta vecram,X
            sta vecram+$100,X
            sta vecram+$200,X
            sta vecram+$300,X
            sta vecram+$400,X
            sta vecram+$500,X
            sta vecram+$600,X
            sta vecram+$700,X
            sta vecram+$800,X
            sta vecram+$900,X
            sta vecram+$A00,X
            sta vecram+$B00,X
            sta vecram+$C00,X
            sta vecram+$D00,X
            sta vecram+$E00,X
            sta vecram+$F00,X
            sta	comram,X
			sta	comram+$100,X
			sta	comram+$200,X
			sta	comram+$300,X
			sta	comram+$400,X
			sta	comram+$500,X
			sta	comram+$600,X
			sta	comram+$700,X
			; sta	comram+$800,X
			; sta	comram+$900,X
			; sta	comram+$A00,X
			; sta	comram+$B00,X
			; sta	comram+$C00,X
			; sta	comram+$D00,X
			; sta	comram+$E00,X
			; sta	comram+$F00,X
            sta watchdog
            clc 
			adc #1
            sta rampg               ;Clear alt RAM page too!
			sec
			sbc #1
            sta $0300,X
            sta $0400,X
            sta $0500,X
            sta $0600,X
            sta $0700,X
            sta rampg               ;Set page back
            inx 
        eqend
        lda inputs              ;If self test
        and #i_slftstbit
        ifne   
			lda #o_swtchmux+o_resetg+o_resetb   ;Need to select self test switch
			sta outputs
			sta out1s
            jmp pwron           ;Goto Game Reset, get out of here at least
        endif
		
		lda #o_swtchmux+o_resetg   ;Let GAMMA start
		sta outputs
		sta out1s
		
		sty $0000				;Special Save of CPU ID 
		;1st Pulse
		jsr pulselig
;********************************************
    .sbttl "Page 9 Test"
;********************************************
		lda #$FF
        ldx #00
        begin
            sta $0900,X            ;First set RAM to $FF
            dex
        eqend
		;X is now $00
        begin
        lda $0900,X
            eor #$FF
            ifeq            ;Got a bad RAM here
                lda #01             ;RAM is 2K x 8, use 1,2,4,8,10,20,40,80 test
?rnxtpat        sta $0900,X         ;Write out test pattern
                tay 
                eor $0900,X
                ifeq    ;Not the same, oh shit! An error!
                    tya 
                    beq ?patdone        ;Stop when RAM is 0
                    eor #$FF            ;Write more than one bit
                    sta $0900,X         ;Ed says this causes some RAM's to fail
                    eor $0900,X
                    ifeq            ;Get an Error?
                        tya 
                        asl a
                        jmp ?rnxtpat
                    endif
                endif
            endif
            ldy #01
            bne ?rambad         ;Report top RAM bad!
            ;* If this RAM bad, watchdog will loop and keep trying for us! *
?patdone    inx 
        eqend                   ;Next location
		;***************************************************
		;special moving around of CPU ID value
		ldy $0000				;Was here
		sty $09FF				;Now here
		;***************************************************
        ;stx rands               ;Test 0 page
        lda #07
        jmp ?tst2k              ;Now test 0 to 7ff
        ;* Above checks 0,1 and swap 0 of swapping RAM 200-7ff and 800
?zpgtst ifcs                    ;Return here from test routine
            ldy #00             ;Report bad low RAM
            beq ?rambad         ;Go and die!!
        endif
        lda # ubyte(vecram)    	;Do vector RAM
        sta ?tstart
        clc 
        adc #07             	;Test first 2K
        jsr ?tst2k
        ldy #02
        ifcs
            tya 
            sta erplc,Y         ;Bad RAM place
            ldx #snd_c4         ;and beep here!
            jmp ?die            ;This one bad
        endif
        lda # ubyte(vecramb2)
        sta ?tstart
        clc 
        adc #07
        jsr ?tst2k
        ldy #03             ;Report low vec RAM
        ifcs
            jsr ?rambad          ;Report and return (if possible)
        endif
        ;Test COMM RAM Here, not needed, but left anyway
        lda # ubyte(comram)    ;Alpha/Beta Communication RAM
        sta ?tstart
        clc    
        adc #07
        jsr ?tst2k          ;Test 2K COMRAM
        ldy #04             ;Error for bad COMRAM
        ifcs
           jsr ?rambad
        endif
        lda #01
        sta rampg           ;Swap to RAM page 1
        lda #02
        sta ?tstart         ;Do 200-8ff
        lda #08
        jsr ?tst2k          ;Test swap RAM
        ldx #00
        stx rampg           ;Reset to 0
        ldy #01         	;This is also swap RAM
        ifcs
            jsr ?rambad
        endif
		
		;Okay, RAM testing is done... get CPU ID back and store in regular location
		lda $09FF
		sta d6502
		
		;also let BETA CPU run now 
		lda #o_swtchmux+o_resetg+o_resetb   ;Let BETA start
		sta outputs
		sta out1s
		
		;2nd Pulse
		jsr pulselig	
        jmp ?stest3      ;RAM Ok, do ROM tests now
        
      
;********************************************
;* We have an error, so how do we tell the  *
;* real world???                            *
;*     y = section that failed              *
;********************************************
?rambad tya 
        sta erplc,Y     ;Save possible failures
        tya             ;See if this was page 0
        ifne
            rts
        endif
        ldx #snd_c7     ;Beep alot
		begin
?die		lda #i_xmigama
			begin
				sta watchdog
				bit inputs      ;Wait for Gamma
			neend
			lda portrd      ;Get data
        neend         	;This is first response
        stx portwr      ;Send the sound
        begin
            lda inputs      ;Wait for switch to turn off
            and #i_slftstbit
            sta watchdog
        neend
        ;Here is how we die
		begin
			jsr pulselig
        doloop     

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
		sta pgidtbl,X		;Save identifier
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
        begin
            begin   
                eor (temp1,Y)
                iny
            eqend
            inc temp1+1
            sta watchdog        ;No Watchdog
            dec temp2
        eqend
        ldx temp3
        sta pgcstbl,X        ;Checksums
        inc temp3           ;Indicated 1 more
        rts 
        
;*********************************************
;* setpgv - set vector page for ROM checksum 
;*          this sets the page, executes a   
;*          vggo and then waits for the halt 
;*          flag. At that point the proper   
;*          page should be selected.         
;*                                           
;* Entry:   A = page to select               
;*********************************************      
?setpgv  
		sta vgreset     ;Make sure stopped
        ora #$60
        sta vecram+1
        sta vecram      ;Stat instruction
        lda #$20
        sta vecram+2
        sta vecram+3        ;Halt instruction
        sta vggo            ;Start VG
        lda #i_haltbit      ;Look for halt
        begin
            bit inputs          ;Stopped?
        neend
        rts
        
;************************************************
    .sbttl "ROM Test"
;************************************************
;* The following is a table of ROM's which must 
;* be check-sumed, their "real" address, and    
;* any paging if necessary.                     
;*                                              
;* Seed Address     Pages   Paging               
;* 00   8000-BFFF   64.     No              
;* 01   C000-FFFF   64.     No              
;* 02   2000-3FFF   32.     PRGM 0          
;* 03   2000-3FFF   32.     PRGM 1          
;* 04   2000-3FFF   32.     PRGM 2          
;* 05   2000-3FFF   32.     PRGM 3   
;* 06   2000-3FFF   32.     PRGM 4         
;* 07   2000-3FFF   32.     PRGM 5          
;* 08   2000-3FFF   32.     PRGM 6          
;* 09   2000-3FFF   32.     PRGM 7   
;* 10   5000-5FFF   16.     No              
;* 11   6000-7FFF   32.     VCTR 0          
;* 12   6000-7FFF   32.     VCTR 1          
;* 13   6000-7FFF   32.     VCTR 2          
;* 14   6000-7FFF   32.     VCTR 3      
;*                                              
;* To set VCTR paging, a small program must be  
;* transferred to the vector RAM which does a   
;* STAT and PAGE instruction then halts. This   
;* is the only way to assure that the proper    
;* vector page is selected. The vector gen      
;* must not be running during the test.         
;* To set program page, a store to global addr  
;* ROMPG must be done with the desired page #   
;*                                       
;* (temp2,+1)    = scratch                
;* (pgcstbl,+14d = 15 checksums for ROM's 
;* (temp3)       = diag step #    
;************************************************
?stest3  
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
        lda #00
        sta temp4               ;Now do programming paging
        begin
            lda temp4
            sta rompg
            lda # ubyte(auxpgm)       ;All pages start at the same place
            sta temp1+1             
            ldx #32d                ;Do 32 pages
            jsr ?dosum              ;This will bump temp3 for us
            inc temp4               ;Next?
            lda temp4
            cmp #08                 ;Done all 8?
        eqend
        lda # ubyte(vecrom) ;Start at 5000
        sta temp1+1
        ldx #16d            ;Number of pages
        jsr ?dosum          ;5000-5FFF
        lda #00
        sta temp4               ;Which page
        begin
            lda temp4               ;Set vector page
            jsr ?setpgv             ;Set vector page
            lda # ubyte(vecromov)   ;All pages start at same place
            sta temp1+1
            ldx #32d
            jsr ?dosum              ;This will bump temp3 for us
            inc temp4               ;next??
            lda temp4
            cmp #04             ;Done all 4?
        eqend
		
		;3rd Flash
		jsr pulselig
;*****************************************
;* End of ROM tests       
;*
;* Begin GAMMA status test              
;*****************************************
        lda #i_xmigama      ;Gamma Xmtd Flag for later  
        ldx #00         
        sta watchdog
        stx stgdead
        stx gammaerr        ;Guess Gamma bad (0 is bad)
        stx gotdata
        begin
			nop
            dex                 ;Try 255 times to get gamma response
            ifeq             
                lda #$80
                sta stgdead   	;Set that Gamma is not reliable
                sta nogamma     ;tell not to send either
				bne ?gdone
            endif
            bit inputs          ;Gamma should return quickly
        neend                   ;Should respond       
		;4th Pulse
		jsr pulselig
?gdone	lda portrd          ;Get the Data
        ;* First response ofter a reset is -1 
		sta gammaerr        ;Gamma should return 0xff
		
;*******************************************
;* Transfer BETA test results from COMRAM
;*******************************************		
		lda comram+comtestbase			;This byte will be NON-ZERO if BETA is present
		ifne
			and #$0F
			cmp #$0A						;Lower Nybble will be $0A if BETA is there
			ifeq
				;copy BETA ROM Template info into pgidtbl for BETA Test screen in ST mainline
				lda comram+comtestbase+1
				sta pgcstbl+IDENTIFIER_BL
				lda comram+comtestbase+2
				sta pgcstbl+IDENTIFIER_BH
				lda comram+comtestbase+3
				sta pgidtbl+IDENTIFIER_BL
				lda comram+comtestbase+4
				sta pgidtbl+IDENTIFIER_BH
				
				lda comram+comtestbase+5
				sta pgvetbl+(IDENTIFIER_BL*2)
				lda comram+comtestbase+6
				sta pgvetbl+(IDENTIFIER_BL*2)+1
				lda comram+comtestbase+7
				sta pgvetbl+(IDENTIFIER_BL*2)+2
				lda comram+comtestbase+8
				sta pgvetbl+(IDENTIFIER_BL*2)+3
				lda #-1
			else
				lda #0
			endif
		endif
		sta has_beta			;Mark BETA as present	


;Finally start ST init stuff		
        ;* Anything else coming back is an error 
        jsr initcolram      ;Init Color RAM
        
        ;Initialize the Self Test Messages too
        lda # ubyte(msgtblen);MSB
        sta litrast+1
        lda # lbyte(msgtblen);LSB
        sta litrast
     
        ;init for first screen
        lda #g_sndstop       ;Start with 'Sounds OFF'
        sta nxtcmd
        jsr getleta         ;Display direct
        sta thisleta
        sta lastleta        ;make these equal
		lda #-1
		sta laststnm
        jmp ?diagm          ;Fall through into Diagnostic mainline
        
;*****************************************************
    .sbttl "Self Test Main Line"
;*****************************************************  
?diagm  begin
            jsr doflash
            lda frame
            and #$10
            lsr A
            lsr A
            lsr A
            lsr A
            ora #o_resetg+o_resetb  ;Keep processor on
            sta out1s               ;For interrupt stuff
            lda stgdead           	;If Gamma is not dead, then wait for it
            ifeq
                ldx #40d
                begin
                    begin
                        lda inputs
                        lsr A
                        lsr A
                    csend
                    begin
                        lda inputs
                        lsr A
                        lsr A
                    ccend
                    dex
                miend
            endif
            inc frame
            ifeq
                inc frame+1         ;2 Bytes for bin scale test
            endif
            begin
                lsr inputs          ;check Halt
            csend
?vgld       sei                     ;Hold interrupts until done with write
            lda # lbyte(vecram)     ;Reload VG
            sta vglist
            lda # ubyte(vecram)    
            sta vglist+1
			lda #1
			jsr chkbut
			ifcs
				lda #00
				sta temp2           ;Buffer flag clear for some routines
				lda #g_sndstop
				sta nxtcmd          ;Start again at 'Sounds Off'
				;clear the dataload flag so the screen can re-get info if needed
				lda #$00
				sta gotdata			;Reset data fetched flag
				inc testnm          ;Next test
				lda testnm
				cmp #01				;BETA test?
				ifeq
					lda has_beta
					ifeq
						inc testnm			;If no BETA, then inc right on past the BETA screen
					endif
				endif
				lda #00
				ldx #06
				begin
					dex
					dex
				miend
				lda #$00
				sta setsel
			endif
            jsr vgcntr
            lda #00
            sta vdata
            sta vdata+1         ;Setup for window
            sta vdata+2
            sta vgbrit
            ldx #$71
            jsr vgadd2          ;Set scale
            lda #01
            sta vdata+3         ;VCTR 100 up
            jsr vgvtr2
            lda #$79
            tax                 ;Set window (x=79)
            jsr vgadd2
            lda #($F0+colred)
            ldx #spot_vpg
            jsr vgadd2
            jsr vgcntr
            VGADD_JSR(frbox)    ;Add red box
            jsr vgcntr
            lda testnm
            cmp #((?sftjse-?sftjsr)/2)-1        ;Last test
            ifeq
                lda temp8
                and #07
                ifeq
                    lda #01
                endif
                ora #$C0                ;Intensity
            else
                lda #($F0+colwhite)               ;Put up white box
            endif
            ldx #spot_vpg
            jsr vgadd2
            VGADD_JSR(frcfl)    ;do box
            jsr dostate         ;Do this routine
            jsr vgcntr
            jsr vghalt          ;Center and halt
            sta vggo            ;Start display
            sta watchdog
            lda #o_swtchmux+o_resetg+o_resetb           ;Player select bit (keep gamma running)
            sta outputs         ;Select player 1
            lda inputs          ;Still Self test??
            cli
            and #i_slftstbit
        neend
        jmp pwron
        ;Stay here until reset upon Self Test exit
;?wdreset 
;		bne ?wdreset         ;Watch dog reset for exit


;***********************************************
    .sbttl "Test Cases"
;***********************************************
?sftjsr .word ?alphat-1      	;Alpha Tests
		.word betat-1			;Beta Tests
        .word ?gammat-1      	;Gamma Tests
		.word gsettings-1		;Game Settings
        .word ?testwarp-1    	;Warp Codes
		.word iodisp-1      	;Inputs and Outputs
		.word teststat1-1   	;Statistics #1
		;.word teststat2-1   	;Statistics #2
        .word testhy-1      	;Scaling/Hysterisis
        .word testbip-1     	;BIPS check
        .word testcr-1      	;Cross hatch and characters
        .word testco-1      	;Color Bars
        .word testcs-1      	;Intensity check
        .word testgr-1      	;Grid
?sftjse

;What page are the routines on?
?sftpg	.db $00	;Alpha Tests
		.db $01	;Beta Tests
		.db $00	;Gamma Tests
		.db $01	;Game Settings
		.db $00	;Warp Codes
		.db $01 ;Inputs/Outputs 
		.db $01	;Stats #1
		;.db $01	;Stats #2
		.db $01	;Scaling/Hysterisis
		.db $01	;Bips
		.db $01 ;Cross Hatch
		.db $01	;Color Bars
		.db $01 ;Intensity
		.db $01	;Grid
		
dostate lda testnm
        cmp #(?sftjse-?sftjsr)/2	;Non valid state?
        ifcs
            lda #00
            sta testnm          	;Start Over!
        endif
		lda testnm
		cmp laststnm
		ifne
			;stuff needs reset on test changing
			lda #0
			sta bconfirm
			ldx testnm
			stx laststnm
		endif
		lda testnm
		tay
		tax
		lda ?sftpg,X			;Get the proper page for this (most are $00)
		sta rompg
		tya
		asl A
		tax
        lda ?sftjsr+1,X
        pha 
        lda ?sftjsr,X
        pha 
		txa	
		lsr A			;Divide by 2
        rts             ;Jump to case		

;************************************************
;* Button Press Testing routine
;************************************************
; A = Button (1=Aux,2=Fire,3=Shield)
; Returns: 
;    Carry Set   = Button Pressed Long enough
;    Carry Clear = Button not Pressed
; 
; If nobut is !0, then return no for Fire/Shield
;************************************************
chkbut	ldx #$20
		cmp #01
		ifeq
			lda inputs
            and #i_auxcoin          ;Aux Coin Switch pushed?
            ifeq
				lda #0
				sta nobut				;Always reset button flag on screen changes
                asl btn_a_time
				rts
			endif
			stx btn_a_time
?nope		clc
			rts
		endif
		cmp #02
		ifeq
			lda nobut
			bne ?nope
			jsr getswitch
			ifpl
				asl btn_f_time
				rts
			endif
			stx btn_f_time
			clc
			rts
		endif
		cmp #03
		ifeq
			lda nobut
			bne ?nope
			jsr getswitch
            asl A
            ifpl
				asl btn_s_time
				rts
			endif
			stx btn_s_time
			clc
			rts
		endif
		clc
		rts

;************************************************
    .sbttl "Alpha CPU Test Routine"
;************************************************
?alphat lda #$F7
        ldx #spot_vpg
        jsr vgadd2
        
        ldx #stmatest     
        jsr stmesg          ;'--- Alpha CPU ---'
        ldx #stmtestra     
        jsr stmesg          ;'RAM TEST:'
        ldx #stmtestro     
        jsr stmesg          ;'ROM TEST:'
        ldx #stmrammain     
        jsr stmesg          ;'ALPHA MAIN'
        ldx #stmramswap     
        jsr stmesg          ;'ALPHA SWAP'
        ldx #stmvectral     
        jsr stmesg          ;'VECTOR LOW'
        ldx #stmvectrah     
        jsr stmesg          ;'VECTOR HIGH'
        ldx #stmacomm    
        jsr stmesg          ;'ALPHA COMM'     
        ldx #stmromlow
        jsr stmesg          ;'ALPHA MAIN LOW'
        ldx #stmromhig
        jsr stmesg          ;'ALPHA MAIN HIGH'
        ldx #stmrompg0
        jsr stmesg          ;'ALPHA PAGE0'
        ldx #stmrompg1
        jsr stmesg          ;'ALPHA PAGE1'
        ldx #stmrompg2
        jsr stmesg          ;'ALPHA PAGE2'
        ldx #stmrompg3
        jsr stmesg          ;'ALPHA PAGE3'
        ldx #stmrompg4
        jsr stmesg          ;'ALPHA PAGE4'
        ldx #stmrompg5
        jsr stmesg          ;'ALPHA PAGE5'
        ldx #stmrompg6
        jsr stmesg          ;'ALPHA PAGE6'
        ldx #stmrompg7
        jsr stmesg          ;'ALPHA PAGE7'
        ldx #stmvectrom
        jsr stmesg          ;'VECTOR MAIN '
        ldx #stmvectro0
        jsr stmesg          ;'VECTOR PAGE0'
        ldx #stmvectro1
        jsr stmesg          ;'VECTOR PAGE1'
        ldx #stmvectro2
        jsr stmesg          ;'VECTOR PAGE2'
        ldx #stmvectro3
        jsr stmesg          ;'VECTOR PAGE3' 
        ldx #stmpaux     
        jsr stmesg          ;'AUX COIN MSG'
        
		;doggie
		sta watchdog
		
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
		
		lda #06
		sta temp8			;Line height
        lda #$1A
        sta temp8+1         ;Starting Y value 
        ldx #IDENTIFIER_AL   
		ldy #IDENTIFIER_V3+1
		jsr shromsum

        ldx #04             ;Test this many RAM results
        stx temp8
        begin
            ldx temp8
            ldy ?msgpos,X
            lda erplc,X         ;Any bad news?
            ifne
                cpx #04
                ifeq
					lda has_beta
					bne ?rmf
					ldx #stmmnotuse  ;Hack for Alpha COMM RAM to show 'NA'
                else
?rmf           		ldx #stmmfail 
                endif
            else
                ldx #stmmpass
            endif
            ;jsr stpg4
            tya
            jsr stmesgv 
            dec temp8
        miend       
        rts
            
?moveit VGADD_VCTRS(0,24d/4,hidden)
		rts

;************************************************
;* Show ROM CSUMs on screen
;* 
;* temp8   = line height (6 for tight, 8 for normal)
;* temp8+1 = starting Y position
;* 
;* X   	   = starting ROM index
;* Y   	   = ending rom index + 1
;*
;* uses tempa,tempa+1
;************************************************
shromsum
		sty	tempa+1
		begin
            stx tempa           ;Save checksum #
            ;Get the pass/fail value from csum table
            lda pgcstbl,X
            ifne                ;Bad CSUM if not zero
                ldx #stmmfail 
            else
                ldx #stmmpass 
            endif
            ;Move current vertical position
            lda temp8+1
            sec 
            sbc temp8          ;32d below current line
            sta temp8+1
            ;Show Location Message
            lda temp8+1
            jsr stmesgv 
            ;Space after text
            jsr ?moveit
			;Show ROM Identifier
			ldx tempa
			lda pgidtbl,X
			clc
			adc #pgidtbl
			sec					;Set carry to not show leading zero
			ldy #01				;1 byte shown
			jsr digits          ;Display 2 digits (byte)
			;space after text
			jsr ?moveit
            ;Show the CSUM now if it was bad (not zero)    
            ldx tempa
            lda pgcstbl,X
            ifne 
				jsr ?moveit
                jsr setcolyellow
                lda tempa
                clc 
                adc #pgcstbl
                ldy #01
                jsr digits          ;Display two digits (1 byte)  
			else
				jsr ?moveit
				jsr ?moveit
				jsr ?moveit
				jsr ?moveit
				jsr ?moveit
            endif
			;ROM version
			lda tempa
			asl A				;words
			tax
			lda pgvetbl,X
			cmp temp9
			bne ?vng
			lda pgvetbl+1,X
			cmp temp9+1
			ifne
				;here if bad version, show it
?vng			jsr setcolred
				ldx tempa
				jsr showversion
			endif
			;finished
            ldx tempa
            inx
            cpx tempa+1
        eqend
		rts
		
showversion
		;temp8 contains the ROM page offset byte (0-15d)
		;lda temp8
		VGADD_JSR(char_v) 
		lda #pgvetbl
		clc 
		adc temp8			
		adc temp8			;This is temp8 x 2
		ldy #01				;1 byte shown
		sec
		jsr digits          ;Display 2 digits (byte)
		lda vgjchpe         ;Dot
		ldx vgjchpe+1
		jsr vgadd2
		lda #pgvetbl+1
		clc 
		adc temp8			
		adc temp8			;This is temp8 x 2
		ldy #01				;1 byte shown
		jsr digits          ;Display 2 digits (byte)
		rts
		
?msgpos .db $44,$3E,$38,$32,$2C

;*********************************************
; Generic Roller Handler for Self Test
; 
; This will poll the LETA and determine if 
; any self test selections driven by the roller
; need to change. It will simply increment
; or decrement rollcnd. The routine using it
; should monitor rollcnt to consume the data 
; and reset rollcnt to 0 when done.
;*********************************************
updroller
        jsr getleta         ;Display direct
        sta thisleta
        cmp lastleta
        ;was there any change at all?
        ifne
            lda thisleta
            ifpl
                lda lastleta
                ifpl
					;no transition, set direction
                    cmp thisleta
					lda #0 				;default: spinning backwards
					ifcc
						lda #$80        	;spinning forwards here
					endif 
					sta lastdir 
                else
                    ;transition, look at direction
                    lda lastdir
                    ifpl
                        inc rollcnt
                    else
                        dec rollcnt
                    endif
                endif
            else
                ;thisleta is minus here
                lda lastleta
                ifmi
                    ;no transition, set direction
                    cmp thisleta
					lda #$01         ;default: spinning forwards because down is up
                    ifcc
                        lda #$81         ;spinning backwards here      
                    endif
                    sta lastdir 
                else
                    ;minus to plus, look at direction
                    lda lastdir
                    ifpl
                        inc rollcnt
                    else
                        dec rollcnt
                    endif
                endif
            endif                          
            lda thisleta
            sta lastleta      
        endif
		rts

;*******************************************
    .sbttl "Gamma Test"
;*******************************************
;   gammaerr will contain:
;       00: Gamma is not responding
;       FF: Responding and fully functional
;       FX: Where X are bitflags as shown 
;           below.
;*******************************************
?gammat lda gammaerr            ;See if Gamma was bad 0=bad, should at least be negative
        ifpl                    ;Bad Gamma
            jsr ?shgdead			;Show 'GAMMA BAD'
            rts         			;RETURN HERE
        endif
        
        lda goteebad
        ifeq
            lda gammaerr
            bit ?bit08
            ifeq
                ;Set EEBAD Flags
                sei             ;Hold IRQ's 
                lda #g_eeflgs   ;Get EE Flags
                jsr xmitr
                cli
                sta eebad
            endif
            lda #$80
            sta goteebad
        endif
        
        ;static text
        lda #(?stgamma&$ff)
        sta stextp
        lda #(?stgamma/256d)
        sta stextp+1
        jsr statict
        
        ;Errors:  Bottom Bits represent errors if 0   
        ;   D0 = Gamma RAM Error
        ;   D1 = Gamma ROM Error
        ;   D2 = Gamma POKEY Error
        ;   D3 = Gamma EEROM Error
        lda gammaerr
        sta temp9
        lda #$44
        sta temp8+1
        ldx #$03
        stx temp8
        begin
            ldx #stmmpass
            lsr temp9           ;Check bit
            ifcc                ;Got an error
                ldx #stmmfail 
            endif
            jsr stpg4
            lda temp8+1
            jsr stmesgv        ;Show Pass/Fail at correct level
            ;Adjust height
            lda temp8+1
            sec 
            sbc #06            ;32d below current line
            sta temp8+1
            dec temp8
        miend
        ;we are positioned after EEROM pass/fail now, get some deets if FAIL
        lda vgjchsp         ;Add a <space>
        ldx vgjchsp+1
        jsr vgadd2
        lda #07
        sta temp2       ;Shows all 8 bits for the EEROM bad bytse 
						;eflg_scores     = 10000000
						;eflg_initials   = 01000000 
						;eflg_xwarp      = 00100000
						;eflg_game       = 00010000
						;eflg_maze       = 00001000
						;eflg_option     = 00000100
						;eflg_warpco     = 00000010
						;eflg_tesser     = 00000001
        lda eebad
        ifne
            jsr dis01
        endif

		jsr doconfirm
      
        ;Check Fire switch
        lda #2
		jsr chkbut
		ifcs
			lda nxtcmd
			cmp #g_clrh           ;Clear high scores??
			ifeq
?toconf         lda #$80
				sta bconfirm
			else
				cmp #g_clrs			;Clear Game Stats
				beq ?toconf
				cmp #g_clro			;Clear Options to DIP Settings
				beq ?toconf
				lda nxtcmd
				jsr dosnd2      	;send this one, it doesn't require a confirmation
			endif
		endif
 
        ;Check Shield Switch, only if waiting for confirm
        lda bconfirm
        ifne
            lda #3
			jsr chkbut
			ifcs
				lda nxtcmd
				cmp #g_clrh		;Clear high scores??
				ifeq
?clrdat             pha
					jsr	xmit     ;Make some beeps
					jsr dodelay     ;Wait a bit
					sei             ;Hold IRQ's 
					pla
					jsr xmit        ;Send it away
					cli
					lda	#snd_c4
				else
					cmp #g_clrs		;Clear Game Stats
					beq ?clrdat
					cmp #g_clro		;Clear Options to DIP Settings
					beq ?clrdat
				endif
				lda #00
				sta bconfirm
			endif
        endif       
        sta watchdog        ;kick the doggie
        ;update with spinner       
		jsr updroller
		
		lda rollcnt
		ifne
			ifpl
				jsr ?incnxtsnd
				dec rollcnt			
			else
				jsr ?decnxtsnd
				inc rollcnt
			endif
			;stop anything playing already
			lda #g_sndstop
			jsr xmit
			;clear some stuff
			lda #00
			sta btn_s_time         ;Don't step right away again
			sta bconfirm           ;If we were confirming, clear this too
			;sta rollcnt
		endif
        
        ;message display section
        lda nxtcmd
        pha 
        sec
        sbc #g_clrh     ;Base Command number
        cmp #(g_sndstop-g_clrh+1)                  
        ifge                ;A Sound??
            lda #03             ;Use the 'Sound' message
        endif
        and #03
        tay 
        ldx ?st_tmesg,Y
        jsr stmesg          ;Display message     
        pla                 ;See if a sound
        cmp #g_sndstop+1
        ifge
            pha
            clc
            sbc #g_sndstop    ;This makes the base sound = Sound #01
            sta temp2           ;Sound numbers here
            lda #temp2
            ldy #01
            clc 
            jsr digits          ;Display this song
            lda vgjchop          ;Open Paren
            ldx vgjchop+1
            jsr vgadd2
            pla                 ;Get raw value
            sta temp2           ;Sound numbers here
            lda #temp2
            ldy #01
            sec 
            jsr digits          ;Display this song
            lda vgjchcp          ;Close Paren
            ldx vgjchcp+1
            jsr vgadd2  
        endif
        rts

doconfirm
		;Show confirm message if needed
        lda bconfirm
        ifne
            lda frame
            and #$04
            ifeq
                ldx #stmpshield  
                jsr stmesg          ;'PRESS SHIELD TO CONFIRM RESET'
            endif
        endif
		rts
		
;Show temp2 bits(0 based) as 1 or 0   
     
dis01  	begin
            pha 
            and #01
            jsr vghex               ;Display 0 or 1
            pla 
            ror A
            dec temp2
        miend
        rts		
		
?shgdead
        ldx #stmgamad  
        jsr stmesg 
		rts
		
?incnxtsnd   
        lda nxtcmd
        clc
        adc #01
        cmp #$snd_off         ;this is the last sound command (stop music)
        ifge
            lda #g_clrh
        endif
        sta nxtcmd
        rts
        
?decnxtsnd   
        lda nxtcmd
        clc
        sbc #01
        cmp #g_clrh 
        iflt
            lda #$snd_off-1  
        endif
        sta nxtcmd
        rts        

;Static text indexes for GAMMA test
?stgamma 
		.db stmgtest,stmtestgde,stmgammram,stmgammrom,stmgammpok,stmgammeer,stmgamcmd,stmpaux,stmprshld,stmpfire,-1
        
?st_tmesg   .db stmrstaths  ;"RESET SCORES"
            .db stmrstatgs  ;"RESET STATS"
            .db stmsoundof  ;"SOUNDS OFF"
            .db stmsoundn   ;"SOUND "
            
diftbl      .db stmactd ;"EASY"
            .db stmacte ;"MEDIUM"
            .db stmactf ;"HARD"
            .db stmactc ;"DEMO"
			
setcolyellow
        lda #($F0+colyellow)
        bne scol

setcolred
		lda #($F0+colred2)
		bne scol
		
setcolwhite
        lda #($F0 + colwhite)
scol   	ldx #$60
        jmp vgadd2         
  
warpcolrtp1
		lda #5
		sta rompg
		lda warpcol,X
		tax
		lda #1		;**** WARNING: Switch to page where tw_test2.asm is
		sta rompg
		txa
		rts

;**********************************************
    .sbttl "Warp Codes Setup"
;**********************************************
?testwarp
		jsr doconfirm		;Show confirm if needed
		lda gammawait
		ifne
			;Waiting for Gamma to return clear
			jsr getgdon
			ifeq
				;yes, it is done
				lda #g_sndstop
				jsr dosnd2
				lda #0				;Clear the data flag so it is fetched again
				sta gotdata
				sta gammawait
				sta bconfirm
				sta nobut			;Normalize operation again
			else
				;Waste some time
				jsr do3del
			endif
			rts
		endif
        ;This page will use data in TWTACT
        jsr stpg5
        lda gotdata
        ifeq
            jsr wrpin        ;Get the Warp Stats only once per self test
            lda #$80
            sta gotdata
			lda #0
			sta setsel		;Setsel starts at zero
        endif
        lda #(?stwarp&$ff)
        sta stextp
        lda #(?stwarp/256d)
        sta stextp+1
        jsr statict
        ;Show warp codes
        lda #$10
        sta temp6               ;Starting location for Warp codes
        lda #g_numwarps-1
        sta temp7
        begin
            asl A               ;Warp index x2
            tax
            lda warpcodes,X
            cmp #-1
            ifeq
?wrpno          ;Nothing defined, static message here;
                ldx #stmwarpn
                lda temp6
                jsr stmesgv
                jmp ?wrpnxt
            else
                lda warpcodes+1,X
                cmp #-1
                beq ?wrpno
            endif
            ;if we are here, then show a loaded warp number
            lda #02
            jsr vgscalz          ;Difference Scale for positioning
            jsr vgcntr
            lda #$E0
            ldx temp6
            jsr vgvtr5          ;Position for digits
            lda #02
            ldy #$30
            jsr vgscal          ;Set scale
            ldy temp7
            lda warpcol,Y
            ldx #$60
            jsr vgadd2          ;Set Color
            ldy temp7  
            lda wdigits,Y
            sta perm4           ;Number of digits (2 or 3)
            tya
            asl A               ;x2
            tax                 ;X is index into warpcodes data  
            lda warpcodes,X
            sta perm5
            lda warpcodes+1,X
            sta perm5+1
            clc
            ldy perm4
            lda #perm5
            jsr sdigits
            ;adjust our text downwards for next time
?wrpnxt     lda temp6
            clc
            adc #$08
            sta temp6
            ;one less
            dec temp7
            lda temp7
        miend
		;Show any commands depending on setsel
		ldx setsel
		ifne
			dex
			lda ?wrpmsg,X
			tax
			jsr stmesg    
		endif
		;Fire Buton
		lda #2
		jsr chkbut
		ifcs
			;move to next
			inc setsel
			lda setsel
			cmp #03
			ifge
				lda #$01
				sta setsel
			endif
		endif
		;Shield Button
		lda #3
		jsr chkbut
		ifcs
			ldx setsel
			ifne
				lda bconfirm
				ifeq
					lda #-1
					sta bconfirm
				else
					;reactor hum
					lda #snd_b1d
					jsr xmit
					jsr do3del
					;send the command
					sei             ;Hold IRQ's please
					ldx setsel
					lda ?wrpcmd-1,X
					jsr xmit       ;Do retry xmit (no recieve)
					cli
					lda #0
					sta setsel
					lda #-1
					sta gammawait
					sta nobut
					sta bconfirm
				endif
			endif
		endif
        rts
        

?stwarp .db stmwarpt,stmwarpr,stmwarpy,stmwarpg,stmwarpa,stmwarpb,stmwarpp,stmwarpk,stmwarp,stmwarpc,stmpaux,stmfiresel,-1, ;stmwshlds,-1
?wrpmsg .db stmwmsg1,stmwmsg2;,stmwmsg3
?wrpcmd	.db g_classwarp,g_genwarp;,g_clrwarp

;***********************************************
;* Static Text Output
;* Will output a list of static text for each
;* page that needs it.
;*
;* Calling routine should save pointer to table
;* of static text bytes in stextp
;*
;* Routine will loop until it hits a byte in
;* text table with a negative value
;***********************************************
statict 
		lda #00
        sta stexti
        begin
            ldy stexti
            lda (stextp,Y)
            ifpl
                tax
                jsr stmesg 
                inc stexti
            endif
        miend
        rts        
        
;**********************************************************
    .sbttl "RAM Test Routines"
;**********************************************************
;* Assumes Page 9 has been tested and is good! 

?vram       =   $0900       ;Use 900 page, we know it is good
?tstart     =   $9F0
?tend       =   $9F2

?bit08		.db 08
;**********************************************************
;* Entry: (A)  = MSB of starting address                  
;*                                                        
;* Exit:  (CC) = Carry set if error occured               
;*        (A)  = Difference between expected and recieved 
;**********************************************************

?tstram 
		sta ?tstart         ;Save MSB of address
        clc 
        adc #$0F            ;Add for end of 4K x 8
?tst2k  sta ?tend

;**********************************************************
    .sbttl "Generic RAM Test Code"
;**********************************************************

?ramtst ldx #00         	;Enter here to test RAM
?nxtpat ldy #vtend-?voltbl
?grt10  lda ?voltbl,Y        ;Move volitile code into RAM
        sta ?vram,Y
        dey 
        bpl ?grt10
        lda ?tstart
        sta vadh1
        sta vadh2
        sta vadh3
        lda ?tend
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
        jmp ?vram
        
;************************************************************
    .sbttl "Crashable Code"
;************************************************************

?voltbl  sta $100

vadl1       =   *-?voltbl-2+?vram
vadh1       =   *-?voltbl-1+?vram

        cpy $101
        
vadl2       =   *-?voltbl-2+?vram
vadh2       =   *-?voltbl-1+?vram

        bne ?cc50
    
vpat        =   *-?voltbl-1+?vram       ;Set to zero for the inital pass

        cmp $100
        
vadl3       =   *-?voltbl-2+?vram
vadh3       =   *-?voltbl-1+?vram

        bne ?cc50               ;RAM Failure
        inc vadl1
        bne ?cc10
        inc vadh1
?cc10   inc vadl2
        bne ?cc30
        inc vadh2
        sta watchdog
        ldy ?tend
        cpy vadh2
        bcs ?cc20
        sta $01FF               ;Set last byte for next pass
        
vadh4   =   *-?voltbl-1+?vram

        jmp ?nxtpat
?cc20   ldy pats,X
?cc30   inc vadl3
        bne ?cc40
        inc vadh3
?cc40   jmp ?vram
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
		
;*****************************************
;* Pulse Light 
;*****************************************
pulselig
		;rts
		lda out1s             ;Lamp On, never reset processors
		ora #(o_lighton)
		sta outputs
		ldx #0
		begin
			dex
		eqend
		and #~(o_lighton)
		sta outputs
		rts
		
;*********************************************
    .TEXT ".TWSTMSG."
;*********************************************
    .title "Self Test Message Routines"
    .sbttl "Self Test Message Data"
;*********************************************
;* Message Macro                             *
;* Will build an absolute address table here *
;* of each message. Space for table has to be*
;* initially declared adequately. The macro  *
;* will also define a new label for each     *
;* message that can be used to get the       *
;* proper index into the message table.      *
;*********************************************
__stmsnum = 0
__stnmsgs = 100d
__sten = $
__stst = __sten+(__stnmsgs*2)
__stvx = __stst+__stnmsgs    
__stvy = __stvx+__stnmsgs
.org __stvy +__stnmsgs       ;Code follows tables

;Pretty Table Bases
msgtblen = __sten
msgtblst = __stst
msgtblvx = __stvx
msgtblvy = __stvy

#define     stmess(xlitst,xcolst,xscast,xpost,ypost)  \ .org __sten
#defcont    \ .word xlitst
#defcont    \__sten .set __sten+2
#defcont    \stm+xlitst = __stmsnum
#defcont    \__stmsnum .set __stmsnum+1
#defcont    \ .org __stst
#defcont    \ .byte ((xcolst*$10)|(xscast))
#defcont    \__stst .set __stst+1
#defcont    \ .org __stvx
#defcont    \ .byte xpost
#defcont    \__stvx .set __stvx+1
#defcont    \ .org __stvy
#defcont    \ .byte ypost
#defcont    \__stvy .set __stvy+1
#defcont    \#if (__stmsnum > __stnmsgs)
#defcont    \ .error "STMSG: Number of messages exceeds defined limit, increase please!"
#defcont    \#endif 


;******************************************************************************
    .sbttl "Self Test Messages"
;******************************************************************************
        ;MESSAGE, Color, Scale, xPos, YPos  
        stmess(paux,colpurple,1,$84,-$50)       
        stmess(prshld,colyellow,1,$84,-$58)
        stmess(pfire,colred,1,$84,-$60)  
        stmess(pshield,colorange,1,$A4,-$28)          
        stmess(gtest,colorange,1,$C6,$60)    
        stmess(atest,colorange,1,$C6,$60)     
		stmess(btest,colorange,1,$CA,$60)    
        stmess(gmstat,colorange,1,$B6,$60)
        stmess(warpt,colorange,1,$D0,$60)
        stmess(inpout,colorange,1,$B6,$60)
        stmess(act1pg,colgreen,1,$88,$50)
        stmess(act2pg,colgreen,1,$88,$48)
        stmess(actwrps,colgreen,1,$9B,$38)
        stmess(actxliv,colgreen,1,$95,$20)
        stmess(avggtim,colwhite,1,$C0,$28)
        stmess(rstaths,colwhite,1,$CA,0)
        stmess(rstatgs,colwhite,1,$DC,0)
        stmess(rstatop,colwhite,1,$DC,0)
        ;stmess(ccl,colwhite,1,$D2,0) 
        ;stmess(ccr,colwhite,1,$D2,0)
        stmess(soundof,colwhite,1,$DC,0)
        stmess(soundn,colwhite,1,$E6,0)             
        stmess(testra,colblue,1,$84,$50)      
        stmess(testro,colblue,1,$84,$20)
        stmess(rammain,colwhite,1,$B2,$44) 
		stmess(brammain,colwhite,1,$B2,$44) 		
        stmess(ramswap,colwhite,1,$B2,$3E)
        stmess(vectral,colwhite,1,$B8,$38) 
        stmess(vectrah,colwhite,1,$B2,$32)
        stmess(acomm,colwhite,1,$B8,$2C)   
		stmess(bcomm,colwhite,1,$B8,$3C)  
        stmess(mpass,colgreen,1,$F0,$38)  
        stmess(mfail,colred2,1,$F0,$38) 
        stmess(mnotuse,colyellow,1,$10,$38)
        stmess(romlow,colwhite,1,$BE,$14)
        stmess(romhig,colwhite,1,$BE,$0E)
		stmess(bromlow,colwhite,1,$C4,$14)
        stmess(bromhig,colwhite,1,$C4,$0C)
        stmess(rompg0,colwhite,1,$BE,$08)
        stmess(rompg1,colwhite,1,$BE,$02)
        stmess(rompg2,colwhite,1,$BE,-$04)
        stmess(rompg3,colwhite,1,$BE,-$0A)         
        stmess(rompg4,colwhite,1,$BE,-$10) 
        stmess(rompg5,colwhite,1,$BE,-$16)          
        stmess(rompg6,colwhite,1,$BE,-$1C)
        stmess(rompg7,colwhite,1,$BE,-$22)
        stmess(vectrom,colwhite,1,$BE,-$28)
        stmess(vectro0,colwhite,1,$BE,-$2E)
        stmess(vectro1,colwhite,1,$BE,-$34)
        stmess(vectro2,colwhite,1,$BE,-$3A)
        stmess(vectro3,colwhite,1,$BE,-$40)     
		stmess(vermiss,colred,1,$D0,$20)
        stmess(testgde,colblue,1,$84,$50)
        stmess(gammram,colwhite,1,$94,$44)
        stmess(gammrom,colwhite,1,$94,$3E)
        stmess(gammpok,colwhite,1,$94,$38)     
        stmess(gammeer,colwhite,1,$94,$32)   
        stmess(gamcmd,colblue,1,$84,$10)
        stmess(warpr,colred2,1,$A7,$40)
        stmess(warpy,colyellow,1,$94,$38)
        stmess(warpg,colgreen,1,$98,$30)
        stmess(warpa,colcyan,1,$A1,$28)
        stmess(warpb,colblue,1,$A1,$20)
        stmess(warpp,colpurple,1,$94,$18)
        stmess(warpk,colpink,1,$A1,$10)

        stmess(warp,colwhite,1,$98,$50)
        stmess(warpc,colwhite,1,$E0,$50)
        stmess(warpn,colwhite,1,$E0,$40)
		stmess(gamad,colred2,1,$E0,$0)
		stmess(gsetts,colorange,1,$C8,$60)
		
		;stmess(pricin,colwhite,1,$94,$40) 
		;stmess(strlif,colwhite,1,$94,$38)   
		;stmess(gamdif,colwhite,1,$94,$30)   
		;stmess(bonlif,colwhite,1,$94,$28)   
		;stmess(attsnd,colwhite,1,$94,$20) 
		;stmess(adadif,colwhite,1,$94,$18)  
		
		stmess(cfree,colgreen,1,-80,$40)     
        stmess(c1p1,colgreen,1,-80,$40)    
        stmess(c1p2,colgreen,1,-80,$40)    
        stmess(c2p1,colgreen,1,-80,$40) 
		
		stmess(actc,colgreen,1,-80,$38)
        stmess(actd,colgreen,1,-80,$38)
        stmess(acte,colgreen,1,-80,$38)
        stmess(actf,colgreen,1,-80,$38)
		
		stmess(livem,colgreen,1,-76,$30)
		
		stmess(bon50,colgreen,1,-80,$28)  
		stmess(bon100,colgreen,1,-80,$28)  
		stmess(bon200,colgreen,1,-80,$28)  
		stmess(bonno,colgreen,1,-80,$28) 
		
		stmess(sndon,colgreen,1,-80,$20) 
		stmess(sndoff,colred,1,-80,$20) 
		
		stmess(rollx1,colgreen,1,-80,$18)
		stmess(rollx2,colgreen,1,-80,$18)
		;stmess(addon,colgreen,1,-80,$18) 
		;stmess(addoff,colred,1,-80,$18) 
		
		stmess(lddips,colwhite,1,-80,$08)
    
		stmess(firesel,colred,1,$84,-$58)
		stmess(rollsel,colyellow,1,$84,-$60)		
		stmess(pshelds,colpink,1,$84,-$68)

		stmess(wmsg1,colyellow,1,-$50,-$20)
		stmess(wmsg2,colyellow,1,-$50,-$20)
		;stmess(wmsg3,colyellow,1,-$50,-$20)
		;stmess(wshlds,colyellow,1,$84,-$60);
		
;************************** 
;Message Strings
;**************************
#IF LANGUAGE = 2
#include "tw_teststr_fr.asm"
#ELSE
#IF LANGUAGE = 1
#include "tw_teststr_de.asm"
#ELSE
#include "tw_teststr_en.asm"
#ENDIF
#ENDIF

;Bonus Messages
bonusmg .byte stmbonno,stmbon50,stmbon100,stmbon200

;Coin Mode Messages
stcmodz	.byte stmcfree,stmc1p2,stmc1p1,stmc2p1  

                  
;*******************************************************
    .sbttl "Self Test Main Message Routine"
;*******************************************************
;* Will output specified message to specified location 
;* on screen.                                          
;*                             
;* stmesg - Normal Message routine, uses all defaults                       
;*  Inputs: X = message # 
;*
;* stmesgv - Override Vertical Position
;*  Inputs: X = message # 
;*          A = vertical position value
;*
;* stmesgc - Override Color and Scale
;*  Inputs: X = message # 
;*	        A = Color 
;*
;* Internal Parameters:                
;*
;*    temp4 - Message Index (Limited to 256 entries)
;*  temp2+1 - Color and Scale
;*    temp2 - Y Position of Text
;*    temp1 - X Position Text
;*  litrast - ZP Pointer to Msg Table for Indexed Indirect Addressing mode
;*******************************************************
stmesgc	sta temp2+1
		stx temp4
		lda msgtblvy,X        ;Store Y Position
		sta temp2
		jmp ?stms

stmesgv sta temp2
		stx temp4
        jmp stmesg_
        
stmesg 	lda msgtblvy,X        ;Store Y Position
        stx temp4
        sta temp2
stmesg_ lda #0
		sta temp2+1				;Clear Color + Scale Override (signifies that 
								;this won't be used and table value will be)
?stms   lda msgtblvx,X        	;Get the X Position 
        sta temp1
        jsr vgcntr 
        lda #00
        sta vgbrit
        lda #02
        jsr vgscalz              ;a=binary scaling value, linear scaling value = 0
        lda temp1
        ldx temp2
        jsr vgvtr1              ;Position Beam (Use vgbrit) 
        lda temp4               ;Set up ptr to literal
        asl A                   ;X2 for words
        tay
        lda (litrast,Y)         ;Get X (vert) position
        sta vdata
        iny 
        lda (litrast,Y)
        sta vdata+1
		;begin color and scale stuff
		lda temp2+1
		ifeq
			ldx temp4
			lda msgtblst,X          ;color and scale of text now
		endif
        pha 
        lsr A
        lsr A
        lsr A
        lsr A
        ora #$F0
        tay 
        lda #00
        jsr vgstat              ;Set color
        pla 					;Get original data back
        and #$0F
        clc 
        adc #01
        ldy #$30
        jsr vgscal              ;Set scale
		;end color and scale stuff
        lda #00                 ;Init vglist offset
        sta temp1
		tay						;Y starts at zero
        begin
            lda (vdata,Y)               ;Get character representation
            sta temp2
            and #$7F
            iny 
            sty temp3
            tax 
            lda vgmsga,X                ;Get correct JSRL
            ldy temp1
            sta (vglist,Y)
            iny 
            lda vgmsga+1,X
            sta (vglist,Y)
            iny 
            sty temp1                   ;Save y
            ldy temp3                   ;Get character ptr
            bit temp2                   ;if not end of string
        miend
        ldy temp1
        dey 
        jmp vgadd
