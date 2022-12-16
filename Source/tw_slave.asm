;*************************************************
; TWSLAVE
;*************************************************
    .sbttl "Gamma Request Routines"         
;************************************************* 
gethsall
		sei 
        lda #$80
        sta nogamma     ;Stop Gamma requests
        cli 
        lda #g_sendh        ;Request High Scores First
        jsr xmit            ;Send it away
        ldx #g_numsths-1        ;Put these away here
        begin
            jsr rcv         ;Get Input
            sta hscore,X        ;Save it
            dex
        miend
        lda #g_sendi        ;Now Get Initials
        jsr xmit
        ldx #g_numstit-1        ;Number of Initials
        begin
            jsr rcv
            sta initl,X
            dex
        miend
		lda #g_sendtes        ;Now Get Tesser data
        jsr xmit
        ldx #g_numtessr-1        ;Number of Tessers
        begin
            jsr rcv
            sta gtesser,X
            dex
        miend
        lda #00
        sta nogamma     ;Gamma Stuff allowed again
        cli
        rts

;******************************************
; Get Defined Warp Codes from Gamma
;******************************************
wrpin   sei 			;Hold IRQ's
        lda #$80
        sta nogamma     ;Stop Gamma requests
        cli 
        lda #g_swarps       ;Request All Warp Codes
        jsr xmit            ;Send it away
        ldx #(g_numwarps*2)-1       ;Put these away here
        begin
            jsr rcv         		;Get Input
            sta warpcodes,X			;Save it
            dex
        miend
        lda #00
        sta nogamma     ;Gamma Stuff allowed again
        cli
        rts

;******************************************
; Generate a Warp Code in GAMMA, return
; new value
; A = Warp Index (Zero Based)
;******************************************        
wrp1in  sei     ;Hold IRQ's
        tay     ;Save to send later
        lda #$80
        sta nogamma     ;Stop IRQ's from sending
        cli 
        begin
            lda #g_gwarp    ;Say we are earning a warp
            jsr xmitr       ;Returns gamma xmit in (A)
            cmp #01
        eqend
        ;Ready for xfer to Gamma
        lda #00
        sta olddata     ;In case something comes back unexpected
        txa
        ldx #00
        begin
            tya           ;Put original Warp level back into A, we are sending this
            jsr xmit        ;Send this byte
            sta watchdog
            jsr dodelay     ;Wait before next send
            lda olddata     ;Not expecting anything back
            ifne                ;Got something
                cmp #02         ;Did Gamma Time Out??
                beq wrp1in      ;Yep, Try again!
                brk             ;Nope, ERROR! Stay here??
            endif
            inx 
            cpx #01         ;We are sending exactly 1 byte here
        csend           ;All data sent, should get 'done' back from Gamma
        sta watchdog
        ldx #2-1        ;We will get back two bytes
        begin
            jsr rcv         ;Get Input
            sta temp1,X        ;Save it
            dex
        miend
        tya         ;Get back original warp offset again
        asl A
        tax
        lda temp1
        sta warpcodes,X
        lda temp1+1
        sta warpcodes+1,X           ;Saved HURRAY!
        ;Allow Other Commands
        lda #00
        sta nogamma
        cli
        rts 

rcv     lda #i_xmigama
        begin
            bit inputs
        neend
        lda portrd      ;Get Data
        rts
        
dosound bit gamest      ;Only during game play
        ifmi
dosnd2      jsr xmit            ;Send Data
            cli
        endif
        rts
        
getswitch   
        sei             ;Hold IRQ's please
        lda #g_swtc     ;Request for Switches
        jsr xmitr       ;Do retry xmit and recieve
        cli
        rts
        
getleta sei             ;Hold IRQ's please
        lda #g_ctrl     ;Request for Leta
        jsr xmitr       ;Do retry xmit and recieve
        cli
        rts
            
getrand sei             ;Hold IRQ's
        lda #g_rand     ;Request for Pokey Rand
        jsr xmitr       ;retry xmit and recieve
        cli 
        rts  

getgdon	sei             ;Hold IRQ's
        lda #g_eedone   ;Request for EEROM requests
        jsr xmitr       ;xmit and recieve
        cli 
        rts 		

;*************************************************************
; Option Switches Master Reference from GAMMA
;
; Game Pricing @8S - Default off OFF
; MSB						  LSB
; 1   2   3   4   5   6   7   8 
;---------------------------------
; 						 Off Off	1 Coin 1 Play (Default)
; 						 On  On		1 Coin 2 Plays
; 						 On  Off	2 Coins 1 Play
; 						 Off On		Free Play
;
; Game Settings @12/13S - Default all OFF
; MSB						  LSB
; 1   2   3   4   5   6   7   8 
;---------------------------------
; Off Off							3 Lives (Default)
; On  On							4 Lives
; On  Off							5 Lives 
; Off On							6 Lives 
; 		  On  On 					Hard Difficulty
;         Off Off					Medium Diffiulty (Default)
;		  Off On					Easy Difficulty
;         On  Off					DEMO Mode
;                 On  On 			Bonus @ 50,000
;                 Off Off			Bonus @ 100,000 (Default)
;                 Off On			Bonus @ 200,000
;                 On  Off			No Bonus
;   					   On		Silent Attract Mode
;                          Off 		Sound in Attract Mode (Default)
;                              On   Roller Multiplier x2
;                              Off  Roller Multiplier x1 (Default)
;       
;*******************************************************************
;00	50K			10	01
;01	No Bonus	11	00
;10	200K		01  11
;11 100K		00	10
;
; mask=10
;00 Hard	10
;11 Medium	01
;10 Easy	00
;01 DEMO	11
;
; mask= 01
;00 1C2P  01
;10 2C1P  11
;01 FREE  00
;11 1C1P  10

;dip_emask0	= $E5 	;$E4
;dip_emask1	= $01


getoptn sei             ;Hold IRQ's 
        lda #g_getset1  ;PRICE Options @ 8S - Straight DIPS
        jsr xmitr
		;TODO: See if we can take this weird config away, it appears that this was the 'quick' way to change 
		;the default coin mode from 1 coin 1 play to 2 coins 1 play.
		;eor #dip_emask1	;Invert switch #7, this is normal operation.
        and #03			;Only allow through Coin Mode bits
		sta c_cmode     ;cmode after mask
						; 00 = Free Play
						; 01 = 1 Coin 2 Plays
						; 02 = 1 Coin 1 Play
						; 03 = 2 Coin 1 Play
		;Wait some time for GAMMA to settle
        jsr dodelay     
        lda #g_getset0  ;PLAY Options @ 13/14S - POKEY DIPS (inverted)
        jsr xmitr
        cli 
		;set up all RAM now with different settings
		;eor #dip_emask0	;#$23            ;So all off is default
		tay 				;Keep EOR'ed data in Y for many times below
		;Roller Multiplier
        and #01
        sta rollmult        ;Roller multiplier 0 = 1x, 1 = 2x
        tya 

        lsr A
        and #01
        sta sndatt      	;Sounds in Attract
        tya 
        lsr A
        lsr A
        and #03         	;Bonus Life bottom 3 bits
        sta nxtbonus        ;Bonus Score level	
							; 00 = No Bonus
							; 01 = 50K Bonus
							; 02 = 100K Bonus
							; 03 = 200K Bonus
        tya 
        lsr A
        lsr A
        lsr A
        lsr A           	;Next two bits = Difficulty
        and #03
		sta rawgamedif     	;Save base Difficulty
							; 00 = Easy 
							; 01 = Medium
							; 02 = Hard
							; 03 = Demo/Medium)
        cmp #03
        ifeq				;demo mode 
			lda #1				;always medium difficulty
			ldx #-1		
		else
			ldx #0
        endif
		stx demo
		sta gamedif
        tya             ;Get options again
        rol A
        rol A
        rol A           ;Top two bits
        and #03
        tax 
        lda gamlvs,X
#IF (((DEBUG != 0) & (NUMLIVES != 0)) | ((LEVEL_EDITOR != 0) & (NUMLIVES != 0)))
        lda #NUMLIVES
#ENDIF
        sta slives
        rts 
		
		
;Send Play Options in A to GAMMA		
setoptn0 
		ldx #g_setopt0
		;eor #dip_emask0		;Mask back to correct values
		jmp ?sendone		;send X and then A

;Send Pricing Options in A to GAMMA		
setoptn1 
		;eor #dip_emask1		;Inverts
		ldx #g_setopt1
		jmp ?sendone		;send X and then A
		
;Send Max sound variable
setmaxv	ldx #g_sendmaxsnd
		jmp ?sendone		;send X and then A
		

;************************************************
; Send Command + 1 Byte
;
; Will send the Command in X and then the value
; in A, then returns response from gamma in A
;************************************************	
?sendone
		pha				;Command to send onto Stack
		sei             ;Hold IRQ's 
		lda #$80
		sta nogamma
		txa     		;Send set command
        jsr xmitr     	;Gamma sends #1 when ready
		cmp #01
		pla
		jsr xmit        ;Send this byte
        begin
            lda #i_xmigama
            bit inputs
        neend
        lda portrd
        pha				;save return data
		lda #00
        sta nogamma
		pla
        cli
        rts 
		
;***********************************************************
; Send High Score, Initial and Tesser data
;***********************************************************        
sendhs  sei             ;Hold IRQ's 
        lda #$80
        sta nogamma     ;Stop IRQ's from sending
        cli 
        begin
            lda #g_geth     ;Send request "gamma get scores"
            jsr xmitr       ;Returns gamma xmit in (A)
            cmp #01
        eqend
        ;
        ;  Ready for xfer to Gamma
        ;
        lda #00
        sta olddata     ;In case something comes back unexpected
        ldx #00
        begin
            lda hscore,X
            jsr xmit        ;Send this byte
            sta watchdog
            jsr dodelay     ;Wait before next send
            lda olddata     ;Not expecting anything back
            ifne                ;Got something
                cmp #02         ;Did Gamma Time Out??
                beq sendhs      ;Yep, Try again!
                brk             ;Nope, ERROR! Stay here??
            endif
            inx 
            cpx #g_numsths
        csend           ;All HS sent, should get 'done' back from Gamma
        begin
            lda #i_xmigama
            bit inputs
        neend
        lda portrd
        cmp #03         ;Okay??
        ifne
            brk             ;Not okay
        endif
        sta watchdog
        jsr do3del      ;Delay * 3
        ;
        ;All High Scores there okay
        ;Now send Initials
        ;
?sendit begin	
			lda #g_geti     ;Now do initials
			jsr xmitr       ;Retry xmit and recieve
			cmp #01         ;Gamma ready??
        eqend
        ;
        ;Ready for Initials
        ;
        lda #00
        sta olddata     ;Nothing here I hope
        ldx #00
        begin
            lda initl,X
            jsr xmit
            sta watchdog
            jsr dodelay
            lda olddata     ;Anything?? (I hope not!)
            ifne
                cmp #02         ;Timed out Gamma
                beq ?sendit
                brk             ;Else, something goofy
            endif
            inx
            cpx #g_numstit
        csend           ;Okay, all sent, now what about status??
        begin
            lda #i_xmigama
            bit inputs
        neend
        lda portrd      ;It sent something
        cmp #03         ;Done??
        ifne
            brk
        endif
        jsr do3del      ;Wait before next
?sendts begin	
			lda #g_getts     ;Now do initials
			jsr xmitr       ;Retry xmit and recieve
			cmp #01         ;Gamma ready??
        eqend
        ;
        ;Ready for tessers
        ;
        lda #00
        sta olddata     ;Nothing here I hope
        ldx #00
        begin
            lda gtesser,X
            jsr xmit
            sta watchdog
            jsr dodelay
            lda olddata     ;Anything?? (I hope not!)
            ifne
                cmp #02         ;Timed out Gamma
                beq ?sendts
                brk             ;Else, something goofy
            endif
            inx
            cpx #g_numtessr
        csend           ;Okay, all sent, now what about status??
        begin
            lda #i_xmigama
            bit inputs
        neend
        lda portrd      ;It sent something
        cmp #03         ;Done??
        ifne
            brk
        endif
        ;jsr do3del      ;Wait before next? Maybe not needed?
        ;Allow IRQ Sends again
        lda #00
        sta nogamma
        cli
        rts 


;************************************************************
;* Send Game Stats to Gamma
;************************************************************
sendgstat
        begin
            sei             ;Hold IRQ's 
            lda #$80
            sta nogamma     ;Stop IRQ's from sending
            cli 
            lda #g_gstap    ;Send request "Gamma Get Game Stats"
            jsr xmitr       ;Returns gamma xmit in (A)
            cmp #01
        eqend
        ;
        ;  Ready for xfer to Gamma
        ;
        lda #00
        sta olddata     ;In case something comes back unexpected
        ldx #00
        begin
            lda st_gtime1,X
            jsr xmit        ;Send this byte
            sta watchdog
            jsr dodelay     ;Wait before next send
            lda olddata     ;Not expecting anything back
            ifne                ;Got something
                cmp #02         ;Did Gamma Time Out??
                beq sendgstat   ;Yep, Try again!
                brk             ;Nope, ERROR! Stay here??
            endif
            inx 
            cpx #08         ;4 bytes per player = 8
        csend           ;All data sent, should get 'done' back from Gamma
        begin
            lda #i_xmigama
            bit inputs
        neend
        lda portrd
        cmp #03         ;Okay??
        ifne
            brk             ;Not okay
        endif
        sta watchdog
        cli 
        lda #00
        sta nogamma
        rts 

;************************************************************
;* Send XLives+Warp Stats to Gamma
;************************************************************
sendwstat
        begin
            sei             ;Hold IRQ's 
            lda #$80
            sta nogamma     ;Stop IRQ's from sending
            cli 
            lda #g_gstawx    ;Send request "Gamma Get XLives+Warp Data"
            jsr xmitr       ;Returns gamma xmit in (A)
            cmp #01
        eqend
        ;Ready for xfer to Gamma
        lda #00
        sta olddata     ;In case something comes back unexpected
        ldx #00
        begin
            lda st_extlie,X ;Base of Xlives and Warps is here
            jsr xmit        ;Send this byte
            sta watchdog
            jsr dodelay     ;Wait before next send
            lda olddata     ;Not expecting anything back
            ifne                ;Got something
                cmp #02         ;Did Gamma Time Out??
                beq sendwstat   ;Yep, Try again!
                brk             ;Nope, ERROR! Stay here??
            endif
            inx 
            cpx #g_numstwx   ;xLives+Warp count defined in Gamma
        csend           ;All data sent, should get 'done' back from Gamma
        begin
            lda #i_xmigama
            bit inputs
        neend
        lda portrd
        cmp #03         ;Okay??
        ifne
            brk             ;Not okay
        endif
        sta watchdog
        cli 
        lda #00
        sta nogamma
        rts 

;************************************************************
;* Send Maze Stats to Gamma
;************************************************************
sendmstat
		jsr dodelay				;just in case
        ;first copy the current maze level into the buffer
        lda dif4mz
        sta st_plrmzl
        lda st_plrmzs           ;Make sure there is non-zero data to send first
        ora st_plrmzs+1
        ora st_plrmzs+2
        ora st_plrmzs+3
        ifne
            sei             ;Hold IRQ's 
            lda #$80
            sta nogamma     ;Stop IRQ's from sending
            cli 
            ;now send the block to Gamma because there *was* data
            begin
                lda #g_gstamz   ;Send request "Gamma Get Maze Stats"
                jsr xmitr       ;Returns gamma xmit in (A)
                cmp #01
            eqend
            ;
            ;  Ready for xfer to Gamma
            ;
            lda #00
            sta olddata     ;In case something comes back unexpected
            ldx #00
            begin
                lda st_plrmzl,X
                jsr xmit        ;Send this byte
                sta watchdog
                jsr dodelay     ;Wait before next send
                lda olddata     ;Not expecting anything back
                ifne                ;Got something
                    cmp #02         ;Did Gamma Time Out??
                    beq sendmstat   ;Yep, Try again!
                    brk             ;Nope, ERROR! Stay here??
                endif
                inx 
                cpx #05         ;5 Bytes = 1 Byte for Level Number + 4 Bytes for the data
            csend           ;All data sent, should get 'done' back from Gamma
            begin
                lda #i_xmigama
                bit inputs
            neend
            lda portrd
            cmp #03         ;Okay??
            ifne
                brk             ;Not okay
            endif
            sta watchdog
            lda #00
            sta nogamma
            cli 
        endif
        rts 


;*************************************************************
;* Send + Receive Data from Gamma
;*
;*  Value to send is in A. Will send using the rules of xmit
;*  and then wait a reasonable amount of time for a response.
;*
;* Calling routine must call cli after this!
;*************************************************************      
xmitr   pha             ;Save Request in case we loop back here
		jsr xmit        ;Will always return with tries=$00, stack neutral
		begin
			inc tries
			ifmi
				jsr resg        ;Reset Gamma and Try again
				pla
				jmp xmitr       ;Try again!
			endif
			lda #i_xmigama	;Wait for response
			bit inputs
		neend
		pla
        lda portrd      ;Get whatever value is here
        rts     
        

;*********************************************************
;* Send Data to Gamma : This will send the value in A
;*                      to the GAMMA. It will wait a 
;*                      resonable amount of time for 
;*                      the GAMMA to say it is ready
;*                      before sending. If it doesn't 
;*                      say it is ready, then it will
;*                      reset the GAMMA and then try
;*                      again.
;*
;* Calling routine must cli after this!!
;* X+Y is SAFE
;*********************************************************
xmit    sei     ;Hold that interrupt please
        pha     ;push this for later
		lda #00
		sta tries           ;Don't wait too long!
		begin
			inc tries
			ifmi
				jsr resg        ;Waited too long, reset the Gamma
				lda #00
				sta tries
			endif
			lda #i_rcvgama
			bit inputs      ;Buffer full??
		neend           ;Wait for Buffer Empty
		;Okay, it is ready now, within a normal wait time
		pla             ;get the original value to send, this balances the stack
		sta portwr      ;Send Request
		lda #i_xmigama
		bit inputs      ;Data Waiting?
		ifne                ;Something already came back, but this is too fast, old data.
			lda portrd      ;Get the data
			sta olddata     ;Save in old
		endif
        lda #00
        sta tries           ;0 for return request
        rts
        
            
dostop  lda #g_sndstop
        jsr dosound         ;Stop all sounds
        
        ;dodelay - for two sounds in a row
dodelay sec 
        lda #$50
        sta watchdog        ;Just in case
        begin
            sbc #01
        ccend
        rts
            
do3del  ldx #02
        begin
            jsr dodelay
            dex
        miend
        rts

;*********************************************************
;Wait for approximately 2000 CPU cycles to kill some time 
;*********************************************************       
gdelay  begin
            pha
            pla
            inc tries
        eqend
        begin
            pha
            pla         ;Do Nothing a bit
            inc tries
        eqend
        rts
        
;**************************************************
;* Reset Gamma if something went wrong somewhere
;*
;* This is only called from xmit or xmitr in cases
;* where GAMMA has taken too long
;**************************************************
resg    begin
			lda out1s
			and #(~o_resetg)    ;$F7 Reset Gamma Data
			sta outputs         ;Do it
			lda #$80
			sta nogamma         ;Hold Gamma Sends
			lda out1s
			ora #o_resetg
			sta outputs         ;Hit Gamma in the head
			sta watchdog
			lda #00
			sta tries
			jsr gdelay
			lda #i_rcvgama
			begin
				bit inputs      ;Wait here forever for Gamma to talk
				;lda #1
				;jsr flasherr
				;bit inputs
			neend
			;Gamma will return 0 to indicate talking but not ready!!
			lda portrd
		eqend				;If other than 0, Reset again
		begin
			lda #i_rcvgama
			begin
				sta watchdog
				bit inputs      ;Gamma is running, we can wait
			neend
			;lda #2
			;jsr flasherr
			;Got a response.... should be a minus something!!!!
			lda portrd
		miend				;stay here until minus, maybe forever!       
        lda #00
        sta nogamma         ;Ok Gamma, start again.
        rts 
        
