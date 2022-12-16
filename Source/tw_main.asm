
;*****************************************
    .title "TWEB Main Line"
;*****************************************

    ;.TEXT "COPYRIGHT 1983 ATARI "
    
    .TEXT ".TWMAIN."

pwron   
		;******************************************************
		;* Identify 6502 - Code to determine the specific 
		;* flavor of 6502 running upon, or emulation (MAME)
		;****************************************************** 
		; Detects the CPU in use
		; Bit 80 - 0=Physical CPU, 1=MAME Emulation
		; Bit 01 - 0=NMOS 6502, 1=65C02 or variant
		; Bit 02 - 0=ROR is normal 1=Early 6502 w/bad ROR instruction
		;****************************************************** 
		;* Used from http://forum.6502.org/viewtopic.php?f=2&t=2263
		;******************************************************
		ifpl
            ifeq
                ifvc
                    ifcc
                        tsx
                        cpx #$BD         ;Check for MAME, MAME starts with the SP = $01BD
                        ifeq
                            lda #$80
                            sta d6502      ;0 = NOT MAME, 1 = MAME
                        endif
                    endif
                endif
            endif
        endif
        .db $1A         ; INC,A on 65C02, NOP on everything else?
        ifne
            lda d6502
            ora #$01
            sta d6502      ; 0=6502, 1=65C02
        endif
        ; Test for very early 6502s with bad ROR instruction
        lda #$08
        clc
        ror A
        cmp #$04
        ifne   
			lda d6502
            ora #$02
            sta d6502      ; Bad ROR
        endif
        ;********************************************
        ;* Identification Complete, result in d6502
        ;********************************************
        ;* Commence Normal Startup now
        ;********************************************
		sei             		
        ldx #$FF
        txs                     ;init stack     
        lda   #o_swtchmux       ;select player 1 so we can read self test switch input
        sta   outputs           ;book it
        lda   inputs            ;Self Test??
        and   #i_slftstbit      ;see if on
        ifeq
			ldy d6502
            jmp selftest            ;Do self test
        endif
        lda #(o_resetg+o_resetb+o_lighton)    ;$0D Turn on Processors AND Light!
        sta out1s             ;Set this too
        sta outputs           ;Allow Gamma and Beta to Start, this also sets player Back to 0 (Switch Inputs)
        ;*********************************************************************************
        ; NOTE:
        ;    Gamma will reset to send a -1 down the port. No Gamma actions or sounds 
        ;    will be allowed until that -1 is removed. Also, sends to Gamma will 
        ;    not cause any effect if this data is not read!
        ;*********************************************************************************
start   sta rampg              	;Page normal RAM and ROM
		jsr resetmem			;Reset memory
		;Check cabinet input and set memory
		lda out1s
		ora #o_swtchmux			
		sta outputs				;Select special inputs
		lda inputs				;cocktail mode?
		and #i_cocktail
		asl A					;40 to 80
		sta cabsel
        lda out1s
		and #(o_swtchmux ^ $ff)	;back to normal switches			
		sta outputs		
		;continue
        jsr stpg2
        jsr scbinit            	;Init Maze 
        jsr stpg0  
        lda #$80
        sta atflag             	;Sets flag to start Attract Mode
        lda #i_xmigama         	;Wait here
        sta watchdog
        begin
            bit inputs      		;See if Gamma sent it's data
        neend
		;*********************************************************************************
		;NOTE: If Gamma is not sending, the dog will hit us here and we will never
		;      proceed. In order to get past a Gamma failure, you must turn on 
		;      the self test switch in this case.
		;*********************************************************************************
        lda portrd          ;Let gamma start
        ifeq
            lda #i_xmigama          ;wait here
            begin
                sta watchdog        ;okay to wait
                bit inputs          ;see if gamma sent it's data
            neend               ;We can wait
            lda inputs      
            ora #$0F            ;Drop errors for now
            cmp #-1           
            ifne
				begin					
					jsr pulselig	;Bad things, pulse light, forever!!
				doloop
            endif
        endif
;Since we test many sounds here, make sure we never accidentally release with a test sound		
#IF (DEBUG > 0)
		lda #sp_test ;sp_maxback ;snd_hiddenbk3 
#ELSE	
		lda #snd_j3				;Start Explosion Sound
#ENDIF	
		
        jsr dosnd2				;Always
	
		;Gamma is good.... get a bunch of info from it to start
        jsr gethsall            ;Get all High Score info
		jsr do3del
        jsr initoption          ;Initialize Option Switches
		jsr do3del
        jsr wrpin               ;Get defined Warp codess
        cli                     ;Start Progress
        ;*********************************************************************************
        ;This is the start of the main loop - Game is running from here out
        ;*********************************************************************************	
mainloop
        begin
            begin       
                jsr   stpg0             ;Default to ROM Page 0
                lda   gamest
                ifpl                   	;Not playing if in here
					jsr coincred
                endif
                ;********************************************
                ;   Coin Stuff/Start is done now
                ;********************************************
playing         lda newmzr          ;New Maze Request
                ifne
                    ldx player          ;Doing Player 2
                    ifne                    ;Yes??
                        lda pl2int          ;Has He Played??
                        ifeq                    ;Nope
                            lda	chngplr
                            bne ?ml5            	;Wait for Select to End
							jsr initstart			;Player 2 start here
                            lda #00
                            sta sellvl
                            ;sta adddif
                            ;sta	addmn 
							sta add4mz
							;sta	adden			;Clear for sure now!!
                            lda #$80
                            sta pl2int            	;Has he played(or will!!)
                            lda #snd_str          	;Start Music
                            jsr dosound
                        endif
                    endif
                    jsr nextmz			;init maze from start
                endif
?ml5            lda	maznum
                cmp olmznm        	;Last Maze Same??
                ifne
                    sta	olmznm
                endif
                ;*********************************************************
                ;Vector Generator Health Check
                ;If the VG hasn't halted in 20d IRQ's, then reset it
                ;*********************************************************
#IF DISABLE_HCOUNT = 0				
				lda   hcount
				cmp   #20d			;Are we waiting this long??
				ifcs
					sta	vgreset		;Stop VG
				endif		
#ENDIF
				lsr	inputs			;Wait for Halt (D0=halt)
            csend
#IF DISABLE_HCOUNT = 0
            lda #0
            sta hcount        ;VG Stopped if here then
#ENDIF            
            ;***********************************
            ; Watchdog and Synchronization
            ;***********************************
#IF DISABLE_FASTTRANSITIONS = 0
            bit gofast
            ifpl
#ENDIF
                begin
                    lsr sync        
                csend           ;Wait for IRQ sync
#IF DISABLE_FASTTRANSITIONS = 0
            endif
#ENDIF
doggie      sta watchdog      ;Wake up Doggie!!!
            jsr getswitch     ;Get player input switches for general use (jump/shield)
            sta lastswitch
			and #$c0			;Check P1 and P2 Start
			cmp #$c0
			ifeq
				lda adden
				cmp #01
				ifeq
					lda #$80
					sta adden		;This makes sure both buttons were up at some point
									;So we don't double trigger on a press. When adden = $80
									;The the real button press will start a game
				endif
			endif
#IF DISABLE_FASTTRANSITIONS = 0
            lda #0
            sta gofast        ;Clear flag here
#ENDIF
            ;*******************************************************
            ;* Notes on frame and frame+1 in attract mode
            ;*
            ;* This variable is a main loop counter
            ;* The MSB frame+1 is used as the state
            ;* for all parts of the attract mode
            ;*
            ;* There are some notable states for frame+1
            ;*
            ;*  00: Seems to be for the Level Select/Wating for Start
            ;*  06: High Score Table
            ;*  07: 
            ;*  08: Attract Mode Story - Logo, Scrolling Text
            ;*  09: Space Launch
            ;*  
            ;*******************************************************           
            inc   frame         ;Another frame
            ifeq
                inc frame+1
            endif
			;*******************************************************
			; Special stuff for demo mode in main loop
			;*******************************************************
			lda demo        ;is Demo Mode
            ;cmp #03         ;Demo Mode yes
            ifne			;Demo Mode yes
				lda adden
				ifmi
					lda #0
					sta frame+1         ;Hold Here
					;sta addmn           ;Allow select of any level
					;sta add4mz
					lda #3				;Keeps the timer sitting @ this many seconds
					sta addtim
;#IF (DEBUG = 1)
					lda #28d		;This allows us to warp into the Final Station and the Hidden levels sitting at 25-28
;#ELSE
;					lda #maxdif
;#ENDIF
					;sta adddif
					sta add4mz
				endif
            endif
            ;*********************************************************
            ;* Flash Color updates
            ;*********************************************************
            jsr doflash
			;do we need special maze colors?
			lda difcty
			cmp #maxdif
			ifeq
				jsr doflashx
			endif
            ;*********************************************************
            ; VRAM Double Buffer Swap - This updates the VJMP sitting
			;                           at the base of VRAM and 
			;                           alternates between VG starting 
			;                           at $4003 or $4803 
			;
			; VECTOR GENERATOR STARTS HERE!!!
            ;*********************************************************
            lda vecram+1
            eor #04         	;Swap Vector Buffers
            sta vecram+1
dovggo      sta vggo            ;And Start VGGO
            ;*********************************************************
            ;* Update Randoms 
            ;*********************************************************
            ldx #nmsparks-1     ;Fill a few random numbers
            begin
                jsr getrand
                sta rands,X
                jsr dodelay         ;Don't clobber NMI's
                dex
            miend
            ;*********************************************************
            ; Main Game Sequence Controller - Inside of TWTBLS
            ;*********************************************************
			lda picseq			;Dont do sequence stuff when emoting, it messes things up
			ifpl
				jsr seqcon          ;Do sequence pic routines
            endif
			;Bit-o-hack warning
			lda scflags	
			cmp #sc_state_fight
			beq ?dospks			;Star Castle shoots sparks too, need to draw them if active
			;sparks need to run in both game mode and attract maze display
            lda gamest          ;Bit 7 is Zero when in attract
            eor #$FF            ;Invert
            ora mzgame
            ifmi
                ;*********************************************************
                ;* Have to draw the sparkoids in these cases
                ;*********************************************************
?dospks         jsr stpg2
                jsr sparks          ;all the sparkly angles and such      	
            endif
bit20		jsr stpg0
            ;*********************************************************
            ;* Time Management here
			.sbttl "Time/Oxygen Counters"
			;*****************************************
			lda gamest
			and #$10                ;Clock Hold??
			ifne                    ;Stop count if sucessful exit
				;Clock hold should never happen in the Maze
				lda gamest
				bit bit20           ;the 6502 doesn't have a BIT,imm opcode, silly but true, must do BIT,abs
				ifne                ;Should not happen
					and #$af            ;Drop Hold bit
					sta gamest          ;Restore
				endif
			else
				bit mzgame
				ifmi			;Only during maze
					;Draw Oxygen Counter etc
					lda oxygen          ;Warning needed for Low O2?
					ifne                ;Not if Empty!!!
						cmp #$20
						bcs ?ml22           ;In case it went back to OK
						lda frame
						and #$1F
						ifeq
							lda #snd_i9     ;Low on Oxygen
							jsr dosound     ;Cue up a sound request
						endif
					endif
					lda frame           ;Check for flashing color too!!!
					and #08
					ifne
						lda #$F0+colwhite
					else
?ml22       			lda #$e0+oxycol
					endif
					sta oxybuf+02
;#IF (DEBUG != 0)
					lda gamedif
					;asl a
					adc #$0E
					sta temp1           ;Normal Count Rate
					lda manstat
					ifpl                ;No Count if Dying
						ifne                ;No Count if Dead
							and #$20            ;Shields On??
							ifne
								asl temp1           ;Cost Oxygen!!
							endif
							lda oxygen+1        ;Oxygen LSB
							sec 
							sbc temp1
							sta oxygen+1
							iflt
								dec oxygen
								ifeq
									lda #00
									sta oxygen+1        ;Zero LSB as well
									lda #$80
									sta manstat     	;He will die
									lda #sp_rexuggh
									jsr dosound
								endif
							endif
						endif
					endif
;#ENDIF
					lda oxygen
					jsr decimal     ;Total time in dec to temp7
					lda #((oxybuf+$0A)/$100)&$FF
					sta vglist+1
					lda #(oxybuf+$0A)&$FF
					sta vglist
					lda #temp7
					ldy #2
					sec 
					jsr digits      ;Display Total Time
				endif
			endif
			;*********************************************************
			; DEBUG DATA DISPLAY
			;*********************************************************
#IF ((DEBUG != 0) | (LEVEL_EDITOR != 0)) & (FORCE_HIDE_FRAMECOUNT = 0)
			jsr showframes
#ENDIF			
            ;********************************************************
            ;* Vector Generator Dual Buffer Management
			;* 
			;* We are goig to swap buffers here and update the vglist 
			;* pointers to the current/new buffer. The VG has already
			;* started rendering from the other buffer at this point
			;* so we are safe to write into this part of the VGRAM now.
            ;********************************************************
            sei             	;Hold Interrupts Here
            lda vecram+1
            ldx # ubyte(vecram)
            and #04
            ifeq
                ldx # ubyte(vecramb2)     ;Use Upper Buffer
            endif
            lda #vecram&$ff+2
            sta vglist
            stx vglist+1        ;Set up next buffer to build in 
            jsr vgcntr			;VGCNTR always starts each VRAM buffer
			;********************************************************
main2       sei                 ;still hold if here
            lda out1s
            ora #o_swtchmux     ;Set switch mux to Self Test switch
            sta outputs
            lda inputs          ;get the status of the self test switch
            cli 				;Enable IRQ again
            and #i_slftstbit    ;mask out the other switches
            ifeq
                jmp selftest        ;Do Self Test
                lda out1s
                and #(~o_swtchmux)  ;Put the switch mux back to the player inputs
                sta outputs
                jmp ?loopend        ;SHORT LOOP!
				;*****************************************
            endif
            lda #00
            sta vdata           ;Convienient Place to Zero These
            sta vdata+1
            sta vdata+2
            sta vgbrit
            ldx #$71
            jsr vgadd2      	;Set Large Scale
            lda #01
            sta vdata+3     	;Set up for Window Vector
            jsr vgvtr2      	;Set Window
			VGADD_SCALE(ywin_on,binscal1,0)		;Set Window back to very top (full screen), Large Scale
			VGADD_SCALE(ywin_off,binscal2,$50)						
            VGADD_JSR(scobuf)
			lda twopl			;Only add jsr to player 2 score buffer if we are in a 2 player game
			ifne
				VGADD_JSR(scobuf2)  ;And 1 to Player 2 also
			endif
            jsr rdjmp       	;Read Jump Button
            jsr points      	;Add in Points
            ;***********************************************************
            ;* Game Over and Change Player Messages 
            ;***********************************************************
            ;jsr vgcntr
            ldx #2
            begin
                stx temp9           ;Check for Game Over
                ldy gmov-1,X        ;May need message
                ifne
                    jsr plrsel      ;Make sure pointing correct direction
                    dec gmov-1,X
                    ldx #mgamov     ;Game Over
                    jsr mesg
                    ldx #mplayr     ;Tell Which Player
                    jsr mesg
                    jmp ?ml30
                endif
                ldx temp9
                dex 
            eqend
            bit updflg      ;Adding to High Score Table??
            ifmi
                jsr jmp3            ;Read Buttons
                jsr geths           ;Get Initials
                lda frame+1
                ifpl                ;Too Late!
                    sta updflg
                    sta updint      ;Clear These
                    lda updcur      ;Only write if Top 3
                    cmp #(3*3)
                    ifcc
                        lda #-1
                        sta wrhflg      ;Write Anyway!!
                    endif
                endif
                jmp ?loopend            ;SHORT LOOP!
				;***********************************
            endif
            lda chngplr     ;Player change message?
            ifne
                ;lda adddif
                ;ora addmn       ;Select Allowed??
				lda add4mz
                beq ?ml29       ;Nope!
                lda pl2int      ;Player 2 not played yet??
                ifeq
					jsr stpg1
                    jsr adddis      ;Display Select Message
					jsr stpg0
                    jsr getswitch   ;Get Start Switches
                    rol A           ;Need CC or +
                    bcc ?ml28
                    ifpl            ;Other??
?ml28                   lda #00
                        sta chngplr
                    endif
                else
?ml29               dec chngplr
                endif           	;If Selecting, Dont Count Now
                ldx #mplayr     	;Player Message
                jsr mesg
				jsr plrsel      	;Point Correctly
                ldx player
                inx 
                stx temp9
?ml30           lda #temp9
                sec             	;Display 1 Digit
                ldy #01
                jsr digits
            else
                ;******************************************
                ;* "Check/Do Attract Message Output"
                ;******************************************
                lda gamest
                ifpl 
					jsr attract
					bcs ?loopend        ;If carry is set from here then skip the maze/space 
					;HACK - Moved this in here for Select-A-Level outside of attract
					;Select-A-Level Display
					lda addtim
					;lda adden
					ifne
						;lda adddif
						;ora addmn           ;Select to Anywhere??
						lda add4mz
						ifne
							jsr stpg1
							jsr adddis      	;Do Add display
							jsr stpg0
							jmp ?loopend
						else
							sta addtim
						endif
					endif
                endif
				;Common across attract and game mode
				lda mzgame
				ifmi
					jsr mmaze
				else
					jsr mspace    
				endif
            endif
            ;*************************************************
            ; wrap up the main loop here, do other stuff
            ;*************************************************
?loopend    ; Coin Counters
			;jsr	vgcntr		; HACK - REMOVED THIS, IF SOMETHING IS WEIRD, PUT IT BACK!!!!
			lda nogamma     ;Gamma on Hold??
			ifpl            ;Nope
				ldx #mechs-1        ;Any coin counters to send:
				begin
					lda cntrs,X     ;If not 0, send the pulse
					ifne
						lda whcounter,X     ;Which counter to hit
						jsr xmit            ;Hit it
						cli                 ;Restart Interrupts
						dec cntrs,X
					endif
					dex
				miend
			endif  
            ;***************************************
			; High Scores
			;***************************************
			lda wrhflg
			ifmi                ;Need to Write High Scores
				jsr sendhs      ;Send them - assume okay write
				lda #00
				sta wrhflg      ;Request Satisfied
			endif
			;***************************************
			; Debugging stuff
			;***************************************
#IF (DEBUG != 0) | (LEVEL_EDITOR != 0)
			inc mloopcnt
			VGADD_JSR(dbgbuf)
#ENDIF
			;***************************************
			; Tidy up the VG
			;***************************************
			;jsr vgcntr
			;vgadd_scale(ywin_off,binscal2,0)
			;vgadd_stat(sparkle_off,xflip_off,0,$F,colred2)       
			;VGADD_VCTRS(-104,104,hidden)
			jsr vgcntr
			jsr vghalt
			lda #00
			sta vgrestart   	;If here, no auto restart on VG		
        neend           ;Always, loop to start 
		
;***********************************************************************************      
;* End of Main Loop                      
;***********************************************************************************
;***********************************************************************************


;Coin and Credit stuff when in attract only
coincred
		lda c_crdt          ;Are there current Credits?
		ifne
		;ifeq
			;lda c_cnct
			;ifne                ;Half Credit??
;?ml50           lda #0          ;Attract, Half Credit, Light Off
			;else
			;	lda #1          ;Attract, No Credit, Light On
			;endif
		;else
			;lda c_tcmflg    ;2 Coin Min??
			;bmi ?ml50       ;Hold Light if Waiting
			lda frame
			lsr A
			lsr A
			lsr A
			lsr A
			and #01			;20 into 1 - For flashing light 
			;rol A
			;rol A 
			;rol A
			;rol A  
			;rol A           ;20 into 1 - For flashing light   C 00100000 
		endif
		eor out1s
		and #o_lighton
		eor out1s
		sta out1s           ;Set Light
		;
		;MOVED INTO TWCOIN
		; lda c_cmode         ;Free Play??
		; ;and #03             ;Look at Coin Bits
		; ifeq
			; lda #02
			; sta c_crdt         ;Free Play
			; sta c_oldc			;Also save here so there isn't a coin sound on start
			; ;sta c_tcmflg       ;Skip This nonsense
		; endif
?ml83   bit updflg          ;Doing Initials??
		ifpl_
			;If we are in DEMO but we aren't in select-a-Level then don't try and start game yet
			jsr getswitch   ;Get Start Switches
			sta lastswitch
			lda demo
			;cmp #3
			ifne
				lda adden
				ifeq
					;lda button
					;jsr getswitch   ;Get Start Switches
					lda lastswitch
					rol A           ;Need CC or +
					bcc ?adden
					ifpl            ;Other??
?adden					lda #01
						sta adden
					endif
?2play				rts			;jmp playing		;Skip coin stuff directly below
				endif
				bpl ?2play		;If it is positive, then we skip normal starts too
								;must be - to allow real start
			endif
			;At this point, okay to do starts
			ldx c_crdt        	;Get Credits
			ifne_				;Not if Credits = 0
				;beq ?ml39           
				cpx #2              ;At least 2 Credits, we can check the 2 Player start then
				ifcs_            
					;jsr getswitch       ;Ask Gamma
					lda lastswitch
					asl a               
					asl a               
					rol swtc2           ;Player 2 Start Debounce
					lda swtc2
					and #07
					ifeq
						jsr stpg2             ;Okay, 2 Player Game is starting now
						jsr scbinit           ;Init Score Buffer
						jsr stpg0
						lda #01
						sta twopl             ;Indicate 2 Players
						sta rampg             ;Select High RAM
						sta player            ;Set to Player 1 for Init Stuff
						lda slives            ;Number of Lives per game
						sta lives+1
						lda	#0
						sta	pl2int            ;Player 2 has not played
						sta tspark
						sta objst+zstuf+1
						sta objst+zstuf+2
						;lda gamedif
						;sta incdif				;Reset incremental difficulty from DIPS
						lda #$80
						sta scoflg            ;Put up a 0 and lives
						jsr	dscore            ;Init Player 2 score 
						dec c_crdt            ;cost... 1 credit
						jmp ?start12          ;and do common init
					endif
				endif
				;We have at least 1 credit: Check Fire/P1Start Switch Status
				;jsr getswitch
				lda lastswitch
				asl a              		;Returned bit into Carry
				rol swtc1             	;Switch 1 Debounce
				lda swtc1
				and #$07
				ifeq_
					jsr stpg2
					jsr scbinit           	;Init Score buffer
					jsr stpg0
					lda #00
					sta twopl               ;1 Player game
					sta lives+1             ;Make sure 0 lives for Player 2
;comes here from P2 too                            
?start12			lda #00
					sta rampg
					sta diedyet             ;Clear this
					sta player              ;Reset Player Index
					sta atflag              ;Turn off Attract mode flag
					sta addtim          
					sta gmov
					sta gmov+1              ;Kill any left over message
					sta vxstat              ;Clear homeworld stuff					
					sta vxcount				;Clear HW status
					sta statst
					sta scflags				;Start castle reset
					ldx #07                 ;Clear game times, both players - 4 bytes per player
					begin
						sta st_gtime1,X
						dex
					miend
					;HACK: Removed from Select-A-Level 
					;sta addmz
					;END HACK
					;(A) must be zero for this
					sta tspark
					sta objst+zstuf+1
					sta objst+zstuf+2
					;lda basedif				;Load base difficulty
					;sta incdif				;Reset incremental difficulty from DIPS
					lda slives            	;# lives from DIP's
					sta lives						
					;--------------------------------------------------------------
					jsr initstart  			;Player 1 Start here
					jsr spcini             	;Init space game stuff
					lda	twopl             	;One Player Game??
					ifeq
						sta add4mz
						;sta	adddif
						;sta	addmn
					endif
					
					lda #$80
					sta gamest            ;Always Start game
						
					lda demo
					ifne				;If demo tho, reset any Select-a-Level stuff
						lda #0
						sta adden
					endif

					lda #o_lighton
					ora out1s             ;Lamp On
					sta out1s
					lda #snd_str          ;Start Music
					jsr dosound
					dec c_crdt            ;Cost at least 1 credit
				endif
			endif
		endif
		rts
		
initstart		
		jsr stpg2
		jsr gminit            ;Re-Init Game
		jsr stpg0
		inc manstat           ;Turn on Man        
		;*********************************************************************************
		;* Design Time First Level Setting:
		;* This will be set in the havoc.ah file which calls this file. It is a quick
		;* developer way to have the game start on a specific level. STARTLEVEL should
		;* not be defined on released code.
		;*********************************************************************************
#IF (((DEBUG != 0) & (STARTLEVEL != 1)) | (LEVEL_EDITOR != 0))
levelst                         
		lda #STARTLEVEL-1     ;Make Zero based
		sta sellvl
.export levelst
;#ELSE
;		lda	sellvl            ;Where to start, this can be set from a continuation or fresh @0
#ENDIF
		;*********************************************************************************
		;*********************************************************************************
		jsr	thisone				;Target level in sellvl!!!!!!
		jsr newscore                                              
		;jsr   incstuf - replaced this with direct code below
; #IF ENABLE_ADAPTIVE_DIFFICULTY != 0
		; ldx dif4mz
		; lda expdth,X
		; sta incded
; #ENDIF
		rts
		
;Which command to send to Gamma for Coin Counter 'clicks'
;  0: Aux Coin so it sends a beep sound command
;  1: Coin Counter #1
;  2: Conn Counter #2
whcounter  .db snd_b2c, g_cn1, g_cn2
     
; #IF ENABLE_ADAPTIVE_DIFFICULTY != 0	 
; ;Determines how many times a person may die on each
; ;level and how it affects the condition level in 
; ;the tactical scanner.
; expdth      .db 0,0,1,1, 1,1,1,1, 1,1,1,1, 2,2,2,2, 3,3,3,3, 3,3,3,3

; #ENDIF         
;*****************************************
    .sbttl "IRQ Routine"
;*****************************************      
irq     cld 
        pha 
        tya 
        pha 
        txa 
        pha 
        sta intack			;Acknowledge
#IF DISABLE_HCOUNT = 0
        inc hcount          ;Yet another Interrupt has passed
#ENDIF
		;**************************************************************************************
		;* VGHALT Check
		;* If the Vector Generator is HALTed we can check if it should be immediately restarted
		;* to redraw the same scene. This mode is used during Maze initialization right when
		;* this ship lands on the surface of the maze. The init process takes a long time so the 
		;* VG is restarted to redraw the same VGRAM objects since they are not changing.
		;**************************************************************************************
        lsr inputs          ;Check VGHALTED
        ifcs                    ;Yes, the VG is stopped here
            lda vgrestart       ;Continue Flag on? This is set when the maze is initializing
								;which takes some time, so the VG will just keep drawing the same
								;VGRAM buffer over and over again.
            ifmi
                sta vggo            ;Start it again
#IF ENABLE_WASTE = 1
            else                	;Not Restart, we don't have enough time to start it again
dowaste         lda vecram+1		;in normal situations, so lets draw a black box to keep
                pha					;the VG doing something instead of sitting idle at VGCNTR.
                lda vecram
                pha             	;Save current vgpointer
                laljsr(waste)
                sta vecram
                lxhjsr(waste)
                stx vecram+1
                sta vggo        	;A jumpl to waste time (draws a full screen black box)
                nop
                nop
                nop
                nop
                nop             	;Because Doug says so!!
                pla             	;Now, put em back!
                sta vecram
                pla
                sta vecram+1
#ENDIF
            endif
        endif
		;************************************************************************
		;* Debug Screen Feedback 
		;************************************************************************
#IF ((DEBUG != 0) | (LEVEL_EDITOR != 0)) & (FORCE_HIDE_FRAMECOUNT = 0)
		ifeq
			lda mloopcnt
			sta mloopdis
			lda #0
			sta mloopcnt
		endif
#ENDIF
		;************************************************************************
        inc c_intct
        lda c_intct
        lsr A               ;Interrupts are every 4ms
        ifcc                ;So do coin/LETA routine every other time = 8ms
            lda out1s
            and #(~o_swtchmux)  ;Make sure player 0
            sta outputs
            jsr moolah          ;Do Coin Routine
            lda inputs          ;Self Test??
            and #$10            ;See if on
            ifne
                bit nogamma         ;Gamma On Hold? : minus if holding
                ifpl
                    jsr getleta         ;Gamma shoudl be ready... Get LETA data
                    jsr neg
                    tay
                    sec
                    sbc oldata
                    sty oldata          ;Save new and get difference
                    cmp #$80
                    ror A
                    clc
                    adc rgdd
                    ifvs
                        ifmi
                            lda #-$7f
                        else
                            lda #$7F
                        endif
                    endif
                    tay
                    lda updflg
                    bmi ?irq5
                    lda addtim              ;Doing Select??
                    bne ?irq5               ;Then Keep Running
                    lda gamest              ;Game On??
                    ifmi                    ;Yep
                        and #$10
                        ifne
                            ldy #00             ;Control On Hold
                        endif
                        lda mzgame          ;No Control in specific states
                        and #$58                ;(Like New Jersey)
                        ifne
                            ldy #00
                        endif
?irq5                   sty	rgdd
                    endif
                endif
            endif
        endif
        lda c_intct         ;Count Interrupts
        and #$07
        ifeq				;Every 8 interrupts
            inc sync 			;Flag for mainloop to sync with IRQ
            bit updflg      	;Adding to High Score Table, if so don't count 
            ifpl
                inc tframe          ;True Frame Counter
                lda tframe
                cmp #$34             ;$34 here is approd 1 second
                ifeq
                    lda player          ;Which player
                    asl A               ;x2 please
                    tax
                    sed                 ;**** Caution Decimal Mode ****
                    lda st_gtime1,X
                    clc
                    adc #01
                    sta st_gtime1,X
                    ifcs                    ;Only if Carry
                        lda st_gtime1+1,X
                        adc #00
                        sta st_gtime1+1,X
                    endif
                    cld                 ;**** End Decimal Mode ****
                    lda #00
                    sta tframe
                endif
            endif
            ;******* Mark Asked for This!!! (originally) *********************
            ; This is to keep sounds from clobering themselves
            ; Each queue counts down to zero, zero allows the sound to play
            ;*****************************************************************
            ldx #sndcuecnt-1
            begin
                lda sndcue,X
                ifne
                    dec sndcue,X
                endif
                dex
            miend               ;Sound Cue Timers
        endif
?irq10  pla
        tax
        pla
        tay
        pla
        rti 

;-----------------------------------------
; Attract Mode 
; Returns with Carry Set if main loop
; should skip gameplay routines
;-----------------------------------------
attract jsr getleta
        pha             ;Save Read Data
        ldy c_cnct      ;Half Credits??
        cpy c_oldcnt    ;Different Than Old??
        bne ?ncoin
        ldx c_crdt          ;Any Credit
        cpx c_oldc          ;Same as last??
        ifne                ;No
            stx c_oldc          ;Now there are  
?ncoin      sty c_oldcnt        ;Save old coin count
            lda #snd_coin       ;Klink on coin
            jsr dosnd2          ;Do these even in attract
            ldx addtim          ;In add time??
            beq ?ml31           ;Go Back to display if not
			ldx #09
            stx addtim          ;Else reset timer
            ldx #00
            stx frame+1         ;And reset frame to show 'Press Start'
        endif
        ;dont do the following if doing select-a-level
		ldx addtim      ;Doing Add Time??
        ifeq                ;If yes, will skip this part
            sec             ;Not add a coin, do force stuff
            sbc atdata      ;Change based upon LETA data pulled above
            ifpl                ;Left spin
                cmp #$10            ;Enough to show scores
                ifge
                    ldy #06         
                    sty frame+1
                endif
            else                ;Right spin
                cmp #-$10
                iflt
?ml31               lda frame+1
                    cmp #06         ;Already Doing Space??
                    ifge                ;Nope
                        ldy #09
                        sty frame+1
                        sty jumpst      ;Clear any left over jump
						;turn off story
						lda strtst
						and #$0F
						sta strtst      ;Turn Off Story
                    endif
                endif
            endif
        endif
        pla				;Get Roller value         
        sta atdata      ;In any case, save old read
        lda frame+1
        cmp #06
        ifge           ;Frame 0-5 is Space Stuff: we aren't showing any logo/story, get out
            cmp #08
            iflt            ;Is 6 or 7
                jsr hstbl           ;Do High Scores
                sec
                jmp ?atend          ;Skip to the end, short loop
            else                ;Frame 8+                       
                ifeq                ;It was 8.... Init logos and story sequence
                    lda frame               
                    ifeq                    ;Start Draw Again??
                        jsr initstory
                    endif
                endif   
                bit strtst      ;Check Story Status
                ifmi                ;Do Story stuff here
                    jsr dostory
                    sec
                    jmp ?atend      ;Short loop
                else            ;Story is OFF, set up to go into Tube Launch
                    lda #00
                    sta manstat     ;Be sure dead
                    sta objst+zreactor  ;Make sure off
                    sta gamest      ;Drop and left over bits
                    sta holmz       ;Don't hold any objects
					sta difcty      
					sta maznum		;Set Maze to Level 0 (level 1) for attract mode
					sta maztype
					sta dif4mz   
                    jsr nextmz      ;Set up the maze now, for attract mode animation
                    jsr stpg2
                    jsr init2       ;Init the maze
                    jsr stpg0
                    lda #03
                    sta spcspd      ;Fast stars again
                    sta shipst      ;We want the ship on!
                    lda #$80
                    sta atflag      ;Put it into Attract mode
                    sta shldok
                    lda #$40
                    sta lauen       ;skip launch
                endif
            endif
        endif
        jsr stpg0
        jsr markjump       ;Mec control of attract mode man
        lda #00
        sta tactde			;No tact in attract mode
		ldx #mcredi         ;Credit Message
        jsr mesg
#IF (LEVEL_EDITOR == 0)
        lda c_crdt
        jsr decimal         ;Convert Credits
        lda #temp7
        sec
        ldy #02
        jsr digits          ;Display Credits
#ENDIF
        lda c_cnct          ;Half's??
        ifne
            lda frame
            and #$08                ;Flash it!
            ifne
                lda #$FA
                ldx #$60
                jsr vgadd2
            endif
            lda vgjchhf		;formsg+$1a
            ldx vgjchhf+1	;formsg+$1b          ;1/2 Char
            jsr vgadd2
        endif
        ;Display Lives and Bonus
        jsr distlibo    
#IF DISPLAY_COPYRIGHT = 1        
        ldx #matari
        jsr mesg         ;Copyright Message
#ENDIF
        lda gmov
        ora gmov+1          ;Ending game??
        ifeq
            lda frame
            and #$20
            ifeq
                ldx #mgamov
            else
                ldx #mpress     ;Press Start
                lda c_crdt
                ifeq    
                    ldx #minsert        ;Insert Coin Message
                endif
                ; lda c_tcmflg
                ; ifmi
                    ; ldx #minsert        ;Insert Coin
                ; endif
            endif
            jsr mesg          ;Game Over
#IF (LEVEL_EDITOR != 0)
			ldx #mledit
#ELSE
			lda demo
			;cmp #3
			ifne
				;demo mode
				ldx #mcmodd
			else
				ldy c_cmode
				;and #03
				;tay
				ldx cmodem,Y        ;Get Coin Mode Message
			endif
#ENDIF
            jsr mesg          ;Put it up
        endif
		;HACK - Moved this into main loop
		;Select-A-Level Display
        ; lda addtim
        ; ifne
            ; lda adddif
            ; ora addmn           ;Select to Anywhere??
            ; ifne
                ; jsr adddis      ;Do Add display
                ; sec
                ; bcs ?atend
            ; else
                ; sta addtim
            ; endif
        ; endif
        clc				;Long attract loop by default
?atend  rts

;*****************************************
    .sbttl "Logo/Story Control"
;*****************************************
initstory
        lda #00
        sta seqx            ;We start all of these at zero
        sta seqst           ;Start Draw
        sta animate
        sta perm1           ;Logo Motion: Position of logo 0=center screen 20=top position
        sta perm2           ;Another logo var for Logo Zoom
        jsr initcolram      ;Init colors for message
		lda strtst
		clc
		adc #1
		and #$0F			;Clear the flags(upper nyble)
		ora #$80			;Turn on Story (Minus flag)
        sta strtst          
        lda #-2             
        sta strtyl
        sta strtyh          ;Start Position
		lda #msco0
		sta strtln          ;Will start with line 0 of story index
        lda sndatt          ;Attract sounds?
        ifne
            dec atsnd               ;Next Sound
            ifmi                    ;ready to play?
                lda rands         	
                and #7
                adc #03
                sta atsnd           	;every 4-11 times
				;lda rands           	;Use a Random Here
				and #03
				tax
				lda atmusic,X
				jsr dosnd2
            endif
        endif
        rts

;********************************************************        
;* Actually draw the story here and change story states
;********************************************************
dostory 
        ;set up the main logo, we always draw this
        vgadd_scale(ywin_off,binscal1,0)
        vgadd_stat(sparkle_off,xflip_off,0,$A,colorange)
        jsr vgcntr
        ldx perm1
        lda #00
        jsr vgvtr5      ;Position initially center, then going up towards top for story
        ;**************************************************
        ;* animate sets states of attract logo drawing
        ;* 
        ;* 00: 'Drawing' Mode - Logo being drawn, line by line, center screen
        ;* 01: Logo with Red Line, center screen
        ;* 02: Subtitle reverse-zoom sequence
        ;* 03: Vertical Slide upwards, whole logo and subtile move up to make room for story
        ;**************************************************
        lda animate     ;What level of Name??
        ifeq
            ;animate=0: Initial logo 'drawing'... center screen
            lal(havoc1)
            sta temp2
            lah(havoc1)
            sta temp2+1     ;Pointer to Name Logo
        else
            cmp #01
            ifeq
                vgadd_stat(sparkle_off,xflip_off,0,$A,colorange)
                vgadd_jsrl(havoc1)                      
                jsr vgcntr
                ldx perm1
                lda #00
                jsr vgvtr5          ;Position   
                lal(havoc2)         ;And scroll in 2nd half
                sta temp2
                lah(havoc2)
                sta temp2+1
            else                    ;Stage 2 or 3
                ;timer to flash the main logo
                lda frame+1
                and #01
                ifeq
                    lda frame
                    cmp #20
                    iflt
                        vgadd_stat(sparkle_off,xflip_off,0,$A,flacol)
                    else
                        vgadd_stat(sparkle_off,xflip_off,0,$A,colorange)
                    endif
                else
                    vgadd_stat(sparkle_off,xflip_off,0,$A,colorange)
                endif
                vgadd_jsrl(havoc1)  ;This is the M_H_ part
                jsr vgcntr
                ldx perm1
                lda #00
                jsr vgvtr5          ;Position
                vgadd_jsrl(havoc2)  ;the 'ajor avoc' parts
                jsr vgcntr
                ldx perm1
                lda #00
                jsr vgvtr5          ;Position
                vgadd_stat(sparkle_off,xflip_off,0,$C,colred2)  ;Set Color for Inside Line
                vgadd_jsrl(havoc3)  ;inside red line
                
                ;subtitle drawing starts here
                lda #01             ;we are going to draw this twice, because it is cool, start @ 1, end on -1
                sta temp8
                begin 
                    jsr vgcntr          ;start from center                  
                    vgadd_scale(ywin_off,1,0)      
                    ;position
                    lda perm1
                    sbc #$0C            ;down a little under the logo
                    tax
                    lda #$FC            ;a little left of center
                    jsr vgvtr5          ;Vertical Position                
                    ;do color
                    lda temp8
                    ifne
                        ldx #($60|v_sparkle)    ;sparkley version
                        lda #$CB
                    else
                        ldx #$60            ;non-sparkley, and glowing
                        ;use frame for glowing... frame flips tho, so we need to do some math
                        lda frame+1
                        and #01
                        ifeq
                            lda frame
                        else
                            lda frame
                            eor #$FF 
                        endif    
						lsr A				;Dimmer, save the phosphor
                        and #$F8
                        sta temp3           ;use temp3 here, super-temp
                        lda frame+1
                        and #03
                        adc #01
                        and #07
                        ora temp3
                        ;ora #colbluer                      
                    endif               
                    jsr vgadd2          ;Set Color for Subtitle
                
                    ;zoom, 
                    lda perm2
                    asl a
                    asl a
                    ldx #$71
                    jsr vgadd2          ;set the zoom level
                    ;and finally actual text
                    ldx #mpromise      
                    jsr mesgonly        ;write it, paged ROM Call here
                    dec temp8           ;loop it
                miend
                jmp skpcp2      ;skip copy2
            endif
        endif       
        jsr copypic         ;Do Name                    
skpcp2  lda seqst           ;Did it just finish??
        ifmi                ;yep!
            lda animate     ;Which Level??
            cmp #03
            iflt                ;Not Done yet
                inc animate
                lda #00
                sta perm1
                sta seqx
                sta seqst
            endif
        endif
        jsr vgcntr
        lda animate     ;In Section 2
        cmp #02         ;In LogoSequence 2??
        ifeq
            ldx perm2   ;only move to next state after these ticks
            cpx #$20
            iflt
                inc perm2           ;Not There yet
            else
                inc animate         ;Done z
                lda #00
                sta perm1
            endif
        else
            cmp #03     ;moving on up now
            ifeq
				lda strtst
				and #07
				tax
				lda perm1
				cmp logolvl,X
                ;ldx perm1   ;logoposition maxes out at 20
                ;cpx #$20
                ifcc
                    inc perm1           ;Not There yet
                endif
                VGADD_SCALE(ywin_on,1,0)    ;Window!
				;story stuff here
                jsr stpg0		;Not needed?
                bit strtst      ;Which story page??
                ifvc
					jsr scormsg		;Do Scoring Summary				
                else
					lda strtst
					and #$20
					ifeq
						jsr backstory		;Do Story
					else
						jsr storydes		;Do Credits
					endif
                endif
                jsr stpg0
            endif
        endif
        rts

;This sorta randomizes the level of the logo to save the phosphor on the attract mode
logolvl	.db $1F,$20,$21,$22,$23,$24,$25,$27
;*****************************************
; Main Code Routines for Maze
;*****************************************
mmaze   ;Do Map Now           
		lda mtim                ;Doing hint?
		ifeq                    ;no, this is okay
			jsr stpg0
			jsr header              ;Do header stuff
			jsr vgcntr              ;Move Up Window
			VGADD_JSR(mapbuf)       ;JSR to the map buffer
			jsr stpg6               ;Select Page 6
			jsr dotmap              ;Should leave us at center, draws object crosshairs
			jsr stpg0
		else
			lda frame
			lsr A
			ifcc
				dec mtim            ;Time it out!
			endif
			lda vxcount
			ifne
				;homeworld complete, show random messages instead
				lda rmazenum
				ifmi
					lda rands
					and #$0F
					sta rmazenum
				endif
				jsr mzmesgr
			else
				lda	dif4mz
				jsr mzmesg
			endif
		endif
		jsr vgcntr
		;lda #00
		;ldx #$71
		;jsr vgadd2      	
		VGADD_SCALE(ywin_off,binscal1,0)	;Set Scale to 1
		VGADD_VCTRS(30,-76,hidden)		;Normal Window Line
		lda #($F0+colwhite)
		ldx #$60            ;White 
		jsr vgadd2
		VGADD_JSR(longline) ;Add white line
		;lda #00
		;ldx #$79            ;Now Set Window
		;jsr vgadd2
		VGADD_SCALE(ywin_on,binscal1,0)		;Set Window, Large Scale
		jsr stpg2
		jsr react           ;Do reactor behavior and drawing
        
        ;***********************************************************************
		;BUG #215: Max sound still playing after sucessful maze exit
        ;***********************************************************************
        lda gamest
        and #$40           ;If sucessful exit, then don't start another max sound or animate discs
        ifeq
            jsr anio2			;Animate discs if needed
            jsr maxsound		;Update the max sound if needed 
        endif
        ;***********************************************************************

        ;common code for maze and space 
        jsr jumpr       ;Read Jump Button          
        jsr posit       ;Position maze based upon man
        jsr upvel       ;Do Velocity
        
        ;wrap up maze stuff here
        jsr stpg6
        jsr trap
        jsr drawm       ;Do Maze
        jsr stpg2
        jsr posmo       ;Position All Maze Objects
        jsr stpg0
        ;***********************************************
        ;* The Above JSR calls the following routines:         
        ;*  fire            move fire balls                
        ;*  discs           place discs in maze      
        ;*  shiponm     place ship 'on' maze         
        ;*  shots           robot's shots motion     
        ;*  robot           move robots              
        ;***********************************************
        ;* The Above routines call the following:              
        ;*  turns           turn robots at walls         
        ;*  fireshot        fire a shot from robot      
        ;*  bounce      bounce object off wall         
        ;*  drawrob     draw robot               
        ;*  locate      draw object in maze      
        ;*  movthg      move maze object         
        ;***********************************************
        rts


        
;****************************************
;Main Code Routines for Space
;****************************************        
mspace  lda maznum
        ora difcty
        ifeq                ;Special help on level 1 only
            lda mzgame
            cmp #04         ;Landing??
            ifeq
                bit gamest
                ifvc                ;And not leaving
					;not attract - JMA: this makes the screen a bit bare, but TBH, I sorta like it
					ifcc
						ldx #mhint0     ;Dock Message
						jsr mesg
					endif
                endif
            endif
        endif
		
        ;-----------------------------------------
        ; "This should be removable later?" original code comment from ORR and MEC
        ;-----------------------------------------
		; JMA - 05152020
		; Took this out after it was noticed
		; Hoping the original team ended up fixing
		; the issue that was causing the spot 
		; killer to kick in. :-)
		;
		; Why take it out? After analyzing the outputted VG commands, this seemed odd
		;-----------------------------------------
        ;ldx #spot_vpg   ;Stat ins, page select
        ;jsr vgadd2
        ;VGADD_JSR(spot) ;Add Spot Killer here
		;-----------------------------------------
		;-----------------------------------------
        jsr shipout     ;Display Player Ship 
        jsr statot      ;Station Control
        lda tstat
        ifmi
            jsr strgen      ;Do Stars
            ;*******************************************************
            ;* The Above JSR calls the following Routines:         
            ;*  strgen      generate stars                 
            ;*  strmov      move active stars            
            ;*  sshots      ship's shots motion            
            ;*  strtshot    start ship's shots             
            ;*  enemy       Enemy ship's motion call       
            ;*-----------------------------------------------------
            ;* The following subroutines are called by the         
            ;* above list:                                         
            ;*  chkstr      check for star off screen      
            ;*  drawstr     draw an active star            
            ;*  stone       place a shot at ship         
            ;*  drawshot    draw ship's shots              
            ;*  pastck      enemy ship off bottom?         
            ;*  oneless     remove enemy ship            
            ;*  drawen      draw and enemy ship      
            ;*  shtchk      enemy shot/collision check
            ;*  mror    a   utilities for above      
            ;*******************************************************
        endif
        
        ;common code for space and maze
        ;jsr stpg0
        ;jsr jumpr       ;;Read Jump Button          
        jsr posit       ;Call this for tube Rex movement and Homeworld surface animation
        jsr upvel       ;Do Velocity - This is not technically needed but it keeps 
                        ;the space maze 'throttled' properly, if we take it out
                        ;we will need to deal with the increased speed of space levels
        jsr tube        ;In Tube?
        rts

;************************************************
    .sbttl "Init RAM and VRAM"
;************************************************
resetmem
		ldx #00
        stx vgreset             ;Stop the Vector Generator
        begin
            lda #$C0                    ;Set all to RTSL
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
            lda #00
            sta 0,X
            cpx #$E0
            ifcc
                sta $100,X              ;Clear most of page 1
            endif
            sta $200,X
            sta $300,X
            sta $400,X
            sta $500,X
            sta $600,X
            sta $700,X
            sta $800,X 
            sta $900,X			
            ;*****************************************************************
                .sbttl "Version 3 Correction"
            ;*****************************************************************
            ;* Major Havoc, Ver 1.2 to 1.3 Bug Fix...                        *
            ;*****************************************************************
            lda #01
            sta rampg
            lda #00
            sta $200,X
            sta $300,X
            sta $400,X
            sta $500,X
            sta $600,X
            sta $700,X
            sta rampg
            ;********** End Patch Area ***************************************      
            inx
        eqend
        ldx #30d-1          ;Copy all, wash later
        begin
            lda dinit,X         ;Get default initials
            sta initl,X
            dex
        miend                   ;Copy default initials in
        ldx #40d-1              ;Move Scores too
        begin
            lda dfscore,X           ;Default Scores
            sta hscore,X
            dex
        miend
        lda #(o_resetg+o_resetb+o_lighton)       ;#$0D ;Pre-Set to..
        ;* Both Processors On
        ;* Gig Lamp On
        ;* Inverts Clear
        ;* Player select to 0
        sta outputs
        sta out1s               ;Set shadow to 0 too
        ;* Init VG Buffers
        lda #01
        sta vecram
        lda #$E4
        sta vecram+1            ;Put in initial jump
        lda #$20
        sta vecram+3            ;And a halt just in case
        sta vecram+$803         ;Here too
        jsr nextmz        		;Set up initial params after game bootup, required
        lda #$FF
        sta manstat             ;Force a restart
        sta frame               ;Start right away
        lda #$80
        sta mzgrnd              ;Start on the ground
        lda #11d
        ldx #05
        begin
            sta pl1last,X           ;Init first entry to AAA
            dex
        miend
		rts
		
;**************************************
;* High Score Table Startup Initals
;* The top 3 will be overwritten by 
;* GAMMA data after fetching.
;**************************************
dinit   .db idxchar_r,idxchar_e,idxchar_x		;REX
        .db idxchar_r,idxchar_e,idxchar_x		;REX
        .db idxchar_r,idxchar_e,idxchar_x		;REX
        .db idxchar_o,idxchar_r,idxchar_r		;ORR
        .db idxchar_m,idxchar_e,idxchar_c		;MEC
        .db idxchar_j,idxchar_m,idxchar_a		;JMA
        .db idxchar_b,idxchar_l,idxchar_r		;BLR
		.db idxchar_j,idxchar_r,idxchar_k		;JRK
        .db idxchar_s,idxchar_space,idxchar_s	;S S
        .db idxchar_a,idxchar_e,idxchar_k		;AEK
		
;**************************************
;* High Score Table Startup Scores
;**************************************        
dfscore .db $32,$45,$09,$00	;94,532
        .db $17,$82,$08,$00 ;88,217
        .db $85,$72,$07,$00 ;77,285	
        .db $22,$11,$06,$00
        .db $72,$70,$05,$00
        .db $52,$58,$04,$00
        .db $54,$15,$03,$00
        .db $65,$93,$02,$00
        .db $09,$81,$00,$00
        .db $40,$52,$00,$00
		
;*****************************************************
    .sbttl "New Score - Bonus for Add-a-Coin Start"
;*****************************************************
newscore    
#IF (LEVEL_EDITOR == 0)
        ldy #00             ;Guess player 1
        lda player
        ifne                ;Is player 1
            ldy #score2-score
        endif		
        lda dif4mz
        asl a               ;x2 for words
        tax 
		;----------------------------------------------------------------
		;check if demo mode - this will force low score on demo mode
		lda demo
		;cmp #3
		ifne
			ldx #0				;reset score to zero on each level
		endif
		;----------------------------------------------------------------
        lda cscores,X       ;Low Score Part
        sta score+2,Y
        lda cscores+1,X     ;High Score Part
        sta score+3,Y
        lda #$80
        sta scrflg          ;Flag Change
#ENDIF
        rts 
 
;Scores awarded on 'Select-a-Level' starts
;where player continues a game. 
cscores .word $0000	;Level 1 - No Bonus
        .word $0005	;Level 2 = 50,000
        .word $0010	;Level 3 = 100,000
        .word $0015	;Level 4 = 150,000
        .word $0020	;Level 5 = 200,000
        .word $0025	;Level 6 = 250,000
        .word $0030	;Level 7 = 300,000
        .word $0035	;Level 8 = 350,000
        .word $0040	;Level 9 = 400,000
        .word $0045	;Level 10 = 450,000
        .word $0050	;Level 11 = 500,000
        .word $0055	;Level 12 = 550,000
        .word $0060	;Level 13 = 600,000
        .word $0065	;Level 14 = 650,000
        .word $0070	;Level 15 = 700,000
        .word $0075	;Level 16 = 750,000
        .word $0080	;Level 17 = 800,000
        .word $0085	;Level 18 = 850,000
        .word $0090	;Level 19 = 900,000
        .word $0095	;Level 20 = 950,000
        .word $0100	;Station A = 1,000,000
        .word $0000 ;Station B = 0
        .word $0000	;Station C = 0
        .word $0000	;Station D = 0
        .word $0000	;Hidden A = 0
		.word $0000	;Hidden B = 0
		.word $0000	;Hidden C = 0
		.word $0000	;Hidden D = 0
        
;*******************************************************
; Page Shims, to get to routines on other pages
;*******************************************************
;special version of thisone that sets back to page5
;for call from paged ROM in tw_tact
thisone_rtpg5
        jsr thisone				;special thisone from Tactical Scanner (warps)
        jmp stpg5

;*************************************************************
;Calls maze init routines and returns to page 2
;This is used in the final space station only in 
;order to secretly increment to the next level
;*************************************************************
nxtmaz_rtpg2 

		;find any keys that Rex still has so we can keep them
		jsr stashkeys		;this breaks things
		
		jsr nxtlvl          ;Next level please
		lda #00
		sta holmz           ;Will Set up objects
		lda #$80
		sta openflg    		;Keep maze doors closed   
		jsr domaze			;set up the maze and draw it
		;fix trip pointers
		lda trinds          ;These may not have changed above
        sta trindptr        ;Copy over Shadow into 0 page
        lda trinds+1
        sta trindptr+1      ;Trip point indirect pointer
		
		;restore keys
		jsr stpg2
		jsr unstashkeys		;this breaks things
		
		;restore hint timer
		lda #$40
        sta mtim        ;Hint message time for maze, half of normal time tho
		lda #03
		sta shldht		;restore shields
        lda #$80
        sta shldok  	;Restore shields
		lda #$21   		;fix tspark   
        sta tspark
		lda #$82
        sta mzgame          ;Set to normal maze play 
        ; The 2 is so maze does not scroll right away (flag to motion)
        sta gamest
        lda #mazcol+$E0
        sta mapbuf+2            ;Turn on map
        lda #oxycol+$E0
        sta oxybuf+2            ;Turn on oxygen counter       
        jmp stpg2        

locate_rtpg4
        jsr stpg2
        jsr locate
        jmp stpg4
		
;***********************************************************
;*  Entry to force start at a particular level             
;*  Zero based Level Number should be in sellvl                         
;***********************************************************             
thisone dec sellvl
        lda sellvl
		inc sellvl
		;---------------------------------------------------------------------------------
		;Odd but true, we have to subtract 1 above into A
		;---------------------------------------------------------------------------------
        sta dif4mz
        lda #$A0
        sta objst+zreactor  ;Set Reactor bit so we advance
        lda #01
        sta manstat     	;Make sure he is on!!
        jsr stpg2
        jsr newbrk      	;Restore Bricks
        jsr stpg0
        ;Now Fall Through!
        ;******************************************************
        ;* Will increment maze level and initialize
        ;******************************************************        
nextmz  lda #01         	;Worth 100
        sta nxtdisc     	;Restart at 100 points per disc
        jsr initcolram  	;Init Colors
        ldy #$80        	;Re-open and outside
        sty openflg     	;A good place to re-signal open
        sty outflg      	;Outside to start always
        sty shldok      	;Shield again
        lda #$F3            
        sta retbuf+2    	;Turn OK back to Blue
        lda #03
        sta shldht      	;4 shot hits allowed
        lda #$A0        	;Blownup or set off??
        bit objst+zreactor  ;Did he set off reactor??
        ifne                ;yep
            lda #$80
            sta mtim        ;Hint message time for maze	
;If LevelEditor build, then skip going to the next level and also tact display
#IF (LEVEL_EDITOR != 0)
			lda #00		
#ENDIF
			sta tactde      	;Disable Tact display next time
			jsr nxtlvl			;Increment everything!
            ;Will do this always in case we may have warped here!!!!
            lda #00
            ;sta trinds      	;Save Shadow (for 2 player games)
            sta jblast      	;Debounce Clear (in case left set)
            sta holmz           ;Will Set up objects
        endif
        lda trinds          ;These may not have changed above
        sta trindptr        ;Copy over Shadow into 0 page
        lda trinds+1
        sta trindptr+1      ;Trip point indirect pointer
        dec scoflg          ;Want to show change
        lda #03
        sta objst+zreactor  ;Turn on Reactor
        sta spcspd          ;Speed up stars again
        lda #$20
        sta olmznm          ;Re-Init Objects
        lda #100d           ;Greatest Amount of time
        sta oxygen
        lda #$FF
        sta oxygen+1        ;Reactor Time LSB
        lda #01
        sta shipst          ;Turn on the ship
		;Good place to adjust landing width
        tax 
        cpx #8
        ifcs
            ldx #7      		;Stick here now
        endif
        lda landl,X     	;Landing width Low
        sta widthl
        lda landh,X
        sta widthh      	;Landing width High
		;Clear a bunch of stuff
        lda #00
        sta wrpwh       	
        sta wrpdat
        sta wrpdat+1    	;Clear Number
        sta wrpdat+2
        sta frame       	;Start all new tunnels on even frame
        sta retime      	;Reset reactor time
        sta sittime     	;Stop flipping
        sta tumble
        sta velxh      	;He will stand for a minute
        sta velyh
        sta nenemy      	;None out either
        sta statst      	;Turn off space station
        sta toolong     	;Reset too long timer
        sta newmzr      	;Clear request flag
        ;sta perm5          ;Reset Breakout Finished this frame flag - took this out
        sta ttime           ;Reset Time
        sta condition       
        sta lroff           ;Reset Offset
        ldx #brstat-ballxl  ;Reset Breakout vars across all vars
        begin
            sta ballxl,X
            dex
        miend
        sta numstr      	;No Stars
        ldx #maxstr-1       ;Number of stars
        begin
            sta strflg,X        ;Turn off stars
            dex
        miend
        lda #01
        sta stroyh      	;Set up initial star origin
        lda #$78
        sta stroyl
        lda gamest
        and #$8F
        sta gamest
        bit mzgame          ;In Tact Scan (Could be force wave)
        ifvs                ;Yep, jump away now!
            jmp spcini
        endif
        ldy #00
		;lda maznum
        lda maztype
        cmp #spacefort      ;This needs it too
        beq ?nm5
        cmp #fighter
		beq ?nm5
		lda difcty
		cmp #5
        ifeq                ;For fighters
?nm5        lda #00
            sta lauen           ;Guess don't skip fight
            ldx #nmspace-1
            begin
                lda sobjst,X        ;See if active
                bmi ?nm10           ;If shot, it's dead!
                ifne                ;Is it active??
                    iny         ;yep
                endif   
?nm10           lda #00         ;Now turn it off
                sta sobjst,X        ;Turn off any that might be left
                sta sobdir,X        ;And reset timers
                dex
            miend
            tya
            clc
            adc nenstr          ;In case not all
            sta nenstr          
            sta wtcnt           ;Also must wait
            ifeq                ;There are no more
                bit gamest
                ifmi                ;Only during game play
					lda difcty
					cmp #5				;Cant end Star Castle when ships are gone, shields must be gone too
										;This will be handled in the castle code.
					ifne
						lda #$40
						sta lauen       ;Indicate Skip Fight
					endif
                endif
            endif
        endif
        lda #00
        ldx #nmspace-1
        begin
            sta sobjst,X        ;Turn all off anyway!!!
            cpx #nmshot         ;Clear old shots too
            ifcc    
                sta objst+zshot,X
            endif
            dex
        miend
        lda manstat     ;Here from sucessful exit
        ifne                ;yep! How about that!
spcini      lda #$50        ;5000 bonus points available each wave
            sta bonusa      ;Bonus Possible Next time
			;******************************************
			; Space Level Difficulty Lookups
			;******************************************
            ldx difcty      ;Start Number depends on level
            ;cpx #maxdif
            ;ifcs
            ;    ldx #maxdif       
            ;endif
            lda #10d            ;Fixed at 10d for Spaceforts
			ldy maztype
            cpy #spacefort
            ifne
                lda howmany,X       ;Number of Fighters to start
            endif
            sta nenstr          ;So give him full amount of fighters next time
            sta wtcnt           ;This many waits also
			
			ldx dif4mz			;Raw level index number
            lda shotsp,X        ;Fighter shot speed
            sta shotspeed
			;******************************************
            lda #$80            ;If good exit
            sta initcz          ;First time in, init this play
            lda #00     
            sta lroff           ;0 Offset to next entry
            sta hscan
            sta lauen           ;Allow Launch Now
            sta scflags         ;Clear Star Castle init flags
        else                ;Sorry Jim... He Died... Beam his body up Scotty
            lda #01
            sta manstat         ;Turn on Man!
            lda gamest
            and #$8F            ;Drop Exit and in Maze bits
            sta gamest
            lda mzgame
            ifne                ;Which Play??
                ifmi                ;Was Maze
                    lda #$28            
                    sta shpscl
                    lda #$72
                    sta shpsch      ;Set Scale
                    lda #00
                    sta stscll      ;Set Base Scale
                    sta shpvel+1    ;Stop any motion
                    lda #$73
                    sta stsclh
                    lda #$86
                    sta shipxl      ;Postion Ship
                    lda #$04
                    sta shipxh
                    sta mzgame      ;Back to quick land
                    sta statxh      ;Station's Position
                    lda #$80
                    sta shipyl      ;Place Y Position
                    lda #$06
                    sta shipyh      
                    lda #$80
                    sta statxl      ;Place Base Ship
                    lda #$08
                    sta statyl
                    lda #$07
                    sta statyh
                    lda #$F9        ;Station Status
                    sta statst
                else                ;Must have died landing
                    lda #$10
                    sta mzgame
                    lda #$06
                    lda #03
                    sta shipxh
                    lda #$B8
                    sta shipyl
                    lda #$0B
                    sta shipyh      ;Place ship
                    sta statyh      ;Put station at bottom
                    lda #$80
                    sta statst      ;Get it moving
                    sta stbflg
                endif
            else                ;Was 0
                lda #$40            ;Back to space (Fake short tube_vpg)
                sta mzgame
                lda #00
                sta tcount      ;Causes no tube_vpg, gives end of tube_vpg action however
                jmp initshp     ;Set ship at 'tube_vpg postion'
            endif
        endif
        rts
	
;*****************************************
;* Increment Level Number
;*****************************************
nxtlvl  lda toktarg
		ifmi
			and #$7F
			sta dif4mz
			lda #0
			sta toktarg				;Clear the redirect so we don't come back here again
		else
			lda tokretr
			ifmi
				and #$7f
				sta dif4mz
				lda #0
				sta tokretr			;clear it out
			else
				inc dif4mz			;"Just regular I guess"
			endif
		endif
		lda	dif4mz
		and #03
		sta maznum
		lda dif4mz
		lsr A 
		lsr A
		cmp #maxdif+1
		ifcs
			;flipped too high, push it back to start, this should
			;only happen in demo mode
			lda #0
			sta toktarg
			sta tokretr
			sta tokpock
			sta isfinal
			sta vxstat
			sta maznum
			sta dif4mz
		endif
		sta difcty
		
		;lda maznum
        ;clc
        ;adc #01         ;Next Maze
        ;and #03
        ;sta maznum
        ;ifeq                ;next diff level
        ;    inc difcty  
        ;endif 
		;;Now set dif4mz
		;lda difcty
        ;asl A
        ;asl A
        ;clc
        ;adc maznum
        ;sta dif4mz
		;Set our maztype var too
		jsr stpg6
        ldx dif4mz
		lda mzty,X
		sta maztype			;Look up the MazeType for this level and store it
							;for all routines to reference, this allows levels to have
							;an arbitrary MazeType that is no longer tied to the 
							;level number pattern.
		;set final flag? Not sure if this will work JMA - 01032021
		ldx dif4mz
		lda mscstl,X
		sta isfinal
		;Trip pad master pointer setup
		lda dif4mz
		asl A				;Words
		tax
		lda trtbll,X        ;Trip Pads
		sta trinds
		lda trtbll+1,X
		sta trinds+1  
		jsr stpg0			;Done with Page 6 now
		rts
 
;*****************************************  
;* Init Ship to 'Tunnel' Position        
;*****************************************  
initshp lda #00
        sta shpscl      ;Ship's Linear Scale
        lda #$71
        sta shpsch      ;Ship's Binary Scale
        lda #$C0
        sta shipxl      ;LSB Positions
        sta shipyl
        lda #$05
        sta shipxh
        lda #$08
        sta shipyh      ;MSB Positions
        rts 
        
;*****************************************
;* Init Color RAM                        *
;*****************************************
initcolram 
		bit gamest
        ifmi
            lda vxcount      	;If homeworld is complete, then do random colors
            ifne				;Final maze complete at least once
				lda #00
				sta colram
				ldx #$0F				;Sparkle colors
				begin           
					lda spcolor,X
					sta colram+$10,X
					dex
				miend
				ldx #$0F
				begin
					jsr	getrand
					and #$0F
					tay         		;But Colors are Random
					lda rncolr,Y
					sta colram,X
					dex
				eqend
				rts			;LEAVING NOW
            endif
        endif      
		;fall through
defaultram
		ldx #$1F            ;Fill Color RAM
        begin
            lda stcolr,X
            sta colram,X
            dex
        miend
		rts

doflash lda frame       	;Frame controls the flash color
        and #0f
        tax                     
        lda stcolr,X    	;get color at index
        sta colram+flacol
        rts

;For hidden levels, update the color index for cyanr, we use that color on hidden levels
;for the cycling color.		
doflashx 	
		lda #white 
		sta colram+colcyanr
		lda frame
		and #$40
		ifne
			ldx lasttok
			lda tokacolor,X				;Absolute color values in special table
			sta colram+colcyanr			;save our color in the cyanr color slot
		endif
		rts 
		
;**************************************
    .sbttl "Init Breakout Bricks"
;**************************************
newbrk  lda #$FC
        ldx #02
        begin
            sta brick,X         ;Breakout bricks on
            dex
        miend
        rts        
        
;***********************************************
    .sbttl "Initialize Options\Literals"         
;***********************************************    
initoption 
        sei 
		;Removed all Two-Coin Options code
        ; lda out1s
        ; ora #o_swtchmux     ;Add in Player Select, toggle the resets
        ; sta outputs         ;Get 2 coin min
        ; ldx inputs          ;check two-coin minimum
        ; lda out1s
        ; ora #(o_resetg+o_resetb+o_lighton)  
        ; sta outputs         ;Let them run now (I think this turns on the light too?)
        ; cli 
        ;txa 
        ;asl a           	;Put 2coin in D7
        ;sta twocoin     	;Read Option
        jmp getoptn     	;Get Gamma Options and update RAM locations
        
;***********************************************
    .sbttl "Misc Tables"         
;***********************************************
bitflags
		.db $01,$02,$04,$08,$10,$20,$40,$80
		
tokcolor
		.db colyellow	;Tesser 1
		.db colgreenr	;Tesser 2
		.db colcyan		;Tesser 3
		.db colwhiter	;Tesser 4
		.db colorange	;Homeworld

tokacolor	;This is the direct color data bits used for the maze wall coloring on the token levels
		.db yellow,greenr,redr,cyanr
		
;Attract Musics        
atmusic .db snd_mys, snd_brk, snd_str, snd_hsc

;Game Lives
gamlvs  .db $03,$06,$05,$04				
gamlvsx	.db $00,$00,$00,$00,$03,$02,$01

;Coin Mode Messages
cmodem  .byte mcmode, mcmod1, mcmod2, mcmod3

;Landing Pad Widths
landl       .byte $10,$00,$D0,$C0,$B0,$A0,$A0,$A0
landh       .byte $01,$01,$00,$00,$00,$00,$00,$00

;How many fighters to start with on Space Figher Wave 
;SpaceFort is static @ 10 fighters   
;             1st 5   9   13  17  21  24th         
howmany .byte 16d,18d,20d,22d,24d,28d,22d

;ISSUE #105 - Fix max shot speed
;shotsp  .byte $14,$1C,$20,$24,$28,$29,$29
;shotsp  .byte $14,$1C,$20,$22,$23,$24,$20
;Shotspeed for enemy fighters, one byte for each level
shotsp 	.db $14,$14,$14,$14			;Level 1-4
		.db $18,$18,$18,$18			;5-8
		.db $1C,$1C,$1C,$1C			;9-12
		.db $20,$20,$20,$20			;13-16
		.db $22,$22,$22,$22			;17-21
		.db $24,$24,$24,$24			;Final Station - keep same
		.db $14,$18,$20,$1C			;Hidden levels
	
	

;flash colors
stcolr  .byte black
        .byte blue
        .byte green
        .byte cyan
        .byte red2
        .byte purple
        .byte yellow
        .byte white
        .byte whiter
        .byte pink
        .byte orange
        .byte redr
        .byte red
        .byte cyanr
        .byte bluer
        .byte greenr
        
;Sparkle RAM Color Inits
spcolor .byte pink
		.byte black
		.byte black
		.byte cyanr
		.byte black
		.byte black
		.byte red2
		.byte black
        .byte black
		.byte bluer
		.byte black
		.byte black
		.byte purple
		.byte black
		.byte black
		.byte whiter
        
rncolr  .byte blue
        .byte green
        .byte cyan
        .byte yellow
        .byte white
        .byte orange
        .byte redr
		.byte pink
		.byte cyanr
		.byte whiter 
		.byte orange 
		.byte purple 
		.byte bluer
		.byte greenr
		.byte bluer 
		.byte greenr
;********************************************
; DEBUG Frame Counter Display
;********************************************
#IF (DEBUG != 0) | (LEVEL_EDITOR != 0)
showframes
		lda mloopdis
		ifne
			;write out the buffer
			lda # ubyte(dbgbuf)
			sta vglist+1
			lda # lbyte(dbgbuf)
			sta vglist
			jsr vgcntr
			VGADD_SCALE(ywin_off,binscal3,0)
			vgadd_stat(sparkle_off,xflip_off,0,$F,colred2)       
			VGADD_VCTRL(-1250,-1000,hidden)
			VGADD_SCALE(ywin_off,binscal2,$30)
			lda mloopdis
			jsr decimal     ;Total time in dec to temp7
			lda #temp7
			ldy #2
			sec 
			jsr digits      ;Display Total Time
		else
			lda #$C0
			sta dbgbuf
		endif
		rts
#ENDIF

