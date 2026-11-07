


;******************************************************************************
    .title "RPM Main Routines"
;******************************************************************************
    .sbttl "IRQ Routine"
;******************************************************************************
;* This is the main timing routine of the RPM module. It takes care of syncing
;* all sound effects and communication.
;*
;* The pokey update function is called every IRQ. The subroutines called from
;* it take care of updating the sounds.
;*
;*  pokdrv - Selects specific pokey chip
;*  prodev - gets pokey address and channel offset
;*  updpokey - ?
;*  ?
;*
;* I believe this is getting called every 200ns
;******************************************************************************
    
g_irq   cld             ;NMOS 6502 bug won't clear decimal on interrupt, must always clear
        sta intack
        pha 
        lda irqcnt      ;irqcnt always counts from 4-1
        lsr A
        dec irqcnt
        ifmi
			;***********************************
			;special code to trigger speech every 3 seconds
;#IF DEBUG != 0
			; inc sirqcnt
			; ifeq
				 ; inc lastoki
				 ; lda lastoki
				 ; cmp #o_okiptr
				 ; ifcs
					; lda #0
					; sta lastoki
				 ; endif
				 ; jsr okicmd
			; endif
;#ENDIF
			;***********************************
            lda #___NPKS
            sta irqcnt      ;Reset when becomes -
        endif
        ifcs
            txa 
            pha 
            tsx 
            cpx #$02		;if SP is at $01 or aggghh... $00, then we are too close to overflow... reset it
            iflt
                ldx #$FF        ;Reset the stack pointer
                txs 
                jmp c_reset     ;Re-init this processor and throw away stack
            endif
            tya 
            pha 
            lda framecnt    ;Get the current frame count
            jsr game_irq    ;Do game specific IRQ stuff
            jsr pokdrv      ;Update the Pokeys
			inc framecnt
            pla 
            tay 
            pla 
            tax
        endif
        pla 
        rti
        
;***********************************************
        .sbttl "Pokey Driver"
;***********************************************
;* This is the main routine for the pokey's    
;* It takes care of sharing pokey time between 
;* the 4 pokeys by updating only 2 at a time.  
;*
;* If Speech is loaded, it calls the speech 
;* update routine too.
;***********************************************       
pokdrv 

#IF ___TLK == 1
        jsr updspeech
#ENDIF
        lda framecnt
        lsr A
        ifcs                ;Lets do the even ones now
            ldx #00
            jsr prodev
            ldx #02
            jsr prodev    
        else               ;Do the odd pokey's instead
            ldx #01
            jsr prodev
            ldx #03   
            jsr prodev
        endif
        
#IF ___TLK == 1
        jsr updspeech
#ENDIF  
        rts
		
;************************************************
;* Initialize all the RPM sound RAM and prepare *
;* for running.                                 *
;************************************************
initrpm lda #00
        php 
        sei 
        sta qhead
        sta qtail
        sta qflag     ;Clear the report flag (not used in MH)
        ldx #___CHNS+(___NPKS*4)+(___NGIS*3)-1 ;#$1F        
        begin
            sta ch_vlnks,X
            dex 
        miend
        ldx #___CHNS-1
        begin
            sta ch_viu,X
            sta ch_onote,X
            sta ch_onote+___CHNS,X
            sta ch_vvol,X
            sta ch_ctrlmask,X		;clear all the volumes and poly settings
            dex
        miend
        ldx #___DVCNT-1             ;Do this for each POKEY in the QUAD
        begin
            lda devadl,X
            sta devptr
            lda devadh,X
            sta devptr+1
            lda #00
            ldy #POKEY_AUDCTL
            sta (devptr,Y)
#IF ___FIXPKY = 1
            stx fasave
            clc 
            lda devptr
            sta fasave+1
            lda #P.SKCTL            ; point to SKCTL register to init.
            adc devptr
            sta devptr
            ldx #00
            txa 
            sta (devptr,X)          ; store with no pre-read...
            begin
                adc #01
                cmp #03             ; (DELAY)
            eqend
            sta (devptr,X)
            ldx fasave
            lda fasave+1
            sta devptr              ; restore DEVPTR and X
#ELSE
            ldy	#P.SKCTL	        ; IFPOKEY-
            sta	(DEVPTR,Y)
	        begin
                adc	#01
                cmp	#03		            ;(DELAY)
            eqend
            sta (devptr,Y)	        ;INIT DEVICE
#ENDIF    
            ldy #POKEY_AUDCTL       ;SET AUDCTL
#IF ___CTL = 0
            lda #00
#ELSE
            lda	#$78		        ;FOR 16-BIT VOICES
            sta	devctl,X	        ;AND ITS SHADOW
#ENDIF

            sta (devptr,Y)
            lda #00
            ldy #07+1		        ;clear pokey device
            begin
                dey 
                sta (devptr,Y)          ;ZERO ALL VOICES
            eqend
            dex 
        miend
		;Special addition... initrpm should also init the max background sound too
		;jsr initmaxbuf		;Initialize the Max background sound data into RAM
        ; nop
        ; nop
        ; nop
        ;Remove above later
        plp 
        rts 
        
;**********************************************************************************************************
;* Slave Command Parser - This routine will check to be sure that our command is valid. It will then   
;* pull that commands controlling routine out of the tables and also load the local parameter for that 
;* command and pass it to the routine via the A reg. 
;**********************************************************************************************************
cmdpro  cpy #o_commandnum           ;Is it above our max command value?
        ifcc            
            lda t_commandtype,Y
            cmp #(rt_immed+1)           ;Is it above the valid command type?
            ifcc    
                asl A               ;X2
                tax 
                lda cmdtbl+1,X         ;Get the address of the function routine
                pha 
                lda cmdtbl,X
                pha 
                lda t_cmdparam,Y        ;Send the Name as well for reference
            endif
        endif
        rts             ;Done

;****************************************************
    .sbttl "ATTRACT/SILENT Level"
;****************************************************
;* This routine sets the priority level at which    
;* sounds will be heard.                            
;*                                                  
;* Accumulator Value            Result          
;*    00                        All Sounds(Noisy)     
;*    FF                        No Sounds (Silent)    
;*    XX                        Priority Cutoff       
;****************************************************
setslvl sta silevel
        rts 
        
;****************************************************
    .sbttl "Set/Reset E-Flag"
;****************************************************
#IF ___FLG = 1
sceflg  pha 
        and #07
        tay 
        pla 
        and #$80
        sta sflags,Y
        rts 
#ENDIF       

;*************************************************
;* Call a routine by name                        *
;*************************************************
callf   asl A           ;Directly call a CPU subroutine.
        tay 
        lda t_userfunction+1,Y
        pha 
        lda t_userfunction,Y
        pha 
        rts             ;Do it!

;*************************************************
;* Kill a sound by priority level                *
;*************************************************          
killp   php 
        sei 
        sta fasave
        ldy #___CHNS-1
        begin
            lda ch_viu,Y
            lsr A
            eor fasave
            ifeq
                lda #-1
                sta ch_sid,Y        ;Kill this Channel
            endif
            dey
        miend
        plp 
        rts
        
;*************************************************
;* Kill a sound by ID which is in A
;*************************************************
killid  php 
        sei 
        tay 
        lda t_commandtype,Y     ;Get sound function type (00-10 or FF for ignore)
        cmp #(rt_sound)         ;Is this a sound function??
        ifeq                    ;yep!
            lda t_cmdparam,Y        ;Get the ID of the sound number to kill
            sta fasave
            ldy #___CHNS-1
            begin
                cmp ch_sid,Y        ;Does this Sound ID match?
                ifeq                ;yes
                    lda #-1     
                    sta ch_sid,Y        ;Kill it!
                    lda fasave
                endif
                dey
            miend
            plp
        endif
        rts

;*************************************************
;* Kill a particular sound by device and channel *
;*************************************************  
killch  sta fasave     ;A contains two nibbles of info
        lsr A           ;MSN = Device Number  (0-3)
        lsr A           ;LSN = Channel Number (0-3)
        lsr A
        lsr A
        tay             ;Y = Device
        lda fasave
        and #$0F
        clc 
        adc devq,Y    ;Channel Offset 
        tay 
        php 
        sei 
        lda ch_vlnks,Y
        ifne
            begin   
                tay
                dey 
                lda #-1
                sta ch_sid,Y        ;Kill this one!
                lda ch_vlnks,Y
            eqend
        endif
        plp 
        rts
            
;*******************************************************
;* Report: Sends a preset bytes back to the Alpha      *
;*         processor. This macro is not used in MH.    *
;*******************************************************
repbyte ldy qtail
        sta gw_queue,Y
        iny 
        cpy #$80
        ifcs
            ldy #00
        endif
        cpy qhead
        ifne
            sty qtail
            rts
        endif   
        lda #$80
        sta qflag     ;Set the report flag (not used in MH)
        rts 

#IF ___IMM != 0    
;********************************************************
;* Immed: Command Immediate Response to a function code 
;*                                                    
;********************************************************   
tryimm  tay 
        lda t_cmdparam,Y    ;Get the ID of the sound
        tax             
        lda t_immfunc,X     ;Get the function number
        cmp #$1C            ;Is it higher than allowed
        bcs ?ret            ;yep, ignore it!
        cmp #F.SCH
        ifcs
            cmp #F.RCH+1
            bcc ?ret
            cmp #F.SUB
            ifcs
                cmp #F.16B+1        ;$12
                bcc ?ret
                cmp #F.RPT          ;$16
                ifcs
                    cmp #F.BR           ;$19
                    bcc ?ret
                endif
            endif
        endif                   ;Here for functions 00,01,02,03,04,05,06,07,0A,0B,0C,12,13,14,15,19,1A,1C
        asl A                   ;x2
        tay                     ;Y = offset into cmdtbl
        lda t_immschn,X
        clc 
        adc #___CHNS            ;$10; point to queue header
        pha 
        lda t_immarg,X
        sta iarg                ;Save it in the Argument holder for later
        lda t_immsid,X
		;and #$7F                ;Why is this here? This will improperly limit immediates to only sid's less then $7F which would be bad.
        tax                     ;get SID from its own start...
        lda t_commandtype,X
        cmp #rt_sound           ;Was this a sound command?
        ifeq                    ;yes
            lda t_cmdparam,X        ;Store the ID of this sound
            sta imm_id      
            pla 
            tax 
            jmp immed
        endif
        pla 
?ret    rts
#ENDIF

;***********************************************************
;R$STUNE
;	SUBROUTINE TO START A TUNE OR NOISE
;
;	IF slave mode,
;		A = SOUND PTR
;		chains through tables starting several sounds
;	otherwise,
;		A = TUNE NUMBER (0...)
;		X = VOICE NUMBER (0...)
;		Y = PRIORITY ( non-0 )
;
;	RETURNS CS IF SOUND CAN'T BE STARTED
;		Y = Channel Number
;
;***********************************************************
stune   sta cur_snd     			;Currend Sound ID# here
        tay 
        ldx t_tunestarts,Y          ;Load the start offset into the tune table
        ;PUT THIS BACK nop
#IF ___REP = 1
        lda t_replacement,Y         ;This happens to always be 0xff in TW = Always Replace
        ifeq
            lda cur_snd                   ; if not REPLACE, see if running
            ldy #___CHNS-1
            begin
                cmp ch_sid,Y        ;this one?
                ifeq                ;yes
                    lda ch_viu,Y        ;Still Playing?
                    ifne
                        rts         		;Yes, get outta here - Ignore 
                    endif
                    lda cur_snd     	;Next data for loop
                endif
                dey         ;Loop it!
            miend   
        endif
#ENDIF
        begin
            stx sindex              ;Store away our start sindex for later
            ldy #___CHNS-1
            begin
                lda ch_viu,Y
                beq openchan            ;Found an empty Channel!
                dey
            miend
            ;If here, there were no empty channels
            ;Look at the channel to see if it has a lower priority
            ldy t_priority,X
            lda t_schan,X
            clc 
            adc #$10                ;Channel + $10
            tax 
            tya 
            php 
            sei                     ; ** CRITICAL SECTION **
            ldy ch_vlnks+___CHNS,X     ;ch_tmp-$10,X        ;X = Channel + $10
            ifne
                dey 
                sec                 ; * 2 + 1
                rol A
                cmp ch_viu,Y
            else_cs
                plp 				;new sounds is not higher priority, just leave now
                sec 
                rts
            endif   
            ldx sindex          	;Get Back sindex Index
            lda ch_vlnks,Y
            sta ch_vlnks+___CHNS,X  ;ELSE UNLINK AND USE THIS BLOCK
            plp                     ;RE-ENABLE INTERRUPTS
            ldx sindex          	;Get Back start sindex Index
            ;Found an open Channel @ Y
            ;Initialize starting values
openchan    lda #DEFAULT_VOLUME 	;Default Volume Level
            sta ch_vvol,Y           ;set up to median volume
            lda #RATE_DEFAULT
            sta ch_rate,Y           ;set up to med. RATE
            lda #CHCTL_NOPOLY		;Poly = PURE TONE (default), Zero volume
            sta ch_ctrlmask,Y       ;store as voice control bits
#IF	___CTL = 0            
            lda #-1
            sta ch_vcctl,Y          ; AND mask
#ENDIF            
            lda #0
#IF	___CTL = 0   
            sta ch_vsctl,Y          ; OR mask
#ENDIF
            sta ch_vlnks,Y          ;clear queue pointer
            sta ch_key,Y            ;set back to unused key
            sta ch_vsn,Y            ;clear tune SEQ. count
            sta ch_odur,Y
            sta ch_odur+___CHNS,Y
            sta ch_synthmode,Y      ;clear SYNTH. MODE
            sta ch_loopcount,Y      ;clear loop count
            sta ch_synthfadj,Y      ;clear synth. mode adjustment
            sta ch_synthfadj+___CHNS,Y
            sta ch_jmppc+___CHNS,Y  ;clear sub-phrase pointer (FLAG)
            
#IF ___CNT != 0	    ;IF NOTE COUNTER ENABLED...
            sta	ch_vcnt,Y		    ;CLEAR NOTE COUNT
#ENDIF
#IF ___SCK != 0 	;IF SYNC. CHECK ENABLED...    
            sta ch_ochkm,Y          ;SET CLASS TO NULL
            sta ch_ochk,Y           ;CLEAR CURRENT CHECK MASK
#ENDIF
            ;ENVELOPE INITIALIZATION
            sta ch_freqdidx,Y
            sta ch_flcnt,Y
            sta ch_ampseq,Y
            sta ch_amplcnt,Y
            lda # lbyte(nullenvelope)
            sta ch_amp,Y            ;SET UP POINTERS TO NULL ENVELOPE
            sta ch_freq,Y
            lda # ubyte(nullenvelope) 
            sta ch_amp+___CHNS,Y
            sta ch_freq+___CHNS,Y
			
            lda t_priority,X        ;Get our new priority
            sec                     ;AND ASSUME 16-BIT CHANNEL 
            rol A                   ; * 2 + 1
            sta ch_viu,Y            ;SET PRIORITY FIELD ( 16-BIT )
            lda t_schan,X           ;get queue header
            clc 
            adc #___CHNS              
            sta ch_tmp              ;Store it too
            lda t_stune,X           ;Get this sounds tune data offset
            tax 
			lda t_tune_l,X
			sta ch_tuneptr,Y
			lda t_tune_h,X
			sta ch_tuneptr+___CHNS,Y
            ldx sindex
#IF ___FLG = 1
            lda t_eflag,X       ;See if this sound needs to clear an Event Flag
            ifpl
                sta ch_eflg,Y       ;Save in this channels data, owns this flag
                tax 
                lda #00
                sta sflags,X     ;Clear the Flag
            endif
#ENDIF
            ldx ch_tmp
            php 
            sei 
?ssl45      stx ch_tmp
            lda ch_vlnks,X		;Is something already playing here?
            ifne
                tax 				;rebias to sound channel index
                dex 
                lda ch_viu,X
                ora #01				;+1 for comparison
                cmp ch_viu,Y
                bcc ?ssl45			;Nope, check next slot
                ifne
                    inx 
                    txa 
                    bcs ?ssl55
                endif
                lda #00
                sta ch_viu,X
                lda ch_vlnks,X
?ssl55          sta ch_vlnks,Y		;Make it so
                ldx ch_tmp
            endif
            lda cur_snd     	;Get Sound ID back
            sta ch_sid,Y        ;Store it in the Channel Defines
            iny 
            tya 
            sta ch_vlnks,X		;Offically book it now! This is POKEY register indexed ($00-01F)
            plp 
            ldy sindex
            ldx t_tunecont,Y    ;Does this sound continue into another?
        eqend
        rts

#IF ___OKI == 1

okicmd	;Command index is in A
		php
		sei
		;***************************
		; For Scott Swazey's FPGA
		; implementation
		;***************************
		sta fpgaspeech
		;***************************
		pha
		tay                     ; Y contains our actual OKI index now I think
		lda t_okicmd,Y        	; Load the OKI Sound Command here
		jsr	okiwrite
		;restore y for bitflip
		pla
		tay
		lda t_okiattch,Y      	; Load the OKI Attenuation data nibble
		;ora t_okichan,Y       	; Load the OKI Channel #
		jsr	okiwrite      		; Send it to the OKI
		plp
		rts

; ;does the /ws dance to latch in a command
; also flips bits due to D0 being connected to D7 etc
okiwrite	
		;jsr bitflip
		sta tidata	
		sta tiwrite
		;ldx #2
		;begin
		;	dex
		;miend
		sta tidata
		rts 

;Swaps bits making D0 = D7 and D7 = D0 plus all bits inbetween		
; bitflip pha
		; and #$0F
		; tax
		; pla
		; lsr A
		; lsr A
		; lsr A
		; lsr A
		; tay
		; lda _bf1,x
		; ora _bf2,y
		; rts

; _bf1 	.db $00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0
; _bf2 	.db $00,$08,$04,$0C,$02,$0A,$06,$0E,$01,$09,$05,$0D,$03,$0B,$07,$0F

#ENDIF




#IF ___TLK == 1

talkit  ldy ti_stat
        ifeq
			;we are TISTAT_IDLE, do it
talkit2 	php
			sei
			;***************************
			; For Scott Swazey's FPGA
			; implementation
			;***************************
			sta fpgaspeech
			;***************************
			asl A
			tay 
			lda t_spdptr,Y
			sta ti_addr
			lda t_spdptr+1,Y
			sta ti_addr+1
			lda t_spdlen,Y 
			sta ti_len
			lda t_spdlen+1,Y 
			sta ti_len+1
			lda #$80
			sta ti_stat
			plp
			rts
		endif
		;This seems to advance the current talk queue and save A into 
		php     
        sei     
        ldy tiquelst
        iny     
        cpy #ticmdbflen
        ifcs
            ldy #00
        endif
        cpy tiquecur
        ifne
            sty tiquelst
            sta tiqueue,y
        endif
        plp     
        rts     
		
;***********************************************
;* Reset Speech: Resets all speech params to   *
;*               initial state.                *
;***********************************************

res_ti  php
        sei
        ldy #00        
        sty tiquecur                ;Clear the buffer pointer offsets
        sty tiquelst 
		;**********************************************************
		; Doesn't do anything in mhavoc
		;**********************************************************
		sty tireset                           
		;**********************************************************
		;**********************************************************
        sty tiwrite                 ;Turn off TI /WS   
        dey                                  
        sty ti_count                        
        lda # ubyte(deftalk)               	;Load up the dummy data into the sample params           
        sta ti_addr+1                          
        lda # lbyte(deftalk)                            
        sta ti_addr                          
        lda # ubyte(deftalklen)    	;Set the dummy data length                       
        sta ti_len+1                          
        lda # lbyte(deftalklen)                           
        sta ti_len                          
        lda irqcnt                         
        clc                
        adc #$10                            
        ora #$01                            
        sta ti_irqcnt                    	;Set to IRQ count+$11      
        lda #TISTAT_START                            
        sta ti_stat                     ;Let it start filling data                   
        plp      
        rts 

;This is default(dummy) data for the speech init to point to
deftalk .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
        .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
deftalkend

deftalklen = deftalkend - deftalk

;******************************************************************************
;* Update Speech: Called from IRQ (twice), to 
;* queue data into the speech chip buffer.           
;******************************************************************************     
TICMD_NOP		= $00	;Same as 80
TICMD_READ		= $10	;Same as 10
TICMD_NOP2		= $20	;Same as 20
TICMD_READB		= $30	;Same as 30
TICMD_LOAD		= $40	;Same as 40
TICMD_SPEAK 	= $50	;Same as 50
TICMD_SPEAKEX	= $60   ;Same as 60
TICMD_RESET 	= $FF   ;Same as 70

updspeech   
        ; lda	ti_irqcnt
        ; ifne
            ; sec     
            ; sbc	irqcnt
            ; beq ?ups10
            ; sbc #08
            ; ifeq
				; rol A
				; sta tireset
; ?ups10          lda	#00
                ; sta ti_irqcnt
            ; endif
            ; rts     
        ; endif
        lda #tirdyflg
        bit tirdy       ;check to see if the TI has read the last value
        ifne            ;if still high, then it is not ready for another byte, start a timer
            lda	irqcnt
            lsr A
            bit ti_count
            ifpl
                eor ti_count
                bne ?tirts      ;get out, will come back again later
                beq res_ti      ;if we are here, the TI was not ready for a long time, must reset it
            endif
            adc #$10
            and #$7f
            sta ti_count
            bpl ?tirts      ;update the counter and leave for later
        endif
        ;If we are here, then we have something to do
        lda #TICMD_RESET            ;reset the counter first
        sta ti_count
        sta tidata	;tiwrite         ;this disables the TI /WS signal (negative -> positive transition)
        lda tiframe
        ifmi
            asl     A
            eor     irqcnt
            bne     ?tirts
            sta     tiframe         ;tiframe = $00 here
            lda     #TICMD_RESET
            bne     ?tiwr       ;write an $FF here
        endif
        lda     #TICMD_RESET	;Data for RESET, written if not changed below
        ldy     ti_stat         ;Get the software status of the TI
        ifeq                
			;we are TISTAT_IDLE 
            ldy     tiquecur        
            cpy     tiquelst
            beq     ?tiwr           ;Is the TALK queue caught up? Yes if zero, write an TICMD_RESET to the TI and then move on.
            iny                     ;Move to next queue slot
            cpy     #ticmdbflen     ;Is it at the top?
            ifcs    
                ldy     #00             ;roll it over
            endif
            sty tiquecur            ;Save new current queue index
            lda tiqueue,y           ;Get the next talk command, fall through to load it up
            jmp talkit2
        endif
        iny     
        bne ?ups12			
        lda (ti_addr,Y)         ;Get next data byte
        inc ti_addr             ;Increment pointer
        ifeq
            inc ti_addr+1           ;Dont forget to carry over!
        endif
        dec ti_len          ;Take one off
        ifeq                ;Out of data LSB!
            dec ti_len+1
            ifmi                    ;Out of data MSB, just plain out of data!
				ldy #$FF
				sty fpgaspeech
                ldy #TISTAT_WAIT
                sty ti_stat         	;$11 cycles to wait for next speech phrase
                ifeq					;NEVER
?ups12              cpy #$81					;Last value of ti_stat + 1
                    bne ?decstat				;This is TRUE 
                    lda #TISTAT_SENDING
                    sta ti_stat
                    lda #TICMD_SPEAKEX        	;TI Speak External Command
                    ifeq						;NEVER - FALL THROUGH
?decstat                dec ti_stat
                        lda #TICMD_NOP       ;This is a TI NOP Command here
                    endif
                endif
            endif
        endif
?tiwr   sta tidata          ;write a byte here!
		sta tiwrite
?tirts  rts 

#ENDIF

;***********************************************
;* Table Pointers to each macro routine        
;***********************************************
#DEFINE CMD(ecall)  \ .word ecall-1

#DEFINE CMDCONFIG(configoption,ecall,dcall) \#IF configoption = 1
#DEFCONT                                                 \ .word ecall-1 
#DEFCONT                                                 \#ELSE
#DEFCONT                                                 \ .word dcall-1
#DEFCONT                                                 \#ENDIF

#DEFINE CMDCONFIGOR(copt1,call1,copt2,call2,nocall) \#IF copt1 = 1
#DEFCONT                                            \#IF copt2 = 1
#DEFCONT                                            \ .error "Both passed configuration options are enabled, which is not allowed. Ensure only one configuration is enabled."
#DEFCONT                                            \#ELSE
#DEFCONT                                            \ .word call1-1 
#DEFCONT                                            \#ENDIF
#DEFCONT                                            \#ELSE
#DEFCONT                                            \#IF copt2 = 1
#DEFCONT                                            \ .word call2-1
#DEFCONT                                            \#ELSE
#DEFCONT                                            \ .word nocall-1
#DEFCONT                                            \#ENDIF
#DEFCONT                                            \#ENDIF

; cmdtbl  .word setslvl-1     ;00 - Set Silent/Attract Level
; #IF ___FLG = 1
        ; .word sceflg-1      ;01 - Set/Reset E-Flag
; #ELSE
        ; .word nocmd
; #ENDIF
        ; .word callf-1       ;02 - Call Subroutine
        ; .word killp-1       ;03 - Kill Sound by Priority
        ; .word killid-1      ;04 - Kill Sound by ID
        ; .word killch-1      ;05 - Kill Sound by Device
        ; .word stune-1       ;06 - Start Sound
        ; .word repbyte-1     ;07 - Report Value
; #IF ___TLK == 1
        ; .word talkit-1      ;08 - Talk LPC
; #ELSE
; #IF ___OKI == 1
		; .word okicmd-1
; #ELSE
		; .word nocmd
; #ENDIF
; #ENDIF

cmdtbl
        CMD(setslvl)                                    ;00 - Set Silent/Attract Level
        CMDCONFIG(___FLG,sceflg,nocmd)                  ;01 - Set/Reset E-Flag
        CMD(callf)                                      ;02 - Call Subroutine
        CMD(killp)                                      ;03 - Kill Sound by Priority
        CMD(killid)                                     ;04 - Kill Sound by ID
        CMD(killch)                                     ;05 - Kill Sound by Device
        CMD(stune)                                      ;06 - Start Sound
        CMD(repbyte)                                    ;07 - Report Value
        CMDCONFIGOR(___TLK,talkit,___OKI,okicmd,nocmd)  ;08 - Talk LPC or ADPCM
        CMDCONFIG(___IMM,tryimm,nocmd)      ;09 - Execute Immediate Mode Command

nocmd	rts
;**********************************************************************************
;**		LNKFLW - Link Follower Routine
;**********************************************************************************
;
;   COMMON OUTPUT ROUTINE FOR ALL VOICES
;    X	- OFFSET TO CURRENT VOICE
;   PREV	- POINTER TO QUEUE HEADER
;
;	RETURNS:
;	    X	- INDEX TO LAST (HIGHEST PRIORITY) IN QUEUE
;	VOLSH	- VOLUME OF 8-BIT CHANNEL
;	VOLSH+1	- ( IF NE,...SVV) VOLUME OF 16-BIT CHANNEL
;	FRQSH	- FREQUENCY OF 8-BIT CHANNEL ( IF EQ,...SVV) WORD OF 16-BIT CHANNEL
;	FRQSH+1	- ( IF NE,...SVV) WORD OF 16-BIT FREQUENCY
;	CSMSK	- SET MASK WORD
;	CCMSK	- CLR MASK WORD ( IF NE,...SVV) FOR AUDCTL
;
;	NOTE:	THAT ANY SOUND CAN BE KILLED BY SETTING VSID TO -1.
;**********************************************************************************
lnkflw  lda ch_viu,X            ;If Priority was #00, then just leave, never plays
        beq ?lnkex
        ldy ch_sid,X            ;See if a sound has been assigned here
        iny 
        ifeq
            tya 
?lnkex      jmp unlink               ;If the ch_sid was #$FF, then leave
        endif
#IF ___SVV != 0
        and #01                 ;Check priority flag LSB
        sta po_svf			    ;Save the Even/Odd flag for the rest of the RPM code
        tay 
        lda ch_viu,X
        sta po_pri,Y
#ENDIF
        lda ch_tuneptr,X        ;Pointer to data stream for this sound LSB
        sta tuneptr         	;Save the pointer in RAM LSB
        lda ch_tuneptr+___CHNS,X  	;Pointer to data stream for this sound MSB
        sta tuneptr+1         	;Save the pointer in RAM MSB
        sec 
        lda ch_odur,X     	    ;Frames to keep this value
        sbc ch_rate,X     		;Subtract our channels sample rate ($40 is default)
        sta ch_odur,X     	    ;Save it for next time
        bcs ?lnk2               ;IF MUST DEC. MSB'S *** 04/08/83 ***                
        dec ch_odur+___CHNS,X	;did it go zero... yes, take away 1 from MSB of sample rate counter 
        ifpl                    ;IF HAVE A NEW NOTE
?lnk2       jmp poksn1   			;IF STILL SAME NOTE *** 04/08/83 ***
        endif
;
;IF HERE, HAVE A NEW NOTE TO PLAY
;        
poknote ldy ch_vsn,X
        tya 
        clc 
        adc #02					;Always 2 bytes per instruction
        sta ch_vsn,X
        ifcs
            inc ch_tuneptr+___CHNS,X		;Update the base high pointer if this flipped over 255
        endif
        lda (tuneptr,Y)         ;Is this data a sound function selection?
        ifmi                    ;Yes, only if minus
            jsr pkfunc             ;special function here
            ifcs
                lda ch_tuneptr+___CHNS,X
                sta tuneptr+1
                bcs poknote     ;Always
            endif
            lda #00
            ldy po_svf
            sta po_pri,Y
            jmp poknoff
			;*******************
        endif
        ;Here for a standard NOTE, A = positive for a NOTE and ZERO for 1-Time Sound
        sty fasave         	;Store away our current pc location for later
        ldy po_svf
        ifne                    ; if music, check for rest..
            cmp #00
            beq ?rpm5               ;IF NOTE IS A REST (NO OUTPUT)
        endif
        clc 
        adc ch_key,X			;POSITION NOTE TO CURRENT KEY
?rpm5   
#IF ___SVV != 0
        ldy po_svf
        ifeq
            ;**   8 BIT REGISTER (NOISE) UPDATE **
#IF ___SYM != 0             	;CHECK IF SYNTH. MODE
            pha                 ;SAVE NEW VALUE, TEMP.
            sec 
            sbc ch_onote,X      ;SUB. PREVIOUS NOTE
            sta ch_synthfadj,X  ;STORE RESULT FOR SYNTH. MODE
            asl A               ; *** 04/08/83 ***
            lda #$FF                ;ELSE, SET MSB'S TO NEG.
            ifcc
                lda #00
            endif
            sta ch_synthfadj+___CHNS,X
            pla                     ;GET BACK. NEW NOTE VALUE	
#ENDIF
            sta ch_onote,X          ;ELSE, DON'T PERFORM TABLE LOOKUP,
            lda #00                 ;USE VALUE DIRECTLY
            sta ch_onote+___CHNS,X  ;8 BIT VALUE ONLY...
        else 
#ENDIF
            ;**** 16-BIT REGISTERS ****
			asl A				    ;SHIFT SO HAVE WORD OFFSET
			tay 
#IF ___SYM != 0	                    ;IF SYNTH. MODE ENABLED...
			sec                     ;COMPUTE DIFF. BETWEEN NEW AND OLD
			lda ch_onote,X          ;GET OLD NOTE VALUE
			sbc notes-2,Y           ;SUB. NEW NOTE VALUE
			sta ch_synthfadj,X
			lda ch_onote+___CHNS,X
			sbc notes-2+1,Y
			sta ch_synthfadj+___CHNS,X    ;STORE DIFF. BETWEEN NEW AND OLD FOR SYNTH MODE
#ENDIF
			lda notes-2,Y
			sta ch_onote,X
			lda notes-2+1,Y
			sta ch_onote+___CHNS,X
#IF ___SVV != 0
		endif
#ENDIF
		ldy fasave             ;RESTORE Y
        iny 					;INC. TO NOTE DURATION *** 04/08/83 ***
        lda #00
        sta fasave             ;CLEAR LSB'S OF DURATION (WILL MOVE INTO ODUR)
        lda (tuneptr,Y)         ;GET CURRENT DURATION
        ifeq                        ;Command Finished
            ;CHECK FOR ENDING SUB-PHRASE OR MAIN TUNE
            lda ch_jmppc+___CHNS,X            ;CHECK SUB-PHRASE CALLING ADDR (FLAG)
            ifne                        ;IF NOT AT END OF MAIN PHRASE
                ;IF HERE, RETURNING FROM SUB-PHRASE
                ;
                ;	NOTE:	to allow synthesizer mode to work across sub-phrases,
                ;		the last notes should be duplicated:
                ;		eg.  NOTEA/DUR  NOTELAST/DUR  NOTELAST/0
                ;		should terminate a sub-phrase.
                sta ch_tuneptr+___CHNS,X
                sta tuneptr+1
                lda ch_jmppc,X
                sta ch_tuneptr,X
                sta tuneptr
                lda #00
                sta ch_jmppc+___CHNS,X
                lda ch_jmplocpc,X
                sta ch_vsn,X
                jmp poknote					;Continue outside execution back at old routine NOTE: NO 8ms 'REST' ON RETURN
            endif
            ;IF HERE, ARE AT END OF THIS VOICE'S TUNE, RESET	
unlink      sta ch_viu,X        
            sta ch_vvol,X
			sta ch_sid,X			;added this JMA 4/27/2020
            lda ch_vlnks,X
            ldx po_prev
            sta ch_vlnks,X
            lda #00
            jmp poknoff				;Continue in current outside context
        endif
        ; IF HERE, STILL HAVE A NEW NOTE, GET AND STORE DURATION
		;********************************************************************
		;SDATA with data byte in A, these are flags of some sort
		; $80-$04: Valid value which is the 
		; $02	 : ROR into fasave, adc into ch_odur+___CHNS, add into ch_odur
		; $01	 : When clear, initialize the FREQ and CRTL streams with next pointer data
		;
		; Special Values:
		; $FF - Branch
		; $FE - 
		;********************************************************************
nxtnote lsr A                   ;SHIFT AND REMOVE LSB (TIED NOTE FLAG)
        lsr A                   ;MULT. DUR * 64 (CARRY IS MSB OF LSB BYTE)
        ror fasave				;fasave now has a bit set at $80 from original bit $02 in SDATA followup command
        pha                     ;SAVE MSB'S
        sec                     ; *** 04/08/83 ***
        lda fasave
        adc ch_odur,X           ;SUBTRACT ANY OVERRUN FROM PREV. NOTE
        sta ch_odur,X
        pla 
        adc ch_odur+___CHNS,X
        sta ch_odur+___CHNS,X   ;STORE MSB'S OF DURATION
#IF ___CNT != 0	                ;IF NOTE COUNTER ENABLED...
        inc	ch_vcnt,X		    ;INC. NOTE COUNT
#ENDIF
        lda (tuneptr,Y)         ;Get initial Data Value, this is the 'note' length it seems
        and #01					;Even intial values will trigger a reset of FREQ and AMP data and move to next of each
        ifeq	                ;If note is tied, go play it
			;****************************************************************
			;FREQ initialization here.... sets up all pointers and init data
			;****************************************************************
			;Clear all these A = 0
            sta ch_synthfadj,X
            sta ch_synthfadj+___CHNS,X
            sta ch_fdelta,X		    ;set freq values to zero
            sta ch_fdelta+___CHNS,X
            sta ch_fdelta+___CHNS+___CHNS,X
            sta ch_flcnt,X
			tay						;Clear Y too 
            lda ch_freq,X			;Load pointer to frequency table
            sta tuneptr
            lda ch_freq+___CHNS,X
            sta tuneptr+1
            lda (tuneptr,Y)         ;Get index freq data which was specified by the SDATA byte 2 data (this value is -1 of actual stored value)
            sta ch_fcnt,X
            iny 					
            ora (tuneptr,Y)         ; CHECK NULL ENVELOPE
            iny 
            ora (tuneptr,Y)         ; point to MSB of Fdelta
            ifne
                inc ch_fcnt,X		; ENSURE DO AT LEAST ONCE
                tya
            endif   
            sta ch_freqdidx,X		;This is zero if all 3 bytes were zero
            ;******************************************
            ;DONE FREQUENCY
			;******************************************
			;AMPLITUDE INIT
			;******************************************
            lda #00
            tay 
            sta ch_ampdlta,X
            sta ch_amplcnt,X
			;Point to base of amplitude table
            lda ch_amp,X
            sta tuneptr
            lda ch_amp+___CHNS,X
            sta tuneptr+1
            lda (tuneptr,Y)         ;Loading Initial amplitude Register data (one less, incremented later) here...
            sta ch_ampcnt,X
            iny 
            ora (tuneptr,Y)         ;CHECK NULL ENVELOPE
            ifne
                inc ch_ampcnt,X		;Add one always in Byte 1
                tya
            endif   
            ;DONE AMPLITUDE
            sta ch_ampseq,X		;Zero will be stored here if AMP data ended with $00,$00 (does this mean it loops back to start?)
            ldy po_svf
            ifne
                jmp poknoff
            endif
        endif
        lda ch_synthmode,X
        ifeq
            sta ch_synthfadj,X
            sta ch_synthfadj+___CHNS,X
        endif
poksn1  lda po_svf
        ifne                            ;never rest on noise channel...
            lda ch_onote,X              ;TEST FOR REST
            ora ch_onote+___CHNS,X
            ifeq
                jmp poknoff
            endif           
        endif
#IF ___SYM != 0
        lda framecnt
        and #06
        ifeq
            lda ch_synthfadj+___CHNS,X
            asl A
            ror ch_synthfadj+___CHNS,X
            ror ch_synthfadj,X
        endif
#ENDIF

#IF ___FRE != 0	                        ;IF FREQ. ENV. ENABLED...
		;*******************************
		; Process Frequency Data Stream
		;*******************************
        lda ch_freq,X
        sta tuneptr
        lda ch_freq+___CHNS,X
        sta tuneptr+1
    #IF ___RLE = 0	                    ;*** DO OLD WAY IF NOT Run length encoded ***
        LDA	#0
        STA	FASV
        LDA	X,VSEQ		;GET FREQ. ENV. SEQ. #
        LSR			;FOR 128 BYTE TABLE
        TAY
        LDA	NY,OPTRC	;GET CURRENT FREQ. ENV. SETTING
        ifmi
            DEC	FASV		; AND SIGN EXTEND INTO FASV
        endif
        #IF ___SVV != 0
        LDY	SVF		;TEST FOR AN 8-BIT VOICE OR NOISE CHANNEL
        ifne		;IF NOT MUSIC SKIP
        #ENDIF
            ASL			;MULT. ENV. * 8 FOR MUSIC
            ROL	FASV
            ASL
            ROL	FASV
            ASL
            ROL	FASV		;MULT FREQ. ENV. *8
        #IF ___SVV != 0    
        endif
        #ENDIF    
        LDY	FASV
        CLC
        ADC	X,ONOTE		        ;COMPUTE LSB'S OF OUTPUT FREQ.
        STA	FASV		        ;SAVE RESULT, TEMP
        TYA			            ;GET MSB OF FREQ. ENV (0 OR FF)
        ADC	X,ONOTE+...CHNS	    ;COMPUTE MSB VALUE
        ;*** END NOT RUN-LENGTH ENCODED ***
    #ELSE                           ; *** YES RUN-LENGTH ENCODED ***
        ldy ch_freqdidx,X
        ifeq                        ;IF NULL, NO ENVELOPE LEFT
            tya 
            clc                     ;(ALWAYS) A,Y=0;CARRY CLEAR
        else    
            dec ch_fcnt,X			;ELSE DECREMENT DURATION TO ZERO - Initial Data +1 is here, now one less (this is the FREQ countdown)
            bne ?rm17
            ;	here we get the next values- so ensure that the pointer won't
            ;	wrap...
            begin						;Here if Freq Counter was Zero (trigger)
                begin
                    begin
                        iny 
                        lda (tuneptr,Y)         ;LOOP or count..
                        sta ch_fcnt,X		    ;Save it to channel RAM
                        iny                     ;Point to first argument
                        cmp #$FF				;Was it $FF - this indicates a loop
                        bne ?rm15               ;No, go process
                        ;Process loop terminator
                        lda ch_flcnt,X		    ;Check for new loop
                        beq ?ldjcnt				;If it was zero then this is first time, load count
                        dec ch_flcnt,X          ;Check for end of loop
                        bne ?rm14
                        iny                     ;Point to last argument, go do next env.
                    eqend                   ;NEVER?
?ldjcnt             lda (tuneptr,Y)         ;Get next byte... new loop's count
                    sta ch_flcnt,X		    ;Save it
?rm14               iny                     ;point to last argument ( offset to loop )
                    sec 
                    lda tuneptr				;Load loop offset (positive numbers go backwards)
                    sbc (tuneptr,Y)         ;Subtract offset
                    sta tuneptr				;Branch backwards
                    sta ch_freq,X			;Save in RAM too
                ccend                   
                dec tuneptr+1
                dec ch_freq+___CHNS,X
            csend                   ;NEVER!
            ;Process envelope delta
?rm15       ora (tuneptr,Y)         ;Get next tune data
            iny                     ;( MSB delta )
            cpy #$FF-6              ;Ensure space for next, even if end-of-loop
            ora (tuneptr,Y)         ;TEST FOR 0,0 ( END SIGNAL )
            bne ?rm16
            sta ch_freqdidx,X       ;IF END,SET FSEQ TO ZERO
            tay 
            clc 
            ifne
?rm16           tya                     ;Else Setup next change
                ifcs
                    sbc #(1+1)              ;-1 to backup pointer, -1 to correct CS on add
                    adc ch_freq,X
                    sta ch_freq,X         ;and add to env.pointer for next time...
                    lda #01
                    ifcs
                        inc ch_freq+___CHNS,X
                    endif
                endif
                sta ch_freqdidx,X       ;set pointer to MSB of delta
?rm17           ;here if FREQ counter is not zero yet
				lda #00
                sta fasave+1
                lda (tuneptr,Y)         ;Get next FREQ data
                ifmi
                    dec fasave+1			;Make MSB negative for loaded byte, sign extend
                endif
                sta fasave				;Finish updating fasave (word) with data
                dey 					;Put Y back
                lda (tuneptr,Y)         ; MSB,sign in FASV.. LSB in ACC
                asl A
                rol fasave
                rol fasave+1
                asl A
                rol fasave
                rol fasave+1
                asl A
                rol fasave             ; FASV,A = HI/LO FREQUENCY
                rol fasave+1           ; 24-BIT (16-bit*8,sign extended...)
                clc 
                adc ch_fdelta,X
                sta ch_fdelta,X     ; fractional part
                lda fasave				
                adc ch_fdelta+___CHNS,X
                sta ch_fdelta+___CHNS,X
                sta fasave             ;LSB OF DELTA
                lda fasave+1
                adc ch_fdelta+(___CHNS*2),X
                sta ch_fdelta+(___CHNS*2),X
                tay                     ;MSB OF DELTA
                clc 
                lda fasave
            endif
        endif
        adc ch_onote,X              ;Y,A = offset (MSB/LSB),carry clear
        sta fasave                  ;LSB OF NOTE W/ MOD
        tya 
        adc ch_onote+___CHNS,X
    #ENDIF                              ;*** RUN-LENGTH ENCODED FREQUENCY ***
    ; FREQUENCY IN A,FASV 
#ENDIF			                    ;NE,...FRE
#IF ___FRE = 0
        LDA	X,ONOTE
        STA	FASV
        LDA	X,ONOTE+...CHNS	        ;LOAD NOTE
#ENDIF
#IF ___SYM != 0	                    ;IF SYNTH. MODE ENABLED...
        tay                         ;SAVE MSB'S TEMP
        ;NOW ADD SYNTH ADJUST TO SUM OF NOTE FREQ. AND ENV.
        clc 
        lda fasave                  ;GET BACK LSB'S
        adc ch_synthfadj,X          ;ADD WITH SYNTH ADJUST
        sta fasave                  ;SAVE BACK LSB'S
        tya 
        adc ch_synthfadj+___CHNS,X  ;COMPUTE FINAL FREQ. MSB VALUE
#ENDIF
#IF ___SVV = 0	                    ; 16-BIT STORE ONLY
        STA	FRQSH+1
        LDA	FASV
        STA	FRQSH
#ELSE
        sta po_frqsh+2              ;STORE MSB'S OF NOTE
        ldy po_svf                  ; ** 04/08/83 **
        lda fasave                  ;GET BACK LSB'S OF NOTE FREQ.
        sta po_frqsh,Y              ;STORE LSB'S OF NOTE
        ; NOTE: 16-BIT NOISE IN FRQSH,FRQSH+2
#ENDIF
		;************************************
		; NOW COMPUTE AMPLITUDE, THIS VOICE
		;************************************
        lda ch_amp,X
        sta tuneptr
        lda ch_amp+___CHNS,X
        sta tuneptr+1				;Set up pointers again
        ldy ch_ampseq,X			;Do we have data at this index?
        ifeq
?2ampdone  lda ch_vvol,X
           jmp ?ampdone				;Nope, move on
        endif
        dec ch_ampcnt,X			;-1.... is it ready now for action?
        ifeq
            begin
                begin
                    begin
                        iny 
                        lda (tuneptr,Y)         ;Get next AMP data
                        sta ch_ampcnt,X
                        iny 					;move next
                        cmp #$FF				;Is this a branch/jump
                        bne ?rm22
                        lda ch_amplcnt,X		;Yes, load the previous loop counter
                        beq ?rm20
                        dec ch_amplcnt,X		;If it wasn't zero, then this is weird, subtract one
                        bne ?rm21				;branch if was at != 1 originally
                        iny 
                    eqend
?rm20               lda (tuneptr,Y)         ;Load the loop counter and save it
                    sta ch_amplcnt,X
?rm21               iny 
                    sec 
                    lda tuneptr
                    sbc (tuneptr,Y)         ;Subtract the offset
                    sta tuneptr
                    sta ch_amp,X			;Save it and continue
                ccend
                dec tuneptr+1
                dec ch_amp+___CHNS,X
            csend
?rm22       cpy #$FF-6				;This must be the max length of a AMP data stream?
            ora (tuneptr,Y)         ;OR with next data
            ifeq
                sta ch_ampseq,X		;AMP data stream is done, end this sound 
                jmp ?2ampdone
            endif
            tya 
            ifcs					;If the first AMP data is $F9 or greater, then do this..
                sbc #02				;This is (FirstByte|SecondByte)-2
                adc ch_amp,X
                sta ch_amp,X			;Adjust the outside loop by this amount (allows a loop to update data)
                lda #01
                ifcs
                    inc ch_amp+___CHNS,X
                endif
            endif
            sta ch_ampseq,X		;Save our last AMP data indexer
        endif
        lda (tuneptr,Y)         ;Get next AMP data byte (Addend)
        clc 
        adc ch_ampdlta,X		; ( 5-BIT INTEGER,3-BIT FRACTION)
        ifvs
            lda #$80
            ifcc
                lda #$7F
            endif
        endif
        sta ch_ampdlta,X		;AND ACCUMULATE
        lsr A
        lsr A
        lsr A
        cmp #$10				;Signed upper nybble, FORCED VOLUME
        ifcs
            ora #$F0				;Mask for PCM Data in LSB
        endif					;Examples: $3C -> $07, $CC -> $F7
        clc 
        adc ch_vvol,X			;Add base volume level back in
        ifmi
            lda #00
        endif
        cmp #$10                ;CHECK FOR VOLUME TOO LARGE
        ifcs                    ;IF VOLUME NOT OK
            lda #$0F			;If it flipped (over 15), then reset to Max volume
        endif
		;Fall through
?ampdone   
		ora ch_ctrlmask,X	    ;Get the TONE bits that define the poly dividers etc
poknoff 
#IF ___SVV != 0
        ldy po_svf
        sta po_volsh,Y
    #IF ___CTL = 0
        cpx #___CHNS
        ifcc
            lda ch_vsctl,X
            sta po_csmsk,Y
            lda ch_vcctl,X
            sta po_ccmsk,Y
        endif
    #ENDIF
#ELSE
        STA	VOLSH
    #IF ___CTL = 0
        CPX	# ...CHNS
        ifcc
            LDA	X,VSCTL
            STA	CSMSK
            LDA	X,VCCTL
            STA	CCMSK
        endif
    #ENDIF
#ENDIF
        lda ch_vlnks,X          ;POINT TO NEXT IN QUEUE
        ifne
            stx po_prev             ;ADJUST BACK-LINK
            tax                     ;AND POINT TO NEXT
            dex                     ;BY CORRECTING POINTER
            jmp lnkflw              ;PROCESS NEXT LOGICAL CHANNEL
        endif
        rts 


#IF ___NPKS != 0
;***************************************************************
;	****  HALFPK  -  PROCESS ONE HALF POKEY  ****
;
;	Y	- QUEUE HEADER OF EVEN HALF
;
;	RETURNS:
;	CS	- 16 BIT MODE
;	VOLSH	- TWO BYTES OF VOLUME
;	FRQSH+1 - TWO BYTES OF FREQUENCY	
;	CCMSK+3 - CLEAR-MASK FOR AUDCTL
;	ACC	- SET MASK FOR AUDCTL
;***************************************************************
;* Parse the next RPM instruction from the logic loop.
;* 
;* Y = channel offset
;*
;* If Carry SET on return, then channels is configured for 
;* 16-Bit Frequency Register values.
;***************************************************************
halfpk  tya 				;Preserve Y
        pha 
        iny                 ;Goto next index first (jump head, will go back down below)
        lda #00             ;Clear all of these parameters by default
        sta po_pri8
        sta po_vol8
        sta po_pri
        sta po_pri+1
        sta po_volsh+1
        sta po_volsh
#IF ___CTL = 0
        sta po_csmsk
        sta po_csmsk+1
        sta po_csmsk+2
        lda #$FF            ;Set to -1 for these
        sta po_ccmsk
        sta po_ccmsk+1
        sta po_ccmsk+2
#ENDIF
        ldx ch_vlnks,Y		;TEST QUEUE FOR 0
        ifne					;If not zero, then we have something to do
            dex                 ;CORRECT POINTER
            sty po_prev
            jsr lnkflw          ;GET SHADOW VALUES FOR POKEY
            lda po_pri          ;ODD-CHANNEL
            sta po_pri8
            lsr A               ;check for R$SLEVL muting
            cmp silevel
            lda #00
            ifcs                ;if R$SLEVL lower than PRI,sound OK
#IF ___CTL = 0
                lda po_csmsk
                sta po_csmsk+2
                lda po_ccmsk
                sta po_ccmsk+2      ;save oddchannel AUDCTL masks
#ENDIF
                lda po_volsh
            endif
            sta po_vol8
            lda po_frqsh
            sta po_frq8
            lda #00
            sta po_pri
            sta po_pri+1
            sta po_volsh+1
            sta po_volsh
#IF ___CTL = 0
            sta po_csmsk
            sta po_csmsk+1
            lda #$FF
            sta po_ccmsk
            sta po_ccmsk+1
#ENDIF
        endif
        pla 
        tay 
        ldx ch_vlnks,Y			
        ifne                    ;GET QUEUE FOR EVEN-CHANNEL
            dex                     ;CORRECT POINTER
            sty po_prev
            jsr lnkflw
        endif
        lda po_pri
        cmp po_pri+1
        ifcc
            lda po_pri+1
        endif
        lsr A                   ;highest even-channel priority...
        cmp silevel             ;if lower than R$SLEVL,silence channel...
        ifcc                    
            lda #00
            sta po_volsh
            sta po_volsh+1
#IF ___CTL = 0
            sta po_csmsk
            sta po_csmsk+1
            lda #$FF
            sta po_ccmsk
            sta po_ccmsk+1
#ENDIF
        endif
        lda po_pri
        cmp po_pri8             ;GET HIGHEST OF 8-BIT PRIORITIES
        ifcs
            sta po_pri8
        endif
        lda po_pri+1
        cmp po_pri8             ;DECIDE 8/16 BIT RESOLUTION
        ifcs
			;Carry Set: 16-Bit Audio Config
            lda #00
            sta po_volsh         ;EVEN CHANNEL OFF FOR MUSIC
#IF ___CTL = 0
            lda po_ccmsk+1
            and po_ccmsk+3
            sta po_ccmsk+3      ;determine AUDCTL masks
            lda po_csmsk+1
            ora po_csmsk+3
#ENDIF            
            ;rts
		else
			;Carry Clear: 8-Bit Audio Config
			lda po_frq8
			sta po_frqsh+2        ;This is the data that will be written to the EVEN Channel AUDF register
			lda po_frqsh
			sta po_frqsh+1         ;GET FREQUENCIES
			lda po_vol8
			sta po_volsh+1        ;AND VOLUMES
#IF ___CTL = 0
			lda po_ccmsk
			and po_ccmsk+2
			and po_ccmsk+3
			sta po_ccmsk+3
			lda po_csmsk
			ora po_csmsk+2
			ora po_csmsk+3
#ENDIF
		endif
		;Carry comes out too
        rts 

;**************************************************************
    .sbttl "Pokey Dump"
;**************************************************************
;* This routine will dump data specific to a physical pokey   
;* chip. The base address of the pokey chip to update is in   
;* devptr.                                       
;*                                                            
;* Inputs: X = Pokey Chip Index                               
;* Pokey Base Address: devptr
;* Channel Offset: devtype
;**************************************************************
;
;	SETUP A,Y  W/ EVEN,ODD QUEUE POINTERS
;	CALL HALFPK
;	USE CS FOR 8/16 BIT MODE SELECT
;
;	INPUT:	X	- INDEX TO DEVICE TABLE
;		    Y	- FIRST QUEUE HEADER
;		    DEVPTR	- POKEY ADDRESS ZPAGE POINTER
;		    DEVTYP	- 0 ( POKEY )
;
;	NOTE: it is possible that shadows should be kept
;	of the first pokey half processed, so that all
;	POKEY registers can be stored at once, after the
;	AUDCTL register has been altered...
;
;	ALSO, ...SVV flag behavior needs to be re-thought..
;**************************************************************
pokeyout  
#IF ___CTL != 0
        lda devctl,X
        sta	ctlsh		;SAVE DEVICE CONTROL SHADOW
#ELSE
        lda #00
        sta po_csmsk+2+(2*___SVV)
        lda #-1
        sta po_ccmsk+2+(2*___SVV)  
#ENDIF
        txa 
        pha                 ;Push pokey index for later
        lda devq,X          ;Load this chips channel offset
        pha                 ;Push for later
        tay 
        iny 
        iny                 ;Y = channel offset + 2
        jsr halfpk
#IF ___CTL = 0
        ifcs
            ora #AUDCTL_CH3_179+AUDCTL_34_16B   ;#$28
        endif
        sta po_csmsk+3
#ELSE
    \#IF ___SVV != 0	            ; IF POSSIBLY 8-BIT,
        lda	ctlsh		    ;SET CONTROL REGISTER
        bcc	?pk1
        ora	#AUDCTL_CH3_179+AUDCTL_34_16B  ;28		        ;16-BIT
        ifcc		        ;(ALWAYS)
?pk1        and	#$D7		    ;8-BIT
        endif
	    sta	ctlsh
        ldy	#P.AUDCTL	    ;AUDCTL
        sta	(devptr,Y)
    \#ENDIF
#ENDIF  ;...SVV ALLOWS RECONFIGURE
        ldy #4              ;upper pokey half
        lda po_frqsh+___SVV	;FREQUENCY
        sta (devptr,Y)      ;Pokey Channel 3 Freq
        iny 
        iny 
        lda po_frqsh+1+___SVV
        sta (devptr,Y)      ;Pokey Channel 4 Freq
        dey 
        lda po_volsh
        sta (devptr,Y)      ;Pokey Channel 3 Control
        iny 
        iny 
#IF ___SVV != 0
        lda po_volsh+1
#ENDIF
        sta (devptr,Y)      ;DONE BOTH CHANNELS

        pla                 ;Pull pokey channel offset
        tay                 ;Channel Offset
        jsr halfpk
#IF ___CTL = 0              ;if automatic-8/16 select,
        ifcs
            ora #AUDCTL_CH1_179+AUDCTL_12_16B   ;$50    
        endif
        sta po_csmsk+3      ;CSMSK+3
#ENDIF
        pla                 ;Pull pokey chip index
        tax 
#IF ___CTL != 0
    \#IF ___SVV != 0             ;If Possibily 8-bit
        lda	ctlsh		    ;SET CONTROL REGISTER 
        ifcs
            ora	#50		        ;16-BIT
            bcs	?pk12		        ;(ALWAYS)
        endif
        and	#$AF		    ; 8-BIT
?pk12	sta	devctl,X
        ldy	#P.AUDCTL	    ;AUDCTL
        sta (devptr,Y)
    \#ENDIF
#ELSE
        lda po_csmsk+2+(2*___SVV)
        and po_ccmsk+2+(2*___SVV)
        ldy #P.AUDCTL       ;Offset to the AUDCTL Register for this entire POKEY
        sta (devptr,Y)    
#ENDIF
        ldy #00
        lda po_frqsh+___SVV  ;FREQUENCY
        sta (devptr,Y)       ;Pokey Channel Odd
        iny 
        iny 
        lda po_frqsh+1+___SVV
        sta (devptr,Y)       ;Pokey Channel 2 Freq
        dey 
        lda po_volsh
        sta (devptr,Y)       ;Pokey Channel 1 Control
        iny 
        iny 
#IF ___SVV != 0
        lda po_volsh+1
#ENDIF
        sta (devptr,Y)       ;Pokey Channel 2 Control
        rts

#ENDIF	;POKEYS EXTANT

;****************************************
	.SBTTL	DEVICE PROCESSOR - PRODEV
;****************************************
;	****  PROCESS DEVICE           
;****************************************
;	ENTER WITH X -	DEVICE INDEX
;	USES DEVTABLES TO PROCESS DEVICE
;****************************************
;* This routine takes the index passed  
;* in X and sets up devptr to hold   
;* the address that pokeyout will update. 
;****************************************   
prodev  lda devadl,X       ;Get Pokey LSB
        sta devptr
        lda devadh,X       ;Get Pokey MSB
        sta devptr+1
        lda devid,X        ;Load the channel offset for later
        sta devtype
#IF ___NPKS > 0
    #IF ___NGIS > 0
        ;both defined (not common)
        ifne               ;PROCESS POKEY IF 0
            jmp	psgiout		    ;ELSE PROCESS GI.
        endif
        jmp pokeyout
    #ELSE
        jmp	pokeyout	   ; ONLY POKEYS
    #ENDIF
#ELSE
    #IF ___NGIS > 0
        jmp psgiout		; ONLY PSGIS
    #ELSE
        .error "No Sound devides are defined. Define either GI or POKEY's in device table."
        ;DONT BOTHER TO COMPILE, YOU HAVE NO SOUND DEVICES
    #ENDIF
#ENDIF


;****************************************
	.SBTTL	FUNCTION DEFINITIONS
;****************************************
;	*****	FUNCTION               ******
;****************************************
;
;	IF HERE, HAVE A FUNCTION, NOT A NOTE...
;
;		ACC	= function number
;		OPTRC,Y	= pointer to function
;
;	functions enter with
;		CARRY SET
;		ACC 	= argument
;		OPTRC,VSN
;			= pointer to next note
;		X	= index to SCB
;
;	Exit with
;		CS	= clear if done with voice this interrupt
;		OPTRC,VSN
;			= pointer to next note
; 
;****************************************       
pkfunc  cmp #($80+_maxfcn)	    ;#f_clctl+1    ;Base of 0x80(must be minus to be a function call) 
        ifcs                    ;With a limit of functions (0x1C)
            jmp unlink               ;Not a function selector
        endif
        iny 
        stx fasave             ;SAVE OFFSET, TEMP
        asl A
        tax 
        lda sfunctbl+1,X        ;POSITION INTO DISP. TABLE
        pha                     ;PUSH ONTO STACK
        lda sfunctbl,X
        pha 
        ldx fasave
        lda (tuneptr,Y)         ;GET VALUE FOR ALL ROUTINES
        ldy ch_tuneptr+___CHNS,X      ;XFER INTO COMMON POINTER (IN CASE INC. OCCURED TO MSB'S)
        sty tuneptr+1
        sec 
        rts                     ;DISPATCH TO APPROPRIATE FUNCTION


#IF ___IMM != 0      
;*******************************************
;**   IMMEDIATES  
;*******************************************
;
;	Similar to functions, IMMEDIATE entry forces changes to
;	one of the control block registers immediately; this is
;	used to alter the characteristics of a sound.
;
;	ENTER:
;		X	- VLNKS index to channel
;		Y	- function index into PKDT
;		IMSID	- SID of sound to be altered
;		IMARG	- argument
;  
;*******************************************  
immed   php 
        sei 
        begin
            lda ch_vlnks,X
            beq ?ms10
            tax 
            dex 
            lda ch_sid,X    
            cmp imm_id
        eqend
        jsr ?doimm
?ms10   plp 
        rts
 
?doimm  lda sfunctbl+1,Y
        pha 
        lda sfunctbl,Y
        pha 
        lda iarg    ;get the arg if passed too
        rts
#ENDIF

;************************************************************************
;* Special Sound Functions - These routines can be called from the main 
;* tune definitions to alter the normal sounds.                         
;*                                                                      
;* All special functions will return with carry set or not set. 
;************************************************************************
sfunctbl 
    FUNC(f_srate,ssetrate)                  ;80 - Set Sample Rate Explicitly
    FUNC(f_arate,saddrate)                  ;81 - Add to Sample Rate
    FUNC(f_sebvol,ssebamp)                  ;82 - Set Amplitude
    FUNC(f_adbvol,sadbamp)                  ;83 - Add to Amplitude
    FUNC(f_sbfreq,ssetkey)                  ;84 - Set Base Frequency 
    FUNC(f_abfreq,saddkey)                  ;85 - Add to Base Frequency  
    CFUNC(___FRE,f_freq,sfreqenv,poknote)   ;86 - Set the Frequency Envelope
    CFUNC(___AME,f_amp,sampenv,poknote)     ;87 - Set the Amplitune Envelope
    CFUNC(___SCK,f_pkchk,spkchk,poknote)    ;88 - SEE IF ALL CHECK POINTS REACHED
    CFUNC(___SCK,f_pkrchk,spkrchk,poknote)  ;89 - RESET MASK, THIS VOICE REACHED CHECK 
    FUNC(f_ctrlm,ssetctrlm)                 ;8A - Set Channel Control Register
    FUNC(f_dctl,sdctl)                      ;8B - Set Pokey Chip Control Register
    CFUNC(___SYM,f_syn,ssynth,poknote)      ;8C - Set value of SYNTH mode
    FUNC(f_sjsr,sjsr)                       ;8D - JSR to another 'Tune'?
    FUNC(f_sloops,sloopi)                   ;8E - Loop Start
    FUNC(f_sloope,sloopj)                   ;8F - Loop End
    CFUNC(___SVV,f_8bitv,set8bitv,poknote)  ;90 - Set to 8-bit Voice
    CFUNC(___SVV,f_16bitv,set16bitv,poknote);91 - Set to 16-bit Voice
    CFUNC(___NGIS,f_envsh,genvsh,poknote)   ;92 - ENVELOPE SHAPE	
    CFUNC(___NGIS,f_nzper,gnper,poknote)    ;93 - NOISE PERIOD
    CFUNC(___NGIS,f_nzperl,genvpl,poknote)  ;94 - ENVELOPE PERIOD LO
    CFUNC(___NGIS,f_nzperh,genvph,poknote)  ;95 - ENVELOPE PERIOD HI
    FUNC(f_repb,srepb)                      ;96 - REPORT BYTE TO GAMEBOARD 
    CFUNC(___FLG,f_skipfl,sskipfl,poknote)  ;97 - SKIP 2 BYTES IF FLAG
    CFUNC(___FLG,f_diefl,sdiefl,poknote)    ;98 - DIE IF FLAG 
    FUNC(f_braback,sbraback)                ;99 - Branch Back
    CFUNCOR(___TLK,___OKI,f_speek,speek)    ;9A - Say Phrase     
    FUNC(f_clctl,sclctl)                    ;9B - CLEAR CONTROL BITS

;IF HERE, HAVE A TIMING CHECK FUNCTION, WAIT FOR OTHER VOICES TO CATCH UP
spkchk ldy ch_ochk,X
        ifne
            iny 
            bne ?sf8_5
            sec 
            rts
        endif
        lda ch_ochkm,X
        sta ch_ochk,X
        ldy #___CHNS-1
        begin
            cmp ch_ochkm,Y
            ifeq
                cmp ch_ochk,Y
                ifne
                    lda ch_viu,Y
                    bne ?sf8_5
                    lda ch_ochkm,X
                endif
            endif
            dey 
        miend
        ldy #___CHNS-1
        begin
            cmp ch_ochkm,Y
            ifeq
                lda #$FF
                sta ch_ochk,Y
                lda ch_ochkm,X
            endif
            dey
        miend   
        clc 
        rts 
		
?sf8_5  lda ch_vsn,X
        cmp #02             ;Will this flip upper byte
        ifcc                    ;yes
            dec ch_tuneptr+___CHNS,X      ;take it down one
        endif
        dec ch_vsn,X
        dec ch_vsn,X            ;Remove them now
        lda #00
        sta ch_odur,X
        sta ch_odur+___CHNS,X
        clc 
        rts 

;IF HERE, WANT TO RESET CHECK FLAG, THIS CHANNEL
spkrchk sta ch_ochkm,X
        lda #00
        sta ch_ochk,X
        rts

;IF HERE, HAVE AN AMPLITUDE ENV. FUNCTION      
sampenv 
		tay 
        lda t_ampenv_l,Y
        sta ch_amp,X
        lda t_ampenv_h,Y
        sta ch_amp+___CHNS,X
        sec 
        rts 
        
;IF HERE, HAVE A FREQ. FUNCTION       
sfreqenv
        tay 
        lda t_freqenv_l,Y
        sta ch_freq,X
        lda t_freqenv_h,Y
        sta ch_freq+___CHNS,X
        sec 
        rts

;IF HERE, HAVE A RATE FUNCTION            
saddrate 
        clc 
        adc ch_rate,X
ssetrate
        sta ch_rate,X
        sec 
        rts 

;IF HERE, HAVE A NEW MEDIAN AMPLITUDE        
sadbamp clc 
        adc ch_vvol,X
ssebamp sta ch_vvol,X
        sec 
        rts 

;*********************************************
; Pitch Shift - Used to set a static shift
;               (positive only?) that will 
;               be added to any notes played.
;               Handy for the same tune at 
;               different harmonies.
;*********************************************    
;IF HERE, HAVE A NEW KEY OFFSET    
saddkey clc 
        adc ch_key,X
ssetkey sta ch_key,X
        sec 
        rts 

;*********************************************************************
; Custom addition to Promised End
;
;Variant of above except A is a Zero page RAM address that contains 
;the actual base freq value;
;*********************************************************************		
saddkeyd 
		tay
		lda 0,Y
		clc 
        adc ch_key,X
		sta ch_key,X
        sec 
        rts 
		
ssetkeyd 
		tay
		lda 0,Y
		sta ch_key,X
        sec 
        rts 

;IF HERE, HAVE NEW VOICE CONTROL BITS        
ssetctrlm
		sta ch_ctrlmask,X
        rts 

;IF HERE, WANT TO SET UP NEW CONTROL BITS        
sdctl 	ldy devtype
        ifne
#IF ___CTL != 0
            eor po_ctlsh
#ELSE
            eor po_csmsk
#ENDIF
            and #09
#IF ___CTL != 0
            eor po_ctlsh
        endif
        sta	po_ctlsh
#ELSE
            eor po_csmsk
        endif
        ora ch_vsctl,X
        sta ch_vsctl,X
#ENDIF            
        rts 

#IF ___CTL = 0
;IF HERE, WANT TO CLEAR BITS IN CONTROL REGISTER        
sclctl  eor #$FF
        ldy devtype
        ifeq
            and ch_vcctl,X
            sta ch_vcctl,X
            rts
        endif
        ora #$F6
        and ch_vsctl,X
        sta ch_vsctl,X
        rts 
#ENDIF

;HERE TO MODIFY SYNTHESIZER MODE SELECT        
ssynth  sta ch_synthmode,X
        rts 
    
;************************************************
;* Jump Sound - JSR to another sound (A). Push  
;* current location into jump locations.        
;************************************************
sjsr    ;asl A
        tay 
        lda ch_vsn,X        ;Push current local channel ptr
        sta ch_jmplocpc,X
        lda ch_tuneptr,X
        sta ch_jmppc,X        ;Put old address low byte
        lda ch_tuneptr+___CHNS,X
        sta ch_jmppc+___CHNS,X        ;Put old address high byte
        lda t_tune_l,Y         	;Get new address
        sta ch_tuneptr,X      ;Push it current
        sta tuneptr             ;Zero page too
        lda t_tune_h,Y
        sta ch_tuneptr+___CHNS,X
        sta tuneptr+1
        lda #00
        sta ch_vsn,X        ;Reset the local channel ptr
        sec 
        rts 
    
;************************************************
;* Loop Init - Sets the loop counter to value   *
;* of accumulator and pushes position of current*
;* tune location onto the loop pc.              *
;************************************************   
sloopi  sta ch_loopcount,X
        lda ch_vsn,X
        sta ch_looplocalpc,X
        lda ch_tuneptr,X
        sta ch_looppc,X
        lda ch_tuneptr+___CHNS,X
        sta ch_looppc+___CHNS,X
        rts 

;************************************************
;* Loop Jump - Will jump to the loop pc location*
;* unless the loopcount is at 0. The loop count *
;* is lowered each call.                        *
;************************************************       
sloopj  dec ch_loopcount,X
        ifne
            lda ch_looplocalpc,X
            sta ch_vsn,X
            lda ch_looppc,X
            sta ch_tuneptr,X
            sta tuneptr
            lda ch_looppc+___CHNS,X
            sta ch_tuneptr+___CHNS,X
            sta tuneptr+1
        endif
        rts 

        
set8bitv 
        ldy po_svf
        lda #00
        sta po_pri,Y
        lda ch_viu,X
        and #$FE
        sta ch_viu,X
        ldy #00
        sty po_svf
        sta po_pri,Y
        rts
            
set16bitv ldy #01
        sty po_svf
        ora ch_viu,X
        sta ch_viu,X
        sta po_pri,Y
        rts 

#IF ___NGIS != 0
genvsh  ldy	#G.ENVSHP	    ; INDEX TO SHAPE REGISTER ( ** 12 ** )
        sta	(devptr,Y)
        rts

gnper	ldy	#G.NZPER	    ; INDEX TO NOISE PERIOD ( ** 13 ** )
        sta	(devptr,Y)
        rts

genvpl	ldy	#G.ENVPER	    ; INDEX TO ENVELOPE PERIOD LO ( ** 14 ** )
        sta	(devptr,Y)
        rts

genvph	ldy	#G.ENVPER+1	    ; INDEX TO ENVELOPE PERIOD HI ( ** 15 ** )
        sta	(devptr,Y)
        rts
#ENDIF
        
srepb   jsr repbyte
        sec 
        rts

#IF ___FLG != 0            
sskipfl ldy ch_eflg,X           ;See if this channel controls an event flag
        ifpl
            lda sflags,Y
            ifne
                clc 
                lda #02
                adc ch_vsn,X
                sta ch_vsn,X
                ifcs
                    inc ch_tuneptr+___CHNS,X
                endif
            endif
        endif
        sec 
        rts
            
sdiefl ldy ch_eflg,X           ;See if this channel controls an event flag
        ifpl
            lda sflags,Y
            ifne
                lda #-1
                sta ch_sid,X        ;Blank the channel sound ID
                clc
            endif
        endif
        rts
#ENDIF       
;*************************************************
;* Branch Back - Go back (A) number of bytes.      
;*************************************************  
sbraback    
        sta fasave      
        lda ch_vsn,X
        sbc fasave
        sta ch_vsn,X
        ifcc
            dec ch_tuneptr+___CHNS,X
        endif
        sec 
        rts
 
#IF (___TLK + ___OKI) != 0
speek   
    #IF ___TLK != 0
        jsr	talkit	        ; SPEECH PHRASE ON THE TI	( ** 1A ** )
    #ELSE
        #IF ___OKI != 0
            jsr okicmd          ; SPEECH PHRASE ON THE OKI	( ** 1A ** )
        #ENDIF
    #ENDIF
        sec
        rts
#ENDIF 
;***********************************************************************************
; DEVICE TABLES
;***********************************************************************************         
devadl  .byte lbyte(POKEY_0),lbyte(POKEY_1),lbyte(POKEY_2),lbyte(POKEY_3)
devadh  .byte ubyte(POKEY_0),ubyte(POKEY_1),ubyte(POKEY_2),ubyte(POKEY_3)
devid   .byte ubyte(POKEY_CH0),ubyte(POKEY_CH1),ubyte(POKEY_CH2),ubyte(POKEY_CH3)
devq    .byte POKEY_CH0+$10,POKEY_CH1+$10,POKEY_CH2+$10,POKEY_CH3+$10

;*********************************************************
    .sbttl "Non-Maskable Interrupt"
;*********************************************************
;* The NMI input is triggered any time the Alpha         *
;* processor sends data to the gamma.                    *
;*********************************************************
g_nmi   pha 
        lda #alpharcvd
        begin
            bit portst
        neend
        tya 
        pha 
        ldy datnum      ;Check Command Mode
        ifeq                ;Data Mode
            clc 
            ldy r_nptr      ;Get the number of bytes written
            tya
            ifmi    
                eor #$80
                tay 
                lda xferbuf
                eor #$80
                sta xferbuf
                ifpl
                    inc xferbuf+1
                endif
                tya
            endif   
            adc #01
            sta r_nptr
            lda indata      ;Get the alpha data
            sta (xferbuf,Y)      ;Put it in the buffer
            jmp discard2        ;outta here!
        endif
        iny             ;If in Command mode Y will now = 0
        ifne                
			;**********************************************************
			; This is an improper state, something is wrong with gamma
			;**********************************************************
            lda indata      	;Read garbage data from alpha
            jmp discard2        ;Get out of here!
			;**********************************************************
			;**********************************************************
        endif
        txa             	;Here if in command mode....
        pha 
?extend jsr advqueue
        lda #o_commandnum
        sta gr_queue,Y				;Store current command in queue
        ldx indata
        cpx #o_commandnum			;Under the max command limit? (valid)
        ifcc
            lda t_exceptidx,X       ;See if this command is an EXCEPTION 
            bpl nmiex               ;yes!
            txa 
            sta gr_queue,Y          ;Put the command in the queue
			; lda t_extendidx,X		;See if this has an EXTENSION command
			; ifpl
				; ; If we are here, then there is an EXTENSION... reset stack
				; jsr advqueue
				; sta gr_queue,Y			;Book it
			; endif
        endif
discard pla                 ;General Cleaup before returning
        tax 
discard2    
        pla 
        tay 
        pla 
        rti 

advqueue
		ldy gr_ia           ;Increment the read queue actual 
        iny 
        cpy #qsize
        ifcs                ;Did it flip?
            ldy #00         ;yep, reset it.
        endif
        sty gr_ia           ;Store it
        cpy gr_i            ;Which queue are we at
        ifeq                ;Are the the same...
            ldx gr_i            ;Yes, make them different
            inx 
            cpx #qsize
            ifcs
                ldx #00
            endif
            stx gr_i            ;Make it so.
        endif
		rts

;EXCEPTION - Do something special and do it now        
nmiex   cmp #numexc         ;Is it a valid exception number         
        bcs discard         ;no, branch to discard command
        asl A               ;times 2 for word index
        tay 
        lda t_exceptptr+1,Y     ;Get the routine
        pha 
        lda t_exceptptr,Y
        pha 
        rts                 ;Do it now.
        
;******************************************************************************
;* Define the actual Sound Table Structures 
;******************************************************************************
t_userfunction  .block  numfcns*2       ;Words  
t_commandtype   .block  numcom
t_cmdparam      .block  numcom
t_tunestarts    .block  numsnd
#IF ___REP = 1
t_replacement   .block  numsnd
#ENDIF
#IF ___FLG = 1
t_eflag         .block  numtun
#ENDIF
t_priority      .block  numtun                                 
t_schan         .block  numtun      
t_stune         .block  numtun      
t_tunecont      .block  numtun
;t_driver		.block  numtun
t_immfunc       .block  numimm
t_immarg        .block  numimm
t_immsid        .block  numimm
t_immschn       .block  numimm
t_exceptidx     .block  numcom
t_tune_l        .block  numtun  
t_tune_h		.block  numtun
t_freqenv_l     .block  numenv   
t_freqenv_h     .block  numenv       
t_ampenv_l      .block  numenv     
t_ampenv_h      .block  numenv   
t_exceptptr     .block  numexc*2        ;Words    
t_mystery       .block  numexc*2        ;Words


;Speech Tables
#IF ___TLK == 1

t_spdptr        .block  numtlk*2        ;Words
t_spdlen        .block  numtlk*2        ;Words

#ENDIF

#IF ___OKI == 1

numoki 			= 127				;OKI can take up to 127 sounds (as long as the binary fits)

t_okicmd		.block  numoki
t_okiattch      .block  numoki        

#ENDIF


;********************************************************************
;* Default Sound Data:
;********************************************************************
;* These first two datapoints are empty frequency and amplitude
;* envelope data sets. These need to exist as sounds that do 
;* not explicity define envelopes, will load this data by default 
nullenvelope                     
NEWSNDFREQ(sfreq_none)                      
NEWSNDAMP(samp_none) 
    .byte $00,$00,$00,$00   
    
    
;NEWSNDFREQ(sfreq_d01)        
;    .byte $02,$00,$05,$02,$00,$00,$02,$00,$FE,$02,$00,$00,$02,$00,$FD,$00,$00,$00
    
;NEWSNDFREQ(sfreq_d02)    
;NEWSNDFREQ(sfreq_d03)    
;NEWSNDFREQ(sfreq_d04)    
;NEWSNDFREQ(sfreq_d05)  
  
NEWSNDFREQ(sfreq_d06)    
	SFREQ(256,1)             ;Eff Slope:16     Dur:1    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:16
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:-16
	SFREQ(256,1)             ;Eff Slope:16     Dur:1    Net:0
	SFREQ(0,0)               ;Eff Slope:0      Dur:0    Net:0
;********************************************************************                                                                
;                                                                    
;                                                                    
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
;     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 
; *  *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 

    
NEWSNDAMP(samp_d01) ;$01
   .byte $08,$04,$08,$FC,$10,$FE,$18,$FC,$FF,$00,$00,$00
;*********************************************************************************************************************************************************************
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                                                                                                                                                                     
;                          *                                                                                                                                          
;                    *  *  *  *  *                                                                                                                                    
;              *  *  *  *  *  *  *  *  *                                                                                                                              
;        *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                        
; 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F 20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E 2F 30 31 32 33 34 35 36

   
NEWSNDAMP(samp_d02) ;$02
   .byte $08,$0A,$50,$FF,$FF,$00,$00,$00
;*********************************************************************************************************************************************************************************************************************************************************************
;                                                                                                                                                                                                                                                                     
;                                                                                                                                                                                                                                                                     
;                                                                                                                                                                                                                                                                     
;                                                                                                                                                                                                                                                                     
;                                                                                                                                                                                                                                                                     
;                                                                                                                                                                                                                                                                     
;                          *                                                                                                                                                                                                                                          
;                          *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                  
;                       *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                          
;                    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                  
;                 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                          
;              *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                  
;              *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                          
;           *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                  
;        *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                          
;     *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                  
; 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F 20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E 2F 30 31 32 33 34 35 36 37 38 39 3A 3B 3C 3D 3E 3F 40 41 42 43 44 45 46 47 48 49 4A 4B 4C 4D 4E 4F 50 51 52 53 54 55 56

   
NEWSNDAMP(samp_d03) ;$03
   .byte $08,$00,$08,$01,$18,$02,$08,$01,$FF,$00,$00,$00
;*********************************************************************************************************************************************
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                                             
;                                                                                                                          *  *  *  *  *  *  *
;                                                                                                              *  *  *  *  *  *  *  *  *  *  *
;                                                                                                  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
;                                                                                      *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
;                                                                          *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
;                                                              *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
;                                                  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
; 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F 20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E

   
NEWSNDAMP(samp_d04) ;$04
   .byte 8,3
   .byte 8,-7
   .byte 8,5
   .byte 8,-4
   .byte 8,5
   .byte 8,-7
   .byte 8,7
   .byte 8,-5
   .byte 8,5
   .byte 8,-6
   .byte 8,5
   .byte 8,-4
   .byte 8,5
   .byte 8,-6
   .byte 8,5
   .byte 8,-6
   .byte 0,0
   
;************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                                                                                                                                                                                                                                                
;                                                                                                                                                                          *                                               *                                                                                               *                                                                     
;                                                                                                                          *                                            *  *  *                                         *  *  *                                            *                                            *  *  *                                            *                     
;                                                                          *                                            *  *  *                                      *  *  *  *  *  *                             *  *  *  *  *  *                                      *  *  *  *                                *  *  *  *  *  *                                      *  *  *                  
;                                                                       *  *  *  *                                *  *  *  *  *  *                                *  *  *  *  *  *  *  *                       *  *  *  *  *  *  *  *  *                          *  *  *  *  *  *  *  *                       *  *  *  *  *  *  *  *  *                          *  *  *  *  *  *               
;                          *                                      *  *  *  *  *  *  *  *                       *  *  *  *  *  *  *  *                          *  *  *  *  *  *  *  *  *  *  *           *  *  *  *  *  *  *  *  *  *  *  *                    *  *  *  *  *  *  *  *  *  *  *           *  *  *  *  *  *  *  *  *  *  *  *                    *  *  *  *  *  *  *  *  *         
;                    *  *  *  *                                *  *  *  *  *  *  *  *  *  *  *           *  *  *  *  *  *  *  *  *  *  *                    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *           *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *           *  *  *  *  *  *  *  *  *  *  *  *      
;           *  *  *  *  *  *  *  *                       *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *              *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   
; 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F 20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E 2F 30 31 32 33 34 35 36 37 38 39 3A 3B 3C 3D 3E 3F 40 41 42 43 44 45 46 47 48 49 4A 4B 4C 4D 4E 4F 50 51 52 53 54 55 56 57 58 59 5A 5B 5C 5D 5E 5F 60 61 62 63 64 65 66 67 68 69 6A 6B 6C 6D 6E 6F 70 71 72 73 74 75 76 77 78 79 7A 7B 7C 7D 7E 7F


;Used in attract mode for the hard swell drop off notes  
;NOTES ON AMP Data Stream here, Full Bit-Depth of AMP is only 7-bits, divided by 8 to give 4-Bit final resolution
NEWSNDAMP(samp_hardswell)   
    .byte $08,$0A	;Attack  - Addend of 10 x 8
	.byte $D0,$FF	;Decay	 - Addend of -1 x 208
	.byte $FF,$00	;Sustain - None
	.byte $00,$00	;Release
;************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
;                          *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
;                          *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
;                       *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
;                    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
;                 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
;              *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
;              *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
;           *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
;        *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                                             
;     *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                                                                                                                                                                                                                                                                     
; 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F 20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E 2F 30 31 32 33 34 35 36 37 38 39 3A 3B 3C 3D 3E 3F 40 41 42 43 44 45 46 47 48 49 4A 4B 4C 4D 4E 4F 50 51 52 53 54 55 56 57 58 59 5A 5B 5C 5D 5E 5F 60 61 62 63 64 65 66 67 68 69 6A 6B 6C 6D 6E 6F 70 71 72 73 74 75 76 77 78 79 7A 7B 7C 7D 7E 7F 80 81 82 83 84 85 86 87 88 89 8A 8B 8C 8D 8E 8F 90 91 92 93 94 95 96 97 98 99 9A 9B 9C 9D 9E 9F A0 A1 A2 A3 A4 A5 A6 A7 A8 A9 AA AB AC AD AE AF B0 B1 B2 B3 B4 B5 B6 B7 B8 B9 BA BB BC BD BE BF C0 C1 C2 C3 C4 C5 C6 C7 C8 C9 CA CB CC CD CE CF D0 D1 D2 D3 D4 D5 D6 D7

;Used in attract mode for the hard swell drop off notes  
;NOTES ON AMP Data Stream here, Full Bit-Depth of AMP is only 7-bits, divided by 8 to give 4-Bit final resolution
NEWSNDAMP(samp_hardswell2)   
    .byte 32,5	;Attack  - Addend of 10 x 8
	.byte -48,-1	;Decay	 - Addend of -32 x 208
	.byte -1,0	;Sustain - None
	.byte 0,0	;Release
	
NEWSNDAMP(samp_funk)   
    .byte 20,4	;Attack  - Addend of 10 x 8
	.byte -64,1	;Decay	 - Addend of -32 x 208
	.byte -2,8	;Sustain - None
	.byte 0,0	;Release
	
NEWSNDAMP(samp_funk2)   
    .byte 80,1		;Attack
	.byte -1,12		;Decay
	;.byte 0,12		;Sustain
	.byte -76,1		;Release
	.byte 0,0

NEWSNDAMP(samp_robot)   
    .byte 16,5	;Attack  - Addend of 16 x 5
	.byte -16,2	;Decay	 - Addend of -32 x 208
	.byte -4,12	;Sustain - None
	.byte 0,0	;Release
	.byte 0,0

;Used in attract mode for the horns
;NOTES ON AMP Data Stream here, Full Bit-Depth of AMP is only 7-bits, divided by 8 to give 4-Bit final resolution
NEWSNDAMP(samp_horn)   
    .byte $08,$0A	;Attack  - Addend of 10 x 8
	.byte $02,$FF	;Decay	 - Addend of -1 x 208
	.byte $FF,$00	;Sustain - None
	.byte $00,$00	;Release

;Not Used - Short Envelope  
NEWSNDAMP(samp_d05) ;$05
    .byte $08,$08,$20,$FE,$20,$00,$01,$80,$FF,$00,$00,$00,$00,$00
;************************************************************************************************************************************************************************************************************************
;                                                                                                                                                                                                                        
;                                                                                                                                                                                                                        
;                                                                                                                                                                                                                        
;                                                                                                                                                                                                                        
;                                                                                                                                                                                                                        
;                                                                                                                                                                                                                        
;                                                                                                                                                                                                                        
;                                                                                                                                                                                                                        
;                          *                                                                                                                                                                                             
;                       *  *  *  *  *  *                                                                                                                                                                                 
;                    *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                                     
;                 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                                         
;              *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                             
;           *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                                 
;        *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                                     
;     *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *                                                                                                         
; 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F 20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E 2F 30 31 32 33 34 35 36 37 38 39 3A 3B 3C 3D 3E 3F 40 41 42 43 44 45 46 47


;********************************************************************
;* 16-Bit Note Table!! Yeah!
;* For sounds using straight tunes, this table is a data word
;* table by byte index. First data point is index $01
;*
;* Havoc uses a 1.50Mhz POKEY Clock
;*  - Notes will be 16.2% slower/lower
;*  - This is 3 half-steps lower, so C becomes A
;********************************************************************
___noteidx	= 1
#DEFINE NOTEDEF(name,reg)		\_+name = ___noteidx
#DEFCONT						\___noteidx .set ___noteidx+1
#DEFCONT						\ .word reg


_REST 		= $00
;********************************************************************
;       Register		Index		Octave	Note	Frequency
notes   
		;PRE OCTAVE 0 NOTES - Due to main clock shift
		NOTEDEF(A_, $B260)	;0x01		-1		A  		13.75Hz
        NOTEDEF(Bb_,$A900)	;0x02		-1		A# 
        NOTEDEF(B_, $9E80)	;0x03 		-1		B 
		;OCTAVE 0
        NOTEDEF(C0, $9680)	;0x04		0		C
        NOTEDEF(Db0,$8F4D)	;0x05		0		C#
        NOTEDEF(D0, $8680)	;0x06		0		D  
        NOTEDEF(Eb0,$7F80)	;0x07		0		D#
        NOTEDEF(E0, $77C0)	;0x08		0		E		20.60Hz
        NOTEDEF(F0, $7180)	;0x09		0		F
        NOTEDEF(Gb0,$6B40)	;0x0a		0		F#
        NOTEDEF(G0, $6580)	;0x0b		0		G
        NOTEDEF(Ab0,$5F90)	;0x0c		0		G#
        NOTEDEF(A0, $5A20)	;0x0d		0		A		27.50Hz 
        NOTEDEF(Bb0,$5580)	;0x0e		0		A#
        NOTEDEF(B0, $5080)	;0x0f		0		B		30.87Hz
		;OCTAVE 1
        NOTEDEF(C1, $4C10)	;0x10		1		C		32.70Hz
        NOTEDEF(Db1,$47A0)	;0x11		1		C#	    34.652Hz
        NOTEDEF(D1, $43C0)	;0x12		1		D
        NOTEDEF(Eb1,$3FF0)	;0x13		1		D#
        NOTEDEF(E1, $3C56)	;0x14		1		E
        NOTEDEF(F1, $38F0)	;0x15		1		F
        NOTEDEF(Gb1,$35C0)	;0x16		1		F#
        NOTEDEF(G1, $32C0)	;0x17		1		G
        NOTEDEF(Ab1,$2FC0)	;0x18		1		G#
        NOTEDEF(A1, $2D28)	;0x19		1		A
        NOTEDEF(Bb1,$2AA0)	;0x1a		1		A#
        NOTEDEF(B1, $2844)	;0x1b		1		B
		;OCTAVE 2
        NOTEDEF(C2, $2600)	;0x1c		2		C
        NOTEDEF(Db2,$23D0)	;0x1d		2		C#
        NOTEDEF(D2, $21E0)	;0x1e		2		D
        NOTEDEF(Eb2,$1FF4)	;0x1f		2		D#
        NOTEDEF(E2, $1E2C)	;0x20		2		E  
        NOTEDEF(F2, $1C74)	;0x21		2		F
        NOTEDEF(Gb2,$1AD4)	;0x22		2		F#
        NOTEDEF(G2, $1958)	;0x23		2		G
        NOTEDEF(Ab2,$17EC)	;0x24		2		G#
        NOTEDEF(A2, $168A)	;0x25		2		A
        NOTEDEF(Bb2,$154E)	;0x26		2		A#
        NOTEDEF(B2, $1419)	;0x27		2		B		121.3
		;OCTAVE 3
        NOTEDEF(C3, $12FE)	;0x28		3		C
        NOTEDEF(Db3,$11E4)	;0x29		3		C#
		NOTEDEF(D3, $10ED)	;0x2a 		3		D
		NOTEDEF(Eb3,$0FF3)	;0x2b		3		D#
		NOTEDEF(E3, $0F0C)	;0x2c		3		E
		NOTEDEF(F3, $0E3B)	;0x2d		3		F
		NOTEDEF(Gb3,$0D6C)	;0x2e		3		F#
		NOTEDEF(G3, $0CAA)	;0x2f		3		G
		NOTEDEF(Ab3,$0BF3)	;0x30		3		G#
        NOTEDEF(A3, $0B60)	;0x31		3		A
		NOTEDEF(Bb3,$0AC0)	;0x32		3		A#
		NOTEDEF(B3, $0A18)	;0x33		3		B
		;OCTAVE 4
		NOTEDEF(C4, $0990)	;0x34		4		C
		NOTEDEF(Db4,$0904)	;0x35		4		C#
		NOTEDEF(D4, $0880)	;0x36		4		D
		NOTEDEF(Eb4,$0804)	;0x37		4		D#
		NOTEDEF(E4, $0792)	;0x38		4		E
        NOTEDEF(F4, $0720)	;0x39		4		F
		NOTEDEF(Gb4,$06C0)	;0x3a		4		F#
		NOTEDEF(G4, $0658)	;0x3b		4		G
		NOTEDEF(Ab4,$05F8)	;0x3c		4		G#
		NOTEDEF(A4, $05A4)	;0x3d		4		A
		NOTEDEF(Bb4,$0554)	;0x3e		4		A#
		NOTEDEF(B4, $0502)	;0x3f		4		B
		;OCTAVE 5
		NOTEDEF(C5, $04BC)	;0x40		5		C
        NOTEDEF(Db5,$047C)	;0x41		5		C#
		NOTEDEF(D5, $043C)	;0x42		5		D
		NOTEDEF(Eb5,$0400)	;0x43		5		D#
		NOTEDEF(E5, $03C4)	;0x44		5		E
		NOTEDEF(F5, $0390)	;0x45		5		F
		NOTEDEF(Gb5,$035E)	;0x46		5		F#
		NOTEDEF(G5, $032C)	;0x47		5		G
		NOTEDEF(Ab5,$02FE)	;0x48		5		G#
        NOTEDEF(A5, $02D0)	;0x49		5		A
		NOTEDEF(Bb5,$02A8)	;0x4a		5		A#
		NOTEDEF(B5, $027E)	;0x4b		5		B
		;OCTAVE 6
		NOTEDEF(C6, $025A)  ;0x4c		6		C
		NOTEDEF(Db6,$0237)	;0x4d		6		C#
		NOTEDEF(D6, $0218)	;0x4e		6		D
		NOTEDEF(Eb6,$01F9)	;0x4f		6		D#
		NOTEDEF(E6, $01DC)	;0x50		6		E
        NOTEDEF(F6, $01C2)	;0x51		6		F
		NOTEDEF(Gb6,$01A8)	;0x52		6		F#
		NOTEDEF(G6, $0190)	;0x53		6		G
		NOTEDEF(Ab6,$0179)	;0x54		6		G#
		NOTEDEF(A6, $0163)	;0x55		6		A
		NOTEDEF(Bb6,$014F)	;0x56		6		A#
		NOTEDEF(B6, $013B)	;0x57		6		B
		;OCTAVE 7
		NOTEDEF(C7, $0129)	;0x58		7		C
        NOTEDEF(Db7,$0118)	;0x59		7		C#
		NOTEDEF(D7, $0107)	;0x5a		7		D
		NOTEDEF(Eb7,$00F7)	;0x5b		7		D#
		NOTEDEF(E7, $00E8)	;0x5c		7		E
		NOTEDEF(F7, $00DA)	;0x5d		7		F
		NOTEDEF(Gb7,$00CC)	;0x5e		7		F#
		NOTEDEF(G7, $00C4)	;0x5f		7		G
		NOTEDEF(Ab7,$00BD)	;0x60		7		G#
        NOTEDEF(A7, $00B7)	;0x61		7		A

        
___RPMROMEND = * 
        
