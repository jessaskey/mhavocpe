;***************************************************************
;* Major Havoc MainLine Program (Gamma Processor)              
;***************************************************************
    .title "Major Havoc GAMMA Processor"               
;*************************************************************** 
#locallabelchar "?"
#include "logic.ah"
#include "macros.ah"
#include "havoc.ah"

;***************************************************************
;**	RPM GAME ASSEMBLY OPTIONS: 
;***************************************************************
;see RPMDOC.DOC for explanations of all options.

___CHNS     = $10		;NUMBER OF LOGICAL CHANNELS
___SLV      = 1		;STAND-ALONE SLAVE PROCESSOR
;___TST      = 0		;SELF-TEST MODE___ (0 = OPERATIONAL MODE)
___NMI      = 1		;NMI on port latch (0 = none)
___EXM      = 1		;EXCEPTION-MODE processing (0= none)
___FRE      = 1		;FREQ. ENVELOPES ENABLE (0=DISABLE)
___AME      = 1		;AMPLITUDE ENVELOPE ENABLE (0=DISABLE)
___RLE      = 1		;RUN-LENGTH ENCODED TABLES (0=ORIGINAL STYLE TABLES)
___MUS      = 1		;INCLUDE MUSIC ENVELOPES (0=NOT INCLUDED)
___SYM      = 1		;SYNTHISIZER MODE ENABLE (0=DISABLE)
___SCK      = 1		;SYNC. CHECK ENABLE (0=DISABLE)
___CNT      = 0		;NOTE COUNTER ENABLE (0=DISABLE)
___SVV      = 1		;ALLOW 8-BIT CHANNELS (0=DISABLE)
___FLG      = 1		;SOUND FLAGS
___REP      = 1     ;REPLACEMENTS ALLOWED (0=ALWAYS REPLACE)
___CTL      = 0		;ALLOW AUTO-CONFIGURATION OF SKCTL (0=DISABLE)
___IMM      = 0		;ALLOW IMMEDIATE-MODE FUNCTIONS (0=DISABLE)
___PIV      = 1		;POKEY CONTROL REG SPLIT (0=standard mem.map)
___SST      = 0		;SYSTEM START ROUTINE R$SYSST ( 0= none)
___RST      = 0		;SYSTEM RESTART ROUTINE R$SYSRST ( 0= none)
___SML      = 0		;SYSTEM MAINLINE ROUTINE R$SYSML ( 0= none)
___SIR      = 0		;SYSTEM IRQ ROUTINE R$SYSIRQ ( 0= none)
___SDI      = 0		;SYSTEM DEFERRED IRQ R$SYSDIRQ ( 0= none )
___OPT      = 0		;INHIBITS AUTO-LOCATION - DECLARE LOC. OF PSECT VARS & CODE
___FIXPKY   = 1	    ; ** FIX POKEY ADDRESSING ERROR - MHAVOC only? **

;Carry through main config into here for speech coding
#IF SPEECH = 0
___OKI .equ 1   ;OKI MSM6295 ADPCM 
___TLK .equ 0   ;TMS 5220 SPEECH
#ELSE
___OKI .equ 0   ;OKI MSM6295 ADPCM 
___TLK .equ 1   ;TMS 5220 SPEECH
#ENDIF    
           
#IF ___TLK != 0 & ___OKI != 0 
	.error "Both ___TLK and ___OKI appear to be enabled, only 1 speech option can be enabled at a time when compiling"
#ENDIF	

;************************************************************
;* Declare all tables large enough. The macros will fill in
;* the entries....
;*
;* ***** Table Limits *****
;* 
;* Byte Tables: these are addressed using Y as an index, so 
;*              they are limited to 256d entries. Defining
;*              them smaller saves space tho.
;************************************************************

numcom  	=   $B0;$89		;$B3     ;Total Number of legal commands (need to include oki commands in here)
numsnd  	=   $90     ;MAX number of sound groups - original value in mhavoc was $60, but this was insufficient because we actually had $67... not sure how this didn't cause issues actualy
numtun  	=   $90     ;MAX number of tunes to group
numimm  	=   $0      ;MAX number of immediates
numtlk  	=   $29     ;MAX number of speech samples
numenv  	=   $90     ;MAX number of envelopes (freq and amplitude) - this was $68 originally

;numextend   =   8		;MAX Extensions

;* Word Tables: these are limited to 128d entries for the 
;*              same reason.

numexc      =   36d     ;MAX Exceptions, even tho many commands outside of EXCEPT can have an
						;exception pointer, this limits our table size (words) to just the amount needed
numfcns     =   2       ;Number of legal user functions
;************************************************************
;* This is the end of the User Configuration Section        
;************************************************************

;****************************************************************
;*   Variables and Hardware Allocation                     
;****************************************************************
g_numhs		= 	10	;Number of shown high scores and initials
g_savecnt   =   3   ;Number of High Scores and Initials Saved
                    ;This is used later for Memory allocation

g_tout      =   $02         ;Signals that Gamma has timed out
g_done      =   $03         ;Signals that Gamma is done sending stuff   

qsize       =   $80     ;Max size of the roundabout command queue
ticmdbflen  =   $08     ;Max size of of the speech command queue

counter_r    =   1       ;Byte Location of Coin Counter 1 (right)
counter_l    =   2       ;Byte Location of Coin Counter 2 (left)

; EEROM Flags
EE_ERASE    =   $80		
EE_WRITE    =   $40
EE_READ     =   $20
EE_DONE		= 	$00

;Inputs
qpokey  =   $2000
portst  =   $2800
input   =   $2800
indata  =   $3000
leta    =   $3800

;Outputs
intack  =   $4000
counter =   $4800
outdata =   $5000

___RPMMEMST = $0200
#include "rpm_def.asm"

;************************************************************
;* Normally the WS signal of the TMS5220 is set via two
;* different address locations (look at the Return of the 
;* Jedi schematics for example). One sets it high and the
;* other clears it. However, in MH, since the speech was
;* not implemented completely, the WS signal is cleared 
;* with every data write, then the code will set it directly
;* afterwards, so tiwrite is meaningless in this case.
;* What we will do is set it to an unused address, which 
;* will allow RPM to be set up as normal, but will not 
;* actually do anything.
;************************************************************
;Speech
tirdy  		=   $2800
tidata      =   $5800
tiwrite     =   $5900		;TI /WS is on a flip flop, must be turned off after by writing to tidata
;tiwson      =   $5800
tireset		= 	$8000 		;NOT WIRED IN HAVOC JUST WRITE TO ROM - DOES NOTHING

;EEROM
eerom       =   $6020		;This is moved up 32 bytes so that we don't overwrite any existing MHAVOC data


;*****************************************************************
;* Game RAM ZP Allocation                                                
;*****************************************************************
temp1       .block  2
temp2       .block  2       ;A few temps
temp3       .block  2       
eerdy       .block  1       ;=80 if stats ready to send
eesrce      .block  2       ;Next byte source pointer
eedest      .block  2       ;Next byte dest(EEROM) pointer
eezflg      .block  1       ;Zero flag for erase
eerequ      .block  1       ;Action request
eerwrq      .block  1       ;Write request
eerzrq      .block  1       ;Zero request
eebad       .block  1       ;If a bit is set, that corresponding request came back bad (csum error)
eeflg       .block  1       ;Busy Flag
eebc        .block  1       ;Byte Index to source data
eeidx       .block  1       ;Byte offset into eerom for next access
eelen       .block  1       ;Total Length of Data Chunk
eesel       .block  1       ;Index code for current 'batch' operation
eecsum      .block  1       ;Checksum (operation)
kilscores   .block  1       ;Request to clobber scores
reqwai      .block  1       ;How long to wait until we re-init the eerom
cnt_r       .block  1       ;Coin Counter Right
cnt_l       .block  1       ;Duh?
;maxsnd		.block 	1		;If not zero, this is the base freq of the Max background sound
;maxcounter	.block	1		;Counter to keep max logic from clobbering itself

    .org $04D6
    orgchk($04D6,$)
;********************************************************************
;* Non ZP Game Variables
;********************************************************************
gw_queue        .block  qsize   
qhead           .block  1
qtail           .block  1

qflag           .block  1
sindex          .block  1

gr_queue        .block  qsize
gr_i            .block  1
gr_ia           .block  1
r_nptr          .block  1
datnum          .block  1
datflg          .block  1
curctrl			.block  1			;Which player do we read controls for? 00 = player 1, 01 = player 2

; Variables used for immediate mode only
#IF ___IMM == 1
imm_id          .block  1
iarg            .block  1
#ENDIF
;*****************************************************************
; RAM Allocation for NVRAM Buffer
;*****************************************************************
	orgchk($0600,$)

;How Many Entries in the High Score Table saved in EEROM
numsths     =   g_savecnt*4     ;8 Digits/4 Bytes per score
numstit     =   g_savecnt*3     ;3 Initials per score
numtessers	=	g_savecnt*1	    ;1 Byte per icons
numwarps    =  	7			    ;Number of Warps to save
numstmz     = 	8*25            ;Maze Stats - NOT USED!
numstop     =   2			    ;Number of Option switch settings
numstxl		=   2			    ;Bytes to save Xlife data

;***************************************
;RAM Address Locations
;***************************************
starts
;-- Options
options     .block  numstop     
cksumop     .block  1 
;-- Lives/Warps Stats
extlie      .block  numstxl     ;Extra Lives Earned 
warps       .block  2*numwarps  ;Number of warps done - 2 bytes per warp
numstxw      = * - extlie       ;Size of XLives+Warp Block
cksumxw     .block  1           ;Checksum for XLives+Warps
;-- Game Stats
game1       .block  4           ;1 Player Games
game2       .block  4           ;2 Player Games
atime1      .block  4           ;Accum time for 1 Player Games
atime2      .block  4           ;Accum time for 2 Player Games
numstgs     = * - game1         ;Size of Game Stats Block
cksumgs     .block  1           ;Checksum for Game Stats
;-- Maze Stats
;Maze Stats hold 4 parameters per maze
;spdt        .block  2       ;Deaths in Space
;lddt        .block  2       ;Deaths in Landing
;mzdt        .block  2       ;Deaths in Maze
;mzd2        .block  2       ;Maze Deaths after Reactor
mzstats     .block  numstmz
cksummz     .block  1       ;Checksum for above
;-- Warps
warpcodes   .block  numwarps*2 ;Defined Warp Number Data
cksumwc     .block  1         ;Checksum for Warp Numbers
;-- Initials
initl       .block  numstit	;Initials
cksumit     .block  1		
;-- High Scores
hscore      .block  numsths	;High Scores
cksumhs     .block  1
;-- Tessers
tessers		.block 	numtessers	;Tesseract data per score
cksumts		.block 1


;This is a little more complicated too
;When Game, Warp and Maze Stats come back, they come back with deltas, 
;not totals with existing data. So we have to add the new data to the 
;existing data, we need a temp place to store the deltas so that after 
;they are all loaded, we can decimal add them into the existing GAMMA data. 
;This can be used by all of them individually, but the Xlife+Warp data
;happens to be the largest, so we allocate that much
stats_temp  .block numstxw

orgchk($07FF,$)         ;Makes sure we are still in RAM

;Define some variables that Alpha needs 
g_numstop   =   numstop  
g_numstxl   =   numstxl
g_numstwx   =   numstxw
g_numstgs   =   numstgs
g_numstmz   =   numstmz
g_numstit   =   numstit
g_numsths   =   numsths
g_numwarps  =   numwarps
g_numtessr	= 	numtessers

;Export these for Alpha
;.export g_idxstwx,g_idxstxl,g_idxstgs,g_idxstmz,g_idxstit,g_idxsths,g_idxstop
.export g_numstwx,g_numstxl,g_numstgs,g_numstmz,g_numstit,g_numsths,g_numstop,g_numwarps,g_numstxl,g_numtessr

;*****************************************************************
; RAM Allocation for Self Testing
;*****************************************************************
    .org $0780
    
vram        .block  6   ;$0700
vadl0       .block  1   ;$0706
vadh0       .block  18  ;$0707
vadl1       .block  1   ;$0719
vadh1       .block  10  ;$071a
vadl2       .block  1   ;$0724
vadh2       .block  2   ;$0725
vadl3       .block  1   ;$0727
vadh3       .block  1   ;$0728

    .org    $07f0
    
tstart      .block  2
tend        .block  2
rambad      .block  1
rombad      .block  1
pokbad      .block  1
eerbad      .block  1


;******************************************************************
;* Special Variables for Scott's FPGA Implementation
;******************************************************************
	.org $07FF
fpgaspeech	.block 	1
;********************************************************************
;* Game Variables Finished
;********************************************************************

;***********************************************************
; Include RPM                                               
;***********************************************************
#include "rpm_mem.asm"
#include "tw_gamdef.asm"
;***********************************************************
    .title "TWGamma"
    .sbttl "MainLine"
    .org $8000
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_GG
	.byte MAJOR_VERSION,MINOR_VERSION

;**************************************************************
    .TEXT ".TWGAMMA."
;**************************************************************
g_main	sei 
        cld 
        ldx #$FF            ;Set Stack Pointer 
        txs 
		lda #00
        sta outdata     	;Ack start but not ready  
        jsr g_stest         ;Test the Gamma system
		;returns to cindy below with stack pointing to cindy as well in case of stack overflows
cindy   pha                 ;Save the result.
        ldx #-1
        stx datnum          ;Set NMI to command mode
		stx fpgaspeech		;For Scott's FPGA
		;stx maxsnd			;Normally at -1 for no max sound
        inx 
        stx framecnt
        stx irqcnt
        stx silevel
        stx qhead           ;Reset the Alpha Write Queues
        stx qtail
        stx gr_i            ;Reset the Alpha Read Queues
        stx gr_ia
		stx curctrl			;Set to player 1 controls by default
        lda indata          ;Read a garbage byte from Alpha just in case it is waiting
        pla                 ;Get Self Test result from above
        sta outdata         ;Send it to the Alpha (Should be -1 if all okay)
c_reset jsr initrpm         ;Initialize all RAM etc.
		;TODO Remove this again? nop
        ;ldx #0				;Only set warps that are at -1
		;jsr genwarpsx		;Generate random warp codes if undefined
        ;
        cli      			;Enable commands from Alpha
		;************ Begin Main Process Loop ***********************
		begin
			lda #alpharcvd
			bit portst          ;Alpha Ready?
			ifne                ;yes
				ldy qhead
				cpy qtail           ;Check for anything in queue
				ifne                ;If not, then skip writing
					lda gw_queue,Y      ;Get the next command
					sta outdata         ;Write it!
					iny 
					cpy #qsize          ;buffer queue top
					ifcs
						ldy #00         ;Loop the queue
					endif
					sty qhead            ;Store it
				endif
			endif
			;***************************************************
			;* Process the Alpha Command Queue now
			;***************************************************
			ldx gr_i
			cpx gr_ia           ;Check the read queue
			ifne                ;Something came in!
				inx 
				cpx #qsize
				ifcs
					ldx #00         ;Reset the queue
				endif               
				stx gr_i            ;Save new queue location
				ldy gr_queue,X
				jsr cmdpro           ;Go to it!
			endif
			;************************
			;Other main loop stuff
			;************************
			jsr framecntl       ;Update the Coin Counters   
			;jsr domaxsound
		doloop

;*****************************************************
; chkwrps lda eebad		;only if EEROM has passed
		; ifeq
			; lda # lbyte(warpcodes)
			; sta eesrce
			; lda # ubyte(warpcodes)
			; sta eesrce+1
			; ldy #cksumwc-warpcodes-1		;skip csum byte at top
			; lda #0		;start with zero
			; begin
				; ora (eesrce,Y) 
                ; ;put this back after RPM fixes nop
                ; bne ?not0
				; sta temp2
				; dey
			; miend
			; lda temp2
			; ifeq
				; ;for some reason warps are all zeros, reset them all
				; jsr warpclass		;reset warps to classic, flag to write back to EEROM
			; endif
		; endif
; ?not0   rts
		
;*****************************************************
        .sbttl "Coin Counter Update"
;*****************************************************
framecntl   
        lda #00             ;Guess Off
        ldy cnt_r           ;Mech On??
        ifmi
            lda #counter_r       ;Turn on this one
        endif
        ldy cnt_l
        ifmi
            ora #counter_l
        endif
        sta counter         ;Output Pulse
        rts 			

.org $812F  ;TEMP
#include "rpm_mac.asm"
		
;******************************************************
        .sbttl "EEROM Applications"
;******************************************************
;* If bit is set, that section is either read or      
;* written depending of request byte in                   
;*      Bit 0 = 00000001 = High Scores   
;*      Bit 1 = 00000010 = Initials                         
;*      Bit 2 = 00000100 = XLives+Warps Stats
;*      Bit 3 = 00001000 = Game Stats 
;*      Bit 4 = 00010000 = Maze Stats - Not currently used           
;*      Bit 5 = 00100000 = Option Switches 
;*      Bit 6 = 01000000 = Defined Warp Data  
;*      Bit 7 = 10000000 = Tesseract Data 
;******************************************************
eflg_scores     = $01 
eflg_initials   = $02  
eflg_xwarp      = $04
eflg_game       = $08   
eflg_maze       = $10 
eflg_option     = $20   
eflg_warpco     = $40     
eflg_tesser     = $80          

;D5
;11010101
;10101011  
;******************************************************
; teax is the table for length of data chunk
;******************************************************       
teax    .db cksumhs-hscore          ;Scores
        .db cksumit-initl           ;Initials  
        .db cksumxw-extlie          ;XLives+Warps
        .db cksumgs-game1           ;Game Stats
        .db cksummz-mzstats         ;Maze Stats - Not Used
        .db cksumop-options         ;Options 
        .db cksumwc-warpcodes       ;Warp Codes 
		.db cksumts-tessers	        ;Tesseracts

;RAM Buffer Location for the EEROM data tranfer
teasrl  .dw hscore                  ;Scores
        .dw initl                   ;Initials
        .dw extlie                  ;XLives+Warps
        .dw game1                   ;Game Stats
        .dw mzstats                 ;Maze Stats - Not Used
        .dw options                 ;Options 
        .dw warpcodes               ;Warp Codes 
		.dw tessers	                ;Tesseracts
        
;EEROM Locations for the data transfer
teadel  .dw hscore-starts+eerom     ;Scores
        .dw initl-starts+eerom      ;Initials
        .dw extlie-starts+eerom     ;XLives+Warps
        .dw game1-starts+eerom      ;Game Stats
        .dw mzstats-starts+eerom    ;Maze Stats - Not Used
        .dw options-starts+eerom    ;Option 
        .dw warpcodes-starts+eerom  ;Warp Codes
		.dw tessers-starts+eerom	;Tesseracts

;teasrh  = teasrl+1

;Clear All bookeeping         
eezbook lda #(eflg_xwarp+eflg_game+eflg_maze)       
        bne ezero

;Write Scores Only
wrhs    lda #eflg_scores 
        bne ewrit
        
; ;Write Initials Only
wrini   lda #eflg_initials 
        bne ewrit

; ;Write Tesseracts Only		
wrtes	lda #eflg_tesser
		bne ewrit
        
;Re-write Initials, Scores and Tessers 
wrhsini lda #eflg_initials+eflg_scores+eflg_tesser
        bne ewrit
        
;Re-write Game Stats        
wrstg   lda #eflg_game       
        bne ewrit

;Re-write Xlives+Warp Stats           
wrxwarp lda #eflg_xwarp
        bne ewrit

;Re-write All Stats 
wrbook  lda #(eflg_xwarp+eflg_game+eflg_maze)               
        bne ewrit       
        
wrmzst  lda #(eflg_maze)
        bne ewrit

;Re-Write all Warp Codes        
wrwarpc lda #eflg_warpco
        bne ewrit

;Re-write Option switches        
wropt   lda #eflg_option       
		bne ewrit
        
ezero   pha
        ora eerzrq      ;Save zero flags  
        sta eerzrq
        pla             ;for below, only put flags in A
        ;fall through
ewrit   ldy #EE_DONE         
        sty eeflg       ;clear flag so next can be init
        pha 
        ora eerequ      ;Put in request section flags
        sta eerequ
        pla 
        ora eerwrq      ;Put in write bytes also
        sta eerwrq
        rts 

;Read in everything        
reinall lda #(eflg_scores+eflg_initials+eflg_xwarp+eflg_game+eflg_maze+eflg_option+eflg_warpco+eflg_tesser)        
        sta eerequ      ;Save request flag
        lda #00
        sta eerwrq      ;Clear write requests
        sta eerzrq      ;Clear zero requests
        sta eeflg       ;Set to Init
		;Fall through to handle requests 
		;**********************************************************************
        .sbttl "EEROM I/O Mainline"
		;**********************************************************************
		;* Input:  eeflg:  00=done, 80=zero, 40=write, 20=read         
		;*         eeidx:  Index into EEROM to access this data                
		;*         eebc:   Offset from eesrce of RAM data to access            
		;*         eexend: Bytes to read for this chunk, done when eecnt >= cclen
		;*                                                                    
		;* Output:  EEROM erased, written or read                             
		;**********************************************************************		
eeupd   lda eeflg
        ifeq                ;Not active
            lda eerequ      ;Any requests??
            ifne   
                jsr eesetup
            endif
        endif
        ;Now do it...
		lda eeflg           ;Anything to do??
        ifne		
			ldy eeidx           ;Get starting index
			asl A				;Flags
			ifcs                ;R, W or Erase?
				;Write/Zero Request
				;sty eeflg           ;Signal busy with write, will clear on next pass
                ;lda #0
                ;sta eeflg
                ;ldy #EE_WRITE
				;sty eezflg			;Flag to zero 
                lda #0
                sta (eesrce,Y)
                jmp ?eewrt
			else                ;Read or Write
				ifmi                ;Write values
					;lda eezflg      ;Was this an erase request?
					;ifne                ;yep
					;	lda #00
					;	sta (eesrce,Y)      ;Clear source too
					;endif
?eewrt				lda (eesrce,Y)      ;Get data to write
					;compare eecnt agains eelen, but keep A,X and Y intact
                    ;jsr compee
                    cpy eelen  
					ifcs                ;yep, done with data, do csum
						lda eecsum          ;Get checksum
						eor #$FF			;We are going to force an extra EOR inversion here so that all values of 00 will result in a CSUM of $FF
						;sta (eesrce,Y)      ;Save the csum to RAM too - NOT NEEDED REALLY?
                        sta (eedest,Y)      ;Write to chip
						lda #EE_DONE
						sta eeflg           ;We are done
						;sta eezflg
						;ldy #01              ;Do only 1 pass at a time for writes
					else 
						;we need Indirect Indexed here put X index into Y
						;ldy eeidx
						sta (eedest,Y)      ;Write to chip
						;ldy #01             ;Signal More to do
					endif
                    ldy #01              ;Do only 1 pass at a time for writes
				else                ;Must be a read
					;Note: Can read all at once, no need to wait
					;ldy eeidx
					lda (eedest,Y)          ;Get data
					;cpx eexend             ;Got all?
					;compare eecnt agains eelen, but keep A,X and Y intact
					;jsr compee
                    cpy eelen  
					ifcs                ;yes
						eor #$FF			;We are going to force an extra EOR inversion here so that all values of 00 will result in a CSUM of $FF
						eor eecsum            ;Match checksum??
						ifne
							lda eesel
							ora eebad
							sta eebad
						endif
						lda #EE_DONE 
						sta eeflg           ;This task is done, A is zero for below too
					else
						sta (eesrce,Y)      ;Copy to RAM
					endif
                    ldy #00         ;Flag to always continue to next (loop on reads)
				endif
				clc 
				adc eecsum          ;Update Checksum
				sta eecsum
				inc eeidx
			endif
			tya
			beq eeupd           ;Loop if more (READS ONLY)
		endif
		rts 

;*************************************************
; Start command for EEROM
;*************************************************
eesetup ldx #00
        ;stx eebc        ;Source index
        stx eeidx       ;Added from teax below
        stx eecsum      ;Checksum initial value 
        stx eesel       ;Select code storage
        ldx #08
        sec             ;Set the bit flag to rotate in
        begin           ;Loop untill first bit is found
            ror eesel       ;Move bit about
            asl A
            dex 
        csend           ;Exit when set bit is found
        ldy #EE_WRITE   ;Guess Write
        lda eesel
        and eerzrq      ;Any zeros requested?
        ifne
            ldy #EE_ERASE
        else 
            lda eesel       ;What selected, this is a single bit
            and eerwrq      ;Any writes requested??
            ifeq                ;no, must be read
                ldy #EE_READ
            endif
        endif
        sty eeflg       ;Save request
        lda eesel
        eor eerequ
        sta eerequ      ;Turn off read request bit (ack)
        lda eesel       ;Also clear any write flags
        eor #$FF        ;Inverse XOR and Mask
        and eerwrq
        sta eerwrq      ;Turn off write request bit (ack)
        lda eesel       ;Also clear any zero flags
        eor #$FF        ;Inverse XOR and Mask
        and eerzrq
        sta eerzrq      ;Turn off write request bit (ack)
        lda teax,X
        sta eelen       ;Save Length of this chunk (X is times 1 for byte table)
        txa 
        asl A
        tax             ;X is x2 now for word tables
        lda teasrl,X
        sta eesrce      ;Source pointer
        lda teasrl+1,X
        sta eesrce+1
        ;set up destination in EEROM now
        lda teadel,X
        sta eedest
        lda teadel+1,X 
        sta eedest+1
		rts

; compee  pha
        ; lda eeidx
        ; cmp eelen    
        ; pla     ;PLA does not affect the C flag, so we are okay
        ; rts
		
;*****************************************************
        .sbttl "Game IRQ"
;*****************************************************
;* This routine is called from the main IRQ for RPM, *
;* it takes care of game specific tasks outside the  *
;* functionality of RPM itself.                      *
;*                                                   *
;* Inputs: A = Frame Counter                         *
;*****************************************************  
game_irq    
        pha             ;Save counter for later too 
        and #3         
        ifeq
            lda eerdy
            ifmi
                jsr eeupd           ;Update EEROM every 4 frames
            endif
        endif
        pla             ;Get Original counter
        lsr A
        ifcc                ;Every Other Time, do the coin counters
            ldy #00         ;Say counters off
            ldx #01         ;Do both counters
            begin
                lda cnt_r,X      ;Counter On??
                ifne
                    cmp #$10
                    ifcs
                        adc #$EF        ;Sub 4 (carry set)
                        iny         ;Got one on
                        sta cnt_r,X
                    endif
                endif
                dex
            miend
            tya         ;Any on?
            ifeq
                ;None on, see if we can start any
                ldx #01
                begin
                    lda cnt_r,X
                    ifne
                        clc 
                        adc #$EF            ;Set 4 MSB, dec 4 LSB
                        sta cnt_r,X      
                        bmi ?si10           ;Exit so only 1 start
                    endif               
                    dex 
                miend
            endif
        endif
?si10   rts
		
;*********************************************
        .sbttl "Utility Routines"
;*********************************************
neg     eor #$FF
        clc
        adc #01
        rts            

;*********************************************
;* Coin Counter Routines 
;*********************************************        
coinr   inc cnt_r
        jmp exitnr
		
coinl   inc cnt_l
        jmp exitnr
        
;********************************************
;* Exception command wrappers
;********************************************
reseths jsr ldhsin
        jmp exitnr
        
resetbk jsr eezbook      ;Clear Bookkeeping
        jmp exitnr
		
resetop jsr loadop
		jmp exitnr

;Reset warp codes to classics
reswarps
		jsr warpclass
		jmp exitnr
		
warpclass
		ldx #(numwarps*2)-1
		begin
			lda classwarp,X
			sta warpcodes,X
			dex
		miend
        jsr wrwarpc     ;Write reset warp codes to EEROM 
		rts

;Generate random warp codes		
genwarps
		jsr genwarpsx
		jmp exitnr

;---------------------------------------
;Generate new warp numbers here... 
;---------------------------------------
genwarpsx
        ldx #(numwarps*2)-2
        begin
            jsr genwarp
            dex			;by twos
            dex
        miend
		jmp wrwarpc     ;Write reset warp codes to EEROM - none defined
    
; ;**************************************************************	
; ;Alpha will send a byte here, it can range from $00 to $FF
; ;and will set the value for the continuous Max sound.
; ;A routine in the main loop will actually queue the sound up
; ;as needed.
; ;**************************************************************
; sendmaxsnd
		; ldx #(maxsnd/256d) ;MSB of Buffer
        ; ldy #(maxsnd&$ff)  ;LSB of Buffer
		; lda #1				;1 byte to receive
		; jsr xfer
		; lda #1				;Send back 1 for Success!
		; jmp exitr		

;****************************************************
; Player Controls toggle - will set the current 
; controls being read between inputs for Player 1 or
; Player 2 which need to be switched if the game is
; in cocktail mode.
;****************************************************
setctrlp1
		lda #0
		sta curctrl
		jmp exitnr
		
setctrlp2
		lda #1
		sta curctrl
		jmp exitnr
;****************************************************
; Option Switches are loaded from DIP and then stored
; in EEROM, since they can be modified from Self Test
; we will always return the RAM/Settings value.
;
; The only way to apply the settings on DIP is to 
; do a reset in the Self Test.
;****************************************************        
;*
;*    MAJOR HAVOC DIP SWITCH SETTINGS -  $=Default
;*
;*    DIP Switch at position 13/14S
;*                                      1    2    3    4    5    6    7    8
;*    STARTING LIVES                  _________________________________________
;*     Free Play   1 Coin   2 Coin    |    |    |    |    |    |    |    |    |
;*        2         3         5      $|Off |Off |    |    |    |    |    |    |
;*        3         4         4       | On | On |    |    |    |    |    |    |
;*        4         5         6       | On |Off |    |    |    |    |    |    |
;*        5         6         7       |Off | On |    |    |    |    |    |    |
;*    GAME DIFFICULTY                 |    |    |    |    |    |    |    |    |
;*     Hard                           |    |    | On | On |    |    |    |    |
;*     Medium                        $|    |    |Off |Off |    |    |    |    |
;*     Easy                           |    |    |Off | On |    |    |    |    |
;*     Demo                           |    |    | On |Off |    |    |    |    |
;*    BONUS LIFE                      |    |    |    |    |    |    |    |    |
;*     50,000                         |    |    |    |    | On | On |    |    |
;*     100,000                       $|    |    |    |    |Off |Off |    |    |
;*     200,000                        |    |    |    |    |Off | On |    |    |
;*     No Bonus Life                  |    |    |    |    | On |Off |    |    |
;*    ATTRACT MODE SOUND              |    |    |    |    |    |    |    |    |
;*     Silence                        |    |    |    |    |    |    | On |    |
;*     Sound                         $|    |    |    |    |    |    |Off |    |
;*    ROLLER MULTIPLIER               |    |    |    |    |    |    |    |    |
;*     2X                             |    |    |    |    |    |    |    | On |
;*     1X                            $|    |    |    |    |    |    |    |Off |
;*                                    -----------------------------------------
;*
;*    DIP Switch at position 8S
;*                                      1    2    3    4    5    6    7    8
;*                                    _________________________________________
;*    Free Play                       |    |    |    |    |    |    | On |Off |
;*     1 Coin for 1 Game             $|    |    |    |    |    |    |Off |Off |
;*     1 Coin for 2 Games             |    |    |    |    |    |    | On | On |
;*     2 Coins for 1 Game             |    |    |    |    |    |    |Off | On |
;*    RIGHT COIN MECHANISM            |    |    |    |    |    |    |    |    |
;*     x1                            $|    |    |    |    |Off |Off |    |    |
;*     x4                             |    |    |    |    |Off | On |    |    |
;*     x5                             |    |    |    |    | On |Off |    |    |
;*     x6                             |    |    |    |    | On | On |    |    |
;*    LEFT COIN MECHANISM             |    |    |    |    |    |    |    |    |
;*     x1                            $|    |    |    |Off |    |    |    |    |
;*     x2                             |    |    |    | On |    |    |    |    |
;*    BONUS COIN ADDER                |    |    |    |    |    |    |    |    |
;*     No Bonus Coins                $|Off |Off |Off |    |    |    |    |    |
;*     Every 4, add 1                 |Off | On |Off |    |    |    |    |    |
;*     Every 4, add 2                 |Off | On | On |    |    |    |    |    |
;*     Every 5, add 1                 | On |Off |Off |    |    |    |    |    |
;*     Every 3, add 1                 | On |Off | On |    |    |    |    |    |
;*                                    -----------------------------------------
;*
;*****************************************************
getdip0 sta	potgo			;Read DIPS @ 13/14S
		nop	
		lda	allpot			;Get input
		eor #$FF			;Pokey sends back DIPS inverted... revert
		jmp exitr
    
getdip1 lda	intack			;Get DIPS @ 8S
		jmp exitr

getset0 lda options			;Play Settings (originally from 13/14S)
		jmp exitr
    
getset1 lda options+1		;Price Settings (originally from 8S)
		jmp exitr
		
;Sets the Options0 value in EEROM		
setopt0 ldx #(options/256d) ;MSB of Buffer
        ldy #(options&$ff)  ;LSB of Buffer
		lda #1				;1 byte to receive
		jsr xfer
		jsr wropt			;update csum
		lda #1				;Send back 1 for Success!
		jmp exitr
		
;Sets the Options1 value in EEROM		
setopt1 ldx #((options+1)/256d) ;MSB of Buffer
        ldy #((options+1)&$ff)  ;LSB of Buffer
		lda #1				;1 byte to receive
		jsr xfer
		jsr wropt			;update csum 
		lda #1				;Send back 1 for Success!
		jmp exitr
        
;Returns the EEBAD Flags
geteeflgs
        lda eebad
        jmp exitr

;Return EEROM request flags		
geteedone
		lda eeflg
		jmp exitr

;****************************************************
; Simple Data returns from direct addresses
;****************************************************  
;gettstr jsr g_stest     ;Retest GAMMA and send results
;        jmp exitr
      
pokran  lda POKEY_0+P.RND
        jmp exitr       ;Leave with a random #
        
switch  lda input       ;Where the switches are
		ldx curctrl
		ifne
			asl A 
			asl A 			;Shift up two positions to get player 2 data into bits 6 and 7
		endif
        jmp exitr
        
rolgig  ldx leta
		ldy curctrl
		ifne 
			ldx leta+1
		endif
		lda options
		lsr A
		ifcs	
			txa				;Roller X 2
			asl A 
			tax
		endif
		txa
exitr   sta outdata
exitnr  pla             ;Exit for no data back
        tax 
        pla 
        tay 
        lda #-1
        sta datnum
        pla 
        rti 
    
;**********************************************
   .sbttl "Default EEROM Scores and Initials"
;**********************************************
dscore      .db $32,$45,$09,$00       ;94,532
            .db $24,$05,$08,$00       ;80,524
            .db $85,$72,$07,$00       ;77,285

dinit       .db $1C,$0F,$22           ;REX
            .db $1C,$0F,$22           ;REX
            .db $1C,$0F,$22           ;REX
            
dtesser     .db $11,$0C,$02           
			
;Classic Warp Digits here   
classwarp   .db $32,$00
			.db $64,$00
			.db $28,$04
			.db $13,$05
			.db $22,$03
			.db $86,$03
			.db $49,$08


;*******************************************
        .sbttl "Poweron Self Test"
;*******************************************
g_stest lda #00
        sta rambad
        sta rombad
        sta pokbad
        sta eerbad        ;Clear any 'old' errors
        lda #$FF
        ldx #00
        begin
            sta $0700,X     ;Clear page 7
            inx
        eqend               ;Clear a section
        begin
            lda $0700,X
            eor #$FF
            ifeq
                begin
                    sta $0700,X
                    eor $0700,X
                    bne ?perr           ;Not the same, oh shit, an error
                    beq ?pdone          ;Stop when RAM is 0
                doloop
            endif
?perr       lda #01
            sta rambad      	;Indicate RAM is bad
            jmp ?rdone          ;Done
?pdone      inx 
        eqend               ;Continue until done
		;At this point we know 7 page is okay
        stx tstart      	;Do the rest now
        lda #06
        sta tend
        stx tstart+1
        dex 
        stx tend+1      	;Test 0-6ff
        sty $0700           ;Save CPUID at base of Page 7
        jsr tst2k           ;Test 2K
zpgtst  ifcs
            ldy #01
            sty rambad      ;Have a RAM error
        endif
?rdone  
        
;********************************************
;* RAM Done... Now checksum ROM and make sure
;* it's okay(this is dumb as how would we be 
;* running if it was nfg!!)
;********************************************
        .sbttl "ROM Test"
;********************************************   
        lda #00
        tay 
        sta temp1
        lda # ubyte($8000)
        sta temp1+1
#IF ___OKI = 1
        lda #64d       ;64 pages of $100 is the range $8000-$BFFF
#ELSE
        lda #128d       ;128 pages of $100 is the range $8000-$FFFF
#ENDIF
        sta temp2
        lda #IDENTIFIER_GG    ;Seed for checksum
        begin
            begin
                eor (temp1,Y)
                iny
            eqend
            inc temp1+1
            dec temp2
        eqend
        sta rombad        ;Save Checksum
;******************************************************
        .sbttl "Pokey Test"
;******************************************************
; Pokay addressing is such that all 4 pokeys
; can be addressed in the space 2020-203F
; So Random Numbers are from 203A,2032,202A and 2022
; pok1rnd = qpokey + $22
; pok2rnd = pok1rnd +8
; pok3rnd = pok2rnd +8
; pok4rnd = pok3rnd +8


        ldy #((___NPKS-1)*8)    ;Start with top pokey
        begin
            ldx #50d			        ;try 50 times - Normal Hardware will change pretty quick, MAME will take longer
            lda POKEY_0+P.RND ,Y
            begin
				nop
                cmp POKEY_0+P.RND ,Y
                bne ?ok1        ;It was different, do next Pokey
                dex
            miend
            lda #01
            sta pokbad      ;Bad Pokey
?ok1        tya 
            sec 
            sbc #08         ;Next Pokey
            tay
        miend
;********************************************
        .sbttl "EEROM startup procedure"
;********************************************
eego    lda #00
        sta eerdy
        jsr reinall         ;Read in everything (this goes fast) - will flag bad csums in eebad
        ;look for bad checksums (shown as set bits against mask)
        lda eebad
        ifne
            lda #eflg_option
            bit eebad           ;Bad copy??
            ifne                ;yep, copy over default
                jsr loadop
            endif   
            lda #(eflg_xwarp+eflg_game)
            bit eebad
            ifne
                lda #$00
                ;XLives+Warps First
                ldx #cksumxw-extlie-1
                begin
                    sta extlie,X
                    dex
                miend
                ldx #cksumgs-game1-1
                begin
                    sta game1,X
                    dex
                miend
                jsr wrbook
            endif
            ;High Scores + Initials
            lda #(eflg_scores+eflg_initials+eflg_tesser)
            bit eebad
            ifne
                jsr ldhsin      ;Reset High Scores, Initials and Tessers to defaults, push to EEROM
            endif
            ;Warp Codes
            lda #(eflg_warpco)
            bit eebad
            ifne
                jsr warpclass     ;Write reset warp codes to EEROM - none defined
            endif  
            ;Maze Stats
            lda #(eflg_maze)
            bit eebad
            ifne
                lda #$00
                ldx #cksummz-mzstats-1
                begin
                    sta mzstats,X
                    dex
                    cpx #-1             ;Have to do comparison because X starts negative
                eqend
                jsr wrmzst
            endif 
            inc eerbad        ;Mark a failure in EEROM
        endif
        lda #-1
        sta eerdy           ;EE stuff is ready
		;********************************************************
		; Emergency Check for 'All Zero Warps' - which can happen
		; if something goes wrong generally in MAME with the 
		; EEROM or first run with a game
		;********************************************************
		;jsr chkwrps
		;********************************************************
		; Finalize test results for return to mainline
		;********************************************************
        lda #00
        ldx #03
        begin
            sec
            ldy rambad,X
            ifne
                clc
            endif
            rol A       ;Set bit
            dex
        miend
        ora #$F0            ;Set Don't care bits
        ;Return A = Status
        jmp cindy		;This will leave the address of Cindy on the top of the 
						;stack at 0x01fe, and 0x01ff
						;If the current stack pointer ever points here, then that
						;means that it has overflowed Page 1 

;********************************************
        .sbttl "Ram Test Section"
;********************************************

;soft    = 1
;ramwid  = 8
;speed   = 2
    
tstram
tst2k
ramtest ldy #vtend-voltbl
        begin
            lda voltbl,Y        ;Move volatile code into VRAM
            sta vram,Y
            dey 
        miend
        lda tstart
        sta vadh0
        sta vadh1
        sta vadh2
        sta vadh3
        jmp vram
        
voltbl  ldy #00
        lda patend
        begin
            sta $0100,Y     ;Preset with first test pattern
            iny 
        eqend
        ldx vadh0
        inc vadh0
        cpx tend
        bne voltbl          ;Restore regs and repeat for all pages
nxtone  ldx #patend-pats
        lda $0100           ;Check the location for the first value 
        eor pats,X
        ifeq                ;RAM failure
nxtpat      lda pats-1,X        ;Now check this location with all test patterns
            sta $0100
            eor $0100
            ifeq            ;RAM failed to hold the new pattern
                dex 
                bne nxtpat          ;Try next pattern
                inc vadl1
                inc vadl2
                inc vadl3
                bne nxtone          ;do next
                lda vadh1
                inc vadh1
                inc vadh2
                inc vadh3
                eor tend
                bne nxtone          ;Try next pattern
                jmp endtst          ;and with zero flag and A=0
            endif
        endif
        sec 
        jmp ramerr
vtend

;Fancy test sequence
pats    .byte $00
		.byte $FE
		.byte $01
		.byte $FD
		.byte $02
		.byte $FB
		.byte $04
		.byte $F7
        .byte $08
		.byte $EF
		.byte $10
		.byte $DF
		.byte $20
		.byte $BF
		.byte $40
		.byte $7F
		.byte $80
patend  .byte $FF

endtst  clc             ;Return with carry clear
ramerr  jmp zpgtst      ;We know stack is no good now

;*********************************************************
        .sbttl "EEROM Routines"
;*********************************************************
;* The following routines are exception modes which are  
;* used to get data in and out of the EEROM & back to    
;* Alpha. They run under NMI and assume that the calling 
;* routine in the Alpha system is smart enough not to    
;* send another command while any of these are running.  
;*********************************************************

        
;***********************************************************
;* Load High Scores and Initials from ROM defaults
;***********************************************************
ldhsin  ldx #numsths-1      ;Xfer scores
        begin
            lda dscore,X
            sta hscore,X
            dex
        miend  
        ldx #numstit-1		;Replace with default initials
        begin
            lda dinit,X     	;Get default for intials too
            sta initl,X
            dex
        miend
		ldx #numtessers-1
		begin
            lda dtesser,X
            sta tessers,X		;Reset any tesser bits
            dex
        miend
        jsr wrhsini
        rts

;***********************************************************
;* Load Option Switches from DIPS and flag write to EEROM
;***********************************************************  
dip_emask0	= $E5 	;$E4
dip_emask1	= $01
      
loadop  ;Option Defaults pull from DIP Switches
        sta potgo       ;Read switches
        nop 
        lda allpot      ;Get input, NOTE these are inverse from straight DIP switches
		eor #$FF		;Invert back for sanity, we will keep these values in EEROM too
		eor #dip_emask0
        sta options     ;Save Options0	@ 13/14S PLAY OPTIONS
        lda intack
		eor #dip_emask1
        sta options+1   ;Save Options1 @ 8S PRICE OPTIONS
        jsr wropt    
        rts
        
;*********************************************************	
;* Read in High Scores from Alpha, place in proper RAM   
;* and start EEROM write of the data                     
;*********************************************************
geths   ldx #(hscore/256d)      ;MSB of Buffer
        ldy #(hscore&$ff)       ;LSB of Buffer
        lda #numsths            ;Number of High Scores
        jsr xfer                ;Get this data
        txa                     ;Good Xfer??
        ifeq                    ;yep
            jsr wrhs                ;Start Write of High Scores
            lda #g_done
            sta outdata         ;Signal all done
        endif
        jmp exitnr              ;And leave the exception
    
;*********************************************************
;* Get Initials and Write to EEROM                       
;*********************************************************
getin   ldx #(initl/256d)
        ldy #(initl&$ff)
        lda #numstit          ;Number of Initials
        jsr xfer
        txa 
        ifeq
            jsr wrini           ;Start Write of initials
            lda #g_done
            sta outdata         ;Tell Alpha we are good
        endif
        jmp exitnr
		
;*********************************************************
;* Get Tesseracts and Write to EEROM                       
;*********************************************************
gettes  ldx #(tessers/256d)
        ldy #(tessers&$ff)
        lda #numtessers          ;Number of tessers
        jsr xfer
        txa 
        ifeq
            jsr wrtes           ;Start Write of tessers
            lda #g_done
            sta outdata         ;Tell Alpha we are good
        endif
        jmp exitnr

;**********************************************************************
;Get the Game Stats, notice we are writing into the stats_temp buffer
; Player 1 Game Time - 4 Bytes
; Player 2 Game Time - 4 Bytes
;**********************************************************************
getstap ldx #(stats_temp/256d)
        ldy #(stats_temp&$ff)
        lda #8            ;8 Bytes
        jsr xfer
        txa 
        ifeq
            ;Now we need to add the new stat deltas into the main EEROM data
            ;Game time (up to 9999 seconds per player), which is 160 minutes
            ;that should be long enough to cover most games.
            lda #01             ;2 Players
            sta temp3
            sed                 ;DECIMAL MODE
            begin
                asl A           ;X= x4
                asl A
                tax  
                clc                 
                lda stats_temp,X      
                adc atime1,X
                sta atime1,X
                lda stats_temp+1,X
                adc atime1+1,X
                sta atime1+1,X
                lda #00             ;Still have to roll up carry's tho
                adc atime1+2,X
                sta atime1+2,X
                lda #00         
                adc atime1+3,X
                ifcc                    ;This will prevent rollover
                    sta atime1+3,X
                endif
                dec temp3
                lda temp3
            miend
             
            ;Increment game counts, if the game time values are not zero
            ;Then we assume there was a player game DOH, we only check
            ;the first two bytes which is up to 9999 seconds
            lda #01
            sta temp3
            begin
                asl A
                asl A               ;x4
                tax         
                lda stats_temp,X    ;check the game timer LSB
                ora stats_temp+1,X  ;and MSB, if they are not zero, then increment the game counter
                ifne
                    lda	game1,X
                    clc	
                    adc	#01
                    sta	game1,X
                    lda	game1+1,X
                    adc	#00
                    sta	game1+1,X
                    lda	game1+2,X
                    adc	#00
                    sta	game1+2,X
                    lda	game1+3,X
                    adc	#00
                    ifcc 
                        sta	game1+3,X   ;Dont let it rollover
                    endif
                endif
                dec temp3
                lda temp3
            miend
            cld                 ;END DECIMAL MODE  
            lda #g_done
            sta outdata         ;Tell Alpha we are good
            ;then book it to EEROM
            jsr wrstg           ;Flag Start Write of Stats
        endif
        jmp exitnr

;****************************************************************************
; Get Warp and XLives Stats back from ALPHA
; XLives Data - 2 Bytes
; Warp Data - 2 Bytes Per Warp
;****************************************************************************        
getstawx
        ldx #(stats_temp/256d)
        ldy #(stats_temp&$ff)
        lda #numstxw              ;Get back XLives total and individual numbers for each warp
        jsr xfer
        txa 
        ifeq
            ;Now we need to add the new xlife delta into the main EEROM data
            sed                ;SET DECIMAL MODE            
            clc 
            lda stats_temp    
            adc extlie
            sta extlie
            lda #00
            adc extlie+1
            ifcc
                sta extlie+1
            endif
            ;now do warps
            lda #numwarps
            sta temp3
            begin
                asl A
                tax
                ;+2 to skip over the xlives data
                clc
                lda stats_temp+2,X                  
                adc warps,X
                sta warps,X
                lda stats_temp+3,X       
                adc warps+1,X
                ifcc
                    sta warps+1,X
                endif
                dec temp3
                lda temp3
            miend
            cld                 ;CLEAR DECIMAL MODE
            lda #g_done
            sta outdata         ;Tell Alpha we are good
            jsr wrxwarp
        endif
        jmp exitnr
        
; getstamz
        ; ldx #(stats_temp/256d)
        ; ldy #(stats_temp&$ff)
        ; lda # (1+4)              ;Get back Maze Level Data (NOTE: We receive single bytes, but over here in GAMMA we save 2 bytes per stat)
        ; jsr xfer
        ; txa 
        ; ifeq
            ; lda stats_temp      ;First byte is the level
            ; asl A 
            ; asl A
            ; asl A                   ;x8
            ; tax
            ; ldy #00
            ; lda #03
            ; sta temp3
            ; begin              
                ; lda stats_temp+1,Y      ;+1 is to skip the level number       
                ; sed                     ;SET DECIMAL MODE
                ; clc
                ; adc mzstats,X
                ; sta mzstats,X
                ; lda #00    
                ; adc mzstats+1,X
                ; ifcc
                    ; sta mzstats+1,X
                ; endif
                ; cld                     ;CLEAR DECIMAL MODE
                ; inx
                ; inx
                ; iny             ;Only 1 for source (4 bytes into 8)
                ; dec temp3
                ; lda temp3
            ; miend
            ; lda #g_done
            ; sta outdata         ;Tell Alpha we are good
            ; jsr wrmaze
        ; endif
        ; jmp exitnr
        
        
;*********************************************************
;* Send the High Scores back to Alpha                    
;*********************************************************
sendhs  ldx #(hscore/256d)
        ldy #(hscore&$ff)
        lda #numsths          ;Address and count
        jmp xfer2
        
;*********************************************************
;* Send the initials                                     
;*********************************************************
sendinit    
        ldx #(initl/256d)
        ldy #(initl&$ff)
        lda #numstit
        jmp xfer2
		
;*********************************************************
;* Send the tesseracts                                     
;*********************************************************
sendtess   
        ldx #(tessers/256d)
        ldy #(tessers&$ff)
        lda #numtessers
        jmp xfer2		

;*********************************************************
;* Send the Game Stats
;*********************************************************
sendstap  
        ldx #(game1/256d)
        ldy #(game1&$ff)
        lda #numstgs
        jmp xfer2 
        
;*********************************************************
;* Send the ExtraLife + Warp Stats
;*********************************************************        
sendstawx
        ldx #(extlie/256d)
        ldy #(extlie&$ff)
        lda #numstxw
        jmp xfer2 

; ;*********************************************************
; ;* Send the Maze Stats
; ;*********************************************************  
; sendstamz
        ; ldx #(mzstats/256d)
        ; ldy #(mzstats&$ff)
        ; lda #numstmz
        ; jmp xfer2 
        
;*********************************************************
;* Send All Warps
;*********************************************************
sendwarps
        ldx #(warpcodes/256d)
        ldy #(warpcodes&$ff)
        lda #numwarps*2
        jmp xfer2 

;************************************************
;* Get Warp Code Level Data
;* Will return a single warp code based upon 
;* the level parameter (zero based) given by
;* ALPHA.
;*
;* If data in RAM is negative, then new data will
;* be fetched from POKEY and then written to 
;* EEROM so it is saved through a power cycle.
;************************************************
getwarp ldx #(stats_temp/256d)
        ldy #(stats_temp&$ff)
        lda #01              ;ALPHA sends a single byte saying which level to generate
        jsr xfer
        txa 
        ifeq
            lda stats_temp
            ; asl A                ;X2 for 2-bytes per warp
            ; tax
            ; lda warpcodes,X
            ; cmp #-1
            ; ifeq
                ; ;these are -1, so they have not been generated.
				; jsr genwarp
                ; ;write to EEROM
                ; jsr wrwarpc         ;Queue it up to Write back to EEROM
            ; else
                ; lda warpcodes+1,X 
                ; cmp #-1
                ; beq ?2genw
            ; endif
            ; ;send them back
            ; lda #(warpcodes&$ff)
            ; sta temp1
            ; lda #(warpcodes/256d)
            ; sta temp1+1
            ; txa                 ;back to x1
            ; lsr A
            clc
            adc temp1
            lda #00
            adc temp1+1         ;temp1 now contains the pointer to the warp code
            ldy temp1
            ldx temp1+1
            lda #02
            jmp xfer2 
        endif
        jmp exitnr

;Will write two random bytes into the warpcodes
;memory locations defined by X (warp number * 2)        
genwarp txa
        clc
        adc POKEY_0+P.RND
        and #07
        tay
        lda ?pokrnd,Y
        tay 
        lda qpokey,Y
        and #$7F
        tay
		lda ?goodcodes,Y
		sta warpcodes,X
        txa
        sec
        adc POKEY_0+P.RND
        and #07
        tay
        lda ?pokrnd,Y
        tay 
        lda qpokey,Y
        and #$7F
		tay
		lda ?goodcodes,Y
		sta warpcodes+1,X 
		rts
        
?pokrnd .db $22,$3A,$2A,$32   
        .db $3A,$32,$22,$2A

?goodcodes		
.db  	$11,$12,$13,$14,$15,$16,$17,$18,$19,$21,$22,$23,$24,$25,$26,$27
.db  	$28,$29,$31,$32,$33,$34,$35,$36,$37,$38,$39,$41,$42,$43,$44,$45
.db  	$46,$47,$48,$49,$51,$52,$53,$54,$55,$56,$57,$58,$59,$61,$62,$63
.db  	$64,$65,$66,$67,$68,$69,$71,$72,$73,$74,$75,$76,$77,$78,$79,$81
.db  	$82,$83,$84,$85,$86,$87,$88,$89,$91,$92,$93,$94,$95,$96,$97,$98
.db  	$11,$12,$13,$14,$15,$16,$17,$18,$19,$21,$22,$23,$24,$25,$26,$27
.db  	$46,$47,$48,$49,$51,$52,$53,$54,$55,$56,$57,$58,$59,$61,$62,$63
.db  	$82,$83,$84,$85,$86,$87,$88,$89,$91,$92,$93,$94,$95,$96,$97,$98
		
;***************************************************************
        .sbttl "EEROM Xfer Utilities"
;***************************************************************
;* Xfer - Transfers bytes from the input port to a RAM buffer 
;*                                                        
;* Entry:   A = Number of bytes to xfer                   
;*      X = MSB of pointer to buffer                  
;*      Y = LSB of pointer to buffer                  
;*                                                        
;* Exit:    On Exit, and 'ack' is send back down the com  
;*      port and the status of the program is         
;*          returned to normal.                           
;*      X = 0 = Status Good                           
;*      X = 2 = Timeout, All data not recieved        
;***************************************************************
xfer    sta datflg      ;Save count total
        stx xferbuf+1,ABS    ;Set up pointer
        sty xferbuf,ABS
        lda #00
        sta datnum      ;Set to accept data, skip commands mode
        sta r_nptr      ;Set count to 0
        lda #01         ;ack ready to send
        sta outdata
        cli             ;Allow sounds to continue now
        ;Ready for Data in
        ldx #04         ;MSB of timeout
        ldy #00         ;Time out if this takes too long
?xf1    dey
        ifeq
            dex
            beq timot           ;Will send timout error
        endif
        lda r_nptr      ;See if we are getting anything
        cmp datflg      ;Did we get all?
        bcc ?xf1            ;Wait and get some more
        ldx #00         ;X = 0 is good exit
        rts 
        
;********************************************************
;* If here, we timed out... Send back timeout and reset *
;********************************************************
timot   ldx #g_tout
        stx outdata
        rts 
        
;********************************************************
;* Routine for sending Data to Alpha       
;********************************************************
;* X: High Address Byte of Data to Transfer
;* Y: Low Address Byte of Data to Transfer
;* A: Number of Bytes
;********************************************************
xfer2   sta datflg      ;Save count total
        stx xferbuf+1,ABS    ;Set up pointer
        sty xferbuf,ABS
        tay             ;Indirect pointer address
        dey             ;One less (offset from 0)
        begin
            begin
                lda #alpharcvd      ;Test for output empty
                bit portst
            neend               ;Alphy ready to recieve
            lda (xferbuf,Y)
            sta outdata     ;Send data
            dey
            cpy #-1
        eqend
        jmp exitnr           ;We are done!

;Include the actuall sound data
#include "tw_gamsnd.asm"

;*************************************
;* System Startup Pointers           
;*************************************

#IF ___OKI = 1
    #if $ > $bff9 \ .error "GAMMA ROM has extended outside of design size." \ #endif
    .org $BFF9  
chkg    .chk $8000,$BFFF,IDENTIFIER_GG
    .org $BFFA  
#ELSE
    #if $ > $fff9 \ .error "GAMMA ROM has extended outside of design size." \ #endif
    .org $FFF9  
chkg    .chk $8000,$FFFF,IDENTIFIER_GG
    .org $FFFA  
#ENDIF 

    .word   g_nmi
    .word   g_main
    .word   g_irq
        
    .end


