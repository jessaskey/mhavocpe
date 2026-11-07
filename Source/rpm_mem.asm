

;Special Sound Data for MAX Background sound
;maxsndcbuf		.block  maxsndcbuflen

;	orgchk($05F6,$)
;maxsndfbuf		.block  maxsndfbuflen

;***********************************************************
;* RPM RAM will begin again at $0200
;***********************************************************
    .org ___RPMMEMST

;**************************************
;	CHANNEL REGISTERS
;**************************************
ch_sid          .block  ___CHNS
#IF ___FLG = 1 
ch_eflg         .block  ___CHNS
#ENDIF
ch_tuneptr      .block  ___CHNS*2		;pointer to start of current tune
ch_amp          .block  ___CHNS*2		;pointer to current amplitude envelope
ch_freq    	    .block  ___CHNS*2		;pointer to current frequency envelope
ch_onote        .block  ___CHNS*2		;CURRENT OUTPUT NOTE COUNT
ch_odur         .block  ___CHNS*2		;CURRENT OUTPUT DURATION REMAINING           
ch_vvol         .block  ___CHNS			;MEDIAN VOICE VOLUME
ch_vsn          .block  ___CHNS			;CURRENT TUNE SEQUENCE #
ch_viu          .block  ___CHNS	        ;VOICE IN USE FLAG
ch_ampseq       .block  ___CHNS         ;AMPLITUDE POINTER
ch_ampcnt       .block  ___CHNS         ;REPETITION COUNTER
ch_ampdlta      .block  ___CHNS         ;ACCUMULATOR
ch_amplcnt      .block  ___CHNS         ;Loop counter
ch_freqdidx     .block  ___CHNS
ch_fcnt         .block  ___CHNS
ch_fdelta       .block  ___CHNS*3
ch_flcnt        .block  ___CHNS
ch_rate         .block  ___CHNS         ;CURRENT OUTPUT RATE (BIGGER # = SHORTER DUR.)
ch_key          .block  ___CHNS			;CURRENT KEY OFFSET
ch_ochk         .block  ___CHNS			;CHECK MASK FOR ALIGNMENT OF DIFFERENT VOICES
ch_ochkm        .block  ___CHNS         ;MASK USED TO STORE INTO OCHK (A CONSTANT)
ch_ctrlmask     .block  ___CHNS         ;VOICE AMPLITUDE CONTROL BITS
ch_synthmode    .block  ___CHNS			;VOICE SYNTH. MODE FLAG
ch_synthfadj    .block  ___CHNS*2		;SYNTH. MODE FREQ. ADJUST		
ch_jmppc        .block  ___CHNS*2		;OUTPUT POINTER SUB-PHRASE SAVE
ch_jmplocpc     .block  ___CHNS
ch_looppc       .block  ___CHNS*2		;OUTPUT POINTER LOOP SAVE
ch_looplocalpc  .block  ___CHNS
ch_loopcount    .block  ___CHNS			;LOOP COUNTER

#IF ___CNT != 0
ch_vcnt         .block  ___CHNS         ;NOTE COUNTER
#ENDIF
    
#IF ___CTL = 0
ch_vcctl        .block  ___CHNS         ;CONTROL-REGISTER CLEAR MASK
ch_vsctl        .block  ___CHNS         ;CONTROL-REGISTER SET MASK
#ENDIF

ch_vlnks        .block  ___CHNS         ;LINK TO NEXT HIGHER PRIORITY TUNE
                .block  ___NPKS*4
                .block  ___NGIS*3	    ;QUEUE HEADER FIELDS ( MUST BE CONTIGUOUS )

;**************************************
;MISC. COMMON DEVICE VARIABLES
;**************************************
ch_tmp          .block  1               ;OTHER ROUTINES CAN USE THIS BYTE
po_pri          .block  2               ;VOICE 8/16 BIT PRIORITIES
#IF ___SVV != 0
po_svf          .block  1               ;CURRENT VOICE CHANNEL SPLIT (8 OR 16 BIT)?
po_pri8         .block  1               ;ODD-CHANNEL PRIORITY
po_vol8         .block  1               ;ODD-CHANNEL VOLUME
po_frq8         .block  1               ;ODD-CHANNEL FREQUENCY
#ENDIF
po_volsh        .block  1+___SVV        ;VOLUME SHADOWS
po_frqsh        .block  2+___SVV        ;FREQUENCY SHADOWS

po_prev         .block  1               ;BACKWARDS LINK- USED IN INTERRUPTS
devtype         .block  1               ;DEVICE TYPE FLAG (0=POKEY,1=PSGI etc..)

#IF ___CTL != 0
ctlsh	        .block	1		        ;DEVICE CONTROL REGISTER SHADOWS
devctl	        .block  ___NPKS+___NGIS	;CONTROL REGISTER SHADOWS
#ELSE
po_csmsk        .block  (2*___SVV)+3    ;AND/OR MASKS FOR CONTROL REGISTER
po_ccmsk        .block  (2*___SVV)+3    ;svv=1 odd/even/8bitsave/halfsave
#ENDIF



