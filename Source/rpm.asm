;******************************************************************************
    .title "Rusty's Pokey Music (RPM) Slave Module"
;******************************************************************************                                       
;******************************************************************************                                       
;* RPM Version 0.4     
;*                                                                       
;* This module is used on the slave processor to handle sound and interfacing 
;* It needs to be linked with game specific code and the following variables  
;* must be predefined for that game in order for RPM to work correctly.       
;*                                                                              
;* The following are pre-compilation flags to enable or disable individual    
;* features within RPM. The must be defined as 0 (not used) or 1 (used).      
;*                                                                            
;*  ___tlk  Enables SPEECH (TMS5220 Hardware installed)     
;*  ___oki  Enables SPEECH (MSM6295 Hardware installed)                  
;*  ___imm  IMMEDIATE Mode Functions                                          
;*  ___nmi  NMI port control                                                  
;*  ___exm  support EXCEPTION mode                                            
;*                                                                                     
;* Device Declarations - This allows the macro STSND to start logical named   
;*                       channels more easily. These equates should be common 
;*                       on all pokey driven games.                           
;*                                                                            
;*  POKEY_CH0 =   0   ;Channel Offsets                                
;*  POKEY_CH1 =   4                                                     
;*  POKEY_CH2 =   8                                                     
;*  POKEY_CH3 =   12                                                    
;*                                                                            
;* Declare all tables large enough. The macros will fill in                   
;* the entries....                                                            
;*                                                                            
;* ***** Table Limits *****                                                   
;*                                                                            
;* Byte Tables: these are addressed using Y as an index, so                   
;*              they are limited to 256d entries.                             
;*                                                                            
;*  numcom  Total Number of legal commands                              
;*  numsnd  MAX number of sound groups                                  
;*  numtun  MAX number of tunes to group                                
;*  numimm  MAX number of immediates                                    
;*                                                                            
;* Word Tables: these are limited to 128d entries for the same reason.                                       
;*                                                                            
;*  numexc  MAX exceptions                              
;*  numfcns Number of legal user functions                  
;******************************************************************************
    .sbttl "RPM Macro Definitions"
;******************************************************************************
;*                                                                            
;* The Valid Macros are:                                                      
;*                                                                            
;* SILENT   name            This sets to ATTRACT/silent mode                
;* NOISY    name            This allows all sounds                          
;* SETATR   name,level      This sets a priority level level below          
;*                              which no sounds are heard.                
;******************************************************************************
rt_attract  =   0

;******************************************************************************
;* SETFL    name,flag       Sets the event flag 1-8                         
;* CLRFL    name,flag       Clear the event flag 1-8                        
;******************************************************************************
rt_eflag    =   1

;******************************************************************************
;* RESTI    name            Reset TI sound chip                             
;* KILALL   name            Installs call to allow resetting RPM.           
;* GOSUB    name,fname      This calls the function FNAME                   
;******************************************************************************
rt_gosub    =    2  
  
;******************************************************************************
;* KILPRI   name,priority   Kills all sounds of given priority/class        
;******************************************************************************0
rt_killpri  =  3

;******************************************************************************
;* KILID    name,ident      Kills sound started by name IDENT.              
;******************************************************************************
rt_killid   =  4

;******************************************************************************
;* KILDEV   name,dev,chan   Kills all sounds on channel CHAN of device.     
;******************************************************************************
rt_killdev  =  5

;******************************************************************************
;* STSND    name,replacement, START SOUND replacement is REPL or NOREPL,      
;*          priority,eflag,     priority 1 is lowest, eflag is 1-8        
;*          queue,tuneID.       queue is device-channel, tuenID is index  
;*                      into tunetable.                           
;*                                                    
;* CONT     pri,efl,q,tuneID  Continues by linking to previous STSND                                                           
;******************************************************************************
rt_sound    =   6

;******************************************************************************
;* REPORT   name,value      Report VALUE throught port to game.             
;******************************************************************************
rt_report   =   7

;******************************************************************************
;* TALK name,phrase     Speak a phrase                                  
;******************************************************************************
rt_speech   =   8 
                                                
;******************************************************************************
;* INSCMD   name,cmd,arg    Execute command CMD with arg ARG                
;* IMMED    name,ident,q,   This will execute the function in               
;*          fon,arg         immediate-mode when enabled.              
;******************************************************************************
rt_immed    =   9

;******************************************************************************
;* EXCEPT   name,function   This causes IMMEDIATE response in NMI systems   
;******************************************************************************
rt_except   =   -1
no_exe      =   -1
no_ext		= 	-1
REPL        =   -1
NOREPL      =   0

;*****************************************************************************
;* Sound Table Macros
;*****************************************************************************
;* NEWTUNE defines a seqence of TUNE parameters.
;*         Also fills the tunetable with the current location and     
;*         also defines an offset into the table with the name given in ID.  
;*****************************************************************************
#define  NEWTUNE(id)    \tmpptr .set *
#defcont                \ .org t_tune_l+i_tune
#defcont                \ .byte (tmpptr&$FF)
#defcont                \ .org t_tune_h+i_tune
#defcont                \ .byte (tmpptr/$100)
#defcont                \id = i_tune
#defcont                \i_tune .set i_tune+1
#defcont                \ .org tmpptr
#defcont                \#IF i_tune > (numtun)
#defcont			    \	.error "RPM: Too many Tunes defined. Increase limit."
#defcont				\#ENDIF 
;*****************************************************************************
;* NEWSNDFREQ defines a pointer to a block of data used for the frequency 
;*            envelope for the sound.  
;*****************************************************************************
#define  NEWSNDFREQ(id) \tmpptr .set *
#defcont                \ .org t_freqenv_l+i_freqenv
#defcont				\ .byte (tmpptr&$FF)
#defcont                \ .org t_freqenv_h+i_freqenv
#defcont                \ .byte (tmpptr/$100)
#defcont                \id = i_freqenv
#defcont                \i_freqenv .set i_freqenv + 1
#defcont                \ .org tmpptr
#defcont                \#IF i_freqenv > (numenv)
#defcont			    \	.error "RPM: Too many frequency envelopes defined. Increase limit."
#defcont				\#ENDIF 
;*****************************************************************************
;* NEWMXSNDFREQ defines a pointer to a block of data used for the frequency 
;*            envelope for the sound - THIS IS USED FOR THE MAX BACKGROUND 
;*            SOUND BUFFER.
;*****************************************************************************
#define  NEWMXSNDFREQ(id) \tmpptr .set *
#defcont				\maxsndfbufd = *
#defcont                \ .org t_freqenv_l+i_freqenv
#defcont				\ .byte (maxsndfbuf&$FF)
#defcont                \ .org t_freqenv_h+i_freqenv
#defcont                \ .byte (maxsndfbuf/$100)
#defcont                \id = i_freqenv
#defcont                \i_freqenv .set i_freqenv + 1
#defcont                \ .org tmpptr
#defcont                \#IF i_freqenv > (numenv)
#defcont			    \	.error "RPM: Too many frequency envelopes defined. Increase limit."
#defcont				\#ENDIF 
;*****************************************************************************
;* NEWSNDENV defines a pointer to a block of data used for the amplitude 
;*            envelope for the sound.  
;*****************************************************************************
#define  NEWSNDENV(id) \tmpptr .set *
#defcont                \ .org t_ampenv_l+i_ampenv
#defcont				\ .byte (tmpptr&$FF)
#defcont                \ .org t_ampenv_h+i_ampenv
#defcont                \ .byte (tmpptr/$100)
#defcont                \id = i_ampenv
#defcont                \i_ampenv .set i_ampenv + 1
#defcont                \ .org tmpptr
#defcont                \#IF i_ampenv > (numenv)
#defcont			    \	.error "RPM: Too many amplitude envelopes defined. Increase limit."
#defcont				\#ENDIF 

#define  NEWMXSNDENV(id) \tmpptr .set *
#defcont				\maxsndcbufd = *
#defcont                \ .org t_ampenv_l+i_ampenv
#defcont				\ .byte (maxsndcbuf&$FF)
#defcont                \ .org t_ampenv_h+i_ampenv
#defcont                \ .byte (maxsndfbuf/$100)
#defcont                \id = i_ampenv
#defcont                \i_ampenv .set i_ampenv + 1
#defcont                \ .org tmpptr
#defcont                \#IF i_ampenv > (numenv)
#defcont			    \	.error "RPM: Too many amplitude envelopes defined. Increase limit."
#defcont				\#ENDIF 
;************************************************************************** 
;#define  CREATEGLOBAL(gname) \gname = o_commandnum
;#defcont                     \.export gname

#define  SETCOMMAND(name,value)     \tmpptr .set *
#defcont                            \.export name
#defcont                            \name = o_commandnum
#defcont                            \ .org t_commandtype+o_commandnum
#defcont                            \ .byte value
#defcont                            \o_commandnum .set o_commandnum+1 
#defcont                            \ .org tmpptr
#defcont                            \#IF o_commandnum > (numcom)
#defcont			                \	.error "RPM: Too many command types defined. Increase 'numcom' limit."
#defcont				            \#ENDIF  

#define  SETCMDPARAM(value) \tmpptr .set *
#defcont                    \ .org t_commandparam+o_commandparam
#defcont                    \ .byte value
#defcont                    \o_commandparam .set o_commandparam+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_commandparam > (numcom)
#defcont			        \	.error "RPM: Too many command parameters defined. Increase 'numcom' limit."
#defcont				    \#ENDIF 

;NOTE: technically since other commands can use the exception pointer table, then the max value needs to be 'numcom' here
;      instead of numexc which might not make sense, but it is because any command can actually have an exception, not
;      really sure if numexc is actually needed in this case?
#define  SETEXPIDX(value)   \tmpptr .set *
#defcont                    \ .org t_exceptidx+o_exceptidx
#defcont                    \ .byte value
#defcont                    \o_exceptidx .set o_exceptidx+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_exceptidx > (numcom) 
#defcont			        \	.error "RPM: Too many exceptions defined across all commands. Increase 'numcom'."
#defcont				    \#ENDIF 

; #define  SETEXTENDIDX(value) \tmpptr .set *
; #defcont                    \ .org t_extendidx+o_extendidx
; #defcont                    \ .byte value
; #defcont                    \o_extendidx .set o_extendidx+1
; #defcont                    \ .org tmpptr
; #defcont                    \#IF o_extendidx > numcom
; #defcont			        \	.error "RPM: Too many extensions defined. Increase limit."
; #defcont				    \#ENDIF 

#define  SETEXPPRT(value)   \tmpptr .set *
#defcont                    \ .org t_exceptptr+(o_exceptptr*2)
#defcont                    \ .word value-1
#defcont                    \o_exceptptr .set o_exceptptr+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_exceptptr > numexc
#defcont			        \	.error "RPM: Too many command exceptions pointers defined. Increase 'numexc' to allow more WORD pointers."
#defcont				    \#ENDIF 

#define  INSERTFUNC(name)   \tmpptr .set *
#defcont                    \ .org t_userfunction+(o_userfunction*2)
#defcont                    \ .word name-1
#defcont                    \o_userfunction .set o_userfunction+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_userfunction > numfcns
#defcont			        \	.error "RPM: Too many user functions defined. Increase limit."
#defcont				    \#ENDIF

#define INSERTTUNEST(name)  \tmpptr .set *
#defcont                    \ .org t_tunestarts+o_tunestarts
#defcont                    \ .byte tunenum
#defcont                    \o_tunestarts .set o_tunestarts+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_tunestarts > numsnd
#defcont			        \	.error "RPM: Too many tune starts defined. Increase limit."
#defcont				    \#ENDIF 

; #define INSERTREPL(thisrep) \tmpptr .set *
; #defcont                    \ .org t_replacement+o_replacement
; #defcont                    \ .byte thisrep
; #defcont                    \o_replacement .set o_replacement+1
; #defcont                    \ .org tmpptr
; #defcont                    \#IF o_replacement > (numsnd)
; #defcont			        \	.error "RPM: Too many replacements defined. Increase limit. $1"
; #defcont				    \#ENDIF 

; #define INSERTEFLAG(flag)   \tmpptr .set *
; #defcont                    \ .org t_eflag+o_eflag
; #defcont                    \ .byte flag
; #defcont                    \o_eflag .set o_eflag+1
; #defcont                    \ .org tmpptr
; #defcont                    \#IF o_eflag > (numtun)
; #defcont			        \	.error "RPM: Too many flags defined. Increase limit."
; #defcont				    \#ENDIF 

#define INSERTPRIORITY(pri) \tmpptr .set *
#defcont                    \ .org t_priority+o_priority
#defcont                    \ .byte pri
#defcont                    \o_priority .set o_priority+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_priority > (numtun)
#defcont			        \	.error "RPM: Too many priorities defined. Increase limit."
#defcont				    \#ENDIF 

#define INSERTQUEUE(que)    \tmpptr .set *
#defcont                    \ .org t_schan+o_queue
#defcont                    \ .byte que
#defcont                    \o_queue .set o_queue+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_queue > (numtun)
#defcont			        \	.error "RPM: Too many queues defined. Increase limit."
#defcont				    \#ENDIF 

#define SETTUNEID(id)       \tmpptr .set *
#defcont                    \ .org t_stune+o_stune
#defcont                    \ .byte id
#defcont                    \o_stune .set o_stune+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_stune > (numtun)
#defcont			        \	.error "RPM: Too many Tune IDs defined. Increase limit."
#defcont				    \#ENDIF 

#define INSERTTCONT(tcont)  \tmpptr .set *
#defcont                    \ .org t_tunecont+tunenum-1
#defcont                    \#IF tunenum!=0
#defcont                    \ .byte tcont, $0
#defcont                    \#ENDIF
#defcont                    \ .org tmpptr

#IF ___tlk = 1
#define INSERTTALK(tstart,tend) \tmpptr .set *
#defcont                        \ .org t_spdptr+(o_spdptr*2)
#defcont                        \ .word tstart
#defcont                        \ .org t_spdlen+(o_spdptr*2)
#defcont                        \ .word tend-tstart
#defcont                        \o_spdptr .set o_spdptr+1
#defcont                        \ .org tmpptr
#defcont                        \#IF o_spdptr > (numtlk)
#defcont			            \	.error "RPM: Too many TALK IDs defined. Increase limit."
#defcont				        \#ENDIF 
#ENDIF

#IF ___oki = 1
#define INSERTADPCM(achan,aatt) \tmpptr .set *
#defcont                        \ .org t_okicmd+(o_okiptr)
#defcont                        \ .db ($80 | o_okiptr+1)
#defcont                        \ .org t_okiattch+(o_okiptr)
#defcont                        \ .db (achan | aatt)
#defcont                        \o_okiptr .set o_okiptr+1
#defcont                        \ .org tmpptr
#defcont                        \#IF o_okiptr > (numoki)
#defcont			            \	.error "RPM: Too many ADPCM IDs defined. Increase limit."
#defcont				        \#ENDIF 
#ENDIF

;*****************************************************************************
;* Define the RPM Macros
;*****************************************************************************

#define  SETATR(name,level) \ SETCOMMAND(name, rt_attract)
#defcont                    \ SETCMDPARAM(level)
#defcont                    \ SETEXPIDX(no_exe)
;#defcont					\ SETEXTENDIDX(no_ext)

#define  SILENT(name)       \ SETATR(name,$ff)    
#define  NOISY(name)        \ SETATR(name,$00)

#define  CLRFL(name,flag)   \ SETCOMMAND(name,rt_eflag)
#defcont                    \ SETCMDPARAM(flag)
#defcont                    \ SETEXPIDX(no_exe)
;#defcont					\ SETEXTENDIDX(no_ext)

#define  SETFL(name,flag)   \ CLRFL(name,flag|$80)

#define  GOSUB(name,fname)  \ SETCOMMAND(name,rt_gosub)
#defcont                    \ SETCMDPARAM(o_userfunction)
#defcont                    \ INSERTFUNC(fname)
#defcont                    \ SETEXPIDX(no_exe)
;#defcont					\ SETEXTENDIDX(no_ext)

#define  KILALL(name)       \ GOSUB(name,initrpm)
#define  RESTI(name)        \ GOSUB(name,res_ti)

#define  KILPRI(name,priority)  \ SETCOMMAND(name,rt_killpri)
#defcont                        \ SETCMDPARAM(priority)
#defcont                        \ SETEXPIDX(no_exe)
;#defcont						\ SETEXTENDIDX(no_ext)

#define  KILID(name,ident)  \ SETCOMMAND(name, rt_killid)
#defcont                    \ SETCMDPARAM(ident)
#defcont                    \ SETEXPIDX(no_exe)
;#defcont					\ SETEXTENDIDX(no_ext)

#define  KILDEV(name,dev,chan)  \ SETCOMMAND(name, rt_killdev)
#defcont                        \ SETCMDPARAM(dev)
#defcont                        \ SETEXPIDX(no_exe)
;#defcont						\ SETEXTENDIDX(no_ext)

#define  STSND(name,replacement,priority,eflag,queue,tuneID) \ SETCOMMAND(name, rt_sound)
#defcont				\cmdp_+name = stsndcount
#defcont                \ SETCMDPARAM(stsndcount) 
#defcont                \ INSERTTUNEST(tuneID)
;#defcont                \ INSERTEFLAG(eflag)
#defcont                \ INSERTPRIORITY(priority)
#defcont                \ INSERTQUEUE(queue)
#defcont                \ SETTUNEID(tuneID)
#defcont                \ INSERTTCONT(0)
;#defcont                \ INSERTREPL(replacement)
#defcont                \ SETEXPIDX(no_exe)
;#defcont				\ SETEXTENDIDX(extension)
#defcont                \tunenum .set tunenum+1
#defcont                \stsndcount .set stsndcount+1

#define  CONT(priority,eflag,queue,tuneID) \ INSERTPRIORITY(priority)
;#defcont                \ INSERTEFLAG(eflag)
#defcont                \ INSERTQUEUE(queue)
#defcont                \ SETTUNEID(tuneID)
#defcont                \ INSERTTCONT(tunenum)
;#defcont                \ INSERTREPL(REPL)
#defcont                \ SETEXPIDX(no_exe)
;#defcont				\ SETEXTENDIDX(no_ext)
#defcont                \tunenum .set tunenum+1

#define  REPORT(name,value) \ SETCOMMAND(name,rt_report)
#defcont                    \ SETCMDPARAM(value)
#defcont                    \ SETEXPIDX(no_exe)
;#defcont					\ SETEXTENDIDX(no_ext)

#IF ___tlk == 1
    #define  TALK(name,spstart,spend) \ SETCOMMAND(name,rt_speech)
    #defcont                          \ SETCMDPARAM(o_spdptr)
    #defcont                          \ INSERTTALK(spstart,spend)
;    #defcont                          \ SETEXPIDX(no_exe)
;	#defcont						  \ SETEXTENDIDX(no_ext)
#ENDIF

#IF ___oki == 1
    #define  ADPCM(name,channel,att)  \ SETCOMMAND(name,rt_speech)
    #defcont                          \ SETCMDPARAM(o_okiptr)
	#defcont                          \ INSERTADPCM(channel,att)
;    #defcont                          \ SETEXPIDX(no_exe)
;	#defcont						  \ SETEXTENDIDX(no_ext)
	
dbm0	.equ	0
dbm3	.equ	1
dbm6	.equ    2
dbm9	.equ    3
dbm12	.equ	4
dbm15	.equ	5
dbm18	.equ	6
dbm21	.equ	7
dbm24	.equ	8

chan1	.equ    $10
chan2	.equ	$20
chan3	.equ	$40
chan4	.equ	$80

#ENDIF

#define  INSCMD(name,scmd,arg) 

#IF ___imm != 0
    #define  IMMED(name,ident,queue,fon,arg)
#ENDIF

#IF ___exm != 0
    #define  EXCEPT(name,function)  \ SETCOMMAND(name,rt_except)
    #defcont                        \ SETCMDPARAM(rt_except)
    #defcont                        \ SETEXPIDX(o_exceptptr)
    #defcont                        \ SETEXPPRT(function)
	;#defcont						\ SETEXTENDIDX(no_ext)
#ENDIF


;******************************************************************************
; RPM Sound Data Macros - These will allow us to define sound data in a 
;                         common way and easily allow switching between 
;                         several output formats.
;******************************************************************************
;#define     SDCTRL(ticks,value)		\ .db ticks \ .db ((value)<<3)

;**********************************************************************************
; Dissection of FREQ data
;**********************************************************************************
;	Frequency data consists of 3 bytes of data
;
;   Byte 1 	= (Timer)  Range $00-$FE, $FF is special and designates a JUMP/LOOP
;   Byte 2/3= (Addend) Encoded Decimal Number MSB+LSB (Signed)
;                      
; 
;   Examples: 
;
;       $01,$E0,$00: Byte1 =   1 Ticks, Byte 2/3 = ($00E0 << 3) =   $7.0  =   7.0d
;    	$02,$F0,$FF: Byte1 =   2 Ticks, Byte 2/3 = ($FFF0 << 3) =  $-0.8  =  -0.5d
;  		$03,$10,$00: Byte1 =   3 Ticks, Byte 2/3 = ($0010 << 3) =   $0.8  =   0.5d
;		$04,$20,$F7: Byte1 =   4 Ticks, Byte 2/3 = ($F720 << 3) = $-47.0  = -71.0d
;		$05,$00,$00: Byte1 =   5 Ticks, Byte 2/3 = ($0000 << 3) =   $0.0  =   0.0d
;
;   Notes on data values:
;      The data value passed in this table is a 8+4 bit number formatted as
;         XXXVVVVV VVV.SSSSX
;
;      Where:
;            X - Not Used
;            V - Whole Number Values
;            S - Significand Values
;
;  
__rpmsloop	= 0;    
#DEFINE SBEGIN	\ .push *

#DEFINE SFREQ(slope,duration)	\ .db duration 
#DEFCONT						\ #if (slope>$7FFF)
#DEFCONT						\ 	.error "Slope value too high, must be less than 256."
#DEFCONT						\ #else
#DEFCONT						\	#if (duration > $FE)
#DEFCONT						\		.error "Duration must be less than 255."
#DEFCONT						\	#else
#DEFCONT						\ 		.dw slope << 1
#DEFCONT						\	#endif
#DEFCONT						\ #endif

#DEFINE SCTRL(slope,duration)	\ #if (slope>$64)
#DEFCONT						\ 	.error "Slope value too high, must be less than 64."
#DEFCONT						\ #else
#DEFCONT						\ 	#if (slope<-$63)
#DEFCONT						\ 		.error "Slope value too high, must be greater than -63."
#DEFCONT						\   #else
#DEFCONT						\		#if (duration > $FE)
#DEFCONT						\			.error "Duration must be less than 255."
#DEFCONT						\		#else
#DEFCONT						\ 			.db duration 
#DEFCONT						\ 			.db slope << 1
#DEFCONT						\		#endif
#DEFCONT						\   #endif
#DEFCONT						\ #endif

#DEFINE SLOOP(loops)	\ .pop __rpmsloop
#DEFCONT				\ .db $FF,loops,(*-__rpmsloop+3)



#DEFINE SFREQ2(slope,duration)	\ .db duration 
#DEFCONT						\ #if (slope>32767)
#DEFCONT						\ 	.error "Slope value too high, must be less than or equal to 32767."
#DEFCONT						\ #else
#DEFCONT						\   #if (slope<-32768)
#DEFCONT						\       .error "Slope value too low, must be greater than or equal too -32768."
#DEFCONT						\	#else
#DEFCONT						\		#if (duration > $FE)
#DEFCONT						\			.error "Duration must be less than 255."
#DEFCONT						\		#else
#DEFCONT						\ 			.dw slope
#DEFCONT						\		#endif
#DEFCONT						\ 	#endif
#DEFCONT						\ #endif
					


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
;*  pokmain - Selects specific pokey chip
;*  dopokey - gets pokey address and channel offset
;*  updpokey - ?
;*  ?
;*
;* I believe this is getting called every 200ns
;******************************************************************************
    
g_irq   sta intack
        pha 
        lda irqcnt      ;irqcnt always counts from 4-1
        lsr A
        dec irqcnt
        ifmi
			;***********************************
			;special code to trigger speech every 3 seconds
; #IF DEBUG != 0
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
; #ENDIF
			;***********************************
            lda #NUMPOKEY
            sta irqcnt      ;Reset when becomes -
        endif
        ifcs
            txa 
            pha 
            tsx 
            ;lda $0100,X     
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
            jsr pokmain     ;Update the Pokeys
			inc framecnt
            pla 
            tay 
            pla 
            tax
        endif
        pla 
        rti
        
;***********************************************
        .sbttl "Pokey Mainline"
;***********************************************
;* This is the main routine for the pokey's    
;* It takes care of sharing pokey time between 
;* the 4 pokeys by updating only 2 at a time.  
;*
;* If Speech is loaded, it calls the speech 
;* update routine too.
;***********************************************       
pokmain 

#IF ___tlk == 1
        jsr updspeech
#ENDIF
        lda framecnt
        lsr A
        ifcs                ;Lets do the even ones now
            ldx #00
            jsr dopokey
            ldx #02
            jsr dopokey    
        else               ;Do the odd pokey's instead
            ldx #01
            jsr dopokey
            ldx #03   
            jsr dopokey
        endif
        
#IF ___tlk == 1
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
        sta gw_i
        sta gw_ia
        sta reportf     ;Clear the report flag (not used in MH)
        ldx #$1F
        begin
            sta preg_chanp,X
            dex 
        miend
        ldx #NUM_CHANNELS-1
        begin
            sta ch_pri,X
            sta ch_mys_28,X
            sta ch_mys_29,X
            sta ch_vol,X
            sta ch_ctrlmask,X		;clear all the volumes and poly settings
            dex
        miend
        ldx #03             ;Do this for each POKEY in the QUAD
        begin
            lda pokaddl,X
            sta r_pokaddr
            lda pokaddh,X
            sta r_pokaddr+1
            lda #00
            ldy #POKEY_AUDCTL
            sta (r_pokaddr,Y)
            stx r_temp2
            clc 
            lda r_pokaddr
            sta r_temp2+1
            lda #$27
            adc r_pokaddr
            sta r_pokaddr
            ldx #00
            txa 
            sta (r_pokaddr,X)
            begin
                adc #01
                cmp #03
            eqend
            sta (r_pokaddr,X)
            ldx r_temp2
            lda r_temp2+1
            sta r_pokaddr
            ldy #POKEY_AUDCTL
            lda #00
            sta (r_pokaddr,Y)
            lda #00
            ldy #08             ;This will store a $00 in each control and frequency address
            begin
                dey 
                sta (r_pokaddr,Y)
            eqend
            dex 
        miend
		;Special addition... initrpm should also init the max background sound too
		jsr initmaxbuf		;Initialize the Max background sound data into RAM
        plp 
        rts 
        
;*****************************************************
;* Slave Command Parser - This routine will check to *
;* be sure that our command is valid. It will then   *
;* pull that commands controlling routine out of the *
;* tables and also load the local parameter for that *
;* command and pass it to the routine via the A reg. *
;*****************************************************
scomm   cpy #o_commandnum           ;Is it above our max command value?
        ifcc            
            lda t_commandtype,Y
            cmp #(rt_immed+1)           ;Is it above the valid command type?
            ifcc    
                asl A               ;X2
                tax 
                lda functbl+1,X         ;Get the address of the function routine
                pha 
                lda functbl,X
                pha 
                lda t_commandparam,Y        ;Send the Name as well for reference
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
setslvl sta r_sysi
        rts 
        
;****************************************************
    .sbttl "Set/Reset E-Flag"
;****************************************************
sceflg  pha 
        and #07
        tay 
        pla 
        and #$80
        sta eflg0,Y
        rts 
        
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
        sta r_temp2
        ldy #NUM_CHANNELS-1
        begin
            lda ch_pri,Y
            lsr A
            eor r_temp2
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
            lda t_commandparam,Y        ;Get the ID of the sound number to kill
            sta r_temp2
            ldy #NUM_CHANNELS-1
            begin
                cmp ch_sid,Y        ;Does this Sound ID match?
                ifeq                ;yes
                    lda #-1     
                    sta ch_sid,Y        ;Kill it!
                    lda r_temp2
                endif
                dey
            miend
            plp
        endif
        rts

;*************************************************
;* Kill a particular sound by device and channel *
;*************************************************  
killch  sta r_temp2     ;A contains two nibbles of info
        lsr A           ;MSN = Device Number  (0-3)
        lsr A           ;LSN = Channel Number (0-3)
        lsr A
        lsr A
        tay             ;Y = Device
        lda r_temp2
        and #$0F
        clc 
        adc choffl,Y    ;Channel Offset 
        tay 
        php 
        sei 
        lda preg_chanp,Y
        ifne
            begin   
                tay
                dey 
                lda #-1
                sta ch_sid,Y        ;Kill this one!
                lda preg_chanp,Y
            eqend
        endif
        plp 
        rts
            
;*******************************************************
;* Report: Sends a preset bytes back to the Alpha      *
;*         processor. This macro is not used in MH.    *
;*******************************************************
repbyte ldy gw_ia
        sta gw_queue,Y
        iny 
        cpy #$80
        ifcs
            ldy #00
        endif
        cpy gw_i
        ifne
            sty gw_ia
            rts
        endif   
        lda #$80
        sta reportf     ;Set the report flag (not used in MH)
        rts 
    
;********************************************************
;* Immed: Command Immediate Response to a function code 
;*                                                    
;********************************************************   
immexe  tay 
        lda t_commandparam,Y    ;Get the ID of the sound
        tax             
        lda t_immfunc,X     ;Get the function number
        cmp #$1C            ;Is it higher than allowed
        bcs ?ret            ;yep, ignore it!
        cmp #08
        ifcs
            cmp #$0A
            bcc ?ret
            cmp #$0D
            ifcs
                cmp #$12
                bcc ?ret
                cmp #$16
                ifcs
                    cmp #$19
                    bcc ?ret
                endif
            endif
        endif               ;Here for functions 00,01,02,03,04,05,06,07,0A,0B,0C,12,13,14,15,19,1A,1C
        asl A           ; x2
        tay             ;Y = offset into functbl
        lda t_immque,X
        clc 
        adc #$10
        pha 
        lda t_immarg,X
        sta iarg            ;Save it in the Argument holder for later
        lda t_immsid,X
		and #$7F
        tax 
        lda t_commandtype,X
        cmp #rt_sound       ;Was this a sound command?
        ifeq                ;yes
            lda t_commandparam,X        ;Store the ID of this sound
            sta imm_id      
            pla 
            tax 
            jmp ssimm
        endif
        pla 
?ret    rts

;*****************************************************************
;* Start Sound: This is the main routine used to start playing a *
;*              sound using RPM. It uses the lookup tables based *
;*              on the sound ID initally put in A to fill the    *
;*              correct pokey registers and start playing a snd. *
;*****************************************************************
strtsnd sta cur_snd     			;Currend Sound ID# here
        tay 
        ldx t_tunestarts,Y          ;Load the start offset into the tunetablele
        ; lda t_replacement,Y         ;This happens to always be 0xff in TW = Always Replace
        ; ifeq
            ; lda cur_snd 
            ; ldy #NUM_CHANNELS-1
            ; begin
                ; cmp ch_sid,Y        ;See if this sound is playing
                ; ifeq                ;yes
                    ; lda ch_pri,Y        ;Still Playing?
                    ; ifne
                        ; rts         		;Yes, get outta here!
                    ; endif
                    ; lda cur_snd     	;Next data for loop
                ; endif
                ; dey         ;Loop it!
            ; miend   
        ; endif
        begin
            stx sindex              ;Store away our start sindex for later
            ldy #NUM_CHANNELS-1
            begin
                lda ch_pri,Y
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
            sei 
            ldy ch_pchan-$10,X         ;X = Channel + $10
            ifne
                dey 
                sec                 ; * 2 + 1
                rol A
                cmp ch_pri,Y
            else_cs
                plp 				;new sounds is not higher priority, just leave now
                sec 
                rts
            endif   
            ldx sindex          	;Get Back sindex Index
            lda preg_chanp,Y
            sta ch_mys_4b,X
            plp 
            ldx sindex          	;Get Back start sindex Index
            ;Found an open Channel @ Y
            ;Initialize starting values
openchan    lda #DEFAULT_VOLUME 	;Default Volume Level
            sta ch_vol,Y
            lda #RATE_DEFAULT
            sta ch_rate,Y
            lda #CHCTL_NOPOLY		;Poly = PURE TONE (default), Zero volume
            sta ch_ctrlmask,Y
            lda #$FF
            sta ch_mys_48,Y
            lda #00
            sta ch_mys_49,Y
            sta preg_chanp,Y
            sta ch_key,Y
            sta ch_localpc,Y
            sta ch_ratedif_l,Y
            sta ch_ratedif_h,Y
            sta ch_synthmode,Y
            sta ch_loopcount,Y
            sta ch_synthfadj_l,Y
            sta ch_synthfadj_h,Y
            sta ch_jmppc_h,Y
            sta ch_mys_3c,Y
            sta ch_mys_3b,Y
            sta ch_freqdidx,Y
            sta ch_freq_lcnt,Y
            sta ch_ctrldidx,Y
            sta ch_ctrl_lcnt,Y
            lda # lbyte(defaultampenv)
            sta ch_ctrl_l,Y
            sta ch_freq_l,Y
            lda # ubyte(defaultampenv) 
            sta ch_ctrl_h,Y
            sta ch_freq_h,Y
			
            lda t_priority,X        ;Get our new priority
            sec 
            rol A                   ; * 2 + 1
            sta ch_pri,Y            ;Store it 
            lda t_schan,X           ;Get the channel requested
            clc 
            adc #$10                ; + 0x10
            sta ch_pchan            ;Store it too
            lda t_stune,X           ;Get this sounds tune data offset
            tax 
			lda t_tune_l,X
			sta ch_tuneptr_l,Y
			lda t_tune_h,X
			sta ch_tuneptr_h,Y
            ldx sindex
            ; lda t_eflag,X       ;See if this sound needs to clear an Event Flag
            ; ifpl
                ; sta ch_eflg,Y       ;Save in this channels data, owns this flag
                ; tax 
                ; lda #00
                ; sta eflg0,X     ;Clear the Flag
            ; endif
            ldx ch_pchan
            php 
            sei 
?ssl45      stx ch_pchan
            lda preg_chanp,X		;Is something already playing here?
            ifne
                tax 				;rebias to sound channel index
                dex 
                lda ch_pri,X
                ora #01				;+1 for comparison
                cmp ch_pri,Y
                bcc ?ssl45			;Nope, check next slot
                ifne
                    inx 
                    txa 
                    bcs ?ssl55
                endif
                lda #00
                sta ch_pri,X
                lda preg_chanp,X
?ssl55          sta preg_chanp,Y		;Make it so
                ldx ch_pchan
            endif
            lda cur_snd     	;Get Sound ID back
            sta ch_sid,Y        ;Store it in the Channel Defines
            iny 
            tya 
            sta preg_chanp,X		;Offically book it now! This is POKEY register indexed ($00-01F)
            plp 
            ldy sindex
            ldx t_tunecont,Y    ;Does this sound continue into another?
        eqend
        rts

#IF ___oki == 1

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




#IF ___tlk == 1

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
functbl .word setslvl-1     ;00 - Set Silent/Attract Level
        .word sceflg-1      ;01 - Set/Reset E-Flag
        .word callf-1       ;02 - Call Subroutine
        .word killp-1       ;03 - Kill Sound by Priority
        .word killid-1      ;04 - Kill Sound by ID
        .word killch-1      ;05 - Kill Sound by Device
        .word strtsnd-1     ;06 - Start Sound
        .word repbyte-1     ;07 - Report Value
#IF ___tlk == 1
        .word talkit-1      ;08 - Talk LPC
#ELSE
#IF ___oki == 1
		.word okicmd-1
#ELSE
		.word nocmd
#ENDIF
#ENDIF
        .word immexe-1      ;09 - Execute Immediate Mode Command


nocmd	rts
;************************************************
; RPM Main Proc - processes the sound through 
; the macro driven commands.
;************************************************
; X = Channel Offset (0-F)
;************************************************
rpmmain 
        lda ch_pri,X            ;If Priority was #00, then just leave, never plays
        beq ?rpmex
        ldy ch_sid,X            ;See if a sound has been assigned here
        iny 
        ifeq
            tya 
?rpmex      jmp sifin               ;If the ch_sid was #$FF, then leave
        endif
        and #01                 ;Check priority flag LSB
        sta po_even_odd			;Save the Even/Odd flag for the rest of the RPM code
        tay 
        lda ch_pri,X
        sta po_4c1,Y
        lda ch_tuneptr_l,X      ;Pointer to data stream for this sound LSB
        sta r_sdptr         	;Save the pointer in RAM LSB
        lda ch_tuneptr_h,X  	;Pointer to data stream for this sound MSB
        sta r_sdptr+1         	;Save the pointer in RAM MSB
        sec 
        lda ch_ratedif_l,X     	;Frames to keep this value
        sbc ch_rate,X     		;Subtract our channels sample rate ($40 is default)
        sta ch_ratedif_l,X     	;Save it for next time
        ifcc                    
            dec ch_ratedif_h,X		;did it go zero... yes, take away 1 from MSB of sample rate counter
        else_mi
            jmp rpmm3   			;Not timed out yet
        endif
        
rpmnext ldy ch_localpc,X
        tya 
        clc 
        adc #02					;Always 2 bytes per instruction
        sta ch_localpc,X
        ifcs
            inc ch_tuneptr_h,X		;Update the base high pointer if this flipped over 255
        endif
        lda (r_sdptr,Y)         ;Is this data a sound function selection?
        ifmi                    ;Yes, only if minus
            jsr simmsel             ;special function here
            ifcs
                lda ch_tuneptr_h,X
                sta r_sdptr+1
                bcs rpmnext     ;Always
            endif
            lda #00
            ldy po_even_odd
            sta po_4c1,Y
            jmp rnext
			;*******************
        endif
        ;Here for a standard NOTE, A = positive for a NOTE and ZERO for 1-Time Sound
        sty r_temp2         	;Store away our current pc location for later
        ldy po_even_odd
        ifne
            cmp #00
            beq ?rpm5
        endif
        clc 
        adc ch_key,X			;Add in the current base freq, typically zero
?rpm5   ldy po_even_odd
        ifeq
            pha 
            sec 
            sbc ch_mys_28,X
            sta ch_synthfadj_l,X
            asl A
            lda #$FF
            ifcc
                lda #00
            endif
            sta ch_synthfadj_h,X
            pla 
            sta ch_mys_28,X
            lda #00
            sta ch_mys_29,X
        else 
			asl A				;words in note table
			tay 
			sec 
			lda ch_mys_28,X
			sbc notes-2,Y
			sta ch_synthfadj_l,X
			lda ch_mys_29,X
			sbc notes-2+1,Y
			sta ch_synthfadj_h,X
			lda notes-2,Y
			sta ch_mys_28,X
			lda notes-2+1,Y
			sta ch_mys_29,X
		endif
		ldy r_temp2
        iny 						;Move to second data byte of NOTE($00,XX) command
        lda #00
        sta r_temp2
        lda (r_sdptr,Y)             ;Get next tune data (2nd Byte)
        ifeq                        ;Command Finished
            lda ch_jmppc_h,X            ;Was there a sound JSR
            ifne                        ;yes, pull old data
                sta ch_tuneptr_h,X
                sta r_sdptr+1
                lda ch_jmppc_l,X
                sta ch_tuneptr_l,X
                sta r_sdptr
                lda #00
                sta ch_jmppc_h,X
                lda ch_jmplocpc,X
                sta ch_localpc,X
                jmp rpmnext					;Continue outside execution back at old routine
            endif
            ;Sound is finished if here, clear all old data
sifin       sta ch_pri,X        
            sta ch_vol,X
			sta ch_sid,X			;added this JMA 4/27/2020
            lda preg_chanp,X
            ldx po_4cc
            sta preg_chanp,X
            lda #00
            jmp rnext				;Continue in current outside context
        endif
		;********************************************************************
		;SDATA with data byte in A, these are flags of some sort
		; $80-$04: Valid value which is the 
		; $02	 : ROR into r_temp2, adc into ch_ratedif_h, add into ch_ratedif_l
		; $01	 : When clear, initialize the FREQ and CRTL streams with next pointer data
		;
		; Special Values:
		; $FF - Branch
		; $FE - 
		;********************************************************************
        lsr A
        lsr A
        ror r_temp2				;r_temp2 now has a bit set at $80 from original bit $02 in SDATA followup command
        pha 
        sec 
        lda r_temp2
        adc ch_ratedif_l,X
        sta ch_ratedif_l,X
        pla 
        adc ch_ratedif_h,X
        sta ch_ratedif_h,X
        lda (r_sdptr,Y)         ;Get initial Data Value, this is the 'note' length it seems
        and #01					;Even intial values will trigger a reset of FREQ and CTRL data and move to next of each
        ifeq	
			;****************************************************************
			;FREQ initialization here.... sets up all pointers and init data
			;****************************************************************
			;Clear all these A = 0
            sta ch_synthfadj_l,X
            sta ch_synthfadj_h,X
            sta ch_freq_lastd,X		;set freq values to zero
            sta ch_freq_lastl,X
            sta ch_freq_lasth,X
            sta ch_freq_lcnt,X
			tay						;Clear Y too 
            lda ch_freq_l,X			;Load pointer to frequency table
            sta r_sdptr
            lda ch_freq_h,X
            sta r_sdptr+1
            lda (r_sdptr,Y)         ;Get index freq data which was specified by the SDATA byte 2 data (this value is -1 of actual stored value)
            sta ch_freq_run,X
            iny 					
            ora (r_sdptr,Y)         ;OR with next data
            iny 
            ora (r_sdptr,Y)         ;OR with next data
            ifne
                inc ch_freq_run,X		;Increment our first value +1
                tya
            endif   
            sta ch_freqdidx,X		;This is zero if all 3 bytes were zero
			;******************************************
			;CTRL Initialization here
			;******************************************
            lda #00
            tay 
            sta ch_ctrl_lastv,X
            sta ch_ctrl_lcnt,X
			;Point to base of control table
            lda ch_ctrl_l,X
            sta r_sdptr
            lda ch_ctrl_h,X
            sta r_sdptr+1
            lda (r_sdptr,Y)         ;Loading Initial Control Register data (one less, incremented later) here...
            sta ch_ctrl_run,X
            iny 
            ora (r_sdptr,Y)         ;OR with next eq data
            ifne
                inc ch_ctrl_run,X		;Add one always in Byte 1
                tya
            endif   
            sta ch_ctrldidx,X		;Zero will be stored here if CTRL data ended with $00,$00 (does this mean it loops back to start?)
            ldy po_even_odd
            ifne
                jmp rnext
            endif
        endif
        lda ch_synthmode,X
        ifeq
            sta ch_synthfadj_l,X
            sta ch_synthfadj_h,X
        endif
rpmm3   lda po_even_odd
        ifne
            lda ch_mys_28,X
            ora ch_mys_29,X
            ifeq
                jmp rnext
            endif           
        endif
        lda framecnt
        and #06
        ifeq
            lda ch_synthfadj_h,X
            asl A
            ror ch_synthfadj_h,X
            ror ch_synthfadj_l,X
        endif
		;*******************************
		; Process Frequency Data Stream
		;*******************************
        lda ch_freq_l,X
        sta r_sdptr
        lda ch_freq_h,X
        sta r_sdptr+1
        ldy ch_freqdidx,X
        ifeq
            tya 
            clc
        else    
            dec ch_freq_run,X			;Initial Data +1 is here, now one less (this is the FREQ countdown)
            bne ?rm17
            begin						;Here if Freq Counter was Zero (trigger)
                begin
                    begin
                        iny 
                        lda (r_sdptr,Y)         ;Get next freq byte, this is the 'delta' data byte
                        sta ch_freq_run,X		;Save it to channel RAM
                        iny 
                        cmp #$FF				;Was it $FF - this indicates a jump
                        bne ?rm15
                        lda ch_freq_lcnt,X		;Get last jump count
                        beq ?ldjcnt				;If it was zero then this is first time, load count
                        dec ch_freq_lcnt,X
                        bne ?rm14
                        iny 
                    eqend
?ldjcnt             lda (r_sdptr,Y)         ;Get next byte... this is the JMP loop counter
                    sta ch_freq_lcnt,X		;Save it
?rm14               iny 
                    sec 
                    lda r_sdptr				;Load loop offset (positive numbers go backwards)
                    sbc (r_sdptr,Y)         ;Subtract offset
                    sta r_sdptr				;Save pointer update
                    sta ch_freq_l,X			;Save in RAM too
                ccend
                dec r_sdptr+1
                dec ch_freq_h,X
            csend
?rm15       ora (r_sdptr,Y)         ;Get next tune data
            iny 
            cpy #$F9
            ora (r_sdptr,Y)         ;OR with next data
            bne ?rm16
            sta ch_freqdidx,X
            tay 
            clc 
            ifne
?rm16           tya 
                ifcs
                    sbc #02
                    adc ch_freq_l,X
                    sta ch_freq_l,X
                    lda #01
                    ifcs
                        inc ch_freq_h,X
                    endif
                endif
                sta ch_freqdidx,X
?rm17           ;here if FREQ counter is not zero yet
				lda #00
                sta r_temp2+1
                lda (r_sdptr,Y)         ;Get next FREQ data
                ifmi
                    dec r_temp2+1			;Make MSB negative for loaded byte
                endif
                sta r_temp2				;Finish updating r_temp2 (word) with data
                dey 					;Put Y back
                lda (r_sdptr,Y)         ;Get next FREQ data (LSB)
                asl A
                rol r_temp2
                rol r_temp2+1
                asl A
                rol r_temp2
                rol r_temp2+1
                asl A
                rol r_temp2
                rol r_temp2+1
                clc 
                adc ch_freq_lastd,X
                sta ch_freq_lastd,X
                lda r_temp2				
                adc ch_freq_lastl,X
                sta ch_freq_lastl,X
                sta r_temp2
                lda r_temp2+1
                adc ch_freq_lasth,X
                sta ch_freq_lasth,X
                tay 
                clc 
                lda r_temp2
            endif
        endif
        adc ch_mys_28,X
        sta r_temp2
        tya 
        adc ch_mys_29,X
        tay 
        clc 
        lda r_temp2
        adc ch_synthfadj_l,X
        sta r_temp2
        tya 
        adc ch_synthfadj_h,X
        sta po_even_freq
        ldy po_even_odd
        lda r_temp2
        sta loc_odd_freq,Y
		;*****************************
		; Process CTRL Data Stream
		;*****************************
        lda ch_ctrl_l,X
        sta r_sdptr
        lda ch_ctrl_h,X
        sta r_sdptr+1				;Set up pointers again
        ldy ch_ctrldidx,X			;Do we have data at this index?
        ifeq
?2ctrldone  lda ch_vol,X
            jmp ?ctrldone				;Nope, move on
        endif
        dec ch_ctrl_run,X			;-1.... is it ready now for action?
        ifeq
            begin
                begin
                    begin
                        iny 
                        lda (r_sdptr,Y)         ;Get next CTRL data
                        sta ch_ctrl_run,X
                        iny 					;move next
                        cmp #$FF				;Is this a branch/jump
                        bne ?rm22
                        lda ch_ctrl_lcnt,X		;Yes, load the previous loop counter
                        beq ?rm20
                        dec ch_ctrl_lcnt,X		;If it wasn't zero, then this is weird, subtract one
                        bne ?rm21				;branch if was at != 1 originally
                        iny 
                    eqend
?rm20               lda (r_sdptr,Y)         ;Load the loop counter and save it
                    sta ch_ctrl_lcnt,X
?rm21               iny 
                    sec 
                    lda r_sdptr
                    sbc (r_sdptr,Y)         ;Subtract the offset
                    sta r_sdptr
                    sta ch_ctrl_l,X			;Save it and continue
                ccend
                dec r_sdptr+1
                dec ch_ctrl_h,X
            csend
?rm22       cpy #$F9				;This must be the max length of a CTRL data stream?
            ora (r_sdptr,Y)         ;OR with next data
            ifeq
                sta ch_ctrldidx,X		;CTRL data stream is done, end this sound 
                jmp ?2ctrldone
            endif
            tya 
            ifcs					;If the first CTRL data is $F9 or greater, then do this..
                sbc #02				;This is (FirstByte|SecondByte)-2
                adc ch_ctrl_l,X
                sta ch_ctrl_l,X			;Adjust the outside loop by this amount (allows a loop to update data)
                lda #01
                ifcs
                    inc ch_ctrl_h,X
                endif
            endif
            sta ch_ctrldidx,X		;Save our last CTRL data indexer
        endif
        lda (r_sdptr,Y)         ;Get next CTRL data byte (Addend)
        clc 
        adc ch_ctrl_lastv,X		;Add it to the last value for this control, running sum value
        ifvs
            lda #$80
            ifcc
                lda #$7F
            endif
        endif
        sta ch_ctrl_lastv,X		;Save adjusted control value
        lsr A
        lsr A
        lsr A
        cmp #$10				;Signed upper nybble, FORCED VOLUME
        ifcs
            ora #$F0				;Mask for PCM Data in LSB
        endif					;Examples: $3C -> $07, $CC -> $F7
        clc 
        adc ch_vol,X			;Add base volume level back in
        ifmi
            lda #00
        endif
        cmp #$10
        ifcs
            lda #$0F			;If it flipped (over 15), then reset to Max volume
        endif
		;Fall through
?ctrldone   
		ora ch_ctrlmask,X			;Get the TONE bits that define the poly dividers etc
rnext   ldy po_even_odd
        sta po_odd_cont,Y
        cpx #$10
        ifcc
            lda ch_mys_49,X
            sta po_4ce,Y
            lda ch_mys_48,X
            sta po_4d2,Y
        endif
        lda preg_chanp,X
        ifne
            stx po_4cc
            tax 
            dex 
            jmp rpmmain
        endif
        rts 

;***************************************************************
;* Parse the next RPM instruction from the logic loop.
;* 
;* Y = channel offset
;*
;* If Carry SET on return, then channels is configured for 
;* 16-Bit Frequency Register values.
;***************************************************************
rpm_inst    
        tya 				;Preserve Y
        pha 
        iny                 ;Goto next index first (jump head, will go back down below)
        lda #00             ;Clear all of these parameters by default
        sta po_4c4
        sta loc_even_cont
        sta po_4c1
        sta po_4c2
        sta po_even_cont
        sta po_odd_cont
        sta po_4ce
        sta po_4cf
        sta po_4d0
        lda #$FF            ;Set to -1 for these
        sta po_4d2
        sta po_4d3
        sta po_4d4
        ldx ch_mys_4b-$10,Y		;Data has $10 added
        ifne					;If not zero, then we have something to do
            dex 
            sty po_4cc
            jsr rpmmain
            lda po_4c1
            sta po_4c4
            lsr A
            cmp r_sysi
            lda #00
            ifcs
                lda po_4ce
                sta po_4d0
                lda po_4d2
                sta po_4d4
                lda po_odd_cont
            endif
            sta loc_even_cont
            lda loc_odd_freq
            sta loc_even_freq
            lda #00
            sta po_4c1
            sta po_4c2
            sta po_even_cont
            sta po_odd_cont
            sta po_4ce
            sta po_4cf
            lda #$FF
            sta po_4d2
            sta po_4d3
        endif
        pla 
        tay 
        ldx ch_mys_4b-$10,Y			;Data has $10 added
        ifne
            dex 
            sty po_4cc
            jsr rpmmain
        endif
        lda po_4c1
        cmp po_4c2
        ifcc
            lda po_4c2
        endif
        lsr A
        cmp r_sysi
        ifcc
            lda #00
            sta po_odd_cont
            sta po_even_cont
            sta po_4ce
            sta po_4cf
            lda #$FF
            sta po_4d2
            sta po_4d3
        endif
        lda po_4c1
        cmp po_4c4
        ifcs
            sta po_4c4
        endif
        lda po_4c2
        cmp po_4c4
        ifcs
			;Carry Set: 16-Bit Audio Config
            lda #00
            sta po_odd_cont
            lda po_4d3
            and po_audctl_mask
            sta po_audctl_mask
            lda po_4cf
            ora po_audctl
            ;rts
		else
			;Carry Clear: 8-Bit Audio Config
			lda loc_even_freq
			sta po_even_freq        ;This is the data that will be written to the EVEN Channel AUDF register
			lda loc_odd_freq
			sta po_odd_freq         ;This is the data that will be written to the ODD Channel AUDF register
			lda loc_even_cont
			sta po_even_cont        
			lda po_4d2
			and po_4d4
			and po_audctl_mask
			sta po_audctl_mask
			lda po_4ce
			ora po_4d0
			ora po_audctl
		endif
		;Carry comes out too
        rts 

;**************************************************************
    .sbttl "Pokey Dump"
;**************************************************************
;* This routine will dump data specific to a physical pokey   
;* chip. The base address of the pokey chip to update is in   
;* r_pokaddr.                                       
;*                                                            
;* Inputs: X = Pokey Chip Index                               
;* Pokey Base Address: r_pokaddr
;* Channel Offset: po_choff
;**************************************************************
updpok  lda #00
        sta po_audctl
        lda #$FF
        sta po_audctl_mask  ;Unmask all bits here
        txa 
        pha             ;Push pokey index for later
        lda choffl,X    ;Load this chips channel offset
        pha             ;Push for later
        tay 
        iny 
        iny             ;Y = channel offset + 2
        jsr rpm_inst
        ifcs
            ora #AUDCTL_CH3_179+AUDCTL_34_16B   ;#$28
        endif
        sta po_audctl
        ldy #04             
        lda po_odd_freq
        sta (r_pokaddr,Y)       ;Pokey Channel 3 Freq
        iny 
        iny 
        lda po_even_freq
        sta (r_pokaddr,Y)       ;Pokey Channel 4 Freq
        dey 
        lda po_odd_cont
        sta (r_pokaddr,Y)       ;Pokey Channel 3 Control
        iny 
        iny 
        lda po_even_cont
        sta (r_pokaddr,Y)       ;Pokey Channel 4 Control
		
        pla                     ;Pull pokey channel offset
        tay                     ;Channel Offset
        jsr rpm_inst
        ifcs
            ora #AUDCTL_CH1_179+AUDCTL_12_16B   ;$50    
        endif
        sta po_audctl
        pla                 ;Pull pokey chip index
        tax 
        lda po_audctl
        and po_audctl_mask
        ldy #POKEY_AUDCTL       ;Offset to the AUDCTL Register for this entire POKEY
        sta (r_pokaddr,Y)
        ldy #00
        lda po_odd_freq
        sta (r_pokaddr,Y)       ;Pokey Channel 1 Freq
        iny 
        iny 
        lda po_even_freq
        sta (r_pokaddr,Y)       ;Pokey Channel 2 Freq
        dey 
        lda po_odd_cont
        sta (r_pokaddr,Y)       ;Pokey Channel 1 Control
        iny 
        iny 
        lda po_even_cont
        sta (r_pokaddr,Y)       ;Pokey Channel 2 Control
        rts

;****************************************
;* This routine takes the index passed  *
;* in X and sets up r_pokaddr to hold   *
;* the address that updpok will update. *
;****************************************   
dopokey lda pokaddl,X       ;Get Pokey LSB
        sta r_pokaddr
        lda pokaddh,X       ;Get Pokey MSB
        sta r_pokaddr+1
        lda choffh,X        ;Load the channel offset for later
        sta po_choff
        jmp updpok      	;Update the selected pokey!

;****************************************
;* Run a sound immediate command.         
;****************************************       
simmsel   
        cmp #($80+_isccount)		;#isc_clctl+1    ;Base of 0x80(must be minus to be a function call) 
        ifcs                ;With a limit of functions (0x1C)
            jmp sifin      ;Not a function selector
        endif
        iny 
        stx r_temp2
        asl A
        tax 
        lda sfunctbl+1,X
        pha 
        lda sfunctbl,X
        pha 
        ldx r_temp2
        lda (r_sdptr,Y)     ;Get original data
        ldy ch_tuneptr_h,X
        sty r_sdptr+1
        sec 
        rts 
        
;*******************************************
;* Prepares to call a sound immediate fuction
;*
;* Inputs:  Y = Method Select
;*          X = Queue + 0x10
;*******************************************
ssimm    
        php 
        sei 
        begin
            lda preg_chanp,X
            beq ?ms10
            tax 
            dex 
            lda ch_sid,X    
            cmp imm_id
        eqend
        jsr dosimm
?ms10   plp 
        rts

;************************************
; Start an immediate-mode function
;   command in Y
;************************************        
dosimm  lda sfunctbl+1,Y
        pha 
        lda sfunctbl,Y
        pha 
        lda iarg    ;get the arg if passed too
        rts

;************************************************************************
;* Special Sound Functions - These routines can be called from the main 
;* tune definitions to alter the normal sounds.                         
;*                                                                      
;* All special functions will return with carry set or not set. 
;************************************************************************
_isccount   = 0
_iscbase    = $

#define ISCMD(name,entry)   \name .equ $80 +_isccount
#defcont                    \_isccount .set _isccount + 1
#defcont                    \ .word entry-1
#defcont                    \_iscbase .set _iscbase + 2

#define SETSNDFREQ(envdata)    \ .db isc_freq,envdata
#define SETSNDCONT(envdata)    \ .db isc_ctrl,envdata
#define SETDIST(data)          \ .db isc_ctrlm,data
#define SETDCTL(data)          \ .db isc_dctl,data
#define SETBVOL(data)          \ .db isc_sebvol,data
#define ADDBVOL(data)          \ .db isc_adbvol,data
#define SETBFREQ(data)         \ .db isc_sbfreq,data
#define ADDBFREQ(data)         \ .db isc_abfreq,data
#define SETBFREQD(zpadd)       \ .db isc_sbfreqd,zpadd
#define ADDBFREQD(zpadd)       \ .db isc_abfreqd,zpadd
#define SETNRATE(data)         \ .db isc_srate,data
#define SJSR(data)             \ .db isc_sjsr,data
#define SLOOPSTART(data)       \ .db isc_sloops,data
#define SLOOPEND               \ .db isc_sloope,$00
#define SNOATTRACT             \ .db isc_sfunc10,$00
#define NOTE(npitch,ndur)      \ .db npitch,ndur
;#define NOTESH(b1,b2,b3)       \ .db b1+b2,b3
#define ENDSND                 \ .dw $0000

sfunctbl 
    ISCMD(isc_srate,ssetrate)   ;80 - Set Sample Rate Explicitly
    ISCMD(isc_arate,saddrate)   ;81 - Add to Sample Rate
    ISCMD(isc_sebvol,ssebamp)   ;82 - Set Amplitude
    ISCMD(isc_adbvol,sadbamp)   ;83 - Add to Amplitude
    ISCMD(isc_sbfreq,ssetkey)   ;84 - Set Base Frequency 
    ISCMD(isc_abfreq,saddkey)   ;85 - Add to Base Frequency
    ISCMD(isc_freq,sfreqenv)    ;86 - Set the Frequency Envelope?
    ISCMD(isc_ctrl,sampenv)     ;87 - Set the Amplitune Envelope?
    ISCMD(isc_unk1,sfunc08)     ;88 - 
    ISCMD(isc_unk2,sfunc09)     ;89 - 
    ISCMD(isc_ctrlm,ssetctrlm)  ;8A - Set Channel Control Register
    ISCMD(isc_dctl,sdctl)       ;8B - Set Pokey Chip Control Register
    ISCMD(isc_syn,ssynth)       ;8C - Set value of SYNTH mode
    ISCMD(isc_sjsr,sjsr)        ;8D - JSR to another 'Tune'?
    ISCMD(isc_sloops,sloopi)    ;8E - Loop Start
    ISCMD(isc_sloope,sloopj)    ;8F - Loop End
    ISCMD(isc_sfunc10,spriclrb0);90 - ?
    ISCMD(isc_sfunc11,spriadd)  ;91 - ?
    ISCMD(isc_envsh,rpmnext)    ;92 - Not Implemented in this code
    ISCMD(isc_nzper,rpmnext)    ;93 - Not Implemented in this code
    ISCMD(isc_nzperl,rpmnext)   ;94 - Not Implemented in this code
    ISCMD(isc_nzperh,rpmnext)   ;95 - Not Implemented in this code
    ISCMD(isc_repb,srepb)       ;96 - ?
    ISCMD(isc_unk4,sfunc17)     ;97 - ?
    ISCMD(isc_unk5,sfunc18)     ;98 - ?
    ISCMD(isc_jmpback,sjmpback) ;99 - Jump Back a certain distance?
    ISCMD(isc_unk6,rpmnext)     ;9A - Not Implemented in this code
    ISCMD(isc_clctl,sfunc1B)    ;9B - ?
	ISCMD(isc_sbfreqd,ssetkeyd) ;9C - Set Base Frequency from ZPRAM Location
    ISCMD(isc_abfreqd,saddkeyd) ;9D - Add to Base Frequency from ZPRAM Location

sfunc08 ldy ch_mys_3b,X
        ifne
            iny 
            bne ?sf8_5
            sec 
            rts
        endif
        lda ch_mys_3c,X
        sta ch_mys_3b,X
        ldy #NUM_CHANNELS-1
        begin
            cmp ch_mys_3c,Y
            ifeq
                cmp ch_mys_3b,Y
                ifne
                    lda ch_pri,Y
                    bne ?sf8_5
                    lda ch_mys_3c,X
                endif
            endif
            dey 
        miend
        ldy #NUM_CHANNELS-1
        begin
            cmp ch_mys_3c,Y
            ifeq
                lda #$FF
                sta ch_mys_3b,Y
                lda ch_mys_3c,X
            endif
            dey
        miend   
        clc 
        rts 
		
?sf8_5  lda ch_localpc,X
        cmp #02             ;Will this flip upper byte
        ifcc                    ;yes
            dec ch_tuneptr_h,X      ;take it down one
        endif
        dec ch_localpc,X
        dec ch_localpc,X            ;Remove them now
        lda #00
        sta ch_ratedif_l,X
        sta ch_ratedif_h,X
        clc 
        rts 
        
sfunc09 sta ch_mys_3c,X
        lda #00
        sta ch_mys_3b,X
        rts

;Load the Amplitude Envelope Pointer        
sampenv 
		tay 
        lda t_ampenv_l,Y
        sta ch_ctrl_l,X
        lda t_ampenv_h,Y
        sta ch_ctrl_h,X
        sec 
        rts 
        
sfreqenv
        tay 
        lda t_freqenv_l,Y
        sta ch_freq_l,X
        lda t_freqenv_h,Y
        sta ch_freq_h,X
        sec 
        rts
            
saddrate 
        clc 
        adc ch_rate,X
ssetrate
        sta ch_rate,X
        sec 
        rts 
        
sadbamp clc 
        adc ch_vol,X
ssebamp sta ch_vol,X
        sec 
        rts 

;*********************************************
; Pitch Shift - Used to set a static shift
;               (positive only?) that will 
;               be added to any notes played.
;               Handy for the same tune at 
;               different harmonies.
;*********************************************        
saddkey clc 
        adc ch_key,X
ssetkey sta ch_key,X
        sec 
        rts 

;Variant of above except A is a Zero page RAM address that contains 
;the actual base freq value;		
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
        
ssetctrlm
		sta ch_ctrlmask,X
        rts 
        
sdctl 	ldy po_choff
        ifne
            eor po_4ce
            and #09
            eor po_4ce
        endif
        ora ch_mys_49,X
        sta ch_mys_49,X
        rts 
        
sfunc1B eor #$FF
        ldy po_choff
        ifeq
            and ch_mys_48,X
            sta ch_mys_48,X
            rts
        endif
        ora #$F6
        and ch_mys_49,X
        sta ch_mys_49,X
        rts 
        
ssynth  sta ch_synthmode,X
        rts 
    
;************************************************
;* Jump Sound - JSR to another sound (A). Push  
;* current location into jump locations.        
;************************************************
sjsr    ;asl A
        tay 
        lda ch_localpc,X        ;Push current local channel ptr
        sta ch_jmplocpc,X
        lda ch_tuneptr_l,X
        sta ch_jmppc_l,X        ;Put old address low byte
        lda ch_tuneptr_h,X
        sta ch_jmppc_h,X        ;Put old address high byte
        lda t_tune_l,Y         	;Get new address
        sta ch_tuneptr_l,X      ;Push it current
        sta r_sdptr             ;Zero page too
        lda t_tune_h,Y
        sta ch_tuneptr_h,X
        sta r_sdptr+1
        lda #00
        sta ch_localpc,X        ;Reset the local channel ptr
        sec 
        rts 
    
;************************************************
;* Loop Init - Sets the loop counter to value   *
;* of accumulator and pushes position of current*
;* tune location onto the loop pc.              *
;************************************************   
sloopi  sta ch_loopcount,X
        lda ch_localpc,X
        sta ch_looplocalpc,X
        lda ch_tuneptr_l,X
        sta ch_looppc_l,X
        lda ch_tuneptr_h,X
        sta ch_looppc_h,X
        rts 

;************************************************
;* Loop Jump - Will jump to the loop pc location*
;* unless the loopcount is at 0. The loop count *
;* is lowered each call.                        *
;************************************************       
sloopj  dec ch_loopcount,X
        ifne
            lda ch_looplocalpc,X
            sta ch_localpc,X
            lda ch_looppc_l,X
            sta ch_tuneptr_l,X
            sta r_sdptr
            lda ch_looppc_h,X
            sta ch_tuneptr_h,X
            sta r_sdptr+1
        endif
        rts 
        
spriclrb0 
        ldy po_even_odd
        lda #00
        sta po_4c1,Y
        lda ch_pri,X
        and #$FE
        sta ch_pri,X
        ldy #00
        sty po_even_odd
        sta po_4c1,Y
        rts
            
spriadd ldy #01
        sty po_even_odd
        ora ch_pri,X
        sta ch_pri,X
        sta po_4c1,Y
        rts 
        
srepb   jsr repbyte
        sec 
        rts
            
sfunc17 ldy ch_eflg,X           ;See if this channel controls an event flag
        ifpl
            lda eflg0,Y
            ifne
                clc 
                lda #02
                adc ch_localpc,X
                sta ch_localpc,X
                ifcs
                    inc ch_tuneptr_h,X
                endif
            endif
        endif
        sec 
        rts
            
sfunc18 ldy ch_eflg,X           ;See if this channel controls an event flag
        ifpl
            lda eflg0,Y
            ifne
                lda #-1
                sta ch_sid,X        ;Blank the channel sound ID
                clc
            endif
        endif
        rts
        
;*************************************************
;* Jump Back - Go back (A) number of bytes.      *
;*************************************************  
sjmpback    
        sta r_temp2
        lda ch_localpc,X
        sbc r_temp2
        sta ch_localpc,X
        ifcc
            dec ch_tuneptr_h,X
        endif
        sec 
        rts
        
            
pokaddl .byte lbyte(POKEY_0),lbyte(POKEY_1),lbyte(POKEY_2),lbyte(POKEY_3)
pokaddh .byte ubyte(POKEY_0),ubyte(POKEY_1),ubyte(POKEY_2),ubyte(POKEY_3)

choffh  .byte ubyte(POKEY_CH0),ubyte(POKEY_CH1),ubyte(POKEY_CH2),ubyte(POKEY_CH3)
choffl  .byte POKEY_CH0+$10,POKEY_CH1+$10,POKEY_CH2+$10,POKEY_CH3+$10

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
;* Build the Skeleton Table Structures 

t_userfunction  .block  numfcns*2       ;Words  
t_commandtype   .block  numcom
t_commandparam  .block  numcom
t_tunestarts    .block  numsnd
;t_replacement   .block  numsnd
;t_eflag         .block  numtun
t_priority      .block  numtun                                 
t_schan         .block  numtun      
t_stune         .block  numtun      
t_tunecont      .block  numtun
;t_driver		.block  numtun
t_immfunc       .block  numimm
t_immarg        .block  numimm
t_immsid        .block  numimm
t_immque        .block  numimm
t_exceptidx     .block  numcom
;t_extendidx    	.block  numcom
;t_extendptr     .block  numextend   
t_tune_l        .block  numtun  
t_tune_h		.block  numtun
t_freqenv_l     .block  numenv   
t_freqenv_h     .block  numenv       
t_ampenv_l      .block  numenv     
t_ampenv_h      .block  numenv   
t_exceptptr     .block  numexc*2        ;Words    
t_mystery       .block  numexc*2        ;Words


;Speech Tables
#IF ___tlk == 1

t_spdptr        .block  numtlk*2        ;Words
t_spdlen        .block  numtlk*2        ;Words
o_spdptr        = 0

#ENDIF

#IF ___oki == 1

numoki 			= 127				;OKI can take up to 127 sounds (as long as the binary fits)
o_okiptr		= 0
t_okicmd		.block  numoki
t_okiattch      .block  numoki        

#ENDIF


   
;*****************************************************************************
;* Declare internal table variables to initial offset.
;*****************************************************************************
o_userfunction  = 0
o_commandnum    = 0
o_commandparam  = 0
o_tunestarts    = 0
;o_replacement   = 0
;o_eflag         = 0
o_priority      = 0                               
o_queue         = 0     
o_stune         = 0     
o_driver	  	= 0
o_tunecont      = 0
o_immfunc       = 0
o_immarg        = 0
o_immsid        = 0
o_immque        = 0
o_exceptidx     = 0
;o_extendidx		= 0
o_exceptptr     = 0

i_freqenv       = 0
i_ampenv        = 0
i_tune          = 0
tunenum         = 0
tmpptr          = 0
stsndcount      = 0

rpm_pc          = 0 ;Holder for PC

;********************************************************************
;* Default Sound Data:
;********************************************************************
;* These first two datapoints are empty frequency and amplitude
;* envelope data sets. These need to exist as sounds that do 
;* not explicity define envelopes, will load this data by default 
defaultampenv                     
NEWSNDFREQ(sfreq_none)                      
NEWSNDENV(sctrl_none) 
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

    
NEWSNDENV(sctrl_d01) ;$01
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

   
NEWSNDENV(sctrl_d02) ;$02
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

   
NEWSNDENV(sctrl_d03) ;$03
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

   
NEWSNDENV(sctrl_d04) ;$04
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
;NOTES ON CTRL Data Stream here, Full Bit-Depth of CTRL is only 7-bits, divided by 8 to give 4-Bit final resolution
NEWSNDENV(sctrl_hardswell)   
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
;NOTES ON CTRL Data Stream here, Full Bit-Depth of CTRL is only 7-bits, divided by 8 to give 4-Bit final resolution
NEWSNDENV(sctrl_hardswell2)   
    .byte 32,5	;Attack  - Addend of 10 x 8
	.byte -48,-1	;Decay	 - Addend of -32 x 208
	.byte -1,0	;Sustain - None
	.byte 0,0	;Release
	
NEWSNDENV(sctrl_funk)   
    .byte 20,4	;Attack  - Addend of 10 x 8
	.byte -64,1	;Decay	 - Addend of -32 x 208
	.byte -2,8	;Sustain - None
	.byte 0,0	;Release
	
NEWSNDENV(sctrl_funk2)   
    .byte 80,1		;Attack
	.byte -1,12		;Decay
	;.byte 0,12		;Sustain
	.byte -76,1		;Release
	.byte 0,0

NEWSNDENV(sctrl_robot)   
    .byte 16,5	;Attack  - Addend of 16 x 5
	.byte -16,2	;Decay	 - Addend of -32 x 208
	.byte -4,12	;Sustain - None
	.byte 0,0	;Release
	.byte 0,0

;Used in attract mode for the horns
;NOTES ON CTRL Data Stream here, Full Bit-Depth of CTRL is only 7-bits, divided by 8 to give 4-Bit final resolution
NEWSNDENV(sctrl_horn)   
    .byte $08,$0A	;Attack  - Addend of 10 x 8
	.byte $02,$FF	;Decay	 - Addend of -1 x 208
	.byte $FF,$00	;Sustain - None
	.byte $00,$00	;Release

;Not Used - Short Envelope  
NEWSNDENV(sctrl_d05) ;$05
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
        NOTEDEF(Bb_,$A900)	;0x02		-1		A#/Bb 
        NOTEDEF(B_, $9E80)	;0x03 		-1		B 
		;OCTAVE 0
        NOTEDEF(C0, $9680)	;0x04		0		C
        NOTEDEF(Db0,$8F4D)	;0x05		0		C#/Db
        NOTEDEF(D0, $8680)	;0x06		0		D  
        NOTEDEF(Eb0,$7F80)	;0x07		0		D#/Eb
        NOTEDEF(E0, $77C0)	;0x08		0		E		20.60Hz
        NOTEDEF(F0, $7180)	;0x09		0		F
        NOTEDEF(Gb0,$6B40)	;0x0a		0		F#/Gb
        NOTEDEF(G0, $6580)	;0x0b		0		G
        NOTEDEF(Ab0,$5F90)	;0x0c		0		G#/Ab
        NOTEDEF(A0, $5A20)	;0x0d		0		A		27.50Hz 
        NOTEDEF(Bb0,$5580)	;0x0e		0		A#/Bb
        NOTEDEF(B0, $5080)	;0x0f		0		B		30.87Hz
		;OCTAVE 1
        NOTEDEF(C1, $4C10)	;0x10		1		C		32.70Hz
        NOTEDEF(Db1,$47A0)	;0x11		1		C#/Db	34.652Hz
        NOTEDEF(D1, $43C0)	;0x12		1		D
        NOTEDEF(Eb1,$3FF0)	;0x13		1		D#/Eb
        NOTEDEF(E1, $3C56)	;0x14		1		E
        NOTEDEF(F1, $38F0)	;0x15		1		F
        NOTEDEF(Gb1,$35C0)	;0x16		1		F#/Gb
        NOTEDEF(G1, $32C0)	;0x17		1		G
        NOTEDEF(Ab1,$2FC0)	;0x18		1		G#/Ab
        NOTEDEF(A1, $2D28)	;0x19		1		A
        NOTEDEF(Bb1,$2AA0)	;0x1a		1		A#/Bb
        NOTEDEF(B1, $2844)	;0x1b		1		B
		;OCTAVE 2
        NOTEDEF(C2, $2600)	;0x1c		2		C
        NOTEDEF(Db2,$23D0)	;0x1d		2		C#/Db
        NOTEDEF(D2, $21E0)	;0x1e		2		D
        NOTEDEF(Eb2,$1FF4)	;0x1f		2		D#/Eb
        NOTEDEF(E2, $1E2C)	;0x20		2		E  
        NOTEDEF(F2, $1C74)	;0x21		2		F
        NOTEDEF(Gb2,$1AD4)	;0x22		2		F#/Gb
        NOTEDEF(G2, $1958)	;0x23		2		G
        NOTEDEF(Ab2,$17EC)	;0x24		2		G#/Ab
        NOTEDEF(A2, $168A)	;0x25		2		A
        NOTEDEF(Bb2,$154E)	;0x26		2		A#/Bb
        NOTEDEF(B2, $1419)	;0x27		2		B		121.3
		;OCTAVE 3
        NOTEDEF(C3, $12FE)	;0x28		3		C
        NOTEDEF(Db3,$11E4)	;0x29		3		C#/Db
		NOTEDEF(D3, $10ED)	;0x2a 		3		D
		NOTEDEF(Eb3,$0FF3)	;0x2b		3		D#/Eb
		NOTEDEF(E3, $0F0C)	;0x2c		3		E
		NOTEDEF(F3, $0E3B)	;0x2d		3		F
		NOTEDEF(Gb3,$0D6C)	;0x2e		3		F#/Gb
		NOTEDEF(G3, $0CAA)	;0x2f		3		G
		NOTEDEF(Ab3,$0BF3)	;0x30		3		G#/Ab
        NOTEDEF(A3, $0B60)	;0x31		3		A
		NOTEDEF(Bb3,$0AC0)	;0x32		3		A#/Bb
		NOTEDEF(B3, $0A18)	;0x33		3		B
		;OCTAVE 4
		NOTEDEF(C4, $0990)	;0x34		4		C
		NOTEDEF(Db4,$0904)	;0x35		4		C#/Db
		NOTEDEF(D4, $0880)	;0x36		4		D
		NOTEDEF(Eb4,$0804)	;0x37		4		D#/Eb
		NOTEDEF(E4, $0792)	;0x38		4		E
        NOTEDEF(F4, $0720)	;0x39		4		F
		NOTEDEF(Gb4,$06C0)	;0x3a		4		F#/Gb
		NOTEDEF(G4, $0658)	;0x3b		4		G
		NOTEDEF(Ab4,$05F8)	;0x3c		4		G#/Ab
		NOTEDEF(A4, $05A4)	;0x3d		4		A
		NOTEDEF(Bb4,$0554)	;0x3e		4		A#/Bb
		NOTEDEF(B4, $0502)	;0x3f		4		B
		;OCTAVE 5
		NOTEDEF(C5, $04BC)	;0x40		5		C
        NOTEDEF(Db5,$047C)	;0x41		5		C#/Db
		NOTEDEF(D5, $043C)	;0x42		5		D
		NOTEDEF(Eb5,$0400)	;0x43		5		D#/Eb
		NOTEDEF(E5, $03C4)	;0x44		5		E
		NOTEDEF(F5, $0390)	;0x45		5		F
		NOTEDEF(Gb5,$035E)	;0x46		5		F#/Gb
		NOTEDEF(G5, $032C)	;0x47		5		G
		NOTEDEF(Ab5,$02FE)	;0x48		5		G#/Ab
        NOTEDEF(A5, $02D0)	;0x49		5		A
		NOTEDEF(Bb5,$02A8)	;0x4a		5		A#/Bb
		NOTEDEF(B5, $027E)	;0x4b		5		B
		;OCTAVE 6
		NOTEDEF(C6, $025A)  ;0x4c		6		C
		NOTEDEF(Db6,$0237)	;0x4d		6		C#/Db
		NOTEDEF(D6, $0218)	;0x4e		6		D
		NOTEDEF(Eb6,$01F9)	;0x4f		6		D#/Eb
		NOTEDEF(E6, $01DC)	;0x50		6		E
        NOTEDEF(F6, $01C2)	;0x51		6		F
		NOTEDEF(Gb6,$01A8)	;0x52		6		F#/Gb
		NOTEDEF(G6, $0190)	;0x53		6		G
		NOTEDEF(Ab6,$0179)	;0x54		6		G#/Ab
		NOTEDEF(A6, $0163)	;0x55		6		A
		NOTEDEF(Bb6,$014F)	;0x56		6		A#/Bb
		NOTEDEF(B6, $013B)	;0x57		6		B
		;OCTAVE 7
		NOTEDEF(C7, $0129)	;0x58		7		C
        NOTEDEF(Db7,$0118)	;0x59		7		C#/Db
		NOTEDEF(D7, $0107)	;0x5a		7		D
		NOTEDEF(Eb7,$00F7)	;0x5b		7		D#/Eb
		NOTEDEF(E7, $00E8)	;0x5c		7		E
		NOTEDEF(F7, $00DA)	;0x5d		7		F
		NOTEDEF(Gb7,$00CC)	;0x5e		7		F#/Gb
		NOTEDEF(G7, $00C4)	;0x5f		7		G
		NOTEDEF(Ab7,$00BD)	;0x60		7		G#/Ab
        NOTEDEF(A7, $00B7)	;0x61		7		A

        
rpm_end
        
