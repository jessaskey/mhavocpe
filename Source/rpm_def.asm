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
;*  ___TLK  Enables SPEECH (TMS5220 Hardware installed)     
;*  ___OKI  Enables SPEECH (MSM6295 Hardware installed)                  
;*  ___IMM  IMMEDIATE Mode Functions                                          
;*  ___NMI  NMI port control                                                  
;*  ___EXM  support EXCEPTION mode                                            
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


;Initial Values - redefined below
___NPKS	    = 0
___NGIS	    = 0
___DVCNT    = 0
___TOTDV    = 0

;DEVICE TYPES
#DEFINE POKEY 0
#DEFINE PSGI  1


;
;	DEVICE - INDEX,TYPE,ADDRESS
;
#DEFINE DEVICE(v1,v2,v3)        \#IF v2 = POKEY
#DEFCONT			            \DTYP+v1	= 0
#DEFCONT                        \POKEY_+v1 = v3
#DEFCONT                        \POKEY_CH+v1 = (v1*4)
#DEFCONT			            \___NPKS .set ___NPKS+1
#DEFCONT                        \#ENDIF
#DEFCONT                        \#IF v2 = PSGI
#DEFCONT			            \DTYP+v1	= 1
#DEFCONT			            \___NGIS .set ___NGIS+1
#DEFCONT				        \#ENDIF 
#DEFCONT                        \DADR+v1 = v3	  
#DEFCONT                        \___DVCNT .set ___DVCNT+1 


#DEFINE QUADPOKEY(baseaddr)     \DEVICE(0,POKEY,baseaddr+$00)
#DEFCONT                        \DEVICE(1,POKEY,baseaddr+$08)
#DEFCONT                        \DEVICE(2,POKEY,baseaddr+$10)
#DEFCONT                        \DEVICE(3,POKEY,baseaddr+$18)

#include "pokey4.ah"
;Quad Pokey
QUADPOKEY(qpokey)           
;
;	********  DEVICE REGISTER OFFSETS  ******
;
#IF ___NPKS != 0
P.FRQ	    =	$00
P.VOL	    =	$01
#IF ___PIV = 0
P.AUDCTL    =	$08
P.SKCTL	    =	$0F
P.POTGO	    =	$0B
P.RND	    =	$0A
P.ALLPOT    =	$08
#ELSE
P.AUDCTL    =	$20		; different memory-map for IV QUAD POKEY
P.SKCTL	    =	$27     ; uses 'compact' addressing mode
P.POTGO	    =	$23
P.RND       =   $22
P.ALLPOT    =   $20
#ENDIF
#ENDIF
#IF ___NGIS != 0
G.AMP	    =	$08
G.FRQ	    =	$00
G.ENABL	    =	$07
G.NZPER	    =	$06
G.ENVPER    =	$0B
G.ENVSHP    =	$0D
#ENDIF

;
;	Define the numbers of the functions that are
;	illegal in IMMEDIATE mode
;
#IF ___IMM
F.SCH	=	$08
F.RCH	=	$09
F.SUB	=	$0D
F.16B	=	$11
F.RPT	=	$16
F.BR	=	$19
ST.BRK	=	$10
STK.PSW	=	$67         ;103d
#ENDIF

;*****************************************************************************
;* Declare internal table variables to initial offset.
;*****************************************************************************
o_userfunction  = 0
o_commandnum    = 0
o_commandparam  = 0
o_tunestarts    = 0
o_replacement   = 0
o_eflag         = 0
o_priority      = 0                               
o_schann        = 0     
o_stune         = 0     
o_driver	  	= 0
o_tunecont      = 0
o_immfunc       = 0
o_immarg        = 0
o_immsid        = 0
o_immschn       = 0
o_exceptidx     = 0
;o_extendidx	= 0
o_exceptptr     = 0
o_spdptr        = 0
o_okiptr		= 0

i_freqenv       = 0
i_ampenv        = 0
i_tune          = 0
tunenum         = 0
tmpptr          = 0
stsndcount      = 0

rpm_pc          = 0 ;Holder for PC


;Input Bit Definitions
alphaxmtd   =   $01     ;Alpha Processor Transmitted
alpharcvd   =   $02     ;Alpha Processor Ready to Recieve
tirdyflg    =   $04     ;TI Speech Chip Ready

;.include "pokey4.ah"

audctl  =   POKEY_0+P.AUDCTL    
allpot  =   POKEY_0+P.ALLPOT   
potgo   =   POKEY_0+P.POTGO    
;random  =   POKEY_0+P.RND
;random1  =  POKEY_1+P.RND   

NOTCLK	=	64d		; steps to decrement duration each IRQ
;				; #ms/IRQ * 16.
;
;
;	***  ADDRESSES ***
;
INTACK	=	$1000		; interrupt-acknowledge strobe address
;

   

;ti_stat has the folling values                    
TISTAT_IDLE		= $00	; 00 = Idle, check for new command in queue
TISTAT_START 	= $80	; 80 = Starting - sends a $60 (Speak External Command) to the TI
TISTAT_WAIT		= $11	; 11 = Here after data runs out, continues to deincrement - Pads silence before next speech starts
TISTAT_SENDING	= $FF	; FF = Sending Data(stays here until data runs out)

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

;******************************************************************************
; REPLACEMENT = Should this sound replace another
;******************************************************************************
REPL        =   -1      ;Always Replace
NOREPL      =   0       ;Don't replace (let sound continue if playing and this
                        ;sound will be thrown away if already playing?)

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
; ;*****************************************************************************
; ;* NEWMXSNDFREQ defines a pointer to a block of data used for the frequency 
; ;*            envelope for the sound - THIS IS USED FOR THE MAX BACKGROUND 
; ;*            SOUND BUFFER.
; ;*****************************************************************************
; #define  NEWMXSNDFREQ(id) \tmpptr .set *
; #defcont				\maxsndfbufd = *
; #defcont                \ .org t_freqenv_l+i_freqenv
; #defcont				\ .byte (maxsndfbuf&$FF)
; #defcont                \ .org t_freqenv_h+i_freqenv
; #defcont                \ .byte (maxsndfbuf/$100)
; #defcont                \id = i_freqenv
; #defcont                \i_freqenv .set i_freqenv + 1
; #defcont                \ .org tmpptr
; #defcont                \#IF i_freqenv > (numenv)
; #defcont			    \	.error "RPM: Too many frequency envelopes defined. Increase limit."
; #defcont				\#ENDIF 
; ;*****************************************************************************
; ;* NEWSNDAMP defines a pointer to a block of data used for the amplitude 
; ;*            envelope for the sound.  
; ;*****************************************************************************
#define  NEWSNDAMP(id) \tmpptr .set *
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

; #define  NEWMXSNDENV(id) \tmpptr .set *
; #defcont				\maxsndcbufd = *
; #defcont                \ .org t_ampenv_l+i_ampenv
; #defcont				\ .byte (maxsndcbuf&$FF)
; #defcont                \ .org t_ampenv_h+i_ampenv
; #defcont                \ .byte (maxsndfbuf/$100)
; #defcont                \id = i_ampenv
; #defcont                \i_ampenv .set i_ampenv + 1
; #defcont                \ .org tmpptr
; #defcont                \#IF i_ampenv > (numenv)
; #defcont			    \	.error "RPM: Too many amplitude envelopes defined. Increase limit."
; #defcont				\#ENDIF 
;************************************************************************** 
;#define  CREATEGLOBAL(gname) \gname = o_commandnum
;#defcont                     \.export gname

#define  SETCOMMAND(name,value)     \tmpptr .set *
#defcont                            \.export name
#defcont                            \name = o_commandnum
#defcont                            \ .org t_commandtype+o_commandnum
#defcont                            \ .print "DEFINE COMMAND: " \ .print o_commandnum \ .print "=" \ .print (value)
#defcont                            \ .byte value
#defcont                            \o_commandnum .set o_commandnum+1 
#defcont                            \ .org tmpptr
#defcont                            \#IF o_commandnum > (numcom)
#defcont			                \	.error "RPM: Too many command types defined. Increase 'numcom' limit."
#defcont				            \#ENDIF  

#define  SETCMDPARAM(value) \tmpptr .set *
#defcont                    \ .org t_cmdparam+o_commandparam
#defcont                    \ .print "COMMAND PARAM: " \ .print o_commandparam \ .print "=" \ .print (value)
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
#defcont                    \ .print "EXCEPTION: " \ .print o_commandparam \ .print "=" \ .print (value)
#defcont                    \ .byte value
#defcont                    \o_exceptidx .set o_exceptidx+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_exceptidx > (numcom) 
#defcont			        \	.error "RPM: Too many exceptions defined across all commands. Increase 'numcom'."
#defcont				    \#ENDIF 

#define  SETEXPPRT(value)   \tmpptr .set *
#defcont                    \ .org t_exceptptr+(o_exceptptr*2)
#defcont                    \ .print "EXCEPTION POINTER: " \ .print o_commandparam \ .print "=" \ .print (value)
#defcont                    \ .word value-1
#defcont                    \o_exceptptr .set o_exceptptr+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_exceptptr > numexc
#defcont			        \	.error "RPM: Too many command exceptions pointers defined. Increase 'numexc' to allow more WORD pointers."
#defcont				    \#ENDIF 

#define  INSERTFUNC(name)   \tmpptr .set *
#defcont                    \ .org t_userfunction+(o_userfunction*2)
#defcont                    \ .print "FUNCTION: " \ .print name
#defcont                    \ .word name-1
#defcont                    \o_userfunction .set o_userfunction+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_userfunction > numfcns
#defcont			        \	.error "RPM: Too many user functions defined. Increase limit."
#defcont				    \#ENDIF

#define INSERTTUNEST(name)  \tmpptr .set *
#defcont                    \ .org t_tunestarts+o_tunestarts
#defcont                    \ .print "TUNE START: " \ .print name
#defcont                    \ .byte tunenum
#defcont                    \o_tunestarts .set o_tunestarts+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_tunestarts > numsnd
#defcont			        \	.error "RPM: Too many tune starts defined. Increase limit."
#defcont				    \#ENDIF 

#IF ___REP = 1
#define INSERTREPL(thisrep) \tmpptr .set *
#defcont                    \ .org t_replacement+o_replacement
#defcont                    \ .print "REPLACEMENT: " \ .print thisrep
#defcont                    \ .byte thisrep
#defcont                    \o_replacement .set o_replacement+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_replacement > (numsnd)
#defcont			        \	.error "RPM: Too many replacements defined. Increase limit. $1"
#defcont				    \#ENDIF 
#ENDIF

#IF ___FLG = 1
#define INSERTEFLAG(flag)   \tmpptr .set *
#defcont                    \ .org t_eflag+o_eflag
#defcont                    \ .print "FLAGS: " \ .print flag
#defcont                    \ .byte flag
#defcont                    \o_eflag .set o_eflag+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_eflag > (numtun)
#defcont			        \	.error "RPM: Too many flags defined. Increase limit."
#defcont				    \#ENDIF 
#ENDIF

#define INSERTPRIORITY(pri) \tmpptr .set *
#defcont                    \ .org t_priority+o_priority
#defcont                    \ .print "PRIORITY: " \ .print pri
#defcont                    \ .byte pri
#defcont                    \o_priority .set o_priority+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_priority > (numtun)
#defcont			        \	.error "RPM: Too many priorities defined. Increase limit."
#defcont				    \#ENDIF 

#define INSERTCHANNEL(schn) \tmpptr .set *
#defcont                    \ .org t_schan+o_schann
#defcont                    \ .print "CHANNEL: " \ .print schn
#defcont                    \ .byte schn
#defcont                    \o_schann .set o_schann+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_schann > (numtun)
#defcont			        \	.error "RPM: Too many channels defined. Increase limit."
#defcont				    \#ENDIF 

#define SETTUNEID(id)       \tmpptr .set *
#defcont                    \ .org t_stune+o_stune
#defcont                    \ .print "TUNE ID: " \ .print id
#defcont                    \ .byte id
#defcont                    \o_stune .set o_stune+1
#defcont                    \ .org tmpptr
#defcont                    \#IF o_stune > (numtun)
#defcont			        \	.error "RPM: Too many Tune IDs defined. Increase limit."
#defcont				    \#ENDIF 

#define INSERTTCONT(tcont)  \tmpptr .set *
#defcont                    \ .org t_tunecont+tunenum-1
#defcont                    \ .print "CONTINUE: " \ .print tcont
#defcont                    \#IF tunenum!=0
#defcont                    \ .byte tcont, $0
#defcont                    \#ENDIF
#defcont                    \ .org tmpptr

#IF ___TLK = 1
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

#IF ___OKI = 1
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

#IF ___IMM = 1

#define INSERTIMMFUNC(ifunc)    \tmpptr .set *
#defcont                        \ .org t_immfunc+(o_immfunc)
#defcont                        \ .print "IMM FUNCTION: " \ .print o_immfunc \ .print "=" \ .print (ifunc&$7F)
#defcont                        \ .db (ifunc&$7F)
#defcont                        \o_immfunc .set o_immfunc+1
#defcont                        \ .org tmpptr
#defcont                        \#IF o_immfunc > (numimm)
#defcont			            \	.error "RPM: Too many Immediate Functions created. Increase limit."
#defcont				        \#ENDIF 

#define INSERTIMMARG(iarg)    \tmpptr .set *
#defcont                        \ .org t_immarg+(o_immarg)
#defcont                        \ .print ": IMM ARG" \ .print o_immarg \ .print "=" \ .print (iarg)
#defcont                        \ .db iarg
#defcont                        \o_immarg .set o_immarg+1
#defcont                        \ .org tmpptr
#defcont                        \#IF o_immarg > (numimm)
#defcont			            \	.error "RPM: Too many Immediate Arguments created. Increase limit."
#defcont				        \#ENDIF 

#define INSERTIMMSID(iside)  \tmpptr .set *
#defcont                        \ .org t_immsid+(o_immsid)
#defcont                        \ .print ": IMM SID" \ .print o_immsid \ .print "=" \ .print (iside)
#defcont                        \ .db iside
#defcont                        \o_immsid .set o_immsid+1
#defcont                        \ .org tmpptr
#defcont                        \#IF o_immsid > (numimm)
#defcont			            \	.error "RPM: Too many Immediate Sound ID created. Increase limit."
#defcont				        \#ENDIF 

#define INSERTIMMCHN(ischn)  \tmpptr .set *
#defcont                        \ .org t_immschn+(o_immschn)
#defcont                        \ .print ": IMM CHN" \ .print o_immschn \ .print "=" \ .print (ischn)
#defcont                        \ .db ischn
#defcont                        \o_immschn .set o_immschn+1
#defcont                        \ .org tmpptr
#defcont                        \#IF o_immschn > (numimm)
#defcont			            \	.error "RPM: Too many Immediate Channels created. Increase limit."
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
#IF ___FLG = 1 
#defcont                \ INSERTEFLAG(eflag)
#ENDIF
#defcont                \ INSERTPRIORITY(priority)
#defcont                \ INSERTCHANNEL(queue)
#defcont                \ SETTUNEID(tuneID)
#defcont                \ INSERTTCONT(0)
#IF ___REP = 1  \
#defcont                \ INSERTREPL(replacement)
#ENDIF
#defcont                \ SETEXPIDX(no_exe)
;#defcont				\ SETEXTENDIDX(extension)
#defcont                \tunenum .set tunenum+1
#defcont                \stsndcount .set stsndcount+1

#define  CONT(priority,eflag,queue,tuneID) \ INSERTPRIORITY(priority)
#IF ___FLG = 1 
#defcont                \ INSERTEFLAG(eflag)
#ENDIF
#defcont                \ INSERTCHANNEL(queue)
#defcont                \ SETTUNEID(tuneID)
#defcont                \ INSERTTCONT(tunenum)
#IF ___REP = 1
#defcont                \ INSERTREPL(REPL)
#ENDIF
#defcont                \ SETEXPIDX(no_exe)
;#defcont				\ SETEXTENDIDX(no_ext)
#defcont                \tunenum .set tunenum+1

#define  REPORT(name,value) \ SETCOMMAND(name,rt_report)
#defcont                    \ SETCMDPARAM(value)
#defcont                    \ SETEXPIDX(no_exe)
;#defcont					\ SETEXTENDIDX(no_ext)

#IF ___TLK == 1
    #define  TALK(name,spstart,spend) \ SETCOMMAND(name,rt_speech)
    #defcont                          \ SETCMDPARAM(o_spdptr)
    #defcont                          \ INSERTTALK(spstart,spend)
;    #defcont                          \ SETEXPIDX(no_exe)
;	#defcont						  \ SETEXTENDIDX(no_ext)
#ENDIF

#IF ___OKI == 1
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

;#define  INSCMD(name,scmd,arg) 

#IF ___IMM != 0
    #define  IMMED(immname,immident,immchan,immfunc,immarg) \ SETCOMMAND(immname,rt_immed)
    #defcont                                    \ SETCMDPARAM(o_immfunc)
    #defcont                                    \ INSERTIMMFUNC(immfunc)
    #defcont                                    \ INSERTIMMARG(immarg)
    #defcont                                    \ INSERTIMMSID(immident)
    #defcont                                    \ INSERTIMMCHN(immchan)
#ENDIF

#IF ___EXM != 0
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
					

;*************************************************************
; Function Definitions
;*************************************************************
_maxfcn   = $1E

_nfcn = 0
_fcnbase  = $

#define FUNC(name,entry)    \name .equ $80+_nfcn
#defcont                    \_nfcn .set _nfcn + 1
#defcont                    \ .word entry-1
#defcont                    \_fcnbase .set _fcnbase + 2
#defcont                    \#IF _nfcn > _maxfcn \ .error "Too many function definitions defined. Increase table size." \#ENDIF

;CONDITIONAL FUNCTIONS, only defined if a configuration flag is != 0
#DEFINE CFUNC(configoption,fenum,ecall,dcall)      \#IF configoption != 0
#DEFCONT                                     \ FUNC(fenum,ecall)
#DEFCONT                                     \#ELSE
#DEFCONT                                     \ FUNC(fenum,dcall)
#DEFCONT                                     \#ENDIF

;Conditional with two configurations logically ORed
#DEFINE CFUNCOR(config1,config2,fenum,fcall) \#IF (config1 | config2) != 0
#DEFCONT                                     \ FUNC(fenum,fcall)
#DEFCONT                                     \#ELSE
#DEFCONT                                     \ FUNC(fenum,dcall)
#DEFCONT                                     \#ENDIF

#define TUNEFREQ(envdata)      \ .db f_freq,envdata
#define TUNEAMP(envdata)       \ .db f_amp,envdata
#define SETDIST(data)          \ .db f_ctrlm,data
#define SETDCTL(data)          \ .db f_dctl,data
#define SETBVOL(data)          \ .db f_sebvol,data
#define ADDBVOL(data)          \ .db f_adbvol,data
#define SETBFREQ(data)         \ .db f_sbfreq,data
#define ADDBFREQ(data)         \ .db f_abfreq,data
#define SETBFREQD(zpadd)       \ .db f_sbfreqd,zpadd
#define ADDBFREQD(zpadd)       \ .db f_abfreqd,zpadd
#define SETNRATE(data)         \ .db f_srate,data
#define SJSR(data)             \ .db f_sjsr,data
#define SLOOPSTART(data)       \ .db f_sloops,data
#define SLOOPEND               \ .db f_sloope,$00
#define SBRANCH(data)          \ .db f_braback,data*2
#define SNOATTRACT             \ .db f_8bitv,$00
#define NOTE(npitch,ndur)      \ .db npitch,ndur
;#define NOTESH(b1,b2,b3)       \ .db b1+b2,b3
#define ENDSND                 \ .dw $0000

;*************************************************************
    .sbttl "RPM ZP RAM Allocation"
;*************************************************************
    

irqcnt      .block  1
framecnt    .block  1

#IF ___FLG = 1
FLGCNT = 8
sflags      .block  FLGCNT       ; FLAGS TO START SOUND IF NON-0
#ENDIF

cur_snd     .block  1
xferbuf     .block  2
tuneptr     .block  2           ;POINTER TO START OF CURRENT TUNE
devptr      .block  2           ;POINTER TO START OF CURRENT DEVICE
fasave      .block  2           ;FREQUENCY ADJUST MSB SAVE (COMMON POKEY TEMP)
silevel     .block  1           ;SILENCE LEVEL

sirqcnt		.block 	1
lastoki		.block  1


; Variables used for speech only
#IF ___TLK == 1
ti_addr         .block  2
ti_len          .block  2
ti_stat         .block  1		    
ti_count        .block  1
ti_irqcnt       .block  1
tiquecur        .block  1           ;the current talk command 
tiquelst        .block  1           ;the last talk command index 
tiqueue         .block  ticmdbflen  ;the amount of speech commands that can queue up in the command buffer
tiframe         .block  1           ;

#ENDIF