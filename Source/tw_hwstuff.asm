

;******************************************
; Do the VAXX bonus screen
;******************************************
; This will consist of a bunch of 'states'
; We will store our state in the bottom half
; of the vxstat variable.
;
; 01 - Init Data
; 02 - Vaxx Bonus Showing
; 03 - Destroy Ship Message
; 04 - Destroy Ship Points
; 05 - Tesseract Message
; 06 - Tesseract Points
; 07 - Extra Lives Message
; 08 - Extra Lives Points
; 09 - Total Points message
; 10 - Total Points Display
; 11 - Award Points Rollup 
; 12 - Award Points complete

;We are going to use this variable for pausing
bonpause  = perm5
textbrit  = perm5+1
mntncntl  = perm4
mntncnth  = perm4+1

dovxbonus	
		lda frame 
		and #$3F
		ifeq
			lda vxstat
			and #$0F			;see if this is the first run, if so do the maths
			ifeq
				sta bonbuf			;Final bonus values reset here always
				sta bonbuf+1
				sta bonbuf+2
				inc vxcount			;Increment our Counter if this is != 0, then this is a flag for many routines
				;we will pre-calculate the bonus here actually, it will be displayed later tho
				;add our points to the point buffer, this doesn't change anything in temp7 or temp7+1
				lda gamest      		
				and #$40			;flag $40 tells us sucessful exit
				ifne
					ldx #$01			;Success 100,000	
					lda #$00					
					jsr addbon  
				else
					ldx #$00			;Destroyed but died 50,000	
					lda #$50					
					jsr addbon  
				endif	 
				;do lives
				lda lives			;how many lives
				jsr decimal			;convert from hex to decimal, output as single byte @ temp7
				lda #0
				sta temp7+1			;Clear the upper digits
				jsr dshift
				lda temp7     		;MSB total time count in decimal
				ldx temp7+1
				jsr addbon      	;Add in these too

				lda tokpock
				and #$0F			;Limit to bottom 4 bits (flags)
				jsr howmbits		;A will contain either 0-4 now, up to 4 tokens
				asl A				;x2
				jsr decimal			;convert from hex to decimal, output in temp7 and temp7+1
				lda #0
				sta temp7+1			;Clear the upper digits
				jsr dshift
				lda temp7     		;MSB total time count in decimal
				ldx temp7+1
				jsr addbon      	;Add in these too
				lda #0
				sta bonpause		;Clear the pause
			endif
			lda bonpause
			ifpl
				inc vxstat
				lda vxstat
				and #$0F
				tax
				lda spsnds,X
				ifne
					jsr dosound			;Play a sound if neeeded
				endif
				inc frame			;We do this so init doesn't run again
			endif
		endif
		
		;Controllers
		lda #0
		sta perm1
		begin
			lda perm1
			jsr docontrol
			inc perm1
			lda vxstat
			and #$0F
			cmp perm1
		ccend
		
		lda vxstat
		and #$0F
		cmp #01
		ifeq
			;State 0, bring in text brighness
			lda textbrit
			clc
			adc #$10
			sta textbrit
			ifcs
				lda #$F0
				sta textbrit
			endif
		else
			cmp #$0B		;This is the last state, dim out text
			ifcs
				lda textbrit
				sec
				sbc #$10
				sta textbrit 
				ifcc
					lda #00
					sta textbrit 
				endif
			endif
		endif
		rts

docontrol
		asl A				;words
		tax
		lda	spstats+1,X
		pha
		lda	spstats,X
		pha
		rts	
			
spstats	.dw spwait-1		;Wait Time
		.dw spvaxxb-1		;Vax Bonus
		.dw spdests-1		;Destroy Ship Text
		.dw spdespt-1		;Ship Points
		.dw spexlit-1		;Extra Lives Text
		.dw spexlip-1		;Extra Lives Points
		.dw sptessm-1		;Tesseract Message
		.dw sptessp-1		;Tesseract Points
		.dw sptotbm-1		;Total Bonus Message
		.dw sptotbp-1		;Total Bonus Points
		.dw sptotbc-1		;Total Bonus Countdown
		.dw sptotbx-1		;Total Bonus Complete
		.dw spdone-1
		.dw spdone2-1

spsnds	.db 0				;Wait
		.db 0				;Vax Bonus
		.db snd_b3a			;Destroy Ship Text
		.db snd_b3b			;Ship Points
		.db snd_b3a			;Extra Lives Text
		.db snd_b3b			;Extra Lives Points
		.db snd_b3a			;Tesseract Message
		.db snd_b3b			;Tesseract Points
		.db snd_b3a			;Total Bonus Message
		.db snd_f4			;Total Bonus Points
		.db snd_bonusr		;Total Bonus Countdown
		.db 0				;Total Bonus Complete
		.db 0				;Done
		.db 0				;Done 2

exlibon
		.db 0

		
spwait	ldx #mvbvaxx
		jsr ?vbmess
		rts

;Vaxx Bonus		
spvaxxb	
		ldx #mvbvaxx
		jsr ?vbmess
		rts

;Destroy Ship Text	
spdests
		ldx #mvbstat		
		jsr ?vbmess
		rts

;Final Station Points		
spdespt jsr vgcntr
		lda #colpurple
		ora textbrit
		ldx #tok_vpg
		jsr vgadd2          ;Purple
		lda gamest      ;Back to tact (downward)		
		and #$40		;flag $40 tells us sucessful exit
		ifne
			lda #$27
			ldx #$26
			jsr vgvtr5 				;Position and 100,000 display
			;escaped sucessfully
			JSR_VGCHAR(idxchar_1)      	;1
			JSR_VGCHAR(idxchar_0)      	;0
		else
			;move it farther right and 50,000 display
			lda #$2F
			ldx #$26
			jsr vgvtr5 
			JSR_VGCHAR(idxchar_5)      	;5
		endif
		JSR_VGCHAR(idxchar_0)      	;0
		jsr dx1000
		lda #$8				;Position
		ldx #$3
		jsr vgvtr5 
		lda textbrit		;badge brightness
		ora tokcolor+4
		ldx #tok_vpg     
		jsr vgadd2
		lda temp2
		vgadd_jsrl(hmtok1)
		lda textbrit		;badge brightness
		ora #colgreenr
		ldx #tok_vpg     
		jsr vgadd2
		vgadd_jsrl(hmtok2)
		lda textbrit		;badge brightness
		ora #colred
		ldx #tok_vpg     
		jsr vgadd2
		vgadd_jsrl(hmtok3)
		rts
		
;Extra Lives Text
spexlit ldx #mvbxlif
		jsr ?vbmess
		rts

;Extra Lives Points
spexlip 
		jsr vgcntr
		lda #$1F
        ldx #$14
        jsr vgvtr5          ;Position for digits
		lda #colpurple
		ora textbrit
		ldx #pod_vpg
		jsr vgadd2          	;Purple
		lda lives			;how many lives
		ifne
			jsr decimal			;convert from hex to decimal, output in temp7 and temp7+1
			;x10 by shifting up 4-bits across two score bytes
			jsr dshift
			;show it 
			lda #temp7
			sec
			ldy #02
			jsr digits          ;Display digits
			jsr dx1000
		else
			;move it farther right and draw just a single zero
			lda #$30
			ldx #0
			jsr vgvtr5 
			lda #colpurple
			ora textbrit
			ldx #pod_vpg
			jsr vgadd2          ;Purple
			;ldy #vgjch0-vgmsga
			JSR_VGCHAR(idxchar_0)      	;0
		endif
		rts

;Tesseract Message
sptessm	ldx #mvbtessr
		jsr ?vbmess
		rts

;Tesseract Points
sptessp
		jsr vgcntr
		lda #$1F
        ldx #0
        jsr vgvtr5          ;Initial Position for digits, transparent
		lda tokpock
		and #$0F			;Limit to bottom 4 bits (flags)
		ifne
			jsr howmbits		;A will contain either 0-4 now, up to 4 tokens
			asl A				;x2
			jsr decimal			;convert from hex to decimal, output in temp7 and temp7+1
			;x10 by shifting up 4-bits across two score bytes
			jsr dshift
			;Now we have x20, ready to show
			;turn on color
			lda #colpurple
			ora textbrit
			ldx #pod_vpg
			jsr vgadd2          ;Purple
			;draw digits
			lda #temp7			;Digit data here
			sec
			ldy #02
			jsr digits          ;Display digits
			jsr dx1000
			
			lda #$8				;Position
			ldx #$3
			jsr vgvtr5 
			lda textbrit
			sta temp1			;this makes the tesser the correct brite for the fadeout effects
			lda tokpock
			jsr outtes2			;Draw earned tesseract icons
		else
			;move it farther right and draw just a single zero
			lda #$30
			ldx #0
			jsr vgvtr5 
			lda #colpurple
			ora textbrit
			ldx #pod_vpg
			jsr vgadd2          ;Purple
			;ldy #vgjch0-vgmsga
			JSR_VGCHAR(idxchar_0)      	;0
		endif
		rts

;Total Bonus Message
sptotbm
		ldx #mvbtotal
		jsr ?vbmess
		rts

;Total Bonus Points
sptotbp
		lda #$18
        ldx #0
        jsr vgvtr5          ;Position for digits
		lda #colflash
		ora textbrit
		ldx #pod_vpg
		jsr vgadd2          ;Flash
		lda bonbuf
		sta temp7
		lda bonbuf+1
		sta temp7+1
		lda bonbuf+2
		sta temp8
		lda #temp7
        sec
        ldy #03
        jsr digits          ;Display digits
		jsr dx0
		rts

;Total Bonus Countdown
sptotbc
		lda vxstat
		and #$0F
		cmp #$0A
		ifeq
			;enable pause
			lda #$80
			sta bonpause
			lda bonbuf
			ora bonbuf+1
			ora bonbuf+2
			ifeq
				sta bonpause
				lda #g_sndstop
				jsr dosound
				jsr dodelay
				lda #snd_c1				;Big Explosion
				jsr dosound
				inc vxstat
			else
				sed					;**** DECIMAL MODE *****
				sec
				lda bonbuf
				sbc #$20			;We are subtracting 200 at a time here
				sta bonbuf
				lda bonbuf+1
				sbc #0
				sta bonbuf+1
				lda bonbuf+2
				sbc #0
				sta bonbuf+2
				ifcc
					;I think we went minus if here, force zero to trigger end
					lda #0
					sta bonbuf
					sta bonbuf+1
					sta bonbuf+2
				endif
				;countdown
				ldx	#00			;Guess Player 0
				lda	player		
				ifne
					ldx	#score2-score	;Point to Player 2 Score
				endif
				clc
				lda #$02
				adc	score+1,X
				sta	score+1,X
				lda	#00
				adc	score+2,X
				sta	score+2,X
				lda	#00
				adc	score+3,X
				sta	score+3,X
				cld					;**** END DECIMAL *****
				lda #$80
				sta scoflg
			endif
		endif
		rts
		
;Total Bonus Complete
sptotbx rts
spdone	rts
		
spdone2 ;lda #$C0		;Move to Animation
		lda vxstat
		ora #$40
		sta vxstat
		jsr resframe
        ldx #maxstr-1       ;Number of stars
        begin
            sta strflg,X        ;Turn off stars
            dex
        miend
		lda #00
        sta stroyh      	;Set up initial star origin
        lda #$78
        sta stroyl
		lda #0
		sta numstr			;show all the stars
		sta mntncntl
		sta mntncnth
		sta hwstat			;we are going to use the hwstat parameter for our planet animation steps
		sta bonusa			;No Bonus
		;sta manstat
		
		
		;set to flyby of maynard
		lda #$00
		sta shipxl			;set ship on left edge
		lda #$04
		sta shipxh			;set ship on left edge
		lda #$00
		sta shipyl
		lda #$0C
		sta shipyh
		lda #$80
		sta statst
		sta stbflg			;station to move down screen
		lda #$10			;do the flyby of Maynard
		sta mzgame
		;Maynard position (0,0 is top left corner)
		lda #$FF
		sta statxl
		lda #$08
		sta statxh
		lda #0
		sta statyl
		lda #1
		sta statyh
		
		lda #snd_bhend		;launch to maynard sound
		jsr dosound
		rts

dx0		JSR_VGCHAR(idxchar_0)		;0	
		rts
		
dx1000	JSR_VGCHAR(idxchar_0)     	;0
        JSR_VGCHAR(idxchar_0)      	;0
        JSR_VGCHAR(idxchar_0)     	;0
		rts

;*******************************************
;Add to bonus buffer		
;*******************************************
; Value in A,X are digits at X,XAA,000
; 
; Example: 150,000 - X=$01 A=$50 
;
; bonbuf is 3 bytes
;   position is : 2,211,00-(last is actual zero)
; The value in A needs to be shifted across two 
; bytes.
;*******************************************
addbon	sed
		clc
		adc	bonbuf+1			;Add MSB
		sta	bonbuf+1
		txa
		adc bonbuf+2
		sta bonbuf+2
		cld
		rts
		
dshift asl temp7
		rol temp7+1
		asl temp7
		rol temp7+1
		asl temp7
		rol temp7+1
		asl temp7
		rol temp7+1
		rts

;******************************************************
; Homeworld Animation Controller, called from tw_ship
;******************************************************
;Size of 'major unit', keep this even all the time!
mntntk = 24d
mntntkq = mntntk/4
mntntkqp2 = mntntkq+2;
mntntkqm2 = mntntkq-2;
mntntkhalf = mntntk/2
mntntkhalfm4 = mntntkhalf-4
mntntkhalfp4 = mntntkhalf+4
mntntkv4 = (mntntkhalf+mntntkqm2);
mntntkm4 = mntntk-4
mntnk1d5 = mntntk+mntntkhalf
mntntk2 = mntntk*2
mntntk25 = mntntk*2+mntntkhalf
mntntk3 = mntntk*3
mntntk4 = mntntk*4
mntntk5 = mntntk*5
mntntk6 = mntntk*6
mntntk7 = mntntk*7
mntntk8 = mntntk*8
mntntk14 = mntntk*14
;l74 units wide = 28 * 174 = 4872

;local vars
hwstat = nenemy
volcen = nenemy+1
elelvl = nenemy+2
veltmp = nenemy+3
zelev = zfire			;We use the first fireball slot for the elevator

;This will draw out the homeworld animation sequence
dohwani jsr ?hwstar				;always stars
		lda hwstat			;Which homeworld state are we in?
		asl	a			;double it for words tho
		tax				;put it back in X now
		lda	?vxjsr+1,X
		pha
		lda	?vxjsr,X
		pha
		rts

		
?vxjsr	.dw ?vxinit-1			\_stinit		= 0
		.dw ?vxfade-1			\_stfade		= 1
		.dw ?vxwait-1			\_stwait		= 2
		.dw ?vxshipl-1			\_stshipl		= 3
		.dw ?vxland-1			\_stland		= 4
		.dw ?vxwait2-1			\_stwait2		= 5
		.dw ?vxrex-1			\_strex 		= 6
		.dw ?vxscroll-1			\_stscroll		= 7	
		.dw ?walkingto-1		\_stwalking     = 8
		.dw ?pushbutton-1		\_stpushbutton  = 9 
        .dw ?elevatorup-1		\_stelevatorup  = 10
        .dw ?elevatorar-1		\_stelevatorar  = 11
        .dw ?elevatop-1			\_stelevatopen  = 12
        .dw ?elevaten-1			\_strexenter    = 13
		.dw ?loadup-1			\_stloadup   	= 14
		.dw ?elevatcl-1			\_stelevatclos  = 15
		.dw ?elevatdn-1			\_stelevatordn	= 16

?vxinit 
;Vax State 0 - Flyby + Initialize stuff

		lda #1
		sta manstat
		lda targf           ;At target above??
        ifmi  
			;and maynard is down off screen?
			;lda statyh
			;cmp #$0B
			;ifcs
				lda #snd_kilmaynard
				jsr dosound 
				lda #0
				sta statst			;turn off station again
				sta obsst+zelev		;Reset 
				lda #$20
				sta mzgame
				jsr resframe
				lda #snd_mys
				jsr dosound
				;lda #1
				;sta manstat			;turn of Rex until landing
				sta elelvl			;reset elevator level
				inc hwstat
			;endif
		endif
		rts
		
;Vax State 1 - Fade in mountains	
?vxfade jsr ?drawland
		lda frame
		cmp #$3C
		ifcs
			inc hwstat
		endif
		rts 
		
;Vax State 2 - Wait for minus frame
?vxwait ;Step 0, wait
		jsr ?drawland
		lda frame
		ifmi
			lda #$40
			sta shipxl
			lda #$FE
			sta shipxh
			lda #$80			;Start Y height = $0280
			sta shipyl
			lda #$02
			sta shipyh
			lda #$70
			sta shpscl
			inc hwstat			;next state
		endif
		rts
		
;Vax State 3 - Land ship	
?vxshipl 
		jsr ?drawland
		jsr ?drawship
		lda shipyl 
		and #$0F
		ifeq
			lda shpscl
			ifne
				dec shpscl
				dec shpscl
			endif
		endif
		;Move ship downwards
		lda shpscl
		ifne
			sec
			lda shipyl 
			sbc #08
			sta shipyl 
			lda shipyh
			sbc #0
			sta shipyh
		else
			;ship has landed
			lda #snd_h1
			jsr dosound
			jsr resframe
			inc hwstat		;next sequence
		endif
		rts
		
;Vax State 4 - Bounce ship		
?vxland jsr ?drawland
		jsr ?drawship
		lda frame
		cmp #$10
		ifeq
			jsr resframe
			inc hwstat
		endif
		rts
		
;Vax State 5 - Wait for Rex
?vxwait2 
		jsr ?drawland
		jsr ?drawship
		lda frame
		cmp #$40
		ifeq
			lda #$80
			sta volcen				;enable volcano
			inc hwstat
		endif
		rts
		
;Vax State 6 - Rex jumps
?vxrex	;State 4 - Rex jumps out	
		jsr ?drawland
		jsr ?drawship
		lda	#$00
		sta	xmot
		lda	#-2
		sta	xmot+1
		;;sta velyh
		lda #00
		sta ymot
		lda	#$FF
		sta	ymot+1
		
		lda #idx_pic0
		sta	piccur			;Make him run
		sta	tspark
		sta	nodraw			;Clear these just in case
		sta rtcol			;No Wall collisions by default
		sta ltcol   
		sta mzgrnd			;not on ground!
		sta jumpst			;not jumping
		
		lda #1
		sta objst			;Re-activate Rex
		
		lda	#$FA
		sta	mazeyh			;Place man/maze on screen, less than $FC also allows jumps with a hack
		lda	#$10			;This is the toss velocity, positive = right
		sta	velxh			;'throw' him out of space ship
		lda	#$00			;This was $80 (left).. lets try $00(right)
		sta	objxl
		sta	direct			;Jump out of ship right direction
		lda	#07
		sta	objxh
		lda #80
		sta ymot 
		inc hwstat			;next vxstate
		rts
		
;Vax State 7 - Rex Running to right	
?vxscroll
		jsr ?drawland
		jsr ?drawship
		lda #0
		sta rgdd
		lda mzgrnd
		ifmi
			;Keep Rex running
			lda #runseq
			sta picseq 			;make Rex run	
			lda #$08
			sta velxh	
			jsr ?mvmnts
			;has elevator been initialized yet?
			lda obsst+zelev
			ifeq
				lda xmot+1					;See if Rex has reached the middle of the screen
				cmp #$FF
				ifeq
					lda #$80         ;New position object for elevator relative to maze
					sta objxl+zelev
					lda #03
					sta objxh+zelev
					lda #80
					sta objyl+zelev
					lda #$FE
					sta objyh+zelev
					lda #$01
					sta obsst+zelev         ;Object active = yes
				endif
			else
				jsr ?drelev
				lda xmot+1
				cmp #0
				ifeq
					lda xmot
					cmp #$F0
					ifcs
						;stop Rex
						lda #$0
						sta velxh
						jsr resframe
						inc hwstat
					endif
				endif
			endif
		endif
		rts

;Vax State 8 - Some wait time, Freeze Rex, no more ship on screen
?walkingto
		jsr ?drawland
		jsr ?drelev
		;stop rex
		lda #0
		sta velxh
        ;lda frame
		;ifpl
			lda #$80
			sta gamest
		;	jsr resframe
			inc hwstat  ;move to next state
        ;endif
        rts
 
;Vax State 9 - Push button sequence, Rex can't move manually 
?pushbutton
		jsr ?drawland
		jsr ?drelev
		;stop rex
		lda #0
		sta velxh
        lda frame
        cmp #$18
        ifge
            cmp #$50
            ifge
                ;Button has been pressed!
                lda #00
                sta elelvl
				lda #-1
				sta velxh
				jsr resframe
                inc hwstat  ;move to next state
            else
                ;time to push the button
                lda #pshbseq
                sta picseq
            endif
			lda frame
			cmp #$30
			iflt
				lda xmot
				clc 
				adc #1
				sta xmot
				lda xmot+1
				adc #0
				sta xmot+1
			endif			
        endif
        rts

;Vax State 10 - Elevator coming up... Rex can move now again too, with limits                    
?elevatorup
		jsr ?drawland
		jsr ?drelev
		lda frame+1
		bne ?freerex
		lda frame
		cmp #$30		;less than $30 still under remote control
		ifcc
			;cmp #$04		;greater than $4
			;ifcs
				cmp #$16
				ifcc			;less than $16
					;finishing button animation, move rex back a little to the left
					sec
					lda xmot
					sbc #1
					sta xmot
					lda xmot+1
					sbc #0
					sta xmot+1
				;endif
			else
				;no motion
				lda #0
				sta velxh
			endif
		else
?freerex	jsr ?limitrex
			jsr ?chkemote
			;start moving elevator up
			lda frame
			and #$1F
			ifeq
				;lda #00
				;sta frame
				bit elelvl
				ifmi
					dec elelvl
					lda elelvl
					and #07
					ifeq
						;elevator has arrived
						lda #snd_i2h
						jsr dosound
						inc hwstat  ;move to next state
						;jsr resframe
					endif
				else
					inc elelvl
					lda elelvl 
					cmp #$07
					ifge
						lda #$87
						sta elelvl
					endif
				endif
			endif
		endif
        rts

;Vax State 11 - Elevator has arrived
?elevatorar
		jsr ?drawland
		jsr ?drelev
		jsr ?limitrex
		jsr ?chkemote
        ;Elevator has arrived
        inc hwstat  ;move to next state
        lda #00
        sta perm1       ;Save this for the next state so we start clean
        rts

;Vax State 12 - Open Elevator doors        
?elevatop
		jsr ?drawland
		jsr ?drelev
		jsr ?limitrex
		jsr ?chkemote
        lda frame
        and #$7F
        sta perm1       ;Save frame here for the drawing routine
        cmp #$7F
        ifeq
            inc hwstat  ;move to next state
        endif
        rts
 
;Vax State 13 - Elevator door is open 
?elevaten
		jsr ?drawland
		jsr ?drelev
		jsr ?limitrex
		lda frame
        cmp #$30
		ifcs			;Has enought time elapsed
			jsr ?getzone
			ifne
				;save direction rex should move and goto next state
				sta veltmp
				lda #0
				sta velxh		;No more player control
				inc hwstat
			endif
		endif
		jsr ?chkemote
        rts
		
;Vax State 14 - rex moves into the elevator		
?loadup jsr ?drawland
		jsr ?drelev
		lda #0
		sta velxh			;No Rex control by player
		lda frame
		and #7
		ifeq
			lda veltmp
			ifne
				ifpl
					lda xmot
					clc
					adc #2
					sta xmot
					lda xmot+1
					adc #0
					sta xmot+1
				else
					lda xmot
					sec
					sbc #2
					sta xmot
					lda xmot+1
					sbc #0
					sta xmot+1
				endif
			endif
			;always move upwards
			lda ymot
			clc
			adc #1
			sta ymot
			lda ymot+1
			adc #0
			sta ymot+1
			;check for target position
			lda ymot
			cmp #$8C			;target +1
			ifcs
				;Rex is in elevator now
				inc hwstat
				jsr resframe
			endif
			lda #$40
			sta picdely			;prevent from going to foot taps
		endif
		rts
		
;Vax State 15 - Rex is in elevator and doors close
?elevatcl
		jsr ?drawland
		jsr ?drelev
		jsr ?limitrex
		;force Rex into our sweet spot
		lda #$8E
		sta xmot
		lda #$8C
		sta ymot
		;make Rex skinny as doors close
		ldx #idx_pic21
		lda perm1
		cmp #$40
		ifcc
			ldx #idx_pic21a
			cmp #$38
			ifcc
				ldx #idx_pic21b
				cmp #$2B
				ifcc
					ldx #idx_pic21c
					cmp #$24
					ifcc
						ldx #idx_pic21d
						cmp #$1C
						ifcc
							ldx #idx_pic21e
							cmp #$18
							ifcc
								ldx #idx_pic21f
							endif
						endif
					endif
				endif
			endif		
		endif
		stx piccur
		stx picdely		;also store here to keep timer from going to foot tap
		;;;
        lda frame
        and #$7F
        sta perm1       ;Save frame here for the drawing routine
        lda #$7f
        sbc perm1
        sta perm1
        cmp #$00
        ifeq
            inc hwstat  ;move to next state
            jsr resframe
        endif
        rts      

;Vax State 16 - We are done, door are closed, fade out everything then goto story
?elevatdn
		jsr ?drawland
		jsr ?drelev
		lda #$0
		sta picdely		;also store here to keep timer from going to foot tap
		;end homeworld, move on
		lda frame
		cmp #$3C
		ifcs
			;lda #0			
			;sta objst
			lda vxstat
			and #$C0
			ora #$20		;Mark animation complete, move to Story
			sta vxstat
			;prep end story
			lda #mendtx00	
			sta strtln
			lda #-2   
			sta strtyl
			sta strtyh          ;Message Start Position
			lda #snd_triumph
			jsr dosound
		endif
		rts		

;Draw all Land: Mountains, Foreground + Volcano
?drawland
		;scale	
		VGADD_SCALE(ywin_off,binscal2,$0)		
		;Foreground Mountains
		lda hwstat
		cmp #_stfade
		ifeq					;fade in
			lda frame 
			and #$3C
			asl A 
			asl A 
		else
			lda #$F0		;full on except for fade in and out
		endif
		pha					;save for later
		ora #colgreen
		ldx #horiz_vpg		;Stat and page (mountains) select
		jsr vgadd2			;NOTE: this destroys Y values
		;foreground terrain
		VGADD_JSR(fterrap)
		VGADD_JSR(fterra)	
		;Background mountains
		pla
		sec
		sbc #$50
		ifcs
			ora #colgreen
			ldx #horiz_vpg		;Stat and page (mountains) select
			jsr vgadd2			;NOTE: this destroys Y values
			jsr ?drawmntns
		endif
		lda volcen
		ifmi
			lda frame
			and #$07
			ifeq
				jsr ?updtvol
			endif
			jsr drawvol			;volcano only erupts once rex is on the ground!
		endif
		rts

;Draw Mountains only
?drawmntns
		;Draw the horizon (always)
		VGADD_JSR(horiz)
		;position based upon major count
		VGADD_JSR(mntnpo)
		;draw the tiny tick which will be an X vector from 0-mntntk 
		lda mntncntl
		ifne
			jsr neg
			sta vdata
			lda #$FF
			sta vdata+1
		else
			sta vdata
			sta vdata+1
		endif
		lda #0
		sta vdata+2
		sta vdata+3		
		jsr vgvtr2
		;draw the position vector for this index 
		lda mntncnth
		asl A
		asl A				;4 bytes per vector/index 
		sta perm2+1
		tay
		lda mntnsb,Y
		ldx mntnsb+1,Y
		jsr vgadd2 
		;restore y 
		ldy perm2+1
		lda mntnsb+2,Y
		ldx mntnsb+3,Y
		jsr vgadd2 
		;now draw mountain segments
		lda #mntnsegcnt		;37d
		sta perm2			;How many to draw max
		ldx mntncnth
		lda mntnsk,X 
		asl A 
		asl A
		sta perm2+1	
		begin
			ldy perm2+1
			;copy 4 vector bytes
			lda mntns,Y
			ldx mntns+1,Y
			cpx #$C0
			ifeq
				;this is the end
				lda #0
				sta perm2
			else
				jsr vgadd2
				ldy perm2+1
				lda mntns+2,Y
				ldx mntns+3,Y	
				jsr vgadd2
			endif
			inc perm2+1
			inc perm2+1
			inc perm2+1
			inc perm2+1
			dec perm2 
		miend
		rts 

;Checks Rex position for the elevator load
; -1 if in left zone
; +1 if in right zone
; 0 if not in either		
?getzone
		lda xmot+1
		ifpl
			cmp #1
			ifeq
				lda xmot
				cmp #$70
				ifcs
					;greater than left edge
					cmp #$90
					ifcc
						;less than center, left zone
						lda #01
						rts
					else
						;greater than center
						cmp #$B0
						ifcc
							lda #-1
							rts
						endif
					endif
				endif
			endif
		endif
		lda #0
		rts

;Check button status for emote animations
?chkemote
		;Button checks for emotes
		lda jbstat
		and #$40
		ifne			;is shield pressed?
			lda picseq
			ifpl
				;We dancin'
				lda #emotseq			;Flag emote sequence
				sta picseq		
				lda #0					;Start emote index at first entry
				sta emoteix
			else
				;we can check fire here
				lda frame
				and #$07
				ifeq
					bit jbstat          ;Button Pressed
					ifmi
						inc picseq
						lda picseq
						and #$81
						sta picseq
						;set to initial index for this sequence, seq in A
						lda #0
						sta emoteix
					endif
				endif
			endif
			;no motion allowed
			lda #0
			sta velxh
			sta velxl
		else
			;only reset if we were emoting
			lda picseq
			ifmi
				lda #0
				sta picseq
				lda #idx_pic21			;default Rex standing
				sta piccur
			endif
		endif
		rts

;Keep Rex on screen
?limitrex
		lda xmot+1
		ifmi			;Not on left side of screen
			cmp #$FE			;Our Limit in X on left is $FD60
			ifcc
				lda xmot
				cmp #$60
				ifcc
					lda velxh		;can't move farther left
					ifmi
?novel					lda #0
						sta velxh
					endif
				endif
			endif
		else
?lr2		cmp #$01		;Our limit in X on right is $0280
			ifcs
				lda hwstat
				cmp #_strexenter		;Once door is open, we can move father right
				bne ?lr3
				cmp #$E0
				ifcs
?lr3				lda velxh		;can't move farther right
					bpl ?novel
				endif
			endif
		endif
		rts

;Scrolls the foreground objects across the screen
?mvmnts ;lets move mountains + ship if not done
		lda #totalmtn-4				;how many major ticks to scroll
		cmp mntncnth
		ifcs
			;move ship
			sec
			lda shipxl
			sbc #$2
			sta shipxl
			lda shipxh
			sbc #0
			sta shipxh
			;now elevator
			sec
			lda objxl+zelev
			sbc #$2
			sta objxl+zelev
			lda objxh+zelev
			sbc #0
			sta objxh+zelev
			;now mountains
			inc mntncntl				;still allowed to scroll here
			lda #(mntntk-1)
			cmp mntncntl
			ifcc
				inc mntncnth
				lda #0
				sta mntncntl
			endif
		endif
		rts

;**********************************************
;Position and then draw the elevator
?drelev	jsr ?drelvp
		jsr ?drevall
		rts

;**********************************************
;Draw the elevator position vector	
;This gets called multiple times to draw 
;differe parts of the elevator.	
?drelvp jsr vgcntr
		lda #$30
		ldx #$72
		jsr vgadd2      ;Set scale, qqqscalqqq
		lda objxl+zelev
		sta vdata
		lda objxh+zelev
		sta vdata+1
		lda objyl+zelev
		sta vdata+2
		lda objyh+zelev
		sta vdata+3
		lda #00
		sta vgbrit      ;Draw it in black
		jsr vgvtr2      ;Move beam to object location here
		;set default scale for elevator parts
        lda #$30
        ldx #$72        ;Our scale for the elevator stuff
        jsr vgadd2      ;Scale for positioning and elevator
        rts

;Draw all the elevator parts
?drevall
		ldx #elev_vpg
        lda #$E0+colred2
        jsr vgadd2
        VGADD_JSR(elevat)  						;draw elevator structure
		;Button is next, different colors
        lda hwstat
        cmp #_stelevatorar   ;03
        ifge
            ;Arrived, button is green
            lda #($E0+colgreen)
        else
            cmp #_stelevatorup
            ifge
                ;Waiting, button is yellow
                lda #($E0+colyellow)
            else
                ;Not requested yet... blue
                lda #($E0+colblue)
            endif
        endif
        ldx #elev_vpg
        jsr vgadd2
        VGADD_JSR(elevatb)
        ;draw the elevator level indicator border
        VGADD_JSR(elevatc)
        ;draw the elevator level pointer here
        lda #($E0+colyellow)
        ldx #elev_vpg
        bit elelvl
        ifmi
            ldx #elev_vpg+v_xflip
        endif
        jsr vgadd2
        lda #($E0+colyellow)
        sta vgbrit
        lda elelvl
        and #07
        asl A
        asl A               ;x4
        clc 
        adc #03
        tay                 ;Input Place
        ldx #03
        begin
            lda ?eleind,Y       ;copy base position to vdata
            sta vdata,X         ;store Y position
            dey 
            dex 
        miend
        jsr vgvtr2      ;Draw it
        ;put flip back
        lda #($E0+colyellow)
        ldx #elev_vpg
        jsr vgadd2
		;plant
		jsr ?drelvp
		VGADD_JSR(elevp) 
        ;Position again for doors
        jsr ?drelvp      		
        ;now center it and color 
        lda #($E0+colyellow)
        ldx #elev_vpg
        jsr vgadd2
        lda hwstat
        ;cmp #_stloadup       ;If we are not done, then al
        ;iflt
            cmp #_stelevatopen
            ifeq
?edrani         ;Doors are either opening or closing now...
                ;lda frame
                ;and #$7F
                lda perm1
?edran2         lsr A
                sta temp2            ;save our value for later
                ;make a vector -x
                eor #$FF 
                ora #$C0
                sta vdata
                lda #$FF
                sta vdata+1
                lda #00
                sta vdata+2
                sta vdata+3
                jsr vgvtr2 
                ;draw the line
                jsr ?elevln
                ;make a vector +2x and -125y 
                lda #00
                sta vdata+1
                lda temp2
                asl A       ;double positive position vector
                sta vdata
                lda #$83
                sta vdata+2
                lda #$FF
                sta vdata+3
                jsr vgvtr2 
                ;draw the line
                jsr ?elevln
                jmp ?elemsg
            endif
			cmp #_stloadup 
			beq ?edrani
            cmp #_stelevatclos
            beq ?edrani
            cmp #_strexenter
            ifeq
                lda #$7f
                bne ?edran2
            endif
            ;here if doors are just plain closed
            jsr ?elevln
        ;endif
?elemsg ;now draw robots only sign frame
        jsr ?drelvp
        VGADD_JSR(elesign)     
        ;and message, this reduces scale, so beware
        VGADD_VCTRL(3d,-13d, hidden)
		lda #$E0
		sta textbrit			;Turn on Text!
        lda #mhwrobs
		sta msg_idx
        jsr ?msgnop
        VGADD_VCTRL(-150,-60, hidden)
        lda #mhwonly
		sta msg_idx
        jsr ?msgnop  
		;final warp 
		jsr ?drwarp
		;top sign
		jsr ?drelvp
		;Sign frame
		lda #($F0+colbluer)
        ldx #elev_vpg
        jsr vgadd2
		VGADD_JSR(elesigt)
		;Welcome message
        lda frame
        and #$40
        ifne
			lda #$E0
			sta textbrit			;Turn on Text!
            ;jsr ?drelvp
            ;VGADD_VCTRL(-135d,193d, hidden)
            lda #mhwvaxx
			sta msg_idx
			jsr ?msgnop   
        endif
        rts		

?drwarp ;black position vector
		lda #$00
		ldx #$60
		jsr vgadd2
		VGADD_VCTRS(110,-128,hidden)
		lda #$3
		ldy #$30					;Linear
        jsr vgscal 
		lda #($C0+colpink)			;Pink Warp Code
		ldx #$60
		jsr vgadd2		
		lda warpcodes
		sta perm5
		lda warpcodes+1
		sta perm5+1
		clc							;Allow zeros
		ldy #3						;Always 3 digits
		lda #perm5
		jmp sdigits
		
		
;Routine to manually draw the elevator door line
?elevln VGADD_VCTRL(0d,125d,visible)
        rts
        
;Elevator pointer data, double words
?eleind .dw -24,4
        .dw -22,9
        .dw -20,13
        .dw -17,16
        .dw -14,19
        .dw -10,22
        .dw -6,24
        .dw 0,25
		
?drawship
		;center!
		jsr vgcntr	
		VGADD_SCALE(ywin_off,binscal2,$0)	
		;position
		;X First
		lda shipxl
		sta vdata
		lda shipxh
		sta vdata+1
		;Y next
		lda shipyl
		sta vdata+2
		lda shipyh
		sta vdata+3	
		jsr vgvtr2				;ships position
		lda hwstat
		cmp #_stland
		ifeq
			;another position vector that does the 'rough landing' animation
			lda frame 
			and #$0C
			lsr A 
			lsr A
			tax
			;X bounce
			lda bouncex,X
			sta vdata
			;Y next
			lda bouncey,X
			sta vdata+2
			lda #0
			sta vdata+1
			sta vdata+3	
			jsr vgvtr2	
		endif
				
		lda shpscl
		ldx #$72
		jsr vgadd2         
		
		ldx #plane_vpg			;Stat, Page Select (Might be needed)
		lda #($E0+colwhite)
		jsr vgadd2 
		
		VGADD_JSR(plne24) 
		rts
		
bouncex  .db 0,1,2,1	
bouncey  .db 0,4,0,4

;Based upon position, how many long vectors to skip
mntnsk
	.db 00	;0
	.db 00	;1
	.db 01	;2
	.db 01	;3
	.db 01	;4
	.db 01	;5
	.db 02	;6
	.db 04	;7
	.db 04	;8
	.db 05	;9
	.db 05	;A
	.db 05	;B
	.db 05	;C
	.db 06	;D
	.db 06	;E
	.db 06	;F
	.db 06	;10
	.db 11	;11
	.db 11	;12
	.db 11	;13
	.db 11	;14
	.db 12	;15
	.db 12	;16
	.db 12	;17
	.db 12	;18
	.db 14	;19
	.db 14	;1A
	.db 16	;1B
	.db 16	;1C
	.db 18	;1D
	.db 18	;1E
	.db 18	;1F
	.db 18	;20
mntnskend

totalmtn = mntnskend - mntnsk

;Mountain Stubs - These are positional vectors that will allow us to get into the right spot to start drawing
mntnsb
	vctrl(0,mntntk,hidden)				;00
	vctrl(-mntntk,mntntk,hidden)		;01
	vctrl(0,mntntk2,hidden)				;02 
	vctrl(-mntntk,mntntk2,hidden)		;03 
	vctrl(-mntntk2,mntntk2,hidden)		;04 
	vctrl(-mntntk3,mntntk2,hidden)		;05 
	vctrl(0,0,hidden)					;06
	vctrl(0,mntntk4,hidden)				;07
	vctrl(-mntntk,mntntk4,hidden)		;08
	vctrl(0,mntntk2,hidden)				;09
	vctrl(-mntntk,mntntk2,hidden)		;0A
	vctrl(-mntntk2,mntntk2,visible)		;0B 
	vctrl(-mntntk3,mntntk2,visible)		;0C
	vctrl(0,mntntk4,hidden)				;0D 
	vctrl(-mntntk,mntntk4,hidden)		;0E 
	vctrl(-mntntk2,mntntk4,hidden)		;0F 
	vctrl(-mntntk3,mntntk4,hidden)		;10
	vctrl(-mntntk4,0,hidden)			;11
	vctrl(-mntntk5,0,hidden)			;12
	vctrl(-mntntk6,0,hidden)			;13
	vctrl(-mntntk7,0,hidden)			;14
	vctrl(0,mntntk4,hidden)				;15
	vctrl(-mntntk,mntntk4,hidden)		;16
	vctrl(-mntntk2,mntntk4,hidden)		;17
	vctrl(-mntntk3,mntntk4,hidden)		;18 
	vctrl(-mntntk2,mntntk2,hidden)		;19
	vctrl(-mntntk3,mntntk2,hidden)		;1A 
	vctrl(-mntntk6,mntntk4,hidden)		;1B
	vctrl(-mntntk7,mntntk4,hidden)		;1C
	vctrl(0,mntntk,hidden)				;1D
	vctrl(-mntntk,mntntk,hidden)		;1E
	vctrl(-mntntk2,mntntk,hidden)		;1F
	vctrl(-mntntk3,mntntk,hidden)		;20
	
mntns
	vctrl(mntntk2,mntntk,visible)	;1	0
	vctrl(mntntk4,-mntntk2,visible)	;2	0
	vctrl(-mntntk2,mntntk,hidden)	;3	1
	vctrl(mntntk3,mntntk3,visible)	;4	1	
	vctrl(mntntk2,-mntntk2,visible)	;5	1
	vctrl(mntntk4,mntntk2,visible)	;6	1
	vctrl(mntntk4,-mntntk2,visible)	;7	2
	vctrl(-mntntk4,mntntk2,hidden)	;8	4
	vctrl(mntntk2,0,visible)		;9	4
	vctrl(mntntk2,-mntntk2,visible)	;10
	vctrl(-mntntk4,-mntntk2,hidden)	;11
	vctrl(mntntk8,mntntk4,visible)	;12
	vctrl(mntntk4,-mntntk4,visible)	;13
	vctrl(-mntntk2,mntntk2,hidden)	;14
	vctrl(mntntk4,-mntntk2,1)		;15
	vctrl(-mntntk6,mntntk4,0)		;16
	vctrl(mntntk4,-mntntk2,1)		;17
	vctrl(mntntk4,-mntntk,1)		;18
	vctrl(mntntk6,-mntntk,1)		;19
	vctrl(-mntntk5,mntntk,0)		;20
	vctrl(mntntk3,mntntk3,1)		;21
	vctrl(mntntk2,-mntntk4,1)		;22
	vctrl(-mntntk2,mntntk4,0)		;23
	vctrl(mntntk8,-mntntk4,1)		;24
	vctrl(-mntntk6,mntntk3,0)		;25
	vctrl(mntntk4,mntntk2,1)		;26
	vctrl(mntntk2,-mntntk2,1)		;27
	vctrl(mntntk6,-mntntk3,1)		;28
	vctrl(-mntntk8,mntntk5,0)		;29
	vctrl(mntntk14,-mntntk5,1)		;30
	vctrl(-mntntk5,mntntk2,0)		;31
	vctrl(mntntk2,mntntk4,1)		;32
	vctrl(mntntkq,-mntntkhalf,1)	;33 -v1
	vctrl(mntntkq,mntntkqp2,1)		;34 -v2 
	vctrl(mntntkq,-mntntkhalf,1)	;35 -v3
	vctrl(mntntkq,mntntkv4,1)		;36 -v4
	vctrl(mntntk3,-mntntk6,1)		;37
	vctrl(mntntk8,mntntk2,1)		;39
	vctrl(mntntk4,0,1)				;40
	vctrl(mntntk6,-mntntk2,1)		;41
	vctrl(-mntntk3,mntntk,0)		;42
	vctrl(mntntk4,mntntk3,1)		;43
	vctrl(mntntk2,-mntntk4,1)		;44
	vctrl(mntntk6,mntntk6,1)		;45
	vctrl(mntntk4,-mntntk6,1)		;46
mntnsend
	rtsl
	rtsl
	;double rtsl is needed here for safety

mntnsegcnt = (mntnsend - mntns) >> 2		;divide by 4 to get real segment count
;********************************************
;Volcano Particle Routines start here
;
;********************************************
volpst = sobjst
volpvx = sobdir
volpvy = sobcog	
volpxl = sobjxl
volpxh = sobjxh
volpyl = sobjyl
volpyh = sobjyh

?updtvol	ldx	#$04            	;max 5 particles (0-4)
?vloop    	begin
				lda	volpst,x   		;is this particle alive?
				ifeq					;no, re-animate it
					; Create new particle, maybe.
					jsr	getrand
					and #$07            	;0-7
					ifeq					;12.5% chance of creating a particle
						jsr getrand      		;get random number
						pha						;save for below
						and #$1C
						ora #$03				;Min life
						;lda #$1f            	;set lifetime to max (5 bits -> $1f)
						sta volpst,x
						pla
						;jsr getrand      		;get random number
						ifmi
							and #$03            	;0-3
							adc #$01            	;1-4 or 2-5 (note this will clear carry)
							eor #$FF
						else 
							and #$03          		;0-3
							adc #$01            	;1-4 or 2-5 (note this will clear carry)
						endif
						sta volpvx,x    		;will be slightly faster on average
						jsr getrand      		;get another random number
						and #$07            	;0-7
						adc #$05            	;5-12
						sta volpvy,x
?icoords     			lda #$00            	;position at particle source (0,0)
						sta volpxl,x
						sta volpxh,x
						sta volpyl,x
						sta volpyh,x
					endif
				else
					dec	volpst,x     		;decay
					beq ?icoords       		;if we reached zero, clear coords and move on
					; Update the particle's Y position.  The Y velocity decrements each time, giving
					; particles a gravitational acceleration.
					dec volpvy,x      		;decrement Y velocity
					ldy #$00              	;assume positive
					clc
					lda volpvy,x      		;are we moving up or down?
					ifmi
						dey                 	;sign-extend negative
					endif
					adc volpyl,x   			;update Y coordinate
					sta volpyl,x
					; Kill particles that hit the ground.  Remember that particles use coordinates
					; relative to the origin point at the top of the volcano, which is at Y=+94, so
					; the ground is at -94.
					lda volpyh,x   ;check high byte
					ifmi			;positive, not at ground level
						lda volpyl,x   ;check low byte
						cmp #$a2              ;-94
						ifcc
							lda #$00              ;kill particle
							sta volpst,x
							beq ?icoords      ;(always)
						endif
					endif
					tya                       ;high byte of Y velocity
					adc volpyh,x   			;update high byte of Y position
					sta volpyh,x
					; Update the X position.  Velocity does not change.
					ldy #$00              ;assume positive
					clc
					lda volpvx,x
					ifmi
						dey                       ;sign-extend negative
					endif
					adc volpxl,x
					sta volpxl,x
					tya
					adc volpxh,x
					sta volpxh,x
				endif
?nextp			dex             ;next particle
			miend
			rts 

;Volcano position relative to the mntnctl and mntncth vars 
voldist = 592
volhite = 140

;***************************************************************
;Volcano particle drawing routine	
;
; temp1 = volcano ref position X 
;         volcano ref position Y (constant - volhite)
; temp2 = particle ref position X 
; temp3 = particle ref position Y
;***************************************************************
drawvol ;figure out where the volcano center is...
		lda # lbyte(voldist)			;When mntncnth is zero then we are at 1024 ($400)
		sec 
		sbc mntncntl
		sta temp1
		lda # ubyte(voldist)
		sbc #0
		sta temp1+1			
		;temp1 has our distance - mntncntl, now do the major count
		ldy mntncnth
		ifne
			begin
				sec
				lda temp1
				sbc #mntntk
				sta temp1
				lda temp1+1
				sbc #0
				sta temp1+1
				dey
			eqend
		endif 				;temp1 has our distance - mntncntl - (mntncth*24d)
		;draw each particle
		ldx #$04              ;loop through 5 particles	
		begin
			stx temp4
			lda volpst,x 
			ifne
				jsr vgcntr
				ldx temp4
				;active, add particle specific position to the volcano point
				;X First
				lda temp1
				clc 
				adc volpxl,X 
				sta temp2 
				lda temp1+1
				adc volpxh,X 
				sta temp2+1
				;Y next 
				lda # lbyte(volhite) 
				clc 
				adc volpyl,X 
				sta temp3 
				lda # ubyte(volhite)
				adc volpyh,X 
				sta temp3+1
						
				;draw a positiion vector to our start point
				;X First
				lda temp2
				sta vdata
				lda temp2+1
				sta vdata+1
				;Y next
				lda temp3
				sta vdata+2
				lda temp3+1
				sta vdata+3	
				jsr vgvtr2
				
				ldx temp4 
				lda volpyh,X 
				ifmi
					lda volpyl,X 
					and #$F8
					asl A
					ora #colred2
				else 
					lda #($F0+colred2)
				endif
				ldx #horiz_vpg
				jsr vgadd2  
				VGADD_JSR(volpart)
				ldx temp4
			endif
			dex 
		miend
		rts
		
; volpst = sobjst
; volpvx = sobdir
; volpvy = sobcog	
; volpxl = sobjxl
; volpxh = sobjxh
; volpyl = sobjyl
; volpyh = sobjyh

;***********************************************
;draw the stars on the horizon
;***********************************************
?hwstar lda numstr
        cmp #maxstr
        ifcc
			ldx #maxstr
			begin
				dex
				bmi ?strdr  	;Jump to drawing
				lda strflg,X
			eqend           
			;Found a dead one, get a random position
			jsr getrand
			sta strxl,X
			jsr getrand
			and #07
			bit rands+2
			ifmi
				jsr neg
			endif
			sta strxh,X
			jsr getrand
			sta stryl,X
			;use again for msb, but alter to keep above mountains
			and #$03
			clc
			adc #2
			sta stryh,X 	;Start at top

			jsr getrand
			and #03     	;Extra Speed Flags
			ora #$80        ;Flag Active
			sta strflg,X
			inc numstr
		endif
?strdr  ldx #maxstr-1       ;Total Stars
        begin
			stx temp1
            lda strflg,X
            ifpl
                jmp ?nxtstr
            endif
            and #03
            sta temp2       ;Used in gss1 later

            jsr drawstr
            ldx temp1
?nxtstr     dex
        miend
		rts

;********************************************************
; Message Routines
;********************************************************
___vbmsgnum = 0
___vbnumsgs = 8

vbmessptrl = $
vbmessptrh = (vbmessptrl + ___vbnumsgs)
vbmessscol = (vbmessptrh + ___vbnumsgs)
vbmessypos = (vbmessscol + ___vbnumsgs)
vbmessxpos = (vbmessypos + ___vbnumsgs)

#define     vbmess(vbxlit,vbxcol,vbxsca,vbypos,vbxpos)  \ .org (vbmessptrl+___vbmsgnum)
#defcont    \ .byte (vbxlit&$FF)
#defcont    \ .org (vbmessptrh+___vbmsgnum)
#defcont    \ .byte ((vbxlit/$100)&$FF)
#defcont    \ .org (vbmessscol + ___vbmsgnum)
#defcont    \ .byte ((vbxcol*$10)|(vbxsca))
#defcont    \ .org (vbmessypos+___vbmsgnum)
#defcont    \ .byte vbypos
#defcont    \ .org (vbmessxpos+___vbmsgnum)
#defcont    \ .byte vbxpos
#defcont    \m+vbxlit = ___vbmsgnum
#defcont    \___vbmsgnum .set ___vbmsgnum+1
#defcont    \#if (___vbnumsgs < ___vbmsgnum) 
#defcont    \ .error "MSG: Number of Vaxx Bonus messages exceeds defined limit, increase please!"
#defcont    \#endif 


	vbmess(vbvaxx,colbluer,0,$48,$BC)      
	vbmess(vbstat,colwhite,1,$20,$90)			
	vbmess(vbxlif,colwhite,1,$10,$90)		
	vbmess(vbtessr,colwhite,1,$00,$90)
	vbmess(vbtotal,colyellow,1,-$20,$C0)
	vbmess(hwrobs,colyellow,3,0,0)    	;ROBOTS
    vbmess(hwonly,colyellow,3,0,0)    	;ONLY
    vbmess(hwvaxx,colred2,2,0,0)    	;Welcome to Maynard

vbvaxx		.ctext "VAXX BONUS"
vbstat		.ctext "DEFEAT STATION"
vbxlif		.ctext "CLONES X 10K"
vbtessr		.ctext "TESSERACTS X 20K"
vbtotal		.ctext "TOTAL BONUS"
hwrobs      .ctext "ROBOTS"         
hwonly      .ctext "ONLY"                  
hwvaxx      .ctext "WELCOME TO MAYNARD"	

?vbmess stx msg_idx
		jsr vgcntr
		ldx msg_idx
		lda vbmessypos,X          ;This is Y position
        sta msg_y
		lda vbmessxpos,X          ;This is X position
        sta msg_x
		lda #00
        sta vgbrit              ;black line for positioning
        lda #02					;Scale for positioning is always 2
        jsr vgscalz             ;a=binary scaling value, linear scaling value = 0
        lda msg_x
        ldx msg_y
        jsr vgvtr1              ;Position Beam (Use vgbrit)
		;Fall through
		;Set Color and Scale Next
?msgnop	ldx msg_idx
		lda vbmessscol,X            ;Color value and scale of text
        pha 
        lsr A
        lsr A
        lsr A
        lsr A
        ora textbrit
		tay 
        lda #00
        jsr vgstat              ;Set color
		pla
        and #$0F
		clc
        adc #01
        ldy #$30
        jsr vgscal              ;Set scale  
		;Fall through 
		ldx msg_idx 
		lda vbmessptrl,X
		sta vdata
		lda vbmessptrh,X
		sta vdata+1
		;***************************************************************************
		lda #00                 ;Init vglist offset
		ldy #00
        sta temp3+1
        begin
            lda (vdata,Y)               ;Get next character
            sta msg_x
            and #$7F                    ;Mask neg bit for end check
            tax 
            iny                         ;Next byte index
            sty temp3                   ;Save Y value
            lda vgmsga,X                ;Get correct vector JSRL from char index
            ldy temp3+1
            sta (vglist,Y)
            iny 
            lda vgmsga+1,X
            sta (vglist,Y)              ;Write it to VGRAM
            iny 
            sty temp3+1                   ;Save y
            ldy temp3                   ;Get character ptr
            bit msg_x                   ;if not end of string
        miend
        ldy temp3+1
        dey 
        jmp vgadd



		
;*********************
; EXTERNALS
;*********************		
.export dovxbonus,dohwani