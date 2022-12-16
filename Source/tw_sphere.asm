;************************************************************
    .TEXT ".TWSPHERE."
;************************************************************
	.title "TWSphere"
	.sbttl "enem0 -  Master Control Routine"
;************************************************************
;* Constants
nweb 	=	25d		;Number of web spinners
nsph	=	10d
statsp 	=	$63

;************************************************************
; Notes on Sphere variables
;************************************************************
; webssta: 00 - Inactive
;          01 - Oribiting
;          02 - Diving
;          04 - Spinning
;          FF - Exploding
; webss2   00 -
;          40 - Down
;          C0 - 
;************************************************************


?localpc = *

 .org $0600
 
difmx3	.block	1		;Difficulty maxing out at number of waves
six		.block	1		;0 to 5 counter
sphrxl	.block	1		;Center of sphere rotation position
sphrxh	.block	1		
sphryl	.block	1
sphryh	.block	1
sphrad	.block	1		;Radius of sphere orbits
sphrst	.block	1		;0 or 1 depending on rotation post
sphscl	.block	1		;Sphere scale LSB
sphsch	.block	1		;Sphere scale MSB
sphcol	.block	1		;Sphere Color
statdeg	.block	1		;Degrees of station
cntrdeg	.block	1		;Degrees of center of rotation
shipadd	.block	1		;Ship offset for randomizing bug motion
corxl	.block	1		;Corrected ship X LSB for tracking
corxh	.block	1		;Corrected ship X MSB for tracking
bugxv	.block	1		;Bug X velocity
bugyv	.block	1		;Bug Y velocity
bugim	.block	1		;Bug picture number
websav	.block	nweb	;Save spinner statuses through orr code
webstim	.block	nweb	;When do the dragonflies drop?
fishmys	.block 	1		;Mystery Value

    ;Space Local RAM must not go beyond $0800
    orgchk($0800,$)
    .org ?localpc


enem0	lda	initcz
		ifmi
			lda	#00
			sta	initcz
			jmp	?zero
		endif
		lda	webssta+nsph-1			;My cue for return from orr code
		ifeq
			jsr	?back					;Restore all statuses
		endif
		ldy	#00
		sty	fishmys
		ldx	#nweb-1
		begin
			lda	webssta,X
			sta	websav,X
			ifne
				iny
			endif
			dex
		miend
		cpy	#01
		ifeq
			lda	shipst
			ifpl
				ifne
?enm10				jmp	noneleft				;All dying or dead
				endif
			endif
		endif
		jsr	?stdisp
		lda	websnum
		sta	webscur
		begin
			jsr	?hvdisp
			dec	webscur
		miend
		lda	websnum
		sta	webscur
		begin
			jsr	?fsdisp
			dec	webscur
		miend
		lda	frame
		and	#$3F
		ifeq
			lda	bonusa
			sed
			sec
			sbc	#01				;Decrement bonus on the basis of time
			sta	bonusa
			cld
			ifeq
				jsr	?back2
				beq	?enm10
			endif
		endif
		lda	six					;Main 'six' counter section, counts 0-5
		clc	
		adc	#01
		cmp	#06
		ifeq
			lda	#00				;If it is at 6, reset it
		endif
		sta	six
		ldy	difmx3
		lda	frame
		and	?framask,Y
		ifeq
			jsr	getrand
			and	#03
			cpy	#02
			ifcc
				tay	
				lda	?enm100,Y
				sta	corxh
				lda	?enm110,Y
			else
				tay
				lda	?enm105,Y
				sta	corxh
				lda	?enm115,Y
			endif
			sta	corxl
		endif
		lda	sndcue
		ifeq
			lda	#$20
			sta	sndcue
			lda	#snd_c5				;Bonus Tick Sound
			jsr	dosound
		endif
		lda	sphrad
		cmp	#$80
		ifcs
			jsr	?orbit2
		else
			cmp	#$30
			ifcc
				lda	sndcue+1
				ifeq
					lda	#08
					sta	sndcue+1
					lda	#snd_d1
					jsr	dosound			;Fish Hatch Sound
				endif
			endif
		endif
		rts	

?enm100	.byte $02,$03,$05,$06
?enm105	.byte $03,$04,$05,$06
?enm110	.byte $F0,$20,$E0,$10
?enm115	.byte $00,$00,$00,$00

;*******************************************
; Issue #104 - Make Fishoids Easier
;*******************************************
?framask	.byte $7F,$7F,$3F,$3F,$1F,$1F,$1F
;?framask	.byte $7F,$3F,$1F,$0F,$FF
;*******************************************

;**************************************************
	.sbttl "Initialize All Spheres"
;**************************************************
?zero	lda	difcty			;Multiply times nweb
		; cmp	#03
		; ifcs
			; lda	#03				;Four waves
		; endif
		sta	difmx3
		lda	#$80
		sta	sphrxl
		sta	corxl
		lda	#04
		sta	sphrxh
		sta	corxh
		lda	#$10
		sta	statdeg
		lda	#00
		sta	stbflg
		sta	shipadd
		sta	sphrad
		sta	sphrst
		sta	six
		lda	#$50
		sta	bonusa
		jsr	?back2
		ldx	difmx3
		ldy	?nhives,X
		lda	?ztabidx,X
		tax	
		sty	websnum
		begin
			lda	?zerotab,X
			sta	webss2,Y				;Initial load of positional info
			lda	#01
			sta	webssta,Y
			dex	
			dey	
		miend
		lda	#$FF
		sta	webssta+nsph-1
		rts	

;Number of fishoids at this Diffcty		
?nhives	    .byte $05,$05,$07,$07,$07,$08,$08
;Index into init table
?ztabidx	.byte $05,$05,$0D,$0D,$0D,$16,$16
;Init data for webss
?zerotab	.byte $00,$2A,$55,$80,$AA,$D5
			.byte $00,$20,$40,$60,$80,$A0,$C0,$E0
			.byte $00,$1D,$39,$56,$72,$8F,$AB,$C8,$E4

;**********************************************
	.sbttl "Restore Statuses After ORR Code"
;**********************************************
?back2  ldx	#$18
		lda	#00
		begin
			sta	sobjst,X
			dex
		miend
		lda	#00
		sta	sndcue
		sta	sndcue+1
		rts	
		
?back   jsr	?back2
		lda	#$FF
		sta	webssta+nsph-1			;Reset the restore indicator
		lda	difmx3
		ifeq
			lda	#04
			sta	difmx3
		endif
		tay						;Level to Y
		lda	?backdata-1,Y
		sta	temp1
		ldx	websnum
		begin
			lda	websav+nsph,X
			ifne
				cmp	#$40
				ifcc
					lda	#01
					sta	webssta+nsph,X
					inc	temp1
					ldy	temp1
					lda	?zerotab,Y
					sta	webss2+nsph,X
					lda	#00
					sta	websseg+nsph,X
					jsr	getrand
					ldy	difmx3
					and	?backdata2,Y
					sta	webstim+nsph,X
					jsr	getrand
					and	#$7F
					sta	websper+nsph,X
				endif
			endif
			dex
		miend	
		lda	#00
		sta	sphrad
		lda	#$80
		sta	statdeg
		rts	
		
?backdata	.byte $06,$0E,$0E
?backdata2	.byte $00,$00,$00,$00,$03

;*********************************************
	.sbttl "Move Bug Position into vdata"
;*********************************************
?movcomp	
        lda	websxl+nsph,X
		sta	vdata
		lda	websxh+nsph,X
		sta	vdata+1
		lda	websyl+nsph,X
		sta	vdata+2
		lda	websyh+nsph,X
		sta	vdata+3
		rts	
		
;*********************************************
	.sbttl "Move vdata into Bug X/Y pos"
;*********************************************
?compmov	
        lda	vdata
		sta	websxl+nsph,X
		lda	vdata+1
		sta	websxh+nsph,X
		lda	vdata+2
		sta	websyl+nsph,X
		lda	vdata+3
		sta	websyh+nsph,X
		rts	
		
;*********************************************
	.sbttl "Move and Display the Station"
;*********************************************
?stdisp	lda	sphrst
		ifne					;Lower Rotation Post
			lda	frame
			and	#01
			clc	
			adc	cntrdeg
			sta	cntrdeg
			cmp	#$19
			ifcs
				dec	cntrdeg
			endif
			inc	sphrad
			ifeq
				dec	sphrad
			endif
			lda	#02
			clc	
			adc	statdeg
			sta	statdeg
			lda	statdeg
			jsr	?stsup0
			sta	statyh
			lda	temp2
			sta	statyl
			lda	statdeg
			clc	
			adc	cntrdeg
			jsr	?stsup0
			sta	sphryh
			lda	temp2
			sta	sphryl
			inc	sphrad			;Timer for bug starts
			lda	sphrad
			ifeq
				lda	#$FF
				sta	sphrad
			endif
			rts
		endif
		ldy	statdeg			;Upper rotation pole
		cpy	#$40
		ifcc
			tya	
			jsr	sin				;Make station move to resting point
			sta	temp2
			lda	#00
			ldx	#01
			begin
				asl	temp2
				rol	A
				dex	
			miend
			sta	temp2+1
			jsr	?hvsup1			;Multiply Result to get 0 to 6 motion
			clc	
			adc	#03
			sta	sphryh
			lda	temp2
			sta	sphryl
			ldx	#03
			begin
				lda	sphrxl,X
				sta	statxl,X
				dex
			miend
			lda	#02
			clc	
			adc	statdeg
			sta	statdeg			;Move ship down to player in a half second
		else_pl
			inc	sphrad
			lda	sphrad
			cmp	#$10
			ifeq
				ldy	difmx3
				ldx	websnum			;Start dragonflies
				begin
					lda	#01
					sta	webssta+nsph,X		;Orbit motion
					lda	#00
					sta	websseg+nsph,X		;No extra bump as if just hit
					lda	webss2,X
					sta	webss2+nsph,X		;Every 20 degrees in spread
					jsr	getrand
					and	?dropmask,Y			;Removable
					sta	webstim+nsph,X		;Drop from formation in first 4 rounds
					jsr	getrand
					and	#$7F
					sta	websper+nsph,X		;Drop from an arbitrary angle
					dex	
				miend
			else_mi
				cmp	#$1B
				ifcs
					lda	#00				;Remove spheres
					ldx	websnum
					begin
						sta	webssta,X
						dex
					miend
					lda	#01
					sta	sphrst
					lda	#$20
					sta	statdeg
					lda	#00
					sta	cntrdeg
				endif
				
			endif
		endif
		lda	sphrad
		cmp	#$10
		ifcc
			lda	#$AB
			sta	sphcol
			lda	#00
			sta	sphscl
			lda	#$FF
		else_ne
			sta	sphscl
			lda	#$FF
			sta	sphcol
			lda	#00
			ldx	#05
			begin
				asl	sphscl
				rol	A
				dex
			miend
			adc	#$FB
		endif
		sta	sphsch
		rts	

;Number of bits against a random defines how many rounds before drop to player
?dropmask	.byte $03,$03,$03,$03,$02,$02,$02

;******************************************
	.sbttl "Support for stdisp"
;******************************************
?stsup0	jsr	sin
		sta	temp2
		lda	#00
		asl	temp2
		rol	A
		asl	temp2
		rol	A
		cmp	#02
		ifcs
			ora	#$FC
		endif
		sta	temp2+1
		lda	temp2
		clc	
		adc	#$80
		sta	temp2
		lda	temp2+1
		adc	#04
		rts
		
;**************************************************
	 .sbttl "Move and Display Sphere Hive Bombs"
;**************************************************	
?hvdisp	ldx	webscur
		lda	webssta,X
		ifeq
			rts
		endif
		lda	webss2,X
		jsr	?orsup1
		lda	sphscl
		sta	scalef
		lda	sphsch
		sta	scalef+1
		jsr	posvc2
		ldy	six
		lda	?mds115,Y
		ldx	sphcol
		cpx	#$FF
		ifeq
			ora	#08
		endif
		tax	
		lda	sphcol
		jsr	vgadd2
		ldx	six
		ldy	?mds120,X
		lda	cerstf,Y
		ldx	cerstf+1,Y
		jsr	vgadd2
		lda	sphrad
		cmp	#$10
		ifcc
			lda	#$40
			sta	temp4
			lda	#08
			sta	temp4+1
			lda	#00
			sta	hitpts				;No points for shooting spheres
			ldx	webscur
			jsr	hitwebs
			ldx	webscur
			lda	webssta,X
			ifmi
				lda	#snd_c3
				jsr	dosound
				ldx	webscur
			endif
			lda	#01					;Shots can't harm spheres
			sta	sobjst,X
		endif
		lda	webss2,X
		clc	
		adc	#02
		sta	webss2,X		;Rotate +3 degrees
		rts	
		
?mds115	.byte statsp,statsp+4,statsp,statsp+4,statsp,statsp+4
?mds120	.byte 0,4,2,2,4,0

;***********************************************
	.sbttl "Support for the Hive Display"
;***********************************************
?hvsup1	sta	temp3+1		;Multiply temp2 times 1.5
		lda	temp2
		clc	
		bit	temp3+1
		ifmi
			sec
		endif
		ror	temp3+1
		ror	A
		clc	
		adc	temp2
		sta	temp2
		lda	temp3+1
		adc	temp2+1
		sta	temp2+1
		rts
			
?hvsup2	lda	temp2
		clc	
		adc	sphrxl,Y
		sta	vdata,Y
		lda	temp2+1
		adc	sphrxh,Y
		sta	vdata+1,Y
		rts
		
;***************************************************
	.sbttl "Display and move the Fish Bombs"
;***************************************************			
?fsdisp	lda	#00
		sta	perm5
		lda	#$50
		sta	temp4
		lda	#$10
		sta	temp4+1
		ldx	webscur
		;Motion stuff now, depends on current status
		lda	webssta+nsph,X
		ifeq
			rts				;If it's dead, don't do anything
		endif
		cmp	#01
		ifeq
			jsr	?orbit		;Status = 01 = Orbiting the station
		else
			inc	fishmys
			cmp	#02
			ifeq
				jsr	?track		;Status = 02 = Diving to the player
			else
				cmp	#03
				ifeq
					jsr	?spiral		;Status = 03 = Spinning around in circles
				else
					cmp #04
					ifeq
						jsr ?top
					else
						jsr	?explode	;Status = FF = Dying!
					endif
				endif
			endif
		endif
		ldx	webscur
		lda	webssta+nsph,X
		ifeq
			rts
		endif
		lda	temp4
		ifne
			lda	webssta+nsph,X
			sta	perm1
			cmp	#$40
			ifcc
				tay
				lda	?fishpts-1,Y
			else_ne
				lda	#00
			endif
			sta	hitpts
			txa	
			clc	
			adc	#nsph
			tax	
			lda	vdata+2
			sec	
			sbc	#$C0				;Make sure can't hit it if behind player
			lda	vdata+3
			sbc	#09
			ifcc
				jsr	hitwebs
				ldx	webscur
				lda	webssta+nsph,X
				ifmi
					lda	perm1
					cmp	#04
					ifcc
						cmp	#01
						ifeq
							lda	websseg+nsph,X
						 	ifeq
						 		lda	#$0C
								sta	websseg+nsph,X		;Faster speed for awhile
							endif
							lda	#snd_d3
							jsr	dosound			;Fishoid Hit sound
							ldx	webscur
							lda	#01
						else_pl
							sta	webss2+nsph,X
							lda	#snd_d4			;Destroy Fish
							jsr	dosound
							ldx	webscur
							lda	#$40
						endif
					endif
					sta	webssta+nsph,X
				endif
			endif
		endif
		lda	#$80
		sta	scalef
		lda	#$FE
		sta	scalef+1
		lda	perm5
		ifne
			lda	#00
			sta	temp2+1
			lda	perm5
			ldx	#03
			begin
				asl	A
				rol	temp2+1
				dex
			miend
			sta	temp2
			lda	#$80				;Generate growing scale
			clc	
			adc	temp2
			sta	scalef
			lda	#$F2
			adc	temp2+1
			sta	scalef+1
		endif
		jsr	posvc2
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#$40
		ifcs
			jmp	?fsexpl
		endif
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#01
		ifeq
			ldy	websseg+nsph,X
			ifne
				lda	#02
			endif
		endif
		sta	perm4
		lda	bugim
		and	#08
		lsr	A
		ora	#statsp
		tax	
		sta	perm1					;perm1 is the xflip and sparkle
		ldy	perm4
		lda	?fishcol1-1,Y
		jsr	vgadd2
		lda	bugim
		sta	temp1
		and	#07
		asl	A
		tay	
		lda	cerpup,Y
		ldx	cerpup+1,Y
		jsr	vgadd2
		ldy	perm4
		lda	?fishcol2-1,Y
		ldx	perm1
		jsr	vgadd2
		lda	bugim
		and	#$30
		lsr	A
		lsr	A
		lsr	A
		sta	temp1
		lda	bugim
		and	#07
		asl	A
		asl	A
		asl	A
		adc	temp1
		tay						;Matrix of wing positions
		lda	cerwng,Y
		ldx	cerwng+1,Y
		jmp	vgadd2
		rts
		
?fishpts	.byte $01,$10,$10
?fishcol1	.byte $F6,$F7,$F7
?fishcol2	.byte $F3,$8B,$8B

;******************************************************
	.sbttl "Rotate the bugs around the Sphere Center"
;******************************************************
?orbit  lda	webss2+nsph,X
		clc	
		adc	#$40					;Bug points perpendicular to direction of motion
		jsr	?fssup0
		lda	webss2+nsph,X
		jsr	?orsup1
		ldx	webscur
		jsr	?compmov				;Put position in websxylx
		lda	websseg+nsph,X
		ifne
			inc	fishmys
			dec	websseg+nsph,X
			lda	#04
		else_ne
			lda	#01
		endif
		sta	temp1
		clc	
		adc	webss2+nsph,X
		sta	webss2+nsph,X			;Rotation
		lda	shipst
		ifpl
			ifne
				ifvs
					lda	webstim+nsph,X		;Ready to releast a bug?
					ifmi					;yes
?dropfish			    lda	#02
						sta	webssta+nsph,X
						lda	webss2+nsph,X
						adc	#$3F				;Correct for look/move direction
						sta	webss2+nsph,X
						rts	
					else					;nope, decrement it
						dec	webstim+nsph,X
					endif
				endif
				lda	webstim+nsph,X
				ifmi
					lda	webss2+nsph,X
					and	#$7F
					cmp	websper+nsph,X		;Check for degree release
					bcs	?dropfish
				endif
				lda	websseg+nsph,X
				ifeq
					lda	temp1
					cmp	#04
					beq	?dropfish
				endif
			endif
		endif
		rts
			
?orbit2	ldy	gamedif
		lda	fishmys
		cmp	?orbval,Y
		ifcc
			ldx	websnum
			begin
				lda	webssta+nsph,X
				cmp	#01
				ifeq
					lda	websseg+nsph,X
					ifeq
						jmp	?dropfish
					endif
				endif
				dex	
		      miend
		endif
		rts
			
?orbval	.byte $01,$01,$02,$02,$03

;*******************************************
	.sbttl "Support for Orbit"
;*******************************************
?orsup0	sta	temp2
		sta	temp3
		lda	#00
		asl	temp2
		rol	A
		asl	temp2
		rol	A
		bit	temp3
		ifmi
			ora	#$FC
		endif
		sta	temp2+1
		rts
		
;*******************************************
	.sbttl "Support for Orbit"
;*******************************************	
?orsup1	sta	perm1
		jsr	cos
		jsr	?orsup0			;Multiply by radius quickly
		jsr	?hvsup1
		ldy	#00
		jsr	?hvsup2			;Store away to vdata
		lda	perm1
		jsr	sin
		jsr	?orsup0
		ldy	#02
		jsr	?hvsup2
		rts	
		
;********************************************
	.sbttl "Dragonfly Tracks the Player"
;********************************************
?track  jsr	?movcomp
		lda	difmx3
		and	#03
		tay	
		lda	webss2+nsph,X
		cmp	#$C0
		ifcs
			lda	?fishrot,Y			;If pointing up, turn down by fastest method
		else_ne					;Turn to right
			cmp	#$80
			ifcs
				lda	?frotl,Y			;Turn to left
			else_ne
				lda	vdata				;Otherwise turn to chase player
				sec	
				sbc	corxl
				lda	vdata+1
				sbc	corxh				;Randomness added to motion
				ifcs					;Bug is to right of ship
					lda	webss2+nsph,X
					cmp	?frotr,Y
					ifcc
						lda	?fishrot,Y
					else_pl
						lda	#$7F
						sta	webss2+nsph,X		;Persue as nearly a horizontal course as possible
						lda	#00
					endif
				else_pl
					lda	webss2+nsph,X
					cmp	?fishrot,Y
					ifcc
						lda	#00
						sta	webss2+nsph,X
					else_pl
						lda	?frotl,Y
					endif
				endif
			endif
		endif
		sta	temp1				;Suggested direction change
		lda	vdata+2
		sec	
		sbc	#$C0
		lda	vdata+3
		sbc	#08
		ifcs					;Bottom of screen
			lda	webss2+nsph,X
			adc	#$3F
			ifmi					;Turn to point straight down
				lda	?frotl,Y
			else_ne
				cmp	?frotr,Y
				ifcs
					lda	#00
				else_eq
					lda	?fishrot,Y
				endif
			endif
			sta	temp1
		endif
		lda	temp1
		clc
		adc	webss2+nsph,X
		sta	webss2+nsph,X
		jmp	?fsdegr
		
?frotr		.byte $7A,$7A,$74,$6E,$6E,$6E,$6E
?frotl		.byte $FA,$FA,$F4,$EE,$EE,$EE,$EE
?fishrot	.byte $06,$06,$0C,$12,$12,$12,$12
		
;********************************************************
	.sbttl "Control the Exploding Dragonfly"
;********************************************************
?explode	
        lda	webssta+nsph,X
		clc	
		adc	#01
		sta	webssta+nsph,X
		ifmi
			lda	#00
			sta	webssta+nsph,X
			rts
		endif
		lda	webssta+nsph,X
		cmp	#$60
		ifcc
			and	#$3F
			cmp	#$11
			ifcs
				lda	#$60
				sbc	webssta+nsph,X
			endif
			tax	
			lda	?fex100,X
			sta	perm5
		endif
		ldx	webscur
		jsr	?movcomp
		lda	#00
		sta	perm4
		sta	temp4
		rts
			
?fex100	.byte $00,$94,$B4,$C6,$D4,$DE,$E6,$ED
		.byte $E6,$DB,$CB,$D9,$E3,$ED,$F5,$FB,$FF

;********************************************************
	.sbttl "Dragonfly draws a constricting spiral"
;********************************************************
?spiral	jsr	?movcomp
		lda	webss2+nsph,X
		sta	perm1					;Angle of dragonfly
		lda	websseg+nsph,X
		ifpl						;Rotation Number
			lda	webstim+nsph,X			;Non zero if in a straight horizontal
			ifne
				dec	webstim+nsph,X
				ifeq
					inc	perm1					;Finished straight section
					ifpl
						dec	websseg+nsph,X			;Rotation complete
					endif
				endif
			endif
		endif
		lda	difmx3
		and	#03
		tay	
		lda	webss2+nsph,X
		cmp	#$C0
		ifeq
			;**********************************************
			; Issue #104 - Fishoids too Difficult
			;**********************************************
			lda websseg+nsph,X	
			ifeq
				lda #04
				sta	webssta+nsph,X				;Tell to return to top
				sta websseg+nsph,X	
				lda #$C0						;Point up?
				jmp	?fsdegr
			endif
			;**********************************************
			;**********************************************
			lda	vdata+2
			sbc	?sphheight,Y				;Begin backturn at this height
			lda	vdata+3
			sbc	?sphturn,Y
			ifcc
				inc	perm1
				jsr	?fishsnd
			endif
		endif
		lda	webss2+nsph,X
		cmp	#$40						;Going down, need to turn
		ifeq
			sec
			lda	websseg+nsph,X
			ifpl
				lda	vdata+2
				sbc	?fturn1,Y
			else
				lda	vdata+2
				sbc	?fturn2,Y
			endif
			lda	vdata+3
			sbc	#09
			ifcs
				inc	perm1					;Begin turn
				jsr	?fishsnd
			endif
		endif
		lda	perm1
		and	#$3F
		ifne						;In process of turning
			cmp	?ftrnlen,Y
			lda	perm1
			ifcs
				clc
				adc	?fishrot,Y				;cs
				and	#$C0					;Stop at 0,40,80,C0
				sta	perm1
				and	#$40
				ifeq
					lda	websseg+nsph,X			;Still need to get rotation?
					ifpl
						bit	perm1					;Yes, we will set a side timer
						ifmi
							clc
							adc	#04
						endif
						sta	temp1
						lda	difmx3
						asl	A
						asl	A
						asl	A
						adc	temp1
						tay
						lda	?fvar,Y
						sta	webstim+nsph,X
					endif
				endif
			else_ne
				adc	?fishrot,Y
				sta	perm1
			endif
		endif
		lda	perm1
		sta	webss2+nsph,X
		lda	websper+nsph,X				;Incorporate reflection
		ifeq
			lda	perm1
		else
			lda	#$80
			sec
			sbc	perm1
		endif
		jmp	?fsdegr

;********************************************
	.sbttl "Dragonfly goes back to top"
;********************************************
?top  	jsr	?movcomp		
		lda	vdata+3
		cmp #05
		ifcc
			;drop at player again
			lda #02
			sta webssta+nsph,X
			lda #0
			sta websseg+nsph,X			;Mark to turn
		endif
		lda #$C0
		jmp	?fsdegr


?rot0	=	6
?rot1	=	6
?rot2	=	12
?rot3	=	12
?rot4	=	12
?rot5	=	18
?rot6	=	18
		
?ftrnlen	.byte $40-?rot0,$40-?rot1,$40-?rot2,$40-?rot3,,$40-?rot4,$40-?rot5,$40-?rot6
?sphheight	.byte $00,$40,$00,$00,$00,$00,$00
?sphturn    .byte $08,$09,$09,$09,$09,$09,$09
?fturn1	    .byte $C0,$90,$90,$90,$90,$90,$90
?fturn2	    .byte $60,$38,$38,$38,$38,$38,$38

;Turn Lengths, sorta messy
;                   0   1   2   3	4   5   6   	;Loops Remaining
?fvar		.byte $01,$02,$03,$04,$04,$04,$04		;Difcty = 0, Moving Left
			.byte $01,$02,$04,$06,$06,$06,$06		;Difcty = 0, Moving Right
			.byte $01,$02,$03,$04,$04,$04,$04		;Difcty = 1, Moving Left
			.byte $01,$02,$03,$06,$06,$06,$06		;Difcty = 1, Moving Right
            .byte $01,$02,$02,$03,$03,$03,$03		;Difcty = 2, Moving Left
			.byte $01,$02,$03,$04,$04,$04,$04		;Difcty = 2, Moving Right
			.byte $01,$01,$02,$03,$03,$03,$03		;Difcty = 3+ Moving Left
			.byte $01,$02,$03,$04,$04,$04,$04		;Difcty = 3+ Moving Right


?fishsnd	
        lda	sndcue+1
		ifeq
			lda	#$18
			sta	sndcue+1
			lda	#snd_d5
			jmp	dosound
		endif
		rts	
		
;*****************************************************
	.sbttl "Generates Velocity from Fish Direction"
;*****************************************************
?fsdegr	sta	perm1
		jsr	?fssup0
		lda	perm1
		jsr	cos				;New X Velocity
		sta	temp1
		ldy	difmx3
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#03
		ifne
			ldx	?fsd60,Y
			bpl	?fsd5
		endif
		ldx	?fsd65,Y
?fsd5	jsr	?fssup1
		sta	bugxv				;Save X Velocity
		lda	perm1
		jsr	sin
		sta	temp1
		ldy	difmx3
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#02
		ifeq
			lda	vdata+3
			cmp	#07
			bcs	?fsd10
			ldx	?fsd70,Y
		else_ne
?fsd10		ldx	?fsd75,Y			;Move slow when low down
		endif
		jsr	?fssup1				;Calculate Y velocity
		sta	bugyv				;Save Y velocity
		ldx	webscur
		lda	bugxv
		ifmi
			dec	vdata+1
		endif
		clc	
		adc	vdata
		sta	vdata
		ifcs
			inc	vdata+1
		endif
		lda	vdata+1
		ifmi
			lda	#00
			sta	vdata
			sta	vdata+1
		endif
		cmp	#02					;Left side screen edge
		ifcc
			lda	websper+nsph,X
			eor	webss2+nsph,X
			ifne
				jsr	?fsrefl
			endif
		endif
		lda	vdata+1
		cmp	#09
		ifcs
			lda	#08
			sta	vdata+1
			lda	#$FF
			sta	vdata
		endif
		cmp	#07
		ifcs
			lda	websper+nsph,X
			eor	webss2+nsph,X
			ifeq
				jsr	?fsrefl
			endif
		endif
		lda	bugyv
		ifmi
			dec	vdata+3
		endif
		clc	
		adc	vdata+2
		sta	vdata+2
		lda	vdata+3
		adc	#00
		sta	vdata+3
		lda	webssta+nsph,X
		cmp	#03
		ifcc			;was ifne
			lda	vdata+2
			sec	
			sbc	#$40
			lda	vdata+3
			sbc	#09
			ifcs
				lda	#$40
				sta	webss2+nsph,X			;Spiral dragonfly at player level
				lda	#03
				sta	webssta+nsph,X
				lda	#04						;Was 3 in original game, number of loops before fish will go back up top now				
				sta	websseg+nsph,X			;Number of circles
				lda	#00
				sta	webstim+nsph,X			;Going down, not horizontally
				lda	vdata
				sbc	#$80
				lda	vdata+1
				sbc	#04
				ifcc
					lda	#$80
				else_mi
					lda	#00
				endif
				sta	websper+nsph,X			;Reflect direction
			endif
		endif
		jsr	?compmov				;Move variables back
		rts	

;X Velocity Divide by 2 of COS
?fsd60		.byte $01,$01,$01,$00,$00,$00,$00 ;,$02
?fsd65		.byte $01,$01,$00,$01,$00,$00,$00 ;,$03

;Y Velocity Divide by 2 of SIN
?fsd70		.byte $03,$02,$02,$01,$01,$01,$01 ;,$03
?fsd75		.byte $03,$03,$02,$02,$02,$01,$01 ;,$03

?fcoreg	    .byte $02,$01,$01,$00,$08,$09,$09,$0A
            .byte $0B,$0C,$0C,$0D,$05,$04,$04,$03

;********************************************
	.sbttl "Support for fsdegr"
;********************************************
?fssup0	lsr	A
		lsr	A
		lsr	A
		lsr	A
		tay	
		lda	frame
		and	#03
		asl	A
		asl	A
		asl	A
		asl	A
		ora	?fcoreg,Y
		sta	bugim
		rts

;************************************************
;Returns final velocity in A = temp1 * 75			
?fssup1	lda	temp1	
        cpx	#00
		ifne
			begin						;Removable, div2x
				cmp	#$80
				ror	A
				dex
			miend
			rts
		endif
		sta	temp1
		cmp	#$80
		ror	A						;Multiply times 5
		lsr	A
		sta	temp1+1
		lsr	A
		clc	
		adc	temp1+1					;Multiply times 75
		bit	temp1
		ifmi
			clc
			adc	#$40
		endif
		rts	
		
;******************************************************
	.sbttl "Reflect the Fish Bomb at Edge of Screen"
;******************************************************
?fsrefl	lda	webssta+nsph,X
		cmp	#03
		ifeq
			lda	websseg+nsph,X
			ifmi
				lda	bugim
				eor	#08
				sta	bugim					;Reverse direction of screen image
				lda	webss2+nsph,X
				and	#$80
				eor	#$80
				sta	webss2+nsph,X
			endif
		endif
		rts
		
;**************************************************
	.sbttl "Display the exploding Fish"
;**************************************************		
?fsexpl	lda	#00
		sta	perm2
		lda	webss2+nsph,X
		sta	perm4
		lda	webssta+nsph,X
		and	#$3F
		lsr	A				;Get the explosion frame
		sta	perm1
		sta	perm1+1
		lsr	A
		ror	perm2				;Minus if an odd frame, zero if even
		asl	perm1+1
		asl	perm1+1
		txa	
		and	#04				;Xflip of sequence
		ora	#statsp
		sta	temp2
		ldy	#00
		ldx	perm4				;Choreography type
		lda	perm5
		ifne
			jsr	getrand
			and	#$7F
			ora	#$20				;Get random color and random restricted intensity
			sta	(vglist,Y)			;Eye color
			iny
			lda	temp2
			sta	(vglist,Y)
			iny	
			lda	cerbng
			sta	(vglist,Y)
			iny
			lda	cerbng+1
			sta	(vglist,Y)
			iny					;Draw the start at explosion center
		endif
		lda	perm1
		lsr	A
		eor	#$0F
		ifeq
			lda	#01
		endif					;Get non-zero intensity
		asl	A
		asl	A
		asl	A
		asl	A
		ora	#$0A				;Body color
		sta	(vglist,Y)			;Fading intensity
		iny	
		lda	temp2
		sta	(vglist,Y)
		iny	
		clc	
		lda	vdata+2
		adc	#$80
		pha					;Get scale for explosion
		lda	vdata+3
		adc	#$FE
		sta	temp1
		clc	
		ifmi
			sec
		endif					;Good place for routineing, removeable
		pla	
		ror	temp1
		ror	A
		lsr	A
		tax	
		lda	fullog,X
		sta	(vglist,Y)			;Linear Scale
		iny	
		lda	#$76
		sec	
		sbc	temp1
		sta	(vglist,Y)
		iny					;Binary scale
		lda	perm1
		and	#$FE
		asl	A
		tax					;Four bytes of data every other frame
		bit	perm2
		ifpl
			begin
				lda	?dfh100,X
				sta	(vglist,Y)
				iny	
				inx	
				tya	
				and	#03
			eqend
		else_eq
			begin
				lda	?dfh100,X			;Cc from loop end or getting here
				clc	
				adc	?dfh100+4,X
				sta	temp1
				lda	?dfh100+1,X
				adc	?dfh100+5,X
				and	#$DF
				cmp	#$10
				ifcs
					ora	#$20				;Sign extend to $3f in high byte
				endif
				lsr	A
				ror	temp1
				pha	
				lda	temp1
				sta	(vglist,Y)
				iny					;Get averaged vector	
				pla	
				sta	(vglist,Y)
				iny	
				inx	
				inx	
				tya
				and	#02
			eqend					;Do it twice
		endif
		lda	perm1
		tax	
		and	#03
		asl	A
		sta	perm3				;Rotation sequence of four
		lda	?dfh125,X
		sta	perm3+1			;Rotation sequence of six
		ldx	#00
		beq	?dfh20			;No vector first time around
		begin
			lda	#00	
			sta	temp2
			sta	temp2+1
			stx	temp1
			ldx	perm4+1
			lda	perm4
			ifmi
				lda	#00
				sec	
				sbc	?dfh115,X
			else
				lda	?dfh115,X
			endif
			ldx	temp1
			asl	A
			rol	temp2
			asl	A
			rol	temp2
			sta	(vglist,Y)
			iny	
			lda	temp2
			cmp	#02
			ifcs
				ora	#$1C
			endif
			sta	(vglist,Y)
			iny	
			stx	temp1
			ldx	perm4+1
			lda	?dfh110,X
			ldx	temp1
			asl	A
			rol	temp2+1
			asl	A
			rol	temp2+1
			sta	(vglist,Y)
			iny	
			lda	temp2+1
			cmp	#02
			ifcs
				ora	#$1C				
			endif
			sta	(vglist,Y)
			iny	
?dfh20		lda	?dfh120,X
			clc	
			ifmi
				and	#$7F
				adc	perm3
			else_ne
				adc	perm3+1				;Add in rotation cycle
			endif
			stx	temp1
			tax	
			lda	cerbng,X
			sta	(vglist,Y)
			iny	
			lda	cerbng+1,X
			sta	(vglist,Y)
			iny	
			ldx	temp1
			inx	
			lda	?dfh105,X
			sta	perm4
			and	#$3F
			clc	
			adc	perm1+1
			sta	perm4+1
			cpx	#08
		csend
		tya	
		clc	
		adc	vglist
		sta	vglist
		ifcs
			inc	vglist+1
		endif							;Removeable
		rts	

?dfh100	.byte $00,$00,$00,$00,$F4,$1F,$C1,$1F,$E8,$1F,$84,$1F,$DB,$1F,$49,$1F
		.byte $CE,$1F,$10,$1F,$C1,$1F,$D9,$1E,$B4,$1F,$A4,$1E,$A6,$1F,$71,$1E
		.byte $98,$1F,$40,$1E,$8A,$1F,$11,$1E,$7C,$1F,$E4,$1D,$6D,$1F,$B9,$1D
		.byte $5E,$1F,$90,$1D,$4F,$1F,$69,$1D,$40,$1F,$44,$1D,$30,$1F,$21,$1D
		.byte $20,$1F,$00,$1D
		
?dfh105	.byte $00,$00,$01,$02,$03,$82,$81,$80
		
?dfh110	.byte $00,$00,$00,$00
		
?dfh115	.byte $00,$00,$00,$00,$03,$03,$0F,$00,$06,$06,$1C,$00,$09,$09,$29,$00
		.byte $0B,$0C,$34,$00,$0E,$0F,$3F,$00,$10,$12,$48,$00,$12,$15,$51,$00
		.byte $14,$18,$58,$00,$16,$1B,$5F,$00,$18,$1E,$64,$00,$1A,$21,$69,$00
		.byte $1B,$24,$6C,$00,$1D,$27,$6F,$00,$1E,$2A,$70,$00,$1F,$2D,$71,$00
		.byte $20,$30,$70,$00,$21,$33,$6F,$00,$22,$36,$6C,$00,$23,$39,$69,$00
		.byte $23,$3C,$64,$00,$24,$3F,$5F,$00,$24,$42,$58,$00,$24,$45,$51,$00
		.byte $24,$48,$48,$00,$24,$4B,$3F,$00,$24,$4E,$34,$00,$24,$51,$29,$00
		.byte $23,$54,$1C,$00,$23,$57,$0F,$00,$22,$5A,$00,$00,$21,$5D,$F1,$00
		
?dfh120	.byte $9A,$02,$0E,$0E,$9A,$02,$9A,$0E
?dfh125	.byte $00,$02,$04,$06,$08,$0A,$00,$02,$04,$06,$08,$0A,$00,$02,$04,$06
		.byte $08,$0A,$00,$02,$04,$06,$08,$0A,$00,$02,$04,$06,$08,$0A,$00,$02
