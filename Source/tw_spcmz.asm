

;***********************************************
    .TEXT ".TWSPCMZ."
;***********************************************
	.title  "Space Maze - Display the Maze"
	.sbttl  "Space Maze Constants"
;***********************************************
;* Some Constants 
nweb		=	12d	            ;Number of web spinners
maxlin		=	(nweb+1)*8	    ;Maximum number of line vertices,8 per spinner
maxinv		=	$10
maxhitln	=	$0F				;Maximum number of hits a line may take
preptim		=	$40				;One second of prepare time

	.sbttl "RAM Defines"
	
?localpc = $
  .org mazexl	;$0600  

linlst		.block	maxlin		;The vertex list for the maze, YX
linhit		.block	maxlin		;The number of hits on each line
linx		.block	maxlin		;The X value of each vertex, unpacked
liny		.block	maxlin		;The Y value of each vertex, unpacked

linstr		.block	nweb+1		;The line start vertex in linlst
linend		.block	nweb+1		;The line end vertex, started at linstr's value plus one
websded		.block	1			;Are all the spinners gone?
state		.block	1			;0 indicates spinners drawing maze
								;1 indicates maze being traversed
smframe		.block	2			;Two bytes timer
finish		.block	1			;Loop terminator
numhit		.block	1			;Use to determine maze line colors
color		.block	1			;Color of current spinner
extrasp		.block	1			;If non-zero, spinners are kept in circulation
three		.block	1			;Three frame timer
maplen		.block	1			;Length of scrolling maze
plfspd		.block	2			;Playfield speed
plfadd		.block	1			;Playfield acceleration
distl		.block	1			;Low byte of three byte dist
difmx3		.block	1			;Difficulty maxing out at number of waves
numinv		.block	1			;Number of invisible lines this wave
smzlvl		.block  1           ;The level for the Space Maze (difcty limited to 0-3)
ilinesz     .block  1
ilines      .block	maxinv-1    ;First vertex of invisible line
sencom		.block	11d 		;RAM used for sentinel startup
websave		.block	nweb+1		;Save spinners statuses through ORR code


;More Cerny Stuff (Space Maze)
webmult     .block  1       ;difcty*nweb(difcty*12), Used for Array Access
dist        .block  2       ;Two Byte Amount of Maze Traversed
linscal     .block  1       ;Overall Linear Scale of Maze
linmult     .block  1       ;Percent of Full Size for Maze
prepare     .block  1       ;Gives prepare time for maze scroll

    ;Space Maze Local RAM must not go beyond $0800
    orgchk($0800,$)

    .org ?localpc
 
;****************************************************************
;* Enemy Space Maze Main Routine - called from mainline         *
;****************************************************************
enem2	lda	initcz				;Do we need to init wave?
		ifmi					;yes
			lda	#00			
			sta	initcz			;Not next time
			jmp	initsm			;Do it!
		endif
		lda	#00
		sta	websded			;Reset Dead Spinner Count
		lda	webssta
		ifeq
			jsr	mz3trav			;Set up traversal mode
		endif
		lda	#nweb-1
		sta	webscur			;Loop through each spinner
		begin
			jsr	dospinner
			dec	webscur
		miend
		lda	#01
		sta	bonusa
		lda	state				;Web State
		ifeq					;Drawing Maze now
			lda	websded			
			cmp	#nweb				;Are all Spinners Dead or off screen?
			ifeq					;yes
				jsr	mz3trav			;Lets start traversing the maze then
			endif
			lda	smframe
			and	#07
			ifeq
				lda	extrasp			;Are spinners still dropping?
				ifne					;yes
					dec	extrasp			;One step closer to not dropping
				endif
			endif
			lda	shipst			;Is ship exploding? 80=yes
			ifne
				bpl	?emt10
			endif
			lda	#00				;Ship is dead
			sta	extrasp			;Stop spinners from dropping
?emt10	else					;We are traversing the maze now!
			lda	shipst			;Ship Exploding?
			ifpl					;Nope
				ifne
					lda	prepare			;Prepare time expired yet?
					bne	?emt30			;Nope, don't scroll yet
					lda	smframe
					and	#$0F
					ifeq	
						lda	plfadd			;Get scroll amount(speed)
						clc	
						adc	distl
						sta	distl				;Add it on!!
						ifcs
							inc	difmx3
						endif
					endif
					jsr	m3dist			;Calulate Bonus and Distance
					lda	dist				
					ifpl
						adc	plfspd+1
						sta	dist
						ifmi
							jsr	sentry		;Place some sentry spinners
						endif
						jmp	?emt20
					endif
					adc	plfspd+1
					sta	dist
					ifcs
						inc	dist+1
						lda	maplen			;Are we at the end?
						cmp	dist+1
						ifcc					;yes, finished with Space Maze
							jmp	noneleft
						endif
						jsr	m3scroll
					endif
?emt20				lda	dist+1
					clc	
					adc	#08
					cmp	maplen			;end of the maze?
					ifcs
						lda	plfspd+1
						clc	
						adc	statyl
						sta	statyl
						ifcs
							inc	statyh
						endif
					endif
					jmp	?emt31
?emt30				dec	prepare		;Closer to Scrolling
				endif
			endif
?emt31		lda	dist
			lsr	A
			lsr	A			;Distance/4
			tax	
			ifeq
				lda	#-1
			else
				lda	qrtlog,X
			endif
			sta	linmult
			lda	fullog2-1,X
			sta	linscal
		endif
		;Here for both states of Maze!
		inc	smframe
		ifeq
			inc	smframe+1
		endif
		lda	shipst			;Ship Exploding?
		ifmi					;Yes
			ldx	#nweb-1
			lda	state				;Web State?
			ifne					;Maze Being Traversed now
				lda	dist+1
				cmp	maplen			;At end of maze?
				ifcs
					lda	#$40
					sta	lauen				;Move on!
				endif
				begin
					lda	webssta,X
					sta	websave,X
					dex
				miend
				bmi	?emt40
			endif
			lda	#$80
			begin
				sta	websave,X
				dex
			miend
		endif
?emt40	lda	smframe
		and	#$0F
		tax	
		lda	t_hitcol,X
		sta	numhit
		lda	three
		clc	
		adc	#01
		cmp	#03
		ifeq
			lda	#00
		endif
		sta	three
		jsr	mzshot
		jsr	mzship
		rts	
		
t_hitcol	.db $01,$08,$05,$0C,$03,$0A,$06,$0D,$02,$09,$05,$0C,$04,$0B,$07,$0E

;******************************************************
;* Calculate Distance along maze and Bonus Value      *
;******************************************************
m3dist	lda	distl				;Distance Along Maze LSB
		sta	temp1
		lda	difmx3
		ldx	smzlvl
		sec	
		sbc	t_smbonus,X
		asl	temp1
		rol	A
		asl	temp1
		rol	A
		jsr	decimal
		lda	temp7				;Decimal Conversion LSB
		ifeq
			lda	#01
		endif
		sta	bonusa
		lda	distl				;Distance along maze LSB
		sta	temp1
		lda	difmx3
		ldx	#02
		begin
			lsr	A
			ror	temp1
			dex
		miend
		sta	temp1+1
		lda	distl				;Distance along maze LSB
		ldy	gamedif             ;incdif ranges from 0-2
		ifeq
			lda	distl				;Get distance LSB
			sec	
			sbc	temp1
			sta	plfspd
			lda	difmx3
			sbc	temp1+1
			sta	plfspd+1
		else
			lda	distl
			sta	plfspd
			lda	difmx3
			sta	plfspd+1
?dis10		cpy	#02
			ifcs
				lda	plfspd
				clc	
				adc	temp1
				sta	plfspd
				lda	plfspd+1
				adc	temp1+1
				sta	plfspd+1
				dey	
				bpl	?dis10
			endif
		endif
		lda	numinv
		clc	
		adc	perm1
		sta	numinv
		rts	
	
;**************************************************
;* Initialize Space Wave                          *
;**************************************************	
initsm	lda	difcty
		cmp	#03			;Which bracket?
		ifge
			lda	#03			;Max at 3
		endif
		tax	
		stx	smzlvl		;Level 0-3
		asl	A
		asl	A
		sta	temp1
		asl	A
		adc	temp1		;X12 for array access
		sta	webmult     ;(difcty*12)
		lda	#nweb-1		;Number of spinners
		tax	
		clc	
		adc	webmult		;X12 for array access
		tay	
		begin
			lda	t_lineoffs,Y
			sta	linstr+1,X		;Line Start vertex
			clc	
			adc	#01
			sta	linend+1,X		;Line End vertex
			lda	#00
			sta	webssta,X
			lda	#$80
			sta	websave,X
			dey	
			dex	
		miend
		lda	#00
		sta	linstr
		sta	state				;Set state to Web 'Drawing' Mode
		sta	numinv
		sta	dist
		sta	dist+1
		sta	smframe
		sta	smframe+1
		sta	numhit
		sta	distl				;Reset Distance along maze to zero
		lda	#01
		sta	linend
		lda	#$50                ;Reset Bonus counter
		sta	bonusa			    ;5000 Bonus
		lda	#$70
		sta	webssta
		sta	webssta+1
		lda	#00
		sta	websxl
		sta	websxl+1
		sta	websxh
		sta	websyl
		sta	websyl+1
		sta	websyh
		sta	websyh+1
		sta	sndcue              ;Clear the soundcue
		lda	#$0A
		sta	websxh+1
		jsr	loadlines			;Get pointer to line list table, put it in temp1
		lda	t_invptr,X
		sta	temp2
		lda	t_invptr+1,X
		sta	temp2+1
		ldx	smzlvl				;Level 0-3
		ldy	t_slevlen,X			;Load table length
		begin
			lda	(temp1,Y)
			sta	linlst,Y			;Get the vertices
			pha	
			and	#$0F
			sta	linx,Y			;Unpack them and save in X and Y
			pla	
			lsr	A
			lsr	A
			lsr	A
			lsr	A
			sta	liny,Y
			lda	#00
			sta	linhit,Y			;Reset hits on this line
			dey	
		miend
		ldy	t_invlen,X			;Load size of invisible line table
		sty	ilinesz
		begin
			lda	(temp2,Y)
			sta	ilines,Y
			dey
		miend
		ldx	smzlvl				;Level 0-3
		lda	t_maplen,X			;Load the Maze Length
		sta	maplen
		lda	t_plfadd,X			;Load the Playfield Scroll Speed
		sta	plfadd		
		lda	t_extrasp,X
		sta	extrasp				;Set up dropping time
		lda	#04
		sta	spcspd			;Set the stars to this speed
		rts	
		
;******************************************************
;* Load Line List Vertex Table Pointer   
;* Puts start location in temp1     
;* Called from initcz and from within m3scroll  
;******************************************************
loadlines	
		lda	smzlvl          ;Which level 0-3 for levels 0-16 repeat
		asl	A
		tax	
		lda	t_slevel,X
		sta	temp1
		lda	t_slevel+1,X
		sta	temp1+1
		rts	

;********************************************************************
;* Setup Maze Traversal Mode                                        *
;********************************************************************	
mz3trav	lda	#preptim			;prepare time
		sta	prepare				;Set prepare time delay before scroll begins
		lda	#$FF
		sta	linmult
		lda	#$4A
		sta	linscal
		lda	#01
		sta	state				;Set state to 'Traversal Mode'
		lda	#03
		sta	spcspd
		ldx	#nweb-1
		begin
			lda	websave,X
			sta	webssta,X
			lda	#00
			sta	websyl,X
			dex
		miend
		lda	#00
		sta	dist
		sta	distl				;Set distance along maze to zero
		lda	dist+1
		ifne
			dec	dist+1
			ldx	#nweb-1
			begin
				lda	websyh,X
				ifne
					dec	websyh,X
				else
					lda	#$80
					sta	webssta,X
				endif
				dex
			miend
		endif
		jsr	m3scroll
		ldx	smzlvl
		lda	t_smbonus,X
		sta	difmx3
		ldx	#nweb-1
		begin
			ldy	linstr,X
			lda	liny,Y
			cmp	#$0B
			bcc	?mzs10
			iny	
			ifeq
?mzs10			lda	liny,Y
				cmp	#$0B
				bcc	?mzs20
			endif
			tya	
			sta	linend,X
			ifeq
?mzs20			iny	
				tya	
				cmp	linend,X
				bne	?mzs10
			endif
			dex	
			cpx	#01
		eqend
		rts	

;******************************************************
		
m3scroll	
		jsr	loadlines       ;Loads the Spinner table appropriate for this level into temp1
		lda	maplen			;Maze Scroll Position
		sec	
		sbc	dist+1
		ifcc
			lda	#$C0
		else
			cmp	#$0A
			ifcc
				eor	#$FF
				adc	#$0C
			else
				lda	#00
			endif
			asl	A
			asl	A
			asl	A
			asl	A
		endif
		sta	temp2
        
		lda	dist+1
		clc	
		adc	#01
?scr10	cmp	#$0C
		ifcs
			sec	
			sbc	#$0B
			jmp	?scr10
		endif
		asl	A
		asl	A
		asl	A
		asl	A
		sta	temp3
        
		lda	dist+1
		clc	
		adc	#01
		cmp	#$0C
		ifcs
			lda	#$0C
		endif
		sta	temp3+1
        ;----------------------------------------
        ;Start the main maze drawing loop 
		;----------------------------------------
		ldx	#nweb-1
?scr20	ldy	linstr,X        ;Get the start
		lda	#00
		sta	temp4
		lda	(temp1,Y)
		clc	
		adc	temp3
		ifcs
?scr30		sbc	#$B0
			pha	
			lda	#$80
			sta	temp4
			pla	
		else
			cmp	#$C0
			bcs	?scr30
		endif
		cmp	temp2
		ifcs
			sta	linlst,Y
			lsr	A
			lsr	A
			lsr	A
			lsr	A
			sta	liny,Y
			cmp	temp3+1
			ifcs
				lda	temp4
				ora	#$40
				sta	temp4
			else_ne
				cmp	#01
				ifeq
					jsr	doline
				endif
			endif					
		else
			and	#$0F
			ora	#$70
			sta	linlst,Y
			lda	#07
			sta	liny,Y
			bne	?scr60
		endif
?scr35	iny	
		tya	
		bit	temp4
		ifvs
			cmp	linend,X
		else
			cmp	linstr+1,X
		endif
		ifcc
			lda	(temp1,Y)
			clc	
			bit	temp4
			bpl	?scr50
			adc	temp3
			bcs	?scr40
			cmp	#$C0
			ifcs
?scr40			sbc	#$B0
				jmp	?scr55
?scr50			adc	temp3
				ifcc
					cmp	#$C0
					ifcc
?scr55					cmp	temp2
						ifcs
							sta	linlst,Y
							lsr	A
							lsr	A
							lsr	A
							lsr	A
							sta	liny,Y
							cmp	#03
							ifcc
								jsr	doline
							endif
							jmp	?scr35
?scr60						iny	
						endif
					endif
				endif
			endif
			tya
		endif
		sta	linend,X
		dex	
		cpx	#01
		ifne
			jmp	?scr20			;back to start of loop
			;--------------------------------------------------------------------
		endif
		lda	dist+1
		clc	
		adc	#$0B
		sec	
		sbc	maplen			;Maze length
		ifcc
?scr70		lda	#01
		else
			beq	?scr70
			cmp	#$0C
			ifcs
				lda	#$0B
			endif
		endif
		sta	liny
		sta	liny+1
		rts
		
;*************************************	
; Update line hits?
;*************************************
doline	lda	#00
		sta	linhit,Y			;Reset hits on this line
		stx	temp5
		tya	
		ldx	ilinesz
		begin
			cmp	ilines,X
			ifeq
				lda	#$0F
				sta	linhit,Y
				bne	dldone
			endif
			dex
		miend
dldone	ldx	temp5
		rts	
		
;*************************************
		
dospinner	
		jsr	mzdisp
		lda	webscur
		clc	
		adc	webmult
		tax	
		lda	t_color,X
		sta	color
		ldx	webscur
		lda	webssta,X
		and	#$F0
		lsr	A
		lsr	A
		lsr	A
		tay	
		lda	spintable+1,Y
		pha	
		lda	spintable,Y
		pha	
		rts

;************************************************
;* Lookup Tables for Spinner Statuses           
;************************************************	
;* Spinner Status - High Nibble							
;************************************************		
spintable	
		.dw spinr0-1    ;0	Spinner not started yet	
        .dw spinr1-1    ;1	Growing at start spot
        .dw spinr2-1    ;2	First Half of motion Sequence
        .dw spinr3-1    ;3 	Center of Sequence	
        .dw spinr4-1    ;4	Center turn Horizontal to Vertical
		.dw spinr5-1    ;5	Center turn Vertical to Horizontal	
        .dw spinr6-1    ;6	Second Half of motion Sequence	
        .dw spinr7-1    ;7	Falling at player, spinning	
        .dw spinr8-1    ;8	Dead or Gone	
        .dw spinr9-1    ;9  Dead Spinner, Disentigrate
		.dw spinra-1    ;A  Sentry Spinner	

;************************************************
;* Spinner Status - 0 Spinner Not Started     	*
;************************************************
spinr0	lda	#$80
		sta	webssta,X
		inc	websded		;Spinner was Killed
		rts	

;************************************************
;* Spinner Status - 1	Growing at start spot	*
;************************************************		
spinr1	ldx	webscur
		ldy	linstr,X
		lda	linx,Y
		sta	temp2
		lda	liny,Y
		sta	temp1
		and	#$01
		ifeq
			ldy	websper,X
			lda	t_sscale,Y
		else 
			lda	websper,X
			asl	A
			asl	A
			asl	A
			asl	A
			eor	#$F0
		endif
		pha	
		lsr	temp1
		lda	#$75        ;SCAL+/YWINDOW+DSCALE
		sec	
		sbc	temp1
		tax	
		pla	
		jsr	vgadd2
		lda	color
		ldy	temp2
		ifeq
			ldx	#spstat_vpg
		else
			ldx	#spstat_vpg+v_xflip
		endif
		jsr	vgadd2
		lda	hfold				;First Frame of Horizontal Fold
		ldx	hfold+1
		jsr	vgadd2
		lda	smframe
		and	#$03
		ifeq
			ldx	webscur
			inc	websper,X
			lda	websper,X
			cmp	#$10
			ifeq
				lda	#$60				;Change Status
				sta	webssta,X
				cpx	#$02
				ifne
					ldy	linstr,X
					lda	linlst,Y
					dex	
					jsr	spinrlp
				endif
			endif
		endif
		rts     	
	
;Growing Spinner Scale Data Bytes	
t_sscale	.byte $F4,$E8,$DD,$D2,$C7,$BB,$B0,$A5,$9A,$8E,$83,$78,$6D,$61,$56,$4B

;******************************************************
;* Spinner Status - 2 First Half of motion Sequence	*
;******************************************************
spinr2	jsr	m3mys1
		lda	webssta,X
		and	#03
		asl	A
		sta	temp1
		ldy	linend,X
		lda	linx-1,Y
		sta	temp1+1
		dey	
		ldx	#00
		jsr	drspinl
		lda	smframe
		and	#01
		ifeq
			ldx	webscur
			lda	webssta,X
			cmp	#$23
			ifne
				clc	
				adc	#01
			else
				lda	#$30			;Status - Center of Sequence
			endif
			sta	webssta,X
		endif
		rts

;************************************************
;* Spinner Status - 3 	Center of Sequence	*
;************************************************	
spinr3	jsr	m3mys1
		lda	#$08
		sta	temp1
		ldy	linend,X
		lda	linx-1,Y
		sta	temp1+1
		dey	
		ldx	#$00
		jsr	drspinl
		lda	smframe
		and	#$01
		ifeq
			ldx	webscur
			lda	linend,X
			cmp	linstr+1,X
			ifeq
				lda	#$70
				sta	webssta,X
				ldy	linend,X
				lda	linx-1,Y
				sta	websxh,X
				lda	liny-1,Y
				sta	websyh,X
				lda	#$00
				sta	websxl,X
				sta	websyl,X
				sta	websper,X
			else
				tay	
				lda	linx-2,Y
				cmp	linx-1,Y
				ifeq
					cmp	linx,Y
					ifeq
?spr30					lda	#$60				;Change Status
						sta	webssta,X
						rts     
					endif
				endif	
				lda	liny-2,Y
				cmp	liny-1,Y
				ifeq
					cmp	liny,Y
					beq	?spr30
				endif
				lda	linx-1,Y
				cmp	linx-2,Y
				ifeq
					lda	#$50
				else
					tay	
					lda	correg3,Y
				endif
				sta	webssta,X				;Change Status
			endif
		endif
		rts  
		
correg3	.byte $40,$40,$41,$41,$41,$41,$41,$41,$41,$40,$40

;**********************************************************
;* Spinner Status - 4	Center turn Horizontal to Vertical*
;**********************************************************
spinr4	jsr	m3mys1
		jsr	enem_x9
		ldx	webscur
		lda	webssta,X
		and	#$0F
		ifeq
			ldx	temp1
			ldy	t_sturn2,X
		else
			cmp	#06
			ifne
				asl	A
				adc	hvframenum
				tay	
			else
				ldy	#$62
			endif
		endif
		lda	t_slines,Y				;Get Folding Spinner Frame 
		ldx	t_slines+1,Y
		jsr	vgadd2
		lda	smframe
		and	#01
		ifeq
			ldx	webscur
			lda	webssta,X
			clc	
			adc	#01
			cmp	#$47
			ifeq
				lda	#$60			;Goto Next Status
			endif
			sta	webssta,X
		endif
		rts

;***********************************************************
;* Spinner Status - 5	Center turn Vertical to Horizontal *
;***********************************************************	
spinr5	jsr	m3mys1
		jsr	enem_x9
		ldx	webscur
		lda	webssta,X
		and	#$0F
		cmp	#$05
		ifcc
			asl	A
			sta	temp2
			lda	vhframenum
			sec	
			sbc	temp2
			tay	
		else
			ifeq
				ldx	temp1
				ldy	t_sturn2,X
			else
				ldx	temp1
				ldy	t_sturn1,X
			endif
		endif
		lda	t_slines,Y				;Get Folding Spinner Frame 
		ldx	t_slines+1,Y
		jsr	vgadd2
		lda	smframe
		and	#$01
		ifeq
			ldx   webscur
			lda   webssta,X
			clc     	
			adc   #$01
			cmp	#$57
			ifeq
				lda	#$60			;Goto Next - Second Half of turn Motion
			endif
			cmp	#$55
			ifeq
				ldy	temp1
				lda	correg5,Y
			endif
			sta	webssta,X
		endif
		rts 
		
correg5	.byte $55,$55,$56,$56,$56,$56,$56,$56,$56,$55,$55

;*******************************************************
;* Spinner Status - 6	Second Half of motion Sequence *
;*******************************************************	
spinr6	jsr	m3mys1
		lda	webssta,X
		and	#$03
		clc	
		adc	#$05
		asl	A
		sta	temp1
		ldy	linend,X
		lda	linx-1,Y
		sta	temp1+1
		ldx	#$04
		jsr	drspinl
		lda	smframe
		and	#$01
		ifeq
			ldx	webscur
			lda	webssta,X
			cmp	#$63
			ifne
				clc	
				adc	#$01				;Increment Status
			else
				ldy	linend,X
				dey	
				tya	
				ldx	ilinesz
				begin
					cmp	ilines,X
					ifeq
						lda	#$0F
						sta	linhit,Y			;Set line hits
						bne	?spr60			;Always
					endif
					dex	
				miend
?spr60			ldx	webscur
				inc	linend,X
				jsr	nextvert
				lda	#$20				;Back to first half of motion
			endif
			sta	webssta,X
		endif
		rts     	

;**************************************************
;* Spinner Status - 7 Falling at player, spinning *
;**************************************************		
spinr7	ldx	webscur
		jsr	webcomp
		cpx	#$02
		ifcc
			lda	#$80
		else
			lda	#$00
			sta	scalef
			lda	#$FF
		endif
		sta	scalef+1
		jsr	posvc2
		lda	color
		ldx	#spstat_vpg
		jsr	vgadd2
		jsr	spinrot			;Rotate Spinner
		cpx	#$02
		ifcc
			lda	websyl,X
			cmp	#$20
			ifcc
				lda	websyh,X
				asl	A
				asl	A
				asl	A
				asl	A
				cpx	#$01
				ifeq
					ora	#$0A
				endif
				ldx	#nweb-1
				jsr	spinrlp
			endif
			jsr	spintrk			;Adjust spinner location
		else
			jsr	spintrk			;Adjust spinner location
			ldx	webscur
spinr7a		lda	#00
			sta	temp5
			lda	#$40
			sta	temp4
			lda	#$0C
			sta	temp4+1
			lda	#05				;Give some points
			sta	hitpts
			jsr	hitcheck			;Check spinner/shot collision
			ldx	webscur
			lda	webssta,X
			cmp	#05
			ifeq
				lda	websxh,X
				cmp	#05
				ifcc
					lda	#00
				else
					lda	#01
				endif
				sta	websper,X
				lda	#$0F
				sta	websseg,X
				lda	#$90				;Change Status
				sta	webssta,X
			endif
		endif
		rts	

spinrlp	begin
			ldy	linstr,X
			cmp	linlst,Y
			ifeq
				lda	#$10
				sta	webssta,X
				jsr	nextvert
				lda	#$00
				sta	websper,X
				bne	?sprl1
			endif
			dex	
			cpx	#$01
		eqend
?sprl1	rts     
		
	
;************************************************
;* Spinner Status - 8	Spinner Gone		*
;************************************************
spinr8	inc	websded		;Spinner Dead
		rts


;************************************************
;* Collision check for Spinners and Ship Shots  *	
;************************************************
hitcheck	
        lda	websyl,X			;First check the Y position
		sbc	#$D0
		lda	websyh,X
		sbc	#09
		ifcc
			jsr	hitwebs		;Goto X collision check in main
		endif
		rts
	
;************************************************
;* Spinner Status - 9 Dead Spinner, Disentigrate*
;************************************************		
spinr9	lda	#$FF
		jsr	posspin
		ldx	webscur
		lda	#$F0
		ldy	websper,X
		ifeq
			ldx	#spstat_vpg+v_sparkle+v_xflip
		else
			ldx	#spstat_vpg+v_sparkle
		endif
		jsr	vgadd2
		jsr	spinrot			;Rotate Spinner
		ldx	webscur
		lda	websxl,X
		ldy	websper,X
		ifeq
			clc	
			adc	#$1C
			sta	websxl,X
			ifcs
				inc	websxh,X
			endif
		else
			sec	
			sbc	#$1C
			sta	websxl,X
			ifcc
				dec	websxh,X
			endif
		endif
		lda	smframe
		and	#01
		ifeq
			dec	websseg,X
			lda	websseg,X
			ifeq
				lda	extrasp			;Spinners dropping still?
				ifne					;yes
					lda	#$70
					sta	webssta,X
					lda	#00
					sta	websyl,X
					sta	websyh,X
					sta	websper,X
					sta	websxl,X
					lda	deadx-2,X
					sta	websxh,X
				else					;Spinners not dropping
					lda	#$80
					sta	webssta,X
				endif
			endif
		endif
		rts	

deadx		.byte $01,$02,$03,$04,$05,$06,$07,$08,$09,$05
	
;************************************************
;* Spinner Status - A	Sentry Spinner		
;************************************************
spinra	ldx	webscur
		lda	websper,X
		ifpl
			lda	#00
			sta	temp5
			lda	#$20
			sta	temp4
			lda	plfspd+1
			sta	temp4+1
			lda	webssta,X
			sta	perm1
			lda	#01
			sta	webssta,X
			jsr	webcomp
			lda	#05
			sta	hitpts
			jsr	hitcheck				;Check spinner/shot collision
			ldx	webscur
			lda	webssta,X
			cmp	#05
			ifeq
				lda	#$80
				ora	websper,X
				sta	websper,X
			endif
			lda	perm1
			sta	webssta,X
		endif
		lda	#$FD
		jsr	posspin
		ldx	webscur
		lda	websper,X
		ifpl
			and	#01
			ifeq
				ldx	#spstat_vpg+v_xflip
			else_ne
				ldx	#spstat_vpg
			endif
			lda	#$F3
		else_ne  
			and	#01
			ifeq
				ldx	#spstat_vpg+v_sparkle+v_xflip
			else_ne
				ldx	#spstat_vpg+v_sparkle
			endif
			lda	#$F0
		endif
		jsr	vgadd2
		jsr	spinrot			;Rotate Spinner
		lda	websper,X
		and	#01
		ifeq
			lda	websxl,X
			cmp	#$B1
			ifcs
				lda	websseg,X
				lsr	A
				lsr	A
				lsr	A
				lsr	A
				cmp	websxh,X
				ifcc
?spra1				inc	websper,X
					bne	?spra3
				endif
				beq	?spra1
			endif
?spra2		lda	websxl,X
			clc	
			adc	#$10
			ifcs
				inc	websxh,X
			endif
		else
			lda	websxl,X
			cmp	#$4F
			ifcc
				lda	websseg,X
				and	#$0F
				cmp	websxh,X
				ifcs
					dec	websper,X
					jmp	?spra2
				endif
			endif
?spra3		lda	websxl,X
			sec	
			sbc	#$10
			ifcc
				dec	websxh,X
			endif
		endif
		sta	websxl,X
		lda	shipst			;Ship Exploding?
		ifpl					;Nope
			ifne
				lda	prepare			;Have we started scrolling?
				ifeq					;yes
					lda	plfspd+1
					bit	plfspd
					clc	
					ifmi
						adc	#01
					endif
					adc	websyl,X
					sta	websyl,X
					ifcs
						inc	websyh,X
					endif
				endif
			endif
		endif
		lda	websyh,X
		cmp	#$0A
		ifcs
			lda	websyl,X
			cmp	#$40
		else_cs
			lda	smframe
			and	#03
			bne	?spra4
			lda	websper,X
			bpl	?spra4
			clc	
			adc	#$10
			sta	websper,X
			bmi	?spra4
		endif
		lda	#$80
		sta	webssta,X
?spra4	rts	

;***********************************************************
;* Sentry - Places the Random Sentry Spinners in the maze  *
;***********************************************************
sentry	ldx	smzlvl
		lda	maplen			;Maze Length
		sec	
		sbc	dist+1
		sbc	t_sentrydl,X
		ifmi
			rts
		endif	
		txa	
		asl	A
		asl	A
		adc	#03
		tax	
		ldy	#03
		lda	dist+1
		begin
			cmp	t_sentrydh,X
			ifeq
				rts
			endif
			dex	
			dey	
		miend
		jsr	getrand
		and	#01
		ifeq
			rts	
		endif
		sta	perm1
		lda	#00
		ldx	#09
		begin
			sta	sencom+1,X
			dex	
		miend
		ldx	#nweb-1
		begin
			ldy	linstr,X
			iny	
			tya	
			cmp	linend,X
			ifne
				begin
					lda	linhit-1,Y
					cmp	#$0F
					bcs	?sen7
					lda	liny-1,Y
					cmp	#02
					bne	?sen6
					begin
						lda	liny,Y
						cmp	#03
						bne	?sen7
?sen5				    sty	temp1
						lda	linx,Y
						tay	
						lda	sencom+1,Y
						ora	#02
						sta	sencom+1,Y
						ldy	temp1
						bne	?sen7
?sen6					cmp	#04
					neend
					cmp	#03
					bne	?sen7
					lda	liny,Y
					cmp	#02
					beq	?sen5
					cmp	#04
					beq	?sen5
					cmp	#03
					ifeq
						lda	linx,Y
						cmp	linx-1,Y
						ifcs
							lda	linx-1,Y
						endif
						sty	temp1
						tay	
						lda	sencom+1,Y
						ora	#01
						sta	sencom+1,Y
						ldy	temp1
					endif
?sen7				iny	
					tya	
					cmp	linend,X
				csend
			endif
			dex	
			cmp	#01
		eqend
		begin
			ldx	#$0F
			begin
				jsr	getrand
				and	#$0F
				cmp	#$0A
				bcc	?sen8
				dex
			miend
			lda	#05
?sen8		sta	perm2
			tax	
?sen9		lda	sencom+1,X
			and	#01
			ifne
				inx	
				cpx	#$0A
				ifeq
					ldx	#00
				endif
				cpx	perm2
				bne	?sen9
				rts	
			endif
			stx	perm2
			cpx	#09
			ifne
?sen10		    lda	sencom+2,X
				ifeq
					lda	sencom+1,X
					ora	#01
					sta	sencom+1,X
					inx	
					cpx	#09
					bne	?sen10
				endif
			endif
			lda	sencom+1,X
			ora	#01
			sta	sencom+1,X
			txa	
			asl	A
			asl	A
			asl	A
			asl	A
			sta	perm3
			ldx	perm2
?sen11		cpx	#00
			ifne
				lda	sencom+1,X
				and	#02
				ifeq
					lda	sencom,X
					and	#01
					ifeq
						lda	sencom,X
						ora	#01
						sta	sencom,X
						dex	
						bpl	?sen11
					endif
				endif
			endif
			txa	
			ora	perm3
			sta	perm3
			ldx	#nweb-1
?sen12		lda	webssta,X
			cmp	#$80
			ifne
				dex	
				bpl	?sen12
				rts
			endif
			lda	#$A0
			sta	webssta,X
			lda	perm3
			sta	websseg,X
			and	#$0F
			sta	websxh,X
			lda	#00
			sta	websper,X
			lda	#$80
			sta	websxl,X
			lda	#02
			sta	websyh,X
			lda	dist
			sta	websyl,X
			dec	perm1
		eqend
		rts	
	
;**********************************

nextvert	
        ldy linend,X
		dey     	
		lda #$00
		sta websxl,X
		sta websyl,X
		lda linx,Y
		sta websxh,X
		lda liny,Y
		sta websyh,X
		rts    
	
;**********************************
:* Draw Something
;**********************************
m3mys1	ldx	webscur
		jsr	webcomp
		jsr	spinr7a
		lda	webssta,X
		cmp	#$90
		ifeq
			pla	
			pla	
			jmp	spinr9
		endif
		ldy	linend,X
		lda	liny-1,Y
		tay	
		lsr	A
		sta	temp1
		lda	#$75
		sec	
		sbc	temp1
		tax	
		tya	
		and	#01
		ifeq
			lda	#$4B    ;Linear Scale value
		else
			lda	#00
		endif
		jsr	vgadd2
		ldx	webscur
		rts	
		
;***********************************
spinrot	ldx	webscur			;Which Spinner
		lda	webssta,X
		and	#03
		asl	A
		tay	
		lda	spinners,Y
		ldx	spinners+1,Y
		jsr	vgadd2
		ldx	webscur
		ldy	webssta,X
		cpy	#$A0
		ifcs
?spra13		ldx	webscur
			lda	webssta,X
			clc	
			adc	#01
			and	#$FB
			sta	webssta,X
		else_ne
			lda	smframe
			and	#01
			beq	?spra13
		endif
		rts
	
;**********************************	

spintrk	ldx	webscur
		lda	websyl,X
		cpx	#$02
		ifcc
			adc	#$20
		else
			adc	#$17
		endif
		sta	websyl,X
		ifcs
			inc	websyh,X
		endif
		cmp	#$80
		ifcs
			lda	websyh,X
			cmp	#$0A
			ifeq
				cpx	#$02
				ifcc
					
?spt10				lda	#$80
					sta	webssta,X
				else
					lda	extrasp			;Spinners dropping?
					beq	?spt10			;No
					lda	#$00				;Here if yes
					sta	websper,X
					sec	
					sbc	websxl,X
					sta	websxl,X
					lda	#$0A
					sbc	websxh,X
					sta	websxh,X
					lda	#$00
					sta	websyl,X
					sta	websyh,X
					rts
				endif
			endif	
		endif
		cpx	#$02
		ifcc
			jmp	?spt30			;Get outta here!
		endif
		jsr	webcomp
        ;Load our spinner tracking here, gamedif (0-2) sets table row, smzlvl (0-3) sets data 
		lda	gamedif              ;gamedif ranges from 0-2
		asl	A
		asl	A
		adc	smzlvl
		tay	
		lda	t_trkoff,Y
		clc	
		adc	websyh,X
		tay	
		lda	vdata
		cmp	shipxl
		lda	vdata+1
		sbc	shipxh
		ifcs
			lda	websper,X
			ifpl
				sbc	t_trkdat,Y
				sec	
				sbc	t_trkdat,Y
			else
				sbc	t_trkdat,Y
				ifpl
					lda	#$80
				endif
			endif
		else
			lda	websper,X
			ifmi
				adc	t_trkdat,Y
				clc	
				adc	t_trkdat,Y
			else
				adc	t_trkdat,Y
				ifmi
					lda	#$7F
				endif
			endif
		endif
		sta	websper,X
		clc	
		ifpl
			adc	websxl,X
			sta	websxl,X
			ifcs
				lda	websxh,X
				cmp	#$09
				ifeq
					lda	#$0A
?spt20				sta	websxh,X
					lda	#$00
					sta	websxl,X
					lda	#$00
					sec	
					sbc	websper,X
					sta	websper,X
					jmp	?spt30
				endif
				adc	#$01
				sta	websxh,X
			endif
		else
			lda	websxl,X
			adc	websper,X
			sta	websxl,X
			ifcc
				lda	websxh,X
				ifeq
					lda	#$00
					beq	?spt20
				endif
				sbc	#$00
				sta	websxh,X
			endif
		endif
?spt30	rts	

		
;**************************************************************
;* Tracking Data - 1 for each spinner higher numbers allow 
;* better tracking of player ship.                      
;**************************************************************
t_trkdat	
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01
		.byte $03,$00,$00,$01,$01,$01,$00,$00,$00,$00,$01,$01,$04,$01,$01,$01
		.byte $01,$03,$00,$00,$00,$00,$01,$01,$06,$01,$01,$02,$02,$06,$00,$00
		.byte $00,$00,$01,$01,$08,$00,$01,$01,$02,$08,$00,$00,$00,$00,$00,$00
		.byte $0A,$00,$00,$01,$03,$0A,$00,$00,$00,$00,$00,$00,$0C,$00,$00,$01
		.byte $04,$0C,$00,$00

;****************************************
;* Offsets into above table             *
;****************************************		
t_trkoff	
        .byte $00,$0C,$18,$30       ;gamedif = 00
		.byte $00,$0C,$24,$3C       ;gamedif = 01
		.byte $00,$18,$30,$40       ;gamedif = 02
		;.byte $00,$24,$30,$48       ;incdif = 03
		;.byte $00,$24,$3C,$48       ;incdif = 04

;****************************************
	
drspinl	lda	linx,Y
		cmp	linx-1,Y
		ifne
			bpl	?mir20
			bmi	?mir10
		endif
		lda	liny,Y
		cmp	liny-1,Y
		ifmi
			inx
		endif
		inx	
?mir10	inx	
?mir20	lda	t_spinoff,X
		clc	
		adc	temp1+1
		tay	
		lda	t_hspini,Y
		sta	temp2
		lda	t_svdat2,Y
		ifpl
			ldx	#spstat_vpg
		else
			ldx	#spstat_vpg+v_xflip
		endif
		and	#$40
		sta	temp2+1
		lda	color
		jsr	vgadd2          ;Set STAT and color
		lda	temp2
		ldy	temp2+1
		ifeq
			clc	
			adc	temp1
		else
			sec	
			sbc	temp1
		endif
		tay	
		lda	t_slines,Y					;Get Folding Spinner Frame 
		ldx	t_slines+1,Y
		jmp	vgadd2
		
t_spinoff	        
        .db hspini0-t_hspini    ;$00
        .db hspini2-t_hspini    ;$16
        .db hspini4-t_hspini    ;$2C
        .db hspini5-t_hspini    ;$37
        .db hspini1-t_hspini    ;$0B
        .db hspini3-t_hspini    ;$21
        .db hspini4-t_hspini    ;$2C
        .db hspini5-t_hspini    ;$37
        
;**************************************
enem_x9	ldx	webscur
		ldy	linend,X
		lda	linx-1,Y
		sta	temp1
		cmp	#06
		ifcc
			ldx	#spstat_vpg
		else
			ldx	#spstat_vpg+v_xflip
		endif
		lda	color
		jmp	vgadd2
		
;*****************************
;* Position a Spinner        *
;*****************************
posspin	sta	scalef+1
		lda	#00
		sta	scalef
		ldx	webscur
		jsr	webcomp
		jmp	posvc2	
		
;*****************************		
;Spinner Colors 12 per level	
;*****************************	

t_color .db ($F0+colyellow),($A0+colred2),  ($F0+colyellow),($F0+colyellow)
        .db ($F0+colyellow),($F0+colyellow),($F0+colyellow),($A0+colred2)
        .db ($A0+colred2),  ($A0+colred2),  ($A0+colred2),  ($A0+colred2)
        
        .db ($F0+colgreen),($E0+colcyan), ($F0+colgreen),($F0+colgreen)
        .db ($F0+colgreen),($F0+colgreen),($E0+colcyan), ($E0+colcyan)
        .db ($E0+colcyan), ($E0+colcyan), ($E0+colcyan), ($E0+colcyan)
        
        .db ($F0+colyellow),($E0+colcyan),($F0+colgreen),($A0+colred2)
        .db ($F0+colyellow),($E0+colcyan),($F0+colgreen),($A0+colred2)
        .db ($F0+colyellow),($E0+colcyan),($F0+colgreen),($C0+colwhite)
        
        .db ($F0+colflash),($F0+colorange),($C0+colflash), ($C0+colflash)
        .db ($C0+colflash),($C0+colflash), ($C0+colflash), ($C0+colwhite)
        .db ($A0+colred2), ($E0+colcyan),  ($F0+colyellow),($F0+colgreen)
	
; Length of Maze		
t_maplen	.byte $15,$19,$1C,$1C
	
; Wave Bonus to Start At		
t_smbonus	.byte $03,$04,$04,$04
	
; Maze Acceleration		
t_plfadd	.byte $58,$50,$2A,$30
		
; Spinner Drop Time		
t_extrasp	.byte $28,$28,$38,$38		
t_sentrydl	.byte $0A,$0C,$0F,$0F
t_sentrydh	.byte $01,$03,$05,$09
t_notused1	.byte $01,$06,$0A,$0B
t_notused2	.byte $02,$05,$0D,$80
t_notused3	.byte $02,$05,$0D,$80

;Invisible Line table start pointers		
t_invptr	.word linei0,linei1,linei2,linei3

;Length of each invisible line table		
t_invlen	.byte (linei0e-linei0-1),(linei1e-linei1-1),(linei2e-linei2-1),(linei3e-linei3-1)

;Invisible Line Table		
linei0	    .byte $0D,$0E,$26,$2E,$2F,$39,$3A
linei0e
		
linei1	    .byte $06,$07,$28,$29,$2D,$2E,$2F,$3E,$3F,$46,$47
linei1e
		
linei2	    .byte $02,$03,$0E,$17,$1F,$20,$32,$33,$45,$4B,$4D,$52,$57
linei2e
		
linei3	    .byte $02,$03,$0E,$17,$1F,$20,$32,$33,$45,$4B,$4D,$52,$57
linei3e

;Pointers to start of each maze line table		
t_slevel	.word slevel0,slevel1,slevel2,slevel3

;Length of the Maze Line table		
t_slevlen	.byte (slevel0e-slevel0-1),(slevel1e-slevel1-1),(slevel2e-slevel2-1),(slevel3e-slevel3-1)

;Table to define the spinners for each difficulty level, 12 max per level		
t_lineoffs	.byte $01,$02,$09,$0D,$18,$1D,$24,$2B,$34,$3E,$45,$4F
			.byte $01,$02,$06,$0D,$15,$1F,$28,$2D,$34,$3E,$46,$50
			.byte $01,$02,$0A,$12,$1F,$2A,$37,$3F,$4B,$57,$61,$68
			.byte $01,$02,$0A,$12,$1F,$2A,$37,$3F,$4B,$57,$61,$68

;********************************************
; Line Data in YX pairs (nybbles), data is
; top down from maxlin
;********************************************            
;Level 3 Space Maze 
slevel0		.byte $00,$0A,$90,$91,$92,$93,$94,$95,$96,$80,$81,$82,$83,$70,$71,$72
			.byte $73,$74,$75,$65,$64,$63,$62,$72,$50,$51,$52,$53,$43,$10,$11,$12
			.byte $22,$23,$33,$34,$9A,$99,$98,$88,$87,$86,$85,$5A,$59,$58,$68,$67
			.byte $66,$56,$55,$65,$4A,$49,$48,$47,$46,$36,$35,$34,$44,$43,$3A,$39
			.byte $38,$37,$36,$26,$25,$0A,$09,$08,$07,$06,$05,$04,$14,$15,$25
slevel0e

;Level 7 Space Maze		
slevel1		.byte $00,$0A,$90,$91,$81,$82,$80,$81,$82,$72,$73,$74,$75,$50,$51,$52
			.byte $53,$54,$55,$65,$75,$20,$21,$22,$23,$24,$34,$33,$32,$31,$30,$9A
			.byte $99,$98,$97,$96,$95,$85,$86,$87,$7A,$79,$78,$77,$87,$5A,$59,$58
			.byte $57,$67,$68,$78,$3A,$39,$38,$37,$36,$35,$45,$46,$47,$57,$1A,$19
			.byte $18,$28,$27,$26,$25,$15,$1A,$19,$18,$17,$07,$06,$05,$04,$14,$15
slevel1e

;Level 11 Space Maze		
slevel2		.byte $00,$0A,$90,$91,$92,$93,$94,$95,$96,$97,$60,$61,$62,$63,$73,$74
			.byte $64,$65,$50,$51,$52,$53,$54,$44,$45,$55,$56,$46,$47,$48,$49,$30
			.byte $31,$32,$33,$34,$35,$25,$24,$23,$22,$32,$00,$01,$11,$12,$02,$03
			.byte $04,$05,$06,$07,$08,$09,$0A,$8A,$89,$88,$87,$86,$85,$84,$94,$7A
			.byte $79,$78,$77,$76,$75,$74,$73,$72,$71,$81,$80,$6A,$69,$68,$67,$66
			.byte $56,$57,$67,$68,$58,$59,$69,$3A,$39,$38,$37,$36,$26,$27,$28,$29
			.byte $39,$1A,$19,$18,$17,$16,$15,$14
slevel2e

;Level 15,19,23 Space Maze		
slevel3		.byte $00,$0A,$9A,$99,$98,$97,$96,$95,$94,$93,$6A,$69,$68,$67,$77,$76
			.byte $66,$65,$5A,$59,$58,$57,$56,$46,$45,$55,$54,$44,$43,$42,$41,$3A
			.byte $39,$38,$37,$36,$35,$25,$26,$27,$28,$38,$0A,$09,$19,$18,$08,$07
			.byte $06,$05,$04,$03,$02,$01,$00,$80,$81,$82,$83,$84,$85,$86,$96,$70
			.byte $71,$72,$73,$74,$75,$76,$77,$78,$79,$89,$8A,$60,$61,$62,$63,$64
			.byte $54,$53,$63,$62,$52,$51,$61,$30,$31,$32,$33,$34,$24,$23,$22,$21
			.byte $31,$10,$11,$12,$13,$14,$15,$16
slevel3e


;************************************************
	.sbttl "Space Maze, Collision Routines"
;************************************************

;*********************************************************
	.sbttl "Convert from Maze to Absolute Coordinates"	
;*********************************************************	
webcomp	lda	websxl,X
		sta	vdata
		lda	websxh,X
		lsr	A
		ror	vdata
		clc	
		adc	#02
		sta	vdata+1			;0 to A becomes 2 to 7
		lda	websyl,X
		sta	vdata+2
		lda	websyh,X
		sta	vdata+3
		rts	

; Table used for Maze to absolute coordinates		
convl		.byte $00,$80,$00,$80,$00,$80,$00,$80,$00,$80,$00
convh		.byte $02,$02,$03,$03,$04,$04,$05,$05,$06,$06,$07

; Table used for absolute to maze
conbck		.byte $80,$80,$00,$02,$04,$06,$08,$0A,$0C

; Table used to find maze horizontals
gridedg		vctrly(-10) ;$1FF6
            vctrly(-14) ;$1FF2
            vctrly(-20) ;$1FEC
            vctrly(-28) ;$1FE4
            vctrly(-40)  ;$1FD8
            vctrly(-57)  ;$1FC7
            vctrly(-80)  ;$1FB0
            vctrly(-113)  ;$1F8F
            vctrly(-160)  ;$1F60
            vctrly(-226)  ;$1F1E
            vctrly(-320)  ;$1EC0
            vctrly(-453)  ;$1E3B

; Tables used for maze edge scroll
gridfx		.byte $FC,$FA,$F8,$F4,$EF,$E9,$DF,$D1,$BE,$A2,$7B
gridfy		.byte $FA,$F7,$F4,$EE,$E7,$DD,$CE,$BA,$9D,$73,$39

; Table for grid X perspective mapping vector (from left)
gridx01		vctrlx(-4,visible)  ;$3FFC
            vctrlx(-10,visible) ;$3FF6
            vctrlx(-18,visible) ;$3FEE
            vctrlx(-30,visible) ;$3FE2
            vctrlx(-47,visible) ;$3FD1
            vctrlx(-70,visible) ;$3FBA
            vctrlx(-103,visible) ;$3F99
            vctrlx(-150,visible) ;$3F6A
            vctrlx(-216,visible) ;$3F28
            vctrlx(-310,visible) ;$3ECA
            
; Table for grid Y perspective mapping vector (from top)            
gridy01		vctrly(-6)  ;$1FFA
            vctrly(-15)  ;$1FF1
            vctrly(-27)  ;$1FE5
            vctrly(-45)  ;$1FD3
            vctrly(-70)  ;$1FBA
            vctrly(-105)  ;$1F97
            vctrly(-155)  ;$1F65
            vctrly(-225)  ;$1F1F
            vctrly(-324)  ;$1EBC
            vctrly(-465)  ;$1E2F

;****************************************************	
	.sbttl "Display the maze"
;****************************************************
mzdisp	ldy	webscur
		ldx	linstr,Y			;Start X with the start vertex
		ldy	#00					;Start Y with Zero
		lda	#$40                ;Doesn't matter, LSB of VCTR command
		sta	(vglist,Y)
		iny	
		lda	#$80                ;VCTR
		sta	(vglist,Y)
		iny	
		lda	#00
		sta	(vglist,Y)
		iny	
		lda	#$71                ;SCAL 0,0
		sta	(vglist,Y)
		iny						
		lda	#($F0+colblack)     ;Intensity 16, Color 0 - Black 
		sta	(vglist,Y)
		iny	
		lda	linx,X
		ifeq
			lda	#spstat_vpg+v_sparkle
		else
			lda	#spstat_vpg+v_sparkle+v_xflip       ;this effects edge lines only, set below again
		endif
		sta	(vglist,Y)
		iny						;Stat no color, xflip if X=0A
		lda	webscur
		cmp	#02
		ifcc						;Edge Spinner
			lda	state
			ifeq						;Not maze traversal, drawing mode
				lda	#$E1
				sta	(vglist,Y)
				iny	
				lda	#00
				sta	(vglist,Y)
				iny	
				lda	#$F6
				sta	(vglist,Y)
				iny	
				lda	#$1F
				sta	(vglist,Y)
				iny					    ;VCTR to start location
				lda	websyh,X
				sta	temp1
				ifne					;A vector needed before the scaled version
					asl	A                   ;words
					tax	
?wsp10				lda	gridy01-2,X
					sta	(vglist,Y)
					iny	
					lda	gridy01-1,X
					sta	(vglist,Y)
					iny	
					lda	gridx01-2,X
					sta	(vglist,Y)
					iny	
					lda	gridx01-1,X
					sta	(vglist,Y)
					iny					;VCTR to closest edge dot
					lda	temp1
					cmp	#$0A
					beq	?wsp15			;If no scaled vector needed
					ldx	webscur
				endif
				lda	websyl,X
				lsr	A
				lsr	A
				tax	
				lda	qrtlog,X
				sta	(vglist,Y)	    ;Linear scale for edge vector
				iny	
				lda	#$71
				sta	(vglist,Y)
				iny					;Binary scale zero
				ldx	temp1
				lda	gridfy,X
				sta	(vglist,Y)
				iny	
				lda	#$1F
				sta	(vglist,Y)
				iny	
				lda	gridfx,X
				sta	(vglist,Y)
				iny	
				lda	#$3F
				sta	(vglist,Y)
				iny					;VCTR to next grid point but don't make it there
?wsp15			jmp	?mdend
			endif
		endif
		lda	liny,X
		sec	
		sbc	state
		asl	A
		tax	
		lda	gridy,X
		sta	(vglist,Y)
		iny	
		lda	gridy+1,X
		sta	(vglist,Y)
		iny						    ;VCTR to edge of grid Y
		lda	gridedg,X
		sta	(vglist,Y)
		iny	
		lda	gridedg+1,X
		sta	(vglist,Y)				;VCTR to edge of grid X
		iny	
		lda	state
		ifne						;Add scrolling vector, maze is being traversed
			lda	linmult
			sta	(vglist,Y)
			iny	
			lda	#$71
			sta	(vglist,Y)
			iny						;Scale 0 to FD linear as dist goes from 0 to ff
			txa	
			lsr	A
			tax	
			lda	gridfy,X
			sta	(vglist,Y)
			iny	
			lda	#$1F
			sta	(vglist,Y)
			iny	
			lda	gridfx,X
			sta	(vglist,Y)
			iny	
			lda	#$1F                ;Was $1F
			sta	(vglist,Y)
			iny						;VCTR down towards screen lower point
			ldx	webscur
			cpx	#02
			ifcc
				lda	#00
			else
				lda	linscal
			endif
			sta	(vglist,Y)
			iny	
			lda	#$71
			sta	(vglist,Y)				;Scale whole drawing down in size
			iny	
			cpx	#02
			ifcc
				ldx	#$14
				stx	temp1
				jmp	?wsp10
			endif
		endif
		ldx	webscur
		lda	linstr,X
		sta	lincur
		lda	#00					;Prepare to write directly to vglist
		sta	cxflip				;Always 0 or 1
		lda	linend,X
		sec	
		sbc	#02
		sta	finish
		ifpl				;To avoid blowup if finish = ff
            cmp	lincur
            ifcs
                begin
                    jsr	lindraw				;Stat and JSRL
                    inc	lincur
                    lda	finish
                    cmp	lincur
                ccend
            endif
        endif
?mdend	tya                 ;Y contains number of bytes added	
		clc	
		adc	vglist
		sta	vglist          ;adjust vglist
		ifcs
			inc	vglist+1
		endif
		rts
		
;*************************************************
;* Draw one line from vertex lincur to lincur+1		
;*************************************************	
lindraw	ldx	lincur
		lda	linx+1,X
		cmp	linx,X              ;Look at previous line X value
		ifne                    ;Are they the same? If not, then horizontal line
            ifpl					;Considering a horizontal line
                inc	cxflip
            endif
            jsr	colrlin			;load the correct color for this line
            ldx	lincur
            lda	liny,X			;Get first YX
            tax	
            lda	weblnah         ;Horizontal Line two byte base
            clc					
            adc	weblnsz,X	    ;Add in line size
            sta	(vglist,Y)
            lda	weblnah+1
        else
            ;Vertical line required!
            cmp	#06				
            ifcs
                sta	temp1
                lda	#$0A
                sbc	temp1
                inc	cxflip
            endif
            sta	temp2
            jsr	colrlin         ;get the correct color
            ldx	lincur			;Now generate stamp JSRL. If positive, use first vertex Y as index
            lda	liny+1,X
            cmp	liny,X
            ifpl
                lda	#00
                clc
            else_eq
                lda	#06				;Line goes from larger to smaller Y
                inx					;Use second vertex Y
            endif
            adc	temp2				;Unflipped 0 to 5, flipped 6 to B
            asl	A					;CC
            sta	temp2				;Index into line type available
            lda	liny,X
            tax	
            lda	weblnsz,X
            ldx	temp2
            adc	weblnan,X
            sta	(vglist,Y)
            lda	weblnan+1,X
        endif
        adc	#00
        iny	
        sta	(vglist,Y)
        iny	
		rts	

		
weblnan	jsrl(webln00)
		jsrl(webln01)
		jsrl(webln02)
		jsrl(webln03)
		jsrl(webln04)
		jsrl(webln05)
		jsrl(webln06)
		jsrl(webln07)
		jsrl(webln08)
		jsrl(webln09)
		jsrl(webln0a)
		jsrl(webln0b)

;***************************************************************        
;* This is a jsrl to the base of an array of horizontal lines
;* The calling code will add the offsets from the next table
;* below, the values below are right shifted to go into the 
;* JSRL properly
;***************************************************************
weblnah jsrl(weblnh0)

;These are offsets into the above table for values 0-B        
weblnsz	.db (weblnh0-weblnh0)>>1 ;$00
        .db (weblnh1-weblnh0)>>1 ;$03
        .db (weblnh2-weblnh0)>>1 ;$06
        .db (weblnh3-weblnh0)>>1 ;$09
        .db (weblnh4-weblnh0)>>1 ;$0C
        .db (weblnh5-weblnh0)>>1 ;$0F
        .db (weblnh6-weblnh0)>>1 ;$12
        .db (weblnh7-weblnh0)>>1 ;$15
        .db (weblnh8-weblnh0)>>1 ;$18
        .db (weblnh9-weblnh0)>>1 ;$1B
        .db (weblnha-weblnh0)>>1 ;$1E
        .db (weblnhb-weblnh0)>>1 ;$21

;******************************************************
	.sbttl "Draw the maze line of the correct color"
;******************************************************
colrlin	lda	linhit,X
		cmp	#maxhitln
		ifeq
            ;Here if the line hit count is equal to maxhitln
            lda	#colblack            ;invisible line: color 0, intensity 0
colrl1	    sta	(vglist,Y)
            iny	
            lda	#spstat_vpg
            lsr	cxflip
            ifcs
                lda	#spstat_vpg+v_xflip
            endif
            sta	(vglist,Y)
            iny	
            rts             ;Return
        endif
        ifcc          ;above Maximum hits, explode it
            cmp	numhit
            ifcs         
                ;Dying
                lda	#($A0+colred2)  ;Bright Red before exploding
                sta	(vglist,Y)
                iny	
                lda	#spstat_vpg
                lsr	cxflip
                ifcs
                    lda	#spstat_vpg+v_xflip
                endif
                sta	(vglist,Y)
                iny	
                rts	        ;Return
            endif
            ;Normal Color
            lda	#($F0+colblack)
            sta	(vglist,Y)
            iny	
            lda	#spstat_vpg+v_sparkle
            lsr	cxflip			;Zeros xflip as well
            ifcs
                lda	#spstat_vpg+v_sparkle+v_xflip
            endif
            sta	(vglist,Y)
            iny	
            rts         ;Return
        endif   
        ;***************************************************
            .sbttl "Draw an exploding maze line"
        ;***************************************************
        cmp	#$20
		ifcs				;Player ship hits line
			lda	smframe
			and	#$1E
			asl	A
			asl	A
			asl	A
			ora	#02
			jmp	colrl1		;Display it
		endif
		cmp	#$18			;10 to 17 red explosion, 18 to 1f green explosion
		ifcs
			lda	#($F0+colgreen)
		else_ne
            lda	#($F0+colred2)
		endif
		sta	perm3
		jsr	statexp		    ;STAT to #F4
		sta	perm1			;save xflip
		lda	state
		ifeq				;Maze Drawing Mode
			lda	liny,X
			clc	
			adc	#01
			lsr	A
			sta	temp1
			ifcc
				lda	#$4B        ;Linear scale
			else
				lda	#$00        ;Linear scale
			endif
			sta	(vglist,Y)
			iny	
			lda	#$76
			sec	
			sbc	temp1
			sta	(vglist,Y)
			iny				;Scale linear 0 or 4B, binary 0 to 4
		else				;Maze traversal mode
			lda	liny,X
			cmp	#$0B
			ifeq
				lda	#00
				sta	(vglist,Y)
				iny
				lda	#$71
				sta	(vglist,Y)
				iny
			else
				sta	temp1
				dec	temp1
				lda	dist
				lsr	temp1
				ror	A
				lsr	A
				tax	
				lda	fullog,X
				sta	(vglist,Y)
				iny	
				lda	#$75
				sec	
				sbc	temp1
				sta	(vglist,Y)
				iny
			endif
		endif
		ldx	lincur
		lda	linhit,X
		and	#07
		asl	A				;20 to 2E
		adc	#$20
		tax	
		lda	cerexp-$20,X
		sta	perm2
		sta	(vglist,Y)
		iny	
		lda	cerexp-$20+1,X
		sta	perm2+1
		sta	(vglist,Y)
		iny					;JSRL to first half of explosion
		lda	#00
		sta	(vglist,Y)
		iny	
		lda	#00
		sta	(vglist,Y)
		iny	
		lda	#$40
		sta	(vglist,Y)
		iny	
		lda	#00
		sta	(vglist,Y)
		iny	
		lda	perm3
		sta	(vglist,Y)
		iny	
		lda	perm1
		eor	#v_xflip        	;Reverse the direction
		sta	(vglist,Y)
		iny					    ;Stat bright red
		lda	perm2
		sta	(vglist,Y)
		iny	
		lda	perm2+1
		sta	(vglist,Y)
		jsr	vgadd				;JSRL to reversed explosion
		lda	#$40
		sta	scalef+1
		lda	#00
		sta	vdata
		ldx	lincur
		lda	linx+1,X
		lsr	A
		ror	vdata
		clc	
		adc	#02
		sta	vdata+1
		lda	dist
		sta	vdata+2
		lda	liny+1,X
		sta	vdata+3
		lda	state
		ifne				;Maze Traversal Mode
			dec	vdata+3
		endif
		jsr	posvc2			;Position to beginning of next line
		ldy	#00
		lda	state
		ifne				;Maze Traversal Mode
			lda	linscal
		endif
		sta	(vglist,Y)
		iny	
		lda	#$71
		sta	(vglist,Y)
		iny					;Scale to overall screen scale
		jsr	nextexp			;Next picture
		pla	
		pla	
		rts	
		
;web_notused		.byte $08,$06,$04,$05,$0C,$09,$06,$08

;*****************************************************
	.sbttl "Stat to Accumulator Color and Xflip"
;*****************************************************
statexp	sta	(vglist,Y)
		iny	
		lsr	cxflip
		lda	#spstat_vpg
		ifcc
			lda	#spstat_vpg+v_xflip
		endif
		sta	(vglist,Y)
		iny					;Stat bright red and possible xflip	
		rts	
		
nextexp	lda	three
		ifeq
			ldx	lincur
			lda	linhit,X
			clc	
			adc	#01
			sta	linhit,X
			and	#07
			ifeq
				lda	#maxhitln		;Allowing maxhitln from 1 to F
				sta	linhit,X
			endif
		endif
		rts
		
;*****************************************************
	.sbttl "Collide Maze with Shots"
;*****************************************************	
mzshot	lda	#nmsshots-1
		sta	shotcur
		lda	#00
		sta	perm5
		sta	perm5+1
		begin
			ldx	shotcur
			lda	shotst,X
			ifeq
?wms15			jmp	?wms20
			endif
			bmi	?wms15
			lda	shotyh,X
			cmp	#$0A
			ifeq
				jmp	?wms20
			endif
			cmp	#09
			ifeq
				lda	shotyl,X
				cmp	#$C0
				ifcs
					jmp	?wms20
				endif
			endif
			lda	shotyl,X
			sec	
			sbc	dist
			ldy	state					;Depends on whether or not playfield has moved in the last frame
			ifne						;yes, traversal mode
				sec	
				adc	plfspd+1
				bcs	?wms17
			endif
			cmp	#shtspd&$ff
			ifcc						;No hit possible
				jmp	?wms20
			endif
?wms17		lda	shotxl,X				;Possible hit
			sta	perm1
			lda	shotxh,X
			sta	perm1+1
			jsr	addoffs				;Generates closest vertex and its distance from shot
			ldx	shotcur
			lda	shotyl,X
			sec	
			sbc	dist
			sta	temp4
			lda	shotyh,X
			sbc	#00
			ifpl
				tay	
				lda	state
				ifne						;Traversal Mode
					iny
				endif
				iny
				tya					;Compensate for scroll of liny
				asl	A
				asl	A
				asl	A
				asl	A
				ora	temp2+1			;From addoffs
				sta	temp1
				lda	#02				;Shot can hit three possible lines
				sta	perm1
				begin
					ldx	perm1
					lda	temp1
					clc
					adc	wblndir,X			;Direction of line
					sta	temp1+1
					jsr	findit
					ifpl
						lda	linhit,X
						cmp	#maxhitln
						ifcs
							cmp	#$20
							bne	?wms40
						endif
						lda	temp3
						ldy	perm1
						cpy	#02
						ifeq
							clc	
							adc	#$10
							bpl	?wms80			;Hit line to right of vertex
						else_mi
							cpy	#01
							ifeq
								sec
								sbc	#$10
								bmi	?wms80			;Hit line to left of vertex
							else_pl 
								cmp	#$80				;Take absolute value
								ifcs
									eor	#$FF
									adc	#00
								endif
								cmp	#$10
								bcc	?wms80			;Hit line of lesser Y
							endif
						endif
					endif
?wms40				dec	perm1
				miend
			endif
			jmp	?wms20
			
?wms80		inc	perm5
			lda	linhit,X
			ldy	perm1
			clc	
			adc	#01
			cmp	#maxhitln
			ifeq
				jsr	addpts
				lda	linhtpt,Y
			else_pl
				cmp	#$21
				ifeq
					lda	shipst			;Ship exploding?
					ifpl					;Nope
						jsr	addpts
						lda	linhtdd,Y
					else_ne				;Yes
						lda	#$20
					endif
				endif
			endif
			sta	linhit,X
			ldx	shotcur
			lda	#$C0
			sta	shotst,X
			lda	temp4
			eor	#$FF
			sec	
			adc	shotyl,X
			sta	shotyl,X
			ifcs
				inc	shotyh,X
			endif
?wms20		dec	shotcur
		miend
		lda	perm5+1
		ifne
			lda	#snd_f4
			jmp	dosound
		endif
		lda	perm5
		ifne
			lda	#snd_f3
			jmp	dosound
		endif
		rts	
			
wblndir	.byte $F0,$FF,$01
linhtpt	.byte maxhitln,$10,$10
linhtdd	.byte maxhitln,$18,$18


addpts	txa	
		pha	
		lda	#$25
		jsr	bpont2			;2500 Points
		inc	perm5+1
		pla	
		tax	
		rts	
		
;***************************************************
	.sbttl "Find a hypothetical segment"
;***************************************************
findit	lda	#nweb-1
		sta	webscur			;Ignore spinners A dn B, they cannon interact
?wfi10	ldy	webscur
		ldx	linstr,Y
		jmp	?wfi30
?wfi20	lda	linlst-1,X
		cmp	temp1
		ifne
			cmp	temp1+1
			bne	?wfi30			;First vertex doesn't fit, discard it.
		endif
		lda	linlst,X
		cmp	temp1
		ifne
			cmp	temp1+1
			bne	?wfi30
		endif
		jmp	?wfi40
?wfi30	inx	
		txa	
		cmp	linend,Y
		bne	?wfi20
?wfi35	dec	webscur
		lda	webscur
		cmp	#02
		bpl	?wfi10
		bmi	?wfi50			;Branch to Failure code
?wfi40	dex	
		lda	#00
?wfi50	rts	

;**************************************************
	.sbttl "Collide Players Ship with Maze"
;**************************************************
mzship	lda	shipst			;Already Exploding?
		bmi	?wmz10			;Yes, get out of here
		beq	?wmz10
		lda	state				;Are we moving?
		ifeq					;No, we can't possibly collide
?wmz10		rts					;get out of here!
		endif
		lda	shipxl
		sta	perm1
		lda	shipxh
		sta	perm1+1
		jsr	addoffs
		lda	temp2+1
		ora	#$A0
		sta	temp1				;Closest maze vertex YX coordinate
		lda	#03
		sta	temp4+1
		begin
			ldy	temp4+1
			lda	temp1
			clc	
			adc	mzlnco,Y
			sta	temp1+1			;Four possible directions of line
			jsr	findit
			bmi	?wmz50
			lda	linhit,X
			cmp	#maxhitln
			ifcs
				cmp	#$20
				bne	?wmz50
			endif
			ldy	temp4+1
			lda	dist				;Find radius of ship at relavant place
			cmp	#$71
			ifcc
				lda	shprad,Y
			else_pl
				cmp	#$8B
				ifcc
					lda	shprad+4,Y
				else_pl
					cmp	#$B5
					ifcc
						lda	shprad+8,Y
					else_pl
						lda	shprad+$c,Y
					endif
				endif
			endif
			beq	?wmz50			;0 radius means no collision possible
			sta	temp4				;Radius
			clc	
			adc	temp3
			sta	temp5+1			;Position + Radius
			lda	temp3
			sec	
			sbc	temp4
			sta	temp5				;Position - Radius
			cpy	#00
			ifeq
				lda	temp5				;Collision if positive
				bpl	?wmz60
				lda	temp5+1
				bpl	?wmz60
			else_mi
				cpy	#01
				ifeq
					lda	temp5
					bmi	?wmz60
					lda	temp5+1
					bmi	?wmz60			;Collision if negative
				else_pl
					lda	temp5
					bpl	?wmz70			;Collision if straddles zero
					lda	temp5+1
					bpl	?wmz60
				endif
			endif
?wmz50		jmp	?wmz70
?wmz60		lda	#$20
			sta	linhit,X
			lda	#$80
			sta	shipst			;Mark Ship as exploding
			lda	dist+1
			cmp	maplen			;Did we finish the maze?
			ifcs					;Yes
				lda	#$40
				sta	lauen				;Move on!
			endif
?wmz70		dec	temp4+1
		miend
		lda	shipst			;Ship Exploding?
		ifmi					;yes
			jsr	blowship			;80 in shpst is not enough
		endif
		rts	
			
radone	=	$08
radtwo	=	$30

mzlnco	.byte $01,$FF,$F0,$10
shprad	.byte 0,0,0,radtwo,radone,radone,radone,radtwo,radtwo,radtwo,radtwo,radtwo,0,0,radtwo,0

;*******************************************
	.sbttl "compensate for ship parallax"
;*******************************************
addoffs	lda	perm1+1
		asl	A
		tay	
		lda	perm1
		ifmi
			iny
		endif					;Y contains uncorrected X maze coordinate
		lda	perm1				;Find the grid point closest to the ship or shot
		clc	
		adc	parallax-5,Y
		sta	temp2
		lda	perm1+1
		adc	#00
		tax	
		lda	conbck,X
		sta	temp2+1
		bit	temp2
		ifmi
			inc	temp2+1			;Contains closest maze vertix X coordinate
		endif
		ldx	temp2+1
		lda	temp2
		sec	
		sbc	convl,X
		sec	
		sbc	#$40
		sta	temp3				;Correct ship LSB relative to closest maze vertex
		rts	
		
parallax	.byte $32,$38,$3C,$3E,$41,$44,$45,$47,$49
			
;*******************************************
;* Walking Spinner Animation Frames
;*******************************************	
t_slines	
        ;Q1->Q3 Folds 
        ;First group @ 45 degrees
        jsrl(lvert00)
		jsrl(lvert01)
		jsrl(lvert02)
		jsrl(lvert03)
		jsrl(lvert04)
		jsrl(lvert05)
		jsrl(lvert06)
		jsrl(lvert07)
		jsrl(lvert08)       
        ;@54 degrees
		jsrl(lvert10)
		jsrl(lvert11)
		jsrl(lvert12)
		jsrl(lvert13)
		jsrl(lvert14)
		jsrl(lvert15)
		jsrl(lvert16)
		jsrl(lvert17)
		jsrl(lvert18)       
        ;@63 degrees
		jsrl(lvert20)
		jsrl(lvert21)
		jsrl(lvert22)
		jsrl(lvert23)
		jsrl(lvert24)
		jsrl(lvert25)
		jsrl(lvert26)
		jsrl(lvert27)
		jsrl(lvert28)      
        ;@72 degrees
		jsrl(lvert30)
		jsrl(lvert31)
		jsrl(lvert32)
		jsrl(lvert33)
		jsrl(lvert34)
		jsrl(lvert35)
		jsrl(lvert36)
		jsrl(lvert37)
		jsrl(lvert38)        
        ;@81 degrees
		jsrl(lvert40)
		jsrl(lvert41)
		jsrl(lvert42)
		jsrl(lvert43)
		jsrl(lvert44)
		jsrl(lvert45)
		jsrl(lvert46)
		jsrl(lvert47)
		jsrl(lvert48)
        ;@90 degrees
		jsrl(lvert50)
		jsrl(lvert51)
		jsrl(lvert52)
		jsrl(lvert53)
		jsrl(lvert54)
		jsrl(lvert55)
		jsrl(lvert56)
		jsrl(lvert57)
		jsrl(lvert58)
		
hfold   jsrl(hfold20)
		jsrl(hfold21)
		jsrl(hfold22)
		jsrl(hfold23)
		jsrl(hfold24)

		jsrl(lhorz00)
		jsrl(lhorz01)
		jsrl(lhorz02)
		jsrl(lhorz03)
		jsrl(lhorz04)
		jsrl(lhorz05)
		jsrl(lhorz06)
		jsrl(lhorz07)
		jsrl(lhorz08)
        
		jsrl(lhorz10)
		jsrl(lhorz11)
		jsrl(lhorz12)
		jsrl(lhorz13)
		jsrl(lhorz14)
		jsrl(lhorz15)
		jsrl(lhorz16)
		jsrl(lhorz17)
		jsrl(lhorz18)
        
		jsrl(lhorz20)
		jsrl(lhorz21)
		jsrl(lhorz22)
		jsrl(lhorz23)
		jsrl(lhorz24)
		jsrl(lhorz25)
		jsrl(lhorz26)
		jsrl(lhorz27)
		jsrl(lhorz28)
        
		jsrl(lhorz30)
		jsrl(lhorz31)
		jsrl(lhorz32)
		jsrl(lhorz33)
		jsrl(lhorz34)
		jsrl(lhorz35)
		jsrl(lhorz36)
horz37  jsrl(lhorz37)
		jsrl(lhorz38)
        
		jsrl(lhorz40)
		jsrl(lhorz41)
		jsrl(lhorz42)
		jsrl(lhorz43)
        
		jsrl(lhorz44)
		jsrl(lhorz45)
		jsrl(lhorz46)
		jsrl(lhorz47)
		jsrl(lhorz48)       
		jsrl(lhorz50)

		
spinners	
        jsrl(spinner0)
		jsrl(spinner1)
		jsrl(spinner2)
		jsrl(spinner3)
		
cerexp	jsrl(cerexp0)
		jsrl(cerexp1)
		jsrl(cerexp2)
		jsrl(cerexp3)
		jsrl(cerexp4)
		jsrl(cerexp5)
		jsrl(cerexp6)
		jsrl(cerexp7)
		
t_sturn1	.byte $6C,$7E,$90,$A2,$B4,$C6,$B4,$A2,$90,$7E,$6C
t_sturn2	.byte $90,$A2,$00,$00,$00,$00,$00,$00,$00,$A2,$90
		
hvframenum	.byte $C6
vhframenum	.byte $D0

;these tables are indexed from t_spinoff way above and 
;they are indexes into the vertical and horizontal fold
;table
t_hspini
	
hspini0 .byte $00,$76,$88,$9A,$AC,$BE,$BC,$AA,$98,$86,$74
hspini1 .byte $64,$76,$88,$9A,$AC,$CE,$BC,$AA,$98,$86,$00
hspini2 .byte $74,$86,$98,$AA,$BC,$BE,$AC,$9A,$88,$76,$00
hspini3 .byte $00,$86,$98,$AA,$BC,$CE,$AC,$9A,$88,$76,$64
hspini4 .byte $00,$12,$24,$36,$48,$5A,$48,$36,$24,$12,$00
hspini5 .byte $10,$22,$34,$46,$58,$6A,$58,$34,$34,$22,$10

;bit 80 here is xflip
;bit 40 here is adc/sbc from table above		
t_svdat2	
        .byte $00,$00,$00,$00,$00,$00,$C0,$C0,$C0,$C0,$C0
        .byte $00,$00,$00,$00,$00,$C0,$C0,$C0,$C0,$C0,$C0
        .byte $40,$40,$40,$40,$40,$80,$80,$80,$80,$80,$80
        .byte $40,$40,$40,$40,$40,$40,$80,$80,$80,$80,$80
        .byte $00,$00,$00,$00,$00,$00,$80,$80,$80,$80,$80
        .byte $40,$40,$40,$40,$40,$40,$C0,$C0,$C0,$C0,$C0


;*********************
; EXTERNALS
;*********************
.export enem2
        