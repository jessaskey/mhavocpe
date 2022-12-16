;***********************************************
    .TEXT ".TWMAZE."
;***********************************************
    .title "TWMAZE - Maze Routines"
;***********************************************
    .sbttl "Draw Maze"
;**************************************************
;* This routine will draw the maze on the screen  
;* based on the values given in maze X and maze Y 
;**************************************************
drawm   jsr ?opcl           	;Open close doors
        lda #00
        sta linen               ;Start with line 0
        lda #$30
        ldx #$72                ;QQQSCALQQQ
        jsr vgadd2
        lda isfinal
        ifne
            lda #fsscol+$e0			;Final Maze Color
        else
			lda difcty
			cmp #maxdif
			ifeq
				;This needs to pulse based upon bits $70 on frame.			
				lda frame
				and #$3C 
				lsr A 
				lsr A 
				tax 
				lda spcolbri,X
				ora #spmzcol
				;lda #spmzcol+$e0
			else
				lda #mazcol+$e0			;Normal Maze Color
			endif
        endif
        bit objst+zreactor
        ifmi                    ;Blowing up?
            ldy retime          ;Not yet
            cpy #$10                ;Only during last 10 counts
            ifcc                    ;yeah, do this
                lda frame               ;Flash color
                lsr A
                ora #$A0
            endif
        endif
        ldx #$60               ;Stat page select
        ldy tspark
        ifne
            ldx #$60+sparkle         ;Sparkle it
        endif
        bit nodraw          ;Skip draw?
        ifmi
            lda #00             ;Draw, but do it black
        endif
        jsr vgadd2
        jsr unitp          	;Get input pointer
        lda mazexh          ;Do we need to change position?
        ifmi                    ;yes
            jsr neg
        else
            lda #00         ;else use 0
        endif
        sec 
        sbc #xoffset
        sta vdata+1
        lda mazexl
        ifeq                    ;0 is a problem
            inc mazexl
            lda #01
        endif
        jsr neg
        sta vdata
        lda #01
        bit mazeyh          ;Need to move down?
        ifpl                    ;yep
            sec 
            sbc mazeyh
        endif
        sta vdata+3
        clc 
        adc #vunits-1           ;Number of units to draw
        sta temp4               ;Units to draw
        lda mazeyl
        ifeq
            inc mazeyl
            lda #01
        endif
        jsr neg
        sta vdata+2
        clc 
        adc ymot                ;Add in scroll amount
        sta vdata+2
        lda #00
        adc #00             ;Save the carry
        sta temp8+1
        lda #00
        sta vgbrit
        lda #06
        sta temp3               ;Initial Guess
        bit mazeyh
        ifmi
            lda mazeyh
            sec 
            sbc ymot+1
            sbc temp8+1         ;Prop carry from above
            bit ymot+1          ;The famous fudge factor
            ifmi
                clc
                adc #01
            endif
            jsr neg
            tax                 ;Number of lines to skip
            begin
                jsr ?skunit         ;Skip H line, point to start of next
                ifmi
                    rts
                endif
                dex
            eqend
        else
            sec 
            sbc mazeyh          	;Skip this many
            sta temp3               ;Temp3 hold line count
        endif
        lda vdata               ;Add in camera scroll
        clc 
        adc xmot                ;From targx routine
        sta vdata
        lda vdata+1
        bit xmot+1          ;Which way??
        ifmi
            lda #00             ;For right hand scroll
            adc #00
            sta temp8
        else
            adc xmot+1          ;if moving right, draw different
            sta vdata+1
            ifpl                    ;Drawing Right???
                cmp #01             ;Skip if drawing too far off right
                ifcs
                    rts
                endif
            endif
        endif
        begin
            ldy #02
            lda (mazpt,Y)       ;Is this a blank line?
            ifne
                jsr vgcntr      	;Return to center for more
                jsr vgvtr2
                jsr ?unit          	;Draw a line
            endif
            jsr ?skunit         ;else skip this one
            bmi ?done
            dec vdata+3
            dec temp4               ;Another line?
        miend
?done   lda temp4
        ifpl                    ;Skip if minus entry
            ifne                    ;Need extra lines
                lda #00
                sta vgbrit
                begin
                    lda #-3
                    sta vdata+1     ;Long H line
                    jsr vgvtr2
                    dec temp4
                miend
            endif
        endif
        rts
 
spcolbri
		.db $70,$80,$90,$A0,$B0,$C0,$E0,$E0
		.db $D0,$C0,$B0,$A0,$90,$80,$70,$60
		
;*************************************************
    .sbttl "Open/Close Doors"
;*************************************************
;* This routine will open and close the main     *
;* doors at the proper time.                     *
;*************************************************          
?opcl   lda objst+zreactor          ;Reactor status
        ifmi                        ;Blowing up?
            lda #$80
            sta openflg             ;Set flag to open
            lda objst+zstuf+3
            ifne
                lda daccy
                bne ?opc10			;Close it down
            endif
            ;lda maznum
			lda maztype
            cmp #01
            ifeq
                ldx difcty              ;WARNING: THERE IS NOT A MAXDIF CHECK HERE... 
										;THIS MAY CAUSE A BUG IF IT ISNT TRIMMED SOMEWHERE ELSE
                lda mpod,X              ;Determine if doors should open
                cmp #02
                beq ?opc10            	;Nope!
            endif
            jsr ?ocdat             ;Pointer to data in Y
            iny
            iny                    ;Pass two bytes close data
            bne ?bad1              ;Output new bytes
        else
            lda gamest
            and #$20                    ;In Maze??
            ifne                        ;yep
                bit mzgrnd              ;On the ground??
                ifmi                        ;Okay to do this now
                    lda openflg             ;Maze open??
                    ifmi                        ;Yes, close it!
?opc10                	lda #00
                        sta openflg                 ;Set open bit to 'closed'
                        jsr ?ocdat                  ;Pointer to data in Y
?bad1                   lda (mazpt,Y)
                        sta mazebuf,X               ;Store new data
                        iny
                        inx
                        lda (mazpt,Y)
                        sta mazebuf,X
                    endif
                endif
            endif
        endif
        rts

;Load Maze Open/Close data pointer
?ocdat  ;lda maznum
		lda maztype
        asl A				;Get the maze type defined for this level
        tay 
        lda mzocd+1,Y
        sta mazpt+1
        lda mzocd,Y
        sta mazpt
        ldy #00
        lda (mazpt,Y)           ;First Byte
        tax                     ;RAM output location pointer
        ; (mazpt,Y) points to close data at this point
        iny
        rts
        
;**************************************************
    .sbttl "Trap Doors (Or shut your trap!)"
;**************************************************
;* For Trap Doors:                                
;* Database:    Index to pg1,offset to time,    
;*              time, open data, close data     
;*                                                
;* Executed once every 16 frames minimum          
;**************************************************
trapal  lda dif4mz          ;Executed only once, permanant wall changes
        asl A
        tax 
        lda mztdal,X
        sta temp3
        lda mztdal+1,X
        sta temp3+1
        ldy #00
		begin
			lda (temp3,Y)
			ifeq
				rts
			endif
			tax 
			iny 
			lda (temp3,Y)
			sta mazebuf,X
			iny 
        doloop

;***********************************************
;* moving walls update routine 
;* called from tw_main
;***********************************************
trap    lda frame
        and #$0F
        eor #$0F
        ifne
            rts
        endif
		lda dif4mz
        asl A
        tax                 ;Get pointer for this maze
        ;cmp #$10
        ;bcc ?trp20
        ;lda mztd-$10,X      ;Trap door data
		lda mztd,X      ;Trap door data
        sta temp3
        ;lda mztd-$0f,X
		lda mztd+1,X
        sta temp3+1         ;Ind pointer
        ldy #00
        lda frame+1
        sta temp1
        lda frame
        ldx #03
        begin
            lsr temp1
            ror A
            dex
        miend
        sta temp1               ;Time counter
		begin
			lda (temp3,Y)           ;Get index (0 = end)
			ifeq
				rts
			endif
			tax                 ;Save pointer to pg1
			iny                 ;Point to time additive
			lda temp1
			clc 
			adc (temp3,Y)           ;Add time
			iny 
			and (temp3,Y)           ;Duty cycle
			ifeq
				iny 
				lda (temp3,Y)           ;Open it (less than or equal to half the time)
				iny
			else
				iny
				iny
				lda (temp3,Y)
			endif
			iny
			sta mazebuf,X         ;Do this one
        doloop

;***************************************************
    .sbttl "Skip Unit"
;***************************************************
;* Will skip one horiz unit and update pointers    *
;* for next unit (if one).                         *
;*                                                 *
;* Can also be used to skip to end of current unit.*
;***************************************************
?skunit inc linen               ;Next line
        jsr unitp               ;Get next pointers
        ldy #00
        lda (mazpt,Y)
        rts                 ;End of maze
        
;***************************************************
    .sbttl "Draw Unit (H line)"
;***************************************************
;* Draws 1 horizontal line of maze. Assumes        
;* position of vector already set. Will draw 10d   
;* segments or until end of current line, which    
;* ever occurs first.                              
;*                                                 
;* RAM: mazpt,mazpt+1   Pointer to current   
;*                      maze stamp. 0 is end 
;*                      of line or end of    
;*                      maze if the only     
;*                      number in a unit.    
;*                                                 
;* Uses:    temp9, temp2                           
;***************************************************
?unit   ldy #00             ;For indirect pointers
        lda #hunits-1           ;10 stamps to right max
        sta temp9               ;Guess correct
        lda mazexh
        ifmi                    ;Another weird fudge factor
            clc
            adc #01
        endif
        bit xmot+1          ;Moving left or right?
        ifmi
            sec 
            sbc xmot+1
            sbc temp8
            clc 
            adc #01
        endif
        tax
        ifne                    ;Skip for 0
            ifpl                    ;Skip for minus
                begin
                    lda (mazpt,Y)
                    bpl ?dmu10          ;End of maze if -
?dmu5               rts
?dmu10              beq ?dmu5               ;End of unit if 0
                    jsr incmaz
                    dex                 ;Any more stamps?
                eqend
            else                        ;We want to skip a few on right now
                lda temp9
                clc                     ;A = temp9
                adc mazexh              ;Adding a negative number
                bit xmot+1              ;Did we move right?
                ifpl
                    sec
                    sbc xmot+1              ;Skip this many
                endif
                sta temp9                   ;Update count (less than 12d)
            endif
        endif
        lda (mazpt,Y)           ;Get first stamp
        ifeq
            rts                 ;Started at end of line
        endif
        ifpl                    ;Not end of maze
            begin
                and #$0F                ;Stamp code
                asl A               	;Need a word pointer
                tay 
                lda mazet-2,Y           ;Get maze JSRL
                ldx mazet-2+1,Y         ;-2 because there is no stamp 0!!!!!
                jsr vgadd2
                dec temp9               ;Did one
                bmi ?badhab         	;Escape out
                jsr incmaz          	;Bump pointer
                lda (mazpt,Y)           ;Next stamp
            eqend                   ;*** Always!!! ***
        else                    ;Must be end of maze
            rts                 	;Return A = 80
        endif           
?badhab rts
        
;******************************************************
    .sbttl "Generate New Maze"
;******************************************************
;* Generates a new maze, places all objects and such  *
;* in the maze.                                       *
;******************************************************
drawmaze 
		jsr stpg6				;ROM Page to tw_maze data
        jsr ?placeall           ;Place always objects
        lda holmz               ;if new maze, zero all statuses
        ifeq
            ldx #nmfire-1
            begin
                sta objst+zfire,X
                dex
            miend
            ldx #nmrob-1
            begin
                sta objst+zrobot,X
                dex
            miend
            ldx #nmmax-1
            begin
                sta objst+zmax,X
                dex
            miend
			ldx #nmlock-1
			begin
				sta objst+zlock,X
				dex
			miend
			ldy isfinal			;final station, don't reset keys between levels
			ifeq
?bkeys			ldx #nmkeys-1
				begin
					sta objst+zkeys,X
					dex
				miend
			;else
			;	ldy maznum
			;	beq ?bkeys			;Erase keys on Level 21, but not 22,23,24
			endif
			sta toktarg
			sta animate
			;sta tokretr		;can't set this as it clears our target?
			sta obsst+1			;reset any token defined previously
        endif
        ldx dif4mz          	;Load in reactor time
        lda outime,X            
        sta lastreac            ;Need this now that reactor can be turned back off
		lda mscstl,X
#IF FORCE_HOMEWORLD_BONUS != 0
		ldx vxcount
		ifeq				;Only force if not already forced
			ldx vxstat
			ifeq
				lda #-1
			endif
		endif
#ENDIF
		sta isfinal
		;*********************************************
		; Load Token info for this maze if defined
		;*********************************************
		ldy #03
		begin
			tya
			asl A 
			asl A
			asl A		;x8
			tax
			lda mtok,X
			ifpl		;If positive, then we have a token definition here
				cmp dif4mz
				ifeq		;and it also matches our current level, lets load it
					tya
					ora #04				;Set a bit so this object is active, even if index is $00 (for 'locate')
					sta obsst+1			;This signals we ahve a token and contains the token style in bits $03
					lda mtok+1,X
					sta obssxl+1
					lda mtok+2,X
					sta obssxh+1
					lda mtok+3,X
					sta obssyl+1
					lda mtok+4,X
					sta obssyh+1
					lda mtok+5,X
					sta toktarg
					lda mtok+6,X
					sta tokretr
					;lda mtok+7,X
					;sta tokvisi
					;break out of here now on first matches
					jmp ?tokdon
				endif
			endif
			dey
		miend
?tokdon	;*****************************
		;CHANGE PAGE TO MAZEDATA2 
		;*****************************
		jsr stpg7
		;*****************************
		lda dif4mz
		asl A					;Get WORD based maze index
        tax
        lda mzinit,X            ;Maze fireball source
        sta temp1
        lda mzinit+1,X          ;Init data location
        sta temp1+1
        ldy #00                 ;Get the data
        lda #01
        sta objst+zreactor      ;Turn on reactor
        ;lda (temp1,Y)           ;Reactor XL
        ;ifeq
        ;    lda #$00                ;If reactor XL is Zero, then Reactor is 'hidden' on this level (HW level 1)
        ;    sta objst+zreactor
        ;endif
        lda (temp1,Y)           ;Reactor XL
        sta objxl+zreactor
        iny 
        lda (temp1,Y)           ;Reactor XH
        sta objxh+zreactor      
        iny 
        lda (temp1,Y)           ;Reactor YL
        sta objyl+zreactor
        iny 
        lda (temp1,Y)           ;Reactor YH
        sta objyh+zreactor
        ldx #zfire              ;Place for first fireball
        iny 
        begin                   ;Now do fireballs and robots
            lda (temp1,Y)       ;Get first XL value or flag value
            cmp #$FF
            ifeq
                jmp ?gnm10              ;$ff ends list
            endif
            cmp #$FE                ;$fe ends fireballs, starts robots
            ifeq
                cpx #zrobot         ;If less than zrobot, then do robots next
                iflt
                    ldx #zrobot
                else
                    ldx #zmax			;otherwise, we do MAX robots next
                endif
                iny
                lda (temp1,Y)       ;we moved, get the actual XL value
				cmp #$FE
				ifeq
					;special case where there are Max robots but no Perkoids
					ldx #zmax
					iny
					lda (temp1,Y)
				endif
            endif
            sta objxl,X
            sta oldxl,X
            lda #00
            sta ltcol,X         ;Make sure collisions are reset
            sta rtcol,X
            sta headcol,X
            iny 
            lda (temp1,Y)       ;Fireball/Robot XH
            sta objxh,X         
            iny 
            lda (temp1,Y)       ;Fireball/Robot YL
            sta objyl,X         
            iny 
            lda (temp1,Y)       ;Fireball/Robot YH
            sta objyh,X   
            iny             
            cpx #zmax
            ifcs                    ;must be max...
                lda (temp1,Y)           ;get the distance parameter
                sta maxdata-zmax,X      ;Save config data for movement
            else
                lda (temp1,Y)       ;Velocity X
                jsr ?incvel         ;Check to see if this is an incrementing velocity			
                sta velxh,X
                cpx #zrobot
                ifcs  				
					sta robdir-zrobot,X     ;Save initial direction  
					sta robvel-zrobot,X     ;Save his velocity!				  
                endif
                iny 
                lda (temp1,Y)           ;Velocity Y
                jsr ?incvel																		
                sta velyh,X             ;Fireball Y velocity
            endif
            bit holmz               ;Only old objects??
            ifpl
                inc objst,X         ;if first time around, turn on objects
            else
                lda objst,X
                ifmi
?gnm9               lda #00
                    sta objst,X         ;Bring back to initial status
                else
                    ora limbo,X         ;if alive or transporting when man died
                    beq ?gnm9
                    lda #01
                    sta objst,X         ;Bring back to initial status
                endif
            endif
            iny
            inx
        eqend               
		;EXITS IN LOOP ABOVE
?gnm10  ;*****************************
		;BACK TO PRIMARY MAZEDATA
		jsr stpg6
		;*****************************
		ldx #00             ;Initialize Keys and Locks
        ldy #00
        bit holmz
        ifpl
            begin
                lda (perm1,Y)       	;Color of set
                beq ?gnm20          	;No more
                sta objst+zkeys,X
                sta objst+zlock,X
                iny
                lda (perm1,Y)			;Key Position
                jsr ?crack
                sta objxh+zkeys,X
                lda perm5
                sta objyh+zkeys,X
                iny 
                lda (perm1,Y)			;Lock Position
                jsr ?crack
                sta objxh+zlock,X
                lda perm5
                sta objyh+zlock,X
                iny 
                inx
            eqend                   ;Never
        endif
?gnm20  lda #$80
        sta holmz           ;Hold init until next new maze
        lda frame
        and #$FC
        sta frame           ;Force start
        lda #00             ;Make sure no objects are in limbo at wave start
        ldx #nmmotobj-1		;Motion objects
        begin
            sta limbo,X
            dex
        miend
		;Load the Reactor size flag into limbo+zreactor, 0 = normal, 1 = big
		ldx dif4mz
		lda reacsz,X 
		sta limbo+zreactor	;Reactor's slot used for sizing
        lda #$83
        sta objfrm          ;Create new map buffer
        ;********************************************
			.sbttl "Copy Maze to RAM"
		;********************************************
		;* This routine copies the current maze to  
		;* RAM so that dynamic maze changes may take
		;* place.                                   
		;********************************************
		;lda maznum
		lda maztype
        asl A               ;To words
        tay 
        lda mazsrc+1,Y
        sta mazpt+1
        lda mazsrc,Y
        sta mazpt           ;Point to maze top
        ldy #-1             ;Will bump to 0 next time
        begin
            iny                 ;Next byte
            lda (mazpt,Y)
            sta mazebuf,Y         ;Store into RAM
        miend
        rts

;************************************
; Crack - Split a blended data point
;************************************        
?crack  sta perm5+1         ;Store away initial data
        lsr A
        lsr A
        lsr A
        lsr A
        ora #$F0                ;Recreate Y coordinate
        sta perm5
        lda perm5+1
        and #$0F
        clc 
        adc #01             ;X coordinate in accumulator
        rts 
        
?incvel cmp #$70
        ifcs
            cmp #$90                ;Is this an incrementing velocity??
            ifcc                    ;yes
                cmp #00
                ifmi
                    and #$7F            ;Sign extend to all 8 bits
                else
                    ora #$80
                endif
                sta perm4               ;Amount to be added to velocity for each difficulty level
                iny
                lda (temp1,Y)           ;Base velocity
                stx perm4+1
                ldx gamedif
                dex
                ifpl                    ;Don't add anything if difficulty zero
                    begin
                        clc
                        adc perm4
                        dex 
                    miend
                endif
                ldx perm4+1         ;Restore X
            endif
        endif
        rts
        
;**************************************************
    .sbttl "Place all the maze Stuff"
;**************************************************
;* This routine is used to place all the   
;* discs and arrows back in the maze        
;* whenever needed without having to        
;* re-init all other objects.               
;**************************************************         
?placeall    
        ;jsr getdifctyw      
		lda dif4mz
		asl A 				;Get WORD based level index
        tax 
        lda mzdc,X          ;Disc Source Data
        sta temp1               
        lda mzdc+1,X
        sta temp1+1
        lda mzar,X          ;Arrow Source Data
        sta temp3               
        lda mzar+1,X
        sta temp3+1
        lda mztr,X          ;Trip Pads
        sta temp4
        lda mztr+1,X
        sta temp4+1
        lda mzlg,X          ;Lightning Source Data
        sta temp5
        lda mzlg+1,X
        sta temp5+1
        lda mone,X          ;One Way Arrows Source Data
        sta temp6
        lda mone+1,X
        sta temp6+1
        lda mtite,X         ;Stalactites
        sta temp7
        lda mtite+1,X
        sta temp7+1
        lda mlock,X         ;Lock Source Data
        sta perm1
        lda mlock+1,X
        sta perm1+1
        lda mtran,X         ;Transporter Source Data
        sta perm2
        lda mtran+1,X
        sta perm2+1
        lda mhand,X         ;DeHand
        sta perm3
        lda mhand+1,X
        sta perm3+1
        lda #00
        ldx #nmlsht+nmcann-1        ;No laser shots or laser cannons
        begin
            sta objst+zlsht,X
            dex
        miend
        ldx #nmshot-1  		;No robot shots
        begin                   
            sta objst+zshot,X
            dex
        miend
		
		ldx #nmtite-1   	;No tites
        begin            
            sta objst+ztite,X
            dex
        miend
		ldx #nmtran-1   	;No transporters
        begin            
            sta objst+ztran,X
            dex
        miend
		
		sta objst+zspecial	;zero player ship 
		sta objst+ztoken	;zero token
		
		ldx #nmstuf-1
		begin
			sta objst+zstuf,X       ;No One of a Kinds
			dex
		miend
		
        ldx #nmdisc+nmligh+nmfrfl+nmtrpp+nmonew+nmarow-1
        begin
            sta objxh+zdisc,X       ;No static objects
            dex
        miend
        ldx #nmonew-1           ;No one way signs
        begin
            sta onewst,X
            dex
        miend
        lda dif4mz
        tax                 
        lda mclock,X        ;Initialize Clock
        ifne
            jsr ?crack
            sta objxh+zstuf
            lda perm5
            sta objyh+zstuf
            lda #01
            sta objst+zstuf
        endif
		
        lda mboots,X        ;Initialize boots
        ifne
            jsr ?crack
            sta objxh+zstuf+1
            lda perm5
            sta objyh+zstuf+1
            lda #01
            sta objst+zstuf+1
        endif
		
        ;KeyPouch
		lda mkeyp,X        ;Initialize keypouch
		ifne
			jsr ?crack
			sta objxh+zstuf+4
			lda perm5
			sta objyh+zstuf+4
			lda #01
			sta objst+zstuf+4
		endif	
		
        ;Init Escape Pod
		lda maztype
        cmp #01             ;Only on Maze Pattern #1 tho
        ifeq
            ldx difcty
            cmp #maxdif
            ifcs
                lda #maxdif
            endif
            lda mpod,X
            ifne
                lda #01
            endif
            sta objst+zstuf+2
            lda #$10
            sta objxh+zstuf+2
            lda #$F6
            sta objyh+zstuf+2
            lda #00
            sta epodfr
            sta epodgr
        endif
		
        ;DeHand
        ldy #00
        lda (perm3,Y)
        ifne
            lda #01
            sta objst+zstuf+3
            lda #00
            sta daccx               ;Starts under the box
            lda (perm3,Y)
            sta objxh+zstuf+3
            iny 
            lda (perm3,Y)
            sta objyh+zstuf+3
            iny 
            lda (perm3,Y)
            sta naccx               ;Number of X accordians
            iny 
            lda (perm3,Y)
            sta naccy               ;Number of Y accordians
            iny 
            lda (perm3,Y)
            sta maccx               ;Maximum X degrees
            iny 
            lda (perm3,Y)
            sta raccy               ;Rest Y degrees
            sta daccy               ;Is what it starts at
            iny 
            lda (perm3,Y)
            sta maccy               ;Maximum Y degrees
            iny 
            lda (perm3,Y)
            sta hxtend          	;Visual Size, so draw accordian properly
            iny 
            lda (perm3,Y)
            sta hytend
        endif
		
        ;Initialize Stalactites
        ldx #ztite
        ldy #00
        begin
            lda (temp7,Y)
            beq ?tidone
            jsr ?crack
            sta objxh,X
            lda perm5
            sta objyh,X
            inc objst,X         ;Turn on
            inx
            iny
        eqend                   ;Never
 
?tidone ldx #00
        ldy #00
        begin
            lda (temp6,Y)           ;Init wall positions
            beq ?pla15              ;End of data
            cmp #$FF                ;Points to left if $ff
            ifeq
                iny
                lda #$80
            else_ne
                lda #01             ;Points to right
            endif
            sta onewst,X
            lda (temp6,Y)
            jsr ?crack
            sta objxh+zonew,X
            lda perm5
            sta objyh+zonew,X
            inx
            iny
        eqend                   ;Never
?pla15  ldy #00
        ldx #00
        begin
            lda (temp1,Y)           ;Init disc positions
            beq ?pla20              ;End of data
            jsr ?crack
            sta objxh+zdisc,X
            lda perm5
            sta objyh+zdisc,X
            inx
            iny
        eqend                   ;Never
?pla20  ldy #00
        ldx #00
        begin
            lda (temp3,Y)       ;Init arrow positions
            ifne                ;End of data
                jsr ?crack
                sta objxh+zarow,X
                lda perm5
                sta objyh+zarow,X
                iny
                lda (temp3,Y)
                sta ardir,X         ;Direction of arrow
                inx
                iny
            endif
        eqend                   ;Never
        ldy #00
        ldx #00
        begin
            lda (temp5,Y)           ;Init lightning positions
            beq ?pla30
            cmp #$FF
            ifeq                    ;Goto vertical ones
                ldx #nmligh
                iny
                lda (temp5,Y)
            endif
            jsr ?crack
            sta objxh+zligh,X
            lda perm5
            sta objyh+zligh,X
            inx
            iny
        eqend                   ;Never
?pla30  ;Trip Pads get drawn here
        ldy #00
        ldx #00
        begin
            lda (temp4,Y)           ;Init Point positions
            beq ?pla35              ;End of data, get out
            jsr ?crack
            sta objxh+ztrpp,X
            lda perm5
            sta objyh+ztrpp,X
            inx
            iny
        eqend                   ;Never
?pla35  ;Lasers!
        ;jsr getdifctyb
		lda dif4mz
		asl A
		asl A 				;X8 (4 Words per level)
		asl A 
        tay
		ldx #00
		begin
			sty temp7			;Save base index for this maze
			lda mcan,Y          ;Start of Laser Pointer Table
			ora mcan+1,Y		;OR both bytes
			ifne                    ;If both are not Zero, then this has a pointer address
				lda mcan,Y        ;Start of pointer table
				sta temp2
				lda mcan+1,Y
				sta temp2+1         
				ldy #0
				lda (temp2,Y)           ;X LSB
				iny 
				sta objxl+zcann,X
				lda (temp2,Y)           ;X MSB
				iny 
				sta objxh+zcann,X
				lda (temp2,Y)           ;Y LSB
				iny 
				sta objyl+zcann,X
				lda (temp2,Y)           ;Y MSB
				iny 
				sta objyh+zcann,X
				lda #00
				sta velxh+zcann,X       ;No initial velocity
				sta velyh+zcann,X
				sta cannfr,X            ;No wait before action
				sta cannss,X            ;No tube_vpg spinning or shooting
				sta canngr,X            ;Point to right barrel all the way out
				sta canndf,X            ;Difficulty of cannon
				lda #04
				sta cannin,X            ;Points to start of movements 
				lda #01
				sta objst+zcann,X       ;Turn it on!!
				lda temp2
				sta cannp1,X
				lda temp2+1
				sta cannp2,X
			endif
			ldy temp7
			iny
			iny
			inx
			cpx #nmcann 
		eqend
		
        ;Initialize Transporter Booths
        ldx #00             
        stx tspark
        stx mestim
        stx jmptim
        stx fldcnt
        stx jumprv          ;Make sure idiot hints are initialized
        ldy #00             
        begin
            lda (perm2,Y)           ;Color of Pair
            beq ?pla40              ;Zero ends list
            sta objst+ztran,X
            iny 
            lda (perm2,Y)
            jsr ?crack
            sta objxh+ztran,X       ;X MSB of first
            lda perm5
            sta objyh+ztran,X       ;Y MSB of first
            lda #00
            sta tranhi,X            ;No sparkels in resting transporter
            iny 
            inx
        eqend                   ;Never
?pla40  iny 
        lda #00             ;Load compacted trasportability info
        sta temp2
        begin
            lda (perm2,Y)
            cmp #$EE
            beq ?pla45          ;$EE ends trasportability flags list
            sty temp2+1
            ldx #07
            begin
                ldy temp2+1
                lda (perm2,Y)
                and ?transmsk,X         ;Crack data
                ifne
                    lda #$80
                endif
                ldy temp2
                sta cktran,Y
                inc temp2
                dex
            miend
            ldy temp2+1
            iny
        eqend
?pla45  ldx #ntrans-1
        begin
            lda ?tspkls,X
            sta ttran,X         ;Stagger Transporter Sparkels
            dex
        miend
        rts
            
?tspkls		.byte $06,$03,$05,$02,$05,$01,$04,$00

;Bit masks for extracting transporter bit data for object transportability
?transmsk   .byte $80,$40,$20,$10,$08,$04,$02,$01

;***************************************************
    .sbttl "Initialize Maze Exit Arrows"
;***************************************************
newarrow    
        lda dif4mz
        asl A
        tax 
		lda mzor,X
		sta tempa
		lda mzor+1,X
		sta tempa+1
		lda #00
		ldx #nmarow-1
		begin
			sta objxh+zarow,X
			dex
		miend
		tax 
		tay
		begin
			lda (tempa,Y)       ;Init arrow positions
			beq ?nar10          ;End of data
			jsr ?crack
			sta objxh+zarow,X
			lda perm5
			sta objyh+zarow,X
			iny
			lda (tempa,Y)
			sta ardir,X         ;Direction of arrow
			inx
			iny
		eqend
?nar10  rts 


