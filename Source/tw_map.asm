;***********************************************
    .TEXT ".TWMAP."
;***********************************************
    .title "TWMAP - Map Stuff"
;***********************************************

;*******************************************************
;* Maze Size Definitions
;* The RAM Buffer (mazebuf) sits at 0900-09FF which is
;* 256 bytes. The max vert of each width is shown below
;* total bytes can't exceed 256 bytes;
;*
;* Width:   DefinedHeight:   MaxHeight:   Maze:
;*  $10        $07             $10          0
;*  $13        $0B             $0D          3
;*  $15        $08             $0C          1
;*  $15        $09             $0C          2
;*    
;* Vertical Length Counters             
;* Number of 'drawn' lines in each maze 

mazdep  .db (m0ulast-m0u1)/(m0u2-m0u1)
		.db (m1ulast-m1u1)/(m1u2-m1u1)
		.db (m2ulast-m2u1)/(m2u2-m2u1)
		.db (m3ulast-m3u1)/(m3u2-m3u1)

;* Horizontal Length of each maze             
mazlen  .db m0u2-m0u1,m1u2-m1u1,m2u2-m2u1,m3u2-m3u1

;**************************************************
    .sbttl "Bit patterns for each stamp"
;**************************************************
;* These patterns are used to draw the little map 
;* at the top of the screen. For each stamp there 
;* is a byte which represents a bit rep of that   
;* stamp. For example.. Stamp 1 -- H line will be 
;* $c0 (only top two bits are used) for the horiz 
;* rep and will be 0 for vert.                    
;**************************************************
;There is no stamp 0, so it is blank
;                 1   2   3   4   5   6   7
mapbits .byte $00,$C0,$80,$80,$40,$40,$00,$00
mapvbt  .byte $00,$00,$40,$80,$80,$40,$C0,$00

;*************************************************
    .sbttl "Place All Objects"
;*************************************************
;* This routine will place and draw all objects  
;* that are active into the map on the top of the
;* screen.                                       
;*                                               
;* Inputs:  frame,objst,objxh,objyh              
;*                                               
;* Outputs: vectors to vglist                    
;*                                               
;* Uses:    temp6,vglist(see also locmap)        
;*************************************************
dotmap  lda #$20
        ldx #$80
        jsr vgadd2          ;All vector JSRL's assume that the generator is
        lda #00
        sta vgbrit
        ldx #$72
        jsr vgadd2          ;Scaled and centered
        lda objst           ;Display man if alive
        ifpl
            ifne
                ldx #00
                jsr ?locmap
            endif
        endif
        inc objfrm
        lda objfrm          ;Keep track of which buffer to display or update
        and #$7F
        cmp #04
        ifcs
            lda objfrm
            and #$80
            eor #$80
            sta objfrm
            jsr ?newobuf         ;Update Buffer
        endif
        ldy #00
        lda objfrm
        ifmi
            ldy #02
        endif
        lda crsbufs,Y           ;Call relevant buffer
        ldx crsbufs+1,Y
        jsr vgadd2
        ldy #nmkeys-1           ;Draw keys next to map
        sty temp9
        begin
            lda objst+zkeys,Y       ;In Rex's Pocket?
            cmp #$10
            ifcs
                jsr vgcntr
                lda #00
                ldx #$71
                jsr vgadd2
                ldy temp9
                lda topkeypos,Y             ;X to key position
                ldx #$27                    ;Y to key position
                jsr vgvtr5
                ldy temp9
                lda objst+zkeys,Y
                and #$0F
                ora #$E0
                ldx #lock_vpg
                jsr vgadd2
                lda glock+2
                ldx glock+3
                jsr vgadd2
            endif
            dec temp9
            ldy temp9
        miend
		;Draw the Keypouch if needed
		lda objst+zstuf+4
		ifne
			ifmi
				jsr vgcntr
				lda #00
                ldx #$71
                jsr vgadd2
				lda #$C8             ;X to key position
                ldx #$27             ;Y to key position
                jsr vgvtr5
				lda #($F0+colblack)
                ldx #lock_vpg+sparkle
                jsr vgadd2
				vgadd_jsrl(keypbox)
			endif
		endif
		;draw any tokens up top (in pocket)
		lda obsst+1	
		ifpl
			and #$10
			ifne
				jsr vgcntr
				;lda #00
                ;ldx #$71
                ;jsr vgadd2
				lda #$C0             ;X to tok position
                ldx #$27             ;Y to tok position
                jsr vgvtr5
				lda #$C0             ;X to tok position
                ldx #$27             ;Y to tok position
                jsr vgvtr5
				lda obsst+1	
				and #03
				sta temp2
				lda #$E0
				sta temp1
				jsr drawtok
			endif
		endif
        rts 
		
topkeypos   .byte $CC,$D2,$D8,$DE

;************************************************
    .sbttl "Create new cross buffer for radar"
;************************************************
?newobuf 
		lda vglist+1
        pha 
        lda vglist
        pha 
        lda objfrm          ;Set vglist to appropriate buffer
        lsr A
        lsr A
        lsr A
        lsr A
        ora #crsbuf/$100
        sta vglist+1
        lda #crsbuf&$ff
        sta vglist
        ldx #zshot-1
        begin
            lda objst,X
            ifne
                ifpl
                    jsr ?locmap
                endif
            endif
            dex
            cpx #01
        eqend
        ;do max robots too
        ldx #zspecial-1
        begin
            lda objst,X
            ifne
                ifpl
                    jsr ?locmap
                endif
            endif
            dex
            cpx #zmax-1
        eqend
        lda objfrm
        ifpl
            lda objst+zreactor
            ifne
				;***************************************************
				; ISSUE #90 - Objects visible on screen
				;***************************************************
				; Don't show reactor if XH is zero - cheap hide
				lda objxh+zreactor
				ifne
					ldx #zreactor
					jsr ?locmap
				endif
				;***************************************************
            endif
        endif
        ldx #$C0
        jsr vgadd2          ;RTSL at end of buffer
        pla 
        sta vglist
        pla 
        sta vglist+1            ;Restore pointer to vgram
        rts 

;*****************************************************
    .sbttl "Locate in Map"
;*****************************************************
;* This routine places and object in the map at the  *
;* top of the screen.                                *
;*                                                   *
;* Input:   (X) = object number                      *
;*                                                   *
;* Output:  vector drawn to map position             *
;*                                                   *
;* Uses:    temp1, temp2, vdata                      *
;*****************************************************
?locmap stx perm1               ;Save X
        lda #$4B                ;Assumed centered and scaled
        sta vdata               ;Assume vgbrit is zero
        lda #-1
        sta vdata+1         ;Position of upper left corner
        lda #$D2                ;$C0 + fudge of 12
        sta vdata+2
        lda #01
        sta vdata+3
        ldx perm1
        lda objxl,X
        sta temp2
        lda objxh,X
        ldy #03
        begin
            cmp #$80
            ror A
            ror temp2
            dey
        miend               ;Shift 4 times
        sta temp2+1         ;High byte
        clc 
        lda vdata
        adc temp2
        sta vdata
        lda vdata+1
        adc temp2+1
        sta vdata+1         ;Corrected X position
        lda objyl,X
        sta temp2
        lda objyh,X
        sta temp2+1
        ldy #03             ;Divide 4 times
        begin
            cmp #$80
            ror A
            ror temp2
            dey
        miend
        sta temp2+1
        lda vdata+2
        clc 
        adc temp2
        sta vdata+2
        lda vdata+3
        adc temp2+1
        sta vdata+3
        jsr vgvtr2
        ldx perm1
        ;hack for Max robots in the radar, since robot shots are under them in the object list, we have to subtract 
        ;the number of shots from the zmax value because we don't want empty jsrs in our table for the missing shots
        ; cmp #zmax
        ; ifge
            ; sbc #nmshot
        ; endif
		lda objtypetab,X
		asl A
		tay
        lda objsr,Y
        ldx objsr+1,Y
        jsr vgadd2          ;Add the JSRL to the cross
        ldx perm1
        rts 

        
;***********************************************
    .sbttl "Draw Whole Maze Routine"    
;***********************************************
;* Draw the maze, and small map and accessories 
;*                                          
;* Uses: temp1,temp2,temp3,temp4,temp5,temp6
;*       temp7,temp8,temp9,A,X,Y            
;***********************************************
domaze 	jsr drawmaze         	;Sets Page 6/7 - Draws the main maze and all objects
		;Do trap door motion
        jsr trapal      
		;Now draw the small maze
		jsr smallmap
		;Draw the maze header parts
        lda #(retbuf/$100)&$ff	;Reactor Time Buffer
        ldx #retbuf&$ff
        jsr vgjsrl
        lda # ubyte(oxybuf)		;Oxygen Counter
        ldx # lbyte(oxybuf)
        jsr vgjmpl				;NOTE: this is JMPL not JSRL
		jsr stpg0
		rts
		
smallmap			
        lda # lbyte(mapbuf)
        sta vglist
        lda # ubyte(mapbuf)
        sta vglist+1		;Save where we are going to write all this
        lda #02
        ldy #00
        sty linen           ;Line 0
        sty temp3+2
        sty temp3+3         ;Set future Y component to 0 for mapline
        sty temp7           ;Set flag for H
        jsr vgscal          ;Set scale
        lda #00             ;Set to black, game will change later
        ldx #$60
        jsr vgadd2          ;Set color of map
        jsr ?getpt2         ;Get maze source pointer
        lda #$60
        sta vdata           ;Now set up map position
        lda #-1
        sta vdata+1         ;XH
        lda #$90
        sta vdata+2         ;YL
        lda #01
        sta vdata+3         ;YH
        begin
            jsr ?clr8
            jsr vgcntr          ;Center for position
            lda #04
            sta temp5           ;Will do 3 before another center
            ldy #00
            ldx #vdata          ;Position this line
            jsr ?advec2         ;Do vgvctr and clear temp3
            begin
                ldy #00             ;Restore Y
                jsr ?build              ;Build first line of this maze
                ifne                    ;Either maze of end
                    jsr ?shift              ;Look at first bit
                    ifcs
                        jsr ?ones
                    else
                        jsr ?zeros
                    endif
                endif
                ldy #01
                lda (mazpt,Y)           ;Look at first stamp, next line
                bmi ?enddata            ;THIS IS END OF MAZE DATA
                lda mazpt
                clc 
                adc #05             ;Pass blank ends
                sta mazpt
                ifcs
                    inc mazpt+1
                endif
                dec temp5
                bmi ?dmb10          ;Done here, reposition
                lda #-$10
                sta temp8+2
                lda #$FF
                sta temp8+3
                ldx #temp8
                ldy #00
                jsr ?advec2
                jsr ?clr8
            neend
?dmb10      lda vdata+2         ;Point next line
            sec 
            sbc #$50
            sta vdata+2
            ifcc
                dec vdata+3
            endif
            lda #00
        neend                   ;Always loop
		;Jumps here from end of data above
?enddata  
		lda #00             ;Done with H lines... now do V lines
        sta temp3
        sta temp3+1         ;X component must be 0
        lda #$80
        sta temp7           ;V flag
        jsr ?getpt2         ;Repoint to top of data
        lda mazpt
        sta temp6
        lda mazpt+1
        sta temp6+1         ;Save this for next line
        lda #$68
        sta vdata
        lda #-1
        sta vdata+1
        lda #$98
        sta vdata+2
        lda #01
        sta vdata+3         ;Position for vert lines
        jsr ?clr8               ;A=0 on return
        sta temp5               ;Force first line to a center position
        begin
            ldy #00
            lda temp6               ;Reload source pointer
            sta mazpt
            lda temp6+1
            sta mazpt+1         ;xfer to mazpt
            jsr buildv          ;Build vert line
            jsr ?shift           
            ifne                    ;Assume no short little line on top
                php                 ;Save carrry
                dec temp5               ;Found a good one?
                ifmi                    ;We need to position here
                    jsr vgcntr
                    lda #04
                    sta temp5               ;Reset count
                    ldx #vdata          ;Position
                else                    ;else just add return vector
                    ldx #temp8
                endif
                ldy #00             ;Don't show it!!
                jsr ?advec2          ;Position for this
                jsr ?clr8
                plp                 ;Restore carry
                ifcs
                    jsr ?ones
                else
                    jsr ?zeros
                endif
            endif
            lda temp8
            clc 
            adc #$10
            sta temp8
            ifcs
                inc temp8+1
            endif
            lda vdata
            clc 
            adc #$10
            sta vdata
            ifcs
                inc vdata+1
            endif                   ;Point to next line
            lda temp6
            clc 
            adc #01
            sta temp6
            ifcs
                inc temp6+1
            endif
            ldy #00
            lda (temp6,Y)           ;End of maze
        eqend
		rts
		
;****************************************************
    .sbttl "Build H Line"
;****************************************************
?build  lda #27d                ;28 Max Across
        sta temp2
        lda (mazpt,Y)
        ifeq
            rts                 ;Skip if no line
        endif
        ifpl
            begin
                and #$0F                ;Drop any special bits
                tax
                lda mapbits,X
                jsr shiftin
                dec temp2               ;Another one down
                iny
                lda (mazpt,Y)
            eqend
            lda temp2               ;Did we do all?
            ifpl                    ;Nope
                begin
?bl4                lda #00
                    jsr shiftin         ;0 fill
                    dec temp2
                miend
            endif
            lda #00             ;Flag for + return
        endif                   ;Returns - if skipped
        ifpl
            tya
            clc
            adc mazpt               ;Update source pointer
            sta mazpt
            ifcs
                inc mazpt+1
            endif
            lda #01             ;Set draw flag
        endif
        rts
        
;******************************************
    .sbttl "Build V line" 
;******************************************
buildv  ;ldx maznum
		ldx dif4mz
		lda mzty,X 
		tax
        lda mazdep,X            ;Count of lines
        sta temp2
        ldy #00
        lda (mazpt,Y)
        begin
            and #$0F                ;Drop special bits
            tax 
            lda mapvbt,X            ;Bit pattern vert
            jsr shiftin
            ldx maztype
            lda mazpt
            clc 
            adc mazlen,X            ;Add length to next line
            sta mazpt
            ifcs
                inc mazpt+1
            endif
            lda (mazpt,Y)           ;Next stamp
            dec temp2
        eqend                   ;All lines
        lda #27d                ;Force shift left
        sec 
        sbc mazdep,X
        sta temp2
        bpl ?bl4
        rts 
        
;*****************************************
    .sbttl "Shift in to Map"
;*****************************************
;* A = Shift pattern                     
;*****************************************
shiftin ldx #01             ;Shift in 2 bits
        begin
            asl A
            rol map6
            jsr rol50
            dex
        miend
        rts 
        
;*****************************************
    .sbttl "Shift 'Bit Map'"          
;*****************************************
?shift  asl map6                ;Next shift
        jsr rol50
        lda map0
        ora map1
        ora map2
        ora map3
        ora map4
        ora map5
        ora map6
        rts 
        
;*****************************************
    .sbttl "Ones Routine" 
;*****************************************
?ones   begin               ;Entered with a known 1
            jsr add8            ;Add 8 to temp3
            jsr ?shift
            ifeq
                ifcs                ;Was 1 on last carry out
                    jsr add8
                endif
                ldy #$C0
                jmp ?advec
            endif
            ifcc                ;We changed
                ldy #$C0            ;So output this vector
                jsr ?advec
                clc             	;And do another routine
                bcc ?zeros          ;Change routines
            endif
        ccend
;*****************************************
    .sbttl "Zeros Routine"
;*****************************************
?zeros  begin
            jsr add8
            jsr ?shift           ;Next bit
            ifeq
                bcs ?zer8           ;Might have been one left
                ldy #00
                jmp ?advec
            endif
            ifcs                ;A change occured
?zer8           ldy #00
                jsr ?advec
                sec
                bcs ?ones            ;Go do ones routine
            endif
        csend               ;Always loop!!

;*****************************************
    .sbttl "Bump Maze Pointer"
;*****************************************
;* Bump indirect pointer for maze draw   
;* routines and leave Y=0 for indirect   
;* use. Also called from tw_maze
;*****************************************
incmaz  inc mazpt
        ifeq
            inc mazpt+1
        endif
        ldy #00             ;Always 0
        rts
        
;*****************************************
    .sbttl "Add vector to List"  
;*****************************************
;* Adds a vector to the vglist. Length   
;* of vector is in temp3(4). If the      
;* vector is under length of +/-32d then 
;* a short vector will be generated.     
;*****************************************
;*** (Y) = 80 to draw, 0 Not to draw   
;*****************************************
?advec  sty vgbrit
        lda temp8
        sec 
        sbc temp3               ;Running total for return
        sta temp8               ;Subtract to create return vector
        lda temp8+1
        sbc temp3+1
        sta temp8+1
        sec 
        lda temp8+2
        sbc temp3+2
        sta temp8+2
        lda temp8+3
        sbc temp3+3
        sta temp8+3         ;Total updated for return later
        lda temp3
        sta temp1
        lda temp3+1
        sta temp1+1
        ifmi                    ;Need absolute value
            jsr dblneg          ;Get positive number
        endif
        ifeq                    ;Testing temp1+1
            lda temp1
            cmp #32d                ;Is X less than 32d?
            ifcc                    ;yep!
                lda temp3+2
                sta temp1
                lda temp3+3
                sta temp1+1
                ifmi                ;Now check Y (absolute value)
                    jsr dblneg
                endif
                ifeq
                    lda temp1
                    cmp #32d
                    ifcc                ;Both X and Y less than 32d
                        lda temp3+2     ;Output vector (get del Y)
                        cmp #$80
                        ror A           ;/2
                        and #$1F
                        ora #$40            ;Add instruction
                        tax
                        lda temp3           ;delta X
                        cmp #$80
                        ror A
                        and #$1F
                        ora vgbrit      ;Add brightness
                        jsr vgadd2
                        jmp ?av7
                    endif
                endif
            endif
        endif
        ldx #temp3          ;Vector data location
?advec2 sty vgbrit
        sta watchdog        ;Kick the doggie!!
        jsr vgvctr          ;Add the vector
?av7    lda #00
        ldx #00
        bit temp7
        ifmi
            ldx #02
        endif
        sta temp3,X
        sta temp3+1,X       ;Clear for next vector
        rts
        
;**********************************************
    .sbttl "Add 8 to temp3(2)"             
;**********************************************
add8    bit temp7               ;V?
        ifpl                    ;Doing H
            lda temp3           	;Add half segment
            clc
            adc #08
            sta temp3
            ifcs
                inc temp3+1
            endif
        else                    ;Do V line
            lda temp3+2
            sec
            sbc #08
            sta temp3+2
            ifcc
                dec temp3+3
            endif
        endif
        rts
        
;***********************************************
    .sbttl "Get Map Data Pointer"
;***********************************************
?getpt2 jsr unitp          		;Points to start of data
        lda mazpt               ;Now point past top blank lines
        clc 
        adc #mazdsiz        	;Data size for maze blocks
        sta mazpt
        ifcs
            inc mazpt+1
        endif
        rts
        
;***********************************************
    .sbttl "Clear temp8(4)"             
;***********************************************    
?clr8   lda #00
        sta temp8
        sta temp8+1
        sta temp8+2
        sta temp8+3         ;Clear return vector
        rts
        
;***********************************************
    .sbttl "ROL50"
;***********************************************
rol50   rol map5
        rol map4
        rol map3
        rol map2
        rol map1
        rol map0
        rts 
