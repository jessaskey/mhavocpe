
;*************************************************
    .sbttl "Space Fort Wave"
;*************************************************
enem3   lda #05
        sta hitpts          ;500 Per Object
        lda mzgame
        ifne
            rts
        endif
        lda saucvl          ;Add or subtract some amount
        ldy saucvd          ;Velocity Direction
        ifpl                    ;Add
            clc 
            adc #01
            cmp #$28                ;Max Speed
            ifcs
                ldy #$80
                sty saucvd          ;Start Slowing
            endif
        else                    ;Else Start Slowing
            sec
            sbc #02
            ifmi                    ;Don't let pass 0
                lda #00
            endif
        endif
        sta saucvl
        ifeq                    ;If 0, get a new direction
            sta saucvd          ;And start up again
            ldx stbix           ;Clear base flag index
            bit rands+2         ;Change
            ifmi                    ;No change bit clear, change ok
                dex                 ;Guess turn left
                ifvc
                    inx
                    inx                 ;Turn Right (2 to compensate for dex)
                endif
            endif
            txa
            and #07             ;Don't let wrap
        else
            lda stbix               ;Else continue same (maybe)
        endif
        ;*----------------- Limit Check ----------------
        ;* Y Check First 
        ;*----------------------------------------------
        ldx stbix               ;Which Direction now?
        ldy statyh          ;Check Y motion
        cpy #02             ;If above, get moving down
        ifcc                    ;It's too high
            ldy ?topok,X        ;Is it going an ok direction??
            ifpl    
                jsr ?stoptime
			;this else was a bad bug I think
            ;else
                ifeq                    ;Change when stopped
                    lda ?topchk,X           ;Get new direction
                    ifmi                    ;It's straight up, get random direction
                        lda #01             ;Either 1 or 7
                        bit rands
                        ifmi
                            lda #07
                        endif
                    endif
                endif
            endif
        endif
        ldy bonusa          ;Bonus 0??
        ifne                    ;If yes, get off screen
            ldy statyh          ;Must recall position
            cpy #06
            ifcs                        ;Too High
                ldy ?botok,X            ;Going an ok direction
                ifpl                        ;no
                    jsr ?stoptime               ;Stop it fast
                    ifeq                        ;if stopped, get new direction
                        lda ?botchk,X           	;Bottom check
                        ifmi
                            lda #03
                            bit rands+1
                            ifmi
                                lda #05                 ;One Random Check
                            endif
                        endif
                    endif
                endif
            endif
        endif
        ;*-------------------------------------------------*
        ;*------------ Check X now ------------------------*
        ;*-------------------------------------------------*
        ldy statxh              ;X MSB
        cpy #01                 ;To far left?
        ifcc
            ldy ?ltok,X             ;Going an ok direction?
            ifpl                        ;no
                jsr ?stoptime               ;Stop it!
                lsr saucvl
                ifeq                            ;Stopped?
                    lda ?sfltchk,X                  ;Get new left direction
                    ifmi                            ;Straight in, pick new direction
                        lda #05                     ;Guess RU
                        bit rands
                        ifmi
                            lda #07                     ;Use RD
                        endif
                    endif
                endif
            endif
        endif
        ldy statxh              ;Reload position
        cpy #rtedge-1               ;To far right?
        ifcs                        ;yes
            ldy ?rtok,X                 ;Going an ok direction?
            ifpl
                jsr ?stoptime
                lsr saucvl
                ifeq
                    lda ?sfrtchk,X
                    ifmi
                        lda #03                 ;Guess LU
                        bit rands+1
                        ifmi
                            lda #01                 ;Use LD
                        endif
                    endif
                endif
            endif
        endif
        sta stbix
        tax 
        lda ?nextmot,X
        ldy bonusa              ;Bonus 0?
        ifeq
            lda #$80            	;Always set down
        endif
        sta stbflg              ;Stop Space Ship
		;Ship drops here
		lda dropcnt             ;Dropping from last time?
        ifeq
            lda frame
            and #$0F                    ;Time Interval
            ifeq                        ;Time for new count
                lda ?shtnum,X               ;How many shots from here
                sta dropcnt             	;New count
            endif
        else
            ldy #00
            sty sobjs2              ;Turn off aux stat on 'Fighter' shooting
            jsr ?gunloc             ;Place 'gun' at station
            lda #zshot+nmshot-3     ;1 shot at first
            ldx difcty              ;Second pass (or better)
            ifne
                lda #zshot+nmshot-2         ;Shoot 2
            endif
            ldx #00
            stx temp5                   ;Flag = from station
            jsr drop3                   ;Fire shot
            ifpl                        ;Started one
                lda #$11
                sta objst,Y             ;Set to line from here
                dec dropcnt             ;Got one
            endif
        endif
        lda nenstr
        beq ?docoreg
        lda frame
        tay 
        ifeq
            lda #$80
            sta lauen                   ;Force a start
        endif
        tya 
        and #$1F
        ifeq
            jsr emem12
        else
?docoreg    jsr coreg
        endif
        rts 
        
?stoptime    
        ldy #$80
        sty saucvd              ;Stop it fast
        ldy saucvl
        rts 
        
;*************************************************
    .sbttl "Gun Location"
;*************************************************
;* Determines the X and Y location of the gun    *
;* based on the current rotation of the base and *
;* which face the gun is on.                     *
;*                                               *
;* Inputs:  temp8,saupic,statx,staty             *
;*          temp8 = face select                  *
;*          a = face (if entry at gunlc2)        *
;*                                               *
;* Output:  vdata(2) = x posit           *
;*          vdata+2(2) = same for y              *
;*                                               *
;* Uses:    A,X,temp8                            *
;*                                               *
;* Note: Results are also stored in the locs     *
;*       of the first 3 enemy space ships. The   *
;*       first one of these will be used later   *
;*       so the routines for dropping a shot may *
;*       be used in twenemy. These locs are not  *
;*       used during this space wave so no harm  *
;*       will occur.                             *
;*************************************************
?gunloc lda statxl
        sta vdata               ;For output of picture
        sta sobjxl,Y
        lda statxh
        sta vdata+1
        sta sobjxh,Y
        lda statyl
        sta vdata+2
        sta sobjyl,Y
        lda statyh
        sta vdata+3         ;vdata+2 = gun's position y
        sta sobjyh,Y            ;for shot routine later
        rts 


;********************************************
    .sbttl "Motion Limits Table"
;********************************************
?nextmot	.byte $80,$A0,$20,$E0,$C0,$F0,$30,$B0

?topchk  	.byte $00,$01,$02,$01,$80,$07,$06,$07   ;Top check alt correction table

?botchk  	.byte $80,$03,$02,$03,$04,$05,$06,$05   ;Bottom check alt correction table

?sfltchk    .byte $00,$07,$80,$05,$04,$05,$06,$07   ;Left edge check alt correction table

?sfrtchk    .byte $00,$01,$02,$03,$04,$03,$80,$01   ;Right edge table

;Direction Okay truth table
?topok      .byte $80,$80,$80,$00,$00,$00,$80,$80   ;80 = direction ok
?botok      .byte $00,$00,$80,$80,$80,$80,$80,$00
?ltok       .byte $80,$00,$00,$00,$80,$80,$80,$80
?rtok       .byte $80,$80,$80,$80,$80,$00,$00,$00

;These are D,DL,L,UL,U,UR,R,DR
?shtnum     .byte $01,$03,$03,$02,$01,$02,$01,$03        



