
;************************************************************
;* LIB3D by Steven L. Judd (1999) - a 6502 3D library
;************************************************************
;* http://www.ffd2.com/fridge/obj3d/lib3dv2.ref 
;************************************************************
;* Ported to the 6502 BETA by Jess M. Askey (2021)
;************************************************************      

; *-------------------------------

; CDEL     .equ $C600        ;x*cos(delta)
; CDELREM  .equ $C700        ;remainder
; SDEL     .equ $C800
; SDELREM  .equ $C900
; PROJTAB  .equ $CA00        ;Projection table
; PROJREM  .equ $CB00        ;remainder
; SIN      .equ $CF00
; COS      .equ $CF20

; $C000-$C2FF	MULTLO, pointed to by $F7-$F8 and $F9-$FA
; $C300-$C5FF	MULTHI, pointed to by $FB-$FC and $FD-$FE
; $C600-$C6FF	CDEL, table of f(x)=x*cos(delta)
; $C700-$C7FF	CDELREM  remainder
; $C800-$C8FF	SDEL	x*sin(delta)
; $C900-$C9FF	SDELREM
; $CA00-$CAFF	PROJTAB, projection table, f(x)=d/x, integer part
; $CB00-$CBFF	PROJREM, projection table, 256*remainder
; $CC00-$CCFF	LOG, logarithm table
; $CD00-$CDFF	EXP, exponential table
; $CE00-$CEFF	NEGEXP, exponential table
; $CF00-$CFFF	TRIG, table of 32*sin(2*pi*x/128) -- 160 bytes.
;
; The tables from $C600-$CFFF are fixed and must not be moved.  The
; tables ROTMATH, MULTLO, and MULTHI on the other hand are only
; accessed indirectly, and thus may be relocated.  Thus there is
; room for a color map and eight sprite definitions in any VIC bank
; except bank 2, where lib3d is located (only enough room for a
; bitmap there).


XOFFSET  .equ $53          ;Center of the screen
YOFFSET  .equ $54

;* Variables used by CALCMAT

T1       .equ $57          ;Temporary variables
T2       .equ $58
T3       .equ $59
T4       .equ $5A
T5       .equ $5B
T6       .equ $5C
T7       .equ $5D
T8       .equ $5E
T9       .equ $5F
T10      .equ $60
TEMP     .equ $61

;thetax   .equ $62          ;Angles
;thetay   .equ $63
;thetaz   .equ $64



VA11     .equ $4A          ;Viewpoint rotation matrix
VB12     .equ $4B
VC13     .equ $4C
VD21     .equ $4D
VE22     .equ $4E
VF23     .equ $4F
VG31     .equ $50
VH32     .equ $51
VI33     .equ $52

;* GLOBROT variables

COUNT    .equ $55
SIGN     .equ $56

CX       .equ $57          ;Temporary variables for centers
CXSGN    .equ $59
CY       .equ $5A
CYSGN    .equ $5C
CZ       .equ $5D
CZSGN    .equ $5F

TEMP1    = $60
TEMP2    = $61
TEMP3    = $62

C0XLO    .equ $63          ;Centers, original points
C0XHI    .equ $65
C0YLO    .equ $67
C0YHI    .equ $69
C0ZLO    .equ $6B
C0ZHI    .equ $6D
TM1      .equ $6F          ;Temporary storage for
TM2      .equ $71          ;multiplication

;* ROTPROJ variables

;* COUNT .equ $55
INDEX    .equ $56          ;Center index

AUXP     .equ $60          ;Auxiliary pointer
TEMPX    .equ $62
TEMPY    .equ $64
TEMPZ    .equ $66

ROTFLAG  .equ $68          ;Rotate or rotate+project.
                          ;Also used by ACCROTX etc.

P0X      .equ $69          ;Points to be rotprojed
P0Y      .equ $6B          ;(pointers)
P0Z      .equ $6D

PLISTZLO .equ $8B          ;Place to store rotated z-coords
PLISTZHI .equ $8D          ;used for depth-sorting, etc.

CXLO     = $A3            ;Pointers to list of (rotated)
CXHI     = $A5            ;centers.
CYLO     = $A7
CYHI     = $A9
CZLO     = $AB
CZHI     = $AD

MATMULT  = $AF            ;Pointer to matrix mult. tables

MULTLO1  .equ $B1          ;Same tables used by POLYFILL
MULTLO2  .equ $B3
MULTHI1  .equ $B5
MULTHI2  .equ $B7

PLISTXLO .equ $BD          ;Place to store rotprojed
PLISTXHI .equ $BF          ;point (same as used by POLYFILL)
PLISTYLO .equ $C1
PLISTYHI .equ $C3

;*
;* Calculate the local matrix
;*
;* Pass in: A,X,Y = angles around z,x,y axis
;*
;* Strategy: M = Ry Rx Rz where Rz=roll, Rx=pitch, Ry=yaw
;*
;* Idea: Given
;*   t1 = tx+ty
;*   t2 = tx-ty
;*   t3 = tx+tz
;*   t4 = tx-tz
;*   t5 = tx+ty+tz = t3+ty
;*   t6 = tx-ty-tz = t4-ty
;*   t7 = tx+ty-tz = t1-tz
;*   t8 = tx-tt+tz = t2+tz
;*   t9 = ty+tz
;*   t10 = ty-tz
;*
;* then the rotation elements are
;*   A11=.5(cos(t9)+c(t10))-.25(sin(t7)+s(t8)-s(t5)-s(t6))
;*   B12=.5(s(t9)-s(t10)) - .25(c(t8)-c(t7) + c(t6)-c(t5))
;*   C13=.5(sin(t2)-sin(t1))
;*   D21=.5(sin(t4)-sin(t3))
;*   E22=.5(cos(t3)+cos(t4))
;*   F23= sin(tx)
;*   G31=.5(s(t9)+s(t10)) - .25(c(t7)-c(t8) + c(t6)-c(t5))
;*   H32=.5(c(t10)-c(t9)) - .25(s(t5)+s(t6) + s(t7)+s(t8))
;*   I33=.5(cos(t1)+cos(t2))
;*

calcmat  ;STA thetaz
         ;STX thetax
         ;STY thetay		;these are already set

         CLC
         ADC thetax
         AND #$7F
         TAX              ;t3
         LDA thetax
         SEC
         SBC thetaz
         AND #$7F
         TAY              ;t4
         LDA COS,X
         CLC
         ADC COS,Y
         STA E22
         LDA SIN,Y
         SEC
         SBC SIN,X
         STA D21          ;These are the t3/t4 elements

         TXA
         CLC
         ADC thetay
         AND #$7F
         TAX              ;t5
         TYA
         SEC
         SBC thetay
         AND #$7F
         TAY              ;t6
         LDA SIN,X
         CLC
         ADC SIN,Y
         CMP #$80         ;Set C if negative, clear otw.
                          ;(pretty sneaky, eh? :)
         ROR A             ;.25(sin(t5)+...)
         STA A11          ;partial result
         LDA COS,Y
         SEC
         SBC COS,X
         CMP #$80
         ROR A
         STA B12          ;partial result

         LDA thetax
         CLC
         ADC thetay
         AND #$7F
         TAX              ;t1
         LDA thetax
         SEC
         SBC thetay
         AND #$7F
         TAY              ;t2
         LDA SIN,Y
         SEC
         SBC SIN,X
         STA C13
         LDA COS,X
         CLC
         ADC COS,Y
         STA I33

         TXA
         SEC
         SBC thetaz
         AND #$7F
         TAX              ;t7
         TYA
         CLC
         ADC thetaz
         AND #$7F
         TAY              ;t8
         LDA SIN,X
         CLC
         ADC SIN,Y
         CMP #$80
         ROR A
         STA TEMP
         CLC
         ADC A11
         STA H32          ;st5+st6+st7+st8
         LDA TEMP
         SEC
         SBC A11          ;st7+st8-st5-st6
         STA A11
         LDA COS,X
         SEC
         SBC COS,Y
         CMP #$80
         ROR A
         STA TEMP
         CLC
         ADC B12
         STA G31          ;ct7-ct8 + ct6-ct5
         LDA B12
         SEC
         SBC TEMP
         STA B12          ;ct6-ct5 + ct8-ct7

         LDA thetay
         CLC
         ADC thetaz
         AND #$7F
         TAX              ;t9
         LDA thetay
         SEC
         SBC thetaz
         AND #$7F
         TAY              ;t10
         LDA COS,X
         CLC
         ADC COS,Y
         SEC
         SBC A11
         STA A11

         LDA COS,Y
         SEC
         SBC COS,X
         SEC
         SBC H32
         STA H32

         LDA SIN,X
         SEC
         SBC SIN,Y
         CLC
         ADC B12
         STA B12

         LDA SIN,X
         CLC
         ADC SIN,Y
         CLC
         ADC G31
         STA G31

         LDA thetax       ;Finally, little F23
         AND #$7F
         TAX
         LDA SIN,X
         ASL A
         STA F23
         RTS              ;Easy-peasy!



;*
;* The next three procedures rotate the accumulation
;* matrix (multiply by Rx, Ry, or Rz).
;*
;* Carry clear means rotate by positive amount, clear
;* means rotate by negative amount.
;*
;* On entry: (.A,.Y) = (lo,hi) pointer to matrix
;*   (bytes 0-8 = integer, 9-17=remainder)
;*

MATP     = T1
REMP     = T3

COMROT   ;Common to all rotations
         ROR ROTFLAG
         STA MATP
         STY MATP+1
         CLC
         ADC #9
         STA REMP
         TYA
         ADC #00
         STA REMP+1
         RTS

ACCROTZ ;rows 1 and 2
        JSR COMROT
        LDY #00
        LDX #3
        ifeq
ACCROTX 	JSR COMROT
			LDY #3
			LDX #6
		endif
						  ;ACCROTX and ACCROTZ function
                          ;identically
        STX CPY+1
		begin
			STY COUNT
			LDA (REMP),Y     ;row 2, remainder
			STA AUXP
			LDA (MATP),Y
			TAX              ;.X = row 2, integer

			INY              ;+3
			INY
			INY
			LDA (REMP),Y     ;row 3, remainder
			STA AUXP+1
			LDA (MATP),Y     ;.Y = row 3
			TAY
			JSR ROTXY
			LDA TEMPX
			LDY COUNT
			STA (REMP),Y
			LDA TEMPX+1
			STA (MATP),Y

			INY
			INY
			INY
			LDA TEMPY
			STA (REMP),Y
			LDA TEMPY+1
			STA (MATP),Y

			DEY
			DEY
CPY     	CPY #6					;This must be in RAM
         csend
         RTS


ACCROTY ;rows 3 and 1
        JSR COMROT
        LDY #6
		begin
LOOP    	STY COUNT
			LDA (REMP),Y     ;row 3, remainder
			STA AUXP
			LDA (MATP),Y
			TAX              ;.X = row 3, integer

			TYA
			SEC
			SBC #6
			TAY
			LDA (REMP),Y     ;row 1, remainder
			STA AUXP+1
			LDA (MATP),Y     ;.Y = row 1
			TAY
			JSR ROTXY
			LDA TEMPX
			LDY COUNT
			STA (REMP),Y
			LDA TEMPX+1
			STA (MATP),Y

			TYA
			SEC
			SBC #6
			TAY
			LDA TEMPY
			STA (REMP),Y
			LDA TEMPY+1
			STA (MATP),Y

			LDY COUNT
			INY
			CPY #9
		csend
		RTS

; ;*-------------------------------

; ACCROTY 
		; ROR ROTFLAG
		; LDX #2
		; begin
			; STX COUNT
			; LDA G31REM,X     ;rows 3 and 1
			; STA AUXP
			; LDA A11REM,X
			; STA AUXP+1

			; LDY A11,X        ;.Y = row 1
			; LDA G31,X        ;.X = row 3
			; TAX
			; JSR ROTXY
			; LDX COUNT
			; LDA TEMPX
			; STA G31REM,X
			; LDA TEMPX+1
			; STA G31,X
			; LDA TEMPY
			; STA A11REM,X
			; LDA TEMPY+1
			; STA A11,X
			; DEX
		; miend
		; RTS

; ACCROTZ  
		; ROR ROTFLAG
		; LDX #2
		; begin
			; STX COUNT
			; LDA A11REM,X     ;rows 1 and 2
			; STA AUXP
			; LDA D21REM,X
			; STA AUXP+1

			; LDY D21,X        ;.Y = row 2
			; LDA A11,X        ;.X = row 1
			; TAX
			; JSR ROTXY
			; LDX COUNT
			; LDA TEMPX
			; STA A11REM,X
			; LDA TEMPX+1
			; STA A11,X
			; LDA TEMPY
			; STA D21REM,X
			; LDA TEMPY+1
			; STA D21,X
			; DEX
		; miend
		; RTS

;*-------------------------------
;*
;* Rotate .X,AUXP .Y,AUXP+1 -> TEMPX,+1 TEMPY,+1
;*
;* If flag is set for negative rotations, swap X and Y
;* and swap destinations (TEMPX TEMPY)
;*
ROTXY    
		LDA ROTFLAG
		ifmi
			STX TEMPX        ;Swap X and Y
			STY TEMPY
			LDX AUXP
			LDY AUXP+1
			STX AUXP+1
			STY AUXP
			LDX TEMPY
			LDY TEMPX
		endif
		LDA CDELREM,X
		CLC
		ADC SDELREM,Y
		STA TEMPX
		LDA CDEL,X       ;x*cos(delta)
		ADC SDEL,Y       ;+y*sin(delta)
		STA TEMPX+1
		LDA TEMPX
		CLC
		ADC AUXP
		STA TEMPX
		ifcs
			INC TEMPX+1
		endif
		LDA CDELREM,Y
		SEC
		SBC SDELREM,X
		STA TEMPY
		LDA CDEL,Y       ;y*cos - x*sin
		SBC SDEL,X
		STA TEMPY+1
		LDA TEMPY
		CLC
		ADC AUXP+1
		STA TEMPY
		ifcs
			INC TEMPY+1
		endif
		LDX ROTFLAG
        ifmi
			LDX TEMPX
			STA TEMPX
			STX TEMPY
			LDA TEMPX+1
			LDX TEMPY+1
			STA TEMPY+1
			STX TEMPX+1
		endif
		RTS

;*
;* Perform a global rotation, i.e. rotate the centers
;* (16-bit signed value) by the rotation matrix.
;*
;* The multiplication multiplies to get a 24-bit result
;* and then divides the result by 64 (mult by 4).  To
;* perform the signed multiplication:
;*   - multiply C*y as normal
;*   - if y<0 then subtract 256*C
;*   - if C<0 then subtract 2^16*y
;*
;* Parameters: .Y = number of points to rotate
;*
;* v2 -- rewritten for efficiency.  Old version computed
;*              1/64 row1*(cx,cy,cz) + 1/64 row2*(cx,cy,cz) + ...
;*       New version computes
;*              [ cx*column1 + cy*column2 + cz*column3 ] / 64
;* Actually, above seems complicated, and doesn't offer
;* a huge advantage, so has been skipped for now 3/5/99.
;*

; MULTAY   MAC              ;Multiply A*Y, store in var1, A
         ; STA MULTLO1      ;A <> 0
         ; STA MULTHI1
         ; EOR #$FF
         ; CLC
         ; ADC #01
         ; STA MULTLO2
         ; STA MULTHI2
         ; LDA (MULTLO1),Y
         ; SEC
         ; SBC (MULTLO2),Y
         ; STA ]1
         ; LDA (MULTHI1),Y
         ; SBC (MULTHI2),Y
; DONE     <<<

; QMULTAY  MAC              ;Assumes pointers already set up
         ; LDA (MULTLO1),Y
         ; SEC
         ; SBC (MULTLO2),Y
         ; STA ]1
         ; LDA (MULTHI1),Y
         ; SBC (MULTHI2),Y
         ; <<<

;* Fix sign and divide by 64

; DIVFIX  ;If Mij<0 subtract 256*C
        ; LDY MULTLO1      ;If C<0 subtract 2^16*Mij
		; ifmi
			; STA TEMP3
			; LDA TEMP2
			; SEC
			; SBC ]1           ;Center
			; STA TEMP2
			; LDA TEMP3
			; SBC ]1+1         ;high byte
		; endif
		; LDY ]1+1
        ; ifmi
			; SEC
			; SBC MULTLO1      ;Subtract Mij
		; endif
		; ASL TEMP1        ;Divide by 64
        ; ROL TEMP2
        ; ROL
        ; ASL TEMP1
        ; ROL TEMP2
        ; ROL
        ; LDY TEMP2
         ; <<<              ;.A, .Y = final result


;* Main routine

GLOBROT begin
			DEY
			STY COUNT

			LDA (C0XLO),Y
			STA CX
			LDA (C0XHI),Y
			STA CX+1

			LDA (C0YLO),Y
			STA CY
			LDA (C0YHI),Y
			STA CY+1

			LDA (C0ZLO),Y
			STA CZ
			LDA (C0ZHI),Y
			STA CZ+1

			LDA VA11         ;Row1
			LDX VB12
			LDY VC13
			JSR MULTROW      ;Returns result in .X .A = lo hi
			LDY COUNT
			STA (CXHI),Y
			TXA
			STA (CXLO),Y

			LDA VD21         ;Row2
			LDX VE22
			LDY VF23
			JSR MULTROW
			LDY COUNT
			STA (CYHI),Y
			TXA
			STA (CYLO),Y

			LDA VG31         ;Row3
			LDX VH32
			LDY VI33
			JSR MULTROW
			LDY COUNT
			STA (CZHI),Y
			TXA
			STA (CZLO),Y
			TYA
        eqend
		RTS


;*
;* Multiply a row by CX CY CZ
;* (A Procedure to save at least a LITTLE memory...)
;*
M1       .equ CXSGN        ;CXSGN etc. no longer used.
M2       .equ CYSGN
M3       .equ CZSGN

MULTROW  
		STA M1
		STX M2
		STY M3
		TAY
		BEQ SKIP1
		LDY CX+1
		;>>> MULTAY,TEMP2
		;Multiply A*Y, store in var1, A
		STA MULTLO1      ;A <> 0
		STA MULTHI1
		EOR #$FF
		CLC
		ADC #01
		STA MULTLO2
		STA MULTHI2
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TEMP2
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<
		STA TEMP3
		LDY CX
		;>>> QMULTAY,TEMP1
		;QMULTAY  MAC              ;Assumes pointers already set up
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TEMP1
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<


		CLC
		ADC TEMP2
		STA TEMP2
		LDA TEMP3
		ADC #00
		;>>> DIVFIX,CX    ;Adjust result and /64
		;If Mij<0 subtract 256*C
        LDY MULTLO1      ;If C<0 subtract 2^16*Mij
		ifmi
			STA TEMP3
			LDA TEMP2
			SEC
			SBC CX           ;Center
			STA TEMP2
			LDA TEMP3
			SBC CX+1         ;high byte
		endif
		LDY CX+1
        ifmi
			SEC
			SBC MULTLO1      ;Subtract Mij
		endif
		ASL TEMP1        ;Divide by 64
        ROL TEMP2
        ROL A
        ASL TEMP1
        ROL TEMP2
        ROL A
        LDY TEMP2
        ;<<<              ;.A, .Y = final result
SKIP1   STY TM1
		STA TM1+1

		LDA M2
		BEQ SKIP2
		LDY CY+1         ;and multiply by CY
		;>>> MULTAY,TEMP2
		;Multiply A*Y, store in var1, A
		STA MULTLO1      ;A <> 0
		STA MULTHI1
		EOR #$FF
		CLC
		ADC #01
		STA MULTLO2
		STA MULTHI2
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TEMP2
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<
		STA TEMP3
		LDY CY
		;>>> QMULTAY,TEMP1
		;QMULTAY  MAC              ;Assumes pointers already set up
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TEMP1
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<

		CLC
		ADC TEMP2
		STA TEMP2
		LDA TEMP3
		ADC #00
		;>>> DIVFIX,CY
		;If Mij<0 subtract 256*C
        LDY MULTLO1      ;If C<0 subtract 2^16*Mij
		ifmi
			STA TEMP3
			LDA TEMP2
			SEC
			SBC CY           ;Center
			STA TEMP2
			LDA TEMP3
			SBC CY+1         ;high byte
		endif
		LDY CY+1
        ifmi
			SEC
			SBC MULTLO1      ;Subtract Mij
		endif
		ASL TEMP1        ;Divide by 64
        ROL TEMP2
        ROL A
        ASL TEMP1
        ROL TEMP2
        ROL A
        LDY TEMP2
        ;<<<              ;.A, .Y = final result
		TAX
		TYA              ;low byte
		CLC
		ADC TM1
		STA TM1
		TXA              ;high byte
		ADC TM1+1
		STA TM1+1

SKIP2   LDA M3
		BEQ ZERO
		LDY CZ+1
		;>>> MULTAY,TEMP2
		;Multiply A*Y, store in var1, A
		STA MULTLO1      ;A <> 0
		STA MULTHI1
		EOR #$FF
		CLC
		ADC #01
		STA MULTLO2
		STA MULTHI2
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TEMP2
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<
		STA TEMP3
		LDY CZ
		;>>> QMULTAY,TEMP1
		;QMULTAY  MAC              ;Assumes pointers already set up
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TEMP1
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<

		CLC
		ADC TEMP2
		STA TEMP2
		LDA TEMP3
		ADC #00
		;>>> DIVFIX,CZ
		;If Mij<0 subtract 256*C
        LDY MULTLO1      ;If C<0 subtract 2^16*Mij
		ifmi
			STA TEMP3
			LDA TEMP2
			SEC
			SBC CZ           ;Center
			STA TEMP2
			LDA TEMP3
			SBC CZ+1         ;high byte
		endif
		LDY CZ+1
        ifmi
			SEC
			SBC MULTLO1      ;Subtract Mij
		endif
		ASL TEMP1        ;Divide by 64
        ROL TEMP2
        ROL A
        ASL TEMP1
        ROL TEMP2
        ROL A
        LDY TEMP2
        ;<<<              ;.A, .Y = final result
		STA TEMP3
		TYA
		CLC
		ADC TM1
		TAX
		LDA TEMP3
		ADC TM1+1
		RTS
		
ZERO    LDX TM1
        LDA TM1+1
UPRTS   RTS              ;.X .A = lo hi bytes of row mult



;*
;* ROTPROJ -- Perform local rotation and project points.
;*
;* Setup needs:
;*   Rotation matrix
;*   Pointer to math table (MATMULT = $AF-$B0)
;*   Pointers to point list (P0X-P0Z = $69-$6E)
;*   Pointers to final destinations (PLISTYLO ... = $BD...)
;*     (Same as used by POLYFILL)
;*   Pointers to lists of centers (CXLO CXHI... = $A3-$AE)
;*   .Y = Number of points to rotate (0..N-1)
;*   .X = Object center index (index to center of object)
;*
;* New addition:
;*   C set means rotate and project, but C clear means just
;*   rotate.  If C=0 then need pointers to rotation
;*   destinations ROTPX,ROTPY,ROTPZ=$59-$5E.
;*
;* v2: Now two rotation matrices, local and viewpoint.
;*     Points are rotated by local matrix; if projecting,
;*     points are now rotated by viewpoint matrix, then
;*     projected.
;*
;*     Rotated z-coordinates are now stored in PLISTZLO/HI
;*     ($6F/$71)
;*
;*     If C=0, then coords just stored in PLISTXLO etc.,
;*     as this now seems to be a pretty useless function.
;*     (low bytes only!).
;*


; SMULT   MAC              ;Signed multiplication
		; STA MATMULT
		; EOR #$FF
		; CLC
		; ADC #01
		; STA AUXP
		; LDA (MATMULT),Y
		; SEC
		; SBC (AUXP),Y
		; <<<

; QMULT   MAC              ;Multiplication that assumes
		; LDA (MATMULT),Y  ;pointers are already initialized
		; SEC
		; SBC (AUXP),Y
		; <<<

; ADD     MAC              ;Simple addition
		; CLC
		; ADC ]1
		; STA ]1
		; <<<
 
ROTPROJ  
		LDA MATMULT+1
		STA AUXP+1
		STX INDEX
		STY COUNT
		ROR ROTFLAG
		ifmi
			LDY INDEX
			LDA (CXLO),Y     ;Centers
			STA CX
			LDA (CXHI),Y
			STA CX+1
			LDA (CYLO),Y
			STA CY
			LDA (CYHI),Y
			STA CY+1
			LDA (CZLO),Y
			STA CZ
			LDA (CZHI),Y
			STA CZ+1
		endif
ROTLOOP LDY COUNT
        BEQ UPRTS
        DEY
        STY COUNT

        LDX #00          ;Use local matrix
        LDA (P0Z),Y      ;This way, can re-use routine
        PHA
        LDA (P0Y),Y
        PHA
        LDA (P0X),Y
ROTLOOP2 
		ifeq
			STA TEMPX
			STA TEMPY
			STA TEMPZ
			BEQ C2
		endif
		LDY A11,X        ;Column 1
        ;>>> SMULT
		;Signed multiplication
		STA MATMULT
		EOR #$FF
		CLC
		ADC #01
		STA AUXP
		LDA (MATMULT),Y
		SEC
		SBC (AUXP),Y
		;<<<
        STA TEMPX
        LDY D21,X
        ;>>> QMULT
		;Multiplication that assumes
		LDA (MATMULT),Y  ;pointers are already initialized
		SEC
		SBC (AUXP),Y
		;<<<
        STA TEMPY
        LDY G31,X
        ;>>> QMULT
		;Multiplication that assumes
		LDA (MATMULT),Y  ;pointers are already initialized
		SEC
		SBC (AUXP),Y
		;<<<
        STA TEMPZ

                          ;Column 2
C2      PLA              ;Py
        ifne
			LDY B12,X
			;>>> SMULT
			;Signed multiplication
			STA MATMULT
			EOR #$FF
			CLC
			ADC #01
			STA AUXP
			LDA (MATMULT),Y
			SEC
			SBC (AUXP),Y
			;<<<
			;>>> ADD,TEMPX
			;Simple addition
			CLC
			ADC TEMPX
			STA TEMPX
			;<<<
			LDY E22,X
			;>>> QMULT
			;Multiplication that assumes
			LDA (MATMULT),Y  ;pointers are already initialized
			SEC
			SBC (AUXP),Y
			;<<<
			;>>> ADD,TEMPY
			;Simple addition
			CLC
			ADC TEMPY
			STA TEMPY
			;<<<
			LDY H32,X
			;>>> QMULT
			;Multiplication that assumes
			LDA (MATMULT),Y  ;pointers are already initialized
			SEC
			SBC (AUXP),Y
			;<<<
			;>>> ADD,TEMPZ
			;Simple addition
			CLC
			ADC TEMPZ
			STA TEMPZ
			;<<<
		endif
                          ;Column 3
		PLA              ;Pz
        BEQ ADDC
        LDY C13,X
        ;>>> SMULT
		;Signed multiplication
		STA MATMULT
		EOR #$FF
		CLC
		ADC #01
		STA AUXP
		LDA (MATMULT),Y
		SEC
		SBC (AUXP),Y
		;<<<
        ;>>> ADD,TEMPX
		;Simple addition
		CLC
		ADC TEMPX
		STA TEMPX
		;<<<
        LDY F23,X
        ;>>> QMULT
		;Multiplication that assumes
		LDA (MATMULT),Y  ;pointers are already initialized
		SEC
		SBC (AUXP),Y
		;<<<
        ;>>> ADD,TEMPY
		;Simple addition
		CLC
		ADC TEMPY
		STA TEMPY
		;<<<
        LDY I33,X
        ;>>> QMULT
		;Multiplication that assumes
		LDA (MATMULT),Y  ;pointers are already initialized
		SEC
		SBC (AUXP),Y
		;<<<
ADDC    CLC
        ADC TEMPZ

		LDY ROTFLAG
		ifpl
			LDY COUNT        ;If just rotating, then just
			STA (PLISTZLO),Y ;store!
			LDA TEMPY
			STA (PLISTYLO),Y
			LDA TEMPX
			STA (PLISTXLO),Y
			JMP ROTLOOP
		endif
		;ROT2    
		CPX #9
		ifne
			LDX #9           ;Use viewpoint matrix
			PHA
			LDA TEMPY
			PHA
			LDA TEMPX
			JMP ROTLOOP2
		endif
		;PROJ    
		LDY #00          ;.Y = sign
		TAX
		ifmi
			DEY
		endif
		CLC              ;Add in centers
		ADC CZ
		STA TEMPZ
		TYA
		ADC CZ+1
		STA TEMPZ+1      ;Assume this is positive!

		LDX #00
		LDY #00
		LDA TEMPY
		ifmi
			DEY
		endif
		CLC
		ADC CY
		STA TEMPY
		TYA
		ADC CY+1
		STA TEMPY+1
		ifmi
			DEX
		endif
		STX CYSGN        ;Need sign for later

		LDX #00
		LDY #00
		LDA TEMPX
		ifmi
			DEY
		endif
		CLC
		ADC CX
		STA TEMPX
		TYA
		ADC CX+1
		STA TEMPX+1
		ifmi
			DEX
		endif
		STX CXSGN

		LDY COUNT
		LDA TEMPZ
		STA (PLISTZLO),Y
		LDA TEMPZ+1
		STA (PLISTZHI),Y
		ifne
			begin
				LSR A            ;Shift everything until
				ROR TEMPZ        ;Z=8-bits
				LSR CXSGN
				ROR TEMPX+1      ;Projection thus loses accuracy
				ROR TEMPX        ;for far-away objects.
				LSR CYSGN
				ROR TEMPY+1      ;(Big whoop)
				ROR TEMPY
				TAX
			eqend
		endif
		LDX TEMPZ
		LDA PROJTAB,X    ;Projection constant
		BNE C1          ;Lots of 0's and 1's, so special
		LDY XOFFSET      ;case them.
		STY TM1
		LDY YOFFSET
		STY TM2
		LDY TEMPY+1
		STA TM1+1
		STA TM2+1
		JMP NOINT
		begin
			LDY TEMPX
			STY TM1
			LDY TEMPX+1
			STY TM1+1
			LDY TEMPY
			STY TM2
			LDY TEMPY+1
			STY TM2+1
			JMP ADDOFF
C1      	CMP #1
		neend
		LDY TEMPX
		;>>> MULTAY,TM1   ;al*N
		;Multiply A*Y, store in var1, A
		STA MULTLO1      ;A <> 0
		STA MULTHI1
		EOR #$FF
		CLC
		ADC #01
		STA MULTLO2
		STA MULTHI2
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TM1
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<
		LDY TEMPX+1
		CLC
		ADC (MULTLO1),Y  ;+256*ah*N (middle byte)
		SEC
		SBC (MULTLO2),Y
		STA TM1+1

		LDY TEMPY
		;>>> QMULTAY,TM2
		;QMULTAY  MAC              ;Assumes pointers already set up
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TM2
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		;<<<

		LDY TEMPY+1
		CLC
		ADC (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		STA TM2+1

ADDOFF  LDA XOFFSET      ;Screen offsets
		CLC
		ADC TM1
		STA TM1
		ifcs
			INC TM1+1
			CLC
		endif
		LDA YOFFSET
		ADC TM2
		STA TM2
		ifcs
			INC TM2+1
		endif
NOINT	LDA PROJREM,X
		ifeq
			;None remaining 
			LDY COUNT
			LDA TM1
			STA (PLISTXLO),Y
			LDA TM1+1
			STA (PLISTXHI),Y
			LDA TM2
			STA (PLISTYLO),Y
			LDA TM2+1
			STA (PLISTYHI),Y
			JMP ROTLOOP
		endif
		
		STA MULTLO1
		STA MULTHI1
		EOR #$FF
		CLC
		ADC #01
		STA MULTLO2
		STA MULTHI2

		LDA (MULTLO1),Y  ;ah*r
		SEC
		SBC (MULTLO2),Y
		TAX
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		CPY #$80
		ifcs
			SBC MULTLO1      ;Subtract 256*r if neg
						  ;(clears carry)
		endif
		TAY
		TXA
		ADC TM2
		STA TM2
		TYA
		ADC TM2+1
		STA TM2+1

		LDY TEMPY
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		LDA (MULTHI1),Y  ;al*r, hi byte
		SBC (MULTHI2),Y
		CLC
		ADC TM2          ;Add remainder
		LDY COUNT
		STA (PLISTYLO),Y
		LDA TM2+1        ;and sign+carry
		ADC #00
		STA (PLISTYHI),Y
		;* Do the same for x
		LDY TEMPX+1
		LDA (MULTLO1),Y  ;ah*r
		SEC
		SBC (MULTLO2),Y
		TAX
		LDA (MULTHI1),Y
		SBC (MULTHI2),Y
		CPY #$80
		ifcs
			SBC MULTLO1      ;Subtract 65536*r if neg (clears carry)
		endif
		TAY
		TXA
		ADC TM1
		STA TM1
		TYA
		ADC TM1+1
		STA TM1+1

		LDY TEMPX
		LDA (MULTLO1),Y
		SEC
		SBC (MULTLO2),Y
		LDA (MULTHI1),Y  ;al*r, hi byte
		SBC (MULTHI2),Y
		CLC
		ADC TM1          ;Add remainder
		LDY COUNT        ;and store
		STA (PLISTXLO),Y
		LDA TM1+1
		ADC #00
		STA (PLISTXHI),Y

		JMP ROTLOOP      ;And on to the next point!



