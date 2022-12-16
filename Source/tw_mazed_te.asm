;********************************************************************
;* MAZECOLLECTION: mhpe 13720 08032022 tournament edition.mhc
;* Generated On 8/3/2022 9:47:03 PM
;********************************************************************
;  Dif 0 - Maze A - Level 1 - Level 1
;********************************************************************
mzdc00  .db $63,$95,$92,$80,$B6,$74                         ;Oxygen
        .db $00                                             

mzlg00  .db $FF                                             
        .db $B6                                             ;VLightningV1
        .db $00                                             

mzar00  .db $94,$02                                         ;Arrow2
        .db $87,$02                                         ;Arrow4
        .db $72,$02                                         ;Arrow10
        .db $00                                             

mzor00  .db $00                                             

mztr00  .db $00                                             

trpa00  .db $00,$00,$00                                     

mzta00  .db $3B,$02                                         ;MazeWall1
        .db $3C,$05                                         ;MazeWall2
        .db $3D,$03                                         ;MazeWall3
        .db $4A,$07                                         ;MazeWall4
        .db $4B,$04                                         ;MazeWall5
        .db $00                                             

mztd00  .db $00                                             

mone00  .db $B5                                             ;OneWay1(right)
        .db $00                                             

tite00  .db $00                                             

lock00  .db $00                                             

tran00  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand00  .db $00                                             

mzty00  = $00
clock00 = $00
boot00  = $00
keyp00  = $00
outi00  = $30
reaz00  = $00
oxyb00  = $05
;********************************************************************
;  Dif 0 - Maze B - Level 2 - Level 2
;********************************************************************
mzdc01  .db $52,$60,$75,$92,$71,$6C,$7B,$88,$97,$B6         ;Oxygen
        .db $00                                             

mzlg01  .db $42                                             ;HLightningH1
        .db $9F                                             ;HLightningH2
        .db $7C                                             ;HLightningH3
        .db $5C                                             ;HLightningH1
        .db $54                                             ;HLightningH2
        .db $A6                                             ;HLightningH1
        .db $FF                                             
        .db $5B                                             ;VLightningV3
        .db $54                                             ;VLightningV4
        .db $A3                                             ;VLightningV1
        .db $00                                             

mzar01  .db $B5,$03                                         ;Arrow6
        .db $99,$03                                         ;Arrow7
        .db $7D,$03                                         ;Arrow11
        .db $00                                             

mzor01  .db $00                                             

mztr01  .db $00                                             

trpa01  .db $00,$00,$00                                     

mzta01  .db $32,$07                                         ;MazeWall6
        .db $33,$05                                         ;MazeWall7
        .db $35,$02                                         ;MazeWall8
        .db $47,$07                                         ;MazeWall9
        .db $48,$03                                         ;MazeWall10
        .db $49,$07                                         ;MazeWall11
        .db $4A,$04                                         ;MazeWall12
        .db $AD,$04                                         ;MazeWall1
        .db $B4,$03                                         ;MazeWall2
        .db $AC,$03                                         ;MazeWall3
        .db $B5,$04                                         ;MazeWall6
        .db $AE,$01                                         ;MazeWall7
        .db $B0,$01                                         ;MazeWall8
        .db $AF,$01                                         ;MazeWall9
        .db $B2,$01                                         ;MazeWall10
        .db $B1,$01                                         ;MazeWall11
        .db $B3,$01                                         ;MazeWall12
        .db $00                                             

mztd01  .db $00                                             

mone01  .db $93                                             ;OneWay1(right)
        .db $78                                             ;OneWay1(right)
        .db $5D                                             ;OneWay2(right)
        .db $00                                             

mcp01a  .dw $0600,$F680                                     ;Position
        cann_pos(canp_tr,0,1,24)                            ;GunPos - TopRight Speed: 0 Shot: Yes ShotVel: $18
        cann_pos(canp_mr,0,1,24)                            ;GunPos - MidRight Speed: 0 Shot: Yes ShotVel: $18
        cann_pos(canp_br,0,1,24)                            ;GunPos - BotRight Speed: 0 Shot: Yes ShotVel: $18
        cann_loc(20,16,0)                                   ;GunLoc - Frames: 20 XVel: 16 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_tl,0,1,24)                            ;GunPos - TopLeft Speed: 0 Shot: Yes ShotVel: $18
        cann_pos(canp_ml,0,1,24)                            ;GunPos - MidLeft Speed: 0 Shot: Yes ShotVel: $18
        cann_pos(canp_bl,0,1,24)                            ;GunPos - BotLeft Speed: 0 Shot: Yes ShotVel: $18
        cann_loc(20,-16,0)                                  ;GunLoc - Frames: 20 XVel: -16 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_tr,0,0,0)                             ;GunPos - TopRight Speed: 0 Shot: No
        cann_pos(canp_tl,0,0,0)                             ;GunPos - TopLeft Speed: 0 Shot: No
        cann_loc(4,-16,0)                                   ;GunLoc - Frames: 4 XVel: -16 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_dn,0,1,8)                             ;GunPos - Down Speed: 0 Shot: Yes ShotVel: $08
        cann_loc(8,0,-4)                                    ;GunLoc - Frames: 8 XVel: 0 YVel: -4
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_ml,0,1,8)                             ;GunPos - MidLeft Speed: 0 Shot: Yes ShotVel: $08
        cann_pos(canp_tr,0,0,0)                             ;GunPos - TopRight Speed: 0 Shot: No
        cann_loc(8,0,4)                                     ;GunLoc - Frames: 8 XVel: 0 YVel: 4
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_mr,0,0,0)                             ;GunPos - MidRight Speed: 0 Shot: No
        cann_loc(4,16,0)                                    ;GunLoc - Frames: 4 XVel: 16 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_end                                            ;Loop It

tite01  .db $00                                             

lock01  .db $00                                             

tran01  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand01  .db $00                                             

mzty01  = $01
clock01 = $00
boot01  = $00
keyp01  = $00
mpod01  = $00
outi01  = $30
reaz01  = $00
oxyb01  = $06
;********************************************************************
;  Dif 0 - Maze C - Level 3 - Level 3
;********************************************************************
mzdc02  .db $63,$93,$82,$87,$6A,$78,$56,$67,$7E,$8D,$9C,$71 ;Oxygen
        .db $00                                             

mzlg02  .db $4C                                             ;HLightningH4
        .db $46                                             ;HLightningH5
        .db $FF                                             
        .db $8B                                             ;VLightningV1
        .db $55                                             ;VLightningV2
        .db $00                                             

mzar02  .db $A8,$07                                         ;Arrow21
        .db $9D,$06                                         ;Arrow22
        .db $86,$06                                         ;Arrow8
        .db $70,$07                                         ;Arrow6
        .db $00                                             

mzor02  .db $00                                             

mztr02  .db $00                                             

trpa02  .db $00,$00,$00                                     

mzta02  .db $2E,$01                                         ;MazeWall13
        .db $43,$01                                         ;MazeWall14
        .db $44,$07                                         ;MazeWall15
        .db $45,$05                                         ;MazeWall16
        .db $57,$01                                         ;MazeWall17
        .db $58,$01                                         ;MazeWall18
        .db $59,$01                                         ;MazeWall19
        .db $5A,$03                                         ;MazeWall20
        .db $5B,$07                                         ;MazeWall21
        .db $5C,$02                                         ;MazeWall22
        .db $6B,$05                                         ;MazeWall23
        .db $6C,$01                                         ;MazeWall24
        .db $6D,$01                                         ;MazeWall25
        .db $6F,$01                                         ;MazeWall26
        .db $70,$02                                         ;MazeWall27
        .db $71,$04                                         ;MazeWall28
        .db $00                                             

mztd02  .db $00                                             

mone02  .db $A5                                             ;OneWay3(right)
        .db $00                                             

tite02  .db $00                                             

lock02  .db $0B,$7E,$7C                                     ;Red
        .db $02,$55,$79                                     ;Green
        .db $00                                             

tran02  .db (colblue+tr_left),$97                           ;Blue
        .db (colblue+tr_right),$A3                          ;Blue
        .db $00                                             
        .db $00,$00,$00,$00,$00,$40                         ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand02  .db $00                                             

mzty02  = $02
clock02 = $00
boot02  = $00
keyp02  = $00
outi02  = $25
reaz02  = $00
oxyb02  = $10
;********************************************************************
;  Dif 1 - Maze D - Level 4 - Level 4
;********************************************************************
mzdc03  .db $4C,$A8,$77,$65,$48,$24,$25,$26,$47,$27,$28,$40 ;Oxygen
        .db $00                                             

mzlg03  .db $8A                                             ;HLightningH6
        .db $8B                                             ;HLightningH7
        .db $52                                             ;HLightningH8
        .db $FF                                             
        .db $23                                             ;VLightningV3
        .db $6A                                             ;VLightningV2
        .db $00                                             

mzar03  .db $34,$00                                         ;Arrow31
        .db $96,$01                                         ;Arrow2
        .db $82,$00                                         ;Arrow4
        .db $58,$01                                         ;Arrow5
        .db $00                                             

mzor03  .db $00                                             

mztr03  .db $00                                             

trpa03  .db $00,$00,$00                                     

mzta03  .db $00                                             

mztd03  .db $00                                             

mone03  .db $74                                             ;OneWay3(right)
        .db $FF                                             
        .db $44                                             ;OneWay1(left)
        .db $00                                             

tite03  .db $00                                             

lock03  .db $02,$9C,$49                                     ;Green
        .db $01,$9A,$29                                     ;Blue
        .db $06,$47,$4A                                     ;Yellow
        .db $0B,$74,$B8                                     ;Red
        .db $00                                             

tran03  .db (colyellow+tr_left),$69                         ;Yellow
        .db (colyellow+tr_right),$B7                        ;Yellow
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand03  .db $00                                             

mzty03  = $03
clock03 = $9D
boot03  = $00
keyp03  = $00
outi03  = $30
reaz03  = $00
oxyb03  = $10
;********************************************************************
;  Dif 1 - Maze A - Level 5 - Level 5
;********************************************************************
mzdc10  .db $A3,$94,$83,$64,$77,$B5,$80,$B3                 ;Oxygen
        .db $00                                             

mzlg10  .db $FF                                             
        .db $A3                                             ;VLightningV6
        .db $77                                             ;VLightningV7
        .db $00                                             

mzar10  .db $63,$04                                         ;Arrow33
        .db $71,$04                                         ;Arrow34
        .db $B6,$05                                         ;Arrow1
        .db $A8,$05                                         ;Arrow2
        .db $00                                             

mzor10  .db $00                                             

mztr10  .db $64                                             ;TripPad1
        .db $83                                             ;TripPad2
        .db $86                                             ;TripPad3
        .db $87                                             ;TripPad4
        .db $94                                             ;TripPad5
        .db $B5                                             ;TripPad6
        .db $78                                             ;TripPad7
        .db $00                                             

trpa10  .db $88,$F6,$84                                     ;TripPad1(pyroid)
        .db $83,$F8,$04                                     ;TripPad2(pyroid)
        .db $09,$F8,$84                                     ;TripPad3(pyroid)
        .db $09,$F8,$84                                     ;TripPad4(pyroid)
        .db $87,$F9,$84                                     ;TripPad5(pyroid)
        .db $89,$FB,$84                                     ;TripPad6(pyroid)
        .db $8A,$F7,$84                                     ;TripPad7(pyroid)
        .db $00,$00,$00                                     

mzta10  .db $00                                             

mztd10  .db $00                                             

mone10  .db $00                                             

tite10  .db $00                                             

lock10  .db $00                                             

tran10  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand10  .db $00                                             

mzty10  = $00
clock10 = $00
boot10  = $00
keyp10  = $00
outi10  = $30
reaz10  = $00
oxyb10  = $02
;********************************************************************
;  Dif 1 - Maze B - Level 6 - Level 6
;********************************************************************
mzdc11  .db $53,$60,$75,$93,$71,$82,$7B,$8A,$97             ;Oxygen
        .db $00                                             

mzlg11  .db $42                                             ;HLightningH9
        .db $9F                                             ;HLightningH10
        .db $7C                                             ;HLightningH11
        .db $00                                             

mzar11  .db $80,$00                                         ;Arrow10
        .db $61,$00                                         ;Arrow11
        .db $89,$00                                         ;Arrow12
        .db $A8,$00                                         ;Arrow13
        .db $00                                             

mzor11  .db $00                                             

mztr11  .db $76                                             ;TripPad8
        .db $78                                             ;TripPad9
        .db $98                                             ;TripPad10
        .db $9A                                             ;TripPad11
        .db $00                                             

trpa11  .db $85,$F7,$06                                     ;TripPad8(pyroid)
        .db $86,$F7,$06                                     ;TripPad9(pyroid)
        .db $88,$F9,$06                                     ;TripPad10(pyroid)
        .db $8D,$F9,$86                                     ;TripPad11(pyroid)
        .db $00,$00,$00                                     

mzta11  .db $00                                             

mztd11  .db $00                                             

mone11  .db $B7                                             ;OneWay3(right)
        .db $A3                                             ;OneWay1(right)
        .db $FF                                             
        .db $B9                                             ;OneWay4(left)
        .db $00                                             

mcp11a  .dw $0680,$F680                                     ;Position
        cann_pos(canp_br,0,1,12)                            ;GunPos - BotRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(16)                                        ;Pause = 16 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pos(canp_tr,0,1,12)                            ;GunPos - TopRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pos(canp_br,0,1,12)                            ;GunPos - BotRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(16)                                        ;Pause = 16 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pos(canp_tr,0,1,12)                            ;GunPos - TopRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_end                                            ;Loop It

mcp11b  .dw $057C,$F680                                     ;Position
        cann_pos(canp_tr,0,1,12)                            ;GunPos - TopRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(16)                                        ;Pause = 16 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pos(canp_br,0,1,12)                            ;GunPos - BotRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pos(canp_tr,0,1,12)                            ;GunPos - TopRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(16)                                        ;Pause = 16 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pos(canp_br,0,1,12)                            ;GunPos - BotRight Speed: 0 Shot: Yes ShotVel: $0C
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_pau(8)                                         ;Pause = 8 frames
        cann_end                                            ;Loop It

tite11  .db $76                                             ;Spikes1
        .db $78                                             ;Spikes2
        .db $82                                             ;Spikes1
        .db $93                                             ;Spikes2
        .db $63                                             ;Spikes3
        .db $00                                             

lock11  .db $00                                             

tran11  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand11  .db $00                                             

mzty11  = $01
clock11 = $00
boot11  = $00
keyp11  = $00
mpod11  = $01
outi11  = $30
reaz11  = $00
oxyb11  = $10
;********************************************************************
;  Dif 1 - Maze C - Level 7 - Level 7
;********************************************************************
mzdc12  .db $71,$53,$78,$66,$49,$8D,$59,$48,$47,$5A         ;Oxygen
        .db $00                                             

mzlg12  .db $4C                                             ;HLightningH12
        .db $46                                             ;HLightningH13
        .db $FF                                             
        .db $8E                                             ;VLightningV8
        .db $47                                             ;VLightningV9
        .db $66                                             ;VLightningV11
        .db $6B                                             ;VLightningV1
        .db $61                                             ;VLightningV2
        .db $00                                             

mzar12  .db $8A,$03                                         ;Arrow12
        .db $84,$03                                         ;Arrow14
        .db $97,$03                                         ;Arrow14
        .db $9D,$03                                         ;Arrow15
        .db $91,$03                                         ;Arrow5
        .db $00                                             

mzor12  .db $00                                             

mztr12  .db $6D                                             ;TripPad12
        .db $54                                             ;TripPad14
        .db $72                                             ;TripPad15
        .db $74                                             ;TripPad16
        .db $93                                             ;TripPad17
        .db $00                                             

trpa12  .db $0C,$F7,$04                                     ;TripPad12(pyroid)
        .db $87,$F5,$83                                     ;TripPad14(pyroid)
        .db $87,$F7,$85                                     ;TripPad15(pyroid)
        .db $82,$F7,$06                                     ;TripPad16(pyroid)
        .db $81,$F9,$04                                     ;TripPad17(pyroid)
        .db $00,$00,$00                                     

mzta12  .db $98,$01                                         ;MazeWall1
        .db $B0,$05                                         ;MazeWall1
        .db $8A,$04                                         ;MazeWall6
        .db $00                                             

mztd12  .db $00                                             

mone12  .db $FF                                             
        .db $A8                                             ;OneWay5(left)
        .db $00                                             

tite12  .db $72                                             ;Spikes4
        .db $74                                             ;Spikes5
        .db $93                                             ;Spikes6
        .db $54                                             ;Spikes7
        .db $00                                             

lock12  .db $01,$65,$AA                                     ;Blue
        .db $00                                             

tran12  .db (colorange+tr_right),$B8                        ;Orange
        .db (colorange+tr_left),$6E                         ;Orange
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand12  .db $09,$F8,$03,$02,$2A,$0B,$20,$02,$01             ;Hand1

mzty12  = $02
clock12 = $00
boot12  = $00
keyp12  = $00
outi12  = $30
reaz12  = $00
oxyb12  = $0C
;********************************************************************
;  Dif 2 - Maze D - Level 8 - Level 8
;********************************************************************
mzdc13  .db $A6,$98,$73,$48,$40,$25,$26,$27,$28,$5A         ;Oxygen
        .db $00                                             

mzlg13  .db $72                                             ;HLightningH15
        .db $FF                                             
        .db $85                                             ;VLightningV5
        .db $3A                                             ;VLightningV6
        .db $00                                             

mzar13  .db $92,$02                                         ;Arrow16
        .db $76,$02                                         ;Arrow17
        .db $59,$02                                         ;Arrow18
        .db $00                                             

mzor13  .db $00                                             

mztr13  .db $A6                                             ;TripPad18
        .db $98                                             ;TripPad19
        .db $67                                             ;TripPad21
        .db $68                                             ;TripPad22
        .db $37                                             ;TripPad23
        .db $38                                             ;TripPad24
        .db $66                                             ;TripPad1
        .db $00                                             

trpa13  .db $89,$FA,$86                                     ;TripPad18(pyroid)
        .db $8B,$F9,$86                                     ;TripPad19(pyroid)
        .db $04,$F6,$04                                     ;TripPad21(pyroid)
        .db $85,$F6,$04                                     ;TripPad22(pyroid)
        .db $04,$F3,$05                                     ;TripPad23(pyroid)
        .db $04,$F3,$05                                     ;TripPad24(pyroid)
        .db $04,$F6,$04                                     ;TripPad1(pyroid)
        .db $00,$00,$00                                     

mzta13  .db $00                                             

mztd13  .db $00                                             

mone13  .db $B7                                             ;OneWay8(right)
        .db $44                                             ;OneWay11(right)
        .db $FF                                             
        .db $A5                                             ;OneWay9(left)
        .db $FF                                             
        .db $69                                             ;OneWay10(left)
        .db $00                                             

tite13  .db $37                                             ;Spikes8
        .db $38                                             ;Spikes9
        .db $47                                             ;Spikes1
        .db $48                                             ;Spikes2
        .db $00                                             

lock13  .db $0E,$72,$29                                     ;Bluer
        .db $0A,$47,$32                                     ;Orange
        .db $04,$44,$9B                                     ;Red2
        .db $00                                             

tran13  .db (colblue+tr_right),$24                          ;Blue
        .db (colblue+tr_left),$74                           ;Blue
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand13  .db $00                                             

mzty13  = $03
clock13 = $00
boot13  = $29
keyp13  = $00
outi13  = $28
reaz13  = $00
oxyb13  = $18
;********************************************************************
;  Dif 2 - Maze A - Level 9 - Level 9
;********************************************************************
mzdc20  .db $A3,$B3,$95,$83,$74,$77,$B6,$80                 ;Oxygen
        .db $00                                             

mzlg20  .db $FF                                             
        .db $A3                                             ;VLightningV13
        .db $B7                                             ;VLightningV14
        .db $99                                             ;VLightningV1
        .db $79                                             ;VLightningV2
        .db $00                                             

mzar20  .db $A1,$07                                         ;Arrow19
        .db $78,$06                                         ;Arrow20
        .db $81,$07                                         ;Arrow22
        .db $98,$06                                         ;Arrow23
        .db $00                                             

mzor20  .db $00                                             

mztr20  .db $64                                             ;TripPad25
        .db $65                                             ;TripPad26
        .db $94                                             ;TripPad27
        .db $A6                                             ;TripPad28
        .db $B5                                             ;TripPad29
        .db $87                                             ;TripPad1
        .db $00                                             

trpa20  .db $08,$F6,$84                                     ;TripPad25(pyroid)
        .db $08,$F6,$84                                     ;TripPad26(pyroid)
        .db $06,$F9,$84                                     ;TripPad27(pyroid)
        .db $08,$FA,$84                                     ;TripPad28(pyroid)
        .db $09,$FB,$84                                     ;TripPad29(pyroid)
        .db $0B,$F8,$83                                     ;TripPad1(pyroid)
        .db $00,$00,$00                                     

mzta20  .db $3B,$05                                         ;MazeWall29
        .db $3C,$01                                         ;MazeWall30
        .db $3D,$03                                         ;MazeWall31
        .db $4C,$07                                         ;MazeWall32
        .db $4F,$05                                         ;MazeWall1
        .db $5F,$04                                         ;MazeWall2
        .db $00                                             

mztd20  .db $2C,$00,$08,$07,$01                             ;MazeWall33
        .db $4B,$00,$08,$04,$03                             ;MazeWall34
        .db $00                                             

mone20  .db $00                                             

tite20  .db $00                                             

lock20  .db $00                                             

tran20  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand20  .db $00                                             

mzty20  = $00
clock20 = $00
boot20  = $00
keyp20  = $00
outi20  = $25
reaz20  = $00
oxyb20  = $06
;********************************************************************
;  Dif 2 - Maze B - Level 10 - Level 10
;********************************************************************
mzdc21  .db $53,$93,$71,$6C,$7B,$8A,$62                     ;Oxygen
        .db $00                                             

mzlg21  .db $42                                             ;HLightningH16
        .db $8F                                             ;HLightningH17
        .db $84                                             ;HLightningH18
        .db $86                                             ;HLightningH1
        .db $9F                                             ;HLightningH1
        .db $FF                                             
        .db $A9                                             ;VLightningV15
        .db $89                                             ;VLightningV16
        .db $69                                             ;VLightningV17
        .db $67                                             ;VLightningV18
        .db $A3                                             ;VLightningV6
        .db $00                                             

mzar21  .db $7A,$02                                         ;Arrow59
        .db $9D,$02                                         ;Arrow25
        .db $AA,$02                                         ;Arrow26
        .db $87,$02                                         ;Arrow27
        .db $00                                             

mzor21  .db $00                                             

mztr21  .db $77                                             ;TripPad30
        .db $98                                             ;TripPad31
        .db $99                                             ;TripPad32
        .db $9A                                             ;TripPad33
        .db $B7                                             ;TripPad35
        .db $B9                                             ;TripPad1
        .db $00                                             

trpa21  .db $05,$F7,$06                                     ;TripPad30(pyroid)
        .db $08,$F9,$03                                     ;TripPad31(pyroid)
        .db $08,$F9,$02                                     ;TripPad32(pyroid)
        .db $08,$F9,$02                                     ;TripPad33(pyroid)
        .db $8C,$FB,$85                                     ;TripPad35(pyroid)
        .db $86,$FB,$05                                     ;TripPad1(pyroid)
        .db $00,$00,$00                                     

mzta21  .db $74,$02                                         ;MazeWall35
        .db $89,$03                                         ;MazeWall36
        .db $62,$01                                         ;MazeWall37
        .db $00                                             

mztd21  .db $72,$00,$06,$01,$02                             ;MazeWall38
        .db $87,$00,$06,$01,$03                             ;MazeWall39
        .db $48,$00,$06,$02,$01                             ;MazeWall40
        .db $49,$00,$06,$05,$01                             ;MazeWall41
        .db $63,$00,$02,$06,$03                             ;MazeWall42
        .db $00                                             

mone21  .db $00                                             

mcp21a  .dw $0B00,$F680                                     ;Position
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_mr,2,1,16)                            ;GunPos - MidRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_tr,2,1,16)                            ;GunPos - TopRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pau(18)                                        ;Pause = 18 frames
        cann_end                                            ;Loop It

tite21  .db $77                                             ;Spikes10
        .db $B7                                             ;Spikes12
        .db $82                                             ;Spikes1
        .db $79                                             ;Spikes2
        .db $B9                                             ;Spikes4
        .db $00                                             

lock21  .db $0A,$8A,$65                                     ;Orange
        .db $01,$82,$5C                                     ;Blue
        .db $00                                             

tran21  .db (colred+tr_left),$66                            ;Red
        .db (colred+tr_right),$B6                           ;Red
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand21  .db $00                                             

mzty21  = $01
clock21 = $00
boot21  = $00
keyp21  = $00
mpod21  = $02
outi21  = $40
reaz21  = $00
oxyb21  = $14
;********************************************************************
;  Dif 2 - Maze C - Level 11 - Level 11
;********************************************************************
mzdc22  .db $92,$72,$82,$6E,$5B,$7C,$7B,$7A,$5A,$A9         ;Oxygen
        .db $00                                             

mzlg22  .db $46                                             ;HLightningH19
        .db $4C                                             ;HLightningH20
        .db $59                                             ;HLightningH21
        .db $6C                                             ;HLightningH22
        .db $98                                             ;HLightningH23
        .db $FF                                             
        .db $B9                                             ;VLightningV19
        .db $99                                             ;VLightningV20
        .db $87                                             ;VLightningV21
        .db $55                                             ;VLightningV22
        .db $AC                                             ;VLightningV23
        .db $00                                             

mzar22  .db $62,$01                                         ;Arrow67
        .db $9B,$01                                         ;Arrow69
        .db $75,$00                                         ;Arrow22
        .db $8E,$00                                         ;Arrow1
        .db $00                                             

mzor22  .db $00                                             

mztr22  .db $8C                                             ;TripPad36
        .db $8D                                             ;TripPad37
        .db $8D                                             ;TripPad38
        .db $6E                                             ;TripPad39
        .db $00                                             

trpa22  .db $89,$F8,$04                                     ;TripPad36(pyroid)
        .db $0A,$F8,$02                                     ;TripPad37(pyroid)
        .db $0B,$F9,$01                                     ;TripPad38(pyroid)
        .db $10,$F6,$84                                     ;TripPad39(pyroid)
        .db $00,$00,$00                                     

mzta22  .db $34,$03                                         ;MazeWall43
        .db $47,$02                                         ;MazeWall44
        .db $48,$05                                         ;MazeWall45
        .db $49,$01                                         ;MazeWall46
        .db $56,$05                                         ;MazeWall47
        .db $57,$01                                         ;MazeWall48
        .db $5B,$07                                         ;MazeWall49
        .db $5C,$04                                         ;MazeWall50
        .db $5D,$03                                         ;MazeWall51
        .db $62,$01                                         ;MazeWall52
        .db $6D,$01                                         ;MazeWall54
        .db $6F,$06                                         ;MazeWall55
        .db $73,$05                                         ;MazeWall56
        .db $74,$01                                         ;MazeWall57
        .db $84,$06                                         ;MazeWall58
        .db $85,$06                                         ;MazeWall59
        .db $87,$07                                         ;MazeWall60
        .db $88,$05                                         ;MazeWall61
        .db $89,$01                                         ;MazeWall62
        .db $8A,$01                                         ;MazeWall63
        .db $8B,$05                                         ;MazeWall64
        .db $99,$06                                         ;MazeWall65
        .db $B1,$04                                         ;MazeWall66
        .db $4B,$01                                         ;MazeWall67
        .db $37,$02                                         ;MazeWall68
        .db $4C,$01                                         ;MazeWall69
        .db $4A,$07                                         ;MazeWall70
        .db $6C,$07                                         ;MazeWall1
        .db $00                                             

mztd22  .db $71,$00,$06,$02,$01                             ;MazeWall71
        .db $72,$00,$06,$05,$01                             ;MazeWall72
        .db $75,$00,$06,$07,$01                             ;MazeWall73
        .db $9A,$00,$12,$04,$06                             ;MazeWall74
        .db $AF,$00,$12,$05,$06                             ;MazeWall75
        .db $00                                             

mone22  .db $FF                                             
        .db $A8                                             ;OneWay6(left)
        .db $00                                             

tite22  .db $00                                             

lock22  .db $05,$BA,$6D                                     ;Purple
        .db $00                                             

tran22  .db (colcyan+tr_left),$7D                           ;Cyan
        .db (colcyan+tr_right),$48                          ;Cyan
        .db $00                                             
        .db $00,$00,$00,$40                                 ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand22  .db $00                                             

mzty22  = $02
clock22 = $00
boot22  = $B9
keyp22  = $00
outi22  = $30
reaz22  = $00
oxyb22  = $14
;********************************************************************
;  Dif 3 - Maze D - Level 12 - Level 12
;********************************************************************
mzdc23  .db $99,$88,$77,$65,$48,$23,$83,$24,$25,$26,$27,$28,$74;Oxygen
        .db $00                                             

mzlg23  .db $97                                             ;HLightningH24
        .db $B9                                             ;HLightningH25
        .db $52                                             ;HLightningH26
        .db $FF                                             
        .db $36                                             ;VLightningV25
        .db $48                                             ;VLightningV26
        .db $55                                             ;VLightningV27
        .db $58                                             ;VLightningV28
        .db $23                                             ;VLightningV29
        .db $75                                             ;VLightningV30
        .db $78                                             ;VLightningV31
        .db $00                                             

mzar23  .db $B7,$03                                         ;Arrow75
        .db $A4,$03                                         ;Arrow20
        .db $91,$03                                         ;Arrow21
        .db $4B,$02                                         ;Arrow3
        .db $38,$02                                         ;Arrow4
        .db $00                                             

mzor23  .db $00                                             

mztr23  .db $54                                             ;TripPad40
        .db $64                                             ;TripPad41
        .db $69                                             ;TripPad42
        .db $49                                             ;TripPad43
        .db $00                                             

trpa23  .db $06,$F5,$83                                     ;TripPad40(pyroid)
        .db $04,$F6,$03                                     ;TripPad41(pyroid)
        .db $0D,$F6,$84                                     ;TripPad42(pyroid)
        .db $0C,$F4,$84                                     ;TripPad43(pyroid)
        .db $00,$00,$00                                     

mzta23  .db $31,$03                                         ;MazeWall76
        .db $42,$02                                         ;MazeWall77
        .db $43,$05                                         ;MazeWall78
        .db $44,$01                                         ;MazeWall79
        .db $55,$04                                         ;MazeWall80
        .db $56,$03                                         ;MazeWall81
        .db $6A,$02                                         ;MazeWall82
        .db $8C,$01                                         ;MazeWall83
        .db $A0,$04                                         ;MazeWall84
        .db $A1,$01                                         ;MazeWall85
        .db $A3,$03                                         ;MazeWall86
        .db $A4,$07                                         ;MazeWall87
        .db $B4,$01                                         ;MazeWall88
        .db $00                                             

mztd23  .db $7A,$00,$02,$03,$02                             ;MazeWall89
        .db $7D,$05,$1C,$04,$05                             ;MazeWall90
        .db $8D,$00,$02,$02,$03                             ;MazeWall91
        .db $90,$05,$1C,$05,$04                             ;MazeWall92
        .db $91,$00,$08,$07,$01                             ;MazeWall93
        .db $B8,$00,$01,$07,$01                             ;MazeWall94
        .db $7C,$00,$10,$01,$07                             ;MazeWall95
        .db $8E,$12,$18,$01,$07                             ;MazeWall96
        .db $8F,$0C,$18,$01,$07                             ;MazeWall97
        .db $00                                             

mone23  .db $FF                                             
        .db $A7                                             ;OneWay7(left)
        .db $00                                             

tite23  .db $54                                             ;Spikes13
        .db $64                                             ;Spikes14
        .db $46                                             ;Spikes1
        .db $76                                             ;Spikes2
        .db $69                                             ;Spikes3
        .db $00                                             

lock23  .db $02,$49,$29                                     ;Green
        .db $01,$41,$28                                     ;Blue
        .db $04,$24,$A8                                     ;Red2
        .db $00                                             

tran23  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand23  .db $00                                             

mzty23  = $03
clock23 = $00
boot23  = $00
keyp23  = $00
outi23  = $24
reaz23  = $00
oxyb23  = $08
;********************************************************************
;  Dif 3 - Maze A - Level 13 - Level 13
;********************************************************************
mzdc30  .db $72,$75,$78,$B5,$B6,$65,$64,$66                 ;Oxygen
        .db $00                                             

mzlg30  .db $94                                             ;HLightningH1
        .db $95                                             ;HLightningH2
        .db $96                                             ;HLightningH3
        .db $93                                             ;HLightningH4
        .db $97                                             ;HLightningH5
        .db $FF                                             
        .db $68                                             ;VLightningV32
        .db $00                                             

mzar30  .db $74,$00                                         ;Arrow5
        .db $71,$00                                         ;Arrow9
        .db $85,$00                                         ;Arrow6
        .db $88,$00                                         ;Arrow7
        .db $82,$00                                         ;Arrow8
        .db $77,$00                                         ;Arrow10
        .db $00                                             

mzor30  .db $00                                             

mztr30  .db $75                                             ;TripPad48
        .db $72                                             ;TripPad49
        .db $78                                             ;TripPad50
        .db $00                                             

trpa30  .db $85,$F7,$05                                     ;TripPad48(pyroid)
        .db $82,$F7,$05                                     ;TripPad49(pyroid)
        .db $88,$F7,$05                                     ;TripPad50(pyroid)
        .db $00,$00,$00                                     

mzta30  .db $4C,$01                                         ;MazeWall98
        .db $4D,$01                                         ;MazeWall99
        .db $6C,$03                                         ;MazeWall100
        .db $5C,$05                                         ;MazeWall101
        .db $5D,$02                                         ;MazeWall102
        .db $6D,$04                                         ;MazeWall103
        .db $59,$05                                         ;MazeWall104
        .db $5E,$07                                         ;MazeWall106
        .db $58,$07                                         ;MazeWall107
        .db $3B,$01                                         ;MazeWall108
        .db $3C,$01                                         ;MazeWall109
        .db $2A,$01                                         ;MazeWall112
        .db $7E,$03                                         ;MazeWall113
        .db $7D,$01                                         ;MazeWall114
        .db $2D,$01                                         ;MazeWall115
        .db $77,$04                                         ;MazeWall118
        .db $79,$01                                         ;MazeWall119
        .db $78,$01                                         ;MazeWall62
        .db $3D,$01                                         ;MazeWall65
        .db $4E,$07                                         ;MazeWall5
        .db $49,$01                                         ;MazeWall1
        .db $48,$01                                         ;MazeWall3
        .db $00                                             

mztd30  .db $00                                             

mone30  .db $FF                                             
        .db $A4                                             ;OneWay4(left)
        .db $FF                                             
        .db $A2                                             ;OneWay7(left)
        .db $FF                                             
        .db $A7                                             ;OneWay1(left)
        .db $00                                             

mcp30a  .dw $0AF8,$F880                                     ;Position
        cann_pos(canp_tl,1,1,12)                            ;GunPos - TopLeft Speed: 1 Shot: Yes ShotVel: $0C
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_bl,2,1,12)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $0C
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_tl,1,1,12)                            ;GunPos - TopLeft Speed: 1 Shot: Yes ShotVel: $0C
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_bl,2,1,12)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $0C
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_tl,1,1,12)                            ;GunPos - TopLeft Speed: 1 Shot: Yes ShotVel: $0C
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_bl,2,1,12)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $0C
        cann_pau(4)                                         ;Pause = 4 frames
        cann_end                                            ;Loop It

tite30  .db $00                                             

lock30  .db $06,$A6,$73                                     ;Yellow
        .db $01,$74,$92                                     ;Blue
        .db $00                                             

tran30  .db (colred2+tr_left),$B7                           ;Red2
        .db (colred2+tr_right),$62                          ;Red2
        .db (colyellow+tr_left),$97                         ;Yellow
        .db (colyellow+tr_right),$B4                        ;Yellow
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand30  .db $00                                             

mzty30  = $00
clock30 = $77
boot30  = $00
keyp30  = $00
outi30  = $23
reaz30  = $00
oxyb30  = $0C
;********************************************************************
;  Dif 3 - Maze B - Level 14 - Level 14
;********************************************************************
mzdc31  .db $5B,$53,$A8,$96,$60,$6D                         ;Oxygen
        .db $00                                             

mzlg31  .db $7C                                             ;HLightningH27
        .db $4A                                             ;HLightningH28
        .db $4B                                             ;HLightningH29
        .db $9F                                             ;HLightningH30
        .db $42                                             ;HLightningH31
        .db $FF                                             
        .db $A9                                             ;VLightningV35
        .db $59                                             ;VLightningV36
        .db $49                                             ;VLightningV37
        .db $68                                             ;VLightningV39
        .db $00                                             

mzar31  .db $74,$02                                         ;Arrow81
        .db $88,$02                                         ;Arrow82
        .db $80,$02                                         ;Arrow83
        .db $7D,$02                                         ;Arrow11
        .db $00                                             

mzor31  .db $00                                             

mztr31  .db $00                                             

trpa31  .db $00,$00,$00                                     

mzta31  .db $21,$01                                         ;MazeWall122
        .db $36,$01                                         ;MazeWall124
        .db $22,$01                                         ;MazeWall125
        .db $B4,$05                                         ;MazeWall126
        .db $23,$02                                         ;MazeWall127
        .db $00                                             

mztd31  .db $35,$00,$05,$01,$07                             ;MazeWall128
        .db $9D,$00,$0E,$02,$01                             ;MazeWall2
        .db $00                                             

mone31  .db $00                                             

mcp31a  .dw $0500,$F680                                     ;Position
        cann_pos(canp_br,2,1,24)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $18
        cann_pos(canp_mr,2,0,0)                             ;GunPos - MidRight Speed: 2 Shot: No
        cann_pos(canp_tr,2,1,24)                            ;GunPos - TopRight Speed: 2 Shot: Yes ShotVel: $18
        cann_loc(16,20,0)                                   ;GunLoc - Frames: 16 XVel: 20 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_bl,2,1,24)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $18
        cann_pos(canp_ml,2,0,0)                             ;GunPos - MidLeft Speed: 2 Shot: No
        cann_pos(canp_tl,2,1,24)                            ;GunPos - TopLeft Speed: 2 Shot: Yes ShotVel: $18
        cann_loc(16,-20,0)                                  ;GunLoc - Frames: 16 XVel: -20 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_end                                            ;Loop It

mcp31b  .dw $0A7C,$F780                                     ;Position
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_bl,2,1,16)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_bl,2,1,16)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_bl,2,0,0)                             ;GunPos - BotLeft Speed: 2 Shot: No
        cann_end                                            ;Loop It

tite31  .db $00                                             

lock31  .db $01,$8D,$B6                                     ;Blue
        .db $02,$72,$BB                                     ;Green
        .db $04,$5A,$5D                                     ;Red2
        .db $00                                             

tran31  .db (colblue+tr_left),$93                           ;Blue
        .db (colblue+tr_right),$99                          ;Blue
        .db (colyellow+tr_left),$BC                         ;Yellow
        .db (colyellow+tr_right),$76                        ;Yellow
        .db $00                                             
        .db $00,$00,$FC,$43,$00,$01                         ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand31  .db $00                                             

mzty31  = $01
clock31 = $00
boot31  = $68
keyp31  = $00
mpod31  = $01
outi31  = $24
reaz31  = $00
oxyb31  = $1C
;********************************************************************
;  Dif 3 - Maze C - Level 15 - Level 15
;********************************************************************
mzdc32  .db $80,$54,$61,$5B,$9C,$93                         ;Oxygen
        .db $00                                             

mzlg32  .db $4C                                             ;HLightningH32
        .db $46                                             ;HLightningH33
        .db $8C                                             ;HLightningH34
        .db $4B                                             ;HLightningH35
        .db $8D                                             ;HLightningH36
        .db $FF                                             
        .db $7B                                             ;VLightningV40
        .db $8A                                             ;VLightningV41
        .db $68                                             ;VLightningV42
        .db $53                                             ;VLightningV43
        .db $00                                             

mzar32  .db $A5,$03                                         ;Arrow86
        .db $90,$03                                         ;Arrow87
        .db $62,$03                                         ;Arrow10
        .db $88,$03                                         ;Arrow13
        .db $9D,$03                                         ;Arrow14
        .db $6D,$03                                         ;Arrow15
        .db $00                                             

mzor32  .db $00                                             

mztr32  .db $73                                             ;TripPad51
        .db $7D                                             ;TripPad53
        .db $00                                             

trpa32  .db $87,$F7,$85                                     ;TripPad51(pyroid)
        .db $8B,$F7,$05                                     ;TripPad53(pyroid)
        .db $00,$00,$00                                     

mzta32  .db $4B,$02                                         ;MazeWall129
        .db $56,$05                                         ;MazeWall130
        .db $57,$01                                         ;MazeWall131
        .db $5F,$02                                         ;MazeWall132
        .db $6C,$07                                         ;MazeWall133
        .db $6D,$01                                         ;MazeWall134
        .db $75,$04                                         ;MazeWall135
        .db $76,$03                                         ;MazeWall136
        .db $77,$04                                         ;MazeWall137
        .db $60,$06                                         ;MazeWall138
        .db $8D,$07                                         ;MazeWall139
        .db $78,$07                                         ;MazeWall140
        .db $9C,$03                                         ;MazeWall141
        .db $97,$02                                         ;MazeWall142
        .db $98,$07                                         ;MazeWall143
        .db $B4,$07                                         ;MazeWall144
        .db $B3,$07                                         ;MazeWall145
        .db $C9,$04                                         ;MazeWall146
        .db $CA,$03                                         ;MazeWall147
        .db $61,$07                                         ;MazeWall148
        .db $00                                             

mztd32  .db $00                                             

mone32  .db $00                                             

mcp32a  .dw $02D0,$F880                                     ;Position
        cann_pos(canp_br,2,1,18)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $12
        cann_pos(canp_br,2,1,18)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $12
        cann_loc(2,0,-16)                                   ;GunLoc - Frames: 2 XVel: 0 YVel: -16
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_br,2,1,18)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $12
        cann_pos(canp_br,2,1,18)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $12
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_loc(2,0,16)                                    ;GunLoc - Frames: 2 XVel: 0 YVel: 16
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_end                                            ;Loop It

mcp32b  .dw $07D0,$F780                                     ;Position
        cann_pos(canp_br,2,1,18)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $12
        cann_pau(11)                                        ;Pause = 11 frames
        cann_end                                            ;Loop It

tite32  .db $73                                             ;Spikes15
        .db $7D                                             ;Spikes16
        .db $B7                                             ;Spikes6
        .db $57                                             ;Spikes3
        .db $00                                             

lock32  .db $06,$7C,$86                                     ;Yellow
        .db $04,$7A,$55                                     ;Red2
        .db $00                                             

tran32  .db (colgreen+tr_left),$96                          ;Green
        .db (colgreen+tr_right),$B8                         ;Green
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand32  .db $00                                             

mzty32  = $02
clock32 = $00
boot32  = $00
keyp32  = $00
outi32  = $12
reaz32  = $00
oxyb32  = $1C
;********************************************************************
;  Dif 4 - Maze D - Level 16 - Level 16
;********************************************************************
mzdc33  .db $35,$26,$37,$40,$A3,$54,$88,$9B,$49,$28,$94,$55 ;Oxygen
        .db $00                                             

mzlg33  .db $44                                             ;HLightningH37
        .db $43                                             ;HLightningH38
        .db $45                                             ;HLightningH39
        .db $46                                             ;HLightningH40
        .db $97                                             ;HLightningH12
        .db $FF                                             
        .db $5C                                             ;VLightningV44
        .db $A2                                             ;VLightningV45
        .db $87                                             ;VLightningV46
        .db $23                                             ;VLightningV47
        .db $52                                             ;VLightningV48
        .db $58                                             ;VLightningV49
        .db $74                                             ;VLightningV50
        .db $00                                             

mzar33  .db $B6,$02                                         ;Arrow16
        .db $A5,$01                                         ;Arrow17
        .db $A7,$00                                         ;Arrow18
        .db $3B,$03                                         ;Arrow19
        .db $4A,$01                                         ;Arrow20
        .db $4C,$00                                         ;Arrow21
        .db $96,$03                                         ;Arrow22
        .db $5B,$02                                         ;Arrow23
        .db $00                                             

mzor33  .db $00                                             

mztr33  .db $5A                                             ;TripPad54
        .db $00                                             

trpa33  .db $88,$F5,$05                                     ;TripPad54(pyroid)
        .db $00,$00,$00                                     

mzta33  .db $3F,$01                                         ;MazeWall149
        .db $67,$07                                         ;MazeWall150
        .db $7A,$01                                         ;MazeWall151
        .db $7B,$01                                         ;MazeWall152
        .db $C4,$03                                         ;MazeWall154
        .db $B2,$03                                         ;MazeWall155
        .db $B3,$03                                         ;MazeWall156
        .db $8D,$07                                         ;MazeWall157
        .db $9E,$07                                         ;MazeWall158
        .db $A5,$01                                         ;MazeWall159
        .db $92,$01                                         ;MazeWall160
        .db $93,$01                                         ;MazeWall161
        .db $AB,$07                                         ;MazeWall162
        .db $9F,$07                                         ;MazeWall163
        .db $9D,$05                                         ;MazeWall164
        .db $CA,$01                                         ;MazeWall166
        .db $C6,$05                                         ;MazeWall167
        .db $66,$02                                         ;MazeWall168
        .db $B1,$06                                         ;MazeWall169
        .db $C5,$07                                         ;MazeWall170
        .db $42,$05                                         ;MazeWall171
        .db $52,$01                                         ;MazeWall172
        .db $3E,$01                                         ;MazeWall173
        .db $41,$02                                         ;MazeWall174
        .db $53,$01                                         ;MazeWall175
        .db $54,$03                                         ;MazeWall176
        .db $94,$03                                         ;MazeWall177
        .db $8B,$04                                         ;MazeWall5
        .db $00                                             

mztd33  .db $A3,$00,$06,$02,$01                             ;MazeWall178
        .db $A4,$00,$06,$05,$01                             ;MazeWall179
        .db $00                                             

mone33  .db $00                                             

mcp33a  .dw $0530,$F380                                     ;Position
        cann_pau(1)                                         ;Pause = 1 frames
        cann_pos(canp_br,2,1,14)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0E
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_tr,2,1,14)                            ;GunPos - TopRight Speed: 2 Shot: Yes ShotVel: $0E
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_br,2,1,14)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0E
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pau(4)                                         ;Pause = 4 frames
        cann_loc(6,0,-16)                                   ;GunLoc - Frames: 6 XVel: 0 YVel: -16
        cann_loc(2,0,0)                                     ;GunLoc - Frames: 2 XVel: 0 YVel: 0
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pau(4)                                         ;Pause = 4 frames
        cann_loc(6,0,16)                                    ;GunLoc - Frames: 6 XVel: 0 YVel: 16
        cann_loc(2,0,0)                                     ;GunLoc - Frames: 2 XVel: 0 YVel: 0
        cann_end                                            ;Loop It

tite33  .db $5A                                             ;Spikes17
        .db $6A                                             ;Spikes7
        .db $36                                             ;Spikes9
        .db $00                                             

lock33  .db $01,$74,$57                                     ;Blue
        .db $06,$43,$6B                                     ;Yellow
        .db $04,$47,$25                                     ;Red2
        .db $00                                             

tran33  .db (colyellow+tr_left),$76                         ;Yellow
        .db (colyellow+tr_right),$3A                        ;Yellow
        .db (colgreen+tr_left),$66                          ;Green
        .db (colgreen+tr_right),$91                         ;Green
        .db (colblue+tr_right),$77                          ;Blue
        .db (colblue+tr_left),$83                           ;Blue
        .db (colred2+tr_right),$67                          ;Red2
        .db (colred2+tr_left),$39                           ;Red2
        .db $00                                             
        .db $00,$00,$FC,$03                                 ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand33  .db $00                                             

mzty33  = $03
clock33 = $00
boot33  = $58
keyp33  = $00
outi33  = $15
reaz33  = $00
oxyb33  = $14
;********************************************************************
;  Dif 4 - Maze A - Level 17 - Level 17
;********************************************************************
mzdc40  .db $95,$94,$B6,$80,$65,$A3,$5A,$A5,$77             ;Oxygen
        .db $00                                             

mzlg40  .db $67                                             ;HLightningH41
        .db $00                                             

mzar40  .db $A7,$03                                         ;Arrow102
        .db $87,$02                                         ;Arrow103
        .db $92,$03                                         ;Arrow41
        .db $72,$02                                         ;Arrow44
        .db $81,$00                                         ;Arrow1
        .db $98,$01                                         ;Arrow2
        .db $00                                             

mzor40  .db $00                                             

mztr40  .db $B5                                             ;TripPad57
        .db $B6                                             ;TripPad58
        .db $00                                             

trpa40  .db $09,$FB,$84                                     ;TripPad57(pyroid)
        .db $09,$FB,$84                                     ;TripPad58(pyroid)
        .db $00,$00,$00                                     

mzta40  .db $2A,$01                                         ;MazeWall180
        .db $3B,$01                                         ;MazeWall181
        .db $5A,$01                                         ;MazeWall182
        .db $5B,$01                                         ;MazeWall183
        .db $6A,$01                                         ;MazeWall184
        .db $6C,$02                                         ;MazeWall185
        .db $00                                             

mztd40  .db $00                                             

mone40  .db $FF                                             
        .db $A4                                             ;OneWay14(left)
        .db $FF                                             
        .db $B4                                             ;OneWay15(left)
        .db $00                                             

mcp40a  .dw $0900,$F780                                     ;Position
        cann_loc(1,-4,0)                                    ;GunLoc - Frames: 1 XVel: -4 YVel: 0
        cann_pos(canp_bl,2,1,16)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_bl,2,1,16)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_loc(1,4,0)                                     ;GunLoc - Frames: 1 XVel: 4 YVel: 0
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_bl,2,0,0)                             ;GunPos - BotLeft Speed: 2 Shot: No
        cann_end                                            ;Loop It

tite40  .db $00                                             

lock40  .db $0E,$67,$A3                                     ;Bluer
        .db $01,$B5,$A6                                     ;Blue
        .db $0A,$B4,$89                                     ;Orange
        .db $00                                             

tran40  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand40  .db $06,$FA,$01,$02,$23,$0B,$21,$01,$01             ;Hand2

mzty40  = $00
clock40 = $8A
boot40  = $00
keyp40  = $00
outi40  = $08
reaz40  = $00
oxyb40  = $0A
;********************************************************************
;  Dif 4 - Maze B - Level 18 - Level 18
;********************************************************************
mzdc41  .db $53,$79,$71,$77,$97,$5D,$78,$99                 ;Oxygen
        .db $00                                             

mzlg41  .db $42                                             ;HLightningH42
        .db $9F                                             ;HLightningH43
        .db $8C                                             ;HLightningH44
        .db $8D                                             ;HLightningH45
        .db $FF                                             
        .db $A6                                             ;VLightningV52
        .db $7B                                             ;VLightningV53
        .db $00                                             

mzar41  .db $70,$00                                         ;Arrow105
        .db $91,$00                                         ;Arrow107
        .db $84,$00                                         ;Arrow109
        .db $63,$00                                         ;Arrow26
        .db $00                                             

mzor41  .db $00                                             

mztr41  .db $00                                             

trpa41  .db $00,$00,$00                                     

mzta41  .db $33,$02                                         ;MazeWall186
        .db $34,$07                                         ;MazeWall187
        .db $35,$01                                         ;MazeWall188
        .db $36,$01                                         ;MazeWall189
        .db $48,$04                                         ;MazeWall190
        .db $74,$01                                         ;MazeWall191
        .db $A0,$01                                         ;MazeWall192
        .db $75,$03                                         ;MazeWall193
        .db $46,$05                                         ;MazeWall194
        .db $00                                             

mztd41  .db $4A,$00,$02,$07,$01                             ;MazeWall195
        .db $00                                             

mone41  .db $FF                                             
        .db $B7                                             ;OneWay16(left)
        .db $00                                             

mcp41a  .dw $0B00,$F680                                     ;Position
        cann_pos(canp_bl,2,1,13)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $0D
        cann_pos(canp_tl,2,1,13)                            ;GunPos - TopLeft Speed: 2 Shot: Yes ShotVel: $0D
        cann_pos(canp_bl,2,1,13)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $0D
        cann_pos(canp_tl,2,0,0)                             ;GunPos - TopLeft Speed: 2 Shot: No
        cann_pau(17)                                        ;Pause = 17 frames
        cann_end                                            ;Loop It

mcp41b  .dw $0C80,$FA80                                     ;Position
        cann_pos(canp_tr,1,0,0)                             ;GunPos - TopRight Speed: 1 Shot: No
        cann_pau(6)                                         ;Pause = 6 frames
        cann_pos(canp_br,1,1,11)                            ;GunPos - BotRight Speed: 1 Shot: Yes ShotVel: $0B
        cann_pos(canp_br,1,1,11)                            ;GunPos - BotRight Speed: 1 Shot: Yes ShotVel: $0B
        cann_end                                            ;Loop It

tite41  .db $00                                             

lock41  .db $01,$72,$7A                                     ;Blue
        .db $0A,$B7,$7A                                     ;Orange
        .db $0E,$6F,$7A                                     ;Bluer
        .db $00                                             

tran41  .db (colorange+tr_left),$6B                         ;Orange
        .db (colorange+tr_right),$A7                        ;Orange
        .db (colblue+tr_left),$AC                           ;Blue
        .db (colblue+tr_right),$76                          ;Blue
        .db (colbluer+tr_left),$BA                          ;Bluer
        .db (colbluer+tr_right),$5C                         ;Bluer
        .db $00                                             
        .db $00,$00,$FC,$03,$00,$0C                         ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand41  .db $00                                             

mzty41  = $01
clock41 = $00
boot41  = $00
keyp41  = $00
mpod41  = $02
outi41  = $12
reaz41  = $00
oxyb41  = $0E
;********************************************************************
;  Dif 4 - Maze C - Level 19 - Level 19
;********************************************************************
mzdc42  .db $A8,$87,$6A,$66,$8D,$B9,$84,$96                 ;Oxygen
        .db $00                                             

mzlg42  .db $37                                             ;HLightningH46
        .db $38                                             ;HLightningH47
        .db $39                                             ;HLightningH48
        .db $3A                                             ;HLightningH49
        .db $3B                                             ;HLightningH50
        .db $3C                                             ;HLightningH51
        .db $FF                                             
        .db $A6                                             ;VLightningV54
        .db $69                                             ;VLightningV55
        .db $6F                                             ;VLightningV56
        .db $4A                                             ;VLightningV57
        .db $00                                             

mzar42  .db $99,$01                                         ;Arrow37
        .db $77,$01                                         ;Arrow38
        .db $B7,$01                                         ;Arrow39
        .db $95,$01                                         ;Arrow40
        .db $00                                             

mzor42  .db $00                                             

mztr42  .db $8B                                             ;TripPad59
        .db $8C                                             ;TripPad60
        .db $8D                                             ;TripPad61
        .db $00                                             

trpa42  .db $10,$F8,$84                                     ;TripPad59(pyroid)
        .db $10,$F8,$84                                     ;TripPad60(pyroid)
        .db $10,$F8,$84                                     ;TripPad61(pyroid)
        .db $00,$00,$00                                     

mzta42  .db $4E,$01                                         ;MazeWall196
        .db $5B,$02                                         ;MazeWall197
        .db $62,$01                                         ;MazeWall198
        .db $63,$01                                         ;MazeWall199
        .db $6D,$01                                         ;MazeWall200
        .db $70,$04                                         ;MazeWall201
        .db $74,$01                                         ;MazeWall202
        .db $75,$01                                         ;MazeWall203
        .db $78,$07                                         ;MazeWall204
        .db $84,$01                                         ;MazeWall205
        .db $85,$01                                         ;MazeWall206
        .db $86,$01                                         ;MazeWall207
        .db $89,$01                                         ;MazeWall208
        .db $8A,$01                                         ;MazeWall209
        .db $8D,$01                                         ;MazeWall210
        .db $8E,$03                                         ;MazeWall211
        .db $99,$01                                         ;MazeWall212
        .db $9A,$01                                         ;MazeWall213
        .db $9B,$02                                         ;MazeWall214
        .db $9D,$05                                         ;MazeWall215
        .db $9E,$01                                         ;MazeWall216
        .db $9F,$02                                         ;MazeWall217
        .db $AE,$01                                         ;MazeWall218
        .db $AF,$02                                         ;MazeWall219
        .db $B0,$03                                         ;MazeWall220
        .db $B1,$07                                         ;MazeWall221
        .db $B3,$07                                         ;MazeWall222
        .db $B4,$04                                         ;MazeWall223
        .db $C3,$07                                         ;MazeWall224
        .db $C4,$06                                         ;MazeWall225
        .db $C5,$07                                         ;MazeWall226
        .db $C6,$07                                         ;MazeWall227
        .db $C7,$07                                         ;MazeWall228
        .db $C8,$06                                         ;MazeWall229
        .db $C9,$07                                         ;MazeWall230
        .db $CA,$06                                         ;MazeWall231
        .db $4F,$02                                         ;MazeWall232
        .db $00                                             

mztd42  .db $60,$00,$0C,$07,$01                             ;MazeWall233
        .db $00                                             

mone42  .db $8B                                             ;OneWay17(right)
        .db $FF                                             
        .db $8D                                             ;OneWay18(left)
        .db $00                                             

mcp42a  .dw $03A0,$F780                                     ;Position
        cann_pos(canp_br,2,1,11)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0B
        cann_pos(canp_tr,2,1,11)                            ;GunPos - TopRight Speed: 2 Shot: Yes ShotVel: $0B
        cann_loc(4,8,0)                                     ;GunLoc - Frames: 4 XVel: 8 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_tl,2,0,0)                             ;GunPos - TopLeft Speed: 2 Shot: No
        cann_loc(4,-8,0)                                    ;GunLoc - Frames: 4 XVel: -8 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pau(16)                                        ;Pause = 16 frames
        cann_end                                            ;Loop It

mcp42b  .dw $0E80,$F780                                     ;Position
        cann_pos(canp_bl,2,1,11)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $0B
        cann_pos(canp_tl,2,1,11)                            ;GunPos - TopLeft Speed: 2 Shot: Yes ShotVel: $0B
        cann_loc(4,-8,0)                                    ;GunLoc - Frames: 4 XVel: -8 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_loc(4,8,0)                                     ;GunLoc - Frames: 4 XVel: 8 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_bl,2,0,0)                             ;GunPos - BotLeft Speed: 2 Shot: No
        cann_pau(16)                                        ;Pause = 16 frames
        cann_end                                            ;Loop It

tite42  .db $00                                             

lock42  .db $01,$86,$61                                     ;Blue
        .db $05,$6A,$63                                     ;Purple
        .db $00                                             

tran42  .db (colorange+tr_left),$9E                         ;Orange
        .db (colorange+tr_right),$65                        ;Orange
        .db $00                                             
        .db $0C                                             ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand42  .db $00                                             

mzty42  = $02
clock42 = $00
boot42  = $62
keyp42  = $00
outi42  = $15
reaz42  = $00
oxyb42  = $12
;********************************************************************
;  Dif 5 - Maze D - Level 20 - Level 20
;********************************************************************
mzdc43  .db $86,$7A,$73,$57,$48,$41,$24,$25,$26,$27,$28,$4A,$87;Oxygen
        .db $00                                             

mzlg43  .db $8C                                             ;HLightningH52
        .db $FF                                             
        .db $34                                             ;VLightningV59
        .db $35                                             ;VLightningV60
        .db $44                                             ;VLightningV61
        .db $58                                             ;VLightningV62
        .db $5A                                             ;VLightningV63
        .db $00                                             

mzar43  .db $84,$04                                         ;Arrow3
        .db $8A,$04                                         ;Arrow5
        .db $72,$05                                         ;Arrow6
        .db $78,$05                                         ;Arrow8
        .db $00                                             

mzor43  .db $00                                             

mztr43  .db $48                                             ;TripPad62
        .db $4A                                             ;TripPad63
        .db $00                                             

trpa43  .db $0B,$F4,$84                                     ;TripPad62(pyroid)
        .db $0D,$F4,$84                                     ;TripPad63(pyroid)
        .db $00,$00,$00                                     

mzta43  .db $43,$07                                         ;MazeWall234
        .db $51,$05                                         ;MazeWall235
        .db $55,$01                                         ;MazeWall236
        .db $56,$01                                         ;MazeWall237
        .db $64,$06                                         ;MazeWall238
        .db $7A,$01                                         ;MazeWall239
        .db $7B,$01                                         ;MazeWall240
        .db $92,$01                                         ;MazeWall241
        .db $93,$01                                         ;MazeWall242
        .db $A0,$01                                         ;MazeWall243
        .db $A1,$02                                         ;MazeWall244
        .db $A2,$07                                         ;MazeWall245
        .db $A4,$07                                         ;MazeWall246
        .db $A5,$01                                         ;MazeWall247
        .db $A6,$07                                         ;MazeWall248
        .db $AE,$06                                         ;MazeWall249
        .db $AF,$01                                         ;MazeWall250
        .db $B3,$02                                         ;MazeWall251
        .db $B9,$01                                         ;MazeWall252
        .db $C1,$04                                         ;MazeWall253
        .db $C2,$01                                         ;MazeWall254
        .db $D7,$01                                         ;MazeWall255
        .db $D6,$04                                         ;MazeWall256
        .db $00                                             

mztd43  .db $B2,$00,$10,$03,$06                             ;MazeWall257
        .db $00                                             

mone43  .db $29                                             ;OneWay19(right)
        .db $3B                                             ;OneWay20(right)
        .db $00                                             

mcp43a  .dw $0400,$FA80                                     ;Position
        cann_pau(16)                                        ;Pause = 16 frames
        cann_pos(canp_br,2,1,18)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $12
        cann_pos(canp_br,2,1,18)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $12
        cann_pau(4)                                         ;Pause = 4 frames
        cann_end                                            ;Loop It

mcp43b  .dw $0604,$F280                                     ;Position
        cann_pos(canp_br,1,1,10)                            ;GunPos - BotRight Speed: 1 Shot: Yes ShotVel: $0A
        cann_pos(canp_br,1,1,20)                            ;GunPos - BotRight Speed: 1 Shot: Yes ShotVel: $14
        cann_end                                            ;Loop It

mcp43c  .dw $0680,$F680                                     ;Position
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,12)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: 12
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,-12)                                  ;GunLoc - Frames: 1 XVel: 12 YVel: -12
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,12)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: 12
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,-4)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -4
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,12)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: 12
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_tl,2,0,0)                             ;GunPos - TopLeft Speed: 2 Shot: No
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,-12)                                 ;GunLoc - Frames: 1 XVel: -12 YVel: -12
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,12)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: 12
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,-12)                                 ;GunLoc - Frames: 1 XVel: -12 YVel: -12
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,12)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: 12
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,-12)                                 ;GunLoc - Frames: 1 XVel: -12 YVel: -12
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_end                                            ;Loop It

mcp43d  .dw $08E4,$F680                                     ;Position
        cann_loc(1,12,-12)                                  ;GunLoc - Frames: 1 XVel: 12 YVel: -12
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,12)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: 12
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,-12)                                  ;GunLoc - Frames: 1 XVel: 12 YVel: -12
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_bl,2,0,0)                             ;GunPos - BotLeft Speed: 2 Shot: No
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,12)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: 12
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,-12)                                 ;GunLoc - Frames: 1 XVel: -12 YVel: -12
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,12)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: 12
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,-12)                                 ;GunLoc - Frames: 1 XVel: -12 YVel: -12
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,12)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: 12
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,-12)                                  ;GunLoc - Frames: 1 XVel: 12 YVel: -12
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,12)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: 12
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_end                                            ;Loop It

tite43  .db $48                                             ;Spikes20
        .db $4A                                             ;Spikes21
        .db $26                                             ;Spikes22
        .db $00                                             

lock43  .db $06,$35,$A4                                     ;Yellow
        .db $0E,$2A,$B7                                     ;Bluer
        .db $0A,$9D,$39                                     ;Orange
        .db $00                                             

tran43  .db (colred2+tr_left),$99                           ;Red2
        .db (colred2+tr_right),$23                          ;Red2
        .db (colblue+tr_left),$6B                           ;Blue
        .db (colblue+tr_right),$95                          ;Blue
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand43  .db $02,$F4,$07,$02,$3C,$0B,$21,$04,$01             ;Hand3

mzty43  = $03
clock43 = $00
boot43  = $00
keyp43  = $00
outi43  = $22
reaz43  = $00
oxyb43  = $1A
;********************************************************************
;  Dif 5 - Maze A - Level 21 - Level 21
;********************************************************************
mzdc50  .db $A4,$B3,$77,$66,$B7,$80                         ;Oxygen
        .db $00                                             

mzlg50  .db $98                                             ;HLightningH53
        .db $99                                             ;HLightningH54
        .db $FF                                             
        .db $B5                                             ;VLightningV65
        .db $B4                                             ;VLightningV1
        .db $00                                             

mzar50  .db $00                                             

mzor50  .db $00                                             

mztr50  .db $00                                             

trpa50  .db $00,$00,$00                                     

mzta50  .db $2A,$01                                         ;MazeWall258
        .db $3B,$01                                         ;MazeWall259
        .db $3C,$07                                         ;MazeWall260
        .db $49,$01                                         ;MazeWall261
        .db $4C,$01                                         ;MazeWall262
        .db $4D,$03                                         ;MazeWall263
        .db $5A,$01                                         ;MazeWall264
        .db $5B,$01                                         ;MazeWall265
        .db $6C,$01                                         ;MazeWall267
        .db $6D,$01                                         ;MazeWall268
        .db $7D,$01                                         ;MazeWall269
        .db $7E,$03                                         ;MazeWall270
        .db $6A,$05                                         ;MazeWall2
        .db $00                                             

mztd50  .db $2C,$00,$0C,$07,$01                             ;MazeWall271
        .db $00                                             

mone50  .db $00                                             

mcp50a  .dw $0700,$F780                                     ;Position
        cann_pos(canp_bl,2,1,16)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $10
        cann_pau(5)                                         ;Pause = 5 frames
        cann_pos(canp_br,2,1,16)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $10
        cann_pau(5)                                         ;Pause = 5 frames
        cann_end                                            ;Loop It

mcp50b  .dw $0400,$F980                                     ;Position
        cann_pos(canp_br,2,1,10)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0A
        cann_pos(canp_br,2,1,10)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0A
        cann_pau(7)                                         ;Pause = 7 frames
        cann_pos(canp_tr,2,1,22)                            ;GunPos - TopRight Speed: 2 Shot: Yes ShotVel: $16
        cann_pau(7)                                         ;Pause = 7 frames
        cann_end                                            ;Loop It

tite50  .db $00                                             

lock50  .db $0C,$88,$89                                     ;Flash
        .db $01,$A9,$81                                     ;Blue
        .db $04,$8A,$BC                                     ;Red2
        .db $00                                             

tran50  .db (colflash+tr_left+tr_special),$67               ;Flash
        .db (colflash+tr_right+tr_hidden+tr_special),$B0    ;Flash
        .db $00                                             
        .db $7C,$01,$FC,$03                                 ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand50  .db $00                                             

mzty50  = $00
clock50 = $81
boot50  = $00
keyp50  = $00
outi50  = $30
reaz50  = $00
oxyb50  = $19
;********************************************************************
;  Dif 5 - Maze B - Level 22 - Level 22
;********************************************************************
mzdc51  .db $71,$B3,$9A,$99,$B5,$98,$75,$82,$B9             ;Oxygen
        .db $00                                             

mzlg51  .db $9F                                             ;HLightningH6
        .db $FF                                             
        .db $75                                             ;VLightningV69
        .db $9C                                             ;VLightningV70
        .db $8D                                             ;VLightningV71
        .db $86                                             ;VLightningV72
        .db $85                                             ;VLightningV2
        .db $93                                             ;VLightningV1
        .db $00                                             

mzar51  .db $00                                             

mzor51  .db $00                                             

mztr51  .db $00                                             

trpa51  .db $00,$00,$00                                     

mzta51  .db $48,$01                                         ;MazeWall272
        .db $4E,$06                                         ;MazeWall273
        .db $39,$06                                         ;MazeWall274
        .db $24,$02                                         ;MazeWall275
        .db $23,$01                                         ;MazeWall276
        .db $22,$01                                         ;MazeWall277
        .db $21,$01                                         ;MazeWall278
        .db $35,$01                                         ;MazeWall279
        .db $36,$01                                         ;MazeWall280
        .db $8B,$07                                         ;MazeWall281
        .db $6D,$03                                         ;MazeWall283
        .db $81,$04                                         ;MazeWall284
        .db $AE,$01                                         ;MazeWall286
        .db $AD,$01                                         ;MazeWall287
        .db $AF,$01                                         ;MazeWall288
        .db $B1,$01                                         ;MazeWall289
        .db $B0,$01                                         ;MazeWall290
        .db $B2,$01                                         ;MazeWall291
        .db $B3,$01                                         ;MazeWall292
        .db $B4,$01                                         ;MazeWall293
        .db $AB,$01                                         ;MazeWall295
        .db $AA,$04                                         ;MazeWall296
        .db $82,$01                                         ;MazeWall299
        .db $98,$01                                         ;MazeWall300
        .db $97,$01                                         ;MazeWall301
        .db $56,$05                                         ;MazeWall1
        .db $38,$01                                         ;MazeWall1
        .db $6E,$07                                         ;MazeWall1
        .db $96,$07                                         ;MazeWall1
        .db $00                                             

mztd51  .db $00                                             

mone51  .db $00                                             

mcp51a  .dw $0984,$FB80                                     ;Position
        cann_pos(canp_br,2,1,15)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0F
        cann_pau(3)                                         ;Pause = 3 frames
        cann_pos(canp_tr,2,1,15)                            ;GunPos - TopRight Speed: 2 Shot: Yes ShotVel: $0F
        cann_pau(3)                                         ;Pause = 3 frames
        cann_pos(canp_br,2,1,15)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0F
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,1,0,0)                             ;GunPos - TopRight Speed: 1 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,0,0,0)                             ;GunPos - TopRight Speed: 0 Shot: No
        cann_end                                            ;Loop It

mcp51b  .dw $0784,$F680                                     ;Position
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pau(1)                                         ;Pause = 1 frames
        cann_pos(canp_tr,0,0,0)                             ;GunPos - TopRight Speed: 0 Shot: No
        cann_pau(4)                                         ;Pause = 4 frames
        cann_end                                            ;Loop It

mcp51c  .dw $0B80,$F680                                     ;Position
        cann_pos(canp_tl,2,0,0)                             ;GunPos - TopLeft Speed: 2 Shot: No
        cann_pau(1)                                         ;Pause = 1 frames
        cann_pos(canp_bl,0,0,0)                             ;GunPos - BotLeft Speed: 0 Shot: No
        cann_pau(4)                                         ;Pause = 4 frames
        cann_end                                            ;Loop It

mcp51d  .dw $0C64,$FB80                                     ;Position
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pau(1)                                         ;Pause = 1 frames
        cann_pos(canp_tr,0,0,0)                             ;GunPos - TopRight Speed: 0 Shot: No
        cann_pau(4)                                         ;Pause = 4 frames
        cann_end                                            ;Loop It

tite51  .db $68                                             ;Spikes1
        .db $BA                                             ;Spikes2
        .db $64                                             ;Spikes3
        .db $55                                             ;Spikes1
        .db $59                                             ;Spikes2
        .db $00                                             

lock51  .db $06,$98,$5C                                     ;Yellow
        .db $05,$82,$7D                                     ;Purple
        .db $0F,$B6,$9B                                     ;Greenr
        .db $04,$50,$50                                     ;Red2
        .db $00                                             

tran51  .db (colcyan+tr_left),$73                           ;Cyan
        .db (colcyan+tr_right),$B7                          ;Cyan
        .db (colwhite+tr_left),$BD                          ;White
        .db (colwhite+tr_right),$63                         ;White
        .db (colflash+tr_right+tr_hidden+tr_special),$74    ;Flash
        .db (colflash+tr_left+tr_special),$6E               ;Flash
        .db $00                                             
        .db $00,$00,$FC,$03                                 ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand51  .db $00                                             

mzty51  = $01
clock51 = $B7
boot51  = $00
keyp51  = $8F
mpod51  = $00
outi51  = $10
reaz51  = $00
oxyb51  = $19
;********************************************************************
;  Dif 5 - Maze C - Level 23 - Level 23
;********************************************************************
mzdc52  .db $8A,$86,$69,$90,$65,$6D,$A9,$46,$8C,$8D,$6E     ;Oxygen
        .db $00                                             

mzlg52  .db $6C                                             ;HLightningH57
        .db $64                                             ;HLightningH58
        .db $FF                                             
        .db $83                                             ;VLightningV74
        .db $8C                                             ;VLightningV75
        .db $45                                             ;VLightningV79
        .db $6D                                             ;VLightningV1
        .db $B9                                             ;VLightningV1
        .db $00                                             

mzar52  .db $00                                             

mzor52  .db $00                                             

mztr52  .db $75                                             ;TripPad68
        .db $7A                                             ;TripPad69
        .db $59                                             ;TripPad70
        .db $56                                             ;TripPad71
        .db $00                                             

trpa52  .db $88,$F7,$82                                     ;TripPad68(pyroid)
        .db $89,$F7,$02                                     ;TripPad69(pyroid)
        .db $8D,$F5,$83                                     ;TripPad70(pyroid)
        .db $84,$F5,$03                                     ;TripPad71(pyroid)
        .db $00,$00,$00                                     

mzta52  .db $72,$06                                         ;MazeWall302
        .db $71,$06                                         ;MazeWall303
        .db $9B,$06                                         ;MazeWall304
        .db $5D,$06                                         ;MazeWall305
        .db $33,$02                                         ;MazeWall306
        .db $32,$05                                         ;MazeWall307
        .db $9C,$06                                         ;MazeWall308
        .db $87,$06                                         ;MazeWall309
        .db $86,$06                                         ;MazeWall310
        .db $B1,$04                                         ;MazeWall311
        .db $B0,$03                                         ;MazeWall312
        .db $B2,$01                                         ;MazeWall313
        .db $AF,$01                                         ;MazeWall314
        .db $B4,$01                                         ;MazeWall315
        .db $B6,$03                                         ;MazeWall316
        .db $9D,$04                                         ;MazeWall317
        .db $88,$05                                         ;MazeWall318
        .db $89,$01                                         ;MazeWall319
        .db $9A,$03                                         ;MazeWall320
        .db $85,$02                                         ;MazeWall321
        .db $84,$01                                         ;MazeWall322
        .db $8A,$02                                         ;MazeWall323
        .db $98,$06                                         ;MazeWall324
        .db $83,$05                                         ;MazeWall325
        .db $73,$02                                         ;MazeWall326
        .db $74,$04                                         ;MazeWall327
        .db $48,$06                                         ;MazeWall328
        .db $47,$03                                         ;MazeWall330
        .db $5C,$02                                         ;MazeWall331
        .db $B3,$01                                         ;MazeWall332
        .db $AE,$01                                         ;MazeWall333
        .db $B5,$01                                         ;MazeWall334
        .db $AC,$01                                         ;MazeWall336
        .db $AB,$04                                         ;MazeWall337
        .db $96,$02                                         ;MazeWall338
        .db $97,$07                                         ;MazeWall339
        .db $62,$02                                         ;MazeWall340
        .db $A0,$07                                         ;MazeWall341
        .db $9F,$06                                         ;MazeWall342
        .db $57,$01                                         ;MazeWall344
        .db $56,$05                                         ;MazeWall346
        .db $C8,$01                                         ;MazeWall347
        .db $C3,$04                                         ;MazeWall348
        .db $5F,$05                                         ;MazeWall349
        .db $82,$06                                         ;MazeWall350
        .db $8B,$06                                         ;MazeWall351
        .db $C9,$01                                         ;MazeWall353
        .db $6F,$01                                         ;MazeWall355
        .db $70,$03                                         ;MazeWall356
        .db $5B,$05                                         ;MazeWall357
        .db $C4,$01                                         ;MazeWall358
        .db $8E,$04                                         ;MazeWall359
        .db $A3,$01                                         ;MazeWall360
        .db $A1,$05                                         ;MazeWall7
        .db $CB,$03                                         ;MazeWall2
        .db $CA,$01                                         ;MazeWall6
        .db $5E,$07                                         ;MazeWall1
        .db $6D,$02                                         ;MazeWall2
        .db $76,$05                                         ;MazeWall3
        .db $6C,$05                                         ;MazeWall4
        .db $00                                             

mztd52  .db $75,$00,$06,$03,$02                             ;MazeWall361
        .db $6E,$00,$06,$04,$05                             ;MazeWall362
        .db $99,$00,$06,$04,$05                             ;MazeWall363
        .db $9E,$00,$06,$03,$02                             ;MazeWall364
        .db $00                                             

mone52  .db $00                                             

mcp52a  .dw $0900,$FA80                                     ;Position
        cann_pos(canp_dn,2,1,17)                            ;GunPos - Down Speed: 2 Shot: Yes ShotVel: $11
        cann_pau(1)                                         ;Pause = 1 frames
        cann_loc(4,4,0)                                     ;GunLoc - Frames: 4 XVel: 4 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pau(5)                                         ;Pause = 5 frames
        cann_pos(canp_dn,2,1,17)                            ;GunPos - Down Speed: 2 Shot: Yes ShotVel: $11
        cann_pau(1)                                         ;Pause = 1 frames
        cann_loc(4,-4,0)                                    ;GunLoc - Frames: 4 XVel: -4 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_loc(4,-4,0)                                    ;GunLoc - Frames: 4 XVel: -4 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pau(5)                                         ;Pause = 5 frames
        cann_pos(canp_dn,2,1,17)                            ;GunPos - Down Speed: 2 Shot: Yes ShotVel: $11
        cann_pau(1)                                         ;Pause = 1 frames
        cann_loc(4,4,0)                                     ;GunLoc - Frames: 4 XVel: 4 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pau(5)                                         ;Pause = 5 frames
        cann_end                                            ;Loop It

mcp52b  .dw $0EFC,$F980                                     ;Position
        cann_pos(canp_tl,0,1,8)                             ;GunPos - TopLeft Speed: 0 Shot: Yes ShotVel: $08
        cann_pos(canp_bl,2,1,16)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $10
        cann_pau(12)                                        ;Pause = 12 frames
        cann_pos(canp_tl,0,1,8)                             ;GunPos - TopLeft Speed: 0 Shot: Yes ShotVel: $08
        cann_pos(canp_bl,2,1,16)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $10
        cann_pau(12)                                        ;Pause = 12 frames
        cann_end                                            ;Loop It

tite52  .db $59                                             ;Spikes25
        .db $56                                             ;Spikes26
        .db $7A                                             ;Spikes27
        .db $75                                             ;Spikes28
        .db $49                                             ;Spikes29
        .db $00                                             

lock52  .db $03,$8B,$62                                     ;Cyan
        .db $0E,$6A,$97                                     ;Bluer
        .db $0A,$66,$9D                                     ;Orange
        .db $04,$40,$40                                     ;Red2
        .db $00                                             

tran52  .db (colwhiter+tr_left),$7D                         ;Whiter
        .db (colwhiter+tr_right),$61                        ;Whiter
        .db (colflash+tr_right+tr_hidden+tr_special),$B7    ;Flash
        .db (colflash+tr_left+tr_special),$6F               ;Flash
        .db (colcyanr+tr_left),$4C                          ;Cyanr
        .db (colcyanr+tr_right),$9A                         ;Cyanr
        .db $00                                             
        .db $00,$00,$FC,$03                                 ;Transportability Flags
        .db $EE                                             ;Transportability Flags

hand52  .db $00                                             

mzty52  = $02
clock52 = $00
boot52  = $61
keyp52  = $00
outi52  = $15
reaz52  = $00
oxyb52  = $18
;********************************************************************
;  Dif 6 - Maze D - Level 24 - Level 24
;********************************************************************
mzdc53  .db $5A,$77,$75,$61,$3A,$40,$51,$93,$31,$99,$46     ;Oxygen
        .db $00                                             

mzlg53  .db $57                                             ;HLightningH61
        .db $54                                             ;HLightningH62
        .db $55                                             ;HLightningH63
        .db $56                                             ;HLightningH64
        .db $58                                             ;HLightningH65
        .db $59                                             ;HLightningH66
        .db $5A                                             ;HLightningH67
        .db $FF                                             
        .db $45                                             ;VLightningV81
        .db $47                                             ;VLightningV82
        .db $71                                             ;VLightningV83
        .db $51                                             ;VLightningV84
        .db $A7                                             ;VLightningV86
        .db $36                                             ;VLightningV1
        .db $00                                             

mzar53  .db $00                                             

mzor53  .db $00                                             

mztr53  .db $00                                             

trpa53  .db $00,$00,$00                                     

mzta53  .db $7D,$07                                         ;MazeWall365
        .db $58,$07                                         ;MazeWall366
        .db $7C,$07                                         ;MazeWall367
        .db $7A,$07                                         ;MazeWall368
        .db $79,$07                                         ;MazeWall369
        .db $8D,$06                                         ;MazeWall370
        .db $8F,$06                                         ;MazeWall371
        .db $91,$06                                         ;MazeWall372
        .db $66,$07                                         ;MazeWall373
        .db $B4,$01                                         ;MazeWall375
        .db $A1,$07                                         ;MazeWall376
        .db $B5,$07                                         ;MazeWall377
        .db $90,$07                                         ;MazeWall378
        .db $8E,$07                                         ;MazeWall379
        .db $7E,$07                                         ;MazeWall380
        .db $66,$01                                         ;MazeWall381
        .db $67,$01                                         ;MazeWall382
        .db $6A,$01                                         ;MazeWall383
        .db $6B,$01                                         ;MazeWall384
        .db $6C,$02                                         ;MazeWall385
        .db $7F,$04                                         ;MazeWall386
        .db $64,$04                                         ;MazeWall387
        .db $78,$07                                         ;MazeWall388
        .db $77,$02                                         ;MazeWall389
        .db $51,$05                                         ;MazeWall390
        .db $76,$05                                         ;MazeWall391
        .db $9C,$01                                         ;MazeWall392
        .db $92,$05                                         ;MazeWall393
        .db $C4,$02                                         ;MazeWall394
        .db $54,$07                                         ;MazeWall395
        .db $53,$03                                         ;MazeWall396
        .db $55,$04                                         ;MazeWall397
        .db $9D,$01                                         ;MazeWall398
        .db $B1,$03                                         ;MazeWall399
        .db $B0,$07                                         ;MazeWall400
        .db $CA,$07                                         ;MazeWall402
        .db $B7,$07                                         ;MazeWall404
        .db $A4,$06                                         ;MazeWall405
        .db $2F,$01                                         ;MazeWall406
        .db $44,$07                                         ;MazeWall407
        .db $C9,$03                                         ;MazeWall408
        .db $C5,$04                                         ;MazeWall409
        .db $31,$07                                         ;MazeWall410
        .db $93,$01                                         ;MazeWall411
        .db $A7,$01                                         ;MazeWall412
        .db $CD,$01                                         ;MazeWall413
        .db $CE,$03                                         ;MazeWall414
        .db $BB,$06                                         ;MazeWall415
        .db $BA,$07                                         ;MazeWall416
        .db $A6,$05                                         ;MazeWall417
        .db $94,$03                                         ;MazeWall418
        .db $B8,$05                                         ;MazeWall419
        .db $DF,$03                                         ;MazeWall420
        .db $DE,$01                                         ;MazeWall421
        .db $CC,$05                                         ;MazeWall422
        .db $A0,$01                                         ;MazeWall423
        .db $8A,$03                                         ;MazeWall424
        .db $CB,$06                                         ;MazeWall2
        .db $9E,$03                                         ;MazeWall4
        .db $00                                             

mztd53  .db $B6,$00,$08,$06,$07                             ;MazeWall374
        .db $B2,$00,$08,$06,$07                             ;MazeWall403
        .db $9F,$00,$08,$05,$07                             ;MazeWall1
        .db $A3,$00,$08,$02,$07                             ;MazeWall2
        .db $00                                             

mone53  .db $B7                                             ;OneWay24(right)
        .db $FF                                             
        .db $5B                                             ;OneWay25(left)
        .db $00                                             

mcp53a  .dw $0400,$F880                                     ;Position
        cann_pos(canp_br,2,1,11)                            ;GunPos - BotRight Speed: 2 Shot: Yes ShotVel: $0B
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_tr,2,1,11)                            ;GunPos - TopRight Speed: 2 Shot: Yes ShotVel: $0B
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_tr,2,0,0)                             ;GunPos - TopRight Speed: 2 Shot: No
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pau(16)                                        ;Pause = 16 frames
        cann_end                                            ;Loop It

mcp53b  .dw $08B0,$F880                                     ;Position
        cann_pos(canp_bl,2,1,11)                            ;GunPos - BotLeft Speed: 2 Shot: Yes ShotVel: $0B
        cann_pau(4)                                         ;Pause = 4 frames
        cann_pos(canp_tl,2,1,11)                            ;GunPos - TopLeft Speed: 2 Shot: Yes ShotVel: $0B
        cann_pos(canp_bl,2,0,0)                             ;GunPos - BotLeft Speed: 2 Shot: No
        cann_pos(canp_tl,2,0,0)                             ;GunPos - TopLeft Speed: 2 Shot: No
        cann_pos(canp_bl,2,0,0)                             ;GunPos - BotLeft Speed: 2 Shot: No
        cann_pau(16)                                        ;Pause = 16 frames
        cann_end                                            ;Loop It

mcp53c  .dw $0684,$F980                                     ;Position
        cann_pos(canp_dn,0,0,0)                             ;GunPos - Down Speed: 0 Shot: No
        cann_pos(canp_dn,2,1,16)                            ;GunPos - Down Speed: 2 Shot: Yes ShotVel: $10
        cann_pos(canp_dn,2,1,16)                            ;GunPos - Down Speed: 2 Shot: Yes ShotVel: $10
        cann_pau(4)                                         ;Pause = 4 frames
        cann_loc(2,0,-16)                                   ;GunLoc - Frames: 2 XVel: 0 YVel: -16
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_pos(canp_bl,2,0,0)                             ;GunPos - BotLeft Speed: 2 Shot: No
        cann_pos(canp_dn,2,0,0)                             ;GunPos - Down Speed: 2 Shot: No
        cann_pau(1)                                         ;Pause = 1 frames
        cann_loc(2,0,16)                                    ;GunLoc - Frames: 2 XVel: 0 YVel: 16
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_end                                            ;Loop It

mcp53d  .dw $05A8,$F280                                     ;Position
        cann_pos(canp_br,2,0,0)                             ;GunPos - BotRight Speed: 2 Shot: No
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,12)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: 12
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,-12)                                  ;GunLoc - Frames: 1 XVel: 12 YVel: -12
        cann_loc(1,12,-6)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: -6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,12)                                   ;GunLoc - Frames: 1 XVel: 12 YVel: 12
        cann_loc(1,12,6)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 6
        cann_loc(1,12,0)                                    ;GunLoc - Frames: 1 XVel: 12 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_pos(canp_tl,2,0,0)                             ;GunPos - TopLeft Speed: 2 Shot: No
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,-12)                                 ;GunLoc - Frames: 1 XVel: -12 YVel: -12
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,12)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: 12
        cann_loc(1,-12,6)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,-12)                                 ;GunLoc - Frames: 1 XVel: -12 YVel: -12
        cann_loc(1,-12,-6)                                  ;GunLoc - Frames: 1 XVel: -12 YVel: -6
        cann_loc(1,-12,0)                                   ;GunLoc - Frames: 1 XVel: -12 YVel: 0
        cann_loc(0,0,0)                                     ;GunLoc - Frames: 0 XVel: 0 YVel: 0
        cann_end                                            ;Loop It

tite53  .db $76                                             ;Spikes30
        .db $78                                             ;Spikes31
        .db $74                                             ;Spikes32
        .db $00                                             

lock53  .db $04,$2F,$3B                                     ;Red2
        .db $0D,$7B,$4A                                     ;Cyanr
        .db $07,$41,$88                                     ;White
        .db $08,$94,$52                                     ;Whiter
        .db $00                                             

tran53  .db (colcyan+tr_left),$5C                           ;Cyan
        .db (colcyan+tr_right),$91                          ;Cyan
        .db (colflash+tr_right+tr_hidden+tr_special),$A5    ;Flash
        .db (colflash+tr_left+tr_special),$2F               ;Flash
        .db (colwhite+tr_left),$28                          ;White
        .db (colwhite+tr_right),$B6                         ;White
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand53  .db $03,$F5,$13,$04,$39,$05,$24,$0B,$02             ;Hand4

mzty53  = $03
clock53 = $00
boot53  = $2B
keyp53  = $00
outi53  = $26
reaz53  = $01
oxyb53  = $20
;********************************************************************
;  Dif 6 - Maze A - Level 25 - Level 25
;********************************************************************
mzdc60  .db $9C,$4C,$90,$25,$26,$27,$28,$66,$40,$68,$A3,$A9,$67,$65;Oxygen
        .db $00                                             

mzlg60  .db $37                                             ;HLightningH7
        .db $96                                             ;HLightningH8
        .db $36                                             ;HLightningH9
        .db $97                                             ;HLightningH10
        .db $98                                             ;HLightningH11
        .db $38                                             ;HLightningH1
        .db $FF                                             
        .db $2A                                             ;VLightningV95
        .db $B8                                             ;VLightningV6
        .db $B5                                             ;VLightningV7
        .db $00                                             

mzar60  .db $72,$03                                         ;Arrow1
        .db $61,$03                                         ;Arrow2
        .db $7B,$02                                         ;Arrow3
        .db $6A,$02                                         ;Arrow4
        .db $00                                             

mzor60  .db $00                                             

mztr60  .db $97                                             ;TripPad4
        .db $96                                             ;TripPad2
        .db $00                                             

trpa60  .db $0D,$F9,$85                                     ;TripPad4(pyroid)
        .db $02,$F9,$05                                     ;TripPad2(pyroid)
        .db $00,$00,$00                                     

mzta60  .db $91,$03                                         ;MazeWall487
        .db $6A,$01                                         ;MazeWall490
        .db $6C,$02                                         ;MazeWall491
        .db $A0,$01                                         ;MazeWall495
        .db $58,$01                                         ;MazeWall500
        .db $55,$01                                         ;MazeWall502
        .db $A1,$01                                         ;MazeWall507
        .db $78,$06                                         ;MazeWall508
        .db $65,$06                                         ;MazeWall509
        .db $56,$01                                         ;MazeWall526
        .db $CA,$01                                         ;MazeWall528
        .db $CB,$01                                         ;MazeWall529
        .db $C4,$02                                         ;MazeWall530
        .db $67,$01                                         ;MazeWall47
        .db $66,$05                                         ;MazeWall48
        .db $53,$01                                         ;MazeWall51
        .db $52,$05                                         ;MazeWall52
        .db $A4,$01                                         ;MazeWall53
        .db $6B,$01                                         ;MazeWall54
        .db $43,$01                                         ;MazeWall56
        .db $B5,$03                                         ;MazeWall59
        .db $79,$06                                         ;MazeWall70
        .db $7A,$07                                         ;MazeWall75
        .db $8D,$01                                         ;MazeWall79
        .db $7D,$01                                         ;MazeWall1
        .db $7E,$02                                         ;MazeWall2
        .db $7B,$01                                         ;MazeWall3
        .db $7C,$01                                         ;MazeWall4
        .db $46,$01                                         ;MazeWall2
        .db $C5,$05                                         ;MazeWall3
        .db $44,$03                                         ;MazeWall3
        .db $41,$04                                         ;MazeWall4
        .db $2E,$05                                         ;MazeWall5
        .db $2F,$01                                         ;MazeWall6
        .db $3F,$01                                         ;MazeWall14
        .db $64,$06                                         ;MazeWall15
        .db $77,$06                                         ;MazeWall2
        .db $7F,$06                                         ;MazeWall8
        .db $80,$06                                         ;MazeWall9
        .db $B1,$04                                         ;MazeWall13
        .db $B6,$04                                         ;MazeWall15
        .db $B3,$03                                         ;MazeWall16
        .db $5A,$07                                         ;MazeWall39
        .db $59,$07                                         ;MazeWall41
        .db $92,$06                                         ;MazeWall45
        .db $93,$06                                         ;MazeWall46
        .db $8C,$04                                         ;MazeWall47
        .db $A5,$03                                         ;MazeWall48
        .db $B8,$03                                         ;MazeWall49
        .db $B0,$07                                         ;MazeWall50
        .db $B9,$07                                         ;MazeWall51
        .db $A6,$06                                         ;MazeWall52
        .db $00                                             

mztd60  .db $00                                             

mone60  .db $33                                             ;OneWay4(right)
        .db $3A                                             ;OneWay1(right)
        .db $FF                                             
        .db $95                                             ;OneWay1(left)
        .db $FF                                             
        .db $98                                             ;OneWay3(left)
        .db $00                                             

tite60  .db $94                                             ;Spikes1
        .db $35                                             ;Spikes2
        .db $99                                             ;Spikes4
        .db $38                                             ;Spikes6
        .db $00                                             

lock60  .db $06,$69,$24                                     ;Yellow
        .db $00                                             

tran60  .db $00                                             
        .db $EE                                             ;Transportability Flags

hand60  .db $00                                             

mzty60  = $03
clock60 = $00
boot60  = $00
keyp60  = $00
outi60  = $12
reaz60  = $00
oxyb60  = $1C
;********************************************************************
;  Dif 6 - Maze B - Level 26 - Level 26
;********************************************************************
mzdc61  .db $98,$49,$95,$4C,$44,$6A,$41,$26,$25,$27,$28,$62,$66,$67;Oxygen
        .db $00                                             

mzlg61  .db $B6                                             ;HLightningH5
        .db $B7                                             ;HLightningH6
        .db $B8                                             ;HLightningH7
        .db $B9                                             ;HLightningH1
        .db $FF                                             
        .db $2A                                             ;VLightningV89
        .db $AB                                             ;VLightningV91
        .db $31                                             ;VLightningV92
        .db $5C                                             ;VLightningV8
        .db $51                                             ;VLightningV9
        .db $A2                                             ;VLightningV15
        .db $00                                             

mzar61  .db $A4,$01                                         ;Arrow5
        .db $A9,$00                                         ;Arrow6
        .db $34,$01                                         ;Arrow7
        .db $39,$00                                         ;Arrow8
        .db $00                                             

mzor61  .db $00                                             

mztr61  .db $00                                             

trpa61  .db $00,$00,$00                                     

mzta61  .db $66,$07                                         ;MazeWall428
        .db $B3,$01                                         ;MazeWall429
        .db $B4,$01                                         ;MazeWall430
        .db $43,$01                                         ;MazeWall437
        .db $46,$01                                         ;MazeWall438
        .db $B9,$01                                         ;MazeWall441
        .db $BA,$01                                         ;MazeWall442
        .db $47,$01                                         ;MazeWall443
        .db $55,$02                                         ;MazeWall444
        .db $5A,$02                                         ;MazeWall445
        .db $56,$05                                         ;MazeWall447
        .db $44,$01                                         ;MazeWall448
        .db $B0,$01                                         ;MazeWall452
        .db $3F,$01                                         ;MazeWall453
        .db $3E,$01                                         ;MazeWall454
        .db $51,$05                                         ;MazeWall456
        .db $AF,$01                                         ;MazeWall457
        .db $CB,$01                                         ;MazeWall459
        .db $CA,$01                                         ;MazeWall460
        .db $C5,$05                                         ;MazeWall461
        .db $C4,$02                                         ;MazeWall462
        .db $59,$05                                         ;MazeWall463
        .db $52,$02                                         ;MazeWall464
        .db $91,$07                                         ;MazeWall468
        .db $92,$04                                         ;MazeWall471
        .db $8B,$03                                         ;MazeWall473
        .db $8A,$04                                         ;MazeWall474
        .db $7E,$07                                         ;MazeWall482
        .db $79,$07                                         ;MazeWall483
        .db $6B,$07                                         ;MazeWall12
        .db $9C,$01                                         ;MazeWall6
        .db $A5,$01                                         ;MazeWall7
        .db $A4,$01                                         ;MazeWall8
        .db $69,$07                                         ;MazeWall53
        .db $68,$07                                         ;MazeWall54
        .db $A2,$07                                         ;MazeWall55
        .db $A1,$07                                         ;MazeWall56
        .db $93,$03                                         ;MazeWall57
        .db $9D,$01                                         ;MazeWall60
        .db $9E,$01                                         ;MazeWall61
        .db $A6,$01                                         ;MazeWall62
        .db $A7,$01                                         ;MazeWall63
        .db $A0,$01                                         ;MazeWall64
        .db $8D,$04                                         ;MazeWall65
        .db $90,$03                                         ;MazeWall66
        .db $58,$07                                         ;MazeWall67
        .db $53,$07                                         ;MazeWall68
        .db $7D,$05                                         ;MazeWall71
        .db $7A,$02                                         ;MazeWall72
        .db $7C,$07                                         ;MazeWall75
        .db $31,$07                                         ;MazeWall76
        .db $30,$07                                         ;MazeWall77
        .db $00                                             

mztd61  .db $6D,$20,$10,$07,$06                             ;MazeWall440
        .db $64,$20,$10,$07,$06                             ;MazeWall458
        .db $78,$10,$20,$06,$07                             ;MazeWall469
        .db $7F,$10,$20,$06,$07                             ;MazeWall470
        .db $6C,$10,$20,$06,$07                             ;MazeWall480
        .db $65,$10,$20,$06,$07                             ;MazeWall481
        .db $77,$20,$10,$07,$06                             ;MazeWall1
        .db $80,$20,$10,$07,$06                             ;MazeWall2
        .db $54,$00,$0C,$05,$07                             ;MazeWall69
        .db $57,$00,$0C,$02,$07                             ;MazeWall70
        .db $67,$00,$0C,$06,$07                             ;MazeWall73
        .db $6A,$00,$0C,$06,$07                             ;MazeWall74
        .db $00                                             

mone61  .db $00                                             

tite61  .db $98                                             ;Spikes15
        .db $95                                             ;Spikes16
        .db $9A                                             ;Spikes3
        .db $93                                             ;Spikes4
        .db $00                                             

lock61  .db $06,$4D,$35                                     ;Yellow
        .db $0B,$41,$38                                     ;Red
        .db $00                                             

tran61  .db (colred2+tr_left),$3B                           ;Red2
        .db (colred2+tr_right),$91                          ;Red2
        .db (colyellow+tr_right),$A3                        ;Yellow
        .db (colyellow+tr_left),$9C                         ;Yellow
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand61  .db $00                                             

mzty61  = $03
clock61 = $00
boot61  = $AB
keyp61  = $00
mpod61  = $00
outi61  = $20
reaz61  = $00
oxyb61  = $10
;********************************************************************
;  Dif 6 - Maze C - Level 27 - Level 27
;********************************************************************
mzdc62  .db $27,$26,$54,$69,$58,$63,$4C,$40,$93,$95,$A4,$A9,$98,$9A,$28,$25;Oxygen
        .db $00                                             

mzlg62  .db $6C                                             ;HLightningH85
        .db $84                                             ;HLightningH6
        .db $85                                             ;HLightningH7
        .db $8A                                             ;HLightningH8
        .db $89                                             ;HLightningH9
        .db $62                                             ;HLightningH1
        .db $FF                                             
        .db $2A                                             ;VLightningV108
        .db $00                                             

mzar62  .db $81,$07                                         ;Arrow9
        .db $8B,$05                                         ;Arrow10
        .db $41,$04                                         ;Arrow11
        .db $4B,$06                                         ;Arrow12
        .db $00                                             

mzor62  .db $00                                             

mztr62  .db $84                                             ;TripPad2
        .db $89                                             ;TripPad4
        .db $00                                             

trpa62  .db $89,$F8,$85                                     ;TripPad2(pyroid)
        .db $86,$F8,$05                                     ;TripPad4(pyroid)
        .db $00,$00,$00                                     

mzta62  .db $CA,$01                                         ;MazeWall603
        .db $CB,$01                                         ;MazeWall604
        .db $6C,$01                                         ;MazeWall606
        .db $6D,$02                                         ;MazeWall607
        .db $B3,$01                                         ;MazeWall608
        .db $5A,$03                                         ;MazeWall609
        .db $53,$01                                         ;MazeWall611
        .db $51,$04                                         ;MazeWall612
        .db $59,$01                                         ;MazeWall613
        .db $80,$06                                         ;MazeWall614
        .db $A5,$01                                         ;MazeWall616
        .db $B4,$03                                         ;MazeWall621
        .db $B5,$04                                         ;MazeWall622
        .db $42,$02                                         ;MazeWall624
        .db $2B,$01                                         ;MazeWall625
        .db $3D,$03                                         ;MazeWall626
        .db $3E,$05                                         ;MazeWall627
        .db $52,$01                                         ;MazeWall629
        .db $47,$02                                         ;MazeWall631
        .db $58,$01                                         ;MazeWall632
        .db $34,$01                                         ;MazeWall633
        .db $35,$02                                         ;MazeWall634
        .db $48,$04                                         ;MazeWall635
        .db $8E,$06                                         ;MazeWall636
        .db $8F,$06                                         ;MazeWall637
        .db $7B,$06                                         ;MazeWall638
        .db $68,$02                                         ;MazeWall639
        .db $7C,$06                                         ;MazeWall640
        .db $69,$05                                         ;MazeWall641
        .db $67,$01                                         ;MazeWall642
        .db $66,$01                                         ;MazeWall643
        .db $6A,$01                                         ;MazeWall644
        .db $6B,$01                                         ;MazeWall645
        .db $8B,$04                                         ;MazeWall646
        .db $93,$06                                         ;MazeWall647
        .db $A6,$03                                         ;MazeWall648
        .db $A4,$01                                         ;MazeWall649
        .db $A3,$04                                         ;MazeWall650
        .db $90,$06                                         ;MazeWall653
        .db $7D,$05                                         ;MazeWall654
        .db $7A,$02                                         ;MazeWall655
        .db $7F,$02                                         ;MazeWall657
        .db $92,$03                                         ;MazeWall658
        .db $2A,$05                                         ;MazeWall659
        .db $91,$07                                         ;MazeWall660
        .db $55,$03                                         ;MazeWall661
        .db $41,$07                                         ;MazeWall664
        .db $43,$05                                         ;MazeWall666
        .db $9E,$01                                         ;MazeWall13
        .db $9D,$04                                         ;MazeWall14
        .db $A0,$03                                         ;MazeWall15
        .db $8D,$06                                         ;MazeWall16
        .db $3F,$07                                         ;MazeWall4
        .db $A2,$06                                         ;MazeWall9
        .db $A1,$06                                         ;MazeWall10
        .db $36,$07                                         ;MazeWall6
        .db $31,$01                                         ;MazeWall1
        .db $B9,$01                                         ;MazeWall3
        .db $B0,$01                                         ;MazeWall2
        .db $46,$07                                         ;MazeWall24
        .db $44,$07                                         ;MazeWall25
        .db $00                                             

mztd62  .db $00                                             

mone62  .db $88                                             ;OneWay6(right)
        .db $FF                                             
        .db $85                                             ;OneWay5(left)
        .db $00                                             

tite62  .db $74                                             ;Spikes5
        .db $79                                             ;Spikes6
        .db $32                                             ;Spikes1
        .db $3B                                             ;Spikes2
        .db $00                                             

lock62  .db $04,$6A,$B7                                     ;Red2
        .db $06,$64,$29                                     ;Yellow
        .db $00                                             

tran62  .db (colred2+tr_right),$24                          ;Red2
        .db (colred2+tr_left),$B8                           ;Red2
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand62  .db $00                                             

mzty62  = $03
clock62 = $00
boot62  = $00
keyp62  = $00
outi62  = $10
reaz62  = $00
oxyb62  = $1C
;********************************************************************
;  Dif 7 - Maze D - Level 28 - Level 28
;********************************************************************
mzdc63  .db $26,$27,$39,$92,$34,$5B,$43,$83,$52,$4A,$8A,$9B,$59,$54,$28,$25;Oxygen
        .db $00                                             

mzlg63  .db $36                                             ;HLightningH3
        .db $38                                             ;HLightningH4
        .db $37                                             ;HLightningH1
        .db $64                                             ;HLightningH1
        .db $6A                                             ;HLightningH2
        .db $FF                                             
        .db $58                                             ;VLightningV1
        .db $55                                             ;VLightningV3
        .db $8C                                             ;VLightningV4
        .db $81                                             ;VLightningV5
        .db $2A                                             ;VLightningV1
        .db $A2                                             ;VLightningV2
        .db $AB                                             ;VLightningV1
        .db $00                                             

mzar63  .db $00                                             

mzor63  .db $00                                             

mztr63  .db $33                                             ;TripPad4
        .db $3A                                             ;TripPad5
        .db $36                                             ;TripPad1
        .db $37                                             ;TripPad2
        .db $00                                             

trpa63  .db $0A,$F3,$85                                     ;TripPad4(pyroid)
        .db $05,$F3,$05                                     ;TripPad5(pyroid)
        .db $82,$F3,$06                                     ;TripPad1(pyroid)
        .db $82,$F3,$06                                     ;TripPad2(pyroid)
        .db $00,$00,$00                                     

mzta63  .db $44,$01                                         ;MazeWall542
        .db $3F,$01                                         ;MazeWall543
        .db $47,$01                                         ;MazeWall545
        .db $CA,$01                                         ;MazeWall546
        .db $CB,$01                                         ;MazeWall547
        .db $55,$05                                         ;MazeWall552
        .db $56,$02                                         ;MazeWall553
        .db $9F,$05                                         ;MazeWall556
        .db $57,$07                                         ;MazeWall558
        .db $54,$07                                         ;MazeWall559
        .db $43,$01                                         ;MazeWall561
        .db $9E,$03                                         ;MazeWall572
        .db $B2,$03                                         ;MazeWall579
        .db $B7,$04                                         ;MazeWall580
        .db $79,$06                                         ;MazeWall581
        .db $7E,$06                                         ;MazeWall582
        .db $7A,$06                                         ;MazeWall593
        .db $7D,$06                                         ;MazeWall594
        .db $3E,$01                                         ;MazeWall27
        .db $64,$02                                         ;MazeWall31
        .db $8A,$03                                         ;MazeWall33
        .db $6D,$05                                         ;MazeWall34
        .db $93,$04                                         ;MazeWall35
        .db $66,$06                                         ;MazeWall37
        .db $65,$07                                         ;MazeWall41
        .db $6B,$06                                         ;MazeWall44
        .db $5B,$01                                         ;MazeWall58
        .db $5A,$05                                         ;MazeWall59
        .db $50,$01                                         ;MazeWall60
        .db $51,$02                                         ;MazeWall61
        .db $9D,$05                                         ;MazeWall4
        .db $A0,$01                                         ;MazeWall10
        .db $B4,$07                                         ;MazeWall12
        .db $B5,$07                                         ;MazeWall13
        .db $B6,$07                                         ;MazeWall14
        .db $68,$03                                         ;MazeWall6
        .db $69,$04                                         ;MazeWall7
        .db $6A,$02                                         ;MazeWall8
        .db $7B,$02                                         ;MazeWall9
        .db $7C,$05                                         ;MazeWall10
        .db $46,$01                                         ;MazeWall11
        .db $7F,$02                                         ;MazeWall14
        .db $91,$04                                         ;MazeWall4
        .db $8C,$03                                         ;MazeWall5
        .db $8B,$05                                         ;MazeWall6
        .db $8D,$06                                         ;MazeWall1
        .db $90,$06                                         ;MazeWall2
        .db $59,$07                                         ;MazeWall10
        .db $52,$07                                         ;MazeWall11
        .db $31,$07                                         ;MazeWall2
        .db $30,$07                                         ;MazeWall3
        .db $00                                             

mztd63  .db $80,$00,$08,$06,$07                             ;MazeWall574
        .db $8F,$00,$08,$06,$07                             ;MazeWall585
        .db $8E,$00,$08,$06,$07                             ;MazeWall586
        .db $A1,$00,$08,$03,$07                             ;MazeWall5
        .db $A2,$00,$08,$04,$07                             ;MazeWall6
        .db $77,$00,$08,$06,$07                             ;MazeWall1
        .db $58,$00,$08,$05,$07                             ;MazeWall8
        .db $53,$00,$08,$02,$07                             ;MazeWall9
        .db $00                                             

mone63  .db $96                                             ;OneWay4(right)
        .db $35                                             ;OneWay1(right)
        .db $38                                             ;OneWay2(right)
        .db $00                                             

tite63  .db $9A                                             ;Spikes1
        .db $93                                             ;Spikes2
        .db $00                                             

lock63  .db $06,$78,$26                                     ;Yellow
        .db $0B,$43,$97                                     ;Red
        .db $0A,$76,$27                                     ;Orange
        .db $04,$4B,$32                                     ;Red2
        .db $00                                             

tran63  .db (colred+tr_right),$91                           ;Red
        .db (colred+tr_left),$AA                            ;Red
        .db (colyellow+tr_right),$24                        ;Yellow
        .db (colyellow+tr_left),$9C                         ;Yellow
        .db $00                                             
        .db $EE                                             ;Transportability Flags

hand63  .db $00                                             

mzty63  = $03
clock63 = $00
boot63  = $A3
keyp63  = $00
outi63  = $20
reaz63  = $00
oxyb63  = $1C
