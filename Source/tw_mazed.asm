;**************** ***********************************
    .TEXT ".TWMAZED."
;***************************************************
    .title "TWMAZED - Maze Data"
;***************************************************
        
;* Init Source Disc Data *
mzdc    .word   mzdc00,mzdc01,mzdc02,mzdc03
        .word   mzdc10,mzdc11,mzdc12,mzdc13
        .word   mzdc20,mzdc21,mzdc22,mzdc23
        .word   mzdc30,mzdc31,mzdc32,mzdc33
        .word   mzdc40,mzdc41,mzdc42,mzdc43
        .word   mzdc50,mzdc51,mzdc52,mzdc53
        .word   mzdc60,mzdc61,mzdc62,mzdc63
        
;* Init Source Lightning Data *
mzlg    .word   mzlg00,mzlg01,mzlg02,mzlg03
        .word   mzlg10,mzlg11,mzlg12,mzlg13
        .word   mzlg20,mzlg21,mzlg22,mzlg23
        .word   mzlg30,mzlg31,mzlg32,mzlg33
        .word   mzlg40,mzlg41,mzlg42,mzlg43
        .word   mzlg50,mzlg51,mzlg52,mzlg53
        .word   mzlg60,mzlg61,mzlg62,mzlg63

;* Init Source Arrow Data *
mzar    .word   mzar00,mzar01,mzar02,mzar03
        .word   mzar10,mzar11,mzar12,mzar13
        .word   mzar20,mzar21,mzar22,mzar23
        .word   mzar30,mzar31,mzar32,mzar33
        .word   mzar40,mzar41,mzar42,mzar43
        .word   mzar50,mzar51,mzar52,mzar53
        .word   mzar60,mzar61,mzar62,mzar63
        
;* Init Source Exit Arrow Data *
mzor    .word   mzor00,mzor01,mzor02,mzor03
		.word   mzor10,mzor11,mzor12,mzor13
		.word   mzor20,mzor21,mzor22,mzor23
		.word   mzor30,mzor31,mzor32,mzor33
		.word   mzor40,mzor41,mzor42,mzor43
		.word   mzor50,mzor51,mzor52,mzor53
		.word   mzor60,mzor61,mzor62,mzor63

;* Init Source Trip Point Data *
mztr    .word   mztr00,mztr01,mztr02,mztr03
        .word   mztr10,mztr11,mztr12,mztr13
        .word   mztr20,mztr21,mztr22,mztr23
        .word   mztr30,mztr31,mztr32,mztr33
        .word   mztr40,mztr41,mztr42,mztr43
        .word   mztr50,mztr51,mztr52,mztr53
        .word   mztr60,mztr61,mztr62,mztr63

;* Init Source Maze Adjustments *
mztdal  .word   mzta00,mzta01,mzta02,mzta03
        .word   mzta10,mzta11,mzta12,mzta13
        .word   mzta20,mzta21,mzta22,mzta23
        .word   mzta30,mzta31,mzta32,mzta33
        .word   mzta40,mzta41,mzta42,mzta43
        .word   mzta50,mzta51,mzta52,mzta53
        .word   mzta60,mzta61,mzta62,mzta63

;* Init Source Dynamic Maze Adjustments *
mztd    .word   mztd00,mztd01,mztd02,mztd03
        .word   mztd10,mztd11,mztd12,mztd13
        .word   mztd20,mztd21,mztd22,mztd23
        .word   mztd30,mztd31,mztd32,mztd33
        .word   mztd40,mztd41,mztd42,mztd43
        .word   mztd50,mztd51,mztd52,mztd53
        .word   mztd60,mztd61,mztd62,mztd63
        
;* Init Source One Way Walls *
mone    .word   mone00,mone01,mone02,mone03
        .word   mone10,mone11,mone12,mone13
        .word   mone20,mone21,mone22,mone23
        .word   mone30,mone31,mone32,mone33
        .word   mone40,mone41,mone42,mone43
        .word   mone50,mone51,mone52,mone53
        .word   mone60,mone61,mone62,mone63
        
;* Init Source Stalactites *
mtite   .word   tite00,tite01,tite02,tite03
        .word   tite10,tite11,tite12,tite13
        .word   tite20,tite21,tite22,tite23
        .word   tite30,tite31,tite32,tite33
        .word   tite40,tite41,tite42,tite43
        .word   tite50,tite51,tite52,tite53
        .word   tite60,tite61,tite62,tite63
        
;* Init Source Locks *
mlock   .word   lock00,lock01,lock02,lock03     
        .word   lock10,lock11,lock12,lock13
        .word   lock20,lock21,lock22,lock23
        .word   lock30,lock31,lock32,lock33
        .word   lock40,lock41,lock42,lock43
        .word   lock50,lock51,lock52,lock53
        .word   lock60,lock61,lock62,lock63

;* Init Source Transporter *
mtran   .word   tran00,tran01,tran02,tran03 
        .word   tran10,tran11,tran12,tran13
        .word   tran20,tran21,tran22,tran23
        .word   tran30,tran31,tran32,tran33
        .word   tran40,tran41,tran42,tran43
        .word   tran50,tran51,tran52,tran53
        .word   tran60,tran61,tran62,tran63
        
;* Init Source De Hand *
mhand   .word   hand00,hand01,hand02,hand03
        .word   hand10,hand11,hand12,hand13
        .word   hand20,hand21,hand22,hand23
        .word   hand30,hand31,hand32,hand33
        .word   hand40,hand41,hand42,hand43 
        .word   hand50,hand51,hand52,hand53
        .word   hand60,hand61,hand62,hand63   	
		
;* Maze Type
mzty   	.byte   mzty00,mzty01,mzty02,mzty03
        .byte   mzty10,mzty11,mzty12,mzty13
        .byte   mzty20,mzty21,mzty22,mzty23
        .byte   mzty30,mzty31,mzty32,mzty33
        .byte   mzty40,mzty41,mzty42,mzty43 
        .byte   mzty50,mzty51,mzty52,mzty53
        .byte   mzty60,mzty61,mzty62,mzty63   
		
;*********************************************************
; Flag to define if the maze is a star castle level
;*********************************************************
mscstl	.byte 	0,0,0,0
		.byte 	0,0,0,0
		.byte 	0,0,0,0
		.byte 	0,0,0,0
		.byte 	0,0,0,0
		.byte 	1,1,1,1
		.byte 	0,0,0,0
       
;**************************************************
    .sbttl "Open/Closed Data Locations"
;**************************************************
mzocd       .word   m0cld,m1cld,m2cld,m3cld

;*** Maze Index Tables ***
;* Source Data Pointers  *
mazsrc  .word   maz0,maz1,maz2,maz3

;**********************************************
    .sbttl "New Maze Objects"
;**********************************************
;* First comes clock and magic boots data     *
;*     Two Bytes for each wave                *
;*     0 in first byte means not present      *
;*     Otherwise X MSB and Y MSB respectively *
;**********************************************
mclock  .db clock00,clock01,clock02,clock03
        .db clock10,clock11,clock12,clock13
        .db clock20,clock21,clock22,clock23
        .db clock30,clock31,clock32,clock33
		.db clock40,clock41,clock42,clock43
		.db clock50,clock51,clock52,clock53
		.db clock60,clock61,clock62,clock63
        
mboots  .db boot00,boot01,boot02,boot03
        .db boot10,boot11,boot12,boot13
        .db boot20,boot21,boot22,boot23
        .db boot30,boot31,boot32,boot33
        .db boot40,boot41,boot42,boot43
        .db boot50,boot51,boot52,boot53
        .db boot60,boot61,boot62,boot63
		
mkeyp	.db keyp00,keyp01,keyp02,keyp03
        .db keyp10,keyp11,keyp12,keyp13
        .db keyp20,keyp21,keyp22,keyp23
        .db keyp30,keyp31,keyp32,keyp33
        .db keyp40,keyp41,keyp42,keyp43
        .db keyp50,keyp51,keyp52,keyp53
        .db keyp60,keyp61,keyp62,keyp63

;******************************************************
    .sbttl "Escape Pod Data"      
;******************************************************
;* 0 Means not on this wave                           
;* 1 Means shows up but player doesn't have to use it 
;* 2 Means show up and doors don't open               
;******************************************************
mpod    .db 0,1,2,mpod31,mpod41,mpod51,mpod61

;******************************************************
    .sbttl "Reactor Time Table"
;******************************************************
outime  .db outi00,outi01,outi02,outi03
        .db outi10,outi11,outi12,outi13
        .db outi20,outi21,outi22,outi23
		.db outi30,outi31,outi32,outi33
		.db outi40,outi41,outi42,outi43
		.db outi50,outi51,outi52,outi53
		.db outi60,outi61,outi62,outi63

;******************************************************
    .sbttl "Reactor Size Table"
;******************************************************
reacsz  .db reaz00,reaz01,reaz02,reaz03
        .db reaz10,reaz11,reaz12,reaz13
        .db reaz20,reaz21,reaz22,reaz23
		.db reaz30,reaz31,reaz32,reaz33
		.db reaz40,reaz41,reaz42,reaz43
		.db reaz50,reaz51,reaz52,reaz53
		.db reaz60,reaz61,reaz62,reaz63		
		
;******************************************************
	.sbttl "Oxygen Award Table"
;******************************************************
oxybonus
		.db oxyb00,oxyb01,oxyb02,oxyb03
		.db oxyb10,oxyb11,oxyb12,oxyb13
		.db oxyb20,oxyb21,oxyb22,oxyb23
		.db oxyb30,oxyb31,oxyb32,oxyb33
		.db oxyb40,oxyb41,oxyb42,oxyb43
		.db oxyb50,oxyb51,oxyb52,oxyb53
		.db oxyb60,oxyb61,oxyb62,oxyb63

;******************************************************
	.sbttl "Trip Point Action Table"
;******************************************************
trtbll  .word trpa00,trpa01,trpa02,trpa03
        .word trpa10,trpa11,trpa12,trpa13
        .word trpa20,trpa21,trpa22,trpa23
        .word trpa30,trpa31,trpa32,trpa33
        .word trpa40,trpa41,trpa42,trpa43
        .word trpa50,trpa51,trpa52,trpa53
        .word trpa60,trpa61,trpa62,trpa63
        
;Speed of Trip Point Sparkoids, x8 (1 block for each Difficulty Level)    
ispeed  .byte $02,$05,$07,$0A,$0E,$12,$15,$1B
        .byte $03,$06,$08,$0B,$10,$13,$18,$1F
        .byte $03,$07,$09,$0D,$12,$16,$1B,$22
        .byte $04,$08,$0A,$10,$14,$18,$1F,$00
        .byte $04,$09,$0B,$11,$16,$1F,$22,$00
 

;***********************************************
    .sbttl "Maze 0 Data"
;***********************************************
maz0        
m0ua    .byte $47,$47,$00
m0ub    .byte $47,$47,$00
m0uc    .byte $47,$47,$00
m0ud    .byte $47,$47,$00
m0ue    .byte $47,$47,$00
m0uf    .byte $47,$47,$00
m0u1    .byte $47,$47,$47,$47,$47,$47,$42,$45,$41,$41,$41,$41,$42,$47,$47,$00
m0u2    .byte $47,$47,$47,$47,$47,$45,$43,$04,$07,$01,$01,$02,$44,$42,$47,$00
m0u3    .byte $47,$47,$47,$47,$47,$46,$05,$01,$01,$07,$02,$06,$07,$46,$47,$00
m0u4    .byte $47,$47,$47,$47,$45,$43,$06,$07,$01,$01,$03,$07,$06,$44,$42,$00
m0u5    .byte $47,$47,$47,$47,$04,$02,$04,$01,$02,$07,$01,$01,$03,$05,$03,$00
m0u6    .byte $47,$47,$47,$47,$47,$04,$01,$02,$04,$01,$07,$05,$01,$03,$47,$00
m0u7    .byte $47,$47,$47,$47,$47,$47,$47,$04,$01,$01,$01,$03,$47,$47,$07,$00
m0u8
m0ulast	.byte $80

m0cld   .byte $28       ;Offset into RAM change place
        .byte $41,$03   ;Closed Data
        .byte $43,$04   ;Open Data

;***********************************************
    .sbttl "Maze 1 Data"
;***********************************************
maz1
m1ua    .byte $47,$47,$00
m1ub    .byte $47,$47,$00
m1uc    .byte $47,$47,$00
m1ud    .byte $47,$47,$00
m1ue    .byte $47,$47,$00
m1uf    .byte $47,$47,$00
m1u1    .byte $47,$47,$47,$47,$47,$47,$47,$45,$42,$45,$41,$41,$41,$41,$41,$42,$47,$47,$47,$47,$00
m1u2    .byte $47,$47,$47,$47,$47,$47,$45,$43,$46,$46,$05,$01,$01,$01,$07,$44,$41,$42,$47,$47,$00   
m1u3    .byte $47,$47,$47,$47,$45,$41,$43,$01,$46,$46,$06,$05,$07,$01,$01,$01,$02,$44,$42,$45,$00
m1u4    .byte $47,$47,$47,$47,$46,$07,$01,$02,$46,$46,$06,$04,$01,$01,$01,$02,$04,$02,$06,$46,$00
m1u5    .byte $47,$47,$47,$47,$46,$05,$02,$06,$46,$46,$04,$01,$01,$01,$07,$06,$05,$03,$06,$46,$00
m1u6    .byte $47,$47,$47,$47,$46,$04,$06,$04,$01,$01,$01,$01,$01,$01,$01,$03,$03,$05,$03,$46,$00
m1u7    .byte $47,$47,$47,$47,$04,$02,$04,$07,$05,$01,$01,$01,$01,$01,$01,$02,$05,$03,$05,$03,$00
m1u8    .byte $47,$47,$47,$47,$47,$46,$05,$01,$03,$07,$07,$07,$07,$07,$07,$04,$01,$01,$03,$47,$00
m1u9    
m1ulast .byte $80

m1cld   .byte $6E       ;RAM offset Location
        .byte $44,$43   ;Closed data
        .byte $46,$46   ;Open data       
        
;***********************************************
    .sbttl "Maze 2 Data"
;***********************************************
maz2
m2ua    .byte $47,$47,$00
m2ub    .byte $47,$47,$00
m2uc    .byte $47,$47,$00
m2ud    .byte $47,$47,$00
m2ue    .byte $47,$47,$00
m2uf    .byte $47,$47,$00
m2u1    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$42,$45,$41,$41,$41,$42,$47,$47,$47,$47,$47,$00 
m2u2    .byte $47,$47,$47,$47,$47,$47,$47,$45,$41,$43,$04,$07,$01,$02,$44,$41,$42,$47,$47,$47,$00
m2u3    .byte $47,$47,$47,$47,$45,$41,$41,$43,$05,$01,$01,$01,$02,$04,$01,$07,$44,$41,$42,$47,$00
m2u4    .byte $47,$47,$47,$47,$46,$07,$05,$01,$03,$05,$01,$07,$04,$01,$01,$01,$01,$07,$44,$42,$00
m2u5    .byte $47,$47,$47,$47,$46,$06,$04,$07,$01,$03,$05,$01,$01,$01,$02,$05,$01,$01,$02,$46,$00
m2u6    .byte $47,$47,$47,$47,$46,$04,$01,$01,$01,$02,$04,$07,$01,$01,$03,$05,$01,$01,$03,$46,$00
m2u7    .byte $47,$47,$47,$47,$04,$01,$01,$02,$07,$03,$05,$01,$07,$05,$01,$03,$05,$01,$01,$03,$00
m2u8    .byte $47,$47,$47,$47,$47,$47,$47,$04,$01,$02,$06,$07,$01,$03,$05,$02,$06,$47,$47,$47,$00
m2u9    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$06,$04,$01,$01,$01,$03,$47,$47,$47,$47,$47,$00
m2u10   
m2ulast	.byte $80

m2cld   .byte $30       ;Offset Location
        .byte $41,$03   ;Closed data
        .byte $43,$04   ;Open data
        
;***********************************************
    .sbttl "Maze 3 Data"
;***********************************************        
maz3
m3ua    .byte $47,$47,$00
m3ub    .byte $47,$47,$00
m3uc    .byte $47,$47,$00
m3ud    .byte $47,$47,$00
m3ue    .byte $47,$47,$00
m3uf    .byte $47,$47,$00
m3u1    .byte $47,$47,$47,$47,$47,$47,$47,$47,$42,$45,$41,$41,$41,$42,$47,$47,$47,$47,$00
m3u2    .byte $47,$47,$47,$47,$47,$47,$45,$41,$43,$04,$07,$01,$02,$44,$41,$42,$47,$47,$00
m3u3    .byte $47,$47,$47,$47,$45,$41,$43,$05,$01,$01,$01,$02,$04,$01,$02,$44,$41,$42,$00
m3u4    .byte $47,$47,$47,$47,$04,$02,$07,$03,$05,$01,$07,$04,$01,$02,$04,$02,$05,$03,$00
m3u5    .byte $47,$47,$47,$47,$47,$46,$05,$01,$03,$05,$01,$01,$07,$04,$07,$06,$46,$47,$00
m3u6    .byte $47,$47,$47,$47,$47,$46,$06,$05,$01,$03,$07,$01,$01,$01,$01,$03,$46,$47,$00
m3u7    .byte $47,$47,$47,$47,$47,$46,$06,$06,$07,$01,$01,$01,$01,$01,$02,$07,$46,$47,$00
m3u8    .byte $47,$47,$47,$47,$45,$43,$06,$04,$01,$07,$05,$01,$01,$02,$04,$02,$44,$42,$00
m3u9    .byte $47,$47,$47,$47,$04,$02,$04,$01,$01,$07,$04,$01,$01,$01,$01,$03,$05,$03,$00
m3u10   .byte $47,$47,$47,$47,$47,$04,$01,$01,$01,$01,$01,$01,$01,$02,$05,$01,$03,$47,$00
m3u11   .byte $47,$47,$47,$47,$47,$47,$47,$04,$01,$01,$01,$01,$01,$01,$03,$47,$47,$47,$00
m3u12   
m3ulast	.byte $80

;Door Close Data
m3cld   .byte $2D       ;RAM Offset
        .byte $41,$03   ;Closed data
        .byte $43,$04   ;Open data   
        
;**************************************************
; Maze Homeworld Animation Data
;**************************************************
;mazhw   .byte $47,$47,$00
;        .byte $47,$47,$00
;        .byte $47,$47,$00
;        .byte $47,$47,$00
;        .byte $47,$47,$00
;        .byte $47,$47,$00
;        .byte $47,$47,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$47,$47,$00
;        .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$00
;        .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$00
;        .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$00
;        .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$00
;        .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$00
;        .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$00
;        .byte $80 

mazdsiz = m0u1-maz0+4 



 
 
;*********************************************************     
; Token Locations (only 4 of these, 8 bytes each
;   Byte 1 = source dif4mz number (-1) if none
;   Byte 2,3,4,5 = Location XL,XH,YL,YH 
;   Byte 6 = dif4mz number of the target level after
;            collecting. Example: Levels 25,26,27,28
;            which is dif4mz = $18,$19,$1A,$1B
;   Byte 7 = dif4mz number of the Level to resume on
;            after the hidden level is completed.
;            Example: Level 1,2,3,4 is dif4mz = 0,1,2,3
;	Byte 8 = Visibility Stamps
;********************************************************* 

#IF DEBUG != 0
mtok 
		;DEBUG data, puts each token on first level for testing
		.db $00,$84,$08,$7C,$F7,$18,$06,$03                 
        .db $01,$64,$05,$BC,$FA,$19,$0A,$03                 
        .db $02,$B8,$09,$34,$F6,$1A,$0E,$01                 
        .db $03,$4C,$0D,$50,$F9,$1B,$12,$03  
#ELSE
#IF TOURNAMENT_EDITION != 0
#include ./tw_mazedtok_te.asm
#ELSE
#include ./tw_mazedtok_pe.asm   
#ENDIF          
#ENDIF
		
;***************************************************************
;***************************************************************
;  BEGIN DYNAMIC LENGTH DATA AREA
;  All data tables after this point are of
;  variable length and typically have their 
;  base address referenced from the static tables
;  above.
;***************************************************************
;***************************************************************
dynamic_base = $
;***************************************************************
;***************************************************************    
;**************************************
    .sbttl "Disc Placement"    
;**************************************
;* -Y/X Position Compressed Byte        
;*  Value of 0 ends list               
;**************************************
;**************************************
    .sbttl "Force Field Placement"
;**************************************
;* X + Direction, Y                   
;* Note: First 7 horizontal, second   
;*       7 vertical. FF finishes off  
;*       Horizontal ones, 00 finishes 
;*       off list.                    
;**************************************
;***************************************
    .sbttl "Arrow Placement"
;***************************************
;* Y Position/X Position, Direction   
;* 0 in Y/X Position ends list           
;***************************************
;**************************************
    .sbttl "Trip Point Placement"
;**************************************
;* X Position, Y Position             
;* 0 in X position ends list          
;**************************************
;**************************************************
    .sbttl "Trip Point Action Data"
;**************************************************
;* Table of values for each trip point for each   
;* maze and difcy level. Each action data is 3 
;* bytes per entry. $00 ends the list.        
;*                                                
;* (X Position, Y Position, X vel index)          
;*                                                
;* X Pos = Position + Flags                       
;* Flag D7 = Only 1 release (bottom or left one)
;**************************************************
;************************************************
    .sbttl "Permanant Changes to Maze"
;************************************************
;* mzta - Permanant Changes                     
;*        byte 1: Stamp Postion                 
;*        byte 2: Replacement Stamp Number           
;************************************************       
;************************************************
    .sbttl "Dynamic Changes to Maze"
;************************************************
;*                                              
;* mztd - Dynamic Changes   
;*        byte 1: Stamp Postion                 
;*        byte 2: Replacement Stamp Number                       
;************************************************		
;*****************************************
    .sbttl "One Way Walls"
;*****************************************
;* X LSB, Y LSB, 0 Ends List             *
;* cmright points to right,              *
;*       cmleft points to left           *
;*****************************************
       
;************************************************
    .sbttl "Code for Cannon Data Sets"
;************************************************
;* First Byte:                                  
;*    D7-D6 = 00    Return to start of list       
;*    D7-D6 = 01    Move Gun to a new orientation 
;*                                              
;*          D5-D3 = Angle of Gun:               
;*                = 000 Top Right               
;*                = 001 Middle Right            
;*                = 010 Bottom Right            
;*                = 011 Point Down              
;*                = 100 Top Left                
;*                = 101 Middle Left             
;*                = 110 Bottom Left 
;*
;* 			D2-D1 = Rotation Speed (0-3)
;*			D0 = Fire Bit (Shot Velocity in next byte)
;*
;*			Second Byte: Shot Velocity (optional)
;*                                              
;*    D7-D6 = 10  Move Gun Location             
;*                                              
;*          D5-D0 = Frames of wait time/4       
;*                = 0  Then Zero X and Y vel.   
;*                > 0  Then Velocities Follow   
;*          Second Byte: X MSB Velocity (optional)         
;*          Third Byte:  Y MSB Velocity (optional)         
;*                                              
;*    D7-D6 = 11  Pause                         
;*          D5-D0 = Frames of wait time/4       
;*
;************************************************
;* Cannon Macros
;************************************************
;Move Cannon Base Location
#define cann_loc(frames,xvel,yvel)			\ .byte ($80|(frames&$3F))
#defcont                    				\#IF (frames > 0)	
#defcont									\ .byte xvel,yvel
#defcont									\#ENDIF

;Shortcut to Stop Movement of Cannon Base Location
#define cann_stop                           cann_loc(0,0,0)

;Change Gun Position
#define cann_pos(position,possp,shot,shotsp)\ .byte (($40)|(position<<3)|((possp&$03)<<1)|(shot))
#defcont                    				\#IF (shot > 0)	
#defcont									\ .byte shotsp
#defcont									\#ENDIF

;Pause Movement
#define cann_pau(frames)					\ .byte ($C0|(frames&$3F))

;End of Loop Marker (required on each sequence)
#define cann_end 							\ .byte $00

;gun positions
canp_tr	= 0     ;Top Right
canp_mr	= 1     ;Middle Right
canp_br = 2     ;Bottom Right
canp_dn = 3     ;Down
canp_tl = 4     ;Top Left
canp_ml	= 5     ;Middle Left
canp_bl = 6     ;Bottom Left

        

;******************************************************
    .sbttl "Transporter Data"    
;******************************************************
;* Color in D0-D3, Direction in D4(1=right),          
;* X MSB, Y MSB                                       
;* Color = 0 ends list                                
;* Next comes packed transportability info            
;* $EE ends list, this can come at any time if
;  no other bits are set                            
;******************************************************
; Transportability Info is a bit mask for each object
; We need a total of 0x32(50 bits) to cover motion objects - excluding Max robots
;
; 00000000 00000000 11111111 11111111 22222222 22222222 33
; 76543210 fedcba98 76543210 fedcba98 76543210 fedcba98 01
; 
; 0x00			Rex
; 0x01			Reactor
; 0x02-0x11		Pyroid/Fireball
; 0x12-0x19		Laser Cannon Shots
; 0x1a-0x1d		Laser Cannons
; 0x1e-0x27		Perkoids
; 0x28-0x31		Perkoid Shots
;*******************************************************



;*****************************************************
    .sbttl "Data Set for Laser Cannon Actions"
;*****************************************************
;  Each Level could have up to 4 Cannons
;  Zero means there is no Cannon at this location
;*****************************************************
mcan
#IF TOURNAMENT_EDITION != 0
#include ./tw_mazedcan_te.asm
#ELSE
#include ./tw_mazedcan_pe.asm   
#ENDIF 


#IF TOURNAMENT_EDITION != 0
#include ./tw_mazed_te.asm
#ELSE
#include ./tw_mazed_pe.asm 
#ENDIF  


  
