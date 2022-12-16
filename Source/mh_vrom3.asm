;********************************************
;* Major Havoc Vector ROM Overlay Page 3    
;* Copyright 1983 Atari                     
;* Transcribed by Jess M. Askey 2000        
;********************************************
    .title "TWOV3"
;********************************************
;* Includes:                                
;********************************************
#include "havoc.ah"		;For build vars
#include "vector.ah"    ;For the various vector macros
#include "./exp/mh_alpha.exp"
;********************************************
;* Program:                                 
;********************************************
    .org $6000
;*****************************************************************
;* Vector ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_V3
	.byte MAJOR_VERSION,MINOR_VERSION
;*****************************************************************
        
thispage = vpage3
        

;********************************************
;* Tactical Scanner Center Station Pics
;********************************************
tacct_vpg = $60|thispage
        
tactc0  vctr(-48d,49d,hidden)
        vctr(97d,0d,visible)
        vctr(0d,-96d,visible)
        vctr(-97d,-1d,visible)
        vctr(0d,97d,visible)
        vctr(22d,-23d,visible)
        vctr(52d,-1d,visible)
        vctr(22d,24d,hidden)
        vctr(-22d,-23d,visible)
        vctr(0d,-52d,visible)
        vctr(23d,-21d,visible)
        vctr(-23d,21d,hidden)
        vctr(-52d,1d,visible)
        vctr(-22d,-22d,visible)
        vctr(22d,21d,hidden)
        vctr(0d,52d,visible)
        vctr(26d,152d,hidden)
        vctr(15d,-9d,visible)
        vctr(0d,-17d,visible)
        vctr(-15d,-7d,visible)
        vctr(-13d,7d,visible)
        vctr(0d,18d,visible)
        vctr(13d,8d,visible)
        vctr(145d,-81d,hidden)
        vctr(14d,-9d,visible)
        vctr(0d,-15d,visible)
        vctr(-13d,-9d,visible)
        vctr(-15d,9d,visible)
        vctr(0d,15d,visible)
        vctr(14d,9d,visible)
        vctr(0d,-162d,hidden)
        vctr(14d,-8d,visible)
        vctr(-1d,-16d,visible)
        vctr(-12d,-8d,visible)
        vctr(-15d,8d,visible)
        vctr(0d,16d,visible)
        vctr(14d,8d,visible)
        vctr(-145d,-79d,hidden)
        vctr(15d,-10d,visible)
        vctr(-1d,-17d,visible)
        vctr(-14d,-6d,visible)
        vctr(-14d,7d,visible)
        vctr(1d,17d,visible)
        vctr(13d,8d,visible)
        vctr(-145d,81d,hidden)
        vctr(14d,-9d,visible)
        vctr(0d,-15d,visible)
        vctr(-14d,-9d,visible)
        vctr(-15d,8d,visible)
        vctr(0d,16d,visible)
        vctr(15d,9d,visible)
        vctr(1d,161d,hidden)
        vctr(13d,-9d,visible)
        vctr(0d,-15d,visible)
        vctr(-13d,-8d,visible)
        vctr(-15d,8d,visible)
        vctr(0d,15d,visible)
        vctr(15d,9d,visible)
        vctr(144d,-97d,hidden)
        rtsl
        
tactc1  vctr(-161d,32d,visible)
        vctr(0d,162d,visible)
        vctr(161d,32d,visible)
        vctr(161d,-32d,visible)
        vctr(0d,-162d,visible)
        vctr(-161d,-33d,visible)
        vctr(-161d,33d,hidden)
        vctr(96d,32d,visible)
        vctr(130d,0d,visible)
        vctr(-65d,-15d,visible)
        vctr(-65d,16d,visible)
        vctr(0d,96d,visible)
        vctr(65d,17d,visible)
        vctr(64d,-17d,visible)
        vctr(0d,-96d,visible)
        vctr(97d,-33d,visible)
        vctr(-322d,162d,hidden)
        vctr(96d,-33d,visible)
        vctr(129d,0d,visible)
        vctr(97d,32d,visible)
        vctr(-136d,-138d,hidden)
        vctr(8d,-6d,visible)
        vctr(-66d,1d,visible)
        vctr(8d,4d,visible)
        vctr(-132d,-123d,hidden)
        vctr(55d,0d,visible)
        vctr(-27d,-29d,visible)
        vctr(-28d,29d,visible)
        vctr(33d,-11d,hidden)
        vctr(3d,-3d,visible)
        vctr(-7d,-9d,visible)
        vctr(-7d,9d,visible)
        vctr(1d,4d,visible)
        vctr(106d,10d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-28d,visible)
        vctr(-28d,28d,visible)
        vctr(34d,-11d,hidden)
        vctr(3d,-4d,visible)
        vctr(-9d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(2d,4d,visible)
        vctr(106d,11d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-27d,visible)
        vctr(-28d,27d,visible)
        vctr(34d,-11d,hidden)
        vctr(3d,-4d,visible)
        vctr(-8d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(3d,4d,visible)
        vctr(-218d,-53d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-29d,visible)
        vctr(-28d,29d,visible)
        vctr(32d,-12d,hidden)
        vctr(4d,-4d,visible)
        vctr(-8d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(2d,4d,visible)
        vctr(105d,12d,hidden)
        vctr(57d,0d,visible)
        vctr(-28d,-29d,visible)
        vctr(-29d,28d,visible)
        vctr(33d,-11d,hidden)
        vctr(3d,-4d,visible)
        vctr(-7d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(2d,4d,visible)
        vctr(-88d,-53d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-28d,visible)
        vctr(-28d,29d,visible)
        vctr(32d,-12d,hidden)
        vctr(5d,-5d,visible)
        vctr(-9d,-8d,visible)
        vctr(-6d,8d,visible)
        vctr(1d,5d,visible)
        vctr(5d,209d,hidden)
        rtsl

tactc2  vctr(-48d,96d,hidden)
        vctr(0d,-192d,visible)
        vctr(96d,0d,visible)
        vctr(0d,192d,visible)
        vctr(-192d,0d,visible)
        vctr(0d,288d,visible)
        vctr(-128d,0d,visible)
        vctr(0d,-768d,visible)
        vctr(128d,0d,visible)
        vctr(0d,96d,visible)
        vctr(-128d,256d,hidden)
        vctr(128d,0d,visible)
        vctr(0d,-160d,visible)
        vctr(96d,0d,visible)
        vctr(0d,-96d,visible)
        vctr(96d,0d,hidden)
        vctr(0d,96d,visible)
        vctr(224d,0d,visible)
        vctr(-96d,-96d,hidden)
        vctr(0d,-96d,visible)
        vctr(-224d,0d,hidden)
        vctr(320d,0d,visible)
        vctr(0d,768d,visible)
        vctr(-320d,0d,visible)
        vctr(0d,-192d,visible)
        vctr(208d,0d,visible)
        vctr(-96d,96d,hidden)
        vctr(208d,0d,visible)
        vctr(-160d,-192d,hidden)
        vctr(96d,0d,visible)
        vctr(0d,-192d,visible)
        vctr(-96d,0d,visible)
        vctr(0d,192d,visible)
        vctr(-112d,-96d,hidden)
        rtsl
        
tactc3  vctr(-84d,0d,hidden)
        vctr(45d,68d,visible)
        vctr(80d,1d,visible)
        vctr(45d,-69d,visible)
        vctr(-45d,-74d,visible)
        vctr(-80d,1d,visible)
        vctr(-44d,73d,visible)
        vctr(54d,0d,visible)
        vctr(15d,-24d,visible)
        vctr(-25d,-48d,visible)
        vctr(25d,48d,hidden)
        vctr(28d,0d,visible)
        vctr(27d,-49d,visible)
        vctr(-27d,49d,hidden)
        vctr(15d,24d,visible)
        vctr(57d,0d,visible)
        vctr(-57d,0d,hidden)
        vctr(-15d,23d,visible)
        vctr(26d,45d,visible)
        vctr(-25d,-45d,hidden)
        vctr(-28d,0d,visible)
        vctr(-26d,45d,visible)
        vctr(26d,-45d,hidden)
        vctr(-16d,-23d,visible)
        vctr(30d,-169d,hidden)      ;vctr(-27d,-138d,hidden)     ;+31Y
        vctr(-145d,82d,visible)      ;vctr(-88d,51d,visible)      
        vctr(-1d,159d,visible)
        vctr(145d,82d,visible)
        vctr(145d,-82d,visible)
        vctr(1d,-161d,visible)
        vctr(-145d,-82d,visible)     ;vctr(-88d,-49d,visible)
        vctr(-10d,96d,hidden)        ;vctr(-67d,65d,hidden)
        vctr(9d,-8d,visible)
        vctr(13d,8d,visible)
        vctr(45d,117d,hidden)
        vctr(11d,-5d,visible)
        vctr(-1d,-13d,visible)
        vctr(-136d,0d,hidden)
        vctr(0d,15d,visible)
        vctr(12d,4d,visible)
        vctr(56d,-45d,hidden)
        ;add the bar in front here
        vctr(-60d,-180d,hidden)
        vctr(5d,-5d,visible)
        vctr(110d,0d,visible)
        vctr(5d,5d,visible)
        vctr(-120d,0d,visible)
        vctr(60d,180d,hidden)    
        rtsl

;Vax Space Station - Center Scanner
tactc4	vctr(238d,231d,hidden)
		vctr(-27d,-277d,visible)
		vctr(-248d,-123d,visible)
		vctr(-10d,304d,visible)
		vctr(287d,96d,visible)
		vctr(-206d,90d,visible)
		vctr(-259d,-64d,visible)
		vctr(178d,-122d,hidden)
		vctr(-178d,122d,visible)
		vctr(30d,-265d,visible)
		vctr(158d,-160d,visible)
		vctr(272d,361d,hidden)
		vctr(-281d,-101d,visible)
		vctr(152d,54d,hidden)
		vctr(-9d,-248d,visible)
		vctr(120d,270d,hidden)
		vctr(-2d,-27d,visible)
		vctr(-14d,21d,hidden)
		vctr(-2d,-27d,visible)
		vctr(-15d,21d,hidden)
		vctr(-2d,-28d,visible)
		vctr(-15d,21d,hidden)
		vctr(-1d,-28d,visible)
		vctr(-16d,21d,hidden)
		vctr(-1d,-28d,visible)
		vctr(-17d,21d,hidden)
		vctr(-1d,-28d,visible)
		vctr(-40d,13d,hidden)
		vctr(0d,-29d,visible)
		vctr(-19d,21d,hidden)
		vctr(0d,-29d,visible)
		vctr(-19d,21d,hidden)
		vctr(0d,-29d,visible)
		vctr(-20d,21d,hidden)
		vctr(0d,-29d,visible)
		vctr(-21d,21d,hidden)
		vctr(0d,-30d,visible)
		vctr(-22d,21d,hidden)
		vctr(0d,-30d,visible)
		vctr(-35d,43d,hidden)
		vctr(-177d,131d,visible)
		vctr(88d,-67d,hidden)
		vctr(19d,-243d,visible)
		vctr(63d,167d,hidden)
		vctr(1d,-30d,visible)
		vctr(-15d,41d,hidden)
		vctr(1d,-29d,visible)
		vctr(-15d,40d,hidden)
		vctr(1d,-29d,visible)
		vctr(-15d,39d,hidden)
		vctr(1d,-29d,visible)
		vctr(-14d,39d,hidden)
		vctr(2d,-28d,visible)
		vctr(-14d,38d,hidden)
		vctr(2d,-28d,visible)
		vctr(-28d,48d,hidden)
		vctr(2d,-27d,visible)
		vctr(-13d,36d,hidden)
		vctr(2d,-27d,visible)
		vctr(-13d,36d,hidden)
		vctr(2d,-27d,visible)
		vctr(-13d,35d,hidden)
		vctr(2d,-27d,visible)
		vctr(-13d,35d,hidden)
		vctr(2d,-26d,visible)
		vctr(-13d,34d,hidden)
		vctr(2d,-26d,visible)


		rtsl

		; vctr(238d, 231d, hidden) ;131d originally
		; vctr(-27d, -277d, visible)
		; vctr(-248d, -123d, visible)
		; vctr(-10d, 304d, visible)
		; vctr(287d, 96d, visible)
		; vctr(-206d, 90d, visible)
		; vctr(-259d, -64d, visible)
		; vctr(178d, -122d, hidden)
		; vctr(-178d, 122d, visible)
		; vctr(30d, -265d, visible)
		; vctr(158d, -160d, visible)
		; vctr(272d, 361d, hidden)
		; vctr(-281d, -101d, visible)
		; vctr(152d, 54d, hidden)
		; vctr(-9d, -248d, visible)
		; vctr(120d, 270d, hidden)
		; vctr(-2d, -27d, visible)
		; vctr(-14d, 21d, hidden)
		; vctr(-2d, -27d, visible)
		; vctr(-15d, 21d, hidden)
		; vctr(-2d, -28d, visible)
		; vctr(-15d, 21d, hidden)
		; vctr(-1d, -28d, visible)
		; vctr(-16d, 21d, hidden)
		; vctr(-1d, -28d, visible)
		; vctr(-17d, 21d, hidden)
		; vctr(-1d, -28d, visible)
		; vctr(-40d, 13d, hidden)
		; vctr(0d, -29d, visible)
		; vctr(-19d, 21d, hidden)
		; vctr(0d, -29d, visible)
		; vctr(-19d, 21d, hidden)
		; vctr(0d, -29d, visible)
		; vctr(-20d, 21d, hidden)
		; vctr(0d, -29d, visible)
		; vctr(-21d, 21d, hidden)
		; vctr(0d, -30d, visible)
		; vctr(-22d, 21d, hidden)
		; vctr(0d, -30d, visible)
		; vctr(-27d, 41d, hidden)
		; vctr(-175d, 128d, visible)
		; vctr(78d, -57d, hidden)
		; vctr(19d, -243d, visible)
		; vctr(63d, 162d, hidden)
		; vctr(1d, -30d, visible)
		; vctr(-15d, 41d, hidden)
		; vctr(1d, -29d, visible)
		; vctr(-15d, 40d, hidden)
		; vctr(1d, -29d, visible)
		; vctr(-15d, 39d, hidden)
		; vctr(1d, -29d, visible)
		; vctr(-14d, 39d, hidden)
		; vctr(2d, -28d, visible)
		; vctr(-14d, 38d, hidden)
		; vctr(2d, -28d, visible)
		; vctr(-28d, 48d, hidden)
		; vctr(2d, -27d, visible)
		; vctr(-13d, 36d, hidden)
		; vctr(2d, -27d, visible)
		; vctr(-13d, 36d, hidden)
		; vctr(2d, -27d, visible)
		; vctr(-13d, 35d, hidden)
		; vctr(2d, -27d, visible)
		; vctr(-13d, 35d, hidden)
		; vctr(2d, -26d, visible)
		; vctr(-13d, 34d, hidden)
		; vctr(2d, -26d, visible)
		; rtsl


        
;****************************
    .sbttl "Maze Arrows"
;****************************  
arrow_vpg = $60 | thispage
     
rtarrow vctr(-18d,4d,hidden)
        vctr(18d,0d,visible)
        vctr(0d,8d,visible)
        vctr(12d,-12d,visible)
        vctr(-12d,-12d,visible)
        vctr(0d,8d,visible)
        vctr(-18d,0d,visible)
        rtsl

uparrow vctr(-4d,-18d,hidden)
        vctr(0d,18d,visible)
        vctr(-8d,0d,visible)
        vctr(12d,12d,visible)
        vctr(12d,-12d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,-18d,visible)
        rtsl

dnarrow vctr(-4d,18d,hidden)
        vctr(0d,-18d,visible)
        vctr(-8d,0d,visible)
        vctr(12d,-12d,visible)
        vctr(12d,12d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,18d,visible)
        rtsl

ltarrow vctr(18d,4d,hidden)
        vctr(-18d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-12d,-12d,visible)
        vctr(12d,-12d,visible)
        vctr(0d,8d,visible)
        vctr(18d,0d,visible)
        rtsl

nearrow vctr(-15d,-9d,hidden)
        vctr(12d,12d,visible)
        vctr(-6d,6d,visible)
        vctr(18d,0d,visible)
        vctr(0d,-18d,visible)
        vctr(-6d,6d,visible)
        vctr(-12d,-12d,visible)
        vctr(25d,-1d,hidden)
        rtsl
        
searrow vctr(-15d,9d,hidden)
        vctr(12d,-12d,visible)
        vctr(-6d,-6d,visible)
        vctr(18d,0d,visible)
        vctr(0d,18d,visible)
        vctr(-6d,-6d,visible)
        vctr(-12d,12d,visible)
        rtsl
        
nwarrow vctr(15d,-9d,hidden)
        vctr(-12d,12d,visible)
        vctr(6d,6d,visible)
        vctr(-18d,0d,visible)
        vctr(0d,-18d,visible)
        vctr(6d,6d,visible)
        vctr(12d,-12d,visible)
        rtsl
        
swarrow vctr(15d,9d,hidden)
        vctr(-12d,-12d,visible)
        vctr(6d,-6d,visible)
        vctr(-18d,0d,visible)
        vctr(0d,18d,visible)
        vctr(6d,-6d,visible)
        vctr(12d,12d,visible)
        rtsl
		
;*************************************
;Out Arrows - Up        
outrw0  vctr(-20d,20d,hidden)
        vctr(0d,32d,visible)
        vctr(-16d,0d,visible)
        vctr(36d,36d,visible)
        vctr(36d,-36d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,-32d,visible)
        vctr(0d,-40d,hidden)
        vctr(0d,-48d,visible)
        vctr(-40d,0d,visible)
        vctr(0d,48d,visible)
        vctr(20d,20d,hidden)
        rtsl
        
outrw1  vctr(-68d,20d,hidden)
        vctr(120d,0d,visible)
        vctr(0d,16d,visible)
        vctr(36d,-36d,visible)
        vctr(-36d,-36d,visible)
        vctr(0d,16d,visible)
        vctr(-120d,0d,visible)
        vctr(0d,40d,visible)
        vctr(68d,-20d,hidden)
        rtsl
        
outrw2  vctr(-20d,-20d,hidden)
        vctr(0d,-32d,visible)
        vctr(-16d,0d,visible)
        vctr(36d,-36d,visible)
        vctr(36d,36d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,32d,visible)
        vctr(0d,40d,hidden)
        vctr(0d,48d,visible)
        vctr(-40d,0d,visible)
        vctr(0d,-48d,visible)
        vctr(20d,-20d,hidden)
        rtsl
        
outrw3  vctr(68d,20d,hidden)
        vctr(-120d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-36d,-36d,visible)
        vctr(36d,-36d,visible)
        vctr(0d,16d,visible)
        vctr(120d,0d,visible)
        vctr(0d,40d,visible)
        vctr(-68d,-20d,hidden)
        rtsl

;OUT word
outwrd  vctr(-20d,0d,hidden)
        vctr(-6d,-12d,visible)
        vctr(-12d,0d,visible)
        vctr(-6d,12d,visible)
        vctr(6d,12d,visible)
        vctr(12d,0d,visible)
        vctr(6d,-12d,visible)
        vctr(8d,12d,hidden)
        vctr(0d,-18d,visible)
        vctr(12d,-6d,visible)
        vctr(12d,6d,visible)
        vctr(0d,18d,visible)
        vctr(8d,0d,hidden)
        vctr(24d,0d,visible)
        vctr(-12d,0d,hidden)
        vctr(0d,-24d,visible)
        vctr(-32d,12d,hidden)
        rtsl        
        
;************************************
    .sbttl "Lightning Parts"
;**************************************
ltg_vpg = $60|thispage
        
ltg_a   vctr(-24d,-8d,visible)
        vctr(40d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(24d,-8d,visible)
        rtsl
        
ltg_b   vctr(8d,-8d,visible)
        vctr(-8d,-8d,visible)
        vctr(16d,-8d,visible)
ltg_b1  vctr(-32d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(24d,-8d,visible)
        rtsl

ltg_c   vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
ltg_c1  vctr(-40d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl        
       
ltg_d   vctr(-16d,-8d,visible)
        vctr(32d,-16d,visible)
        vctr(-24d,-8d,visible)
        vctr(24d,-8d,visible)
        rtsl
        
ltg_e   vctr(-40d,-8d,visible)
        vctr(16d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(32d,-8d,visible)
ltg_e1  vctr(-16d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-32d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl
        
;Assembled Lightning (Vertical)

ltng0   vctr(0d,128d,hidden)
        vctr(0d,-256d,visible)
        rtsl
        
ltng1   vctr(0d,128d,hidden)
        vctr(24d,-8d,visible)
        jsrl(ltg_e) ;$63F6
        vctr(0d,-168d,visible)
        rtsl
        
ltng2   vctr(0d,128d,hidden)
        vctr(0d,-16d,visible)
        jsrl(ltg_d)     ;$63EA
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_e)     ;$63F6
        vctr(0d,-72d,visible)
        rtsl
        
ltng3   vctr(0d,128d,hidden)
        vctr(0d,-24d,visible)
        vctr(8d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(16d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-40d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(8d,-8d,visible)
        jsrl(ltg_d)     ;$63EA
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-32d,-8d,visible)
        vctr(8d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(8d,-8d,visible)
        vctr(0d,-8d,visible)
        rtsl
        
ltng4   vctr(0d,128d,hidden)
        vctr(-32d,-8d,visible)
        vctr(40d,-8d,visible)
        jsrl(ltg_e1)    ;$6402
        vctr(0d,-48d,visible)
        vctr(8d,-8d,visible)
        vctr(-8d,-8d,visible)
        vctr(16d,-8d,visible)
        jsrl(ltg_b1)    ;$63CC
        jsrl(ltg_c)     ;$63D8
        vctr(-16d,-8d,visible)
        vctr(32d,-16d,visible)
        vctr(-24d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl
        
ltng5   vctr(0d,128d,hidden)
        vctr(0d,-8d,visible)
        vctr(-8d,-8d,visible)
        vctr(24d,-8d,visible)
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_e)     ;$63F6
        vctr(0d,-48d,visible)
        jsrl(ltg_b)     ;$63C6
        rtsl
        
ltng6   vctr(0d,128d,hidden)
        vctr(0d,-16d,visible)
        vctr(-16d,-8d,visible)
        vctr(24d,-8d,visible)
        jsrl(ltg_c)     ;$63D8
        jsrl(ltg_a)     ;$63BA
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_e)     ;$63F6
        vctr(0d,-8d,visible)
        rtsl
        
ltng7   vctr(0d,128d,hidden)
        vctr(0d,-72d,visible)
        jsrl(ltg_b)     ;$63C6
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_c1)    ;$63DE
        jsrl(ltg_a)     ;$63BA
        jsrl(ltg_b1)    ;$63CC
        vctr(-8d,-8d,visible)
        rtsl
        
ltng8   vctr(0d,128d,hidden)
        vctr(0d,-168d,visible)
        jsrl(ltg_b)     ;$63C6
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-40d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl

;Lightning Parts (Horizontal)
        
lng_ax  vctr(-8d,24d,visible)
        vctr(-8d,-40d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-24d,visible)
        rtsl
        
lng_bx  vctr(-8d,-8d,visible)
        vctr(-8d,8d,visible)
        vctr(-8d,-16d,visible)
lng_bx1 vctr(-8d,32d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-24d,visible)
        rtsl
 
lng_cx  vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
lng_cx1 vctr(-8d,40d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-8d,visible)
        rtsl
        
lng_dx  vctr(-8d,16d,visible)
        vctr(-16d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-24d,visible)
        rtsl
        
lng_ex  vctr(-8d,40d,visible)
        vctr(-8d,-16d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-32d,visible)
lng_ex1 vctr(-8d,16d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,32d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-8d,visible)
        rtsl

;Assembled Lightning (Horizontal)
        
ltng0x  vctr(128d,0d,hidden)
        vctr(-256d,0d,visible)
        rtsl
    
ltng1x  vctr(128d,0d,hidden)
        vctr(-8d,-24d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-168d,0d,visible)
        rtsl
        
ltng2x  vctr(128d,0d,hidden)
        vctr(-16d,0d,visible)
        jsrl(lng_dx)    ;$6544
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-72d,0d,visible)
        rtsl
        
ltng3x  vctr(128d,0d,hidden)
        vctr(-24d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-16d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,40d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-8d,visible)
        jsrl(lng_dx)    ;$6544
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,32d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,0d,visible)
        rtsl
        
ltng4x  vctr(128d,0d,hidden)
        vctr(-8d,32d,visible)
        vctr(-8d,-40d,visible)
        jsrl(lng_ex1)   ;$655C
        vctr(-48d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,8d,visible)
        vctr(-8d,-16d,visible)
        jsrl(lng_bx1)   ;$6526
        jsrl(lng_cx)    ;$6532
        vctr(-8d,16d,visible)
        vctr(-16d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-8d,visible)
        rtsl
        
ltng5x  vctr(128d,0d,hidden)
        vctr(-8d,0d,visible)
        vctr(-8d,8d,visible)
        vctr(-8d,-24d,visible)
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-48d,0d,visible)
        jsrl(lng_bx)    ;$6520
        rtsl
        
ltng6x  vctr(128d,0d,hidden)
        vctr(-16d,0d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-24d,visible)
        jsrl(lng_cx)    ;$6532
        jsrl(lng_ax)    ;$6514
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-8d,0d,visible)
        rtsl
        
ltng7x  vctr(128d,0d,hidden)
        vctr(-72d,0d,visible)
        jsrl(lng_bx)    ;$6520
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_cx1)   ;$6538
        jsrl(lng_ax)    ;$6514
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,8d,visible)
        vctr(256d,0d,hidden)
        rtsl
        
ltng8x  vctr(128d,0d,hidden)
        vctr(-168d,0d,visible)
        jsrl(lng_bx)    ;$6520
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,40d,visible)
        vctr(-8d,-8d,visible)
        rtsl     
        
dod0    vctr(91d,137d,hidden)
        vctr(-101d,21d,visible)
        vctr(-88d,-28d,visible)
        vctr(-60d,-89d,visible)
        vctr(10d,-93d,visible)
        vctr(57d,-85d,visible)
        vctr(101d,-21d,visible)
        vctr(88d,28d,visible)
        vctr(60d,89d,visible)
        vctr(-10d,93d,visible)
        vctr(-57d,85d,visible)
        vctr(-24d,-39d,visible)
        vctr(41d,-111d,visible)
        vctr(50d,-28d,visible)
        vctr(-50d,28d,hidden)
        vctr(-91d,-71d,visible)
        vctr(-7d,-74d,visible)
        vctr(7d,74d,hidden)
        vctr(-98d,66d,visible)
        vctr(-67d,-34d,visible)
        vctr(67d,34d,hidden)
        vctr(31d,111d,visible)
        vctr(-48d,37d,visible)
        vctr(48d,-37d,hidden)
        vctr(117d,5d,visible)
        vctr(-67d,-98d,hidden)
        rtsl
        
dod1    vctr(91d,110d,hidden)
        vctr(-24d,38d,visible)
        vctr(-117d,5d,visible)
        vctr(-48d,-37d,visible)
        vctr(-61d,-98d,visible)
        vctr(50d,-88d,visible)
        vctr(42d,-78d,visible)
        vctr(117d,-5d,visible)
        vctr(48d,37d,visible)
        vctr(61d,98d,visible)
        vctr(-11d,24d,visible)
        vctr(-57d,104d,visible)
        vctr(-102d,-21d,visible)
        vctr(-6d,-115d,visible)
        vctr(98d,-51d,visible)
        vctr(67d,83d,visible)
        vctr(-67d,-83d,hidden)
        vctr(-31d,-76d,visible)
        vctr(-67d,127d,hidden)
        vctr(-92d,-44d,visible)
        vctr(98d,159d,hidden)
        vctr(-87d,27d,visible)
        vctr(98d,-116d,hidden)
        rtsl
        
dod2    vctr(100d,123d,hidden)
        vctr(-69d,33d,visible)
        vctr(-112d,-13d,visible)
        vctr(-50d,-81d,visible)
        vctr(-31d,-91d,visible)
        vctr(62d,-94d,visible)
        vctr(69d,-33d,visible)
        vctr(112d,13d,visible)
        vctr(50d,81d,visible)
        vctr(31d,91d,visible)
        vctr(-62d,94d,visible)
        vctr(-69d,-32d,visible)
        vctr(19d,-115d,visible)
        vctr(81d,-38d,visible)
        vctr(-81d,38d,hidden)
        vctr(-100d,-58d,visible)
        vctr(19d,-74d,visible)
        vctr(-19d,74d,hidden)
        vctr(-81d,79d,visible)
        vctr(-31d,-26d,visible)
        vctr(31d,26d,hidden)
        vctr(50d,106d,visible)
        vctr(0d,40d,visible)
        vctr(0d,-40d,hidden)
        vctr(112d,-12d,visible)
        vctr(-31d,-91d,hidden)
        rtsl


;Space Puppies
        
pupl00  vctr(-32d,-4d,hidden)
        jsrl(puple01)   ;$67B4
        jsrl(puple00)   ;$67B0
        vctr(-4d,0d,hidden)
        rtsl

puple00 vctr(36d,4d,hidden)   ;7B0
puple01 vctr(8d,14d,visible)
        vctr(8d,-10d,visible)
        vctr(-8d,-14d,visible)
        vctr(-8d,10d,visible)
        rtsl
        
puple02 vctr(18d,26d,visible) ;7BE
        vctr(18d,-22d,visible)
        vctr(-18d,-28d,visible)
        vctr(-36d,48d,visible)
        vctr(-16d,-28d,visible)
        vctr(16d,-24d,visible)
        vctr(18d,28d,visible)
        vctr(8d,14d,hidden)
        vctr(-8d,6d,visible)
        vctr(-60d,26d,visible)
        rtsl
        
pupc0   vctr(40d,0d,visible)  ;7D8
        vctr(20d,22d,visible)
        vctr(0d,30d,visible)
        vctr(-6d,4d,visible)
        rtsl

wing00  jsrl(puple02)   ;$67BE
        vctr(8d,-50d,visible)
        vctr(8d,-2d,hidden)
        vctr(-68d,14d,visible)
        vctr(76d,-44d,visible)
        vctr(-8d,6d,hidden)
        vctr(0d,-2d,visible)
        vctr(24d,-28d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(0d,2d,hidden)
        vctr(68d,18d,visible)
        vctr(-62d,-48d,visible)
        vctr(-38d,32d,hidden)
        rtsl
        
wing01  jsrl(puple02)   ;$67BE
        vctr(8d,-52d,visible)
        vctr(8d,0d,hidden)
        vctr(-66d,-8d,visible)
        vctr(74d,-22d,visible)
        vctr(-10d,4d,hidden)
        vctr(24d,-28d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(70d,0d,visible)
        vctr(-64d,-28d,visible)
        vctr(-38d,32d,hidden)
        rtsl
        
wing02  jsrl(puple02)   ;$67BE
        vctr(10d,-54d,visible)
        vctr(6d,2d,hidden)
        vctr(-66d,-28d,visible)
        vctr(74d,-2d,visible)
        vctr(-8d,0d,hidden)
        vctr(22d,-24d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(6d,-4d,hidden)
        vctr(64d,-16d,visible)
        vctr(-64d,-10d,visible)
        vctr(-38d,34d,hidden)
        rtsl
        
wing03  jsrl(puple02)   ;$67BE
        vctr(10d,-58d,visible)
        vctr(6d,6d,hidden)
        vctr(-66d,-56d,visible)
        vctr(74d,26d,visible)
        vctr(-6d,-2d,hidden)
        vctr(20d,-22d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(6d,-6d,hidden)
        vctr(64d,-34d,visible)
        vctr(-64d,10d,visible)
        vctr(-38d,34d,hidden)
        rtsl
        
pupl10  vctr(-22d,-2d,hidden)
        jsrl(puple1_)   ;$6888
        jsrl(puple10)   ;$6886
        vctr(-2d,-6d,hidden)
        rtsl
        
puple10 vctr(24d,8d,hidden)       ;886
puple1_ vctr(8d,14d,visible)
        vctr(8d,-12d,visible)
        vctr(-8d,-14d,visible)
        vctr(-8d,12d,visible)
        rtsl
        
puple11 vctr(-14d,-26d,visible)   ;892
        vctr(-18d,24d,visible)
        vctr(18d,26d,visible)
        vctr(26d,-42d,visible)
        vctr(16d,26d,visible)
        vctr(-16d,24d,visible)
        vctr(-16d,-22d,visible)
        vctr(-12d,8d,hidden)
        vctr(-8d,2d,visible)
        vctr(-94d,-2d,visible)
        rtsl
        
puple12 vctr(34d,6d,visible)  ;8AC
        vctr(12d,22d,visible)
        vctr(0d,28d,visible)
        vctr(-12d,6d,visible)
        rtsl
        
wing10  jsrl(puple11)   ;$6892
        vctr(36d,-32d,visible)
        vctr(30d,0d,hidden)
        vctr(-60d,0d,visible)
        vctr(88d,-26d,visible)
        vctr(-42d,12d,hidden)
        vctr(22d,-18d,visible)
        vctr(34d,-16d,visible)
        jsrl(puple12)   ;$68AC
        vctr(-2d,16d,hidden)
        vctr(50d,18d,visible)
        vctr(-36d,-48d,visible)
        vctr(-34d,14d,hidden)
        rtsl
        
wing11  jsrl(puple11)   ;$6892
        vctr(46d,-40d,visible)
        vctr(20d,8d,hidden)
        vctr(-58d,-20d,visible)
        vctr(86d,-6d,visible)
        vctr(-30d,2d,hidden)
        vctr(10d,-8d,visible)
        vctr(34d,-16d,visible)
        jsrl(puple12)   ;$68AC
        vctr(4d,8d,hidden)
        vctr(50d,10d,visible)
        vctr(-42d,-36d,visible)
        vctr(-34d,18d,hidden)
        rtsl
        
wing12  jsrl(puple11)   ;$6892
        vctr(50d,-44d,visible)
        vctr(16d,12d,hidden)
        vctr(-54d,-40d,visible)
        vctr(82d,14d,visible)
        vctr(-24d,-4d,hidden)
        vctr(4d,-2d,visible)
        vctr(34d,-16d,visible)
        jsrl(puple12)   ;$68AC
        vctr(52d,0d,visible)
        vctr(-46d,-24d,visible)
        vctr(-34d,24d,hidden)
        rtsl
        
wing13  jsrl(puple11)   ;$6892
        vctr(52d,-46d,visible)
        vctr(14d,14d,hidden)
        vctr(-52d,-58d,visible)
        vctr(80d,32d,visible)
        vctr(-18d,-8d,hidden)
        vctr(32d,-14d,visible)
        jsrl(puple12)   ;$68AC
        vctr(12d,-6d,hidden)
        vctr(50d,-10d,visible)
        vctr(-50d,-12d,visible)
        vctr(-34d,28d,hidden)
        rtsl
        
pupl20  vctr(-8d,-12d,hidden)
        vctr(-8d,12d,visible)
        vctr(8d,12d,visible)
        vctr(10d,-8d,hidden)
        vctr(-8d,14d,visible)
        vctr(6d,8d,visible)
        vctr(0d,-26d,hidden)
        rtsl
        
pupl21  vctr(-16d,-24d,visible)
        vctr(-18d,24d,visible)
        vctr(26d,36d,visible)
        vctr(16d,-22d,visible)
        vctr(-10d,-14d,visible)
        vctr(-16d,24d,visible)
        vctr(-10d,-8d,hidden)
        vctr(-16d,4d,visible)
        vctr(-126d,-28d,visible)
        rtsl
        
pupl22  vctr(46d,2d,visible)
        vctr(20d,26d,visible)
        vctr(-10d,34d,visible)
        vctr(-10d,2d,visible)
        rtsl
        
wing20  jsrl(pupl21)    ;$696E
        vctr(124d,-50d,visible)
        vctr(-16d,42d,hidden)
        vctr(-14d,-28d,visible)
        vctr(52d,8d,visible)
        vctr(-22d,-22d,hidden)
        jsrl(pupl22)    ;$6986
        vctr(-34d,12d,hidden)
        vctr(-34d,38d,visible)
        vctr(0d,-30d,visible)
        vctr(-4d,-24d,hidden)
        rtsl
        
wing21  jsrl(pupl21)    ;$696E
        vctr(98d,-40d,visible)
        vctr(10d,32d,hidden)
        vctr(-14d,-42d,visible)
        vctr(52d,22d,visible)
        vctr(-36d,-16d,hidden)
        vctr(14d,-6d,visible)
        jsrl(pupl22)    ;$6986
        vctr(-34d,12d,hidden)
        vctr(14d,8d,visible)
        vctr(12d,8d,hidden)
        vctr(16d,10d,visible)
        vctr(-4d,-24d,visible)
        vctr(-8d,-18d,hidden)
        rtsl
        
wing22  jsrl(pupl21)    ;$696E
        vctr(100d,-40d,visible)
        vctr(8d,32d,hidden)
        vctr(-14d,-58d,visible)
        vctr(52d,38d,visible)
        vctr(-28d,-20d,hidden)
        vctr(6d,-2d,visible)
        jsrl(pupl22)    ;$6986
        vctr(0d,20d,hidden)
        vctr(14d,4d,visible)
        vctr(-10d,-24d,visible)
        vctr(-8d,-4d,hidden)
        rtsl
        
wing23  jsrl(pupl21)    ;$696E
        vctr(102d,-42d,visible)
        vctr(6d,34d,hidden)
        vctr(-14d,-72d,visible)
        vctr(52d,52d,visible)
        vctr(-22d,-22d,hidden)
        jsrl(pupl22)    ;$6986
        vctr(6d,12d,hidden)
        vctr(14d,2d,visible)
        vctr(-12d,-16d,visible)
        vctr(-12d,-2d,hidden)
        rtsl

pupl30  vctr(26d,-12d,hidden)
        vctr(-8d,12d,visible)
        vctr(8d,12d,visible)
        vctr(-26d,-12d,hidden)
        rtsl
        
        
pupl31  vctr(18d,24d,visible)
        vctr(16d,-24d,visible)
        vctr(-18d,-26d,visible)
        vctr(-16d,26d,visible)
        vctr(-6d,14d,hidden)
        vctr(16d,18d,visible)
        vctr(6d,-10d,visible)
        vctr(-8d,-10d,hidden)
        vctr(-36d,2d,visible)
        vctr(-112d,-52d,visible)
        vctr(134d,-22d,visible)
        rtsl
        
pupl32  vctr(12d,34d,visible)
        vctr(-16d,26d,visible)
        vctr(-4d,0d,visible)
        rtsl
        
wing33  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(18d,-70d,visible)
        vctr(22d,66d,visible)
        vctr(-8d,-24d,hidden)
        vctr(18d,4d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-62d,2d,hidden)
        vctr(0d,4d,visible)
        vctr(18d,-2d,visible)
        vctr(16d,-14d,hidden)
        rtsl

wing30  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(46d,-30d,visible)
        vctr(-6d,26d,visible)
        vctr(6d,-22d,hidden)
        vctr(4d,2d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-54d,4d,hidden)
        vctr(8d,38d,visible)
        vctr(16d,-34d,visible)
        vctr(2d,-18d,hidden)
        rtsl
        
wing31  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(36d,-42d,visible)
        vctr(4d,38d,visible)
        vctr(-2d,-24d,hidden)
        vctr(12d,4d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-56d,4d,hidden)
        vctr(4d,26d,visible)
        vctr(20d,-24d,visible)
        vctr(4d,-16d,hidden)
        rtsl

wing32  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(28d,-56d,visible)
        vctr(12d,52d,visible)
        vctr(-6d,-24d,hidden)
        vctr(16d,4d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-58d,2d,hidden)
        vctr(0d,16d,visible)
        vctr(24d,-14d,visible)
        vctr(6d,-14d,hidden)
        rtsl

pupl40  vctr(26d,-12d,hidden)     ;AD6
        vctr(-8d,12d,visible)
        vctr(8d,12d,visible)
        vctr(-26d,-8d,hidden)
        vctr(-2d,6d,visible)
        vctr(6d,8d,visible)
        vctr(-2d,-18d,hidden)
        rtsl


pupl41  vctr(18d,24d,visible)     ;AE6
        vctr(16d,-22d,visible)
        vctr(-16d,-26d,visible)
        vctr(-18d,24d,visible)
        vctr(8d,10d,hidden)
        vctr(-14d,22d,visible)
        vctr(-16d,-22d,visible)
        vctr(8d,-10d,visible)
        vctr(18d,4d,hidden)
        vctr(-32d,-8d,visible)
        vctr(-82d,-72d,visible)
        vctr(120d,8d,visible)
        rtsl

wing43  jsrl(pupl41)    ;$6AE6
        vctr(18d,10d,visible)
        vctr(-26d,28d,hidden)
        vctr(60d,-62d,visible)
        vctr(-24d,52d,visible)
        vctr(4d,-10d,hidden)
        vctr(2d,2d,visible)
        vctr(2d,34d,visible)
        vctr(-12d,14d,visible)
        vctr(-88d,-26d,hidden)
        vctr(-16d,4d,visible)
        vctr(36d,14d,visible)
        vctr(34d,8d,hidden)
        rtsl
        
wing42  jsrl(pupl41)    ;$6AE6
        vctr(28d,16d,visible)
        vctr(-36d,22d,hidden)
        vctr(68d,-38d,visible)
        vctr(-32d,28d,visible)
        vctr(6d,-6d,hidden)
        vctr(2d,32d,visible)
        vctr(-12d,14d,visible)
        vctr(-82d,-20d,hidden)
        vctr(-20d,18d,visible)
        vctr(46d,0d,visible)
        vctr(22d,2d,hidden)
        rtsl
        
wing41  jsrl(pupl41)    ;$6AE6
        vctr(34d,20d,visible)
        vctr(0d,6d,visible)
        vctr(-42d,12d,hidden)
        vctr(78d,-16d,visible)
        vctr(-42d,6d,visible)
        vctr(6d,2d,hidden)
        vctr(2d,24d,visible)
        vctr(-12d,14d,visible)
        vctr(-76d,-16d,hidden)
        vctr(-22d,32d,visible)
        vctr(44d,-18d,visible)
        vctr(20d,2d,hidden)
        rtsl
        
wing40  jsrl(pupl41)    ;$6AE6
        vctr(34d,20d,visible)
        vctr(0d,10d,visible)
        vctr(-42d,8d,hidden)
        vctr(86d,8d,visible)
        vctr(-50d,-18d,visible)
        vctr(8d,14d,hidden)
        vctr(0d,12d,visible)
        vctr(-12d,14d,visible)
        vctr(-72d,-12d,hidden)
        vctr(-24d,50d,visible)
        vctr(46d,-38d,visible)
        vctr(16d,0d,hidden)
        rtsl
        
pupl50  vctr(-4d,8d,hidden)       ;BA8
        jsrl(pupl51_)   ;$6BB6
        jsrl(pupl51)    ;$6BB2
        vctr(-22d,-18d,hidden)
        rtsl
        
pupl51  vctr(46d,-14d,hidden)     ;BB2
pupl51_ vctr(-6d,-8d,visible)
        vctr(-10d,12d,visible)
        vctr(6d,8d,visible)
        rtsl
        
pupl52  vctr(18d,24d,visible)     ;BBE
        vctr(16d,-22d,visible)
        vctr(-4d,-6d,visible)
        vctr(-30d,6d,hidden)
        vctr(-18d,24d,visible)
        vctr(-16d,-22d,visible)
        vctr(6d,-8d,visible)
        vctr(-16d,-22d,visible)
        vctr(-2d,-84d,visible)
        vctr(78d,48d,visible)
        rtsl
        
pupl53  vctr(-6d,14d,visible)     ;BD8
        vctr(-36d,4d,visible)
        vctr(-24d,-4d,visible)
        rtsl
        
wing53  jsrl(pupl52)    ;$6BBE
        vctr(4d,14d,visible)
        vctr(-12d,8d,hidden)
        vctr(68d,-36d,visible)
        vctr(-54d,58d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-32d,hidden)
        vctr(-56d,-22d,visible)
        vctr(68d,48d,visible)
        vctr(32d,10d,hidden)
        rtsl
        
wing52  jsrl(pupl52)    ;$6BBE
        vctr(6d,20d,visible)
        vctr(-14d,2d,hidden)
        vctr(72d,-10d,visible)
        vctr(-58d,32d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-30d,hidden)
        vctr(-56d,0d,visible)
        vctr(70d,26d,visible)
        vctr(30d,8d,hidden)
        rtsl
        
wing51  jsrl(pupl52)    ;$6BBE
        vctr(8d,24d,visible)
        vctr(-16d,-2d,hidden)
        vctr(80d,16d,visible)
        vctr(-66d,6d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-26d,hidden)
        vctr(-56d,22d,visible)
        vctr(70d,0d,visible)
        vctr(30d,8d,hidden)
        rtsl
        
wing50  jsrl(pupl52)    ;$6BBE
        vctr(10d,30d,visible)
        vctr(-18d,-8d,hidden)
        vctr(82d,42d,visible)
        vctr(-68d,-20d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-22d,hidden)
        vctr(-56d,42d,visible)
        vctr(70d,-22d,visible)
        vctr(30d,6d,hidden)
        rtsl
        
bang0   vctr(-184d,28d,hidden)
        vctr(148d,36d,visible)
        vctr(64d,96d,visible)
        vctr(40d,-112d,visible)
        vctr(144d,-40d,visible)
        vctr(-176d,-52d,visible)
        vctr(-40d,-112d,visible)
        vctr(-56d,140d,visible)
        vctr(-124d,44d,visible)
        vctr(184d,-28d,hidden)
        rtsl
        
ngwi0   vctr(-32d,-56d,hidden)
        vctr(56d,48d,visible)
        vctr(8d,16d,visible)
        vctr(-12d,0d,visible)
        vctr(4d,16d,visible)
        vctr(-24d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-12d,0d,visible)
        vctr(-20d,-96d,visible)
        vctr(32d,56d,hidden)
        rtsl
        
ngwi1   vctr(-64d,0d,hidden)
        vctr(88d,-28d,visible)
        vctr(16d,8d,visible)
        vctr(-6d,12d,visible)
        vctr(14d,6d,visible)
        vctr(-16d,18d,visible)
        vctr(16d,8d,visible)
        vctr(-12d,8d,visible)
        vctr(-100d,-32d,visible)
        vctr(64d,0d,hidden)
        rtsl
        
ngwi2   vctr(-32d,54d,hidden)
        vctr(76d,-66d,visible)
        vctr(-4d,-16d,visible)
        vctr(-16d,12d,visible)
        vctr(-12d,-20d,visible)
        vctr(-12d,12d,visible)
        vctr(-8d,-8d,visible)
        vctr(-12d,12d,visible)
        vctr(-12d,74d,visible)
        vctr(32d,-54d,hidden)
        rtsl
        
ngwi3   vctr(32d,54d,hidden)
        vctr(-76d,-66d,visible)
        vctr(4d,-16d,visible)
        vctr(16d,12d,visible)
        vctr(12d,-20d,visible)
        vctr(12d,12d,visible)
        vctr(8d,-8d,visible)
        vctr(12d,12d,visible)
        vctr(12d,74d,visible)
        vctr(-32d,-54d,hidden)
        rtsl
        
ngwi4   vctr(64d,0d,hidden)
        vctr(-88d,-28d,visible)
        vctr(-16d,8d,visible)
        vctr(6d,12d,visible)
        vctr(-14d,6d,visible)
        vctr(16d,18d,visible)
        vctr(-16d,8d,visible)
        vctr(12d,8d,visible)
        vctr(100d,-32d,visible)
        vctr(-64d,0d,hidden)
        rtsl
        
ngwi5   vctr(32d,-56d,hidden)
        vctr(-56d,48d,visible)
        vctr(-8d,16d,visible)
        vctr(12d,0d,visible)
        vctr(-4d,16d,visible)
        vctr(24d,0d,visible)
        vctr(0d,16d,visible)
        vctr(12d,0d,visible)
        vctr(20d,-96d,visible)
        vctr(-32d,56d,hidden)
        rtsl
        
coil0   vctr(4d,4d,hidden)
        vctr(4d,-4d,visible)
        vctr(-6d,-8d,visible)
        vctr(-10d,8d,visible)
        vctr(12d,16d,visible)
        vctr(20d,-16d,visible)
        vctr(-20d,-24d,visible)
        vctr(-26d,22d,visible)
        vctr(22d,2d,hidden)
        rtsl    
        
coil1   vctr(-2d,6d,hidden)
        vctr(4d,2d,visible)
        vctr(6d,-10d,visible)
        vctr(-10d,-6d,visible)
        vctr(-10d,18d,visible)
        vctr(22d,12d,visible)
        vctr(14d,-26d,visible)
        vctr(-32d,-18d,visible)
        vctr(8d,22d,hidden)
        rtsl
        
coil2   vctr(-6d,2d,hidden)
        vctr(0d,4d,visible)
        vctr(12d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(-22d,0d,visible)
        vctr(0d,26d,visible)
        vctr(30d,0d,visible)
        vctr(0d,-36d,visible)
        vctr(-14d,16d,hidden)
        rtsl
        
coil3   vctr(-4d,-4d,hidden)
        vctr(-4d,4d,visible)
        vctr(6d,8d,visible)
        vctr(10d,-8d,visible)
        vctr(-12d,-16d,visible)
        vctr(-20d,16d,visible)
        vctr(20d,24d,visible)
        vctr(26d,-22d,visible)
        vctr(-22d,-2d,hidden)
        rtsl
        
coil4   vctr(2d,-6d,hidden)
        vctr(-4d,-2d,visible)
        vctr(-6d,10d,visible)
        vctr(10d,6d,visible)
        vctr(10d,-18d,visible)
        vctr(-22d,-12d,visible)
        vctr(-14d,26d,visible)
        vctr(32d,18d,visible)
        vctr(-8d,-22d,hidden)
        rtsl
        
coil5   vctr(6d,-2d,hidden)
        vctr(0d,-4d,visible)
        vctr(-12d,0d,visible)
        vctr(0d,12d,visible)
        vctr(22d,0d,visible)
        vctr(0d,-26d,visible)
        vctr(-30d,0d,visible)
        vctr(0d,36d,visible)
        vctr(14d,-16d,hidden)
        rtsl
        
flap0   vctr(-30d,14d,visible)
        vctr(0d,-8d,visible)
        vctr(14d,-14d,visible)
        vctr(16d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-14d,14d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,-8d,visible)
        rtsl
        
flap1   vctr(-12d,32d,visible)
        vctr(-6d,-6d,visible)
        vctr(0d,-20d,visible)
        vctr(12d,-12d,visible)
        vctr(12d,12d,visible)
        vctr(0d,20d,visible)
        vctr(-12d,6d,visible)
        vctr(-6d,0d,visible)
        rtsl
        
flap2   vctr(14d,30d,visible)
        vctr(-8d,0d,visible)
        vctr(-14d,-14d,visible)
        vctr(0d,-16d,visible)
        vctr(16d,0d,visible)
        vctr(14d,14d,visible)
        vctr(0d,16d,visible)
        vctr(-8d,0d,visible)
        rtsl
        
wdgt0   vctr(12d,20d,hidden)
        vctr(0d,-12d,visible)
        vctr(-16d,-28d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,12d,visible)
        vctr(16d,28d,visible)
        vctr(14d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(-16d,-28d,visible)
        vctr(-6d,0d,visible)
        vctr(-2d,6d,hidden)
        vctr(0d,8d,visible)
        vctr(12d,12d,hidden)
        vctr(0d,8d,visible)
        vctr(-6d,-14d,hidden)
        rtsl

wdgt1   vctr(26d,0d,hidden)
        vctr(-6d,-6d,visible)
        vctr(-40d,0d,visible)
        vctr(-6d,6d,visible)
        vctr(6d,6d,visible)
        vctr(40d,0d,visible)
        vctr(6d,-6d,visible)
        vctr(0d,-6d,visible)
        vctr(-6d,-6d,visible)
        vctr(-40d,0d,visible)
        vctr(-6d,6d,visible)
        vctr(0d,6d,visible)
        vctr(6d,0d,hidden)
        vctr(8d,0d,visible)
        vctr(24d,0d,hidden)
        vctr(8d,0d,visible)
        vctr(-20d,0d,hidden)
        rtsl
        
wdgt2   vctr(-20d,4d,hidden)
        vctr(28d,-16d,visible)
        vctr(12d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-28d,16d,visible)
        vctr(-12d,0d,visible)
        vctr(0d,-14d,visible)
        vctr(28d,-16d,visible)
        vctr(12d,0d,visible)
        vctr(0d,6d,visible)
        vctr(-12d,6d,hidden)
        vctr(8d,0d,visible)
        vctr(-32d,12d,hidden)
        vctr(8d,0d,visible)
        vctr(8d,-6d,hidden)
        rtsl
        
wdgt3   vctr(8d,-16d,hidden)
        vctr(-8d,-4d,visible)
        vctr(-8d,4d,visible)
        vctr(0d,32d,visible)
        vctr(8d,4d,visible)
        vctr(8d,-4d,visible)
        vctr(0d,-38d,visible)
        vctr(-8d,-4d,visible)
        vctr(-8d,4d,visible)
        vctr(0d,6d,visible)
        vctr(8d,0d,hidden)
        vctr(0d,8d,visible)
        vctr(0d,16d,hidden)
        vctr(0d,8d,visible)
        vctr(0d,-16d,hidden)
        rtsl
    
;**************************************
    .sbttl "Clock Parts"
;**************************************
clock_vpg = $60|thispage

sqr     vctr(40d,40d,hidden)
        vctr(0d,-80d,visible)
        vctr(-80d,0d,visible)
        vctr(0d,80d,visible)
        vctr(80d,0d,visible)
        vctr(-16d,-16d,visible)
        vctr(6d,-12d,visible)
        vctr(2d,-12d,visible)
        vctr(-2d,-12d,visible)
        vctr(-6d,-12d,visible)
        vctr(-12d,-6d,visible)
        vctr(-12d,-2d,visible)
        vctr(-12d,2d,visible)
        vctr(-12d,6d,visible)
        vctr(-6d,12d,visible)
        vctr(-2d,12d,visible)
        vctr(2d,12d,visible)
        vctr(6d,12d,visible)
        vctr(12d,6d,visible)
        vctr(12d,2d,visible)
        vctr(12d,-2d,visible)
        vctr(12d,-6d,visible)
        vctr(0d,-48d,hidden)
        vctr(16d,-16d,visible)
        vctr(-80d,0d,hidden)
        vctr(16d,16d,visible)
        vctr(0d,48d,hidden)
        vctr(-16d,16d,visible)
        vctr(40d,-40d,hidden)
        rtsl
        
dial    vctr(0d,24d,hidden)
        vctr(2d,4d,visible)
        vctr(-4d,0d,visible)
        vctr(2d,-4d,visible)
        vctr(24d,-24d,hidden)
        vctr(4d,2d,visible)
        vctr(0d,-4d,visible)
        vctr(-4d,2d,visible)
        vctr(-24d,-24d,hidden)
        vctr(2d,-4d,visible)
        vctr(-4d,0d,visible)
        vctr(2d,4d,visible)
        vctr(-24d,24d,hidden)
        vctr(-4d,-2d,visible)
        vctr(0d,4d,visible)
        vctr(4d,-2d,visible)
        vctr(24d,0d,hidden)
        rtsl
        
;**************************************
    .sbttl "Boots"
;**************************************
boot_vpg = $60|thispage

shoes   vctr(-12d,0d,hidden)
        vctr(0d,12d,visible)
        vctr(2d,2d,visible)
        vctr(8d,0d,visible)
        vctr(-2d,-2d,visible)
        vctr(-8d,0d,visible)
        vctr(10d,2d,hidden)
        vctr(0d,-8d,visible)
        vctr(-1d,-6d,visible)
        vctr(1d,-4d,visible)
        vctr(-2d,-6d,visible)
        vctr(-2d,0d,visible)
        vctr(0d,2d,visible)
        vctr(-2d,-4d,visible)
        vctr(-4d,-4d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,4d,visible)
        vctr(4d,4d,visible)
        vctr(2d,8d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-6d,visible)
        vctr(14d,12d,hidden)
        vctr(0d,6d,visible)
        vctr(-4d,2d,visible)
        vctr(6d,0d,visible)
        vctr(4d,-2d,visible)
        vctr(-6d,0d,visible)
        vctr(6d,0d,hidden)
        vctr(0d,-10d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,-6d,visible)
        vctr(4d,6d,hidden)
        vctr(6d,-6d,visible)
        vctr(6d,-2d,visible)
        vctr(0d,-2d,visible)
        vctr(-4d,-2d,visible)
        vctr(-4d,0d,visible)
        vctr(-6d,2d,visible)
        vctr(0d,-2d,visible)
        vctr(-6d,0d,visible)
        vctr(-2d,4d,visible)
        vctr(3d,5d,visible)
        vctr(-3d,9d,visible)
        vctr(0d,6d,visible)
        rtsl

        
bootz1  vctr(-36d,-70d,hidden)
        vctr(0d,-18d,visible)
        vctr(30d,0d,visible)
        vctr(-6d,18d,visible)
        vctr(26d,0d,hidden)
        vctr(-6d,-18d,visible)
        vctr(30d,0d,visible)
        vctr(0d,18d,visible)
        vctr(-38d,70d,hidden)
        rtsl

        
; bootz2  vctr(-36d,-64d,hidden)
        ; vctr(2d,-18d,visible)
        ; vctr(-12d,-4d,visible)
        ; vctr(0d,-6d,visible)
        ; vctr(14d,0d,visible)
        ; vctr(12d,6d,visible)
        ; vctr(0d,-4d,visible)
        ; vctr(12d,2d,visible)
        ; vctr(0d,8d,visible)
        ; vctr(-4d,4d,visible)
        ; vctr(0d,12d,visible)
        ; vctr(-24d,0d,visible)
        ; vctr(50d,0d,hidden)
        ; vctr(0d,-12d,visible)
        ; vctr(-4d,-4d,visible)
        ; vctr(0d,-8d,visible)
        ; vctr(10d,-2d,visible)
        ; vctr(0d,2d,visible)
        ; vctr(12d,-4d,visible)
        ; vctr(14d,0d,visible)
        ; vctr(0d,6d,visible)
        ; vctr(-10d,4d,visible)
        ; vctr(2d,18d,visible)
        ; vctr(-24d,0d,visible)
        ; vctr(-14d,64d,hidden)
        ; rtsl

        
magic0  vctr(-30d,-96d,hidden)
        vctr(10d,-8d,visible)
        vctr(40d,0d,visible)
        vctr(10d,8d,visible)
        vctr(-30d,96d,hidden)
        rtsl

        
magic1  vctr(-34d,-98d,hidden)
        vctr(14d,-10d,visible)
        vctr(40d,0d,visible)
        vctr(14d,10d,visible)
        vctr(-34d,98d,hidden)
        rtsl    

        
magic2  vctr(-38d,-102d,hidden)
        vctr(18d,-12d,visible)
        vctr(40d,0d,visible)
        vctr(18d,12d,visible)
        vctr(-38d,102d,hidden)
        rtsl
        
magic3  vctr(-46d,-106d,hidden)
        vctr(26d,-18d,visible)
        vctr(40d,0d,visible)
        vctr(26d,18d,visible)
        vctr(-46d,106d,hidden)
        rtsl

        
;************************************
    .sbttl "Stalactite"
;**************************************
tite_vpg = $60|thispage

tite_   vctr(4d,14d,visible)      ;FF2
        vctr(4d,-6d,visible)
        vctr(6d,16d,visible)
        vctr(6d,-24d,visible)
        rtsl        
        
ovhng   vctr(26d,16d,hidden)
        vctr(-52d,0d,visible)
        vctr(6d,-32d,visible)
        jsrl(tite_)     ;$6FF2
        jsrl(tite_)     ;$6FF2
        vctr(6d,32d,visible)
        rtsl
        
;************************************
    .sbttl "Locks and Keys"
;**************************************
lock_vpg = $60|thispage

key     vctr(0d,6d,hidden)
        vctr(4d,2d,visible)
        vctr(0d,5d,visible)
        vctr(-4d,3d,visible)
        vctr(-4d,-3d,visible)
        vctr(0d,-5d,visible)
        vctr(4d,-2d,visible)
        vctr(0d,-18d,visible)
        vctr(-6d,0d,visible)
        vctr(2d,2d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,2d,hidden)
        vctr(-4d,0d,visible)
        rtsl
		
keyp	vctr(-22,0,hidden)
		vctr(0,18,visible)
		vctr(2,2,visible)
		vctr(40,0,visible)
		vctr(2,-2,visible)
		vctr(0,-40,visible)
		vctr(-2,-2,visible)
		vctr(-40,0,visible)
		vctr(-2,2,visible)
		vctr(0,18,visible)
		vstat(sparkle_off,xflip_off,thispage,$F,colred)
		vctr(10,0,hidden)
		jsrl(key)
		vstat(sparkle_off,xflip_off,thispage,$F,colgreen)
		vctr(16,8,hidden)
		jsrl(key)
		vstat(sparkle_off,xflip_off,thispage,$F,colyellow)
		vctr(16,8,hidden)
		jsrl(key)
		rtsl
		
keypbox vctr(0,22,visible)
		vctr(2,2,visible)
		vctr(98,0,visible)
		vctr(2,-2,visible)
		vctr(0,-40,visible)
		vctr(-2,-2,visible)
		vctr(-98,0,visible)
		vctr(-2,2,visible)
		vctr(0,14,visible)
		rtsl
		
        
lock    vctr(1d,0d,hidden)
        vctr(3d,2d,visible)
        vctr(0d,4d,visible)
        vctr(-4d,3d,visible)
        vctr(-4d,-3d,visible)
        vctr(0d,-4d,visible)
        vctr(3d,-2d,visible)
        vctr(-2d,-10d,visible)
        vctr(6d,0d,visible)
        vctr(-2d,10d,visible)
        vctr(-9d,12d,hidden)
        vctr(16d,0d,visible)
        vctr(0d,-26d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,26d,visible)
        vctr(8d,0d,visible)
        vctr(0d,40d,visible)
        vctr(0d,-66d,hidden)
        vctr(0d,-38d,visible)
        rtsl

;************************************
    .sbttl "Transporter"
;**************************************
tran_vpg = $60|thispage
 
; Transporter Booth Main Structure
booth3		vctr(0d,-36d,hidden)
            vctr(4d,4d,visible)
            vctr(0d,64d,visible)
            vctr(-4d,4d,visible)
            vctr(4d,0d,visible)
            vctr(4d,-4d,visible)
            vctr(0d,-64d,visible)
            vctr(-4d,-4d,visible)
            vctr(-48d,0d,visible)
            vctr(0d,-4d,visible)
            vctr(4d,-4d,visible)
            vctr(56d,0d,visible)
            vctr(4d,4d,visible)
            vctr(0d,4d,visible)
            vctr(-2d,4d,visible)
            vctr(0d,64d,visible)
            vctr(2d,4d,visible)
            vctr(0d,4d,visible)
            vctr(-4d,4d,visible)
            vctr(-56d,0d,visible)
            vctr(-4d,-4d,visible)
            vctr(0d,-4d,visible)
            vctr(44d,0d,visible)
            rtsl

;Top and bottom inside lines
booth4		vctr(-4d,-4d,visible)
            vctr(-32d,0d,visible)
            vctr(-4d,4d,visible)
            vctr(0d,-72d,hidden)
            vctr(4d,4d,visible)
            vctr(32d,0d,visible)
            vctr(4d,-4d,visible)
            rtsl

;Two Transporter Circles			
booth5		vctr(10d,0d,hidden)
            jsrl(boothdot)
            vctr(0d,75d,hidden)
            jsrl(boothdot)
            vctr(-30d,-39d,hidden)
            rtsl

;Actual booth dot            
boothdot	vctr(3d,2d,visible)
            vctr(3d,-2d,visible)
            vctr(0d,-3d,visible)
            vctr(-3d,-2d,visible)
            vctr(-3d,2d,visible)
            vctr(0d,3d,visible)
            rtsl
        
star0   vctr(2d,0d,hidden)
        vctr(-4d,0d,visible)
        vctr(2d,-2d,hidden)
        vctr(0d,4d,visible)
        vctr(0d,-2d,hidden)
        rtsl

star1   vctr(4d,0d,hidden)
        vctr(-8d,0d,visible)
        vctr(4d,-4d,hidden)
        vctr(0d,8d,visible)
        vctr(0d,-4d,hidden)
        rtsl

star2   vctr(6d,0d,hidden)
        vctr(-12d,0d,visible)
        vctr(6d,-6d,hidden)
        vctr(0d,12d,visible)
        vctr(0d,-6d,hidden)
        rtsl

star3   vctr(8d,0d,hidden)
        vctr(-16d,0d,visible)
        vctr(8d,-8d,hidden)
        vctr(0d,16d,visible)
        vctr(0d,-8d,hidden)
        rtsl

;************************************
;* Hand Parts
;**************************************
hand_vpg = $60|thispage 

hand    vctr(6d,0d,visible)
        vctr(4d,2d,visible)
        vctr(22d,0d,visible)
        vctr(0d,-2d,visible)
        vctr(-4d,-2d,visible)
        vctr(-10d,0d,visible)
        vctr(0d,4d,hidden)
        vctr(0d,-6d,visible)
        vctr(-4d,0d,visible)
        vctr(-2d,0d,hidden)
        vctr(2d,4d,visible)
        vctr(0d,-6d,visible)
        vctr(8d,0d,visible)
        vctr(0d,4d,visible)
        vctr(-2d,-4d,hidden)
        vctr(0d,-4d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,4d,visible)
        vctr(2d,0d,visible)
        vctr(4d,-4d,hidden)
        vctr(0d,-4d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,4d,visible)
        vctr(0d,-4d,hidden)
        vctr(-4d,0d,visible)
        vctr(-4d,2d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,12d,visible)
        rtsl
        
box     vctr(16d,0d,visible)
        vctr(0d,-16d,visible)
        vctr(-32d,0d,visible)
        vctr(0d,16d,visible)
        vctr(16d,0d,visible)
        vctr(-2d,0d,hidden)
        rtsl

swtch0  vctr(8d,16d,visible)
        vctr(4d,0d,visible)
        vctr(-8d,-16d,visible)
        vctr(-2d,-16d,hidden)
        rtsl

swtch1  vctr(-8d,16d,visible)
        vctr(4d,0d,visible)
        vctr(8d,-16d,visible)
        vctr(-2d,-16d,hidden)
        rtsl

;************************************
    .sbttl "Escape Pod"
;**************************************
pod_vpg = $60|thispage

escpod  vctr(-24d,-144d,hidden)
        vctr(-72d,24d,visible)
        vctr(0d,216d,visible)
        vctr(96d,96d,visible)
        vctr(96d,-96d,visible)
        vctr(0d,-216d,visible)
        vctr(-72d,-24d,visible)
        vctr(24d,-32d,visible)
        vctr(-96d,0d,visible)
        vctr(24d,32d,visible)
        vctr(24d,144d,hidden)
        rtsl
        
flame0  vctr(-8d,-192d,hidden)
        vctr(8d,-24d,visible)
        vctr(8d,24d,visible)
        vctr(-8d,192d,hidden)
        rtsl

flame1  vctr(-16d,-192d,hidden)
        vctr(16d,-48d,visible)
        vctr(16d,48d,visible)
        vctr(-16d,192d,hidden)
        rtsl
        
flame2  vctr(-24d,-192d,hidden)
        vctr(24d,-72d,visible)
        vctr(24d,72d,visible)
        vctr(-24d,192d,hidden)
        rtsl
        
flame3  vctr(-32d,-192d,hidden)
        vctr(32d,-96d,visible)
        vctr(32d,96d,visible)
        vctr(-32d,192d,hidden)
        rtsl
        
crash0  vctr(8d,-144d,hidden)
        vctr(-16d,-40d,visible)
        vctr(88d,24d,visible)
        vctr(-24d,24d,visible)
        vctr(64d,40d,visible)
        vctr(-40d,208d,visible)
        vctr(-120d,80d,visible)
        vctr(-72d,-112d,visible)
        vctr(40d,-216d,visible)
        vctr(80d,-8d,visible)
        vctr(-8d,144d,hidden)
        rtsl
        
flame4  vctr(32d,-192d,hidden)
        vctr(12d,-24d,visible)
        vctr(4d,28d,visible)
        vctr(-48d,188d,hidden)
        rtsl

flame5  vctr(24d,-192d,hidden)
        vctr(24d,-40d,visible)
        vctr(8d,48d,visible)
        vctr(-56d,184d,hidden)
        rtsl
        
flame6  vctr(16d,-192d,hidden)
        vctr(36d,-60d,visible)
        vctr(12d,72d,visible)
        vctr(-64d,180d,hidden)
        rtsl
        
flame7  vctr(8d,-192d,hidden)
        vctr(48d,-88d,visible)
        vctr(16d,104d,visible)
        vctr(-72d,176d,hidden)
        rtsl
        
crash1  vctr(40d,-144d,hidden)
        vctr(-8d,-40d,visible)
        vctr(88d,40d,visible)
        vctr(-32d,16d,visible)
        vctr(56d,48d,visible)
        vctr(-80d,200d,visible)
        vctr(-128d,56d,visible)
        vctr(-48d,-120d,visible)
        vctr(80d,-208d,visible)
        vctr(72d,8d,visible)
        vctr(-40d,144d,hidden)
        rtsl
        
flame8  vctr(64d,-180d,hidden)
        vctr(16d,-16d,visible)
        vctr(0d,20d,visible)
        vctr(-80d,176d,hidden)
        rtsl

flame9  vctr(56d,-184d,hidden)
        vctr(32d,-36d,visible)
        vctr(0d,52d,visible)
        vctr(-88d,168d,hidden)
        rtsl
        
flamea  vctr(48d,-188d,hidden)
        vctr(48d,-52d,visible)
        vctr(0d,76d,visible)
        vctr(-96d,164d,hidden)
        rtsl
        
flameb  vctr(40d,-192d,hidden)
        vctr(64d,-72d,visible)
        vctr(0d,100d,visible)
        vctr(-104d,164d,hidden)
        rtsl
        
crash2  vctr(88d,-120d,hidden)
        vctr(8d,-40d,visible)
        vctr(64d,72d,visible)
        vctr(-40d,0d,visible)
        vctr(32d,72d,visible)
        vctr(-152d,152d,visible)
        vctr(-136d,0d,visible)
        vctr(0d,-136d,visible)
        vctr(152d,-144d,visible)
        vctr(72d,32d,visible)
        vctr(-88d,120d,hidden)
        rtsl
        
flamec  vctr(136d,-136d,hidden)
        vctr(24d,-16d,visible)
        vctr(-16d,24d,visible)
        vctr(-144d,128d,hidden)
        rtsl

flamed  vctr(128d,-144d,hidden)
        vctr(48d,-24d,visible)
        vctr(-28d,48d,visible)
        vctr(-148d,120d,hidden)
        rtsl
        
flamee  vctr(124d,-148d,hidden)
        vctr(68d,-36d,visible)
        vctr(-36d,68d,visible)
        vctr(-156d,116d,hidden)
        rtsl
        
flamef  vctr(116d,-156d,hidden)
        vctr(92d,-44d,visible)
        vctr(-48d,96d,visible)
        vctr(-160d,104d,hidden)
        rtsl
        
crash3  vctr(144d,-24d,hidden)
        vctr(-24d,-72d,visible)
        vctr(-216d,0d,visible)
        vctr(-96d,96d,visible)
        vctr(96d,96d,visible)
        vctr(216d,0d,visible)
        vctr(24d,-72d,visible)
        vctr(32d,24d,visible)
        vctr(0d,-96d,visible)
        vctr(-32d,24d,visible)
        vctr(-144d,24d,hidden)
        rtsl
        
crash4  vctr(88d,120d,hidden)
        vctr(8d,40d,visible)
        vctr(64d,-72d,visible)
        vctr(-40d,0d,visible)
        vctr(32d,-72d,visible)
        vctr(-152d,-152d,visible)
        vctr(-136d,0d,visible)
        vctr(0d,136d,visible)
        vctr(152d,144d,visible)
        vctr(72d,-32d,visible)
        vctr(-88d,-120d,hidden)
        rtsl
        
crash5  vctr(40d,144d,hidden)
        vctr(-8d,40d,visible)
        vctr(88d,-40d,visible)
        vctr(-32d,-16d,visible)
        vctr(56d,-48d,visible)
        vctr(-80d,-200d,visible)
        vctr(-128d,-56d,visible)
        vctr(-48d,120d,visible)
        vctr(80d,208d,visible)
        vctr(72d,-8d,visible)
        vctr(-40d,-144d,hidden)
        rtsl
        
crash6  vctr(8d,144d,hidden)
        vctr(-16d,40d,visible)
        vctr(88d,-24d,visible)
        vctr(-24d,-24d,visible)
        vctr(64d,-40d,visible)
        vctr(-40d,-208d,visible)
        vctr(-120d,-80d,visible)
        vctr(-72d,112d,visible)
        vctr(40d,216d,visible)
        vctr(80d,8d,visible)
        vctr(-8d,-144d,hidden)
        rtsl

        
crash7  vctr(-72d,-160d,hidden)
        vctr(-64d,80d,visible)
        vctr(-56d,-8d,visible)
        vctr(16d,32d,visible)
        vctr(0d,-16d,visible)
        vctr(32d,16d,visible)
        vctr(-16d,-16d,visible)
        vctr(16d,0d,visible)
        vctr(48d,-16d,visible)
        vctr(-8d,24d,visible)
        vctr(40d,16d,visible)
        vctr(-48d,24d,visible)
        vctr(-8d,48d,visible)
        vctr(72d,32d,visible)
        vctr(-24d,0d,hidden)
        vctr(8d,72d,visible)
        vctr(80d,8d,visible)
        vctr(-16d,32d,visible)
        vctr(32d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(64d,-16d,visible)
        vctr(-32d,-32d,visible)
        vctr(72d,-32d,visible)
        vctr(8d,8d,visible)
        vctr(0d,8d,visible)
        vctr(8d,-40d,hidden)
        vctr(8d,8d,visible)
        vctr(-12d,8d,visible)
        vctr(-16d,-8d,visible)
        vctr(-20d,-72d,visible)
        vctr(-40d,-8d,visible)
        vctr(16d,-8d,hidden)
        vctr(24d,-16d,visible)
        vctr(-24d,-60d,visible)
        vctr(8d,-12d,visible)
        vctr(-8d,0d,hidden)
        vctr(-48d,-16d,visible)
        vctr(16d,-16d,visible)
        vctr(-40d,-16d,visible)
        rtsl


;**************************************
    .sbttl "Extra Life"
;**************************************
live_vpg    = thispage

live    vctr(6d,0d,visible)
        vctr(2d,8d,visible)
        vctr(2d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(-4d,14d,visible)
        vctr(8d,0d,visible)
        vctr(-10d,6d,visible)
        vctr(2d,4d,visible)
        vctr(-4d,2d,visible)
        vctr(-4d,-2d,visible)
        vctr(2d,-4d,visible)
        vctr(-10d,-6d,visible)
        vctr(8d,0d,visible)
        vctr(-4d,-14d,visible)
        vctr(24d,0d,hidden)
        rtsl


;Special Tokens
tok_vpg = $60|thispage
         
raditok vctr(5d,9d,hidden)
		vctr(-10d,-18d,visible)   
        vctr(-5d,9d,visible)      
        vctr(20d,0d,visible)
        vctr(-5d,-9d,visible)
        vctr(-10d,18d,visible)
        vctr(10d,0d,visible)
		vctr(15d,-8d,hidden)
        rtsl    ;?,-8 net
		
startok vctrl(0d,13d,hidden)
		vctrl(10d,-25d,visible)
		vctrl(-23d,16d,visible)
		vctr(26d,0d,visible)
		vctrl(-23d,-16d,visible)
		vctr(10d,25d,visible)
		vctr(20d,-14d,hidden)
		rtsl
		
cubetok vctrl(7d,11d,hidden) ;-6
		vctrl(-14d,0d,visible)	;top
		vctrl(-6d,-11d,visible)
		vctrl(6d,-11d,visible)
		vctrl(14d,0d,visible)
		vctrl(6d,11d,visible)
		vctrl(-6d,11d,visible)
		vctrl(-14d,-22d,visible)
		vctrl(0d,22d,hidden)
		vctrl(14d,-22d,visible)
		vctrl(-20d,11d,hidden)
		vctrl(26d,0d,visible)
		vctr(7d,0d,hidden)
		rtsl
		
fuzetok vctrl(7d,11d,hidden) ;-6
		vctrl(-14d,-22d,visible)
		vctrl(0d,22d,hidden)
		vctrl(14d,-22d,visible)
		vctrl(-20d,11d,hidden)
		vctrl(26d,0d,visible)
		vctr(7d,0d,hidden)
		rtsl

hometok jsrl(hmtok1)
		vstat(sparkle_off,xflip_off,thispage,$F,colgreenr)
		jsrl(hmtok2)
		vstat(sparkle_off,xflip_off,thispage,$F,colred)
		jsrl(hmtok3)
		rtsl
		
hmtok1	vctrl(-14d,6d,hidden) 
		;from lifech in vrom for little rex
		vctr(4d,4d,visible)
		vctr(0d,4d,visible)
		vctr(4d,0d,visible)
		vctr(4d,4d,visible)
		vctr(4d,-4d,visible)
		vctr(4d,0d,visible)
		vctr(0d,-4d,visible)
		vctr(4d,-4d,visible)
		vctr(-4d,-4d,visible)
		vctr(0d,-4d,visible)
		vctr(-4d,0d,visible)
		vctr(-4d,-4d,visible)
		vctr(-4d,4d,visible)
		vctr(-4d,0d,visible)
		vctr(0d,4d,visible)
		vctr(-4d,4d,visible)
		vctr(7d,-8d,hidden)
		rtsl
		
hmtok2	vctr(-4d,-13d,visible)
		vctr(4d,1d,visible)
		vctr(2d,-4d,visible)
		vctr(3d,9d,visible)
		vctr(-1d,4d,hidden)
		vctr(4d,-13d,visible)
		vctr(2d,4d,visible)
		vctr(4d,-1d,visible)
		vctr(-4d,13d,visible)
		vctr(-3d,2d,hidden)
		rtsl 
		
hmtok3	vctr(-4d,0d,visible)
		vctr(-4d,4d,visible)
		vctr(0d,4d,visible)
		vctr(4d,4d,visible)
		vctr(4d,0d,visible)
		vctr(4d,-4d,visible)
		vctr(0d,-4d,visible)
		vctr(-4d,-4d,visible)
		rtsl
		
		; vctr(-1d,3d,hidden)
		; vctr(-2d,0d,visible)
		; vctr(-2d,2d,visible)
		; vctr(0d,2d,visible)
		; vctr(2d,2d,visible)
		; vctr(2d,0d,visible)
		; vctr(2d,-2d,visible)
		; vctr(0d,-2d,visible)
		; vctr(-2d,-2d,visible)
		; rtsl

tokhex  vctr(-10d,17d,visible)
        vctr(-20d,0d,visible)
        vctr(-10d,-17d,visible)
        vctr(10d,-17d,visible)
        vctr(20d,0d,visible)
        vctr(10d,17d,visible)
		rtsl

;****************************************************
    .sbttl "Smart Bomb Explosions"
;****************************************************
smtb_vpg    =   $60|thispage

smtb0   vctr(0d,19d,hidden)
        vctr(-10d,-24d,visible)
        vctr(6d,-6d,visible)
        vctr(7d,1d,visible)
        vctr(4d,3d,visible)
        vctr(-4d,7d,visible)
        vctr(-6d,2d,visible)
        vctr(-7d,-7d,visible)
        vctr(-12d,3d,hidden)
        vctr(29d,-5d,visible)
        vctr(-3d,-13d,hidden)
        vctr(9d,24d,visible)
        vctr(11d,-2d,hidden)
        vctr(-28d,7d,visible)
        vctr(10d,-3d,hidden)
        vctr(-3d,-6d,visible)
        vctr(-5d,8d,hidden)
        vctr(-2d,-6d,visible)
        vctr(4d,-2d,hidden)
        rtsl
        
smtb1   vctr(-8d,16d,hidden)
        vctr(0d,-23d,visible)
        vctr(6d,-4d,visible)
        vctr(7d,1d,visible)
        vctr(6d,5d,visible)
        vctr(-6d,7d,visible)
        vctr(-7d,0d,visible)
        vctr(-6d,-9d,visible)
        vctr(-12d,0d,hidden)
        vctr(31d,2d,visible)
        vctr(1d,-13d,hidden)
        vctr(0d,21d,visible)
        vctr(-7d,5d,visible)
        vctr(0d,-6d,visible)
        vctr(18d,6d,hidden)
        vctr(-25d,1d,visible)
        vctr(-6d,-5d,visible)
        vctr(6d,5d,hidden)
        vctr(0d,-7d,visible)
        vctr(2d,-2d,hidden)
        rtsl
        
smtb2   vctr(-20d,18d,hidden)
        vctr(16d,-27d,visible)
        vctr(4d,-3d,visible)
        vctr(8d,5d,visible)
        vctr(5d,6d,visible)
        vctr(-6d,6d,visible)
        vctr(-7d,-2d,visible)
        vctr(-4d,-12d,visible)
        vctr(-13d,-7d,hidden)
        vctr(31d,15d,visible)
        vctr(7d,-12d,hidden)
        vctr(-16d,26d,visible)
        vctr(14d,5d,hidden)
        vctr(-31d,-13d,visible)
        vctr(8d,4d,hidden)
        vctr(4d,-7d,visible)
        vctr(3d,10d,hidden)
        vctr(4d,-6d,visible)
        vctr(-7d,-6d,hidden)
        rtsl

smtb3   vctr(-23d,2d,hidden)
        vctr(27d,-12d,visible)
        vctr(0d,10d,visible)
        vctr(-7d,4d,visible)
        vctr(-8d,-7d,visible)
        vctr(8d,-8d,visible)
        vctr(8d,-4d,visible)
        vctr(6d,6d,visible)
        vctr(5d,9d,visible)
        vctr(-21d,-21d,visible)
        vctr(32d,18d,hidden)
        vctr(-26d,12d,visible)
        vctr(8d,7d,hidden)
        vctr(-20d,-19d,visible)
        vctr(20d,8d,hidden)
        vctr(-5d,-5d,visible)
        vctr(-4d,0d,hidden)
        rtsl



;****************************************
    .sbttl "Base Stations"
;****************************************
bas_vpg =   $60|thispage

ltlsh   vctr(-256d,0d,hidden)
        vctr(48d,144d,visible)
        vctr(112d,112d,visible)
        vctr(192d,0d,visible)
        vctr(112d,-112d,visible)
        vctr(48d,-144d,visible)
        vctr(-128d,-128d,visible)
        vctr(-256d,0d,visible)
        vctr(-128d,128d,visible)
        vctr(512d,0d,visible)
        vctr(-144d,208d,visible)
        vctr(-224d,0d,visible)
        vctr(-144d,-224d,visible)
        vctr(144d,224d,hidden)
        vctr(16d,48d,visible)
        vctr(208d,-48d,hidden)
        vctr(-16d,48d,visible)
        vctr(-96d,-256d,hidden)
        vstat(sparkle_off,xflip_off,thispage,$C,colyellow)
        jsrl(xtra1)
        rtsl
        
xtra1   vctr(0d,64d,hidden)
        vctr(64d,64d,visible)
        vctr(-128d,0d,visible)
        vctr(64d,-64d,visible)
        vctr(-144d,96d,hidden)
        vctr(0d,16d,visible)
        vctr(-32d,-48d,visible)
        vctr(32d,32d,visible)
        vctr(320d,0d,hidden)
        vctr(0d,16d,visible)
        vctr(32d,-48d,visible)
        vctr(-32d,32d,visible)
        vctr(-160d,-160d,hidden)
        rtsl
        
hexsh   vctr(-128d,-88d,hidden)
        vctr(-80d,96d,visible)
        vctr(0d,120d,visible)
        vctr(120d,72d,visible)
        vctr(176d,0d,visible)
        vctr(120d,-72d,visible)
        vctr(0d,-120d,visible)
        vctr(-80d,-96d,visible)
        vctr(-256d,0d,visible)
        vctr(64d,-16d,visible)
        vctr(128d,0d,visible)
        vctr(64d,16d,visible)
        vctr(0d,136d,visible)
        vctr(80d,80d,visible)
        vctr(-128d,48d,visible)
        vctr(-24d,-24d,visible)
        vctr(64d,-104d,visible)
        vctr(-256d,0d,visible)
        vctr(0d,-136d,visible)
        vctr(64d,288d,hidden)
        vctr(-24d,-24d,visible)
        vctr(-120d,-48d,visible)
        vctr(80d,-80d,visible)
        vctr(64d,104d,visible)
        vctr(-24d,24d,visible)
        vctr(24d,-24d,hidden)
        vctr(128d,0d,visible)
        vctr(24d,24d,hidden)
        vctr(-24d,24d,visible)
        vctr(-64d,-200d,hidden)
        vstat(sparkle_off,xflip_off,thispage,$C,colyellow)
        jsrl(xtra2)
        rtsl
        
xtra2   vctr(-64d,112d,hidden)
        vctr(8d,16d,visible)
        vctr(112d,0d,visible)
        vctr(8d,-16d,visible)
        vctr(-128d,0d,visible)
        vctr(-64d,-24d,hidden)
        vctr(0d,24d,visible)
        vctr(-16d,16d,visible)
        vctr(-24d,0d,visible)
        vctr(32d,8d,visible)
        vctr(16d,-16d,visible)
        vctr(-8d,-32d,visible)
        vctr(256d,0d,hidden)
        vctr(0d,24d,visible)
        vctr(16d,16d,visible)
        vctr(24d,0d,visible)
        vctr(-32d,8d,visible)
        vctr(-16d,-16d,visible)
        vctr(8d,-32d,visible)
        vctr(-128d,-88d,hidden)
        rtsl
        
ftrsh   vctr(72d,18d,visible)
        vctr(30d,-18d,visible)
        vctr(156d,42d,visible)
        vctr(0d,150d,visible)
        vctr(-60d,54d,visible)
        vctr(-126d,24d,visible)
        vctr(-144d,0d,visible)
        vctr(-126d,-24d,visible)
        vctr(-60d,-54d,visible)
        vctr(0d,-150d,visible)
        vctr(156d,-42d,visible)
        vctr(30d,18d,visible)
        vctr(72d,-18d,visible)
        vctr(0d,126d,visible)
        vctr(258d,66d,visible)
        vctr(-162d,54d,visible)
        vctr(-96d,-24d,visible)
        vctr(-96d,24d,visible)
        vctr(-162d,-54d,visible)
        vctr(258d,-66d,visible)
        vctr(0d,60d,hidden)
        vstat(sparkle_off,xflip_off,thispage,$C,colyellow)
        vctr(54d,12d,visible)
        vctr(18d,24d,visible)
        vctr(-72d,-18d,visible)
        vctr(-72d,18d,visible)
        vctr(18d,-24d,visible)
        vctr(54d,-12d,visible)
        vctr(96d,60d,hidden)
        vstat(sparkle_off,xflip_off,thispage,$C,colgreen)
        vctr(-24d,24d,visible)
        vctr(-144d,0d,hidden)
        vctr(-24d,-24d,visible)
        vctr(96d,-246d,hidden)
        rtsl
        
dmbsh   vctr(144d,208d,hidden)
        vctr(-96d,16d,visible)
        vctr(-96d,0d,visible)
        vctr(-96d,-16d,visible)
        jsrl(edge)
        jsrl(tops)
        vctr(-384d,-96d,hidden)
        vctr(0d,-48d,visible)
        jsrl(edge)
        vctr(0d,48d,visible)
        vctr(0d,-48d,hidden)
        jsrl(tops)
        vctr(-112d,80d,hidden)
        vstat(sparkle_off,xflip_off,thispage,$C,colyellow)
        vctr(0d,16d,visible)
        vctr(-48d,-16d,visible)
        vctr(0d,-16d,visible)
        vctr(48d,16d,visible)
        vctr(-96d,-24d,hidden)
        vctr(0d,16d,visible)
        vctr(-64d,0d,visible)
        vctr(0d,-16d,visible)
        vctr(64d,0d,visible)
        vctr(-112d,8d,hidden)
        vctr(0d,16d,visible)
        vctr(-48d,16d,visible)
        vctr(0d,-16d,visible)
        vctr(48d,-16d,visible)
        vstat(sparkle_off,xflip_off,thispage,$C,colgreen)
        jsrl(xtra3)
        rtsl

edge    vctr(0d,-16d,visible)
        vctr(80d,-32d,visible)
        vctr(128d,0d,visible)
        vctr(80d,32d,visible)
        vctr(0d,16d,visible)
        rtsl
        
tops    vctr(96d,-48d,visible)
        vctr(0d,-56d,visible)
        vctr(-96d,-72d,visible)
        vctr(-80d,-32d,visible)
        vctr(-128d,0d,visible)
        vctr(-80d,32d,visible)
        vctr(-96d,72d,visible)
        vctr(0d,56d,visible)
        vctr(96d,48d,visible)
        vctr(-96d,-80d,hidden)
        vctr(128d,-64d,visible)
        vctr(224d,0d,visible)
        vctr(128d,64d,visible)
        rtsl
        
xtra3   vctr(16d,-32d,hidden)
        vctr(-48d,-96d,visible)
        vctr(224d,0d,hidden)
        vctr(-48d,96d,visible)
        vctr(0d,64d,hidden)
        vctr(48d,64d,visible)
        vctr(-224d,0d,hidden)
        vctr(48d,-64d,visible)
        vctr(64d,0d,hidden)
        rtsl

;Final Space Station (VAX Cube)
;Zoom 1.4
hwsh	vstat(sparkle_off, xflip_off, thispage,$C, colwhite)
		vctr(370d, 204d, hidden)
		vctr(-42d, -431d, visible)
		vctr(-387d, -192d, visible)
		vctr(-17d, 473d, visible)
		vctr(447d, 150d, visible)
		vctr(-321d, 140d, visible)
		vctr(-403d, -100d, visible)
		vctr(277d, -190d, hidden)
		vctr(-277d, 190d, visible)
		vctr(48d, -413d, visible)
		vctr(246d, -249d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colcyan)
		vctr(424d, 562d, hidden)
		vctr(-438d, -157d, visible)
		vctr(444d, 217d, hidden)
		vctr(-446d, -150d, visible)
		vctr(446d, 150d, hidden)
		vctr(-5d, -59d, visible)
		vctr(-441d, -91d, hidden)
		vctr(2d, -66d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colwhite)
		vctr(236d, 85d, hidden)
		vctr(-15d, -387d, visible)
		vctr(187d, 420d, hidden)
		vctr(-3d, -43d, visible)
		vctr(-22d, 33d, hidden)
		vctr(-3d, -43d, visible)
		vctr(-23d, 33d, hidden)
		vctr(-3d, -43d, visible)
		vctr(-24d, 33d, hidden)
		vctr(-2d, -44d, visible)
		vctr(-25d, 33d, hidden)
		vctr(-2d, -44d, visible)
		vctr(-26d, 33d, hidden)
		vctr(-2d, -44d, visible)
		vctr(-62d, 20d, hidden)
		vctr(-1d, -45d, visible)
		vctr(-29d, 33d, hidden)
		vctr(0d, -45d, visible)
		vctr(-31d, 33d, hidden)
		vctr(0d, -46d, visible)
		vctr(-32d, 33d, hidden)
		vctr(0d, -46d, visible)
		vctr(-33d, 33d, hidden)
		vctr(0d, -46d, visible)
		vctr(-34d, 34d, hidden)
		vctr(1d, -47d, visible)
		vctr(56d, 113d, hidden)
		vctr(-24d, 31d, visible)
		vctr(24d, 8d, hidden)
		vctr(-24d, -48d, visible)
		vctr(-6d, -2d, hidden)
		vctr(-13d, 35d, visible)
		vctr(-11d, -44d, visible)
		vctr(0d, 40d, hidden)
		vctr(-11d, -44d, visible)
		vctr(-13d, 36d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colred)
		vctr(405d, 120d, hidden)
		vctr(-19d, -6d, visible)
		vctr(-69d, -24d, hidden)
		vctr(-20d, -7d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colgreen)
		vctr(80d, 28d, hidden)
		vctr(-19d, -6d, visible)
		vctr(-9d, -3d, hidden)
		vctr(-20d, -7d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colcyan)
		vctr(-358d, -153d, hidden)
		vctr(-273d, 200d, visible)
		vctr(270d, -133d, hidden)
		vctr(-277d, 190d, visible)
		vctr(277d, -190d, hidden)
		vctr(2d, -66d, visible)
		vctr(-279d, 256d, hidden)
		vctr(6d, -56d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colwhite)
		vctr(121d, -89d, hidden)
		vctr(31d, -379d, visible)
		vctr(98d, 252d, hidden)
		vctr(2d, -46d, visible)
		vctr(-24d, 63d, hidden)
		vctr(2d, -46d, visible)
		vctr(-24d, 62d, hidden)
		vctr(2d, -45d, visible)
		vctr(-23d, 61d, hidden)
		vctr(2d, -45d, visible)
		vctr(-23d, 60d, hidden)
		vctr(3d, -44d, visible)
		vctr(-22d, 59d, hidden)
		vctr(3d, -44d, visible)
		vctr(-43d, 74d, hidden)
		vctr(3d, -43d, visible)
		vctr(-21d, 57d, hidden)
		vctr(3d, -43d, visible)
		vctr(-21d, 56d, hidden)
		vctr(4d, -42d, visible)
		vctr(-21d, 55d, hidden)
		vctr(4d, -42d, visible)
		vctr(-20d, 54d, hidden)
		vctr(4d, -41d, visible)
		vctr(-20d, 53d, hidden)
		vctr(4d, -41d, visible)
		vctr(18d, 61d, hidden)
		vctr(-15d, 44d, visible)
		vctr(11d, -8d, hidden)
		vctr(-8d, -27d, visible)
		vctr(-2d, 2d, hidden)
		vctr(-9d, 39d, visible)
		vctr(-1d, -31d, visible)
		vctr(-3d, 35d, hidden)
		vctr(-1d, -31d, visible)
		vctr(-9d, 39d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colred)
		vctr(252d, -194d, hidden)
		vctr(-16d, 11d, visible)
		vctr(-54d, 38d, hidden)
		vctr(-14d, 10d, visible)
		vstat(sparkle_off, xflip_off, thispage,$C, colgreen)
		vctr(61d, -43d, hidden)
		vctr(-15d, 11d, visible)
		vctr(-7d, 5d, hidden)
		vctr(-15d, 10d, visible)
		rtsl


;****************************************************
;This is the 'firework' graphic from Space Duel
;Used in High Score screen
;****************************************************
flare_vpg = $60 | thispage

flare
	;vscal(ywin_off,binscal2,$0)
	;This was in the CPU code in SD
	;vstat(sparkle_off,xflip_off,thispage,$F,colwhite)
	;COLOR WHITE,15.
	vscal(ywin_off,binscal0,$0)
	;SCAL 0,0
	vctr(0d,-2d,visible)
	;VCTR 0,-2,1
	vstat(sparkle_off,xflip_off,thispage,$F,colyellow)
	;COLOR YELLOW,15.
	vctr(0d,-3d,visible)
	;VCTR 0,-3,1
	vstat(sparkle_off,xflip_off,thispage,$A,colyellow)
	;COLOR YELLOW,10.
	vctr(0d,-2d,visible)
	;VCTR 0,-2,1
	vstat(sparkle_off,xflip_off,thispage,$C,colred)
	;COLOR RED,12.
	vctr(0d,-2d,visible)
	;VCTR 0,-2,1
	vstat(sparkle_off,xflip_off,thispage,$A,colred)
	;COLOR RED,10.
	vctr(0d,-4d,visible)
	;VCTR 0,-4,1
	rtsl
	;RTSL	


;Homeworld Horizon Scene ala Battlezone
horiz_vpg = $60 | thispage

;600 X is the left edge, kick it out a little more to make it full screen
;may need to tweak this on the actual monitor.
horiz
	vcntr
	vctrl(-620,-1,hidden)		;down 1 pixel
	vctrl(1240,0,visible)
	rtsl
	
;left position for mountain starts
mntnpo
	vcntr
	vctrl(-620d,0d,hidden)		;Ready to draw actual mountains
	rtsl

;foreground terrain	position
fterrap
	vcntr
	vctrl(-620d,-350d,hidden)		;Ready to draw foreground actual mountains
	rtsl

;foreground terrain 	
fterra
	vctrl(48,-96,visible)	;1
	vctrl(16,16,visible)	;2
	vctrl(48,-128,visible)	;3
	vctrl(-16,48,hidden)	;4
	vctrl(64,48,visible)	;5
	vctrl(192,-48,visible)	;6
	vctrl(-32,8,hidden)		;7
	vctrl(96,72,visible)	;8
	vctrl(128,-96,visible)	;9
	vctrl(-32,24,hidden)	;10
	vctrl(128,-16,visible)	;11
	vctrl(-64,8,hidden)		;12
	vctrl(96,24,visible)	;13
	vctrl(96,-32,visible)	;14
	vctrl(-36,16,hidden)	;15
	vctrl(64,64,visible)	;
	vctrl(192,-64,visible)	;
	vctrl(-48,16,hidden)	;
	vctrl(64,64,visible)	;
	vctrl(192,-64,visible)	;
	vctrl(-48,16,hidden)	;
	vctrl(192,160,visible)	
	rtsl
	

volpart
	vctr(2d,0d,visible)
    vctr(-1d,-1d,hidden)
    vctr(0d,2d,visible)
    rtsl

	
       
;*************************************
    .sbttl "Elevator for Homeworld"
;*************************************
elev_vpg =     $60|thispage

;main building
elevat  ;bottom cube
        vctrl(-125d,0d,visible)
        vctrl(0d,145d,visible)
        vctrl(250d,0d,visible)
        vctrl(0d,-145d,visible)
		vctrl(-125d,0d,visible)
		;elevator door border
        vctrl(65d,0d,hidden)
        vctrl(0d,125d,visible)
        vctrl(-130d,0d,visible)
        vctrl(0d,-125d,visible)
		;roof
        vctrl(-60d,125d,hidden)  	;125+60+30 high
        vctr(-20d,-20d,visible)		;-80 to the left of center
        vctrl(-40d,40d,visible)
        vctrl(40d,40d,visible)
        vctrl(290d,0d,visible)
        vctrl(-290d,0d,hidden)		;back to do square top
        vctrl(0d,30d,visible)
        vctrl(290d,0d,visible)
        vctrl(0d,-30d,visible)
        vctrl(40d,-40d,visible)
        vctrl(-40d,-40d,visible)
        vctr(-20d,20d,visible)
        vctrl(-225d,-95d,hidden)		;Do button border too
		vstat(sparkle_off,xflip_off,thispage,$E,colred)
        vctr(0d,17d,visible)
        vctr(2d,2d,visible)
        vctr(6d,0d,visible)
        vctr(2d,-2d,visible)
        vctr(0d,-17d,visible)
        vctr(-2d,-2d,visible)
        vctr(-6d,0d,visible)
        vctr(-2d,2d,visible)
        vctrl(2d,7d,hidden)     ;position vector for button next
        rtsl

elevp   vstat(sparkle_off,xflip_off,thispage,$E,colyellow)
		vctrl(150,0,hidden)
		vctrl(-10,40,visible)
		vctrl(40,0,visible)
		vctrl(-10,-40,visible)
		vctrl(-20,0,visible)
		vctrl(-8,32,hidden)
		vctrl(36,0,visible)
		vstat(sparkle_off,xflip_off,thispage,$E,colgreen)
		vctrl(-18,8,hidden)
		vctrl(-16,12,visible)
		jsrl(elevpl)
		vctrl(16,-12,hidden)
		vctrl(-8,24,visible)
		jsrl(elevpl)
		vctrl(8,-24,hidden)
		vctrl(8,32,visible)
		jsrl(elevpl)
		vctrl(-8,-32,hidden)
		vctrl(16,12,visible)
		jsrl(elevpl)
		rtsl
		
elevpl  vctr(-2,5,visible)
		vctr(2,-2,hidden)
		vctr(-5,-2,visible)
		vctr(5,2,hidden)
		vctr(-3,3,visible)
		vctr(3,-3,hidden)
		vctr(2,5,visible)
		vctr(-2,-5,hidden)
		vctr(5,1,visible)
		vctr(-5,-1,hidden)
		rtsl
		

;The button 
elevatb vctr(0d,2d,visible)
        vctr(2d,2d,visible)
        vctr(2d,0d,visible)
        vctr(2d,-2d,visible)
        vctr(0d,-2d,visible)
        vctr(-2d,-2d,visible)
        vctr(-2d,0d,visible)
        vctr(-2d,2d,visible)
        rtsl

;Semi-circle for Elevator floor indicator        
elevatc vstat(sparkle_off,xflip_off,thispage,$D,colgreen)
        vctrl(80d,120d, hidden)			;-100 original
        vctrl(1d,4d,visible)
        vctrl(2d,5d,visible)
        vctrl(2d,4d,visible)
        vctrl(3d,3d,visible)
        vctrl(3d,3d,visible)
        vctrl(4d,3d,visible)
        vctrl(4d,2d,visible)
        vctrl(6d,1d,visible)
        vctrl(6d,-1d,visible)
        vctrl(4d,-2d,visible)
        vctrl(4d,-3d,visible)
        vctrl(3d,-3d,visible)
        vctrl(3d,-3d,visible)
        vctrl(2d,-4d,visible)
        vctrl(2d,-5d,visible)
        vctrl(1d,-4d,visible)
        vctrl(-50d,0d,visible)
        vctrl(25d,0d,hidden)        ;position for needle
        rtsl

elesigt vctrl(-80,215,hidden)
		vctrl(0,30,visible)
		vctrl(160,-30,hidden)
		vctrl(0,30,visible)
		vstat(sparkle_off,xflip_off,thispage,$E,colyellow)
		vctrl(-240,0,hidden)
		vctrl(0,40,visible)
		vctrl(320,0,visible)
		vctrl(0,-40,visible)
		vctrl(-320,0,visible)
		vctrl(18,12,hidden)			;Position for text now
		rtsl
		
;Border for ROBOTS ONLY sign        
elesign vstat(sparkle_off,xflip_off,thispage,$D,colgreen)
        vctrl(68d,110d,hidden)		;was 122
        vctrl(54d,0d,visible)
        vctrl(0d,-35d,visible)
        vctrl(-54d,0d,visible)
        vctrl(0d,35d,visible)
        rtsl     


    #if $ > $7FFF \ .error "VECTOR Page 3 has extended outside of design size." \ #endif
	
;********************************
;* Vector Page 3 Checksum
;******************************** 
    .org $7FFF
    .chk $6000,$7FFF,IDENTIFIER_V3
;********************************
    .end
    
;.export mapdot,maze1,maze2,maze3,maze4,maze5,maze6,maze7
;.export mape1,mape2,mape3,mape4,mape5,mape6,mape7
.export tactc0,tactc1,tactc2,tactc3,tactc4
.export rtarrow,uparrow,dnarrow,ltarrow,nearrow,searrow,nwarrow,swarrow
.export outrw0,outrw1,outrw2,outrw3,outwrd
.export ltng0,ltng1,ltng2,ltng3,ltng4,ltng5,ltng6,ltng7,ltng8
.export ltng0x,ltng1x,ltng2x,ltng3x,ltng4x,ltng5x,ltng6x,ltng7x,ltng8x
.export pupl00,pupl10,pupl20,pupl30,pupl40,pupl50
.export wing00,wing01,wing02,wing03
.export wing10,wing11,wing12,wing13
.export wing20,wing21,wing22,wing23
.export wing30,wing31,wing32,wing33
.export wing40,wing41,wing42,wing43
.export wing50,wing51,wing52,wing53,bang0
.export dod0,dod1,dod2
.export ngwi0,ngwi1,ngwi2,ngwi3,ngwi4,ngwi5
.export coil0,coil1,coil2,coil3,coil4,coil5
.export wdgt0,wdgt1,wdgt2,wdgt3
.export magic0,magic1,magic2,magic3
.export bootz1,shoes,lock,key,keyp,keypbox,escpod,sqr,dial
.export crash0,crash1,crash2,crash3,crash4,crash5,crash6,crash7
.export flame0,flame1,flame2,flame3,flame4,flame5,flame6,flame7
.export flame8,flame9,flamea,flameb,flamec,flamed,flamee,flamef
.export star0,star1,star2,star3
.export hand,box,swtch0,swtch1,flare
;.export bootz2		;Commented out - Not used
.export ovhng,tite_vpg,tran_vpg
.export booth3,booth4,booth5

.export ltlsh,hexsh,ftrsh,dmbsh,hwsh

.export horiz,horiz_vpg,mntnpo,volpart,fterrap,fterra
.export elevat,elevatb,elevatc,elesign,elesigt,elevp

.export raditok,startok,cubetok,fuzetok,tokhex,hometok
.export hmtok1,hmtok2,hmtok3

.export smtb0,smtb1,smtb2,smtb3 

.export live_vpg,ltg_vpg,clock_vpg,boot_vpg,lock_vpg,hand_vpg,smtb_vpg
.export pod_vpg,tacct_vpg,flare_vpg,elev_vpg,bas_vpg,arrow_vpg,tok_vpg

;.export mapdt_vpg,maze_vpg 