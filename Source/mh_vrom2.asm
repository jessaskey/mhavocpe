;********************************************
;* Major Havoc Vector ROM Overlay Page 2    *
;* Copyright 1983 Atari                     *
;* Transcribed by Jess M. Askey 2000        *
;********************************************
    .title "TWOV2"
;********************************************
;* Includes:                                *
;********************************************
#include "havoc.ah"		;For build vars
#include "vector.ah"    ;For the various vector macros
#include "./exp/mh_alpha.exp"
;********************************************
;* Program:                                 *
;********************************************
    .org $6000
;*****************************************************************
;* Vector ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_V2
	.byte MAJOR_VERSION, MINOR_VERSION
;*****************************************************************
	
thispage = vpage2
        
        
;**************************************
    .sbttl "Reactor Rods"
;**************************************
rods_vpg    = $60|thispage

rod0    vctr(5d,-20d,hidden)
        vctr(-10d,27d,visible)
        vctr(15d,-27d,hidden)
        vctr(0d,30d,visible)
        vctr(5d,-30d,hidden)
        vctr(10d,27d,visible)
        vctr(2d,-42d,hidden)
        jmpl(rodpeg)
        
rod1    vctr(4d,-15d,hidden)
        vctr(-10d,27d,visible)
        vctr(16d,-27d,hidden)
        vctr(0d,30d,visible)
        vctr(6d,-30d,hidden)
        vctr(10d,27d,visible)
        vctr(2d,-47d,hidden)
        jmpl(rodpeg)
        
rod2    vctr(3d,-10d,hidden)
        vctr(-10d,27d,visible)
        vctr(17d,-27d,hidden)
        vctr(0d,30d,visible)
        vctr(7d,-30d,hidden)
        vctr(10d,27d,visible)
        vctr(1d,-52d,hidden)
        jmpl(rodpeg)
        
rod3    vctr(2d,-5d,hidden)
        vctr(-10d,27d,visible)
        vctr(18d,-27d,hidden)
        vctr(0d,30d,visible)
        vctr(8d,-30d,hidden)
        vctr(10d,27d,visible)
        vctr(0d,-57d,hidden)
rodpeg  vctr(5d,0d,visible)
        vctr(0d,-5d,visible)
        vctr(-3d,0d,visible)
        vctr(-20d,10d,hidden)
        rtsl

broffs = -30d

bigrod0 vctr(15d,-60d+broffs,hidden)
        vctr(-30d,81d,visible)
        vctr(45d,-81d,hidden)
        vctr(0d,90d,visible)
        vctr(15d,-90d,hidden)
        vctr(30d,81d,visible)
        vctr(6d,-126-broffs,hidden)
        jmpl(bigrodpeg)
        
bigrod1 vctr(12d,-45d+broffs,hidden)
        vctr(-30d,81d,visible)
        vctr(48d,-81d,hidden)
        vctr(0d,90d,visible)
        vctr(18d,-90d,hidden)
        vctr(30d,81d,visible)
        vctr(6d,-141-broffs,hidden)
        jmpl(bigrodpeg)
        
bigrod2 vctr(9d,-30d+broffs,hidden)
        vctr(-30d,81d,visible)
        vctr(51d,-81d,hidden)
        vctr(0d,90d,visible)
        vctr(21d,-90d,hidden)
        vctr(30d,81d,visible)
        vctr(3d,-156-broffs,hidden)
        jmpl(bigrodpeg)
        
bigrod3 vctr(6d,-15d+broffs,hidden)
        vctr(-30d,81d,visible)
        vctr(54d,-81d,hidden)
        vctr(0d,90d,visible)
        vctr(24d,-90d,hidden)
        vctr(30d,81d,visible)
        vctr(0d,-171-broffs,hidden)	
bigrodpeg  
		vctr(15d,0d,visible)
        vctr(0d,-15d,visible)
        vctr(-9d,0d,visible)
        vctr(-60d,30d,hidden)
        rtsl
        
;**************************************
    .sbttl "Robot/Reactor Parts"
;**************************************
body_vpg    = $60|thispage
gun_vpg     = $60|thispage    ;Use Robot head for star castle gun

body    vctr(-10d,30d,hidden)
        vctr(20d,0d,visible)
        vctr(10d,-40d,visible)
        vctr(-10d,-10d,visible)
        vctr(-20d,0d,visible)
        vctr(-10d,10d,visible)
        vctr(10d,40d,visible)
        rtsl

;For larger reactor	- Note the Y position has additional spacing to make the larger
;                     reactor work with the original(non-mega) position
bigbody vctr(-30d,90d+15d,hidden)
        vctr(60d,0d,visible)
        vctr(30d,-120d,visible)
        vctr(-30d,-30d,visible)
        vctr(-60d,0d,visible)
        vctr(-30d,30d,visible)
        vctr(30d,120d,visible)
        rtsl
        
bodyt   vctr(-5d,30d,hidden)
        vctr(20d,0d,visible)
        vctr(5d,-40d,visible)
        vctr(-10d,-10d,visible)
        vctr(-25d,5d,visible)
        vctr(-5d,10d,visible)
        vctr(15d,35d,visible)
        rtsl
        
head0   vctr(10d,10d,visible)
        vctr(10d,-10d,visible)
        vctr(-20d,0d,visible)
        rtsl
        
head1   vctr(17d,8d,visible)
        vctr(-8d,4d,visible)
        vctr(-9d,-12d,visible)
        rtsl
        
head2   vctr(8d,17d,visible)
        vctr(-12d,-4d,visible)
        vctr(4d,-13d,visible)
        rtsl
        
head3   vctr(0d,20d,visible)
        vctr(-10d,-10d,visible)
        vctr(10d,-10d,visible)
        rtsl
        
head4   vctr(-8d,17d,visible)
        vctr(-4d,-12d,visible)
        vctr(12d,-5d,visible)
        rtsl
        
head5   vctr(-17d,8d,visible)
        vctr(4d,-12d,visible)
        vctr(13d,4d,visible)
        rtsl
        
head6   vctr(-20d,0d,visible)
        vctr(10d,-10d,visible)
        vctr(10d,10d,visible)
        rtsl
        
tail0   vctr(5d,-50d,hidden)
        vctr(-5d,-10d,visible)
        vctr(20d,0d,visible)
        vctr(-5d,10d,visible)
        vctr(-5d,50d,hidden)
        rtsl
        
tail1   vctr(-5d,-50d,hidden)
        vctr(-5d,-5d,visible)
        vctr(15d,-5d,visible)
        vctr(0d,10d,visible)
        vctr(5d,50d,hidden)
        rtsl
        
gun0    rtsl ;.db $00,$C0
gun1    vctr(-5d,0d,hidden)
        vctr(0d,5d,visible)
        vctr(10d,0d,visible)
        vctr(0d,-5d,visible)
        vctr(-5d,0d,hidden)
        rtsl

gun2    vctr(0d,5d,visible)
        vctr(-5d,0d,visible)
        vctr(0d,5d,visible)
        vctr(10d,0d,visible)
        vctr(0d,-5d,visible)
        vctr(-5d,0d,visible)
        vctr(0d,-5d,hidden)
        rtsl

gun3    vctr(0d,10d,visible)
        vctr(-5d,0d,visible)
        vctr(0d,5d,visible)
        vctr(10d,0d,visible)
        vctr(0d,-5d,visible)
        vctr(-5d,0d,visible)
        vctr(0d,-10d,hidden)
        rtsl

eye0    vctr(-5d,-15d,hidden)
        vctr(6d,-5d,visible)
        vctr(5d,5d,visible)
        vctr(2d,-2d,visible)
        vctr(-2d,-4d,hidden)
        vctr(0d,-3d,visible)
        vctr(-8d,3d,hidden)
        vctr(0d,-3d,visible)
        vctr(-2d,-6d,hidden)
        vctr(12d,0d,visible)
        vctr(-10d,-2d,visible)
        vctr(-2d,2d,visible)
        rtsl

eye1    vctr(5d,-15d,hidden)
        vctr(5d,-5d,visible)
        vctr(2d,3d,visible)
        vctr(1d,-4d,hidden)
        vctr(0d,-3d,visible)
        vctr(-9d,3d,hidden)
        vctr(0d,-3d,visible)
        vctr(9d,-7d,hidden)
        vctr(-10d,-3d,visible)
        vctr(1d,2d,visible)
        vctr(9d,3d,visible)
        rtsl

eye2    vctr(5d,-15d,hidden)
        vctr(5d,-5d,visible)
        vctr(2d,2d,visible)
        vctr(-5d,-5d,hidden)
        vctr(0d,-3d,visible)
        vctr(4d,-5d,hidden)
        vctr(-5d,-5d,visible)
        vctr(1d,-1d,visible)
        vctr(2d,4d,visible)
        rtsl

;**************************************
    .sbttl "Max Robots"
;**************************************
maxrob_vpg = $60|thispage

maxhead0
        vctr(10d,32d,hidden)
        vctr(-2d,24d,visible)
        vctr(-4d,4d,visible)
        vctr(-8d,0d,visible)
        vctr(-4d,-4d,visible)
        vctr(-2d,-24d,visible)
        vctr(8d,28d,hidden)
        vctr(0d,12d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(6d,-12d,hidden)
        vctr(-16d,0d,visible)
        vctr(8d,-8d,visible)
        vctr(8d,8d,visible)
        vctr(-8d,-48d,hidden)
        rtsl
        
maxbody0  
        vctr(24d,20d,hidden)
        vctr(0d,-20d,visible)
        vctr(12d,0d,visible)
        vctr(0d,20d,visible)
        vctr(-72d,0d,visible)
        vctr(0d,-20d,visible)
        vctr(12d,0d,visible)
        vctr(0d,20d,visible)
        vctr(12d,12d,visible)
        vctr(24d,0d,visible)
        vctr(12d,-12d,visible)
        vctr(-6d,-28d,visible)
        vctr(-36d,0d,visible)
        vctr(-6d,28d,visible)
        vctr(-4d,-20d,hidden)
        vctr(0d,-32d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,16d,visible)
        vctr(4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(26d,-8d,hidden)
        vctr(0d,-12d,visible)
        vctr(12d,12d,hidden)
        vctr(0d,-12d,visible)
        vctr(10d,0d,hidden)
        vctr(-32d,0d,visible)
        vctr(8d,-36d,visible)
        vctr(16d,0d,visible)
        vctr(8d,36d,visible)
        vctr(12d,20d,hidden)
        vctr(0d,-32d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(-32d,0d,hidden)
        rtsl
        
maxhead1  
        vctr(10d,32d,hidden)
        vctr(-2d,24d,visible)
        vctr(-4d,4d,visible)
        vctr(-8d,0d,visible)
        vctr(-4d,-4d,visible)
        vctr(-2d,-24d,visible)
        vctr(8d,28d,hidden)
        vctr(0d,12d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(0d,12d,hidden)
        vctr(2d,-4d,visible)
        vctr(0d,-8d,visible)
        vctr(-12d,-12d,hidden)
        vctr(14d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-7d,6d,visible)
        vctr(9d,-46d,hidden)
        rtsl
        
maxbody1  
        vctr(20d,20d,hidden)
        vctr(0d,-20d,visible)
        vctr(12d,0d,visible)
        vctr(0d,20d,visible)
        vctr(-64d,0d,visible)
        vctr(0d,-20d,visible)
        vctr(12d,0d,visible)
        vctr(44d,20d,hidden)
        vctr(-12d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(-12d,-12d,visible)
        vctr(6d,-28d,visible)
        vctr(36d,0d,visible)
        vctr(2d,8d,visible)
        vctr(4d,0d,hidden)
        vctr(0d,-32d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(-22d,-8d,hidden)
        vctr(0d,-12d,visible)
        vctr(-12d,12d,hidden)
        vctr(0d,-12d,visible)
        vctr(-10d,0d,hidden)
        vctr(8d,-36d,visible)
        vctr(16d,0d,visible)
        vctr(8d,36d,visible)
        vctr(-32d,0d,visible)
        vctr(-8d,20d,hidden)
        vctr(0d,-32d,visible)
        vctr(2d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-2d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-2d,0d,visible)
        vctr(0d,8d,visible)
        vctr(4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(28d,0d,hidden)
        rtsl
        
maxhead2  
        vctr(10d,32d,hidden)
        vctr(-2d,24d,visible)
        vctr(-4d,4d,visible)
        vctr(-8d,0d,visible)
        vctr(-4d,-4d,visible)
        vctr(-2d,-24d,visible)
        vctr(8d,28d,hidden)
        vctr(0d,12d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(0d,12d,hidden)
        vctr(8d,-6d,visible)
        vctr(2d,-34d,visible)
        vctr(-20d,16d,hidden)
        vctr(12d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-5d,4d,visible)
        vctr(9d,-44d,hidden)
        rtsl
        
maxbody2  
        vctr(16d,20d,hidden)
        vctr(0d,-20d,visible)
        vctr(12d,0d,visible)
        vctr(0d,20d,visible)
        vctr(-56d,0d,visible)
        vctr(0d,-20d,visible)
        vctr(8d,0d,visible)
        vctr(0d,-32d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-16d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,8d,visible)
        vctr(4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(48d,20d,hidden)
        vctr(-12d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(-12d,-12d,visible)
        vctr(6d,-28d,visible)
        vctr(36d,0d,visible)
        vctr(2d,8d,visible)
        vctr(0d,-32d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(-18d,-8d,hidden)
        vctr(0d,-12d,visible)
        vctr(-12d,12d,hidden)
        vctr(0d,-12d,visible)
        vctr(-10d,0d,hidden)
        vctr(8d,-36d,visible)
        vctr(16d,0d,visible)
        vctr(8d,36d,visible)
        vctr(-32d,0d,visible)
        vctr(16d,20d,hidden)
        rtsl
        
maxhead3  
        vctr(10d,32d,hidden)
        vctr(-2d,24d,visible)
        vctr(-4d,4d,visible)
        vctr(-8d,0d,visible)
        vctr(-4d,-4d,visible)
        vctr(-2d,-24d,visible)
        vctr(8d,28d,hidden)
        vctr(0d,12d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(0d,12d,hidden)
        vctr(10d,-10d,visible)
        vctr(0d,-30d,visible)
        vctr(-20d,16d,hidden)
        vctr(10d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-3d,3d,visible)
        vctr(9d,-43d,hidden)
        rtsl
        
maxbody3  
        vctr(-24d,20d,hidden)
        vctr(48d,0d,visible)
        vctr(-12d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(-12d,-12d,visible)
        vctr(6d,-28d,visible)
        vctr(32d,0d,visible)
        vctr(8d,28d,hidden)
        vctr(0d,-20d,visible)
        vctr(-14d,0d,visible)
        vctr(0d,20d,visible)
        vctr(10d,-20d,hidden)
        vctr(0d,-32d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-16d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,8d,visible)
        vctr(4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(-8d,-8d,hidden)
        vctr(0d,-12d,visible)
        vctr(-12d,12d,hidden)
        vctr(0d,-12d,visible)
        vctr(-8d,12d,hidden)
        vctr(0d,-12d,visible)
        vctr(-8d,32d,hidden)
        vctr(0d,-12d,visible)
        vctr(2d,0d,visible)
        vctr(2d,-8d,hidden)
        vctr(0d,-24d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(6d,0d,visible)
        vctr(22d,8d,hidden)
        vctr(-4d,-16d,visible)
        vctr(-16d,0d,visible)
        vctr(-8d,36d,visible)
        vctr(30d,0d,visible)
        vctr(-14d,20d,hidden)
        rtsl
 
maxhead4  
        vctr(10d,32d,hidden)
        vctr(-2d,24d,visible)
        vctr(-4d,4d,visible)
        vctr(-8d,0d,visible)
        vctr(-4d,-4d,visible)
        vctr(-2d,-24d,visible)
        vctr(10d,28d,hidden)
        vctr(0d,12d,visible)
        vctr(2d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(0d,12d,hidden)
        vctr(14d,-10d,visible)
        vctr(0d,-34d,visible)
        vctr(-24d,20d,hidden)
        vctr(8d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-1d,1d,visible)
        vctr(9d,-42d,hidden)
        rtsl
        
maxbody4  
        vctr(12d,-8d,hidden)
        vctr(6d,0d,visible)
        vctr(6d,28d,visible)
        vctr(-12d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(-12d,-12d,visible)
        vctr(48d,0d,visible)
        vctr(-16d,-28d,hidden)
        vctr(-26d,0d,visible)
        vctr(-6d,28d,visible)
        vctr(26d,0d,hidden)
        vctr(0d,-20d,visible)
        vctr(14d,0d,visible)
        vctr(0d,20d,visible)
        vctr(-4d,-20d,hidden)
        vctr(0d,-32d,visible)
        vctr(2d,0d,visible)
        vctr(2d,12d,visible)
        vctr(-4d,0d,visible)
        vctr(2d,-12d,hidden)
        vctr(2d,0d,visible)
        vctr(0d,-16d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,8d,visible)
        vctr(4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(-14d,-8d,hidden)
        vctr(0d,-12d,visible)
        vctr(-4d,12d,hidden)
        vctr(0d,-12d,visible)
        vctr(18d,0d,hidden)
        vctr(-24d,0d,visible)
        vctr(8d,-36d,visible)
        vctr(16d,0d,visible)
        vctr(2d,8d,visible)
        vctr(-20d,0d,hidden)
        vctr(-2d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,8d,visible)
        vctr(2d,0d,visible)
        vctr(14d,32d,hidden)
        rtsl
        
maxhead5  
        vctr(10d,32d,hidden)
        vctr(-2d,24d,visible)
        vctr(-4d,4d,visible)
        vctr(-8d,0d,visible)
        vctr(-4d,-4d,visible)
        vctr(-2d,-24d,visible)
        vctr(12d,28d,hidden)
        vctr(0d,12d,visible)
        vctr(14d,-10d,visible)
        vctr(0d,-34d,visible)
        vctr(-24d,20d,hidden)
        vctr(6d,0d,visible)
        vctr(-7d,-8d,visible)
        vctr(9d,-40d,hidden)
        rtsl
        
maxbody5  
        vctr(4d,-8d,hidden)
        vctr(14d,0d,visible)
        vctr(6d,28d,visible)
        vctr(-12d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(-12d,-12d,visible)
        vctr(6d,-28d,visible)
        vctr(18d,0d,visible)
        vctr(24d,28d,hidden)
        vctr(-48d,0d,visible)
        vctr(18d,0d,hidden)
        vctr(0d,-20d,visible)
        vctr(14d,0d,visible)
        vctr(0d,20d,visible)
        vctr(-4d,-20d,hidden)
        vctr(0d,-32d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-16d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,8d,visible)
        vctr(4d,0d,visible)
        vctr(0d,32d,visible)
        vctr(6d,-8d,hidden)
        vctr(0d,-12d,visible)
        vctr(-12d,12d,hidden)
        vctr(0d,-12d,visible)
        vctr(6d,0d,hidden)
        vctr(-16d,0d,visible)
        vctr(8d,-36d,visible)
        vctr(16d,0d,visible)
        vctr(8d,36d,visible)
        vctr(-12d,0d,visible)
        vctr(-4d,20d,hidden)
        rtsl
        
maxhead6  
        vctr(0d,32d,hidden)
        vctr(-2d,24d,visible)
        vctr(-4d,4d,visible)
        vctr(-8d,0d,visible)
        vctr(-4d,-4d,visible)
        vctr(0d,-24d,visible)
        vctr(10d,28d,hidden)
        vctr(-2d,12d,visible)
        vctr(14d,-8d,visible)
        vctr(4d,-36d,visible)
        vctr(-26d,20d,hidden)
        vctr(8d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(18d,-40d,hidden)
        rtsl
        
maxbody6  
        vctr(-2d,-10d,hidden)
        vctr(14d,0d,visible)
        vctr(4d,30d,visible)
        vctr(-12d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(-12d,-12d,visible)
        vctr(10d,-30d,visible)
        vctr(15d,0d,visible)
        vctr(7d,30d,hidden)
        vctr(2d,-20d,visible)
        vctr(-14d,0d,visible)
        vctr(-2d,20d,visible)
        vctr(10d,-20d,hidden)
        vctr(4d,-30d,visible)
        vctr(4d,0d,visible)
        vctr(4d,-16d,visible)
        vctr(-8d,0d,visible)
        vctr(-2d,8d,visible)
        vctr(-4d,0d,visible)
        vctr(-2d,8d,visible)
        vctr(4d,0d,visible)
        vctr(-4d,30d,visible)
        vctr(8d,-10d,hidden)
        vctr(2d,-10d,visible)
        vctr(-14d,10d,hidden)
        vctr(2d,-10d,visible)
        vctr(4d,0d,hidden)
        vctr(-14d,0d,visible)
        vctr(12d,-36d,visible)
        vctr(16d,0d,visible)
        vctr(4d,36d,visible)
        vctr(-12d,0d,visible)
        vctr(0d,20d,hidden)
        rtsl
        
maxhead7
        vctr(-8d,32d,hidden)
        vctr(-4d,24d,visible)
        vctr(-6d,4d,visible)
        vctr(-8d,0d,visible)
        vctrl(-4d,-4d,visible)
        vctr(2d,-24d,visible)
        vctr(8d,28d,hidden)
        vctr(-2d,12d,visible)
        vctr(14d,-4d,visible)
        vctr(8d,-40d,visible)
        vctr(-29d,20d,hidden)
        vctr(7d,0d,visible)
        vctr(-6d,-8d,visible)
        vctr(28d,-40d,hidden)
        rtsl
          
maxbody7  
        vctr(-6d,-12d,hidden)
        vctr(12d,0d,visible)
        vctr(2d,32d,visible)
        vctr(-12d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(-12d,-12d,visible)
        vctr(12d,-36d,visible)
        vctr(18d,0d,visible)
        vctr(2d,36d,hidden)
        vctr(4d,-20d,visible)
        vctr(-16d,0d,visible)
        vctr(-4d,20d,visible)
        vctr(14d,-20d,hidden)
        vctr(8d,-28d,visible)
        vctr(4d,0d,visible)
        vctr(4d,-16d,visible)
        vctr(-8d,0d,visible)
        vctr(-2d,8d,visible)
        vctr(-4d,0d,visible)
        vctr(-2d,8d,visible)
        vctr(4d,0d,visible)
        vctr(-8d,28d,visible)
        vctr(10d,-12d,hidden)
        vctr(2d,-8d,visible)
        vctr(-16d,8d,hidden)
        vctr(2d,-8d,visible)
        vctr(8d,0d,hidden)
        vctr(-16d,0d,visible)
        vctr(16d,-36d,visible)
        vctr(16d,0d,visible)
        vctr(2d,36d,visible)
        vctr(-14d,0d,visible)
        vctr(4d,20d,hidden)
        rtsl
        
maxeye0 vctr(-5d,46d,hidden)
        vctr(10d,0,visible)
        rtsl
        
maxeye1 vctr(-3d,46d,hidden)
        vctr(5d,0,visible)
        rtsl
        
maxeye2 vctr(-3d,46d,hidden)
        vctr(4d,0,visible)
        rtsl    
        
maxeye3 vctr(-3d,46d,hidden)
        vctr(3d,0,visible)
        rtsl
        
maxeye4 vctr(-3d,46d,hidden)
        vctr(2d,0,visible)
        rtsl
        
maxeye5 vctr(-4d,46d,hidden)
        vctr(1d,0,visible)
        rtsl
        
maxeye6 vctr(-13,46d,hidden)
        vctr(1d,0,visible)
        rtsl
        
maxeye7 vctr(-26,46d,hidden)
        vctr(1d,0,visible)
        rtsl

 
;**************************************
; Levitations
;**************************************   
#IF MAX_LEVITATIONS != 0     
levt0   vctr(-8d,-64d,hidden)
        vctr(16d,0d,visible)
        vctr(2d,-10d,hidden)
        vctr(-20d,0d,visible)
        vctr(-6d,-14d,hidden)
        vctr(32d,0d,visible)
		vctr(-16d,88d,hidden)
        rtsl
        
levt1   vctr(-8d,-58d,hidden)
        vctr(16d,0d,visible)
        vctr(0d,-8d,hidden)
        vctr(-16d,0d,visible)
        vctr(-4d,-10d,hidden)
        vctr(24d,0d,visible)
        vctr(6d,-16d,hidden)
        vctr(-36d,0d,visible)
		vctr(18d,92d,hidden)
        rtsl

levt2   vctr(-8d,-60d,hidden)
        vctr(16d,0d,visible)
        vctr(0d,-8d,hidden)
        vctr(-16d,0d,visible)
        vctr(-4d,-12d,hidden)
        vctr(24d,0d,visible)
		vctr(-12d,80d,hidden)
        rtsl

levt3   vctr(-8d,-62d,hidden)
        vctr(16d,0d,visible)
        vctr(0d,-10d,hidden)
        vctr(-16d,0d,visible)
        vctr(-6d,-12d,hidden)
        vctr(28d,0d,visible)
		vctr(-14d,84d,hidden)
        rtsl
#ENDIF

;**************************************
    .sbttl "Ion Cannon Parts"
;**************************************
mzls_vpg = $60|thispage        

mount   vctr(-8d,0d,visible)
        vctr(-8d,8d,visible)
        vctr(32d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,0d,visible)
        rtsl

lgun0   vctr(32d,8d,hidden)
        vctr(0d,-16d,visible)
        vctr(-64d,0d,visible)
        vctr(0d,16d,visible)
        vctr(64d,0d,visible)
        vctr(-32d,-8d,hidden)
        rtsl
        
lgun1   vctr(32d,-4d,hidden)
        vctr(-6d,-16d,visible)
        vctr(-60d,24d,visible)
        vctr(6d,16d,visible)
        vctr(60d,-24d,visible)
        vctr(-32d,4d,hidden)
        rtsl

lgun2   vctr(20d,-28d,hidden)
        vctr(-16d,-6d,visible)
        vctr(-24d,60d,visible)
        vctr(16d,6d,visible)
        vctr(24d,-60d,visible)
        vctr(-20d,28d,hidden)
        rtsl

lgun3   vctr(8d,32d,hidden)
        vctr(0d,-64d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,64d,visible)
        vctr(16d,0d,visible)
        vctr(-8d,-32d,hidden)
        rtsl

brl00   vctr(32d,4d,hidden)
        vctr(16d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-16d,0d,visible)
        vctr(16d,6d,hidden)
        vctr(16d,0d,visible)
        vctr(0d,-4d,visible)
        vctr(-16d,0d,visible)
        vctr(-48d,2d,hidden)
        rtsl
        
brl01   vctr(32d,4d,hidden)
        vctr(12d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-12d,0d,visible)
        vctr(12d,6d,hidden)
        vctr(8d,0d,visible)
        vctr(0d,-4d,visible)
        vctr(-8d,0d,visible)
        vctr(-44d,2d,hidden)
        rtsl
        
brl02   vctr(32d,4d,hidden)
        vctr(8d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-8d,0d,visible)
        vctr(8d,6d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,-4d,visible)
        vctr(-4d,0d,visible)
        vctr(-40d,2d,hidden)
        rtsl
        
brl10   vctr(31d,-8d,hidden)
        vctr(14d,-6d,visible)
        vctr(-3d,-8d,visible)
        vctr(-14d,6d,visible)
        vctr(16d,0d,hidden)
        vctr(15d,-6d,visible)
        vctr(-1d,-4d,visible)
        vctr(-15d,6d,visible)
        vctr(-43d,20d,hidden)
        rtsl
        
brl11   vctr(31d,-8d,hidden)
        vctr(9d,-4d,visible)
        vctr(-3d,-8d,visible)
        vctr(-9d,4d,visible)
        vctr(12d,2d,hidden)
        vctr(8d,-3d,visible)
        vctr(-2d,-4d,visible)
        vctr(-8d,3d,visible)
        vctr(-38d,18d,hidden)
        rtsl

brl12   vctr(31d,-8d,hidden)
        vctr(5d,-2d,visible)
        vctr(-3d,-8d,visible)
        vctr(-5d,2d,visible)
        vctr(7d,4d,hidden)
        vctr(3d,-1d,visible)
        vctr(-1d,-4d,visible)
        vctr(-3d,1d,visible)
        vctr(-34d,16d,hidden)
        rtsl
        
brl20   vctr(16d,-29d,hidden)
        vctr(6d,-16d,visible)
        vctr(-8d,-3d,visible)
        vctr(-6d,16d,visible)
        vctr(12d,-14d,hidden)
        vctr(6d,-14d,visible)
        vctr(-4d,-1d,visible)
        vctr(-6d,14d,visible)
        vctr(-16d,47d,hidden)
        rtsl

brl21   vctr(16d,-29d,hidden)
        vctr(5d,-12d,visible)
        vctr(-8d,-3d,visible)
        vctr(-5d,12d,visible)
        vctr(11d,-10d,hidden)
        vctr(3d,-8d,visible)
        vctr(-4d,-1d,visible)
        vctr(-3d,8d,visible)
        vctr(-15d,43d,hidden)
        rtsl
        
brl22   vctr(16d,-29d,hidden)
        vctr(3d,-8d,visible)
        vctr(-8d,-3d,visible)
        vctr(-3d,8d,visible)
        vctr(9d,-6d,hidden)
        vctr(2d,-4d,visible)
        vctr(-4d,-1d,visible)
        vctr(-2d,4d,visible)
        vctr(-13d,39d,hidden)
        rtsl

brl30   vctr(4d,-32d,hidden)
        vctr(0d,-16d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,16d,visible)
        vctr(6d,-16d,hidden)
        vctr(0d,-16d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,16d,visible)
        vctr(2d,48d,hidden)
        rtsl

brl31   vctr(4d,-32d,hidden)
        vctr(0d,-12d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,12d,visible)
        vctr(6d,-12d,hidden)
        vctr(0d,-8d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,8d,visible)
        vctr(2d,44d,hidden)
        rtsl
        
brl32   vctr(4d,-32d,hidden)
        vctr(0d,-8d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,8d,visible)
        vctr(6d,-8d,hidden)
        vctr(0d,-4d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,4d,visible)
        vctr(2d,40d,hidden)
        rtsl

        
lazcom0 vctr(4d,0d,visible)
        vctr(2d,-8d,visible)
        vctr(4d,-16d,hidden)
        vctr(2d,-8d,visible)
        vctr(4d,0d,visible)
        rtsl        
        
laz00   vctr(-32d,-8d,hidden)
        vctr(6d,24d,visible)
        jsrl(lazcom0)
        vctr(8d,32d,visible)
        jsrl(lazcom0)
        vctr(8d,32d,visible)
        vctr(2d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-24d,-8d,hidden)
        rtsl
        
laz01   vctr(-32d,-8d,hidden)
        vctr(0d,-8d,visible)
        vctr(4d,0d,visible)
        vctr(8d,32d,visible)
        jsrl(lazcom0)
        vctr(8d,32d,visible)
        jsrl(lazcom0)
        vctr(2d,8d,visible)
        vctr(-22d,8d,hidden)
        rtsl
        
laz02   vctr(-28d,-8d,hidden)
        vctr(2d,-8d,visible)
        vctr(4d,0d,visible)
        vctr(8d,32d,visible)
        jsrl(lazcom0)
        vctr(8d,32d,visible)
        vctr(4d,0d,visible)
        vctr(2d,-8d,visible)
        vctr(4d,-16d,hidden)
        vctr(2d,-8d,visible)
        vctr(2d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-24d,8d,hidden)
        rtsl

laz03   vctr(-32d,8d,hidden)
        vctr(0d,8d,visible)
        jsrl(lazcom0)
        vctr(8d,32d,visible)
        jsrl(lazcom0)
        vctr(8d,32d,visible)
        vctr(4d,0d,visible)
        vctr(2d,-8d,visible)
        vctr(-22d,-8d,hidden)
        rtsl
 
lazcom1 vctr(4d,-2d,visible)
        vctr(0d,-8d,visible)
        vctr(-4d,-16d,hidden)
        vctr(0d,-8d,visible)
        vctr(4d,-2d,visible)
        vctr(18d,28d,visible)
        rtsl 
        
laz10   vctr(-34d,4d,hidden)
        vctr(14d,20d,visible)
        jsrl(lazcom1)
        jsrl(lazcom1)
        vctr(4d,-2d,visible)
        vctr(0d,-8d,visible)
        vctr(-24d,2d,hidden)
        rtsl

lazcom2        
        vctr(-6d,-4d,visible)
        vctr(-16d,-10d,hidden)
        vctr(-6d,-4d,visible)
        vctr(2d,-4d,visible)
        vctr(32d,4d,visible)
        rtsl
        
laz11 = laz01
laz12 = laz02
laz13 = laz03

laz20   vctr(-20d,26d,hidden)
        vctr(26d,2d,visible)
        vctr(2d,-4d,visible)
        jsrl(lazcom2)
        vctr(2d,-4d,visible)
        jsrl(lazcom2)
        vctr(2d,-4d,visible)
        vctr(-6d,-4d,visible)
        vctr(-17d,20d,hidden)
        rtsl

lazcom3 vctr(0d,-4d,visible)
        vctr(-8d,-2d,visible)
        vctr(-16d,-4d,hidden)
        vctr(-8d,-2d,visible)
        vctr(0d,-4d,visible)
        rtsl

laz21 = laz01
laz22 = laz02
laz23 = laz03
        
laz30   vctr(-8d,32d,hidden)
        vctr(24d,-6d,visible)
        jsrl(lazcom3)
        vctr(32d,-8d,visible)
        jsrl(lazcom3)
        vctr(32d,-8d,visible)
        vctr(0d,-2d,visible)
        vctr(-8d,0d,visible)
        vctr(-8d,24d,hidden)
        rtsl
        
laz31   vctr(-8d,32d,hidden)
        vctr(-8d,0d,visible)
        vctr(0d,-4d,visible)
        vctr(32d,-8d,visible)
        jsrl(lazcom3)
        vctr(32d,-8d,visible)
        jsrl(lazcom3)
        vctr(8d,-2d,visible)
        vctr(8d,22d,hidden)
        rtsl
        
laz32   vctr(-8d,28d,hidden)
        vctr(-8d,-2d,visible)
        vctr(0d,-4d,visible)
        vctr(32d,-8d,visible)
        jsrl(lazcom3)
        vctr(32d,-8d,visible)
        vctr(0d,-4d,visible)
        vctr(-8d,-2d,visible)
        vctr(-16d,-4d,hidden)
        vctr(-8d,-2d,visible)
        vctr(0d,-2d,visible)
        vctr(8d,0d,visible)
        vctr(8d,24d,hidden)
        rtsl
        
laz33   vctr(8d,32d,hidden)
        vctr(8d,0d,visible)
        jsrl(lazcom3)
        vctr(32d,-8d,visible)
        jsrl(lazcom3)
        vctr(32d,-8d,visible)
        vctr(0d,-4d,visible)
        vctr(-8d,-2d,visible)
        vctr(-8d,22d,hidden)
        rtsl
        
dot0    vctr(-4d,0d,visible)
        vctr(0d,1d,visible)
        vctr(4d,0d,visible)
        vctr(0d,1d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,1d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-3d,hidden)
        rtsl  
        
dot1    vctr(-6d,0d,visible)
        vctr(0d,1d,visible)
        vctr(6d,0d,visible)
        vctr(0d,1d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,1d,visible)
        vctr(6d,0d,visible)
        vctr(0d,-3d,hidden)
        rtsl
        
beacn3
dot2    vctr(-10d,0d,visible)
        vctr(0d,1d,visible)
        vctr(10d,0d,visible)
        vctr(0d,1d,visible)
        vctr(-10d,0d,visible)
        vctr(0d,1d,visible)
        vctr(10d,0d,visible)
        vctr(0d,-3d,hidden)
        rtsl

;**************************************
    .sbttl "Beacon"
;**************************************
becn_vpg = $60|thispage
        
beacn0
beacn1  vctr(-10d,0d,hidden)
        jsrl(dot0)
        vctr(10d,0d,hidden)
        rtsl

beacn2  vctr(-8d,0d,hidden)
        jsrl(dot1)
        vctr(8d,0d,hidden)
        rtsl

beacn4  vctr(8d,0d,hidden)
        jsrl(dot1)
        vctr(-8d,0d,hidden)
        rtsl

beacn5
beacn6  vctr(10d,0d,hidden)
        jsrl(dot0)
        vctr(-10d,0d,hidden)
beacn7  rtsl



;********************************
    .sbttl "Ship Shots"
;********************************
shpsh_vpg = $60|thispage 

shipsh  jsrl(beacn3)
        .db $D6,$62
        vctr(-6d,-2d,hidden)
        vctr(2d,0d,visible)
        vctr(0d,1d,visible)
        vctr(-2d,0d,visible)
        rtsl

hive0   vctr(38d,134d,hidden)
        vctr(-78d,0d,visible)
        vctr(-58d,-34d,visible)
        vctr(-26d,-64d,visible)
        vctr(-2d,-68d,visible)
        vctr(42d,-76d,visible)
        vctr(50d,-26d,visible)
        vctr(58d,0d,visible)
        vctr(52d,24d,visible)
        vctr(48d,74d,visible)
        vctr(2d,76d,visible)
        vctr(-34d,58d,visible)
        vctr(-54d,34d,visible)
        vctr(-36d,-30d,visible)
        vctr(-42d,32d,visible)
        vctr(42d,-32d,hidden)
        vctr(-2d,-66d,visible)
        vctr(28d,-54d,visible)
        vctr(-62d,2d,visible)
        vctr(36d,54d,visible)
        vctr(26d,-56d,hidden)
        vctr(50d,-38d,visible)
        vctr(46d,20d,visible)
        vctr(-48d,-18d,hidden)
        vctr(0d,-56d,visible)
        vctr(-110d,94d,hidden)
        vctr(-48d,-38d,visible)
        vctr(-2d,-52d,visible)
        vctr(2d,52d,hidden)
        vctr(-44d,22d,visible)
        rtsl
      

;HACK: Removed this to see if it breaks anything	  
 ;.org $7000

;***************************************************
;* Linear Line Growth - 3rd Quad
;***************************************************    
; hardlin is used in tw_spcmz as the base 
; pointer to this data
;hardlin
spstat_vpg    = $60|thispage

webln00 vctrl(-4d,-6d,visible)      \ rtsl  
        vctrl(-6d,-9d,visible)      \ rtsl  
        vctrl(-8d,-12d,visible)     \ rtsl  
        vctrl(-12d,-18d,visible)    \ rtsl  
        vctrl(-17d,-25d,visible)    \ rtsl  
        vctrl(-23d,-35d,visible)    \ rtsl  
        vctrl(-33d,-50d,visible)    \ rtsl  
        vctrl(-47d,-70d,visible)    \ rtsl  
        vctrl(-66d,-99d,visible)    \ rtsl  
        vctrl(-94d,-141d,visible)   \ rtsl  
        vctrl(-133d,-199d,visible)  \ rtsl     
        
webln01 vctrl(-3d,-6d,visible)      \ rtsl 
        vctrl(-5d,-9d,visible)      \ rtsl 
        vctrl(-7d,-12d,visible)     \ rtsl 
        vctrl(-9d,-18d,visible)     \ rtsl 
        vctrl(-13d,-25d,visible)    \ rtsl 
        vctrl(-19d,-35d,visible)    \ rtsl 
        vctrl(-27d,-50d,visible)    \ rtsl 
        vctrl(-37d,-70d,visible)    \ rtsl 
        vctrl(-53d,-99d,visible)    \ rtsl 
        vctrl(-75d,-141d,visible)   \ rtsl 
        vctrl(-106d,-199d,visible)  \ rtsl 
        
webln02 vctrl(-2d,-6d,visible)      \ rtsl 
        vctrl(-4d,-9d,visible)      \ rtsl 
        vctrl(-5d,-12d,visible)     \ rtsl 
        vctrl(-7d,-18d,visible)     \ rtsl 
        vctrl(-10d,-25d,visible)    \ rtsl 
        vctrl(-14d,-35d,visible)    \ rtsl 
        vctrl(-20d,-50d,visible)    \ rtsl 
        vctrl(-28d,-70d,visible)    \ rtsl 
        vctrl(-40d,-99d,visible)    \ rtsl 
        vctrl(-56d,-141d,visible)   \ rtsl 
        vctrl(-80d,-199d,visible)   \ rtsl 
        
webln03 vctrl(-2d,-6d,visible)      \ rtsl 
        vctrl(-2d,-9d,visible)      \ rtsl 
        vctrl(-3d,-12d,visible)     \ rtsl 
        vctrl(-5d,-18d,visible)     \ rtsl 
        vctrl(-7d,-25d,visible)     \ rtsl 
        vctrl(-9d,-35d,visible)     \ rtsl 
        vctrl(-13d,-50d,visible)    \ rtsl 
        vctrl(-19d,-70d,visible)    \ rtsl 
        vctrl(-27d,-99d,visible)    \ rtsl 
        vctrl(-37d,-141d,visible)   \ rtsl 
        vctrl(-53d,-199d,visible)   \ rtsl 
        
webln04 vctrl(-1d,-6d,visible)      \ rtsl 
        vctrl(-1d,-9d,visible)      \ rtsl 
        vctrl(-2d,-12d,visible)     \ rtsl 
        vctrl(-2d,-18d,visible)     \ rtsl 
        vctrl(-3d,-25d,visible)     \ rtsl 
        vctrl(-5d,-35d,visible)     \ rtsl 
        vctrl(-7d,-50d,visible)     \ rtsl 
        vctrl(-9d,-70d,visible)     \ rtsl 
        vctrl(-13d,-99d,visible)    \ rtsl 
        vctrl(-19d,-141d,visible)   \ rtsl 
        vctrl(-27d,-199d,visible)   \ rtsl 
        
        
webln05 vctrl(0d,-6d,visible)       \ rtsl 
        vctrl(0d,-9d,visible)       \ rtsl 
        vctrl(0d,-12d,visible)      \ rtsl 
        vctrl(0d,-18d,visible)      \ rtsl 
        vctrl(0d,-25d,visible)      \ rtsl 
        vctrl(0d,-35d,visible)      \ rtsl 
        vctrl(0d,-50d,visible)      \ rtsl 
        vctrl(0d,-70d,visible)      \ rtsl 
        vctrl(0d,-99d,visible)      \ rtsl 
        vctrl(0d,-141d,visible)     \ rtsl 
        vctrl(0d,-199d,visible)     \ rtsl 
        

;******************************************
;* Linear Line Growth - 1st Quad
;******************************************
webln06 vctrl(4d,6d,visible)        \ rtsl 
        vctrl(6d,9d,visible)        \ rtsl 
        vctrl(8d,12d,visible)       \ rtsl 
        vctrl(12d,18d,visible)      \ rtsl 
        vctrl(17d,25d,visible)      \ rtsl 
        vctrl(23d,35d,visible)      \ rtsl 
        vctrl(33d,50d,visible)      \ rtsl 
        vctrl(47d,70d,visible)      \ rtsl 
        vctrl(66d,99d,visible)      \ rtsl 
        vctrl(94d,141d,visible)     \ rtsl 
        vctrl(133d,199d,visible)    \ rtsl 
        
webln07 vctrl(3d,6d,visible)        \ rtsl 
        vctrl(5d,9d,visible)        \ rtsl 
        vctrl(7d,12d,visible)       \ rtsl 
        vctrl(9d,18d,visible)       \ rtsl 
        vctrl(13d,25d,visible)      \ rtsl 
        vctrl(19d,35d,visible)      \ rtsl 
        vctrl(27d,50d,visible)      \ rtsl 
        vctrl(37d,70d,visible)      \ rtsl 
        vctrl(53d,99d,visible)      \ rtsl 
        vctrl(75d,141d,visible)     \ rtsl 
        vctrl(106d,199d,visible)    \ rtsl 
        
webln08 vctrl(2d,6d,visible)        \ rtsl 
        vctrl(4d,9d,visible)        \ rtsl 
        vctrl(5d,12d,visible)       \ rtsl 
        vctrl(7d,18d,visible)       \ rtsl 
        vctrl(10d,25d,visible)      \ rtsl 
        vctrl(14d,35d,visible)      \ rtsl 
        vctrl(20d,50d,visible)      \ rtsl 
        vctrl(28d,70d,visible)      \ rtsl 
        vctrl(40d,99d,visible)      \ rtsl 
        vctrl(56d,141d,visible)     \ rtsl 
        vctrl(80d,199d,visible)     \ rtsl 
        
webln09 vctrl(2d,6d,visible)        \ rtsl 
        vctrl(2d,9d,visible)        \ rtsl 
        vctrl(3d,12d,visible)       \ rtsl 
        vctrl(5d,18d,visible)       \ rtsl 
        vctrl(7d,25d,visible)       \ rtsl 
        vctrl(9d,35d,visible)       \ rtsl 
        vctrl(13d,50d,visible)      \ rtsl 
        vctrl(19d,70d,visible)      \ rtsl 
        vctrl(27d,99d,visible)      \ rtsl 
        vctrl(37d,141d,visible)     \ rtsl 
        vctrl(53d,199d,visible)     \ rtsl 
        
webln0a vctrl(1d,6d,visible)        \ rtsl 
        vctrl(1d,9d,visible)        \ rtsl 
        vctrl(2d,12d,visible)       \ rtsl 
        vctrl(2d,18d,visible)       \ rtsl 
        vctrl(3d,25d,visible)       \ rtsl 
        vctrl(5d,35d,visible)       \ rtsl 
        vctrl(7d,50d,visible)       \ rtsl 
        vctrl(9d,70d,visible)       \ rtsl 
        vctrl(13d,99d,visible)      \ rtsl 
        vctrl(19d,141d,visible)     \ rtsl 
        vctrl(27d,199d,visible)     \ rtsl 
        
webln0b vctrl(0d,6d,visible)        \ rtsl 
        vctrl(0d,9d,visible)        \ rtsl 
        vctrl(0d,12d,visible)       \ rtsl 
        vctrl(0d,18d,visible)       \ rtsl 
        vctrl(0d,25d,visible)       \ rtsl 
        vctrl(0d,35d,visible)       \ rtsl 
        vctrl(0d,50d,visible)       \ rtsl 
        vctrl(0d,70d,visible)       \ rtsl 
        vctrl(0d,99d,visible)       \ rtsl 
        vctrl(0d,141d,visible)      \ rtsl 
        vctrl(0d,199d,visible)      \ rtsl 

;***************************************************
;* Linear Line Growth - Horizontal Left
;***************************************************            
weblnh0 vctrl(-2d,0d,visible)   \ rtsl  
weblnh1 vctrl(-3d,0d,visible)   \ rtsl  
weblnh2 vctrl(-4d,0d,visible)   \ rtsl  
weblnh3 vctrl(-6d,0d,visible)   \ rtsl  
weblnh4 vctrl(-8d,0d,visible)   \ rtsl  
weblnh5 vctrl(-11d,0d,visible)  \ rtsl  
weblnh6 vctrl(-16d,0d,visible)  \ rtsl  
weblnh7 vctrl(-23d,0d,visible)  \ rtsl  
weblnh8 vctrl(-32d,0d,visible)  \ rtsl  
weblnh9 vctrl(-45d,0d,visible)  \ rtsl  
weblnha vctrl(-64d,0d,visible)  \ rtsl  
weblnhb vctrl(-91d,0d,visible)  \ rtsl  
        
        
;**************************
;* Vertical Line Folds
;**************************
        
lvert00 vctr(66d,99d,visible)
        rtsl
        
lvert01 vctr(62d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-52d,-97d,visible)
        vctr(73d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-69d,-85d,visible)
        vctr(56d,85d,visible)
        rtsl
        
lvert02 vctr(54d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-33d,-87d,visible)
        vctr(74d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-66d,-63d,visible)
        vctr(41d,63d,visible)
        rtsl
        
lvert03 vctr(38d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(-9d,-68d,visible)
        vctr(68d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-57d,-32d,visible)
        vctr(20d,32d,visible)
        rtsl
        
lvert04 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        
lvert05 vctr(-47d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(90d,56d,visible)
        vctr(-3d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(20d,107d,visible)
        vctr(-73d,-107d,visible)
        rtsl
        
lvert06 vctr(-71d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(105d,95d,visible)
        vctr(-36d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(49d,136d,visible)
        vctr(-92d,-136d,visible)
        rtsl
        
lvert07 vctr(-87d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(107d,124d,visible)
        vctr(-66d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(74d,148d,visible)
        vctr(-99d,-148d,visible)
        rtsl
        
lvert08 vctr(-93d,-140d,visible)
        rtsl        
        
lvert10 vctr(53d,99d,visible)
        rtsl
        
lvert11 vctr(50d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-39d,-97d,visible)
        vctr(60d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-56d,-85d,visible)
        vctr(43d,85d,visible)
        rtsl
        
lvert12 vctr(43d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-23d,-87d,visible)
        vctr(63d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-55d,-63d,visible)
        vctr(30d,63d,visible)
        rtsl
        
lvert13 vctr(31d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(-1d,-68d,visible)
        vctr(61d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-49d,-32d,visible)
        vctr(12d,32d,visible)
        rtsl
        
lvert14 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        
lvert15 vctr(-37d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(81d,56d,visible)
        vctr(5d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(10d,107d,visible)
        vctr(-64d,-107d,visible)
        rtsl
        
lvert16 vctr(-56d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(91d,95d,visible)
        vctr(-22d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(35d,136d,visible)
        vctr(-78d,-136d,visible)
        rtsl
        
lvert17 vctr(-69d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(90d,124d,visible)
        vctr(-49d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(57d,148d,visible)
        vctr(-82d,-148d,visible)
        rtsl
        
lvert18 vctr(-74d,-140d,visible)
        rtsl
        
lvert20 vctr(39d,99d,visible)
        rtsl
        
lvert21 vctr(37d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-27d,-97d,visible)
        vctr(48d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-44d,-85d,visible)
        vctr(31d,85d,visible)
        rtsl
        
lvert22 vctr(32d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-12d,-87d,visible)
        vctr(52d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-44d,-63d,visible)
        vctr(19d,63d,visible)
        rtsl    
        
lvert23 vctr(23d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(6d,-68d,visible)
        vctr(53d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-41d,-32d,visible)
        vctr(4d,32d,visible)
        rtsl
        
lvert24 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        
lvert25 vctr(-28d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(71d,56d,visible)
        vctr(15d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(1d,107d,visible)
        vctr(-55d,-107d,visible)
        rtsl
        
lvert26 vctr(-42d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(77d,95d,visible)
        vctr(-7d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(21d,136d,visible)
        vctr(-64d,-136d,visible)
        rtsl
        
lvert27 vctr(-52d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(72d,124d,visible)
        vctr(-31d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(39d,148d,visible)
        vctr(-64d,-148d,visible)
        rtsl
        
lvert28 vctr(-56d,-140d,visible)
        rtsl
        
lvert30 vctr(26d,99d,visible)
        rtsl
        
lvert31 vctr(25d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-14d,-97d,visible)
        vctr(35d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-31d,-85d,visible)
        vctr(18d,85d,visible)
        rtsl
        
lvert32 vctr(21d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-1d,-87d,visible)
        vctr(41d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-34d,-63d,visible)
        vctr(9d,63d,visible)
        rtsl
        
lvert33 vctr(15d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(14d,-68d,visible)
        vctr(45d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-34d,-32d,visible)
        vctr(-2d,32d,visible)
        rtsl
        
lvert34 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        
lvert35 vctr(-18d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(62d,56d,visible)
        vctr(24d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(-8d,107d,visible)
        vctr(-45d,-107d,visible)
        rtsl
        
lvert36 vctr(-28d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(63d,95d,visible)
        vctr(6d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(6d,136d,visible)
        vctr(-49d,-136d,visible)
        rtsl
        
lvert37 vctr(-34d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(55d,124d,visible)
        vctr(-14d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(22d,148d,visible)
        vctr(-47d,-148d,visible)
        rtsl
        
lvert38 vctr(-37d,-140d,visible)
        rtsl
        
lvert40 vctr(13d,99d,visible)
        rtsl
        
lvert41 vctr(12d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-1d,-97d,visible)
        vctr(23d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-19d,-85d,visible)
        vctr(6d,85d,visible)
        rtsl
        
lvert42 vctr(10d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(9d,-87d,visible)
        vctr(30d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-23d,-63d,visible)
        vctr(-1d,63d,visible)
        rtsl
        
lvert43 vctr(7d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(22d,-68d,visible)
        vctr(37d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-26d,-32d,visible)
        vctr(-10d,32d,visible)
        rtsl
        
lvert44 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl

lvert45 vctr(-8d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(52d,56d,visible)
        vctr(34d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(-17d,107d,visible)
        vctr(-36d,-107d,visible)
        rtsl
        
lvert46 vctr(-14d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(49d,95d,visible)
        vctr(20d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(-7d,136d,visible)
        vctr(-35d,-136d,visible)
        rtsl
        
lvert47 vctr(-17d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(37d,124d,visible)
        vctr(2d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(4d,148d,visible)
        vctr(-30d,-148d,visible)
        rtsl
        
lvert48 vctr(-18d,-140d,visible)
        rtsl
        
lvert50 vctr(0d,99d,visible)
        rtsl
        
lvert51 vctr(0d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(10d,-97d,visible)
        vctr(10d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-6d,-85d,visible)
        vctr(-6d,85d,visible)
        rtsl
        
lvert52 vctr(0d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(20d,-87d,visible)
        vctr(20d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-12d,-63d,visible)
        vctr(-12d,63d,visible)
        rtsl
        
lvert53 vctr(0d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(29d,-68d,visible)
        vctr(29d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-18d,-32d,visible)
        vctr(-18d,32d,visible)
        rtsl
        
lvert54 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        
lvert55 vctr(0d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,56d,visible)
        vctr(43d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(-26d,107d,visible)
        vctr(-26d,-107d,visible)
        rtsl
        
lvert56 vctr(0d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(34d,95d,visible)
        vctr(34d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(-21d,136d,visible)
        vctr(-21d,-136d,visible)
        rtsl
        
lvert57 vctr(0d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(20d,124d,visible)
        vctr(20d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-12d,148d,visible)
        vctr(-12d,-148d,visible)
        rtsl
        
lvert58 vctr(0d,-140d,visible)
        rtsl
;***********************
        
hfold20 vctr(0d,45d,visible)
        vctr(34d,18d,hidden)
        vctr(-34d,-63d,visible)
        vctr(-40d,-44d,visible)
        vctr(16d,-32d,hidden)
        vctr(24d,76d,visible)
        vctr(20d,0d,visible)
        rtsl
        
hfold21 vctr(17d,41d,visible)
        vctr(28d,16d,hidden)
        vctr(-46d,-59d,visible)
        vctr(-17d,-40d,visible)
        vctr(12d,-28d,hidden)
        vctr(3d,70d,visible)
        vctr(36d,0d,visible)
        rtsl
        
hfold22 vctr(32d,32d,visible)
        vctr(20d,14d,hidden)
        vctr(-53d,-46d,visible)
        vctr(6d,-30d,visible)
        vctr(8d,-22d,hidden)
        vctr(-17d,53d,visible)
        vctr(45d,0d,visible)
        rtsl
        
hfold23 vctr(41d,17d,visible)
        vctr(10d,8d,hidden)
        vctr(-53d,-25d,visible)
        vctr(28d,-16d,visible)
        vctr(4d,-10d,hidden)
        vctr(-34d,28d,visible)
        vctr(48d,0d,visible)
        rtsl
        
hfold24 vctr(45d,0d,visible)
        rtsl
    
;**************************
;* Horizontal Line Folds
;************************** 
        
lhorz00 vctr(-45d,0d,visible)
        rtsl
        
lhorz01 vctr(-41d,17d,visible)
        vctr(12d,8d,hidden)
        vctr(28d,-24d,visible)
        vctr(-56d,-16d,visible)
        vctr(4d,-10d,hidden)
        vctr(50d,28d,visible)
        vctr(-33d,0d,visible)
        rtsl
        
lhorz02 vctr(-31d,32d,visible)
        vctr(22d,14d,hidden)
        vctr(8d,-46d,visible)
        vctr(-58d,-30d,visible)
        vctr(10d,-22d,hidden)
        vctr(47d,53d,visible)
        vctr(-16d,0d,visible)
        rtsl
        
lhorz03 vctr(-17d,41d,visible)
        vctr(28d,16d,hidden)
        vctr(-10d,-59d,visible)
        vctr(-49d,-40d,visible)
        vctr(12d,-28d,hidden)
        vctr(36d,70d,visible)
        vctr(2d,0d,visible)
        rtsl
        
lhorz04 vctr(0d,45d,visible)
        vctr(26d,18d,hidden)
        vctr(-27d,-63d,visible)
        vctr(-32d,-44d,visible)
        vctr(13d,-32d,hidden)
        vctr(19d,76d,visible)
        vctr(16d,0d,visible)
        rtsl
        
lhorz05 vctr(17d,41d,visible)
        vctr(22d,16d,hidden)
        vctr(-40d,-59d,visible)
        vctr(-9d,-40d,visible)
        vctr(10d,-28d,hidden)
        vctr(-1d,70d,visible)
        vctr(32d,0d,visible)
        rtsl
        
lhorz06 vctr(32d,32d,visible)
        vctr(16d,14d,hidden)
        vctr(-48d,-46d,visible)
        vctr(12d,-30d,visible)
        vctr(6d,-22d,hidden)
        vctr(-20d,53d,visible)
        vctr(42d,0d,visible)
        rtsl
        
lhorz07 vctr(41d,17d,visible)
        vctr(8d,8d,hidden)
        vctr(-50d,-25d,visible)
        vctr(32d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(-36d,28d,visible)
        vctr(47d,0d,visible)
        rtsl
        
lhorz08 vctr(45d,0d,visible)
        rtsl
        
lhorz10 vctr(-45d,0d,visible)
        rtsl
        
lhorz11 vctr(-41d,17d,visible)
        vctr(10d,8d,hidden)
        vctr(30d,-24d,visible)
        vctr(-53d,-16d,visible)
        vctr(4d,-10d,hidden)
        vctr(48d,28d,visible)
        vctr(-35d,0d,visible)
        rtsl
        
lhorz12 vctr(-32d,32d,visible)
        vctr(18d,14d,hidden)
        vctr(13d,-46d,visible)
        vctr(-52d,-30d,visible)
        vctr(8d,-22d,hidden)
        vctr(44d,53d,visible)
        vctr(-20d,0d,visible)
        rtsl
        
lhorz13 vctr(-17d,41d,visible)
        vctr(20d,16d,hidden)
        vctr(-4d,-59d,visible)
        vctr(-42d,-40d,visible)
        vctr(10d,-28d,hidden)
        vctr(32d,70d,visible)
        vctr(-2d,0d,visible)
        rtsl
        
lhorz14 vctr(0d,45d,visible)
        vctr(20d,18d,hidden)
        vctr(-20d,-63d,visible)
        vctr(-24d,-44d,visible)
        vctr(9d,-32d,hidden)
        vctr(14d,76d,visible)
        vctr(12d,0d,visible)
        rtsl
        
lhorz15 vctr(17d,41d,visible)
        vctr(16d,16d,hidden)
        vctr(-34d,-59d,visible)
        vctr(-2d,-40d,visible)
        vctr(6d,-28d,hidden)
        vctr(-5d,70d,visible)
        vctr(26d,0d,visible)
        rtsl
        
lhorz16 vctr(32d,32d,visible)
        vctr(10d,14d,hidden)
        vctr(-43d,-46d,visible)
        vctr(18d,-30d,visible)
        vctr(4d,-22d,hidden)
        vctr(-24d,53d,visible)
        vctr(39d,0d,visible)
        rtsl
        
lhorz17 vctr(41d,17d,visible)
        vctr(4d,8d,hidden)
        vctr(-47d,-25d,visible)
        vctr(35d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(-38d,28d,visible)
        vctr(45d,0d,visible)
        rtsl
        
lhorz18 vctr(45d,0d,visible)
        rtsl
        
lhorz20 vctr(-45d,0d,visible)
        rtsl
        
lhorz21 vctr(-41d,17d,visible)
        vctr(8d,8d,hidden)
        vctr(33d,-25d,visible)
        vctr(-50d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(47d,28d,visible)
        vctr(-36d,0d,visible)
        rtsl
        
lhorz22 vctr(-32d,32d,visible)
        vctr(12d,14d,hidden)
        vctr(18d,-46d,visible)
        vctr(-47d,-30d,visible)
        vctr(4d,-22d,hidden)
        vctr(41d,53d,visible)
        vctr(-22d,0d,visible)
        rtsl
        
lhorz23 vctr(-17d,41d,visible)
        vctr(14d,16d,hidden)
        vctr(2d,-59d,visible)
        vctr(-35d,-40d,visible)
        vctr(6d,-28d,hidden)
        vctr(27d,70d,visible)
        vctr(-6d,0d,visible)
        rtsl
        
lhorz24 vctr(0d,45d,visible)
        vctr(12d,18d,hidden)
        vctr(-13d,-63d,visible)
        vctr(-16d,-44d,visible)
        vctr(6d,-32d,hidden)
        vctr(9d,76d,visible)
        vctr(8d,0d,visible)
        rtsl
        
lhorz25 vctr(17d,41d,visible)
        vctr(10d,16d,hidden)
        vctr(-27d,-59d,visible)
        vctr(5d,-40d,visible)
        vctr(4d,-28d,hidden)
        vctr(-10d,70d,visible)
        vctr(22d,0d,visible)
        rtsl
        
lhorz26 vctr(32d,32d,visible)
        vctr(6d,14d,hidden)
        vctr(-38d,-46d,visible)
        vctr(24d,-30d,visible)
        vctr(2d,-22d,hidden)
        vctr(-27d,53d,visible)
        vctr(36d,0d,visible)
        rtsl
        
lhorz27 vctr(41d,17d,visible)
        vctr(2d,8d,hidden)
        vctr(-44d,-25d,visible)
        vctr(38d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(-39d,28d,visible)
        vctr(43d,0d,visible)
        rtsl
        
lhorz28 vctr(41d,17d,visible)
        vctr(2d,8d,hidden)
        vctr(-44d,-25d,visible)
        vctr(38d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(-39d,28d,visible)
        vctr(43d,0d,visible)
        rtsl
        
lhorz30 vctr(-45d,0d,visible)
        rtsl
        
lhorz31 vctr(-41d,17d,visible)
        vctr(4d,8d,hidden)
        vctr(36d,-25d,visible)
        vctr(-47d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(45d,28d,visible)
        vctr(-38d,0d,visible)
        rtsl
        
lhorz32 vctr(-32d,32d,visible)
        vctr(8d,14d,hidden)
        vctr(23d,-46d,visible)
        vctr(-41d,-30d,visible)
        vctr(2d,-22d,hidden)
        vctr(37d,53d,visible)
        vctr(-26d,0d,visible)
        rtsl
        
lhorz33 vctr(-17d,41d,visible)
        vctr(8d,16d,hidden)
        vctr(8d,-59d,visible)
        vctr(-27d,-40d,visible)
        vctr(4d,-28d,hidden)
        vctr(23d,70d,visible)
        vctr(-10d,0d,visible)
        rtsl
        
lhorz34 vctr(0d,45d,visible)
        vctr(6d,18d,hidden)
        vctr(-6d,-63d,visible)
        vctr(-8d,-44d,visible)
        vctr(3d,-32d,hidden)
        vctr(4d,76d,visible)
        vctr(4d,0d,visible)
        rtsl
        
lhorz35 vctr(17d,41d,visible)
        vctr(2d,16d,hidden)
        vctr(-21d,-59d,visible)
        vctr(12d,-40d,visible)
        vctr(0d,-28d,hidden)
        vctr(-14d,70d,visible)
        vctr(18d,0d,visible)
        rtsl
        
lhorz36 vctr(31d,32d,visible)
        vctr(0d,14d,hidden)
        vctr(-33d,-46d,visible)
        vctr(30d,-30d,visible)
        vctr(0d,-22d,hidden)
        vctr(-31d,53d,visible)
        vctr(32d,0d,visible)
        rtsl
        
lhorz37 vctr(41d,17d,visible)
        vctr(0d,8d,hidden)
        vctr(-42d,-25d,visible)
        vctr(41d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(-41d,28d,visible)
        vctr(41d,0d,visible)
        rtsl
        
lhorz38 vctr(45d,0d,visible)
        rtsl
        
lhorz40 vctr(-45d,0d,visible)
        rtsl
        
lhorz41 vctr(-41d,17d,visible)
        vctr(2d,8d,hidden)
        vctr(39d,-25d,visible)
        vctr(-44d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(43d,28d,visible)
        vctr(-40d,0d,visible)
        rtsl
        
lhorz42 vctr(-31d,32d,visible)
        vctr(2d,14d,hidden)
        vctr(28d,-46d,visible)
        vctr(-35d,-30d,visible)
        vctr(0d,-22d,hidden)
        vctr(34d,53d,visible)
        vctr(-28d,0d,visible)
        rtsl
        
lhorz43 vctr(-17d,41d,visible)
        vctr(2d,16d,hidden)
        vctr(14d,-59d,visible)
        vctr(-20d,-40d,visible)
        vctr(0d,-28d,hidden)
        vctr(19d,70d,visible)
        vctr(-14d,0d,visible)
        rtsl

;Rotation of Spinner horizontally        
lhorz44 vctr(0d,45d,visible)
        vctr(0d,18d,hidden)
        vctr(0d,-63d,visible)
        vctr(0d,-44d,visible)
        vctr(0d,-32d,hidden)
        vctr(0d,76d,visible)
        vctr(2d,0d,visible)
        rtsl
        
lhorz45 vctr(0d,45d,visible)
        vctr(-6d,8d,hidden)
        vctr(7d,-55d,visible)
        vctr(7d,-34d,visible)
        vctr(-2d,-35d,hidden)
        vctr(-4d,70d,visible)
        vctr(-4d,-6d,visible)
        rtsl
        
lhorz46 vctr(0d,45d,visible)
        vctr(-14d,0d,hidden)
        vctr(14d,-46d,visible)
        vctr(14d,-24d,visible)
        vctr(-5d,-38d,hidden)
        vctr(-8d,63d,visible)
        vctr(-8d,-12d,visible)
        rtsl
        
lhorz47 vctr(0d,45d,visible)
        vctr(-20d,-6d,hidden)
        vctr(21d,-38d,visible)
        vctr(20d,-14d,visible)
        vctr(-8d,-41d,hidden)
        vctr(-13d,56d,visible)
        vctr(-12d,-18d,visible)
        rtsl
        
lhorz48 vctr(0d,45d,visible)
        vctr(-28d,-14d,hidden)
        vctr(28d,-30d,visible)
        vctr(28d,-4d,visible)
        vctr(-10d,-44d,hidden)
        vctr(-17d,50d,visible)
        vctr(-16d,-24d,visible)
        rtsl
        
lhorz50 vctr(0d,45d,visible)
        vctr(-35d,-23d,hidden)
        vctr(35d,-22d,visible)
        vctr(35d,4d,visible)
        vctr(-13d,-47d,hidden)
        vctr(-22d,43d,visible)
        vctr(-22d,-30d,visible)
        rtsl
    
;************************************** 
;* Side View Spinner Rotation Frames  *
;************************************** 
spinnr_vpg = $60|thispage 

spinner0    vctr(-60d,19d,visible)
            vctr(-24d,48d,visible)
            vctr(27d,44d,visible)
            vctr(-67d,-132d,hidden)
            vctr(33d,-40d,visible)
            vctr(53d,8d,visible)
            vctr(37d,51d,visible)
            vctr(37d,-51d,visible)
            vctr(-8d,-53d,visible)
            vctr(-48d,-19d,visible)
            vctr(132d,67d,hidden)
            vctr(-3d,52d,visible)
            vctr(-48d,24d,visible)
            vctr(-60d,-19d,visible)
            vctr(0d,64d,visible)
            vctr(38d,38d,visible)
            vctr(51d,-12d,visible)
            rtsl
        
spinner1    vctr(-19d,-60d,visible)
            vctr(-48d,-24d,visible)
            vctr(-44d,27d,visible)
            vctr(132d,-67d,hidden)
            vctr(40d,33d,visible)
            vctr(-8d,53d,visible)
            vctr(-51d,37d,visible)
            vctr(51d,37d,visible)
            vctr(53d,-8d,visible)
            vctr(19d,-48d,visible)
            vctr(-67d,132d,hidden)
            vctr(-52d,-3d,visible)
            vctr(-24d,-48d,visible)
            vctr(19d,-60d,visible)
            vctr(-64d,0d,visible)
            vctr(-38d,38d,visible)
            vctr(12d,51d,visible)
            rtsl
        
spinner2    vctr(60d,-19d,visible)
            vctr(24d,-48d,visible)
            vctr(-27d,-44d,visible)
            vctr(67d,132d,hidden)
            vctr(-33d,40d,visible)
            vctr(-53d,-8d,visible)
            vctr(-37d,-51d,visible)
            vctr(-37d,51d,visible)
            vctr(8d,53d,visible)
            vctr(48d,19d,visible)
            vctr(-132d,-67d,hidden)
            vctr(3d,-52d,visible)
            vctr(48d,-24d,visible)
            vctr(60d,19d,visible)
            vctr(0d,-64d,visible)
            vctr(-38d,-38d,visible)
            vctr(-51d,12d,visible)
            rtsl
        
spinner3    vctr(19d,60d,visible)
            vctr(48d,24d,visible)
            vctr(44d,-27d,visible)
            vctr(-132d,67d,hidden)
            vctr(-40d,-33d,visible)
            vctr(8d,-53d,visible)
            vctr(51d,-37d,visible)
            vctr(-51d,-37d,visible)
            vctr(-53d,8d,visible)
            vctr(-19d,48d,visible)
            vctr(67d,-132d,hidden)
            vctr(52d,3d,visible)
            vctr(24d,48d,visible)
            vctr(-19d,60d,visible)
            vctr(64d,0d,visible)
            vctr(38d,-38d,visible)
            vctr(-12d,-51d,visible)
            rtsl
        
;***************************************    
;* Web Spinner Explosions              *
;***************************************    
cerexp0     vctr(5d,1d,visible)
            vctr(1d,0d,hidden)
            vctr(9d,1d,visible)
            vctr(2d,0d,hidden)
            vctr(21d,3d,visible)
            vctr(2d,0d,hidden)
            vctr(4d,1d,visible)
            vctr(-44d,-5d,hidden)
            rtsl
        
cerexp1     vctr(3d,1d,hidden)
            vctr(10d,3d,visible)
            vctr(1d,0d,hidden)
            vctr(22d,6d,visible)
            vctr(5d,2d,visible)
            vctr(-31d,-7d,hidden)
            vctr(6d,3d,visible)
            vctr(8d,6d,visible)
            vctr(6d,6d,visible)
            vctr(17d,-3d,hidden)
            vctr(3d,2d,visible)
            vctr(10d,-1d,hidden)
            vctr(2d,1d,visible)
            vctr(-62d,-19d,hidden)
            rtsl
        
cerexp2     vctr(0d,1d,hidden)
            vctr(15d,7d,visible)
            vctr(1d,1d,hidden)
            vctr(9d,6d,visible)
            vctr(1d,1d,hidden)
            vctr(7d,10d,visible)
            vctr(1d,0d,hidden)
            vctr(2d,4d,visible)
            vctr(-20d,-24d,hidden)
            vctr(5d,2d,visible)
            vctr(3d,1d,hidden)
            vctr(6d,3d,visible)
            vctr(5d,4d,visible)
            vctr(19d,14d,hidden)
            vctr(3d,2d,visible)
            vctr(5d,-5d,hidden)
            vctr(2d,1d,visible)
            vctr(-64d,-28d,hidden)
            rtsl
        
cerexp3     vctr(14d,11d,visible)
            vctr(10d,11d,visible)
            vctr(1d,7d,visible)
            vctr(-3d,5d,visible)
            vctr(-5d,4d,visible)
            vctr(-14d,4d,hidden)
            vctr(-3d,0d,visible)
            vctr(27d,-3d,hidden)
            vctr(4d,-4d,visible)
            vctr(3d,-6d,visible)
            vctr(3d,1d,hidden)
            vctr(-6d,-9d,visible)
            vctr(-6d,-6d,visible)
            vctr(-8d,-5d,visible)
            vctr(-17d,-10d,hidden)
            rtsl
        
cerexp4     vctr(29d,18d,hidden)
            vctr(0d,5d,visible)
            vctr(1d,6d,visible)
            vctr(3d,6d,visible)
            vctr(4d,4d,visible)
            vctr(2d,2d,hidden)
            vctr(9d,5d,visible)
            vctr(11d,3d,visible)
            vctr(-24d,-5d,hidden)
            vctr(-3d,-3d,visible)
            vctr(-4d,-5d,visible)
            vctr(-2d,-5d,visible)
            vctr(-26d,-30d,hidden)
            rtsl
        
cerexp5     vctr(18d,37d,hidden)
            vctr(1d,3d,visible)
            vctr(2d,3d,visible)
            vctr(4d,-3d,hidden)
            vctr(3d,4d,visible)
            vctr(-5d,2d,hidden)
            vctr(6d,5d,visible)
            vctr(7d,4d,visible)
            vctr(3d,-9d,hidden)
            vctr(8d,4d,visible)
            vctr(9d,2d,visible)
            vctr(-56d,-52d,hidden)
            rtsl
        
cerexp6     vctr(12d,38d,hidden)
            vctr(2d,4d,visible)
            vctr(6d,7d,hidden)
            vctr(4d,4d,visible)
            vctr(4d,3d,visible)
            vctr(10d,5d,hidden)
            vctr(4d,2d,visible)
            vctr(-42d,-63d,hidden)
            rtsl
        
cerexp7     vctr(8d,40d,hidden)
            vctr(1d,2d,visible)
            vctr(9d,14d,hidden)
            vctr(4d,3d,visible)
            vctr(21d,8d,hidden)
            vctr(3d,2d,visible)
            vctr(-46d,-69d,hidden)
            rtsl

;*****************************************
    .sbttl "Tactical Displays"
;*****************************************
tact_vpg    =   $60|thispage

tactd0  ;outer border first
        vcntr
        ;vstat(sparkle_off,xflip_off,thispage,$7,colblue)
        vctr(-896d,640d,hidden)
        vctr(0d,-1280d,visible)
        vctr(8d,-48d,visible)
        vctr(24d,-48d,visible)
        vctr(48d,-24d,visible)
        vctr(48d,-8d,visible)
        vctr(1536d,0d,visible)
        vctr(48d,8d,visible)
        vctr(48d,24d,visible)
        vctr(24d,48d,visible)
        vctr(8d,48d,visible)
        vctr(0d,1280d,visible)
        vctr(-8d,48d,visible)
        vctr(-24d,48d,visible)
        vctr(-48d,24d,visible)
        vctr(-48d,8d,visible)
        vctr(-1536d,0d,visible)
        vctr(-48d,-8d,visible)
        vctr(-48d,-24d,visible)
        vctr(-24d,-48d,visible)
        vctr(-8d,-48d,visible)
		rtsl
		
        ;inner border next
tactd1  vcntr
        ;vstat(sparkle_off,xflip_off,thispage,$7,colwhite)
        vctr(-800d,512d,hidden)
        vctr(0d,-1152d,visible)
        jsrl(llc)
        vctr(1536d,0d,visible)
        jsrl(lrc)
        vctr(0d,1152d,visible)
        jsrl(urc)
        vctr(-1536d,0d,visible)
        jmpl(ulc)
		;End with Jump for RTSL
		
        ;long range scanner
tactd2  vcntr
        ;vstat(sparkle_off,xflip_off,thispage,$7,colgreen)
        vctr(-768d,480d,hidden)
        vctr(0d,-1024d,visible)
        jsrl(llc)
        vctr(192d,0d,visible)
        jsrl(lrc)
        vctr(0d,1024d,visible)
        jsrl(urc)
        vctr(-192d,0d,visible)
        jmpl(ulc)
		;End with Jump for RTSL
		
        ;bottom of homeworld
tactd3  ;vstat(sparkle_off,xflip_off,thispage,$D,colred2)
        vctr(45d,26d,hidden)
        vctr(0d,-5d,visible)
        vctr(80d,-20d,visible)
        vctr(80d,20d,visible)
        vctr(0d,5d,visible)
		rtsl
		
        ;bottom planet
tactd4  ;vstat(sparkle_off,xflip_off,thispage,$D,colcyan)
        vctr(-174d,-1080d,hidden)
        vctr(20d,18d,visible)
        vctr(24d,12d,visible)
        vctr(24d,8d,visible)
        vctr(24d,2d,visible)
        vctr(24d,-2d,visible)
        vctr(24d,-8d,visible)
        vctr(24d,-12d,visible)
        vctr(20d,-18d,visible)
		rtsl

;long range scanner markers		
tactd5  vcntr
		vctr(-645d,-404d,hidden)
        vstat(sparkle_off,xflip_off,thispage,$F,colgreen)
        jsrl(planbx)
        vctr(0d,192,hidden)
        vstat(sparkle_off,xflip_off,thispage,$F,colpurple)
        jsrl(planbx)
        vctr(0d,192,hidden)
        vstat(sparkle_off,xflip_off,thispage,$F,colyellow)
        jsrl(planbx)
        vctr(0d,192,hidden)
        vstat(sparkle_off,xflip_off,thispage,$F,colred2)
        jsrl(planbx)
        vctr(0d,192d,hidden)
        vstat(sparkle_off,xflip_off,thispage,$F,colorange)
        jmpl(planbx)
		;End with Jump for RTSL
		
        ;upper right scanner box
tactd6  vcntr
        ;vstat(sparkle_off,xflip_off,thispage,$7,colcyan)
        vctr(512d,480d,hidden)
        vctr(0d,-320d,visible)
        jsrl(llc)
        vctr(192d,0d,visible)
        jsrl(lrc)
        vctr(0d,320d,visible)
        jsrl(urc)
        vctr(-192d,0d,visible)
        jmpl(ulc)
		;End with Jump for RTSL
		
        ;heart rate scanner box
tactd7  vcntr
        ;vstat(sparkle_off,xflip_off,thispage,$7,colred)
        vctr(512d,-32d,hidden)
        vctr(0d,-32d,visible)
        jsrl(llc)
        vctr(192d,0d,visible)
        jsrl(lrc)
        vctr(0d,32d,visible)
        jsrl(urc)
        vctr(-192d,0d,visible)
        jmpl(ulc)
		;End with Jump for RTSL
		
        ;breakout box
tactd8  vcntr
        ;vstat(sparkle_off,xflip_off,thispage,$7,colpurple)
        vctr(512d,-160d,hidden)
        vctr(0d,-448d,visible)
        jsrl(llc)
        vctr(192d,0d,visible)
        jsrl(lrc)
        vctr(0d,448d,visible)
        jsrl(urc)
        vctr(-192d,0d,visible)
        jmpl(ulc)
		;End with Jump for RTSL
		
        ;Center scanner box
tactd9  vcntr
        ;vstat(sparkle_off,xflip_off,thispage,$7,colyellow)
        vctr(-384d,448d,hidden)
        vctr(0d,-1024d,visible)
        vctr(4d,-24d,visible)
        vctr(12d,-24d,visible)
        vctr(24d,-12d,visible)
        vctr(24d,-4d,visible)
        vctr(640d,0d,visible)
        vctr(24d,4d,visible)
        vctr(24d,12d,visible)
        vctr(12d,24d,visible)
        vctr(4d,24d,visible)
        vctr(0d,1024d,visible)
        vctr(-4d,24d,visible)
        vctr(-12d,24d,visible)
        vctr(-24d,12d,visible)
        vctr(-24d,4d,visible)
        vctr(-640d,0d,visible)
        vctr(-24d,-4d,visible)
        vctr(-24d,-12d,visible)
        vctr(-12d,-24d,visible)
        vctr(-4d,-24d,visible)
        rtsl

;*****************************************
    .sbttl "Corner Subroutines"
;*****************************************

llc     vctr(2d,-12d,visible)
        vctr(6d,-12d,visible)
        vctr(12d,-6d,visible)
        vctr(12d,-2d,visible)
        rtsl
        
lrc     vctr(12d,2d,visible)
        vctr(12d,6d,visible)
        vctr(6d,12d,visible)
        vctr(2d,12d,visible)
        rtsl
        
urc     vctr(-2d,12d,visible)
        vctr(-6d,12d,visible)
        vctr(-12d,6d,visible)
        vctr(-12d,2d,visible)
        rtsl
        
ulc     vctr(-12d,-2d,visible)
        vctr(-12d,-6d,visible)
        vctr(-6d,-12d,visible)
        vctr(-2d,-12d,visible)
        rtsl

;*****************************************
    .sbttl "Planet Box (Small Dot)"
;*****************************************
planbx  vctr(4d,0d,visible)
        vctr(0d,-4d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,4d,visible)
        rtsl

;****************************************************
    .sbttl "Breakout Brick (+ pos to next brick"
;****************************************************
brick_vpg   =   $60|thispage

brickp  vctr(16d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,8d,visible)
brline  vctr(22d,0d,hidden)
        rtsl
        
padlep  vctr(24d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(-24d,0d,visible)
        vctr(0d,8d,visible)
        rtsl

;****************************************************
    .sbttl "Long Range Scanner Box"
;****************************************************

lrsrbx  ;vctr(0d,-32d,visible)
        ;vctr(1d,-3d,visible)
        ;vctr(4d,-4d,visible)
        ;vctr(3d,-1d,visible)
        ;vctr(80d,0d,visible)
        ;vctr(3d,1d,visible)
        ;vctr(4d,4d,visible)
        ;vctr(1d,3d,visible)
        ;vctr(0d,32d,visible)
        ;vctr(-1d,3d,visible)
        ;vctr(-4d,4d,visible)
        ;vctr(-3d,1d,visible)
        ;vctr(-80d,0d,visible)
        ;vctr(-3d,-1d,visible)
        ;vctr(-4d,-4d,visible)
        ;vctr(-1d,-3d,visible)
        vctr(42d,-44d,hidden)
        vctr(-2d,-8d,visible)
        vctr(16d,0d,visible)
        vctr(-2d,8d,visible)
        vctr(1d,-4d,hidden)
        vctr(-3d,1d,visible)
        vctr(-4d,14d,visible)
        vctr(-4d,-14d,visible)
        vctr(-3d,-1d,visible)
        vctr(-14d,10d,hidden)
        vctr(-36d,18d,visible)
        vctr(76d,-18d,hidden)
        vctr(36d,18d,visible)
        rtsl


;****************************************************
    .sbttl "Shot Explosions"
;****************************************************
shtex_vpg =     $60|thispage

exp9    vstat(sparkle_off,xflip_off,thispage,$F,colwhite)
        vctr(-18d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-18d,-18d,hidden)
        vctr(4d,0d,visible)
        vctr(18d,-18d,hidden)
        vctr(4d,0d,visible)
        vctr(26d,8d,hidden)
        vctr(4d,0d,visible)
        vctr(18d,-8d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,18d,hidden)
        vctr(4d,0d,visible)
        vctr(8d,26d,hidden)
        vctr(4d,0d,visible)
        vctr(-8d,18d,hidden)
        vctr(4d,0d,visible)
        vctr(-34d,-8d,hidden)
        vctr(4d,0d,visible)
        vctr(-26d,8d,hidden)
        vctr(4d,0d,visible)
        rtsl        
        
exp10   vstat(sparkle_off,xflip_off,thispage,$F,colwhite)
        vctr(-20d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-20d,-20d,hidden)
        vctr(4d,0d,visible)
        vctr(20d,-20d,hidden)
        vctr(4d,0d,visible)
        vctr(30d,10d,hidden)
        vctr(4d,0d,visible)
        vctr(20d,-10d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,20d,hidden)
        vctr(4d,0d,visible)
        vctr(10d,30d,hidden)
        vctr(4d,0d,visible)
        vctr(-10d,30d,hidden)
        vctr(4d,0d,visible)
        vctr(-40d,-10d,hidden)
        vctr(4d,0d,visible)
        vctr(-30d,10d,hidden)
        vctr(4d,0d,visible)
        rtsl

exp11   vstat(sparkle_off,xflip_off,thispage,$E,colyellow)
        vctr(-22d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-22d,-22d,hidden)
        vctr(4d,0d,visible)
        vctr(22d,-22d,hidden)
        vctr(4d,0d,visible)
        vctr(34d,12d,hidden)
        vctr(4d,0d,visible)
        vctr(22d,-12d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,22d,hidden)
        vctr(4d,0d,visible)
        vctr(12d,34d,hidden)
        vctr(4d,0d,visible)
        vctr(-12d,34d,hidden)
        vctr(4d,0d,visible)
        vctr(-46d,-12d,hidden)
        vctr(4d,0d,visible)
        vctr(-34d,12d,hidden)
        vctr(4d,0d,visible)
        rtsl

exp12   vstat(sparkle_off,xflip_off,thispage,$C,colyellow)
        vctr(-24d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-24d,-24d,hidden)
        vctr(4d,0d,visible)
        vctr(24d,-24d,hidden)
        vctr(4d,0d,visible)
        vctr(36d,12d,hidden)
        vctr(4d,0d,visible)
        vctr(24d,-12d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,24d,hidden)
        vctr(4d,0d,visible)
        vctr(12d,36d,hidden)
        vctr(4d,0d,visible)
        vctr(-12d,36d,hidden)
        vctr(4d,0d,visible)
        vctr(-48d,-12d,hidden)
        vctr(4d,0d,visible)
        vctr(-36d,12d,hidden)
        vctr(4d,0d,visible)
        rtsl
        
exp13   vstat(sparkle_off,xflip_off,thispage,$E,colred)
        vctr(-26d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-26d,-26d,hidden)
        vctr(4d,0d,visible)
        vctr(26d,-26d,hidden)
        vctr(4d,0d,visible)
        vctr(38d,12d,hidden)
        vctr(4d,0d,visible)
        vctr(26d,-12d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,26d,hidden)
        vctr(4d,0d,visible)
        vctr(12d,38d,hidden)
        vctr(4d,0d,visible)
        vctr(-12d,38d,hidden)
        vctr(4d,0d,visible)
        vctr(-52d,-12d,hidden)
        vctr(-4d,0d,visible)
        vctr(-38d,14d,hidden)
        vctr(4d,0d,visible)
        rtsl
		
exp14   vstat(sparkle_off,xflip_off,thispage,$D,colred)
        vctr(-28d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-28d,-28d,hidden)
        vctr(4d,0d,visible)
        vctr(28d,-28d,hidden)
        vctr(4d,0d,visible)
        vctr(42d,14d,hidden)
        vctr(4d,0d,visible)
        vctr(28d,-14d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,28d,hidden)
        vctr(4d,0d,visible)
        vctr(14d,42d,hidden)
        vctr(4d,0d,visible)
        vctr(-14d,42d,hidden)
        vctr(4d,0d,visible)
        vctr(-56d,-14d,hidden)
        vctr(4d,0d,visible)
        vctr(-42d,14d,hidden)
        vctr(4d,0d,visible)
        rtsl

exp15   vstat(sparkle_off,xflip_off,thispage,$C,colred)
        vctr(-30d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-30d,-30d,hidden)
        vctr(4d,0d,visible)
        vctr(30d,-30d,hidden)
        vctr(4d,0d,visible)
        vctr(44d,14d,hidden)
        vctr(4d,0d,visible)
        vctr(30d,-14d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,30d,hidden)
        vctr(4d,0d,visible)
        vctr(14d,44d,hidden)
        vctr(4d,0d,visible)
        vctr(-14d,44d,hidden)
        vctr(4d,0d,visible)
        vctr(-60d,-14d,hidden)
        vctr(4d,0d,visible)
        vctr(-44d,14d,hidden)
        vctr(4d,0d,visible)
        rtsl        

exp16   vstat(sparkle_off,xflip_off,thispage,$B,colred)
        vctr(-32d,0d,hidden)
        vctr(4d,0d,visible)
        vctr(-32d,-32d,hidden)
        vctr(4d,0d,visible)
        vctr(32d,-32d,hidden)
        vctr(4d,0d,visible)
        vctr(48d,16d,hidden)
        vctr(4d,0d,visible)
        vctr(32d,-16d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,32d,hidden)
        vctr(4d,0d,visible)
        vctr(16d,48d,hidden)
        vctr(4d,0d,visible)
        vctr(-16d,48d,hidden)
        vctr(4d,0d,visible)
        vctr(-64d,-16d,hidden)
        vctr(4d,0d,visible)
        vctr(-48d,16d,hidden)
        vctr(4d,0d,visible)
        rtsl

;****************************************************
    .sbttl "Ion Cannon Shots"
;****************************************************
lshot_vpg =     $60|thispage

ashot   vctr(-4d,0d,hidden)
        vctr(4d,12d,visible)
        vctr(4d,-12d,visible)
        vctr(-4d,-12d,visible)
        vctr(-4d,12d,visible)
        rtsl

bshot   vctr(-6d,0d,hidden)
        vctr(6d,10d,visible)
        vctr(6d,-10d,visible)
        vctr(-6d,-10d,visible)
        vctr(-6d,10d,visible)
        rtsl

cshot   vctr(-8d,0d,hidden)
        vctr(8d,8d,visible)
        vctr(8d,-8d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,8d,visible)
        rtsl

dshot   vctr(-10d,0d,hidden)
        vctr(10d,6d,visible)
        vctr(10d,-6d,visible)
        vctr(-10d,-6d,visible)
        vctr(-10d,6d,visible)
        rtsl

eshot   vctr(-12d,0d,hidden)
        vctr(12d,4d,visible)
        vctr(12d,-4d,visible)
        vctr(-12d,-4d,visible)
        vctr(-12d,4d,visible)
        rtsl

fshot   vctr(16d,4d,visible)
        vctr(-4d,-16d,visible)
        vctr(-16d,-4d,visible)
        vctr(4d,16d,visible)
        vctr(0d,-12d,hidden)
        vctr(-4d,16d,visible)
        vctr(16d,-4d,visible)
        vctr(4d,-16d,visible)
        vctr(-16d,4d,visible)
        rtsl
        
gshot   vctr(16d,8d,visible)
        vctr(-8d,-16d,visible)
        vctr(-16d,-8d,visible)
        vctr(8d,16d,visible)
        vctr(0d,-8d,hidden)
        vctr(-8d,16d,visible)
        vctr(16d,-8d,visible)
        vctr(8d,-16d,visible)
        vctr(-16d,8d,visible)
        rtsl
        
hshot   vctr(16d,12d,visible)
        vctr(-12d,-16d,visible)
        vctr(-16d,-12d,visible)
        vctr(12d,16d,visible)
        vctr(0d,-4d,hidden)
        vctr(-12d,16d,visible)
        vctr(16d,-12d,visible)
        vctr(12d,-16d,visible)
        vctr(-16d,12d,visible)
        rtsl
        
ishot   vctr(14d,6d,visible)
        vctr(-6d,-14d,visible)
        vctr(-14d,-6d,visible)
        vctr(6d,14d,visible)
        vctr(0d,-8d,hidden)
        vctr(-6d,14d,visible)
        vctr(14d,-6d,visible)
        vctr(6d,-14d,visible)
        vctr(-14d,6d,visible)
        rtsl
        
jshot   vctr(12d,8d,visible)
        vctr(-8d,-12d,visible)
        vctr(-12d,-8d,visible)
        vctr(8d,12d,visible)
        vctr(0d,-4d,hidden)
        vctr(-8d,12d,visible)
        vctr(12d,-8d,visible)
        vctr(8d,-12d,visible)
        vctr(-12d,8d,visible)
        rtsl
        
nwsht0  jsrl(ashot) ;$7BC6
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(-2d,6d,hidden)
        jsrl(fshot) ;$7C02
        rtsl
        
nwsht1  jsrl(bshot) ;$7BD2
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(2d,4d,hidden)
        jsrl(gshot) ;$7C16
        rtsl
        
nwsht2  jsrl(cshot) ;$7BDE
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(6d,2d,hidden)
        jsrl(hshot) ;$7C2A
        rtsl
        
nwsht3  jsrl(dshot) ;$7BEA
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(6d,4d,hidden)
        jsrl(gshot) ;$7C16
        rtsl
        
nwsht4  jsrl(eshot) ;$7BF6
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(6d,6d,hidden)
        jsrl(fshot) ;$7C02
        rtsl
        
nwsht5  jsrl(dshot) ;$7BEA
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(6d,4d,hidden)
        jsrl(ishot) ;$7C3E
        rtsl
        
nwsht6  jsrl(cshot) ;$7BDE
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(6d,2d,hidden)
        jsrl(jshot) ;$7C52
        rtsl
        
nwsht7  jsrl(bshot) ;$7BD2
        vstat(sparkle_off,xflip_off,thispage,$A,colred2)
        vctr(2d,4d,hidden)
        jsrl(ishot) ;$7C3E
        rtsl
        


        
    #if $ > $7FFF \ .error "VECTOR Page 2 has extended outside of design size." \ #endif
	        
;********************************
;* Vector Page 2 Checksum
;******************************** 
    .org $7FFF
    .chk $6000,$7FFF,IDENTIFIER_V2
;********************************

    .end

;**************************************
;* VROM Page 2 exports
.export shipsh
.export rod0,rod1,rod2,rod3,bigrod0,bigrod1,bigrod2,bigrod3
.export head0,head1,head2,head3,head4,head5,head6
.export tail0,tail1,gun0,gun1,gun2,gun3,eye0,eye1,eye2
.export body,bodyt,bigbody
.export mount,lgun0,lgun1,lgun2,lgun3
.export brl00,brl01,brl02,brl10,brl11,brl12,brl20,brl21,brl22,brl30,brl31,brl32
.export laz00,laz01,laz02,laz03,laz10,laz20,laz30,laz31,laz32,laz33
.export maxhead0,maxhead1,maxhead2,maxhead3,maxhead4,maxhead5,maxhead6,maxhead7
.export maxbody0,maxbody1,maxbody2,maxbody3,maxbody4,maxbody5,maxbody6,maxbody7
.export maxeye0,maxeye1,maxeye2,maxeye3,maxeye4,maxeye5,maxeye6,maxeye7

#IF MAX_LEVITATIONS != 0  
.export levt0,levt1,levt2,levt3
#ENDIF

.export beacn0,beacn1,beacn2,beacn3,beacn4,beacn5,beacn6,beacn7

.export spinner0,spinner1,spinner2,spinner3  

.export lvert00,lvert01,lvert02,lvert03,lvert04,lvert05,lvert06,lvert07,lvert08
.export lvert10,lvert11,lvert12,lvert13,lvert14,lvert15,lvert16,lvert17,lvert18
.export lvert20,lvert21,lvert22,lvert23,lvert24,lvert25,lvert26,lvert27,lvert28
.export lvert30,lvert31,lvert32,lvert33,lvert34,lvert35,lvert36,lvert37,lvert38
.export lvert40,lvert41,lvert42,lvert43,lvert44,lvert45,lvert46,lvert47,lvert48
.export lvert50,lvert51,lvert52,lvert53,lvert54,lvert55,lvert56,lvert57,lvert58

.export hfold20,hfold21,hfold22,hfold23,hfold24

.export lhorz00,lhorz01,lhorz02,lhorz03,lhorz04,lhorz05,lhorz06,lhorz07,lhorz08
.export lhorz10,lhorz11,lhorz12,lhorz13,lhorz14,lhorz15,lhorz16,lhorz17,lhorz18
.export lhorz20,lhorz21,lhorz22,lhorz23,lhorz24,lhorz25,lhorz26,lhorz27,lhorz28
.export lhorz30,lhorz31,lhorz32,lhorz33,lhorz34,lhorz35,lhorz36,lhorz37,lhorz38
.export lhorz40,lhorz41,lhorz42,lhorz43,lhorz44,lhorz45,lhorz46,lhorz47,lhorz48
.export lhorz50

.export cerexp0,cerexp1,cerexp2,cerexp3,cerexp4,cerexp5,cerexp6,cerexp7

.export webln00,webln01,webln02,webln03,webln04,webln05,webln06
.export webln07,webln08,webln09,webln0a,webln0b

.export weblnh0,weblnh1,weblnh2,weblnh3,weblnh4,weblnh5,weblnh6
.export weblnh7,weblnh8,weblnh9,weblnha,weblnhb

.export rods_vpg,body_vpg,gun_vpg,mzls_vpg,becn_vpg
.export shpsh_vpg,maxrob_vpg,spinnr_vpg,spstat_vpg
.export tact_vpg,brick_vpg,shtex_vpg,lshot_vpg

.export brickp,padlep,brline,lrsrbx
.export tactd0,tactd1,tactd2,tactd3,tactd4,tactd5,tactd6,tactd7,tactd8,tactd9

.export exp9,exp10,exp11,exp12,exp13,exp14,exp15,exp15,exp16
.export nwsht0,nwsht1,nwsht2,nwsht3,nwsht4,nwsht5,nwsht6,nwsht7 