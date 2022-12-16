;***************************************************************
;* Major Havoc MainLine Program (Alpha Processor)              
;*                                                             
;* This ROM image will reside at 0x8000-0xffff                 
;* Definitions for paged ROM areas and all vector ROM are      
;* defined in their associated export files. Because TASM does 
;* not used a relocatable linker, those sections must be       
;* compiled first in order for a valid export file to exist.    
;* Care should be taken to be sure that the export definition  
;* files for each section are correct                          
;***************************************************************
;* Files for the Gamma and Beta Processors are compiled        
;* seperately. Alpha will import Gamma exports for valid
;* sound commands etc
;***************************************************************

.locallabelchar "?"

#include "logic.ah"
#include "macros.ah"
#include "vector.ah"
#include "havoc.ah"

#include "./exp/mh_vrom.exp"
#include "./exp/auxpgm_0.exp"
#include "./exp/auxpgm_1.exp"
#include "./exp/auxpgm_2.exp"
#include "./exp/auxpgm_3.exp"
#include "./exp/auxpgm_4.exp"
#include "./exp/auxpgm_5.exp"
#include "./exp/auxpgm_6.exp"
#include "./exp/auxpgm_7.exp"
#include "./exp/mh_gamma.exp"
#include "./exp/mh_beta.exp"

.module alpha
#include "a_def.ah"
#include "a_ram.ah"

	.org $8000
;*****************************************************************
;* Alpha Low ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_AL
	.byte MAJOR_VERSION,MINOR_VERSION
;*****************************************************************
#include ./tw_motion.asm
#include ./tw_enemy.asm
#include ./tw_ship.asm
#include ./tw_map.asm
;********************************
;* Alpha Low Checksum
;********************************
chka1    .chk $8000,$BFFF,IDENTIFIER_AL

#if $ > $C000 \ .error "Alpha LOW has extended outside of design size." \ #endif

	.org $c000
;*****************************************************************
;* Alpha High ROM Header
;*****************************************************************
	.word ((LANGUAGE & $FF) * $100) + IDENTIFIER_AH
	.byte MAJOR_VERSION,MINOR_VERSION
;*****************************************************************
#include ./tw_maze.asm
#include ./tw_things.asm
#include ./tw_util.asm
#include ./tw_coin.asm
#include ./tw_vector.asm
#include ./tw_control.asm
#include ./tw_main.asm
#include ./tw_slave.asm
#include ./tw_mess.asm

;Make TEST routines up high
;.org $EE00

#include ./tw_test.asm 

;********************************
;* Alpha High Checksum
;********************************
chka2   .chk $C000,$FFFF,IDENTIFIER_AH

#if $ > $FFFA \ .error "Alpha HIGH has extended outside of design size." \ #endif

;******************************************
;* Don't forget to do the entry points!!! 
;******************************************	
	.org $fffa
	
		.word pwron
		.word pwron
		.word irq

	.end

;****************************************
;* Global Exports
;****************************************
; Temps
.export temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,tempa
.export perm1,perm2,perm3,perm4,perm5

.export updflg,frame,wrhflg,pl1last,pl2last,digits,hscore,gtesser,vdata,mtim,colchkx,colchky

.export updwho,updcur,updint,rgdr,wrpdl,initl,jblast,jbstat

; Vector Routines
.export vgcntr,vgadd2,vgcntr,vgvtr5,vgadd,vgvtr1,vgvtr2,vgbrit,vgvtr,vgscal,vgjmpl
.export vgjsrl,vgvctr,vgreset,vgrtsl,vgscalz,vgstat,copypic

; Stuff for TW_SPCMZ
.export initcz   
;these moved into tw_spcmz
;,prepare,linmult,linscal,dist,webmult, 

.export sphere,fighter,spacemaze,spacefort

.export breakx

.export vecram
.export player,score,dostop,dodelay,dosound,out1s,bonusa,shipst 
.export map0,map1,map2,map3,map4,map5,map6
.export st_gtime1,st_warps,st_extlie,dblneg,howmbits,watchdog
.export noneleft,statyl,statyh,decimal,rawgamedif,gamedif,
.export cabsel,rollmult

.export hxtend,hytend,crsbuf,mapbuf,retbuf,retbufs,oxybuf,scobuf,scobuf2,bonbuf,scrbuf,trnbuf
.export qrtlog,fullog,fullog2,sin,cos,tstat,scbdis,sobjst,trinds,tcount,piccur,picdely,mzgame
.export lauen,lincur,outputs,manstat,nxtdly,olmznm,target
.export tactde,nxtptr,bronce,statst,tact,rearview ;,colcnt
.export difcty,sndcue,spcspd
.export websper,websseg
.export scalef,posvc2,hitpts,hitwebs,vglist,gridx,gridy

;space variable remaps
.export webscur,webssta,websnum,webss2
.export websxl,websxh,websyl,websyh
.export shotxl,shotxh,shotyl,shotyh,shotst
.export shtspd,shotcur

.export shipxl,shipxh,blowship,multiply
.export getrand,cxflip,posvc2,nmsshots,bpont2,linen
.export objst,zreactor,retime,tspark,tmatch,nodraw
.export mazexl,mazexh,mazeyl,mazeyh,neg,xoffset,vunits,ymot,xmot,mazpt
.export holmz,openflg,maznum,maztype,gamest,mzgrnd,mazebuf,dif4mz,isfinal,unitp,hunits
.export drawtok

; New Stuff
;.export getdifctyb
;.export getdifctyw

; Maze Objects
.export zman,nmfire,zfire,nmrob,zrobot,nmmax,zmax,nmlock,nmkeys,zlock,zkeys,nmmotobj
.export nmlsht,zlsht,nmcann,zcann,nmshot,zshot,nmtite,ztite,nmtran,ztran
.export nmstuf,zstuf,nmdisc,zdisc,nmligh,zligh,nmfrfl,zfrfl,nmtrpp,ztrpp
.export nmonew,zonew,nmarow,zarow,nmsparks,ztoken
.export onewst,epodfr,epodgr,ardir,cannfr,cannss,canngr,canndf,cannin,cannp1,cannp2

; Max Stuff
.export maxdata,maxspd_slowest,maxspd_slow,maxspd_normal,maxspd_aggres
.export maxdist_0,maxdist_1,maxdist_2,maxdist_3,maxdist_4,maxdist_5,maxdist_6,maxdist_7
;.export maxdist_8,maxdist_9,maxdist_a,maxdist_b,maxdist_c,maxdist_d,maxdist_e,maxdist_f

; Max Custom Collisions
.export abvstmp,lftwal,abvg,lthdck,rthdck,rtwal,lcolv,sltcol,rcolv,srtcol,bitrt,bitlt
.export curstmp,tempgrnd,setlft,undg,rtchk,ltchk,setrt,abvchk,gndv,

.export mestim,jmptim,fldcnt,jumprv,tranhi,cktran,ntrans,ttran
.export naccx,naccy,maccx,maccy,daccx,daccy,raccy,botcase,stasav
.export mzgms,initshp,score2,wrplvl,scrflg,brick,stbflg,statxl,statyl
.export lastreac,maxdif,objxl,objxh,objyl,objyh,objfrm,side

;robot types
;.export rtypperk,rtypmax

; Colors
.export shtcol,fircol,headcol,mancol,mancol2,mazcol,fsscol,ltcol,rtcol,robcol,stcolr,defaultram,initcolram,flacol,reacol,rodcol,re2col
.export green,black,redr,yellow,blue

.export lsbsy,lsbsx,zindex,stloc,sttype,atsnd,pastbit,gndvt,celt
.export oldxl,velxh,velyh,robdir,robvel,limbo,reintn,rodstat,xflip
.export nxtexp,sparkle,sparkb,inblow,sparkangle
.export cannon,sldnfr,nxtdisc,accbuf,thisarw,zspecial
.export obsst,obssxl,obssxh,obssyl,obssyh
.export ztop,ground,rampg
.export shipdis2,ctran,xtran,ytran,colram

; Object Enums
.export objtypetab,obj_max

;Stuff for TWTACT
.export shpsch,shpscl,seqst,seqx,seqp,condition,brstat,ttime,targf,newscore
.export lroff,rands,tactln,wrpwh,diedyet,scoflg,rgdd,wrpdat,vghex,hscan,sdigits
.export ballxl,ballxh,ballyl,ballyh,ballvx,ballvy,nmballs,getswitch,colflg,ballsp,paddle,lives,yellow,warpcodes,wrp1in
.export ballsout,balltime,sincos,animate,lasttok,rmazenum

; #IF ENABLE_ADAPTIVE_DIFFICULTY != 0
; .export expdth,incded,addap
; #ENDIF

;.export litral,litra2,litra3,
.export litrast,strtst,strtyl,strtyh,strtln,slives,c_cmode

.export thisone_rtpg5,mesg_rtp5,bitflags,tokcolor,tokretr,toktarg,thisone

;Stuff for TWTBLS
.export twopl,addtim,bonsnd,pl2int,sellvl,chngplr,toolong,tumble,add4mz ;addmn,adddif
.export newmzr,direct,oxygen,bpoint,shipyh,shipyl,center,gmov
;HACK: Removed from Select-A-Level ,addmz,
.export demo,adden
.export spcini,landflg,jumpst,markgm,markpt,markls,shldok,shldht,atflag,plrsel
.export velxl,velyl,lastfire

;Stuff for TWCONTROL
.export dislive,velxh,dcreq,runseq,picseq,jogseq,pshbseq,landseq,sittime,face,upflg,nxtbonus
;.export twocoin,c_tcmflg
.export c_crdt,dosnd2,lastswitch,jboswt,nogamma,xmitr,olddata,xmit,i_xmigama
.export inputs,portrd,st_plrmzs,st_plrmzl
.export sendgstat,sendwstat

;Stuff for TWMAZE
.export nextmz,nxtmaz_rtpg2,nxtlvl,incmaz,newbrk

;Stuff for TWMAZE & TWMAZEO
.export tr_hidden,tr_match,tr_left,tr_right,tr_special,drawtok,maxsnd,setmaxv
;,tokvisi

.export oxystat
;Stuff for TWFIGHT
.export sobdir,sobjst,sobcog
;.export cor3p,enszx,enszy
;.export drawshot

;Stuff for TWMAZEG
.export o_resetg,o_resetb,o_lighton,tcolsq

;Stuff for TWHWSTUFF
.export vxstat,locate_rtpg4,tokpock,vxcount,strgen,nenemy,tossvel,posit
.export numstr,maxstr,strflg,strxl,strxh,stryl,stryh,drawstr,stroyl,stroyh
.export jumpr
.export oxycol,vgjsr1
.export emotseq
;.export robonly_rtpg4,welvaxx_rtpg4,velyh

;Stuff for TWEMOTE
.export emoteix

;Stuff for TWCASTLE
.export scflags,lasst,shirotl,shibits,shiroth,posvec,shiests,shiecovr,shiroen,drop2
.export drop3,emem12,coreg,mvenst,hitship,addvel
.export saucvl,saucvd,stbix,statxh,rtedge,dropcnt,sobjs2,nenstr,sobjxl,sobjxh,sobjyl,sobjyh
.export ckbase,castsnd

;Stuff for TWTEST
.export gotdata,stextp,statict,scol,setcolwhite,setcolyellow,stmesg,stmesgc,warpcolrtp1,statbuff
.export copymem,gammaerr,thisleta,getleta,o_swtchmux,dis01,initoption,diftbl,sndatt,cmodem
.export stb_game1,stb_game2,stb_atime1,stb_atime2,stb_xwarps,stb_extlie,stb_mzstats
.export stcmodz,stmlivem,bonusmg,setsel,gamlvsx
.export stmact2pg,stmactwrps,stmactxliv,stmgmstat,stmact1pg,stminpout,stmgsetts,stmpshelds
.export stmbtest,stmtestra,stmtestro,stmbcomm,stmbromlow,stmbromhig,stmbrammain
.export stmpaux,stmfiresel,stmrollsel,stmmfail,stmmpass,stmesgv
.export stmrollx1,stmrollx2
.export stmsndon,stmsndoff,stmlddips
.export chkbut,thisleta,lastleta,lastdir,doconfirm,bconfirm,nobut,gammawait,getgdon
.export set_cmode,set_diff,set_lives,set_bonus,set_rollmult,set_attsd
.export rollcnt,setoptn0,setoptn1,do3del,updroller,d6502,showversion,shromsum,pgvetbl
.export comram,comtestbase

;Message exports
.export msg_x,msg_y,msg_idx


.export sendmstat
.export resframe

;****************************************
;* Color Indexes
.export colblack,colblue,colgreen,colcyan,colflash,colpurple,colyellow,colwhite,colwhiter,colpink,colorange,colred2,colred,colcyanr,colbluer,colgreenr

;****************************************
;* External Routines
.export mesg, mesgonly

;****************************************
; Special Exports for Level Editor
;****************************************
.export chka2                         

;;***************************************
;* Special Exports for Trace tooling
;****************************************
.export irq,mainloop,doggie,dovggo,mmaze,mspace
#IF ENABLE_WASTE = 1
.export dowaste
#ENDIF
