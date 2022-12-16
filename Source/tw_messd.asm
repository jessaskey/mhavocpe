;*********************************************
    .TEXT ".TWMESSD."
;*********************************************
    .sbttl "Message Data"
;*********************************************
;* Message Macro                             
;* Will build an absolute address table here 
;* of each message. Space for table has to be
;* initially declared adequately. The macro  
;* will also define a new label for each     
;* message that can be used to get the       
;* proper index into the message table.      
;*********************************************
___msgnum = 0
___numsgs = 190d	
___templen = 0
___messtmp = 0

messptrl .block ___numsgs
messptrh .block ___numsgs
messscol .block ___numsgs
messypos .block ___numsgs
messxpos .block ___numsgs

#define     mess(xlit,xcol,xsca,ypos,xpos)  \___messtmp .set $
#defcont	\ .org (messptrl+___msgnum)
#defcont    \ .byte (xlit&$FF)
#defcont    \ .org (messptrh+___msgnum)
#defcont    \ .byte ((xlit/$100)&$FF)
#defcont    \ .org (messscol + ___msgnum)
#defcont    \ .byte ((xcol*$10)|(xsca))
#defcont    \ .org (messypos+___msgnum)
#defcont    \ .byte ypos
#defcont    \ .org (messxpos+___msgnum)
#defcont    \ .byte xpos
#defcont    \m+xlit = ___msgnum
#defcont    \___msgnum .set ___msgnum+1
#defcont    \#if (___numsgs < ___msgnum) 
#defcont    \ .error "MSG: Number of messages exceeds defined limit, increase please!"
#defcont    \#endif 
#defcont    \ .org ___messtmp

#define     cmess(xlit,xcol,xsca,ypos,txtlen,offset)  \___messtmp .set $
#defcont	\___templen .set txtlen+offset
#defcont	\ .org (messptrl+___msgnum)
#defcont    \ .byte (xlit&$FF)
#defcont    \ .org (messptrh+___msgnum)
#defcont    \ .byte ((xlit/$100)&$FF)
#defcont    \ .org (messscol + ___msgnum)
#defcont    \ .byte ((xcol*$10)|(xsca))
#defcont    \ .org (messypos+___msgnum)
#defcont    \ .byte ypos
#defcont    \ .org (messxpos+___msgnum)
#defcont    \ .byte (-1*((___templen)*3))
#defcont    \m+xlit = ___msgnum
#defcont    \___msgnum .set ___msgnum+1
#defcont    \#if (___numsgs < ___msgnum) 
#defcont    \ .error "MSG: Number of messages exceeds defined limit, increase please!"
#defcont    \#endif 
#defcont    \ .org ___messtmp

#IF LANGUAGE = 2
#include "tw_messstr_fr.asm"
#ELSE
#IF LANGUAGE = 1
#include "tw_messstr_de.asm"
#ELSE
#include "tw_messstr_en.asm"
#ENDIF
#ENDIF
		   


;****************************************
;* Message Exports
.export messptrl,messptrh,messscol,messypos,messxpos
            
; Message Numbers
.export mplayr,mgtsc,menin,mgetout,mhis,mgamov,mcredi,mpress,mbonus,mreac,moxyg
.export minsert,mhint0,melife,mbolif,mcmodd,maddm1,maddm2,mcmode,mcmod1,mcmod2,mcmod3,matari
.export mpub0,mpub1,mcont,mgarbage,mtesser0,mtesser1,mtesser2,mtesser3,mahome,mconf,mmdist,mcols,mhold,metyp,mmlevel
.export mmwred,mmwyel,mmwgrn,mmwaqu,mmwblue,mmwpurp,mmwpink
;.export mmwspkl,mwstrn
.export msphr,mfigh,mspin,mfort,mmainf
.export mwarpi,mwarp0,mwarp1,mwarp2,mwarp3,mwarp4,mwarp5;,mwarp6
.export mtmes0,mtmes1,mtmes2,mtmes3,mtmes4,mcgrn,mcyel,mcred,mccrit
.export mpromise

.export mtext0,mtext1,mtext2,mtext3,mtext4,mtext5,mtext6,mtext7,mtext8,mtext9
.export mtext10,mtext11,mtext12,mtext13,mtext14,mtext15,mtext16,mtext17,mtext18,mtext19
.export mtxtcmfr
;.export mtxtcm13,mtxtcm25

.export msco0,msco27,mledit
.export mendtx00,mendtxxx

