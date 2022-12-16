#include "./havoc.ah"
#include "./exp/auxpgm_6.exp"
#include "./exp/auxpgm_7.exp"
#include "./exp/mh_alpha.exp"

.export mzsc00,mzinit,dynamic_base,mzdc,mzlg,mzar,mzor,mkeyp,mtok,mscstl
.export mztr,mztdal,mztd,mone,mtite,mlock,mtran,mhand,mclock,mboots,mpod,outime,oxybonus,trtbll,mcan,mzty,reacsz

.export messagesbase,mazehints,zmessptrl,zmessptrh,zmessypos,zmessxpos

.export chka2

#IF (LEVEL_EDITOR != 0)
.export levelst
#ENDIF

 .end