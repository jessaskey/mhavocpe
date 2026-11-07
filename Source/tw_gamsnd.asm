;*******************************************************************
;*******************************************************************
;*******************************************************************
    .sbttl "Tune Data"
;*******************************************************************
;* This is the actual data for each sound defined above
;*******************************************************************

    NEWSNDAMP(samp_blank)    
    NEWSNDFREQ(sfreq_blank)   
    NEWTUNE(s_blank) 
    ENDSND
	
    NEWSNDAMP(samp_coin) ;$07
	SCTRL(20,2)              ;Eff Slope:10     Dur:2    Net:20
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:20
	SCTRL(-4,2)              ;Eff Slope:-2     Dur:2    Net:16
	SCTRL(-2,14)             ;Eff Slope:-1     Dur:14   Net:2
	SCTRL(18,2)              ;Eff Slope:9      Dur:2    Net:20
	SCTRL(0,12)              ;Eff Slope:0      Dur:12   Net:20
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:18
	SCTRL(-4,4)              ;Eff Slope:-2     Dur:4    Net:10
	SCTRL(-2,8)              ;Eff Slope:-1     Dur:8    Net:2

    NEWSNDFREQ(sfreq_coin)   ;,$07
    SFREQ(40,2)              ;Eff Slope:2.5    Dur:2    Net:5
	SFREQ(0,22)              ;Eff Slope:0      Dur:22   Net:5
	SFREQ(16,2)              ;Eff Slope:1      Dur:2    Net:7
	SFREQ(0,28)              ;Eff Slope:0      Dur:28   Net:7

    NEWTUNE(s_coin)  
    TUNEFREQ(sfreq_coin)
    TUNEAMP(samp_coin)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$36)
    ENDSND
    
;****************************************
    NEWSNDAMP(samp_launch)  ;$08
    SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:4
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:4
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:8
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:8
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:10
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:10
	SCTRL(2,4)               ;Eff Slope:1      Dur:4    Net:14
	SCTRL(0,26)              ;Eff Slope:0      Dur:26   Net:14
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:16
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:16
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:20
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:20
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:22
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:22
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:24
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:24
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:26
	SCTRL(0,32)              ;Eff Slope:0      Dur:32   Net:26
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:28
	SCTRL(0,26)              ;Eff Slope:0      Dur:26   Net:28
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:30
	SCTRL(0,82)              ;Eff Slope:0      Dur:82   Net:30
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:28
	SCTRL(0,40)              ;Eff Slope:0      Dur:40   Net:28
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:26
	SCTRL(0,18)              ;Eff Slope:0      Dur:18   Net:26
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:24
	SCTRL(0,20)              ;Eff Slope:0      Dur:20   Net:24
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:22
	SCTRL(0,12)              ;Eff Slope:0      Dur:12   Net:22
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:20
	SCTRL(0,12)              ;Eff Slope:0      Dur:12   Net:20
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:18
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:18
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:16
	SCTRL(0,10)              ;Eff Slope:0      Dur:10   Net:16
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:14
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:14
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:12
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:12
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:10
	SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:10
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:8
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:8
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:6
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:6
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:4
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:4
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:2
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:2
    
    NEWSNDFREQ(sfreq_launch) ;,$08
    SFREQ(1832,2)            ;Eff Slope:114.5  Dur:2    Net:229
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:228
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:225
	SFREQ(-8,10)             ;Eff Slope:-0.5   Dur:10   Net:220
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:216
	SFREQ(-48,2)             ;Eff Slope:-3     Dur:2    Net:210
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:206
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:204
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:200
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:198
	SFREQ(-32,4)             ;Eff Slope:-2     Dur:4    Net:190
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:188
	SFREQ(-40,2)             ;Eff Slope:-2.5   Dur:2    Net:183
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:180
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:179
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:177
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:175
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:173
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:171
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:169
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:166
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:165
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:165
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:162
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:161
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:159
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:156
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:154
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:152
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:151
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:147
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:147
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:145
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:142
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:141
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:139
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:137
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:135
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:132
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:132
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:131
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:128
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:127
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:125
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:123
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:121
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:118
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:117
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:115
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:113
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:109
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:109
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:107
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:104
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:100
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:98
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:96
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:93
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:92
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:90
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:88
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:85
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:83
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:81
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:79
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:78
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:76
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:76
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:72
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:71
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:68
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:67
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:67
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:65
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:64
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:61
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:59
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:59
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:57
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:54
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:52
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:52
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:50
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:49
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:49
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:47
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:46
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:46
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:45
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:43
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:43
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:41
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:40
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:40
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:38
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:37
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:37
	SFREQ(-8,6)              ;Eff Slope:-0.5   Dur:6    Net:34
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:34
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:31
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:31
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:29
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:29
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:28
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:28
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:27
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:27
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:26
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:26
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:25
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:25
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:24
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:24
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:23
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:23
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:22
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:22
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:21
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:21
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:20
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:20
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:19
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:19
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:18
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:18
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:17
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:17
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:16
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:16
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:15
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:15
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:14
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:14
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:13
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:13
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:12
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:12
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:11
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:11
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:10
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:10
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:9
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:9
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:8
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:8
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:7
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:7
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:6
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:6
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:5
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:5
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:4
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:4
    
    NEWTUNE(s_launch)
    TUNEFREQ(sfreq_launch)
    TUNEAMP(samp_launch)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$FE)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$8F)
    ENDSND
    
;**********************************
    NEWSNDAMP(samp_launch2) ;$09
    SCTRL(30,2)              ;Eff Slope:15     Dur:2    Net:30
	SCTRL(0,34)              ;Eff Slope:0      Dur:34   Net:30
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:28
	SCTRL(0,18)              ;Eff Slope:0      Dur:18   Net:28
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:26
	SCTRL(0,20)              ;Eff Slope:0      Dur:20   Net:26
	SCTRL(-2,4)              ;Eff Slope:-1     Dur:4    Net:22
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:22
	SCTRL(-2,4)              ;Eff Slope:-1     Dur:4    Net:18
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:18
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:16
	SCTRL(-4,2)              ;Eff Slope:-2     Dur:2    Net:12
	SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:12
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:10
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:10
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:8
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:8
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:6
	SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:6
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:4
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:4
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:2
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:2
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:0
	SCTRL(0,254)             ;Eff Slope:0      Dur:254  Net:0
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:0
    
    NEWSNDFREQ(sfreq_launch2)    ;,$09
    SFREQ(720,2)             ;Eff Slope:45     Dur:2    Net:90
	SFREQ(0,254)             ;Eff Slope:0      Dur:254  Net:90
	SFREQ(0,140)             ;Eff Slope:0      Dur:140  Net:90
    
    NEWTUNE(s_launch2)
    TUNEFREQ(sfreq_launch2)
    TUNEAMP(samp_launch2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$FE)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$8F)
    ENDSND
    
;****************************************
    NEWSNDAMP(samp_passby)  ;$0a
    SCTRL(2,4)               ;Eff Slope:1      Dur:4    Net:4
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:8
	SCTRL(6,2)               ;Eff Slope:3      Dur:2    Net:14
	SCTRL(8,4)               ;Eff Slope:4      Dur:4    Net:30
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:30
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:28
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:28
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:26
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:26
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:24
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:24
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:22
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:22
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:20
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:20
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:18
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:18
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:16
	SBEGIN
	 SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:16
	 SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:14
	SLOOP(6)                ;Loop Back 6 times.
	SCTRL(0,26)              ;Eff Slope:0      Dur:26   Net:14
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:12
	SCTRL(0,152)             ;Eff Slope:0      Dur:152  Net:12
    
    NEWSNDFREQ(sfreq_passby) ;,$0a
	SFREQ(112,2)             ;Eff Slope:7      Dur:2    Net:14
	SFREQ(-8,10)             ;Eff Slope:-0.5   Dur:10   Net:9
	SFREQ(8,254)             ;Eff Slope:0.5    Dur:254  Net:136
	SFREQ(8,12)              ;Eff Slope:0.5    Dur:12   Net:142
	SFREQ(-1136,2)           ;Eff Slope:-71    Dur:2    Net:0
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:0
    
    NEWTUNE(s_passby)   
    TUNEFREQ(sfreq_passby)
    TUNEAMP(samp_passby)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$FE)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$03)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$1B)
    ENDSND
    
;****************************************
    NEWSNDAMP(samp_passby2) ;$0b
    .byte $04,$04
	.byte $02,$08
	.byte $02,$0C
	.byte $04,$10
	.byte $02,$00
	.byte $02,$FC
	.byte $02,$00
	.byte $02,$FC
	.byte $02,$00
	.byte $04,$FC
	.byte $FF,$02
	.byte $07,$02
	.byte $00,$0A
	.byte $FC,$02
	.byte $00,$02
	.byte $FC,$02
	.byte $00,$02
	.byte $FC,$F8
	.byte $00
    
    NEWSNDFREQ(sfreq_passby2)    ;,$0b
	SFREQ(208,2)             ;Eff Slope:13     Dur:2    Net:26
	SFREQ(-8,10)             ;Eff Slope:-0.5   Dur:10   Net:21
	SFREQ(8,254)             ;Eff Slope:0.5    Dur:254  Net:148
	SFREQ(8,2)               ;Eff Slope:0.5    Dur:2    Net:149
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:146
	SFREQ(8,10)              ;Eff Slope:0.5    Dur:10   Net:151
	SFREQ(16,2)              ;Eff Slope:1      Dur:2    Net:153
	SFREQ(-1224,2)           ;Eff Slope:-76.5  Dur:2    Net:0
	SFREQ(0,22)              ;Eff Slope:0      Dur:22   Net:0
	
    ; .byte $02,$A0,$01
	; .byte $0A,$F0,$FF
	; .byte $FE,$10,$00
	; .byte $02,$10,$00
	; .byte $02,$D0,$FF
	; .byte $0A,$10,$00
	; .byte $02,$20,$00
	; .byte $02,$70,$F6
	; .byte $16,$00,$00
    
    NEWTUNE(s_passby2)
    TUNEFREQ(sfreq_passby2)
    TUNEAMP(samp_passby2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$FE)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$1F)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$03)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$13)
    ENDSND
    
;***************************************
    NEWSNDAMP(samp_galert)  ;$0c
    .byte $02,$0C
	.byte $FE,$00
    
    NEWSNDFREQ(sfreq_galert) ;,$0c
    .byte $02,$30,$07,$02,$10,$00,$02,$00,$00,$02,$00,$FE,$04,$00,$00,$02,$20
    .byte $01,$04,$00,$00,$02,$60,$FE,$04,$00,$00,$02,$60,$02,$04,$00,$00
    .byte $02,$20,$FE,$04,$00,$00,$02,$C0,$02,$04,$00,$00,$02,$00,$FE,$04
    .byte $00,$00,$02,$00,$01,$04,$00,$00,$02,$30,$FE,$04,$00,$00,$02,$20
    .byte $00,$04,$00,$00,$02,$B0,$FE,$04,$00,$00,$02,$00,$05,$04,$00,$00
    .byte $02,$C0,$FD,$04,$00,$00,$02,$E0,$FF,$04,$00,$00,$02,$50,$FE,$04
    .byte $00,$00,$02,$A0,$02,$04,$00,$00,$02,$20,$FE,$04,$00,$00,$02,$50
    .byte $03,$04,$00,$00,$02,$C0,$FD,$04,$00,$00,$02,$10,$04,$04,$00,$00
    .byte $02,$30,$FD,$04,$00,$00,$02,$90,$01,$04,$00,$00,$02,$90,$FD,$04
    .byte $00,$00,$02,$E0,$FE,$04,$00,$00,$02,$80,$FE,$04,$00,$00,$02,$C0
    .byte $01,$04,$00,$00,$02,$70,$FE,$04,$00,$00,$02,$D0,$01,$04,$00,$00
    .byte $02,$60,$FE,$04,$00,$00,$02,$E0,$01,$04,$00,$00,$02,$50,$FE,$04
    .byte $00,$00,$02,$10,$02,$04,$00,$00,$02,$30,$FE,$04,$00,$00,$02,$40
    .byte $02,$04,$00,$00,$02,$20,$FE,$04,$00,$00,$02,$50,$02,$04,$00,$00
    .byte $02,$00,$FE,$04,$00,$00,$02,$80,$02,$04,$00,$00,$02,$E0,$FD,$04
    .byte $00,$00,$02,$A0,$02,$04,$00,$00,$02,$C0,$FD,$04,$00,$00,$02,$D0
    .byte $02,$04,$00,$00
    
    NEWTUNE(s_galert)
    TUNEFREQ(sfreq_galert)
    TUNEAMP(samp_galert)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$FE)
    ENDSND
    
;*************************************  
    NEWSNDAMP(samp_blaunch) ;$0e
    .byte $02,$3C
	.byte $04,$00
    
    NEWSNDFREQ(sfreq_blaunch)    ;,$0e
    .byte $02,$10,$05
	.byte $04,$00,$00

    NEWTUNE(s_blaunch)  
    TUNEFREQ(sfreq_blaunch)
    TUNEAMP(samp_blaunch)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$06)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_bbrick)  ;$0f
    .byte $02,$3C	
	.byte $04,$00
    
    NEWSNDFREQ(sfreq_bbrick) ;,$0f
	.byte $02,$80,$03
	.byte $02,$10,$00
	.byte $02,$00,$00
	    
    NEWTUNE(s_bbrick)   
    TUNEFREQ(sfreq_bbrick)
    TUNEAMP(samp_bbrick)
    SNOATTRACT
    SETBVOL($00)				;Starting Volume level
    SETDIST(CHCTL_NOPOLY)		;Upper CTRL bits (
    NOTE(_REST,$06)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_bpaddle) ;,$10
    .byte $02,$3C,$04
	.byte $00
    
    NEWSNDFREQ(sfreq_bpaddle)    ;,$10
    .byte $02,$90,$07
	.byte $04,$00,$00
	
    ;79
    NEWTUNE(s_bpaddle)  
    TUNEFREQ(sfreq_bpaddle)
    TUNEAMP(samp_bpaddle)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$06)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_bmissp)  ;,$11
    .byte $02,$3C
	.byte $22,$00
    
    NEWSNDFREQ(sfreq_bmissp) ;,$11
    .byte $02,$40,$0E
	.byte $02,$30,$00
	.byte $02,$D0,$FF
	.byte $FF,$04,$09
	.byte $02,$30,$00
	.byte $02,$F0,$FF
	.byte $02,$40,$00
	.byte $02,$E0,$FF
	.byte $02,$50,$00
	.byte $02,$D0,$FF
	.byte $02,$60,$00
    
    NEWTUNE(s_bmissp)   
    TUNEFREQ(sfreq_bmissp)
    TUNEAMP(samp_bmissp)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$24)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_digit)   ;,$12
	.byte $02,$10 ;02		$C2,$C4,$C7,$CA,$CC,$CF,$CF,$CF,... $CD,$CB,$C9,$C7
	.byte $02,$18 ;03		$02,$02,$03,$03,$02,$03
	.byte $02,$14 ;02
	.byte $0E,$00 ;00
	.byte $06,$F0 ;FE
	.byte $02,$F4 ;FE
	
    
    NEWSNDFREQ(sfreq_digit)  ;,$12		;;$26,$4C,$4A,$48,
	.byte $02,$C0,$04		
	.byte $02,$C0,$FF
	.byte $02,$50,$FF
	.byte $02,$C0,$FF
	.byte $02,$A0,$FF
	.byte $04,$D0,$FF
    .byte $02,$E0,$FF
	.byte $02,$D0,$FF
	.byte $02,$C0,$FF
	.byte $08,$00,$00


    NEWTUNE(s_digit)    
    TUNEFREQ(sfreq_digit)
    TUNEAMP(samp_digit)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$1C)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_notused1)    ;,$13
    NEWSNDFREQ(sfreq_notused1)   ;,$13
    NEWTUNE(s_notused1) 
    ENDSND

;**************************************
    NEWSNDAMP(samp_notused2)    ;,$14
    NEWSNDFREQ(sfreq_notused2)   ;,$14
    NEWTUNE(s_notused2) 
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_notused3)    ;,$15
    NEWSNDFREQ(sfreq_notused3)   ;,$15
    NEWTUNE(s_notused3) 
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_ccorrect)    ;,$16
    .byte $02,$20
	.byte $12,$00
	.byte $02,$F8
	.byte $06,$00
	.byte $02,$FC
	.byte $02,$00
	.byte $FF,$02
	.byte $07,$04
    .byte $FC,$3C
	.byte $00
    
    NEWSNDFREQ(sfreq_ccorrect)  ;,$16
    .byte $02,$20,$03,$02,$F0,$00,$02,$70,$FE,$02,$00,$01,$02
    .byte $90,$FE,$02,$F0,$00,$02,$A0,$FE,$02,$F0,$00,$02,$C0,$FE,$02,$E0
    .byte $00,$02,$F0,$FE,$02,$D0,$00,$02,$10,$FF,$02,$C0,$00,$02,$20,$FF
    .byte $02,$B0,$00,$FF,$02,$09,$02,$40,$FF,$02,$B0,$00,$FF,$04,$09,$02
    .byte $50,$FF,$02,$A0,$00,$02,$50,$FF,$02,$A0,$00,$02,$60,$FF,$02,$90
    .byte $00,$02,$60,$FF,$02,$90,$00,$02,$70,$FF,$02,$80,$00,$02,$70,$FF
    .byte $02,$80,$00,$02,$80,$FF,$02,$70,$00,$02,$80,$FF,$02,$70,$00,$02
    .byte $90,$FF,$02,$60,$00,$02,$90,$FF,$02,$60,$00,$02,$A0,$FF,$02,$60
    .byte $00,
    
    NEWTUNE(s_ccorrect) 
    TUNEFREQ(sfreq_ccorrect)
    TUNEAMP(samp_ccorrect)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$68)
    ENDSND

;**************************************
    NEWSNDAMP(samp_exp) ;,$17
    .byte $02,$3C
	.byte $FE,$00
	.byte $FE,$00
    
    NEWSNDFREQ(sfreq_exp)    ;,$17
    .byte $04,$00,$00
	.byte $FE,$10,$00
	.byte $FE,$10,$00
    
    NEWTUNE(s_exp)  
    TUNEFREQ(sfreq_exp)
    TUNEAMP(samp_exp)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$FE)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$FF)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$03)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_exp2)    ;,$18
    .byte $02,$1C,$10,$04
	.byte $28,$00,$02,$FC
	.byte $0A,$00,$02,$F8
    .byte $08,$00,$02,$FC
	.byte $FF,$08,$07,$06
	.byte $00,$02,$FC,$04
	.byte $00,$02,$FC,$02
    .byte $00,$02,$FC
    
    NEWSNDFREQ(sfreq_exp2)   ;,$18
    .byte $02,$60,$00,$02,$A0,$FF,$02,$00,$00,$02,$A0,$00,$02
    .byte $00,$00,$02,$60,$FF,$02,$00,$00,$02,$90,$00,$02,$C0,$FF,$02,$10
    .byte $00,$02,$D0,$FF,$02,$30,$00,$02,$40,$00,$02,$C0,$FF,$02,$A0,$00
    .byte $02,$30,$FF,$02,$00,$00,$02,$30,$01,$02,$00,$00,$02,$60,$FF,$02
    .byte $00,$00,$02,$80,$00,$02,$90,$FF,$02,$50,$00,$02,$F0,$FF,$02,$B0
    .byte $FF,$04,$80,$00,$02,$00,$00,$02,$80,$FF,$02,$D0,$FF,$02,$00,$00
    .byte $02,$B0,$00,$02,$80,$FF,$02,$00,$02,$02,$70,$FE,$02,$00,$01,$02
    .byte $00,$FF,$02,$90,$00,$02,$90,$01,$02,$00,$00,$02,$20,$FF,$02,$00
    .byte $00,$02,$B0,$01,$02,$10,$FF,$02,$20,$FF,$02,$00,$00,$02,$60,$02
    .byte $02,$E0,$FE,$02,$00,$00,$02,$60,$FF,$02,$60,$01,$02,$30,$FE,$02
    .byte $F0,$02,$02,$F0,$FE,$02,$00,$00,$02,$40,$FF,$02,$B0,$02,$02,$10
    .byte $FD,$02,$00,$00,$02,$40,$01,$02,$00,$00,$02,$50,$FF,$02,$00,$00
    .byte $02,$D0,$01,$02,$50,$FF,$02,$C0,$01,$02,$70,$FD,$02,$00,$00,$02
    .byte $40,$01,$02,$00,$00,$02,$80,$FF,$02,$00,$00,$02,$70,$01,$02,$E0
    .byte $FE,$02,$B0,$00,$02,$00,$00,$02,$F0,$00,$02,$A0,$FD,$02,$40,$01
    .byte $02,$00,$00,$02,$70,$FF,$02,$30,$01,$02,$B0,$00,$02,$D0,$FD,$02
    .byte $00,$00,$02,$10,$01,$02,$00,$00,$02,$D0,$01,$02,$00,$00
    
    NEWTUNE(s_exp2) 
    SETDCTL($80)
    TUNEFREQ(sfreq_exp2)
    TUNEAMP(samp_exp2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$B4)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_exp3)    ;,$19)
    .byte $02,$3C
    .byte $02,$00,$02,$C4,$04,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC,$FF,$02
    .byte $07,$04,$00,$02,$FC,$FF,$02,$07,$06,$00,$02,$FC,$0C,$00,$02,$FC
    .byte $0A,$00,$02,$FC,$FF,$02,$07,$0E,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_exp3)   ;,$19
    .byte $02,$70,$01,$06,$10
    .byte $00,$08,$F0,$FF,$08,$10,$00,$FF,$06,$09,$02,$F0,$FF
    
    NEWTUNE(s_exp3) 
    TUNEFREQ(sfreq_exp3)
    TUNEAMP(samp_exp3)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$7A)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_exp4)    ;,$1a
    .byte $02,$3C,$02,$00,$02
    .byte $C4,$04,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC,$02,$00,$02,$FC,$02
    .byte $00,$02,$FC,$04,$00,$FF,$04,$07,$02,$FC,$02,$00,$02,$FC,$02,$00
    .byte $02,$FC,$04,$00,$02,$FC,$04,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_exp4)   ;,$1a
    .byte $02,$D0,$00,$06,$10,$00
    .byte $08,$F0,$FF,$08,$10,$00,$08,$F0,$FF,$FF,$03,$09
    
    NEWTUNE(s_exp4) 
    TUNEFREQ(sfreq_exp4)
    TUNEAMP(samp_exp4)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$50)
    ENDSND
;**************************************
    NEWSNDAMP(samp_exp5)    ;,$1b
    .byte $02,$3C,$0C,$00,$02,$C4
    .byte $06,$00,$02,$3C,$06,$00,$02,$FC,$FF,$03,$07,$04,$00,$02,$FC,$04
    .byte $00,$02,$FC,$02,$00,$02,$FC,$06,$00,$02,$FC,$02,$00,$02,$FC,$02
    .byte $00,$02,$FC,$04,$00,$02,$FC,$06,$00,$02,$FC,$06,$00,$02,$FC,$08
    .byte $00,$02,$FC,$06,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_exp5)   ;,$1b
    .byte $02,$60,$07,$0A,$F0,$FF,$0C,$10,$00
    .byte $0C,$F0,$FF,$FF,$03,$09,$0C,$10,$00,$08,$F0,$FF
    
    NEWTUNE(s_exp5) 
    TUNEFREQ(sfreq_exp5)
    TUNEAMP(samp_exp5)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$80)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_exp6)    ;,$1c
    .byte $02,$3C,$06,$00,$02,$C4
    .byte $06,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC,$FF,$02,$07,$04,$00,$02
    .byte $FC,$FF,$02,$07,$06,$00,$02,$FC,$FF,$02,$07,$08,$00,$02,$FC,$FF
    .byte $02,$07
    
    NEWSNDFREQ(sfreq_exp6)   ;,$1c
    .byte $02,$C0,$03,$6A,$00,$00
    
    NEWTUNE(s_exp6) 
    TUNEFREQ(sfreq_exp6)
    TUNEAMP(samp_exp6)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$6C)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_pfire)   ;,$1d
    .byte $02,$1C,$0A,$00
    
    NEWSNDFREQ(sfreq_pfire)  ;,$1d
    .byte $02,$60,$01,$02,$70,$00
    .byte $02,$B0,$00,$02,$D0,$00,$02,$30,$01,$02,$80,$01
    
    NEWTUNE(s_pfire)    
    TUNEFREQ(sfreq_pfire)
    TUNEAMP(samp_pfire)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$0C)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_shotstat)    ;,$1e
    .byte $02,$3C
	.byte $04,$00
	.byte $06,$F8
	.byte $02,$F4
	.byte $02,$F8
    
    NEWSNDFREQ(sfreq_shotstat)   ;,$1e
    .byte $12,$00,$00
    
    NEWTUNE(s_shotstat)
    TUNEFREQ(sfreq_shotstat)
    TUNEAMP(samp_shotstat)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$10)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_fishhatch)   ;,$1f
    .byte $02,$20,$12,$00,$02,$F8,$06,$00,$02,$FC,$02
    .byte $00,$02,$FC,$02,$00,$02,$FC,$02,$00,$06,$FC
    
    NEWSNDFREQ(sfreq_fishhatch)  ;,$1f
    .byte $02,$20,$03,$02,$F0
    .byte $00,$02,$70,$FE,$02,$00,$01,$02,$90,$FE,$02,$F0,$00,$02,$A0,$FE
    .byte $02,$F0,$00,$02,$C0,$FE,$02,$E0,$00,$02,$F0,$FE,$02,$D0,$00,$02
    .byte $10,$FF,$02,$C0,$00,$02,$20,$FF,$02,$B0,$00,$02,$30,$FF,$02,$B0
    .byte $00,$02,$30,$FF,$02,$B0,$00,$02,$40,$FF,$02,$B0,$00,$02,$C0,$FE

    NEWTUNE(s_fishhatch)
    TUNEFREQ(sfreq_fishhatch)
    TUNEAMP(samp_fishhatch)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$2E)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_reactor) ;,$22
    .byte $02,$28,$04,$00,$02,$FC,$02,$00,$02,$FC,$FF,$05,$07,$06
    .byte $00,$02,$FC,$06,$00,$02,$FC,$20,$00,$02,$04,$06,$00,$02,$04,$06
    .byte $00,$02,$04,$02,$00,$FF,$05,$07,$02,$04,$06,$00
    
    NEWSNDFREQ(sfreq_reactor)    ;,$22
    .byte $02,$80,$03,$02
    .byte $F0,$FF,$02,$40,$00,$02,$A0,$FF,$02,$80,$00,$02,$A0,$FF,$02,$40
    .byte $00,$04,$E0,$FF,$FF,$07,$15,$02,$40,$00,$02,$A0,$FF,$02,$80,$00
    .byte $02,$A0,$FF,$02,$40,$00,$02,$E0,$FF
    
    NEWTUNE(s_reactor)
    TUNEFREQ(sfreq_reactor)
    TUNEAMP(samp_reactor)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$80)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_reactor2)    ;,$23
    .byte $02,$04,$04,$00,$02,$04,$02,$00,$FF
    .byte $05,$07,$02,$04,$06,$00,$02,$04,$06,$00,$02,$04,$20,$00,$02,$FC
    .byte $06,$00,$02,$FC,$06,$00,$02,$FC,$02,$00,$FF,$05,$07,$02,$FC,$06,$00
    
    NEWSNDFREQ(sfreq_reactor2)   ;,$23
    .byte $02,$B0,$01,$02,$E0,$FF,$02,$40,$00,$02,$A0,$FF,$02,$80,$00
    .byte $02,$A0,$FF,$02,$40,$00,$04,$E0,$FF,$FF,$07,$15,$02,$40,$00,$02
    .byte $A0,$FF,$02,$80,$00,$02,$A0,$FF,$02,$40,$00,$02,$E0,$FF

    NEWTUNE(s_reactor2) 
    TUNEFREQ(sfreq_reactor2)
    TUNEAMP(samp_reactor2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$80)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_goosefish)   ;,$24
    .byte $02,$04,$04,$10,$02,$18,$12,$00,$02,$E8,$04,$F0
    
    NEWSNDFREQ(sfreq_goosefish)  ;,$24
    .byte $02,$10,$06,$0A,$00,$00,$14,$E0,$FF

    NEWTUNE(s_goosefish)    
    SETDCTL($40)
    TUNEFREQ(sfreq_goosefish)
    TUNEAMP(samp_goosefish)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$20)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_blowfish)    ;,$25
    .byte $02,$1C,$10,$04,$28,$00,$02,$FC,$0A,$00,$02,$F8,$08,$00,$02
    .byte $FC,$FF,$08,$07,$06,$00,$02,$FC,$04,$00,$02,$FC,$02,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_blowfish)   ;,$25
    .byte $02,$60,$00,$02,$A0,$FF,$02,$00,$00,$02,$A0,$00,$02,$00,$00,$02
    .byte $60,$FF,$02,$00,$00,$02,$90,$00,$02,$C0,$FF,$02,$10,$00,$02,$D0
    .byte $FF,$02,$30,$00,$02,$40,$00,$02,$C0,$FF,$02,$A0,$00,$02,$30,$FF
    .byte $02,$00,$00,$02,$30,$01,$02,$00,$00,$02,$60,$FF,$02,$00,$00,$02
    .byte $80,$00,$02,$90,$FF,$02,$50,$00,$02,$F0,$FF,$02,$B0,$FF,$04,$80
    .byte $00,$02,$00,$00,$02,$80,$FF,$02,$D0,$FF,$02,$00,$00,$02,$B0,$00
    .byte $02,$80,$FF,$02,$00,$02,$02,$70,$FE,$02,$00,$01,$02,$00,$FF,$02
    .byte $90,$00,$02,$90,$01,$02,$00,$00,$02,$20,$FF,$02,$00,$00,$02,$B0
    .byte $01,$02,$10,$FF,$02,$20,$FF,$02,$00,$00,$02,$60,$02,$02,$E0,$FE
    .byte $02,$00,$00,$02,$60,$FF,$02,$60,$01,$02,$30,$FE,$02,$F0,$02,$02
    .byte $F0,$FE,$02,$00,$00,$02,$40,$FF,$02,$B0,$02,$02,$10,$FD,$02,$00
    .byte $00,$02,$40,$01,$02,$00,$00,$02,$50,$FF,$02,$00,$00,$02,$D0,$01
    .byte $02,$50,$FF,$02,$C0,$01,$02,$70,$FD,$02,$00,$00,$02,$40,$01,$02
    .byte $00,$00,$02,$80,$FF,$02,$00,$00,$02,$70,$01,$02,$E0,$FE,$02,$B0
    .byte $00,$02,$00,$00,$02,$F0,$00,$02,$A0,$FD,$02,$40,$01,$02,$00,$00
    .byte $02,$70,$FF,$02,$30,$01,$02,$B0,$00,$02,$D0,$FD,$02,$00,$00,$02
    .byte $10,$01,$02,$00,$00,$02,$D0,$01,$02,$00,$00
    
    NEWTUNE(s_blowfish)
    SETDCTL($80)
    TUNEFREQ(sfreq_blowfish)
    TUNEAMP(samp_blowfish)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$B4)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_blowfish2)   ;,$26
    .byte $02,$1C,$10,$04,$28
    .byte $00,$02,$FC,$0A,$00,$02,$F8,$08,$00,$02,$FC,$FF,$08,$07,$06,$00
    .byte $02,$FC,$04,$00,$02,$FC,$02,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_blowfish2)  ;,$26
    .byte $02,$60,$00,$02,$A0,$FF
    .byte $02,$00,$00,$02,$A0,$00,$02,$00,$00,$02,$60,$FF,$02,$00,$00,$02
    .byte $90,$00,$02,$C0,$FF,$02,$10,$00,$02,$D0,$FF,$02,$30,$00,$02,$40
    .byte $00,$02,$C0,$FF,$02,$A0,$00,$02,$30,$FF,$02,$00,$00,$02,$30,$01
    .byte $02,$00,$00,$02,$60,$FF,$02,$00,$00,$02,$80,$00,$02,$90,$FF,$02
    .byte $50,$00,$02,$F0,$FF,$02,$B0,$FF,$04,$80,$00,$02,$00,$00,$02,$80
    .byte $FF,$02,$D0,$FF,$02,$00,$00,$02,$B0,$00,$02,$80,$FF,$02,$00,$02
    .byte $02,$70,$FE,$02,$00,$01,$02,$00,$FF,$02,$90,$00,$02,$90,$01,$02
    .byte $00,$00,$02,$20,$FF,$02,$00,$00,$02,$B0,$01,$02,$10,$FF,$02,$20
    .byte $FF,$02,$00,$00,$02,$60,$02,$02,$E0,$FE,$02,$00,$00,$02,$60,$FF
    .byte $02,$60,$01,$02,$30,$FE,$02,$F0,$02,$02,$F0,$FE,$02,$00,$00,$02
    .byte $40,$FF,$02,$B0,$02,$02,$10,$FD,$02,$00,$00,$02,$40,$01,$02,$00
    .byte $00,$02,$50,$FF,$02,$00,$00,$02,$D0,$01,$02,$50,$FF,$02,$C0,$01
    .byte $02,$70,$FD,$02,$00,$00,$02,$40,$01,$02,$00,$00,$02,$80,$FF,$02
    .byte $00,$00,$02,$70,$01,$02,$E0,$FE,$02,$B0,$00,$02,$00,$00,$02,$F0
    .byte $00,$02,$A0,$FD,$02,$40,$01,$02,$00,$00,$02,$70,$FF,$02,$30,$01
    .byte $02,$B0,$00,$02,$D0,$FD,$02,$00,$00,$02,$10,$01,$02,$00,$00,$02
    .byte $D0,$01,$02,$00,$00

    NEWTUNE(s_blowfish2)    
    SETDCTL($80)
    TUNEFREQ(sfreq_blowfish2)
    TUNEAMP(samp_blowfish2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$B4)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_circfish)    ;,$27
    .byte $14,$04,$12,$00
    .byte $02,$F0,$02,$FC
    .byte $02,$00,$02,$FC
    .byte $16,$00,$02,$FC
    .byte $02,$00,$02,$FC
    .byte $02,$00,$02,$FC
    .byte $02,$00,$02,$FC
    .byte $2C,$00
    
    NEWSNDFREQ(sfreq_circfish)   ;,$27
    .byte $02,$F0
    .byte $0F,$24,$00,$00
    .byte $02,$F0,$FE,$56
    .byte $00,$00
    
    NEWTUNE(s_circfish)
    TUNEFREQ(sfreq_circfish)
    TUNEAMP(samp_circfish)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$7E)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_circfish2)   ;,$28
    .byte $0A,$04,$06,$00,$02,$04,$08,$00,$06,$FC,$08,$00,$02,$04
    .byte $1A,$00,$04,$FC,$06,$00,$04,$FC,$2C,$00
    
    NEWSNDFREQ(sfreq_circfish2)  ;,$28
    .byte $02,$E0,$0F,$24,$00,$00,$02,$10,$FF,$56,$00,$00
    
    NEWTUNE(s_circfish2)
    TUNEFREQ(sfreq_circfish2)
    TUNEAMP(samp_circfish2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$7E)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_feject)  ;,$29
    .byte $02,$3C,$06,$F8,$02,$FC,$02,$00,$02,$FC,$02,$00,$02,$FC,$04,$00,$02
    .byte $FC,$12,$00,$02,$FC,$06,$00,$02,$FC,$0A,$00,$02,$FC,$0C,$00,$02
    .byte $FC,$0A,$00
    
    NEWSNDFREQ(sfreq_feject) ;,$29
    .byte $02,$60,$02,$04,$40,$FF,$02,$30,$FF,$38,$10,$00,$02
    .byte $00,$00,$02,$20,$00,$14,$10,$00

    NEWTUNE(s_feject)       
    TUNEFREQ(sfreq_feject)
    TUNEAMP(samp_feject)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY5X)
    NOTE(_REST,$58)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_fshot)   ;,$2a
    .byte $02,$30,$04,$00,$02,$04,$02,$00,$02,$04
    .byte $02,$00,$02,$04,$04,$FC,$02,$00,$02,$FC,$04,$00,$02,$FC,$06,$00
    .byte $02,$FC,$06,$00,$02,$FC,$04,$00,$02,$FC,$06,$00,$02,$FC,$FF,$03
    .byte $07,$0C,$00,$02,$FC,$0E,$00,$02,$FC,$0E,$00,$02,$FC,$0E,$00,$02
    .byte $FC
    
    NEWSNDFREQ(sfreq_fshot)  ;,$2a
    .byte $02,$20,$07
	.byte $0C,$E0,$FF
	.byte $02,$D0,$F9
	.byte $7E,$40,$00
	.byte $02,$10,$F0		;second batch
    .byte $02,$00,$00

    NEWTUNE(s_fshot)    
    TUNEFREQ(sfreq_fshot)
    TUNEAMP(samp_fshot)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$0C)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$03)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$27)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$4D)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$13)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_blowspin)    ;,$2e
    .byte $04,$00,$02,$10,$02,$14,$02,$18,$3E
    .byte $00,$1C,$FC
    
    NEWSNDFREQ(sfreq_blowspin)   ;,$2e
    .byte $02,$30,$06,$2E,$F0,$FF,$32,$10,$00
    
    NEWTUNE(s_blowspin) 
    SETDCTL($40)
    TUNEFREQ(sfreq_blowspin)
    TUNEAMP(samp_blowspin)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$62)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_mazehit) ;,$2f
    .byte $12,$04,$06,$08
    
    NEWSNDFREQ(sfreq_mazehit)    ;,$2f
    .byte $02,$30,$00,$16,$00,$00
    
    NEWTUNE(s_mazehit)  
    TUNEFREQ(sfreq_mazehit)
    TUNEAMP(samp_mazehit)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$18)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_exp7)    ;,$30
    .byte $02,$3C
	.byte $0E,$00
	.byte $02,$F4
	.byte $06,$F8
	.byte $02,$F4
	.byte $02,$F8
    .byte $02,$FC
    
    NEWSNDFREQ(sfreq_exp7)   ;,$30
    .byte $02,$A0,$08
	.byte $02,$00,$00
	.byte $02,$20,$02
	.byte $02,$00,$00
	.byte $02,$50,$02		;second batch?
	.byte $02,$00,$00
	.byte $02,$B0,$F4
	.byte $10,$00,$00
    
    NEWTUNE(s_exp7) 
    TUNEFREQ(sfreq_exp7)
    TUNEAMP(samp_exp7)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$0C)		;This is even, which means we need to jump to postition $0c in the FREQ table
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$13)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_lshot)   ;,$31
    .byte $02,$3C,$02,$00
    .byte $02,$F8,$02,$00,$02,$F8,$02,$00,$02,$F8,$02,$00,$02,$F0,$02,$00
    .byte $02,$04,$12,$00,$02,$04,$06,$00,$08,$04,$04,$08
    
    NEWSNDFREQ(sfreq_lshot)  ;,$31
    .byte $02,$60,$02,$02
    .byte $00,$00,$02,$40,$FF,$02,$00,$00,$02,$40,$FF,$02,$00,$00,$02,$30
    .byte $FF,$1C,$00,$00,$02,$10,$00,$08,$00,$00,$02,$10,$00,$06,$00,$00
        
    NEWTUNE(s_lshot)    
    TUNEFREQ(sfreq_lshot)
    TUNEAMP(samp_lshot)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$10)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$2D)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_hitshield)   ;,$32
    .byte $02,$04,$02,$00,$02,$04,$02,$08,$06,$0C,$02,$CC,$18,$00
    
    NEWSNDFREQ(sfreq_hitshield)  ;,$32
    .byte $02,$80,$00
	.byte $1C,$00,$00
	.byte $02,$80,$FF
	.byte $08,$00,$00
    
    NEWTUNE(s_hitshield)    
    TUNEFREQ(sfreq_hitshield)
    TUNEAMP(samp_hitshield)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$12)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$17)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_noshield)    ;,$33
    .byte $02,$18,$04,$00,$02,$04,$04,$00,$02,$04,$02,$00,$04,$04,$02,$08,$02,$0C
    
    NEWSNDFREQ(sfreq_noshield)   ;,$33
    .byte $02,$20,$04,$02,$00,$00,$02,$F0,$FF,$02,$00,$00,$06,$F0,$FF,$04
    .byte $E0,$FF,$02,$C0,$FF,$02,$A0,$FF,$02,$80,$FF
    
    NEWTUNE(s_noshield) 
    TUNEFREQ(sfreq_noshield)
    TUNEAMP(samp_noshield)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$18)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_manhit)  ;,$34
    .byte $02,$14,$08,$00,$02,$04,$04
    .byte $00,$02,$04,$02,$00,$FF,$02,$07,$08,$04,$02,$08,$0A,$00,$02,$C4
    
    NEWSNDFREQ(sfreq_manhit) ;,$34
    .byte $02,$30,$00,$30,$00,$00
    
    NEWTUNE(s_manhit)   
    TUNEFREQ(sfreq_manhit)
    TUNEAMP(samp_manhit)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$32)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_mantrip) ;,$35
    .byte $02,$20,$04,$00,$02,$F8,$02,$FC,$06,$0C,$02,$04
    .byte $06,$00,$02,$FC,$02,$00,$FF,$03,$07,$0E,$F8,$02,$FC,$10,$00
    
    NEWSNDFREQ(sfreq_mantrip)    ;,$35
    .byte $02,$50,$01,$04,$E0,$FF,$02,$F0,$FF,$02,$30,$03,$02,$20,$00,$02,$10
    .byte $00,$02,$00,$00,$02,$E0,$FF,$04,$D0,$FF,$02,$E0,$FF,$02,$F0,$FF
    .byte $02,$E0,$FF,$02,$00,$00,$02,$D0,$FF,$02,$F0,$FF,$02,$E0,$FF,$02
    .byte $00,$00,$04,$F0,$FF,$04,$00,$00,$02,$F0,$FF,$04,$00,$00,$02,$F0
    .byte $FF,$08,$00,$00,$02,$10,$00,$06,$00,$00
    
    NEWTUNE(s_mantrip)  
    SETDCTL($01)
    TUNEFREQ(sfreq_mantrip)
    TUNEAMP(samp_mantrip)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY2)
    NOTE(_REST,$46)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_spikes)  ;,$38
    .byte $02,$04,$02,$00,$02,$04,$02,$08,$28,$00,$02,$FC,$30,$00,$02,$FC,$2E,$00
    .byte $02,$FC,$26,$00
    
    NEWSNDFREQ(sfreq_spikes) ;,$38
    .byte $02,$F0,$0F,$B8,$00,$00
    
    NEWTUNE(s_spikes)   
    SETDCTL($C1)
    TUNEFREQ(sfreq_spikes)
    TUNEAMP(samp_spikes)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$BA)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_spikes2) ;,$39
    .byte $02,$04,$02,$00,$02,$04
    .byte $08,$08,$02,$14,$12,$00,$02,$FC,$08,$00,$02,$FC,$06,$00,$02,$FC
    .byte $06,$00,$02,$FC,$06,$00,$02,$FC,$08,$00,$02,$FC,$06,$00,$02,$FC
    .byte $08,$00,$02,$FC,$06,$00,$02,$FC,$08,$00,$02,$FC,$08,$00,$02,$FC
    .byte $0E,$00,$02,$FC,$12,$00,$02,$FC,$14,$00,$02,$FC,$02,$00
    
    NEWSNDFREQ(sfreq_spikes2)    ;,$39
    .byte $02,$D0,$0F,$B8,$00,$00
    
    NEWTUNE(s_spikes2)  
    SETDCTL($C1)
    TUNEFREQ(sfreq_spikes2)
    TUNEAMP(samp_spikes2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$BA)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_spikes3) ;,$3a
    .byte $02,$3C,$04,$00,$02,$F4,$06,$F8,$02,$F4,$02,$F8
    
    NEWSNDFREQ(sfreq_spikes3)    ;,$3a
    .byte $02,$70,$00
	.byte $10,$00,$00
    
    NEWTUNE(s_spikes3)  
    SETDCTL($C1)
    TUNEFREQ(sfreq_spikes3)
    TUNEAMP(samp_spikes3)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$12)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_key) ;,$3b
    .byte $02,$30,$02,$FC,$02,$F4,$02,$18,$02,$F0
    .byte $02,$0C,$02,$00,$04,$F8,$02,$08,$02,$F8,$02,$F4,$02,$F8,$02,$0C
    .byte $02,$FC,$02,$08,$02,$04,$02,$08,$02,$F4,$02,$14,$02,$08,$02,$F8
    .byte $02,$04,$02,$EC,$02,$FC,$02,$08,$02,$F8,$02,$04,$02,$F4,$02,$0C
    .byte $02,$F0,$02,$08,$02,$04,$02,$EC,$02,$10,$02,$F0,$02,$0C,$02,$F0
    .byte $02,$08,$02,$FC,$02,$08,$02,$F4,$04,$04,$04,$FC,$02,$08,$08,$FC
    .byte $08,$00
    
    NEWSNDFREQ(sfreq_key)    ;,$3b
    .byte $02,$50,$03,$6C,$00,$00
    
    NEWTUNE(s_key)  
    SETDCTL($40)
    TUNEFREQ(sfreq_key)
    TUNEAMP(samp_key)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$6E)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_door)    ;,$3c
    .byte $02,$3C,$04,$00,$02,$FC,$04,$00
    .byte $02,$FC,$02,$00,$04,$FC,$06,$00,$04,$FC,$04,$00,$06,$FC,$08,$00
    .byte $02,$FC,$FF,$05,$07
    
    NEWSNDFREQ(sfreq_door)   ;,$3c
    .byte $02,$70,$00,$62,$00,$00
    
    NEWTUNE(s_door) 
    SETDCTL($01)
    TUNEFREQ(sfreq_door)
    TUNEAMP(samp_door)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$06)
    SETDIST(CHCTL_POLY5)
    NOTE(_REST,$07)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$57)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_handoff) ;,$41
	.byte $02,$3C
	.byte $04,$00
	.byte $08,$00
	.byte $02,$6C
	.byte $FF,$0A
	.byte $07,$02
	.byte $EC,$02
	.byte $10,$0C
	.byte $00,$02
	.byte $F0,$02
	.byte $10,$06
	.byte $00,$02
	.byte $F0,$02
	.byte $0C,$08
	.byte $00,$02
	.byte $F4,$02
	.byte $0C,$06,
	.byte $00,$02
	.byte $FC,$04,$00,$02,$F8,$02,$08,$04,$00,$02,$F8,$02,$04,$02,$FC,$FF,$04
    .byte $07
    
    NEWSNDFREQ(sfreq_handoff)    ;,$41  
    .byte $02,$00,$0C
	.byte $02,$00,$00
	.byte $02,$10,$00
	.byte $FF,$1B,$09
	.byte $02,$00,$00
	.byte $02,$10,$00
	.byte $FF,$06,$09
	.byte $02,$00,$00
	.byte $02,$20,$00
	.byte $FF,$0B,$09
	.byte $02,$00,$00
	.byte $02,$30,$F0
    
    NEWTUNE(s_handoff)  
    SETDCTL($01)
    TUNEFREQ(sfreq_handoff)
    TUNEAMP(samp_handoff)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$C8)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_handon)  ;,$42
    .byte $02,$04,$02,$FC,$FF,$04,$07,$02,$08,$04,$00
    .byte $02,$F8,$02,$08,$04,$00,$02,$F8,$02,$08,$02,$00,$02,$04,$08,$00
    .byte $02,$F4,$02,$0C,$06,$00,$02,$04,$06,$00,$02,$F0,$02,$10,$08,$00
    .byte $02,$04,$09,$00,$FF,$10,$07
    
    NEWSNDFREQ(sfreq_handon) ;,$42
    .byte $02,$D0,$0F,$02,$00,$00,$02,$E0,$FF
    .byte $FF,$0B,$09,$02,$00,$00,$02,$F0,$FF,$FF,$24,$09,$04,$00,$00
    
    NEWTUNE(s_handon)   
    SETDCTL($01)
    TUNEFREQ(sfreq_handon)
    TUNEAMP(samp_handon)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$CA)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_trans)   ;,$45
    .byte $02,$04,$04,$00,$02,$04,$02,$00,$02,$04,$02,$00,$04
    .byte $04,$04,$08,$04,$0C,$48,$00,$1C,$FC
    
    NEWSNDFREQ(sfreq_trans)  ;,$45
    .byte $02,$30,$00,$7C,$00,$00
    
    NEWTUNE(s_trans)    
    SETDCTL($64)
    TUNEFREQ(sfreq_trans)
    TUNEAMP(samp_trans)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$7E)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_trans2)  ;,$46
    .byte $04,$00
    
    NEWSNDFREQ(sfreq_trans2) ;,$46
    .byte $02,$A0,$0F
    
    NEWTUNE(s_trans2)   
    ENDSND

    
;**************************************
    NEWSNDAMP(samp_cann)    ;,$47
    .byte $10,$00,$02,$38,$02,$C8,$2A,$00
    
    NEWSNDFREQ(sfreq_cann)   ;,$47
    .byte $3E,$00,$00

    NEWTUNE(s_cann) 
    TUNEFREQ(sfreq_cann)
    TUNEAMP(samp_cann)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$3C)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_cann2)   ;,$48
    .byte $1E,$04,$02,$00,$1C,$FC
    
    NEWSNDFREQ(sfreq_cann2)  ;,$48
    .byte $02,$A0,$00,$38,$00,$00,$02,$60,$FF
    
    NEWTUNE(s_cann2)    
    TUNEFREQ(sfreq_cann2)
    TUNEAMP(samp_cann2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$3C)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_cann3)   ;,$49
    .byte $10,$00
	.byte $02,$3C
    .byte $02,$00
	.byte $02,$C4
	.byte $02,$00
	.byte $02,$3C
	.byte $06,$00
	.byte $0E,$F8
	.byte $02,$FC
	.byte $0E,$00
    
    NEWSNDFREQ(sfreq_cann3)  ;,$49
    .byte $02,$30,$00
	.byte $3A,$00,$00
    
    NEWTUNE(s_cann3)    
    TUNEFREQ(sfreq_cann3)
    TUNEAMP(samp_cann3)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$3C)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_cann4)   ;,$4a
    .byte $10,$00
	.byte $02,$3C
	.byte $1E,$FC
	.byte $0E,$00
    
    NEWSNDFREQ(sfreq_cann4)  ;,$4a
    .byte $12,$00,$00,$28
    .byte $10,$00,$04,$00,$00
    
    NEWTUNE(s_cann4)    
    TUNEFREQ(sfreq_cann4)
    TUNEAMP(samp_cann4)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$3C)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_ssplash) ;,$4b
    .byte $02,$3C
	.byte $06,$00
	.byte $1E,$FC
	.byte $0C,$00
    
    NEWSNDFREQ(sfreq_ssplash)    ;,$4b
    .byte $02,$30,$00,$2A,$00
    .byte $00,$02,$70,$00,$04,$00,$00
    
    NEWTUNE(s_ssplash)  
    TUNEFREQ(sfreq_ssplash)
    TUNEAMP(samp_ssplash)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$2C)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$05)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_rblow)   ;,$4c
    .byte $02,$3C,$06,$00
    .byte $02,$C4,$06,$00
    .byte $02,$3C,$04,$FC
    .byte $02,$00,$02,$FC
    .byte $04,$00,$02,$FC
    .byte $06,$00,$02,$FC
    .byte $08,$00,$02,$FC
    .byte $0A,$00,$02,$FC
    .byte $0E,$00,$02,$FC
    .byte $10,$00,$02,$FC
    .byte $12,$00,$02,$FC
    .byte $14,$00,$02,$FC
    .byte $16,$00,$02,$FC
    .byte $18,$00,$02,$FC
    .byte $1A,$00,$02,$FC
    .byte $10,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_rblow)  ;,$4c
    .byte $02,$C0,$03,$E8
    .byte $00,$00
    
    NEWTUNE(s_rblow)    
    TUNEFREQ(sfreq_rblow)
    TUNEAMP(samp_rblow)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$EA)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_rblow2)  ;,$4d
    .byte $02,$3C,$06
    .byte $00,$02,$C4,$06,$00,$02,$3C,$04,$FC,$02,$00,$02,$FC,$04,$00,$02
    .byte $FC,$06,$00,$02,$FC,$08,$00,$02,$FC,$0A,$00,$02,$FC,$0E,$00,$02
    .byte $FC,$10,$00,$02,$FC,$12,$00,$02,$FC,$14,$00,$02,$FC,$16,$00,$02
    .byte $FC,$18,$00,$02,$FC,$1A,$00,$02,$FC,$16,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_rblow2) ;,$4d
    .byte $02,$80,$01
    .byte $04,$F0,$FF,$08,$10,$00,$08,$F0,$FF,$FF,$0D,$09,$08,$10,$00,$02
    .byte $60,$FE
    
    NEWTUNE(s_rblow2)   
    TUNEFREQ(sfreq_rblow2)
    TUNEAMP(samp_rblow2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$F0)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_rblow3)  ;,$4e
    .byte $02,$3C,$02,$00,$02,$C4,$08,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC
    .byte $FF,$02,$07,$04,$00,$02,$FC,$FF,$02,$07,$06,$00,$02,$FC,$FF,$02
    .byte $07,$08,$00,$02,$FC,$08,$00,$02,$FC,$0A,$00,$02,$FC
    
    NEWSNDFREQ(sfreq_rblow3) ;,$4e
    .byte $02,$30,$02,$0A,$F0,$FF,$02,$D0,$FE,$5E,$10,$00
    
    NEWTUNE(s_rblow3)   
    TUNEFREQ(sfreq_rblow3)
    TUNEAMP(samp_rblow3)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY5)
    NOTE(_REST,$32)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$03)
    SETDIST(CHCTL_POLY5)
    NOTE(_REST,$39)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_escfall) ;,$52
    .byte $02,$08
	.byte $FE,$00
	.byte $FE,$00
	.byte $FE,$00
	.byte $FE,$00
    
    NEWSNDFREQ(sfreq_escfall)    ;,$52
    .byte $3E,$00,$00
	.byte $02,$10,$00
	.byte $FF,$0E,$09		;This is a jump value... 
							;	$FF - indicates jump
							;   $0E - indicates counter of times to loop
							;   $09 - is number of bytes to subtract from frequency PC 
	.byte $3A,$00,$00
    
    NEWTUNE(s_escfall)  
    TUNEFREQ(sfreq_escfall)
    TUNEAMP(samp_escfall)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$02)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$FF)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$FF)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$FF)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$FF)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_mazekill)    ;,$55
    .byte $04,$00,$02
    .byte $04,$02,$00,$02,$04,$04,$00,$02,$04,$06,$00,$02,$04,$08,$00,$FF
    .byte $08,$07,$02,$08,$0A,$00,$02,$04,$28,$00,$10,$FC
    
    NEWSNDFREQ(sfreq_mazekill)   ;,$55
    .byte $02,$50,$08,$02
    .byte $00,$00,$02,$30,$FE,$02,$00,$00,$02,$F0,$FE,$02,$00,$00,$02,$30
    .byte $02,$02,$50,$FF,$02,$D0,$FE,$02,$90,$00,$02,$00,$00,$02,$C0,$FE
    .byte $02,$60,$02,$02,$10,$FF,$02,$00,$00,$02,$50,$FF,$02,$20,$01,$02
    .byte $90,$FE,$02,$00,$00,$02,$80,$00,$02,$00,$00,$02,$C0,$FE,$02,$00
    .byte $00,$02,$90,$02,$02,$40,$FE,$02,$B0,$00,$02,$30,$FE,$02,$00,$00
    .byte $02,$B0,$00,$02,$00,$00,$02,$C0,$FE,$02,$00,$00,$02,$F0,$02,$02
    .byte $50,$FD,$02,$C0,$00,$02,$00,$00,$02,$10,$01,$02,$10,$FD,$02,$D0
    .byte $01,$02,$A0,$FE,$02,$A0,$00,$02,$00,$00,$02,$20,$01,$02,$A0,$FD
    .byte $02,$00,$00,$02,$E0,$00,$02,$F0,$00,$02,$50,$FE,$02,$00,$00,$02
    .byte $E0,$00,$02,$00,$00,$02,$70,$FE,$02,$70,$FF,$02,$00,$01,$02,$00
    .byte $FF,$02,$90,$01,$02,$00,$FE,$02,$80,$00,$02,$50,$FF,$02,$00,$00
    .byte $02,$30,$00,$02,$80,$00,$02,$00,$00,$04,$80,$FF,$02,$50,$00,$02
    .byte $10,$00,$02,$B0,$FF,$02,$70,$00,$02,$80,$FF,$02,$00,$00,$02,$A0
    .byte $00,$02,$00,$00,$02,$D0,$FE,$02,$00,$00,$02,$D0,$00,$02,$60,$FF
    .byte $02,$40,$00,$02,$C0,$FF,$02,$D0,$FF,$02,$30,$00,$02,$F0,$FF,$02
    .byte $40,$00,$02,$70,$FF,$02,$00,$00,$02,$A0,$00,$02,$00,$00,$02,$60
    .byte $FF,$02,$00,$00,$02,$60,$00
    
    NEWTUNE(s_mazekill) 
    SETDCTL($80)
    TUNEFREQ(sfreq_mazekill)
    TUNEAMP(samp_mazekill)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
    NOTE(_REST,$02)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$B3)
    ENDSND
    
;**************************************
    NEWSNDAMP(samp_mazekill2)   ;,$56
    .byte $04,$00,$02,$04,$02
    .byte $00,$02,$04,$04,$00,$02,$04,$06,$00,$02,$04,$08,$00,$FF,$08,$07
    .byte $02,$08,$0A,$00,$02,$04,$28,$00,$10,$FC
    
    NEWSNDFREQ(sfreq_mazekill2)  ;,$56
    .byte $02,$50,$08,$02,$00,$00
    .byte $02,$30,$FE,$02,$00,$00,$02,$F0,$FE,$02,$00,$00,$02,$30,$02,$02
    .byte $50,$FF,$02,$D0,$FE,$02,$90,$00,$02,$00,$00,$02,$C0,$FE,$02,$60
    .byte $02,$02,$10,$FF,$02,$00,$00,$02,$50,$FF,$02,$20,$01,$02,$90,$FE
    .byte $02,$00,$00,$02,$80,$00,$02,$00,$00,$02,$C0,$FE,$02,$00,$00,$02
    .byte $90,$02,$02,$40,$FE,$02,$B0,$00,$02,$30,$FE,$02,$00,$00,$02,$B0
    .byte $00,$02,$00,$00,$02,$C0,$FE,$02,$00,$00,$02,$F0,$02,$02,$50,$FD
    .byte $02,$C0,$00,$02,$00,$00,$02,$10,$01,$02,$10,$FD,$02,$D0,$01,$02
    .byte $A0,$FE,$02,$A0,$00,$02,$00,$00,$02,$20,$01,$02,$A0,$FD,$02,$00
    .byte $00,$02,$E0,$00,$02,$F0,$00,$02,$50,$FE,$02,$00,$00,$02,$E0,$00
    .byte $02,$00,$00,$02,$70,$FE,$02,$70,$FF,$02,$00,$01,$02,$00,$FF,$02
    .byte $90,$01,$02,$00,$FE,$02,$80,$00,$02,$50,$FF,$02,$00,$00,$02,$30
    .byte $00,$02,$80,$00,$02,$00,$00,$04,$80,$FF,$02,$50,$00,$02,$10,$00
    .byte $02,$B0,$FF,$02,$70,$00,$02,$80,$FF,$02,$00,$00,$02,$A0,$00,$02
    .byte $00,$00,$02,$D0,$FE,$02,$00,$00,$02,$D0,$00,$02,$60,$FF,$02,$40
    .byte $00,$02,$C0,$FF,$02,$D0,$FF,$02,$30,$00,$02,$F0,$FF,$02,$40,$00
    .byte $02,$70,$FF,$02,$00,$00,$02,$A0,$00,$02,$00,$00,$02,$60,$FF,$02
    .byte $00,$00,$02,$60,$00
    
    NEWTUNE(s_mazekill2)    
    SETDCTL($80)
    TUNEFREQ(sfreq_mazekill2)
    TUNEAMP(samp_mazekill2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$B4)
    ENDSND


;***************************************************************************
;* Music Tables
;***************************************************************************        
RATE_LARGO 		= 52d	;$34
RATE_DEFAULT	= 64d	;$40
RATE_MODERATO 	= 100d	;$64
RATE_ALLEGRO 	= 145d	;$91
RATE_VIVACE 	= 157d	;$9d
RATE_PRESTO 	= 177d	;$B1

;************************************
;* Start Sound 
;************************************
	;High Stacatto
    NEWTUNE(s_start)    
    SETBVOL($02)
    SETNRATE(RATE_VIVACE)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($09)
		NOTE(_B5,$20)
		NOTE(_Gb5,$20)
		NOTE(_B4,$20)
		NOTE(_Gb5,$20)
		NOTE(_B5,$20)
		NOTE(_Gb5,$20)
    SLOOPEND
    NOTE(_B5,$40)
    NOTE(_REST,$80)
    ENDSND
        
	;Harmony High
    NEWTUNE(s_start2)   
    SETBVOL($03)
    SETNRATE(RATE_VIVACE)
    TUNEAMP(samp_hardswell)
    NOTE(_Eb4,$C0)
    NOTE(_Eb4,$C1)
    NOTE(_Db4,$C0)
    NOTE(_Db4,$C1)
    NOTE(_B3,$C0)
    NOTE(_B3,$C1)
    NOTE(_Db4,$C0)
    NOTE(_Db4,$41)
    NOTE(_REST,$60)
    NOTE(_Eb4,$20)
    NOTE(_Eb4,$C0)
    NOTE(_Eb4,$41)
    NOTE(_REST,$80)
    ENDSND
        
	;Harmony Mid
    NEWTUNE(s_start3)   
    SETBVOL($03)
    SETNRATE(RATE_VIVACE)
    TUNEAMP(samp_hardswell)
    NOTE(_Gb3,$C0)
    NOTE(_Gb3,$C1)
    NOTE(_E3,$C0)
    NOTE(_E3,$C1)
    NOTE(_D3,$C0)
    NOTE(_D3,$C1)
    NOTE(_E3,$C0)
    NOTE(_E3,$41)
    NOTE(_REST,$60)
    NOTE(_Gb3,$20)
    NOTE(_Gb3,$C0)
    NOTE(_Gb3,$41)
    NOTE(_REST,$80)
    ENDSND
     
	;Harmony Low
    NEWTUNE(s_start4)   
    SETBVOL($03)
    SETNRATE(RATE_VIVACE)
    TUNEAMP(samp_hardswell)
    NOTE(_B2,$C0)
    NOTE(_B2,$C1)
    NOTE(_A2,$C0)
    NOTE(_A2,$C1)
    NOTE(_G3,$C0)
    NOTE(_G3,$C1)
    NOTE(_A2,$C0)
    NOTE(_A2,$41)
    NOTE(_REST,$60)
    NOTE(_B2,$20)
    NOTE(_B2,$C0)
    NOTE(_B2,$41)
    NOTE(_REST,$80)
    ENDSND
        
	;High Beeps
    NEWTUNE(s_start5)   
    NOTE(_REST,$10)
    NOTE(_B5,$10)
    NOTE(_B5,$10)
    NOTE(_B5,$10)
    ADDBVOL(-1)
    NOTE(_B5,$10)
    NOTE(_B5,$10)
    NOTE(_B5,$10)
    NOTE(_B5,$10)
    ADDBVOL(1)
    ENDSND

;************************************	
;* Enter Maze Music - High Monotone
;************************************
    NEWTUNE(s_mystery)  
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($02)
		NOTE(_REST,$80)
		SJSR(s_start5)
    SLOOPEND
    NOTE(_REST,$80)
    NOTE(_REST,$80)
    SJSR(s_start5)
    NOTE(_B5,$40)
    NOTE(_REST,$40)
    ENDSND

;* Enter Maze Music - Harmony High
    NEWTUNE(s_mystery2) 
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($02)
		NOTE(_G4,$80)
		NOTE(_Gb4,$40)
		NOTE(_REST,$40)
    SLOOPEND
    NOTE(_F4,$80)
    NOTE(_E4,$80)
    NOTE(_Gb4,$C0)
    NOTE(_REST,$40)
    ENDSND

;* Enter Maze Music - Harmony Low       
    NEWTUNE(s_mystery3) 
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($02)
		NOTE(_E4,$80)
		NOTE(_Eb4,$40)
		NOTE(_REST,$40)
    SLOOPEND
    NOTE(_D4,$80)
    NOTE(_Db4,$80)
    NOTE(_Eb4,$C0)
    NOTE(_REST,$40)
    ENDSND

;* Enter Maze Music - Bass Line     
    NEWTUNE(s_mystery4) 
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($07)
		NOTE(_B1,$10)
		NOTE(_REST,$10)
		NOTE(_B2,$10)
		NOTE(_REST,$10)
		NOTE(_Bb2,$10)
		NOTE(_REST,$10)
		NOTE(_Bb1,$10)
		NOTE(_REST,$10)
    SLOOPEND
    NOTE(_B1,$40)
    NOTE(_REST,$40)
    ENDSND
 
;************************************ 
;* Finish Breakout Music
;************************************
    NEWTUNE(s_breakout) 
    SETBVOL($02)
    SETNRATE(RATE_PRESTO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($02)
		NOTE(_D5,$C0)
		NOTE(_Db5,$60)
		NOTE(_REST,$40)
		NOTE(_D5,$20)
    SLOOPEND
    NOTE(_D5,$60)
    NOTE(_REST,$40)
    NOTE(_Db5,$20)
    NOTE(_Db5,$60)
    NOTE(_REST,$40)
    NOTE(_D5,$20)
    NOTE(_G4,$C0)
    NOTE(_G4,$C1)
    NOTE(_B5,$C0)
    ENDSND
        
    NEWTUNE(s_breakout2)    
    SETBVOL($02)
    SETNRATE(RATE_PRESTO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($02)
		NOTE(_B4,$C0)
		NOTE(_Bb4,$60)
		NOTE(_REST,$40)
		NOTE(_Bb4,$20)
    SLOOPEND
    NOTE(_B4,$60)
    NOTE(_REST,$40)
    NOTE(_Bb4,$20)
    NOTE(_Bb4,$60)
    NOTE(_REST,$40)
    NOTE(_Bb4,$20)
    NOTE(_B4,$C0)
    NOTE(_B4,$C1)
    NOTE(_D5,$C0)
    ENDSND
        
    NEWTUNE(s_breakout3)    
    SETBVOL($02)
    SETNRATE(RATE_PRESTO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($02)
		NOTE(_G5,$C0)
		NOTE(_G5,$60)
		NOTE(_REST,$40)
		NOTE(_G5,$20)
    SLOOPEND
    NOTE(_G5,$60)
    NOTE(_REST,$40)
    NOTE(_G5,$20)
    NOTE(_G5,$60)
    NOTE(_REST,$40)
    NOTE(_G5,$20)
    NOTE(_G5,$C0)
    NOTE(_G5,$C1)
    NOTE($53,$C0)
    ENDSND
        
    NEWTUNE(s_breakout4)    
    SETBVOL($02)
    SETNRATE(RATE_PRESTO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($03)
		NOTE(_B3,$20)
		NOTE(_G3,$20)
		NOTE(_B3,$20)
		NOTE(_D4,$20)
		NOTE(_B3,$20)
		NOTE(_G3,$20)
		NOTE(_Bb3,$20)
		NOTE(_G3,$20)
		NOTE(_Bb3,$20)
		NOTE(_Db4,$20)
		NOTE(_Bb3,$20)
		NOTE(_G3,$20)
    SLOOPEND
    NOTE(_G3,$20)
    NOTE(_G2,$20)
    NOTE(_A2,$20)
    NOTE(_B2,$20)
    NOTE(_C3,$20)
    NOTE(_Db3,$20)
    NOTE(_D3,$20)
    NOTE(_Db3,$20)
    NOTE(_E3,$20)
    NOTE(_D3,$20)
    NOTE(_E3,$20)
    NOTE(_Gb3,$20)
    NOTE(_G3,$C0)
    ENDSND

;************************************
;* Maze Escape Music
;************************************
    NEWTUNE(s_escape)   
    SETBVOL($02)
    TUNEAMP(samp_hardswell)
    SETNRATE(RATE_MODERATO)
    SLOOPSTART($10)
		NOTE(_A5,$80)
		NOTE(_A5,$81)
		ADDBFREQ($01)
    SLOOPEND
    ENDSND
        
    NEWTUNE(s_escape2)  
    SETBVOL($02)
    TUNEAMP(samp_hardswell)
    SETNRATE(RATE_MODERATO)
    SLOOPSTART($10)
		NOTE(_REST,$60)
		NOTE(_REST,$10)
		NOTE(_D4,$10)
		NOTE(_D4,$40)
		NOTE(_A3,$40)
		ADDBFREQ($01)
    SLOOPEND
    ENDSND
        
    NEWTUNE(s_escape3)  
    SETBVOL($02)
    TUNEAMP(samp_hardswell)
    SETNRATE(RATE_MODERATO)
    SLOOPSTART($10)
		NOTE(_REST,$60)
		NOTE(_REST,$10)
		NOTE(_A3,$10)
		NOTE(_A3,$40)
		NOTE(_Gb3,$40)
		ADDBFREQ($01)
    SLOOPEND
    ENDSND
        
    NEWTUNE(s_escape4)  
    SETBVOL($02)
    TUNEAMP(samp_hardswell)
    SETNRATE(RATE_MODERATO)
    SLOOPSTART($10)
		NOTE(_A1,$10)
		NOTE(_E2,$10)
		NOTE(_A2,$10)
		NOTE(_E3,$10)
		NOTE(_Db3,$10)
		NOTE(_A2,$10)
		NOTE(_Db3,$10)
		NOTE(_D3,$10)
		NOTE(_E3,$10)
		NOTE(_D3,$10)
		NOTE(_B2,$10)
		NOTE(_A2,$10)
		NOTE(_E2,$10)
		NOTE(_D2,$10)
		NOTE(_Db2,$10)
		NOTE(_A1,$10)
		ADDBFREQ($01)
    SLOOPEND
    ENDSND

;************************************	
;*  Triumph Music (4 parts)
;*  s_triumph
;*  s_triumph2
;*  s_triumph3
;*  s_triumph4
;************************************
    NEWTUNE(s_triumph)   
    SETBVOL($02)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore)
    NOTE(_G5,$60)
    NOTE(_G5,$20)
    NOTE(_E5,$20)
    NOTE(_C5,$20)
    NOTE(_G4,$40)
    NOTE(_REST,$20)
    NOTE(_G4,$20)
    NOTE(_C4,$20)
    NOTE(_E4,$20)
    SLOOPSTART($05)
		NOTE(_G5,$10)
		NOTE(_A5,$10)
    SLOOPEND
    NOTE(_G5,$10)
    NOTE(_Gb5,$10)
    NOTE(_G5,$20)
    NOTE(_REST,$40)
    NOTE(_E5,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore)
    SLOOPSTART($02)
		NOTE(_A5,$60)
		NOTE(_A5,$20)
		NOTE(_G5,$20)
		NOTE(_F5,$20)
    SLOOPEND
    SLOOPSTART($05)
		NOTE(_A5,$10)
		NOTE(_B5,$10)
    SLOOPEND
    NOTE(_A5,$10)
    NOTE(_Ab5,$10)
    NOTE(_A5,$60)
    NOTE(_REST,$60)
    ENDSND
        
    NEWTUNE(s_triumph2)   
    SETBVOL($02)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore2)  
    SLOOPSTART($02)
		ADDBVOL(2)
		NOTE(_E4,$10)
		NOTE(_F4,$10)
		NOTE(_E4,$10)
		NOTE(_D4,$10)
		NOTE(_C4,$10)
		NOTE(_B3,$10)
		ADDBVOL(-2)
		NOTE(_C4,$20)
		NOTE(_REST,$40)
    SLOOPEND
    ADDBVOL(2)
    NOTE(_D4,$10)
    NOTE(_E4,$10)
    NOTE(_F4,$10)
    NOTE(_E4,$10)
    NOTE(_D4,$10)
    NOTE(_C4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_B3,$10)
    NOTE(_C4,$10)
    NOTE(_D4,$10)
    NOTE(_C4,$10)
    ADDBVOL(-2)
    NOTE(_D4,$20)
    NOTE(_REST,$40)
    NOTE(_D4,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore2)
    NOTE(_C5,$60)
    NOTE(_C5,$20)
    NOTE(_C5,$20)
    NOTE(_C5,$20)
    NOTE(_B4,$60)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_E4,$60)
    NOTE(_E4,$20)
    NOTE(_E4,$20)
    NOTE(_E4,$20)
    NOTE(_A4,$60)
    NOTE(_REST,$60)
    ENDSND
        
    NEWTUNE(s_triumph3)   
    SETBVOL($02)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore3)
    NOTE(_C5,$60)
    NOTE(_C5,$20)
    NOTE(_G4,$20)
    NOTE(_E4,$20)
    NOTE(_E4,$40)
    NOTE(_REST,$20)
    NOTE(_E4,$20)
    NOTE(_G4,$20)
    NOTE(_C5,$20)
    NOTE(_B4,$60)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_REST,$40)
    NOTE(_Ab4,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore3)
    ADDBVOL(2)
    NOTE(_C4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_F4,$10)
    NOTE(_A3,$10)
    NOTE(_C4,$10)
    ADDBVOL(-2)
    NOTE(_F4,$20)
    NOTE(_REST,$40)
    ADDBVOL(2)
    NOTE(_D4,$10)
    NOTE(_C4,$10)
    NOTE(_B3,$10)
    NOTE(_G3,$10)
    NOTE(_B3,$10)
    NOTE(_D4,$10)
    ADDBVOL(-2)
    NOTE(_F4,$20)
    NOTE(_REST,$40)
    ADDBVOL(2)
    NOTE(_Db4,$10)
    NOTE(_D4,$10)
    NOTE(_Db4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_B3,$10)
    ADDBVOL(-2)
    NOTE(_Db4,$20)
    NOTE(_Db4,$20)
    NOTE(_Db4,$20)
    NOTE(_Db4,$60)
    NOTE(_REST,$60)
    ENDSND
        
    NEWTUNE(s_triumph4)   
    SETBVOL($02)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore4)
    SLOOPSTART($03)
		NOTE(_G2,$60)
		NOTE(_G2,$20)
		NOTE(_G2,$20)
		NOTE(_G2,$20)
    SLOOPEND
    NOTE(_G2,$20)
    NOTE(_REST,$40)
    NOTE(_E2,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore4)
    SLOOPSTART($03)
		NOTE(_F2,$60)
		NOTE(_F2,$20)
		NOTE(_F2,$20)
		NOTE(_F2,$20)
		ADDBFREQ($02)
    SLOOPEND
    SETBFREQ($00)
    NOTE(_A2,$60)
    NOTE(_REST,$60)
    ENDSND


;************************************	
;*  Common Music for High Score and
;*  Triumph tunes. (4 parts)
;*  s_highscore
;*  s_highscore2
;*  s_highscore3
;*  s_highscore4
;************************************
    NEWTUNE(s_highscore)    
    NOTE(_Db5,$60)
    NOTE(_Db4,$20)
    NOTE(_E4,$20)
    NOTE(_A4,$20)
    NOTE(_Db5,$20)
    NOTE(_REST,$40)
    NOTE(_B4,$40)
    NOTE(_REST,$20)
    NOTE(_A4,$60)
    NOTE(_Gb4,$20)
    NOTE(_A4,$20)
    NOTE(_Db5,$20)
    NOTE(_Gb5,$20)
    NOTE(_REST,$40)
    NOTE(_A5,$40)
    NOTE(_REST,$20)
    ENDSND
        
    NEWTUNE(s_highscore2)   
    ADDBVOL(2)
    NOTE(_Db4,$10)
    NOTE(_D4,$10)
    NOTE(_Db4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_Ab3,$10)
    ADDBVOL(-2)
    NOTE(_A3,$20)
    NOTE(_Db4,$20)
    NOTE(_E4,$20)
    NOTE(_A4,$20)
    NOTE(_REST,$40)
    NOTE(_Ab4,$40)
    NOTE(_REST,$20)
    ADDBVOL(2)
    NOTE(_Db4,$10)
    NOTE(_D4,$10)
    NOTE(_Db4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_Ab3,$10)
    ADDBVOL(-2)
    NOTE(_A3,$20)
    NOTE(_Db4,$20)
    NOTE(_Gb4,$20)
    NOTE(_A4,$20)
    NOTE(_REST,$40)
    NOTE(_Eb4,$40)
    NOTE(_REST,$20)
    ENDSND
        
    NEWTUNE(s_highscore3)   
    NOTE(_E3,$60)
    NOTE(_E3,$20)
    NOTE(_E3,$20)
    NOTE(_E3,$20)
    NOTE(_E3,$20)
    NOTE(_REST,$40)
    NOTE(_F3,$40)
    NOTE(_REST,$20)
    NOTE(_Gb3,$60)
    NOTE(_Gb3,$20)
    NOTE(_Gb3,$20)
    NOTE(_Gb3,$20)
    NOTE(_Gb3,$20)
    NOTE(_REST,$40)
    NOTE(_Gb3,$40)
    NOTE(_REST,$20)
    ENDSND
        
    NEWTUNE(s_highscore4)   
    NOTE(_A2,$60)
    NOTE(_A2,$20)
    NOTE(_A2,$20)
    NOTE(_A2,$20)
    NOTE(_A2,$20)
    NOTE(_REST,$40)
    NOTE(_Ab2,$40)
    NOTE(_REST,$20)
    NOTE(_Gb2,$60)
    NOTE(_Gb2,$20)
    NOTE(_Gb2,$20)
    NOTE(_Gb2,$20)
    NOTE(_Gb2,$20)
    NOTE(_REST,$40)
    NOTE(_B1,$40)
    NOTE(_REST,$20)
    ENDSND
 
;************************************
;* High Score Music - 4 Parts
;*  s_highscore5
;*  s_highscore6
;*  s_highscore7
;*  s_highscore8
;************************************
    NEWTUNE(s_highscore5)   
    SETBVOL($02)
    SETNRATE(RATE_ALLEGRO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore)
    NOTE(_G5,$60)
    NOTE(_G5,$20)
    NOTE(_E5,$20)
    NOTE(_C5,$20)
    NOTE(_G4,$40)
    NOTE(_REST,$20)
    NOTE(_G4,$20)
    NOTE(_C4,$20)
    NOTE(_E4,$20)
    SLOOPSTART($05)
		NOTE(_G5,$10)
		NOTE(_A5,$10)
    SLOOPEND
    NOTE(_G5,$10)
    NOTE(_Gb5,$10)
    NOTE(_G5,$20)
    NOTE(_REST,$40)
    NOTE(_E5,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore)
    SLOOPSTART($02)
		NOTE(_A5,$60)
		NOTE(_A5,$20)
		NOTE(_G5,$20)
		NOTE(_F5,$20)
    SLOOPEND
    SLOOPSTART($05)
		NOTE(_A5,$10)
		NOTE(_B5,$10)
    SLOOPEND
    NOTE(_A5,$10)
    NOTE(_Ab5,$10)
    NOTE(_A5,$60)
    NOTE(_REST,$60)
    ENDSND
        
    NEWTUNE(s_highscore6)   
    SETBVOL($02)
    SETNRATE(RATE_ALLEGRO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore2)  
    SLOOPSTART($02)
		ADDBVOL(2)
		NOTE(_E4,$10)
		NOTE(_F4,$10)
		NOTE(_E4,$10)
		NOTE(_D4,$10)
		NOTE(_C4,$10)
		NOTE(_B3,$10)
		ADDBVOL(-2)
		NOTE(_C4,$20)
		NOTE(_REST,$40)
    SLOOPEND
    ADDBVOL(2)
    NOTE(_D4,$10)
    NOTE(_E4,$10)
    NOTE(_F4,$10)
    NOTE(_E4,$10)
    NOTE(_D4,$10)
    NOTE(_C4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_B3,$10)
    NOTE(_C4,$10)
    NOTE(_D4,$10)
    NOTE(_C4,$10)
    ADDBVOL(-2)
    NOTE(_D4,$20)
    NOTE(_REST,$40)
    NOTE(_D4,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore2)
    NOTE(_C5,$60)
    NOTE(_C5,$20)
    NOTE(_C5,$20)
    NOTE(_C5,$20)
    NOTE(_B4,$60)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_E4,$60)
    NOTE(_E4,$20)
    NOTE(_E4,$20)
    NOTE(_E4,$20)
    NOTE(_A4,$60)
    NOTE(_REST,$60)
    ENDSND
        
    NEWTUNE(s_highscore7)   
    SETBVOL($02)
    SETNRATE(RATE_ALLEGRO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore3)
    NOTE(_C5,$60)
    NOTE(_C5,$20)
    NOTE(_G4,$20)
    NOTE(_E4,$20)
    NOTE(_E4,$40)
    NOTE(_REST,$20)
    NOTE(_E4,$20)
    NOTE(_G4,$20)
    NOTE(_C5,$20)
    NOTE(_B4,$60)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_B4,$20)
    NOTE(_REST,$40)
    NOTE(_Ab4,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore3)
    ADDBVOL(2)
    NOTE(_C4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_F4,$10)
    NOTE(_A3,$10)
    NOTE(_C4,$10)
    ADDBVOL(-2)
    NOTE(_F4,$20)
    NOTE(_REST,$40)
    ADDBVOL(2)
    NOTE(_D4,$10)
    NOTE(_C4,$10)
    NOTE(_B3,$10)
    NOTE(_G3,$10)
    NOTE(_B3,$10)
    NOTE(_D4,$10)
    ADDBVOL(-2)
    NOTE(_F4,$20)
    NOTE(_REST,$40)
    ADDBVOL(2)
    NOTE(_Db4,$10)
    NOTE(_D4,$10)
    NOTE(_Db4,$10)
    NOTE(_B3,$10)
    NOTE(_A3,$10)
    NOTE(_B3,$10)
    ADDBVOL(-2)
    NOTE(_Db4,$20)
    NOTE(_Db4,$20)
    NOTE(_Db4,$20)
    NOTE(_Db4,$60)
    NOTE(_REST,$60)
    ENDSND
        
    NEWTUNE(s_highscore8)   
    SETBVOL($02)
    SETNRATE(RATE_ALLEGRO)
    TUNEAMP(samp_hardswell)
    SJSR(s_highscore4)
    SLOOPSTART($03)
		NOTE(_G2,$60)
		NOTE(_G2,$20)
		NOTE(_G2,$20)
		NOTE(_G2,$20)
    SLOOPEND
    NOTE(_G2,$20)
    NOTE(_REST,$40)
    NOTE(_E2,$40)
    NOTE(_REST,$20)
    SJSR(s_highscore4)
    SLOOPSTART($03)
		NOTE(_F2,$60)
		NOTE(_F2,$20)
		NOTE(_F2,$20)
		NOTE(_F2,$20)
		ADDBFREQ($02)
    SLOOPEND
    SETBFREQ($00)
    NOTE(_A2,$60)
    NOTE(_REST,$60)
    ENDSND

;* Homeworld Tension Entrance
    NEWTUNE(s_tense_1) 
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($07)
		NOTE(_B0,$18)
		NOTE(_REST,$18)
		NOTE(_B1,$18)
		NOTE(_REST,$18)
		NOTE(_A1,$18)
		NOTE(_REST,$18)
		NOTE(_Bb0,$18)
		NOTE(_REST,$18)
    SLOOPEND
    ENDSND

	NEWTUNE(s_tense_2) 
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($07)
		NOTE(_E2,$60)
		NOTE(_Eb2,$60)
    SLOOPEND
    ENDSND

	NEWTUNE(s_tense_3) 
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($07)
		NOTE(_G4,$60)
		NOTE(_Gb4,$60)
    SLOOPEND
    ENDSND
	
	NEWTUNE(s_tense_4) 
    SETBVOL($00)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($0E)
		NOTE(_REST,$18)
		NOTE(_Gb6,$0C)
		NOTE(_REST,$0C)
		NOTE(_Gb6,$0C)
		NOTE(_REST,$0C)
		NOTE(_Gb6,$10)
		NOTE(_REST,$08)
    SLOOPEND
    ENDSND
	
    ; NEWTUNE(s_newmusic_1) 
    ; SETBVOL($01)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
    ; SLOOPSTART($07)
		; ;NOTE(_E1,$D0)
		; ;NOTE(_F1,$D1)
		; ;NOTE(_E1,$20)
		; ;NOTE(_Eb1,$20)
		; ;NOTE(_Ab1,$20)
		; ;NOTE(_G1,$40)
		; ;NOTE(_REST,$60)
		
		; ;NOTE(_E1,$50)
		; ;NOTE(_REST,$30)
		; ;NOTE(_E1,$20)
		; ;NOTE(_Eb1,$20)
		; ;NOTE(_Ab1,$20)
		; ;NOTE(_G1,$20)
		; ;NOTE(_B1,$20)
		; ;NOTE(_Bb1,$40)
		; ;NOTE(_REST,$10)
		
		; NOTE(_E0,$40)
		; ;NOTE(_REST,$30)
		; NOTE(_REST,$D0)
		; ;NOTE(_E0,$20)
		; ;NOTE(_Eb0,$20)
		; ;NOTE(_Ab1,$20)
		; ;NOTE(_G1,$40)
		; ;NOTE(_REST,$10)
		
		; ;NOTE(_E0,$40)
		; ;NOTE(_REST,$30)
		; ;NOTE(_REST,$D0)
		; ;NOTE(_E0,$20)
		; ;NOTE(_Eb0,$20)
		; ;NOTE(_B1,$20)
		; ;NOTE(_Bb1,$40)
			
		; ;SLOOPSTART(5)
	; ;		NOTE(_B1,$20)
	; ;		NOTE(_Bb1,$20)
	; ;		ADDBFREQ($02)
	; ;	SLOOPEND
		
		
		; ;NOTE(_Ab1,$20)
		; ;NOTE(_G1,$20)
		
		; ;NOTE(_B1,$20)
		; ;NOTE(_Bb1,$20)
		
		; ;NOTE(_C2,$20)
		; ;NOTE(_Bb1,$20)
		; ;ADDBFREQ($02)
    ; SLOOPEND
    ; ;NOTE(_B1,$40)
    ; NOTE(_REST,$40)
    ; ENDSND


; ;* Harmony High
    ; NEWTUNE(s_newmusic_2) 
    ; SETBVOL($01)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
    ; SLOOPSTART($07)
		; NOTE(_REST,$70)
		; NOTE(_E2,$20)
		; NOTE(_Eb2,$20)
		; NOTE(_Ab3,$20)
		; NOTE(_G3,$40)
    ; SLOOPEND
    ; ;NOTE(_F4,$80)
    ; ;NOTE(_E4,$80)
    ; ;NOTE(_Gb4,$C0)
    ; NOTE(_REST,$40)
    ; ENDSND

; ;* Harmony Low       
    ; NEWTUNE(s_newmusic_3) 
    ; SETBVOL($01)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
    ; SLOOPSTART($07)
		; NOTE(_REST,$70)
		; NOTE(_Db2,$20)
		; NOTE(_C2,$20)
		; NOTE(_F3,$20)
		; NOTE(_E3,$40)
    ; SLOOPEND
    ; ;NOTE(_D4,$80)
    ; ;NOTE(_Db4,$80)
    ; ;NOTE(_Eb4,$C0)
    ; NOTE(_REST,$40)
    ; ENDSND

    ; NEWTUNE(s_newmusic_4)    
    ; SETBVOL($00)
    ; SETNRATE(RATE_PRESTO)
    ; TUNEAMP(samp_hardswell)
    ; SLOOPSTART($40)
		; NOTE(_G7,$0C)
		; NOTE(_Gb7,$0C)
		; NOTE(_REST,$0C)
		; NOTE(_A7,$0C)
		; NOTE(_G7,$0C)
		; NOTE(_REST,$10)
		; NOTE(_A7,$0C)
		; NOTE(_Ab7,$0C)
		; NOTE(_REST,$0C)
		; NOTE(_F7,$0C)
		; NOTE(_A7,$0C)
		; NOTE(_Ab7,$0C)
		; NOTE(_REST,$0C)
		; NOTE(_F7,$0C)
    ; SLOOPEND
    ; ENDSND
	
	NEWTUNE(s_newmusic_5)    
    SETBVOL($00)
    SETNRATE(RATE_PRESTO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($40)
		NOTE(_REST,$0C)
		NOTE(_E6,$0C)
		NOTE(_REST,$12)
		NOTE(_F6,$0C)
		NOTE(_REST,$10)
		NOTE(_G6,$0C)
		NOTE(_REST,$10)
		NOTE(_E6,$0C)
		NOTE(_REST,$0C)
		NOTE(_Gb6,$0C)
		NOTE(_REST,$0C)
		NOTE(_E6,$0C)
    SLOOPEND
    ENDSND
	
    ; NEWTUNE(s_newmusic_4)  
    ; SETBVOL($01)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
    ; SLOOPSTART($02)
		; NOTE(_REST,$80)
		; SJSR(s_start5)
    ; SLOOPEND
    ; NOTE(_REST,$80)
    ; NOTE(_REST,$80)
    ; SJSR(s_start5)
    ; NOTE(_B5,$40)
    ; NOTE(_REST,$40)
    ; ENDSND

	
    NEWTUNE(s_spbonus_1) 
    SETBVOL($01)
    SETNRATE(RATE_LARGO)
    TUNEAMP(samp_hardswell)
    SLOOPSTART($07)
		NOTE(_B1,$10)
		NOTE(_REST,$10)
		NOTE(_B2,$10)
		NOTE(_REST,$10)
		NOTE(_Bb2,$10)
		NOTE(_REST,$10)
		NOTE(_Bb1,$10)
		NOTE(_REST,$10)
    SLOOPEND
    NOTE(_B1,$40)
    NOTE(_REST,$40)
    ENDSND

; ;****************************************************************
; ; Victory Music - Bass line

; ;High Beeps
    ; NEWTUNE(s_vic_high)   
    ; NOTE(_REST,$10)
    ; NOTE(_B5,$10)
    ; NOTE(_B5,$10)
    ; NOTE(_B5,$10)
    ; ADDBVOL(-1)
    ; NOTE(_B5,$10)
    ; NOTE(_B5,$10)
    ; NOTE(_B5,$10)
    ; NOTE(_B5,$10)
    ; ADDBVOL(1)
    ; ENDSND
	
; ;Common Bass Tune #1
	; NEWTUNE(s_vic_bcommon1)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	
	; NOTE(_F1,$80)
	; ;NOTE(_F1,$20)
	; ;NOTE(_F1,$40)
	; ;NOTE(_F1,$20)
	; NOTE(_REST,$80)
	; ;NOTE(_F1,$20)
	; ;NOTE(_F1,$40)
	; ;NOTE(_F1,$20)
	
	; NOTE(_F1,$80)
	; NOTE(_REST,$80)
	; ;NOTE(_F1,$20)
	; ;NOTE(_F1,$20)
	; ;NOTE(_F1,$20)
	; ;NOTE(_F1,$20)
	; ;NOTE(_F1,$20)
	; ;NOTE(_F1,$20)
	
	; NOTE(_G1,$80)
	; NOTE(_REST,$80)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	
	; NOTE(_G1,$80)
	; NOTE(_REST,$80)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ;NOTE(_G1,$20)
	; ENDSND
	
	; NEWTUNE(s_vic_bcommon2)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	; NOTE(_F1,$10)
	
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; NOTE(_G1,$10)
	; ENDSND
	
	; NEWTUNE(s_vic_pcommon1)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; SLOOPSTART(4)
		; NOTE(_A7,$01)
		; NOTE(_REST,$F)
		; NOTE(_A7,$01)
		; NOTE(_REST,$F)
		; NOTE(_A7,$01)
		; NOTE(_REST,$F)
	; SLOOPEND
	; ENDSND
	
	; NEWTUNE(s_vic_pcommon2)
	; SLOOPSTART(4)
		; NOTE(_A7,$10)
		; NOTE(_REST,$10)
		; NOTE(_A7,$01)
		; NOTE(_REST,$F)
		; NOTE(_A7,$01)
		; NOTE(_REST,$F)
	; SLOOPEND
	; ENDSND
	
	
	; NEWTUNE(s_vic_perc) 
    ; SETBVOL($06)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_d01)
	; SETDIST(CHCTL_POLY17_5)
	; SJSR(s_vic_pcommon1)
    ; SLOOPSTART($02)
		; SJSR(s_vic_pcommon2)
	; SLOOPEND
    ; ENDSND

    ; NEWTUNE(s_vic_b1) 
    ; SETBVOL($03)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
	; SETBFREQ(12)
	; SJSR(s_vic_bcommon1)
    ; SLOOPSTART($02)
		; SJSR(s_vic_bcommon2)
	; SLOOPEND
    ; ENDSND
	
	; NEWTUNE(s_vic_b2) 
    ; SETBVOL($03)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
	; SETBFREQ(4)						;4 steps higher - 3rd
	; SJSR(s_vic_bcommon1)
    ; SLOOPSTART($02)
		; SJSR(s_vic_bcommon2)
	; SLOOPEND
    ; ENDSND
	
	; NEWTUNE(s_vic_b3) 
    ; SETBVOL($03)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
	; SETBFREQ(7)						;7 steps higher - 5th
	; SJSR(s_vic_bcommon1)
    ; SLOOPSTART($08)
		; SJSR(s_vic_bcommon2)
	; SLOOPEND
    ; ENDSND
	
	; ;Main Tune - INTRO
	; NEWTUNE(s_vic_tcommon1)
	; ;First Measure
	; NOTE(_Eb3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_Eb3,$10)
	; ;Second Measure
	; NOTE(_F3,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_Ab3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_Eb3,$10)
	; ;Second Measure
	; NOTE(_F3,$30)
	; NOTE(_REST,$10)
	
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_Ab3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_Eb3,$10)
	; ;Third Measure
	; NOTE(_G3,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$28)
	; NOTE(_Ab3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_Eb3,$10)
	; ;Fourth Measure
	; NOTE(_Bb3,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_G3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_C3,$10)
	; ENDSND
	
	; ;Main Tune 
	; NEWTUNE(s_vic_tcommon2)
	; ;First Measure
	; NOTE(_F3,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$10)
	; NOTE(_Eb3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_Eb3,$10)
	; ;Second Measure
	; NOTE(_F3,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_Ab3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_Eb3,$10)
	; ;Third Measure
	; NOTE(_G3,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_Ab3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_Eb3,$10)
	; ;Fourth Measure
	; NOTE(_Bb3,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_G3,$10)
	; NOTE(_Ab3,$10)
	; NOTE(_G3,$10)
	; NOTE(_C3,$10)
	; ENDSND
	
	; ;Fill Tune - INTRO
	; NEWTUNE(s_vic_lead)   
    ; SETBVOL($03)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
	; SETDIST(CHCTL_NOPOLY)
	; ;First Measure
	; NOTE(_C3,$10)
	; NOTE(_F3,$10)
	; NOTE(_F3,$10)
	; NOTE(_C3,$10)
	; NOTE(_D3,$10)
	; ;Second Measure
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_REST,$20)
	; NOTE(_A4,$20)
	; NOTE(_A4,$20)
	; NOTE(_G4,$10)
	; NOTE(_F4,$10)
	; NOTE(_F4,$20)   ;here
	; NOTE(_E4,$20)	 ;here
	; ;Second Measure
	; NOTE(_REST,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_A4,$20)
	; NOTE(_A4,$20)
	; NOTE(_G4,$10)
	; NOTE(_F4,$10)
	; NOTE(_F4,$20)   ;here
	; NOTE(_E4,$20)	 ;here
	; ;Third Measure
	; NOTE(_REST,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_A4,$20)
	; NOTE(_A4,$20)
	; NOTE(_G4,$10)
	; NOTE(_F4,$10)
	; NOTE(_F4,$20)   ;here
	; NOTE(_E4,$20)	 ;here
	; ;Fourth Measure
	; NOTE(_REST,$30)
	; NOTE(_REST,$10)
	; NOTE(_REST,$20)
	; NOTE(_A4,$20)
	; NOTE(_A4,$20)
	; NOTE(_G4,$10)
	; NOTE(_F4,$10)
	; NOTE(_F4,$20)   ;here
	; NOTE(_E4,$20)	 ;here
	; ENDSND
	
	; ;Harmony Mid - Key Tone
    ; NEWTUNE(s_vic_mid)   
    ; SETBVOL($03)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
	; SETDIST(CHCTL_NOPOLY)
	; SJSR(s_vic_tcommon1)
	; SLOOPSTART($02)
		; SJSR(s_vic_tcommon2)
	; SLOOPEND
    ; ENDSND
	
	; ;Harmony Low - 3rd
    ; NEWTUNE(s_vic_mid1)   
    ; SETBVOL($03)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
	; SETBFREQ(7)	
	; SJSR(s_vic_tcommon1)
	; SLOOPSTART($02)
		; SJSR(s_vic_tcommon2)
	; SLOOPEND
    ; ENDSND
	

	; ;Harmony Low - 5th
    ; NEWTUNE(s_vic_mid2)   
    ; SETBVOL(-12)
    ; SETNRATE(RATE_DEFAULT)
    ; TUNEAMP(samp_hardswell)
	; SETBFREQ(7)	
	; SJSR(s_vic_tcommon1)
	; SLOOPSTART($02)
		; SJSR(s_vic_tcommon2)		
	; SLOOPEND
    ; ENDSND
;****************************************************************
 
    NEWSNDAMP(samp_xlife)   ;,$59
    .byte $02,$3C
	.byte $0E,$00
	.byte $02,$C4
	.byte $0E,$00
	.byte $FF,$04,$0B			;Loop back 4 times
    
    NEWSNDFREQ(sfreq_xlife)  ;,$59
    .byte $02,$60,$00
	.byte $9E,$00,$00

    NEWTUNE(s_xlife)    
    TUNEFREQ(sfreq_xlife)
    TUNEAMP(samp_xlife)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$A0)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_bonus)   ;,$5a
    .byte $02,$10,$06,$00
    
    NEWSNDFREQ(sfreq_bonus)  ;,$5a
    .byte $02,$F0,$00,$06,$00,$00
        
    NEWTUNE(s_bonus)    
    TUNEFREQ(sfreq_bonus)
    TUNEAMP(samp_bonus)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$08)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_shield)  ;,$5b
    ;.byte $02,$20,$02,$00,$02,$04,$FF,$06,$05
    SCTRL(16,2)              ;Eff Slope:8      Dur:2    Net:16
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:16
	SBEGIN
		SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:18
	SLOOP(6)                 ;Loop Back 6 times.

    NEWSNDFREQ(sfreq_shield) ;,$5b
    SFREQ(56,2)              ;Eff Slope:56     Dur:2    Net:112
	SFREQ(0,18)              ;Eff Slope:0      Dur:18   Net:112
	;.byte $02,$70,$00,$12,$00,$00
        
    NEWTUNE(s_shield)   
    TUNEFREQ(sfreq_shield)
    TUNEAMP(samp_shield)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$12)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_manwall) ;,$5c
    .byte $02,$3C,$08,$00,$02,$F0,$08,$00,$02,$F8,$02,$00,$02,$F8,$FF,$04
    .byte $07,$1A,$00
    
    NEWSNDFREQ(sfreq_manwall)    ;,$5c
    .byte $0E,$00,$00
	.byte $02,$20,$01
	.byte $06,$00,$00
	.byte $02,$00,$FF
	.byte $04,$00,$00
	.byte $02,$20,$00
	.byte $04,$00,$00
	.byte $FF,$05,$09
	.byte $02,$30,$00
	.byte $04,$00,$00
            
    NEWTUNE(s_manwall)  
    TUNEFREQ(sfreq_manwall)
    TUNEAMP(samp_manwall)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$44)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_oxygen)  ;,$5d
    .byte $02,$3C,$04,$00,$02,$FC,$FF,$08,$07,$02,$00,$02,$FC,$FF,$05,$07
    
    NEWSNDFREQ(sfreq_oxygen) ;,$5d
    .byte $50,$00,$00
        
    NEWTUNE(s_oxygen)   
    TUNEFREQ(sfreq_oxygen)
    TUNEAMP(samp_oxygen)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$50)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_robshot) ;,$5e
    .byte $02,$3C,$02,$F8,$02,$CC,$02,$0C,$02,$00,$02,$FC,$02,$00,$02,$FC,$02,$00
    
    NEWSNDFREQ(sfreq_robshot)    ;,$5e
    .byte $02,$40,$01
	.byte $04,$00,$00
	.byte $02,$C0,$03
	.byte $0A,$00,$01
        
    NEWTUNE(s_robshot)  
    TUNEFREQ(sfreq_robshot)
    TUNEAMP(samp_robshot)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$12)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_footstep)    ;,$5f
    .byte $02,$3C,$02,$C4
    
    NEWSNDFREQ(sfreq_footstep)   ;,$5f
    .byte $02,$10,$00
	.byte $02,$00,$00
        
    NEWTUNE(s_footstep) 
    TUNEFREQ(sfreq_footstep)
    TUNEAMP(samp_footstep)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$04)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_nooxy)   ;,$60
    .byte $02,$20
	.byte $26,$00
	.byte $02,$E0
	.byte $26,$00
	.byte $FF,$02,$0B
    
    NEWSNDFREQ(sfreq_nooxy)  ;,$60
    .byte $02,$40,$01
	.byte $EE,$00,$00
        
    NEWTUNE(s_nooxy)    
    TUNEFREQ(sfreq_nooxy)
    TUNEAMP(samp_nooxy)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$F0)
    ENDSND

    
;*************************************
    NEWSNDAMP(samp_rhum)  	 ;,$63
	SCTRL(8,2)               ;Eff Slope:4      Dur:2    Net:8
	SCTRL(0,14)              ;Eff Slope:0      Dur:14   Net:8
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:10
	SCTRL(4,4)               ;Eff Slope:2      Dur:4    Net:18
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:20
	SCTRL(4,4)               ;Eff Slope:2      Dur:4    Net:28
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:30
	SCTRL(0,24)              ;Eff Slope:0      Dur:24   Net:30
	SCTRL(-2,22)             ;Eff Slope:-1     Dur:22   Net:8
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:8
	
	;Validated
    ; .byte $02,$10
    ; .byte $0E,$00
    ; .byte $02,$04
    ; .byte $04,$08
    ; .byte $02,$04
    ; .byte $04,$08
    ; .byte $02,$04
    ; .byte $18,$00
    ; .byte $16,$FC
    ; .byte $08,$00
    
    NEWSNDFREQ(sfreq_rhum) ;,$63
	SFREQ(480,2)             ;Eff Slope:480    Dur:2    Net:960
	SFREQ(0,82)              ;Eff Slope:0      Dur:82   Net:960
	
	;Validated
    ;.byte $02,$C0,$03
	;.byte $52,$00,$00

    NEWTUNE(s_rhum)   
    SETDCTL($01)
    TUNEFREQ(sfreq_rhum)
    TUNEAMP(samp_rhum)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY4)
    NOTE(_REST,$54)
    ENDSND
    
;*************************************
    NEWSNDAMP(samp_redalert)  ;,$64
    .byte $02,$24,$56,$00
    
    NEWSNDFREQ(sfreq_redalert)   ;,$64
    .byte $02,$40,$07,$02,$00,$00,$02
    .byte $00,$FE,$04,$00,$00,$02,$F0,$01,$04,$00,$00,$02,$10,$FE,$04,$00
    .byte $00,$02,$E0,$01,$04,$00,$00,$02,$20,$FE,$04,$00,$00,$02,$E0,$01
    .byte $04,$00,$00,$FF,$04,$0F
        
    NEWTUNE(s_redalert) 
    TUNEFREQ(sfreq_redalert)
    TUNEAMP(samp_redalert)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$58)
    ENDSND
    
    
;*************************************
    NEWSNDAMP(samp_beeps)  ;,$67
    .byte $02,$10
	.byte $06,$00
    .byte $02,$F0
	.byte $02,$00
    .byte $FF,$4A
    .byte $0B
    
    NEWSNDFREQ(sfreq_beeps)  ;,$67
    .byte $02,$F0,$00
	.byte $0A,$00,$00
    .byte $0C,$00,$00
	.byte $FF,$4A,$06
        
    NEWTUNE(s_beeps)    
    TUNEFREQ(sfreq_beeps)
    TUNEAMP(samp_beeps)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$8A)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$FF)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$FF)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$FF)
    ENDSND
    	
	; .byte $33,$55
	; .byte $05,$54
	; .byte $FB,$01
	; .byte $00,$04
	; .byte $00,$01
	; .byte $40,$00
	; .byte $00,$10
	; .byte $00,$01
	; .byte $10,$00
	; .byte $00	

;* Shield Energize Sound 

	NEWSNDFREQ(ir_scenergize_f1)
	.db $02,$00,$FF 
	.db $78,$F0,$FF  
	ENDSND
	
	NEWSNDFREQ(ir_scenergize_f2)
	.db $02,$10,$FF  
	.db $78,$F0,$FF 
	ENDSND
	
	NEWSNDFREQ(ir_scenergize_f3)
	.db $02,$00,$06 
	.db $78,$F0,$FF 
	ENDSND
	
	NEWSNDFREQ(ir_scenergize_f4)
	.db $02,$10,$06    
	.db $78,$F0,$FF 
	ENDSND
	
	NEWSNDAMP(ir_scenergize_c)
	.db $02,$90,$FA   
	.db $78,$00,$00
	ENDSND

rate_scenergize = 38d	
	
	NEWTUNE(s_scenergize) 
	;SETDCTL($01)
    TUNEFREQ(ir_scenergize_f1)
    TUNEAMP(ir_scenergize_c)
    SNOATTRACT
    SETBVOL($00)
	SETNRATE(rate_scenergize)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$4A)
    ENDSND
	
	NEWTUNE(s_scenergize2) 
    TUNEFREQ(ir_scenergize_f2)
    TUNEAMP(ir_scenergize_c)
    SNOATTRACT
    SETBVOL($00)
	SETNRATE(rate_scenergize)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$4A)
    ENDSND
	
	NEWTUNE(s_scenergize3) 
    TUNEFREQ(ir_scenergize_f3)
    TUNEAMP(ir_scenergize_c)
    SNOATTRACT
    SETBVOL($00)
	SETNRATE(rate_scenergize)
    SETDIST(CHCTL_POLY5X)
    NOTE(_REST,$4A)
    ENDSND
	
	NEWTUNE(s_scenergize4) 
    TUNEFREQ(ir_scenergize_f4)
    TUNEAMP(ir_scenergize_c)
    SNOATTRACT
    SETBVOL($00)
	SETNRATE(rate_scenergize)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$4A)
    ENDSND

;Shield Constant Background sound	
    ; NEWSNDAMP(s_scspin_c) ;,$52
    ; .byte $02,$10
	; .byte $FE,$00
	; .byte $FE,$00
	; .byte $FE,$00
	; .byte $FE,$00
    
    ; NEWSNDFREQ(s_scspin_f)    
    ; .byte $3E,$10,$09
	; .byte $02,$10,$00
	; .byte $FF,$0C,$09		
	; .byte $3A,$00,$00
	
	; NEWTUNE(s_scspin) 
	; ;SETDCTL($01)
    ; TUNEFREQ(s_scspin_f)
    ; TUNEAMP(s_scspin_c)
    ; SNOATTRACT
	; ;SETNRATE(rate_shield)
	; SETBVOL($00)
    ; SETDIST(CHCTL_NOPOLY)
	; SLOOPSTART($7F)
		; NOTE(_REST,$16)
		; ;SETDIST(CHCTL_NOPOLY)
		; ;NOTE(_REST,$FF)
		; ;SETDIST(CHCTL_NOPOLY)
		; ;NOTE(_REST,$FF)
		; ;SETDIST(CHCTL_NOPOLY)
		; ;NOTE(_REST,$FF)
		; ;SETDIST(CHCTL_NOPOLY)
		; ;NOTE(_REST,$FF)
	; SLOOPEND
    ; ENDSND
	
	; ;SETBVOL($00)
    ; ;SETDIST(CHCTL_POLY17_5)
    ; ;NOTE(_REST,$02)
    ; ;ENDSND

;**************************************
;* Star Castle Shield Hit

    NEWSNDAMP(s_schit_c)  
    .byte $02,$3C
	.byte $04,$00
	.byte $06,$F8
	.byte $02,$F4
	.byte $02,$F8
    
    NEWSNDFREQ(s_schit_f)   
    .byte $02,$70,$00
	.byte $10,$F0,$FF
	
    
    NEWTUNE(s_schit) 
    TUNEFREQ(s_schit_f)
    TUNEAMP(s_schit_c)
    SNOATTRACT
    SETBVOL($00)
	SETDIST(CHCTL_POLY5) 
    NOTE(_REST,$10)
    ENDSND

;**************************************
;* Star Castle Panel Drop
;**************************************

	NEWSNDAMP(s_scdrop_c)  
    .byte $02,$3C
	.byte $2E,$00
    
    NEWSNDFREQ(s_scdrop_f)   
    .byte $02,$00,$0F
	.byte $2E,$00,$04
	
    
    NEWTUNE(s_scdrop) 
    TUNEFREQ(s_scdrop_f)
    TUNEAMP(s_scdrop_c)
    SNOATTRACT
    SETBVOL($00)
	SETDIST(CHCTL_NOPOLY) 
    NOTE(_REST,$30)
    ENDSND

;**************************************
;* Tact Fail
;**************************************
    NEWSNDAMP(samp_fail)   ;,$59
    .byte $02,$3C
	.byte $1E,$00
	.byte $02,$C4
	.byte $1E,$00
	.byte $FF,$03,$0B	;Branch back 4 times, $0B bytes (to the start $02)
    
    NEWSNDFREQ(sfreq_fail)  ;,$59
    .byte $02,$60,$00
	.byte $FE,$00,$00
	
	;.byte $02,$60,$00
	;.byte $9E,$00,$00

    NEWTUNE(s_fail)    
    TUNEFREQ(sfreq_fail)
    TUNEAMP(samp_fail)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY5)
    NOTE(_REST,$FE)
    ENDSND
	

;**************************************
;* Tact Fail - Background Tone
;**************************************
    NEWSNDAMP(samp_fail2)   ;,$59
    .byte $02,$3C
	.byte $1E,$00
	.byte $02,$C4
	.byte $1E,$00
	.byte $FF,$03,$0B	;Branch back 4 times, $0B bytes (to the start $02)
    
    NEWSNDFREQ(sfreq_fail2)  ;,$59
    .byte $02,$00,$06
	.byte $FE,$00,$00
	
	;.byte $02,$60,$00
	;.byte $9E,$00,$00

    NEWTUNE(s_fail2)    
    TUNEFREQ(sfreq_fail2)
    TUNEAMP(samp_fail2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    NOTE(_REST,$FE)
    ENDSND
	
;**************************************
;* Broken Transporter Sound
;**************************************
    ; NEWSNDAMP(samp_notrans)  
    ; .byte $02,$3C
	; .byte $42,$00
	
	; NEWSNDFREQ(sfreq_notrans) 
    ; .byte $04,$20,$0E
	; .byte $04,$00,$00
	; .byte $06,$10,$00
	; .byte $FF,$04,$06
	; .byte $0E,$F0,$FF
	
    
    ; NEWTUNE(s_notrans) 
    ; TUNEFREQ(sfreq_notrans)
    ; TUNEAMP(samp_notrans)
    ; SNOATTRACT
    ; SETBVOL($00)
	; SETDIST(CHCTL_NOPOLY) 
    ; NOTE(_REST,$28)
    ; ENDSND
	
	
;*Maynard Fly In
	NEWTUNE(s_maynard)
    TUNEFREQ(sfreq_launch)
    TUNEAMP(samp_launch)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$FE)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,$8F)

    TUNEFREQ(sfreq_bhend)
    TUNEAMP(samp_bhend)
    SNOATTRACT
	SETNRATE(RATE_LARGO)
    SETBVOL($0F)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,236)
	SETDIST(CHCTL_POLY17)
	NOTE(_REST,235)
	SETDIST(CHCTL_POLY17)
	NOTE(_REST,235)
    ENDSND
	
	NEWTUNE(s_maynard2)
    TUNEFREQ(sfreq_launch2)
    TUNEAMP(samp_launch2)
    SNOATTRACT
    SETBVOL($00)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$FE)
    SETDIST(CHCTL_POLY17_5)
    NOTE(_REST,$8F)
    ENDSND
	
;* Black Hole End s_bhend
    NEWSNDAMP(samp_bhend)  
    SCTRL(12,1)              ;Eff Slope:6      Dur:1    Net:6
	SBEGIN
	 SCTRL(0,235)             ;Eff Slope:0      Dur:235  Net:6
	SLOOP(3)
	
	NEWSNDFREQ(sfreq_bhend) 
    SFREQ(12,1)              ;Eff Slope:12     Dur:1    Net:12
	SBEGIN
	 SFREQ(1,235)             ;Eff Slope:0.09      Dur:235  Net:12
	SLOOP(3)
    
    
    NEWTUNE(s_bhend) 
    TUNEFREQ(sfreq_bhend)
    TUNEAMP(samp_bhend)
    SNOATTRACT
	SETNRATE(RATE_LARGO)
    SETBVOL($0F)
    SETDIST(CHCTL_POLY17)
    NOTE(_REST,236)
	SETDIST(CHCTL_POLY17)
	NOTE(_REST,235)
	SETDIST(CHCTL_POLY17)
	NOTE(_REST,235)
    ENDSND

;************************************
; Bonus Rollup Sound	
;************************************

    NEWSNDAMP(samp_bonusr)  
    SCTRL(60,1)              ;Eff Slope:6      Dur:1    Net:6
	SBEGIN
	 SCTRL(0,235)             ;Eff Slope:0      Dur:235  Net:6
	SLOOP(15)
	
	NEWSNDFREQ(sfreq_bonusr) 
    SFREQ(7,1)              ;Eff Slope:12     Dur:1    Net:12
	SBEGIN
	 	 .byte $EB,$FE,$FF
	SLOOP(15)
	
    NEWTUNE(s_bonusr)   
    TUNEFREQ(sfreq_bonusr)
    TUNEAMP(samp_bonusr)
    SNOATTRACT
	SETNRATE(RATE_LARGO)
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
	NOTE(_REST,236)
	SLOOPSTART(15)	
		NOTE(_REST,235)
    SLOOPEND
    ENDSND

	;Second Note
    NEWSNDAMP(samp_bonusr2)  
    SCTRL(60,1)              ;Eff Slope:6      Dur:1    Net:6
	SBEGIN
	 SCTRL(0,235)             ;Eff Slope:0      Dur:235  Net:6
	SLOOP(15)
	
	NEWSNDFREQ(sfreq_bonusr2) 
    SFREQ(12,1)              ;Eff Slope:12     Dur:1    Net:12
	SBEGIN
	 	 .byte $EB,$FE,$FF
	SLOOP(15)
	
    NEWTUNE(s_bonusr2)   
    TUNEFREQ(sfreq_bonusr2)
    TUNEAMP(samp_bonusr2)
    SNOATTRACT
	SETNRATE(RATE_LARGO)
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
	NOTE(_REST,236)
	SLOOPSTART(15)	
		NOTE(_REST,235)
    SLOOPEND
    ENDSND	

	;Third Note
    NEWSNDAMP(samp_bonusr3)  
    SCTRL(60,1)              ;Eff Slope:6      Dur:1    Net:6
	SBEGIN
	 SCTRL(0,235)             ;Eff Slope:0      Dur:235  Net:6
	SLOOP(15)
	
	NEWSNDFREQ(sfreq_bonusr3) 
    SFREQ(15,1)              ;Eff Slope:12     Dur:1    Net:12
	SBEGIN
	 	 .byte $EB,$FE,$FF
	SLOOP(15)
	
    NEWTUNE(s_bonusr3)   
    TUNEFREQ(sfreq_bonusr3)
    TUNEAMP(samp_bonusr3)
    SNOATTRACT
	SETNRATE(RATE_LARGO)
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
	NOTE(_REST,236)
	SLOOPSTART(15)	
		NOTE(_REST,235)
    SLOOPEND
    ENDSND	

	;Fourth Note
    NEWSNDAMP(samp_bonusr4)  
    SCTRL(60,1)              ;Eff Slope:6      Dur:1    Net:6
	SBEGIN
	 SCTRL(0,235)             ;Eff Slope:0      Dur:235  Net:6
	SLOOP(15)
	
	NEWSNDFREQ(sfreq_bonusr4) 
    SFREQ(13,1)              ;Eff Slope:12     Dur:1    Net:12
	SBEGIN
	 	 .byte $EB,$FE,$FF
	SLOOP(15)
	
    NEWTUNE(s_bonusr4)   
    TUNEFREQ(sfreq_bonusr4)
    TUNEAMP(samp_bonusr4)
    SNOATTRACT
	SETNRATE(RATE_LARGO)
    SETBVOL($00)
    SETDIST(CHCTL_POLY4_5)
	NOTE(_REST,236)
	SLOOPSTART(15)	
		NOTE(_REST,235)
    SLOOPEND
    ENDSND		
	
;Max WooWoo Background Sound
__maxmvfreqbase = -36
__maxmvfreqadd = -500
__maxmvampbase = 24
__maxmvampadd  = 8

_mmf0 = __maxmvfreqbase+(__maxmvfreqadd*0)
_mma0 = __maxmvampbase+(__maxmvampadd*0)
_mmf1 = __maxmvfreqbase+(__maxmvfreqadd*1)
_mma1 = __maxmvampbase+(__maxmvampadd*1)
_mmf2 = __maxmvfreqbase+(__maxmvfreqadd*2)
_mma2 = __maxmvampbase+(__maxmvampadd*2)
_mmf3 = __maxmvfreqbase+(__maxmvfreqadd*3)
_mma3 = __maxmvampbase+(__maxmvampadd*3)
    
    NEWSNDFREQ(sfreq_maxmv0) 
	SFREQ2(_mmf0,1)	;SFREQ2(-1536,1)
    SBEGIN   
	SFREQ2(-64,40)
	SFREQ2(64,40)
    SLOOP(253)
    
    NEWSNDAMP(samp_maxmv0) 
	SCTRL(_mma0,1)  
    SBEGIN    	
	SCTRL(0,80)  
    SLOOP(253)   
    
;maxsndcbuflen	=  * - maxsndcbufd   

               
	NEWTUNE(s_maxmv0)   
    TUNEFREQ(sfreq_maxmv0)
    TUNEAMP(samp_maxmv0)
    SNOATTRACT
	SETNRATE(RATE_DEFAULT)
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    SLOOPSTART($F0)
        NOTE(_REST,80)		;MUST BE EVEN
    SLOOPEND
    ENDSND
    


    NEWSNDFREQ(sfreq_maxmv1) 
	SFREQ2(_mmf1,1)	;SFREQ2(-1536,1)
    SBEGIN   
	SFREQ2(-64,40)
	SFREQ2(64,40)
    SLOOP(253)     

    NEWSNDAMP(samp_maxmv1) 
	SCTRL(_mma1,1)  	;12
    SBEGIN    
	SCTRL(0,80)  
    SLOOP(253)    

	NEWTUNE(s_maxmv1)   
    TUNEFREQ(sfreq_maxmv1)
    TUNEAMP(samp_maxmv1)
    SNOATTRACT
	SETNRATE(RATE_DEFAULT)
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    SLOOPSTART($F0)
        NOTE(_REST,80)		;MUST BE EVEN
    SLOOPEND
    ENDSND
    
    
    NEWSNDFREQ(sfreq_maxmv2) 
	SFREQ2(_mmf2,1)	;SFREQ2(-1536,1)
    SBEGIN   
	SFREQ2(-64,40)
	SFREQ2(64,40)
    SLOOP(253)  
    
    NEWSNDAMP(samp_maxmv2) 
	SCTRL(_mma2,1)  	;12
    SBEGIN    
	SCTRL(0,80)  
    SLOOP(253)    

	NEWTUNE(s_maxmv2)   
    TUNEFREQ(sfreq_maxmv2)
    TUNEAMP(samp_maxmv2)
    SNOATTRACT
	SETNRATE(RATE_DEFAULT)
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    SLOOPSTART($F0)
        NOTE(_REST,80)		;MUST BE EVEN
    SLOOPEND
    ENDSND
    
    NEWSNDFREQ(sfreq_maxmv3) 
	SFREQ2(_mmf3,1)	;SFREQ2(-1536,1)
    SBEGIN   
	SFREQ2(-64,40)
	SFREQ2(64,40)
    SLOOP(253)  
    
    NEWSNDAMP(samp_maxmv3) 
	SCTRL(_mma3,1)  	;12
    SBEGIN    
	SCTRL(0,80)  
    SLOOP(253)    

	NEWTUNE(s_maxmv3)   
    TUNEFREQ(sfreq_maxmv3)
    TUNEAMP(samp_maxmv3)
    SNOATTRACT
	SETNRATE(RATE_DEFAULT)
    SETBVOL($00)
    SETDIST(CHCTL_NOPOLY)
    SLOOPSTART($F0)
        NOTE(_REST,80)		;MUST BE EVEN
    SLOOPEND
    ENDSND


; ;Hidden Level background sound #1
hlbksl = $10

	NEWTUNE(s_hiddenbk1) 
    SETBVOL($08)
	;ADDBVOL(-4)
    SETNRATE($25) ;RATE_LARGO
    TUNEAMP(samp_hardswell)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	SJSR(s_hiddenbk1x)
	ENDSND
	
	;sub-sound for above
	NEWTUNE(s_hiddenbk1x) 	
    SLOOPSTART($8)
		NOTE(_A0,hlbksl)
		NOTE(_A0,hlbksl)
		NOTE(_REST,$hlbksl)
		NOTE(_A0,$hlbksl)
		NOTE(_A0,$hlbksl)
		NOTE(_REST,hlbksl)
		NOTE(_A0,hlbksl)
		NOTE(_A0,hlbksl)
		NOTE(_REST,hlbksl)
		NOTE(_A0,hlbksl) ;*2
		NOTE(_REST,$hlbksl)
		NOTE(_C1,hlbksl)
		NOTE(_D1,hlbksl)
		NOTE(_C1,hlbksl)
		NOTE(_D1,hlbksl)
		NOTE(_C1,hlbksl)
		
		NOTE(_A0,hlbksl)
		NOTE(_A0,hlbksl)
		NOTE(_REST,$hlbksl)
		NOTE(_A0,$hlbksl)
		NOTE(_A0,$hlbksl)
		NOTE(_REST,hlbksl)
		NOTE(_A0,hlbksl)
		NOTE(_A0,hlbksl)
		NOTE(_REST,hlbksl)
		NOTE(_A0,hlbksl) ;*2
		NOTE(_REST,$hlbksl)
		NOTE(_C1,hlbksl)
		NOTE(_D1,hlbksl)
		NOTE(_E1,hlbksl)
		NOTE(_F1,hlbksl)
		NOTE(_G1,hlbksl)
	SLOOPEND
	
	NOTE(_A1,hlbksl*2)
	NOTE(_G1,hlbksl*2)
	NOTE(_F1,hlbksl*2)
	NOTE(_D1,hlbksl*2)
	NOTE(_C1,hlbksl*2)
	NOTE(_G0,hlbksl*2)
	NOTE(_Ab0,hlbksl*2)
    ENDSND

; ;Hidden Level background sound #2	
; ;Robots - Flight of the Conchords
; hlbksl2 = $16

	; NEWTUNE(s_hiddenbk2) 
    ; SETBVOL($08)
	; ;ADDBVOL(-4)
    ; SETNRATE(RATE_LARGO)
	; SETDIST(CHCTL_NOPOLY)
    ; TUNEAMP(samp_hardswell) ;samp_robot)
	
	; SLOOPSTART(8)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_E2,hlbksl2)
	; SLOOPEND
	; SLOOPSTART(2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A2,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A2,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D3,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D3,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_C3,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_C3,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D3,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D3,hlbksl2)
	; SLOOPEND
	; SLOOPSTART(8)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_E2,hlbksl2)
	; SLOOPEND
	; SLOOPSTART(1)
		; NOTE(_F2,hlbksl2)
		; NOTE(_F2,hlbksl2)
		; NOTE(_F2,hlbksl2)
		; NOTE(_F3,hlbksl2)
		; NOTE(_F2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_F2,hlbksl2)
		; NOTE(_F3,hlbksl2)
		; NOTE(_F2,hlbksl2)
		; NOTE(_F3,hlbksl2)
		; NOTE(_F2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		
		; NOTE(_G2,hlbksl2)
		; NOTE(_G2,hlbksl2)
		; NOTE(_G2,hlbksl2)
		; NOTE(_G3,hlbksl2)
		; NOTE(_G2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_G2,hlbksl2)
		; NOTE(_G3,hlbksl2)
		; NOTE(_G2,hlbksl2)
		; NOTE(_G3,hlbksl2)
		; NOTE(_G2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		
		; NOTE(_D2,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D3,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D3,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_D2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		
		; NOTE(_C2,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_C3,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_C3,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_C2,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
		; NOTE(_REST,hlbksl2)
	; SLOOPEND
	; SLOOPSTART(8)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_A1,hlbksl2)
		; NOTE(_E2,hlbksl2)
	; SLOOPEND
    ; ENDSND
	
; ;Hidden Level background sound #3
; ;Chameleon
; hlbksl3 = 30

	; NEWTUNE(s_hiddenbk3) 
    ; SETBVOL($08)
	; ;ADDBVOL(-4)
    ; SETNRATE(RATE_LARGO)
    ; TUNEAMP(samp_funk)
    ; SLOOPSTART(4)
		; NOTE(_REST,hlbksl3)
		; NOTE(_G0,hlbksl3)
		; NOTE(_Ab0,hlbksl3)
		; NOTE(_A0,hlbksl3)
		; NOTE(_Bb0,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Ab1+1,$hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Bb1+1,hlbksl3)
		; NOTE(_REST,hlbksl3)
		; ;NOTE(_REST,hlbksl3)

		; NOTE(_REST,hlbksl3)
		; NOTE(_C1,hlbksl3)
		; NOTE(_Db1,hlbksl3)
		; NOTE(_D1,hlbksl3)
		; NOTE(_Eb1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Db2+1,$hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Eb2+1,hlbksl3)
		; NOTE(_REST,hlbksl3)

	; SLOOPEND
	; TUNEAMP(samp_hardswell)
	; SLOOPSTART(4)
		; NOTE(_Bb1+1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Ab1+1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_F1+1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Db3+1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Bb3+1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
	    ; NOTE(_Bb1+1,hlbksl3/2)
		; NOTE(_Ab1+1,hlbksl3/2)
		; NOTE(_F1+1,hlbksl3/2)
		; NOTE(_Ab1+1,hlbksl3/2)
		; NOTE(_Bb1+1,hlbksl3/2)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Db3+1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
		; NOTE(_Eb3+1,hlbksl3)
		; NOTE(_REST,hlbksl3/2)
	; SLOOPEND
	
    ; ENDSND

; ;Hidden Level background sound #4
; ;I Robot
; hlbksl4 = $18
; hlbksr4 = $18

	; NEWTUNE(s_hiddenbk4) 
    ; SETBVOL($08)
	; ;ADDBVOL(-4)
    ; SETNRATE(150d) ;Default is 64d
    ; TUNEAMP(samp_hardswell2)
	; SLOOPSTART(7)
		; NOTE(_F1,hlbksl4)			;D String Fret 3
		; NOTE(_REST,hlbksr4)
		; NOTE(_G1,hlbksl4)			;G String Fret 0
		; NOTE(_REST,hlbksr4)
		; NOTE(_Bb1,hlbksl4)			;G String Fret 3
		; NOTE(_REST,hlbksr4)
		; NOTE(_G1,hlbksl4)			;G String Fret 0
		; NOTE(_REST,hlbksr4)
		; NOTE(_F1,hlbksl4)			;D String Fret 3
		; NOTE(_REST,hlbksr4)
		; NOTE(_G1,hlbksl4)			;G String Fret 0
		; NOTE(_REST,hlbksr4)
		; NOTE(_C1,hlbksl4*4)		;whole - A String Fret 3
    ; SLOOPEND
	; NOTE(_F1,hlbksl4)			;D String Fret 3
	; NOTE(_REST,hlbksr4)
	; NOTE(_G1,hlbksl4)			;G String Fret 0
	; NOTE(_REST,hlbksr4)
	; NOTE(_F1,hlbksl4)			;D String Fret 3
	; NOTE(_REST,hlbksr4)
	; NOTE(_G1,hlbksl4)			;G String Fret 0
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;G String Fret 3
	; NOTE(_REST,hlbksr4)
	; NOTE(_G1,hlbksl4)			;G String Fret 0
	; NOTE(_REST,hlbksr4)
	; NOTE(_C1,hlbksl4*4)		;whole - A String Fret 3
	; SLOOPSTART(7)
		; NOTE(_C1,hlbksl4)			;A String Fret 3
		; NOTE(_REST,hlbksr4)
		; NOTE(_G1,hlbksl4)			;G String Fret 0
		; NOTE(_REST,hlbksr4)
		; NOTE(_F1,hlbksl4)			;D String Fret 3
		; NOTE(_REST,hlbksr4)
		; NOTE(_G1,hlbksl4)			;G String Fret 0
		; NOTE(_REST,hlbksr4)
		; NOTE(_Bb1,hlbksl4)			;G String Fret 3
		; NOTE(_REST,hlbksr4)
		; NOTE(_G1,hlbksl4)			;G String Fret 0
		; NOTE(_REST,hlbksr4)
		; NOTE(_C1,hlbksl4*4)		;whole - A String Fret 3
	; SLOOPEND
	; ;Bar 51
	; NOTE(_C1,hlbksl4)			;A String Fret 3
	; NOTE(_REST,hlbksr4)
	; NOTE(_G1,hlbksl4)			;G String Fret 0
	; NOTE(_REST,hlbksr4)
	; NOTE(_F1,hlbksl4)			;D String Fret 3
	; NOTE(_REST,hlbksr4)
	; NOTE(_G1,hlbksl4)			;G String Fret 0
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;G String Fret 3
	; NOTE(_REST,hlbksr4)
	; NOTE(_G1,hlbksl4)			;G String Fret 0
	; NOTE(_REST,hlbksr4)
	; NOTE(_Eb1,hlbksl4*4)			;whole - D String Fret 1

	; NOTE(_Eb1,hlbksl4)			;whole - D String Fret 1
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;G String 3rd Fret = Bb
	; NOTE(_REST,hlbksr4)
	; NOTE(_Ab1,hlbksl4)			;G String 1st Fret = Ab
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;G String 3rd Fret = Bb
	; NOTE(_REST,hlbksr4)
	; NOTE(_Eb2,hlbksl4)			;G String 8th Fret = Eb2
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;D String 8th Fret = Bb
	; NOTE(_REST,hlbksr4)
	; NOTE(_Eb1,hlbksl4*4)		;double A String 6th Fret = Eb
	; NOTE(_REST,hlbksr4)
	; ;Bar 52
	; NOTE(_Eb1,hlbksl4)			;A String 6th Fret = Eb
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;G String 3rd Fret = Bb
	; NOTE(_REST,hlbksr4)
	; NOTE(_Ab1,hlbksl4)			;G String 1st Fret = Ab
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;G String 3rd Fret = Bb
	; NOTE(_REST,hlbksr4)
	; NOTE(_Eb2,hlbksl4)			;G String 8th Fret = Eb2
	; NOTE(_REST,hlbksr4)
	; NOTE(_Bb1,hlbksl4)			;D String 8th Fret = Bb
	; NOTE(_REST,hlbksr4)
	; NOTE(_C0,hlbksl4*4)		;double E String 8th Fret = C
	; NOTE(_REST,hlbksr4)
    ; ENDSND

;****************************************** 
;* Speech Data goes here 
;* NOTE: Each block must end with $FF,$FF           
;******************************************
 
#IF ___TLK == 1

;Speech Chip Test binary from Gauntlet
sps_test
    .db $0C,$F8,$21,$9C,$01,$3F,$A6,$33,$E0,$A7,$76,$01,$FC,$D4,$0C,$06
    .db $E8,$32,$AC,$EC,$43,$28,$E4,$76,$AD,$B2,$4E,$A6,$90,$3B,$95,$CA
    .db $3C,$99,$40,$DD,$46,$6E,$E3,$14,$02,$B9,$1B,$B9,$F5,$93,$09,$D6
    .db $5E,$A5,$D2,$4E,$CA,$D8,$BD,$91,$42,$DD,$65,$98,$9B,$D9,$36,$C0
    .db $AF,$65,$0A,$78,$7B,$D3,$00,$6F,$6E,$0A,$E0,$B5,$6D,$06,$DC,$36
    .db $05,$06,$B8,$D9,$DD,$00,$EF,$6C,$19,$E0,$AD,$EB,$51,$DE,$B6,$E1
    .db $02,$B2,$47,$BE,$1C,$93,$50,$CC,$1E,$69,$53,$A2,$91,$53,$69,$C4
    .db $43,$BA,$B8,$77,$CD,$16,$36,$11,$9A,$D6,$95,$9A,$5F,$79,$6A,$58
    .db $4E,$1A,$6E,$96,$E1,$69,$31,$39,$39,$95,$87,$A5,$66,$6D,$70,$40
    .db $31,$2E,$0A,$28,$36,$84,$01,$C5,$95,$00,$80,$03,$8E,$50,$72,$40
    .db $03,$22,$C3,$CE,$CA,$2C,$B2,$60,$0D,$AB,$EA,$D0,$8C,$B5,$B4,$CC
    .db $A6,$4A,$D2,$A7,$D2,$32,$BA,$6C,$4D,$EF,$49,$43,$6F,$72,$24,$AD
    .db $2B,$35,$2D,$BB,$F0,$F0,$15,$9D,$D4,$A4,$C3,$C3,$46,$54,$52,$26
    .db $49,$76,$68,$53,$49,$DE,$50,$C8,$49,$13,$06,$E9,$BB,$90,$60,$B1
    .db $19,$C4,$1F,$CA,$4A,$D4,$B1,$11,$7E,$2A,$35,$16,$3A,$20,$80,$AB
    .db $D2,$09,$F0,$55,$E8,$03,$FF,$FF
spx_test
 
; 1-10 countdown data from Gauntlet 
sps_one
	.db $AC,$53,$A2,$AB,$52,$4D,$B3,$4E,$A8,$AE,$74,$B3,$CC,$5A,$6E,$BA
	.db $CA,$CC,$16,$AB,$99,$DB,$0C,$35,$DB,$AA,$60,$B1,$CB,$D9,$A3,$98
	.db $84,$E7,$ED,$24,$9A,$92,$22,$1B,$DE,$C9,$6D,$76,$89,$22,$FF,$C6
	.db $B0,$85,$2D,$0A,$E6,$83,$2A,$6B,$B7,$38,$9A,$0B,$89,$A8,$32,$D2
	.db $A4,$CF,$24,$A3,$CA,$28,$92,$1F,$F1,$8C,$AA,$A3,$CE,$B1,$D9,$BC
	.db $CA,$8E,$BE,$A4,$10,$B6,$89,$12,$86,$AC,$D8,$59,$27,$8D,$9B,$B2
	.db $16,$27,$ED,$BA,$6E,$CA,$92,$4D,$B5,$EB,$BA,$B9,$68,$56,$91,$6A
	.db $AA,$D6,$AC,$59,$45,$AA,$A9,$DA,$B2,$71,$14,$8B,$26,$6A,$2F,$C9
	.db $C1,$3A,$CB,$A2,$93,$67,$C9,$36,$A2,$FB,$00,$FF,$FF
spx_one

sps_two
		.db $02,$68,$41,$8D,$01,$25,$39,$27,$37,$47,$36,$B6,$58,$92,$BC,$14
		.db $59,$D5,$F3,$4D,$F2,$BC,$35,$8B,$F2,$3B,$C9,$B7,$26,$22,$4A,$E7
		.db $A4,$D0,$D8,$F4,$08,$99,$1D,$12,$9D,$DD,$DC,$AB,$A1,$CB,$54,$D4
		.db $31,$CE,$45,$21,$E7,$C5,$D3,$34,$17,$85,$92,$D7,$32,$B7,$6C,$ED
		.db $6A,$91,$22,$9D,$7D,$AB,$6B,$85,$6B,$4F,$D6,$B6,$6E,$90,$A6,$23
		.db $45,$D2,$9A,$49,$BA,$89,$10,$4D,$6A,$16,$69,$3B,$43,$24,$A9,$DA
		.db $94,$1D,$77,$16,$B7,$EC,$D0,$66,$D2,$58,$9D,$92,$9B,$2C,$63,$8D
		.db $8A,$F9,$00,$FF,$FF
spx_two

sps_three
		.db $04,$48,$C2,$DC,$A5,$B1,$92,$BB,$45,$5A,$13,$85,$81,$56,$A1,$69
		.db $4C,$18,$3A,$78,$3B,$67,$09,$41,$6C,$A8,$35,$DC,$3A,$84,$79,$A2
		.db $46,$C8,$A3,$16,$97,$AC,$24,$ED,$4D,$5A,$52,$82,$B0,$6E,$D6,$6D
		.db $69,$11,$26,$B6,$55,$65,$64,$4D,$1B,$73,$F5,$DC,$91,$0F,$69,$4C
		.db $35,$73,$5A,$D9,$85,$32,$F7,$D4,$69,$D5,$10,$46,$54,$53,$A7,$35
		.db $93,$2B,$51,$CD,$E2,$D6,$4F,$6E,$44,$35,$4D,$CA,$B0,$B8,$12,$D6
		.db $D4,$4E,$D3,$92,$46,$98,$33,$3B,$4D,$93,$2B,$71,$CD,$EC,$34,$0F
		.db $AE,$24,$3D,$93,$DD,$5A,$99,$92,$CF,$54,$71,$5B,$15,$4A,$3E,$5B
		.db $86,$EC,$56,$71,$56,$3B,$E9,$07,$FF,$FF
spx_three

sps_four
		.db $08,$C8,$D6,$9C,$02,$2E,$96,$AB,$52,$B8,$AA,$86,$48,$F5,$57,$76
		.db $9B,$9A,$22,$35,$36,$8C,$AD,$6A,$8A,$65,$DF,$74,$D2,$BA,$29,$51
		.db $69,$2B,$88,$A7,$A4,$CC,$FA,$AD,$24,$9C,$93,$72,$93,$B7,$82,$68
		.db $76,$29,$74,$9D,$4A,$92,$25,$A5,$B4,$B5,$2B,$50,$57,$97,$DA,$A7
		.db $AA,$14,$5A,$55,$5A,$DF,$23,$52,$74,$55,$E9,$7D,$37,$2F,$B5,$55
		.db $65,$08,$43,$AD,$42,$57,$A6,$31,$76,$8E,$36,$DD,$98,$A6,$D4,$B1
		.db $4A,$6D,$55,$98,$53,$C7,$2C,$97,$4D,$61,$4E,$1D,$2B,$5D,$36,$B9
		.db $39,$0D,$AC,$74,$DD,$E5,$D6,$34,$B0,$D2,$AC,$37,$59,$7D,$C1,$75
		.db $0D,$93,$0F,$FF,$FF
spx_four

sps_five
		.db $08,$C8,$C6,$9C,$02,$04,$08,$33,$2C,$65,$61,$A5,$46,$4E,$94,$96
		.db $C5,$15,$6E,$D9,$51,$46,$96,$F2,$98,$7B,$56,$19,$59,$F6,$EB,$16
		.db $59,$67,$14,$29,$B6,$BB,$F7,$94,$51,$A5,$52,$16,$D9,$53,$47,$93
		.db $4A,$A9,$47,$57,$1D,$7D,$1E,$66,$5E,$13,$65,$0C,$B9,$B8,$45,$4D
		.db $99,$31,$E6,$AA,$61,$3D,$51,$C6,$98,$93,$7A,$64,$47,$1D,$63,$89
		.db $6A,$5E,$5D,$A5,$4D,$C5,$A9,$7B,$F5,$98,$32,$65,$CF,$56,$53,$65
		.db $DA,$5C,$8C,$6A,$F4,$4C,$69,$4B,$F1,$4A,$D1,$B5,$B8,$2D,$D5,$AA
		.db $B8,$C7,$E3,$B0,$D6,$20,$EA,$E2,$4B,$D0,$8A,$83,$44,$45,$9A,$02
		.db $B2,$98,$20,$69,$D9,$A2,$C8,$6A,$A3,$36,$67,$10,$41,$AB,$4F,$68
		.db $E5,$42,$EA,$01,$FF,$FF
spx_five

sps_six
		.db $08,$58,$22,$8D,$00,$47,$55,$10,$E0,$48,$57,$02,$6C,$DD,$4E,$80
		.db $29,$53,$4B,$5C,$BC,$98,$F8,$D6,$1D,$71,$31,$C6,$1E,$3D,$77,$64
		.db $DD,$87,$A8,$E6,$93,$51,$76,$1F,$A2,$96,$4B,$5A,$DF,$53,$B0,$B8
		.db $AF,$31,$47,$5B,$2C,$E6,$39,$07,$80,$00,$E5,$50,$10,$60,$A8,$0B
		.db $02,$2C,$35,$41,$80,$A3,$32,$08,$B0,$54,$3B,$01,$96,$28,$47,$C0
		.db $E6,$EE,$08,$B8,$2A,$1D,$1E,$FF,$FF
spx_six

sps_seven
		.db $08,$98,$26,$8D,$00,$4B,$86,$12,$60,$89,$D0,$10,$75,$55,$2C,$5C
		.db $B5,$4A,$9C,$A2,$4A,$4C,$46,$1D,$71,$C9,$EE,$E2,$5D,$67,$C5,$D5
		.db $95,$B9,$E6,$9C,$91,$14,$57,$EA,$9A,$53,$4A,$9E,$6D,$AA,$4B,$5C
		.db $51,$75,$14,$AE,$61,$51,$29,$95,$39,$B8,$2A,$C7,$94,$51,$57,$9B
		.db $EA,$12,$77,$46,$57,$63,$89,$6A,$2D,$19,$43,$0B,$E5,$22,$B1,$38
		.db $4D,$4D,$86,$B2,$C9,$1C,$37,$55,$25,$6A,$EC,$F3,$DC,$54,$8D,$AA
		.db $52,$CC,$37,$4B,$D1,$AC,$2A,$55,$4F,$AD,$D9,$88,$0A,$57,$3D,$B5
		.db $25,$2B,$C2,$52,$4D,$1F,$FF,$FF
spx_seven

sps_eight
		.db $A9,$28,$5E,$D4,$AB,$AB,$8C,$74,$38,$13,$C9,$5A,$33,$92,$11,$5C
		.db $D8,$73,$C9,$48,$BA,$37,$91,$EC,$25,$23,$E9,$4E,$59,$7A,$E6,$8C
		.db $7C,$18,$16,$9E,$99,$DD,$9A,$15,$4C,$C0,$67,$4D,$19,$56,$0A,$05
		.db $9A,$D6,$7A,$00,$00,$06,$4C,$16,$8A,$80,$61,$59,$10,$D0,$8C,$29
		.db $02,$B2,$76,$01,$00,$78,$FF,$FF
spx_eight

sps_nine
		.db $6A,$48,$9C,$B2,$DC,$6D,$9B,$D6,$4B,$2E,$B3,$8C,$35,$F2,$16,$3C
		.db $49,$6C,$F1,$C8,$4B,$F1,$24,$CF,$29,$23,$2B,$A1,$C2,$25,$C6,$8E
		.db $34,$DB,$8D,$50,$9D,$32,$D2,$EC,$D6,$34,$A3,$CA,$48,$B3,$5B,$D3
		.db $88,$2A,$23,$CB,$76,$4D,$23,$AA,$8C,$2C,$DB,$35,$CD,$A8,$32,$F2
		.db $E2,$D6,$34,$62,$F2,$A8,$4A,$6E,$37,$CD,$B2,$A3,$69,$A1,$C2,$D8
		.db $E7,$8C,$B6,$87,$70,$35,$5F,$3C,$FA,$1E,$52,$C5,$7C,$C9,$18,$46
		.db $70,$65,$8B,$45,$63,$1C,$5E,$45,$22,$66,$B5,$69,$3A,$65,$09,$5F
		.db $14,$96,$26,$54,$A4,$BC,$92,$59,$8A,$22,$0B,$CB,$32,$66,$C9,$9A
		.db $5C,$AD,$E3,$A8,$35,$2B,$36,$B5,$AA,$2D,$D6,$62,$44,$44,$B2,$F2
		.db $03,$FF,$FF
spx_nine

sps_ten
		.db $0C,$18,$C2,$8D,$00,$4B,$B8,$A6,$A4,$C7,$14,$95,$A8,$52,$C2,$62
		.db $C5,$23,$27,$CA,$08,$6B,$E2,$90,$6E,$3B,$2B,$68,$59,$5D,$AD,$2B
		.db $AF,$B0,$65,$57,$D3,$6E,$BC,$A2,$96,$43,$4D,$A6,$CE,$4A,$6A,$29
		.db $35,$AE,$3A,$2B,$AB,$39,$5D,$A5,$AA,$AE,$A2,$46,$57,$D3,$A9,$32
		.db $EA,$1A,$8A,$55,$6A,$4C,$EB,$9A,$1D,$62,$F5,$C9,$A1,$2F,$D6,$48
		.db $A8,$AB,$BA,$A9,$1A,$15,$85,$AE,$E6,$A6,$6A,$44,$15,$BB,$A9,$9B
		.db $AB,$32,$36,$8C,$69,$66,$29,$46,$C5,$28,$E7,$EA,$E5,$01,$FF,$FF
spx_ten

; sps_xxx
	; .db $00,$82,$49,$D2,$53,$23,$E2,$0A,$80,$19,$F3,$00,$0C,$69,$99,$80
	; .db $EE,$34,$1A,$C0,$9A,$78,$C9,$9C,$D4,$CC,$A6,$64,$4D,$54,$21,$53
	; .db $4C,$92,$0E,$39,$B8,$4A,$09,$4A,$D3,$54,$17,$3D,$D5,$A8,$4D,$D3
	; .db $BD,$CB,$D4,$80,$A4,$C5,$F4,$26,$D2,$13,$D2,$04,$5B,$F8,$2E,$4D
	; .db $4C,$2A,$4E,$F8,$D2,$D9,$A9,$11,$EB,$E1,$9B,$80,$C0,$C6,$C4,$85
	; .db $2F,$4B,$18,$D2,$22,$1F,$BC,$09,$30,$48,$4B,$2C,$30,$AB,$D5,$41
	; .db $29,$73,$71,$B4,$19,$63,$D9,$CC,$C5,$C1,$7A,$5C,$E5,$30,$CF,$5A
	; .db $CA,$09,$4D,$4C,$9C,$68,$D9,$5B,$35,$2E,$DA,$72,$50,$1B,$91,$68
	; .db $00,$00,$00,$00,$00,$00,$80,$10,$F0,$26,$CD,$A8,$B2,$D0,$C0,$98
	; .db $4A,$87,$66,$AA,$04,$73,$D3,$12,$EA,$28,$15,$CC,$C9,$08,$68,$6A
	; .db $14,$70,$26,$DC,$B0,$A9,$91,$C0,$9E,$F0,$C0,$BA,$4D,$36,$D9,$DC
	; .db $8D,$12,$17,$25,$64,$17,$0F,$4C,$55,$8A,$D0,$4D,$C2,$C9,$55,$52
	; .db $43,$57,$09,$A7,$58,$49,$0B,$53,$34,$8C,$62,$A5,$DA,$4F,$B1,$50
	; .db $AE,$94,$CC,$D0,$D8,$53,$A9,$76,$B2,$42,$12,$2F,$C7,$44,$C9,$F1
	; .db $2E,$CC,$1A,$53,$07,$D7,$DA,$C8,$48,$6C,$E3,$06,$69,$73,$2C,$31
	; .db $8B,$1A,$68,$EC,$55,$E5,$26,$C2,$23,$71,$56,$D4,$2B,$90,$1D,$BC
	; .db $0C,$4E,$93,$89,$06,$F0,$3C,$C8,$C5,$26,$B2,$C1,$CD,$66,$15,$5B
	; .db $C8,$05,$3D,$C7,$0C,$6D,$23,$17,$C4,$EA,$28,$8C,$85,$06,$AA,$A3
	; .db $62,$38,$11,$69,$92,$51,$6D,$D3,$24,$E8,$28,$41,$6D,$92,$AD,$20
	; .db $A0,$56,$25,$74,$26,$AF,$BE,$2A,$56,$01,$00,$00,$00,$00,$00,$C0
	; .db $00,$D3,$B3,$36,$B9,$08,$DA,$49,$52,$5A,$64,$6D,$7A,$43,$28,$51
	; .db $50,$78,$9F,$25,$A2,$26,$49,$E3,$75,$56,$11,$5D,$07,$83,$A7,$39
	; .db $03,$4A,$62,$1C,$1A,$F6,$18,$39,$AE,$B8,$68,$9C,$63,$E0,$A6,$24
	; .db $C0,$69,$96,$90,$DA,$A2,$0E,$BB,$3E,$41,$AE,$8D,$38,$B8,$66,$4D
	; .db $45,$34,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$1A,$4B,$43
	; .db $C0,$10,$E5,$08,$18,$2E,$83,$00,$23,$A4,$32,$60,$F8,$74,$06,$4C
	; .db $EB,$CE,$80,$11,$52,$05,$D0,$94,$A9,$03,$79,$F2,$EE,$C0,$D8,$45
	; .db $D6,$A5,$D2,$12,$1A,$17,$C5,$B7,$08,$0B,$6C,$54,$D4,$D0,$3C,$2D
	; .db $21,$71,$D1,$42,$8D,$D0,$C4,$24,$C1,$D0,$35,$52,$1B,$92,$08,$87
	; .db $0E,$4B,$0F,$76,$CC,$6C,$EA,$22,$25,$CD,$89,$71,$44,$8A,$F6,$22
	; .db $D6,$C1,$D5,$BE,$46,$1B,$99,$18,$97,$C7,$E8,$30,$6C,$C2,$3C,$5C
	; .db $63,$53,$C1,$35,$F2,$C0,$AB,$44,$C5,$C6,$C8,$03,$AB,$4B,$15,$9C
	; .db $20,$17,$B4,$6E,$33,$B2,$8D,$6E,$6C,$AA,$BD,$D9,$16,$1A,$BD,$D2
	; .db $C8,$92,$24,$A8,$CA,$5A,$2C,$4A,$1C,$A3,$27,$6B,$55,$2F,$49,$0C
	; .db $00,$00,$00,$00,$00,$00,$80,$80,$26,$DD,$11,$D0,$B5,$0B,$03,$98
	; .db $0F,$23,$40,$57,$1E,$04,$18,$21,$94,$00,$DD,$86,$32,$A0,$1B,$35
	; .db $01,$54,$9B,$A9,$80,$66,$DD,$8D,$4B,$5A,$75,$05,$24,$49,$8A,$F6
	; .db $B3,$31,$10,$27,$69,$DA,$D6,$64,$93,$D2,$64,$3A,$1D,$1B,$49,$76
	; .db $9C,$EB,$74,$9C,$39,$DA,$41,$BA,$4C,$3D,$6A,$6C,$0B,$00,$14,$D0
	; .db $AA,$B8,$02,$6A,$C6,$10,$C0,$88,$5B,$08,$E8,$22,$04,$01,$53,$84
	; .db $21,$60,$28,$57,$04,$74,$5B,$86,$00,$C6,$43,$C9,$13,$85,$79,$85
	; .db $25,$23,$84,$27,$95,$55,$15,$99,$00,$87,$85,$23,$A0,$04,$53,$00
	; .db $04,$CC,$65,$09,$00,$00,$00,$00,$00,$80,$80,$E6,$23,$10,$30,$75
	; .db $26,$02,$86,$B2,$40,$C0,$D4,$6E,$86,$11,$AD,$4C,$42,$93,$14,$39
	; .db $2C,$76,$2F,$74,$5D,$94,$B8,$D8,$BD,$30,$71,$29,$E3,$12,$B3,$C6
	; .db $C4,$E5,$89,$4B,$DD,$0A,$13,$97,$35,$0E,$75,$2B,$48,$5C,$F4,$D8
	; .db $35,$AC,$30,$71,$32,$63,$D1,$B4,$84,$C4,$C9,$8A,$49,$D3,$13,$52
	; .db $07,$C7,$C5,$28,$0E,$76,$1C,$2E,$1B,$A3,$C5,$38,$89,$71,$8D,$8D
	; .db $91,$C0,$24,$C6,$35,$C6,$C7,$92,$1C,$AB,$41,$59,$1B,$2F,$52,$C5
	; .db $06,$1A,$63,$CC,$15,$29,$32,$B4,$51,$AF,$74,$25,$04,$A8,$C9,$94
	; .db $01,$3D,$B8,$33,$80,$85,$72,$01,$B4,$58,$C1,$CE,$2C,$DD,$BD,$3C
	; .db $09,$6A,$53,$72,$B7,$62,$35,$00,$00,$00,$00,$00,$80,$80,$66,$C2
	; .db $10,$C0,$4C,$18,$02,$86,$09,$43,$40,$93,$6A,$04,$18,$3A,$DC,$48
	; .db $41,$9B,$7B,$78,$12,$27,$E9,$87,$55,$2B,$AC,$9D,$A2,$1F,$46,$9D
	; .db $70,$32,$AA,$7E,$98,$75,$C6,$5A,$69,$FA,$41,$EE,$38,$6B,$A5,$CB
	; .db $87,$91,$AB,$98,$8D,$A1,$1F,$66,$2E,$B3,$31,$96,$3D,$D8,$39,$8C
	; .db $DA,$D9,$BE,$E3,$64,$0A,$1B,$E3,$98,$46,$E3,$6E,$4C,$4C,$2F,$BB
	; .db $0C,$9B,$2B,$35,$AE,$4D,$BC,$DC,$AC,$56,$B9,$36,$CA,$52,$29,$53
	; .db $E6,$7A,$4F,$2B,$E1,$48,$49,$67,$1D,$AE,$95,$33,$45,$5B,$49,$98
	; .db $AE,$29,$05,$01,$D5,$63,$03,$00,$00,$00,$00,$00,$00,$00,$00,$10
	; .db $A0,$6A,$37,$47,$65,$ED,$E1,$E1,$89,$11,$D0,$B9,$3B,$01,$9A,$CE
	; .db $70,$49,$D2,$1E,$5E,$E2,$C4,$28,$B8,$66,$6B,$B0,$5C,$A7,$F2,$A2
	; .db $E3,$A9,$74,$4C,$49,$97,$8E,$A6,$4A,$0E,$0A,$9F,$B6,$66,$C2,$38
	; .db $A8,$A2,$C6,$5A,$10,$ED,$A0,$89,$38,$1B,$8A,$B2,$83,$26,$6D,$5D
	; .db $2A,$CA,$76,$BA,$34,$75,$21,$60,$DB,$98,$DC,$F5,$B9,$80,$2D,$65
	; .db $D3,$78,$6B,$88,$8E,$D4,$C9,$CD,$5C,$12,$24,$62,$2E,$B7,$73,$09
	; .db $98,$8A,$8D,$42,$D6,$25,$42,$12,$E2,$52,$BD,$EB,$88,$76,$90,$2D
	; .db $58,$5D,$31,$46,$42,$63,$41,$9C,$ED,$9A,$18,$01,$95,$4A,$21,$80
	; .db $09,$2A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$06,$2C,$1B,$A6,$80
	; .db $69,$53,$1D,$30,$94,$5B,$00,$A6,$56,$0B,$C0,$F0,$AA,$41,$E0,$3A
	; .db $B7,$92,$91,$A5,$8C,$C7,$5E,$36,$50,$17,$64,$5E,$67,$54,$88,$6D
	; .db $50,$79,$AD,$F5,$04,$C4,$AE,$E2,$B5,$D6,$93,$90,$38,$9D,$F7,$9C
	; .db $08,$81,$6D,$4C,$BA,$7D,$32,$15,$89,$B2,$C9,$F3,$F4,$74,$D8,$CC
	; .db $21,$DF,$C2,$D3,$E1,$21,$97,$3C,$8E,$76,$85,$8B,$5C,$F4,$D9,$D2
	; .db $8C,$0E,$72,$C0,$53,$2B,$0B,$C9,$68,$0B,$43,$C4,$2C,$9C,$A2,$34
	; .db $16,$37,$73,$77,$8C,$56,$99,$CD,$DC,$DB,$29,$00,$00,$00,$00,$00
	; .db $00,$00,$00,$00,$00,$00,$00,$20,$03,$7C,$E6,$0C,$B7,$4B,$2C,$F8
	; .db $99,$C3,$DC,$0E,$33,$E8,$17,$EC,$D0,$B0,$4E,$D3,$8F,$71,$9A,$6D
	; .db $27,$2D,$1C,$C1,$19,$56,$5C,$B4,$B8,$99,$7B,$90,$71,$A9,$E3,$14
	; .db $9D,$20,$47,$C5,$8C,$5D,$A5,$92,$6C,$16,$2B,$26,$B5,$2A,$74,$54
	; .db $9C,$E8,$25,$AB,$D0,$B1,$39,$85,$8F,$8C,$81,$DA,$C2,$C5,$C3,$C7
	; .db $4C,$23,$31,$17,$CD,$2C,$77,$AC,$44,$5C,$B8,$2A,$CD,$29,$32,$71
	; .db $C1,$8C,$76,$C5,$58,$E8,$06,$A3,$DA,$15,$E4,$20,$07,$B4,$EC,$74
	; .db $92,$83,$7A,$6C,$A3,$6A,$D8,$0A,$7A,$A2,$34,$8B,$D1,$58,$68,$CB
	; .db $D2,$4C,$47,$12,$A3,$2D,$7B,$D6,$09,$B3,$8C,$B6,$EC,$4D,$3B,$C4
	; .db $0E,$00,$00,$00,$00,$00,$00,$00,$51,$40,$DA,$0E,$37,$3A,$C4,$02
	; .db $B5,$B3,$0C,$99,$B1,$16,$A4,$69,$37,$52,$C2,$0E,$50,$AB,$DC,$58
	; .db $89,$B0,$41,$EC,$0E,$23,$25,$A2,$05,$B1,$3B,$9C,$D8,$BA,$46,$8A
	; .db $E8,$69,$81,$16,$34,$EE,$7A,$23,$40,$71,$D2,$A4,$AF,$2D,$67,$26
	; .db $49,$D7,$DE,$37,$42,$D8,$04,$C3,$64,$5D,$73,$65,$E2,$2C,$D5,$75
	; .db $39,$99,$89,$3A,$E4,$E6,$E5,$62,$A6,$C2,$91,$87,$57,$4C,$98,$1A
	; .db $57,$1E,$1E,$33,$63,$EC,$5C,$DB,$B8,$2D,$45,$B1,$19,$4C,$97,$B2
	; .db $10,$C7,$66,$D0,$D5,$52,$D2,$99,$A8,$41,$35,$89,$08,$A7,$2B,$3C
	; .db $D9,$35,$34,$1D,$8D,$18,$C5,$10,$2B,$0F,$B8,$64,$E4,$93,$3D,$B3
	; .db $A0,$22,$17,$1E,$92,$B5,$90,$82,$5C,$30,$C5,$3B,$32,$36,$3A,$6D
	; .db $74,$13,$2D,$5B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF
; spx_xxx

sps_maxintruder
	.db $FF,$FF
spx_maxintruder

sps_maxdestint
	.db $FF,$FF
spx_maxdestint

sps_maxdestroy
	.db $FF,$FF
spx_maxdestroy

sps_maxhaha1
	.db $FF,$FF
spx_maxhaha1

sps_maxhaha2
	.db $FF,$FF
spx_maxhaha2

sps_maxhaha3
	.db $FF,$FF
spx_maxhaha3

sps_maxwewin
	.db $FF,$FF
spx_maxwewin

sps_maxyoulose
	.db $FF,$FF
spx_maxyoulose

sps_maxattack
	.db $FF,$FF
spx_maxattack

sps_maxcannot
	;youcannot_F100
	.db $00,$C0,$E6,$80,$35,$EB,$5A,$B0,$9B,$33,$94,$AC,$AB,$40,$61
	.db $4E,$5C,$AC,$3F,$03,$85,$D9,$29,$A9,$3B,$0F,$14,$66,$61,$B3,$B7
	.db $A8,$A4,$99,$49,$D9,$0C,$A3,$51,$6C,$C6,$ED,$4C,$43,$06,$A2,$19
	.db $F8,$8C,$F0,$68,$8C,$66,$CA,$DD,$CB,$55,$D2,$00,$66,$75,$2B,$40
	.db $D7,$6A,$03,$E8,$91,$65,$CD,$59,$6B,$4F,$B0,$D5,$31,$87,$10,$9B
	.db $29,$52,$C7,$1C,$5D,$4C,$B9,$09,$6D,$73,$F0,$D9,$99,$4E,$B4,$CC
	.db $49,$58,$FB,$A8,$50,$37,$53,$9F,$13,$61,$4A,$CC,$4C,$63,$AE,$87
	.db $3A,$0E,$B3,$D6,$31,$D6,$2A,$BB,$CC,$21,$7A,$C5,$98,$E0,$36,$87
	.db $50,$6E,$A5,$66,$CA,$1C,$E2,$A4,$A4,$99,$49,$73,$A8,$65,$DA,$62
	.db $3A,$CD,$21,$4F,$70,$59,$E0,$30,$FB,$D9,$A6,$E7,$81,$D5,$EC,$5E
	.db $88,$9E,$85,$46,$B3,$5D,$A5,$5C,$2A,$0A,$CD,$36,$8D,$B2,$A8,$4C
	.db $34,$FB,$38,$AA,$2A,$12,$D9,$4C,$9F,$07,$C4,$44,$52,$B3,$78,$6A
	.db $9C,$1B,$CA,$CD,$6A,$97,$58,$79,$C1,$32,$3B,$37,$91,$CD,$A6,$CA
	.db $1C,$74,$75,$0F,$89,$4D,$B3,$73,$35,$5D,$4C,$AE,$CC,$5E,$D7,$74
	.db $29,$B1,$34,$7B,$D7,$DD,$A9,$E4,$D2,$6C,$73,$57,$87,$42,$0B,$B3
	.db $73,$BD,$A5,$4A,$4D,$CC,$34,$F4,$58,$8A,$1C,$36,$93,$54,$CB,$62
	.db $A9,$C8,$4C,$42,$2D,$73,$28,$26,$33,$CA,$B9,$C2,$EE,$88,$CC,$24
	.db $67,$9B,$B8,$62,$35,$3B,$3B,$13,$6A,$42,$D4,$9C,$84,$B7,$B9,$0B
	.db $53,$73,$74,$5D,$96,$6A,$9D,$CC,$51,$75,$5B,$69,$3C,$34,$7B,$DB
	.db $E1,$A1,$4E,$FF,$FF
spx_maxcannot

sps_maxwillnot
	;youwillnot_F100
	.db $00,$08,$A8,$84,$54,$CC,$09,$EA,$D4,$A8,$30,$33,$07,$EC,$93
	.db $6B,$44,$DC,$EC,$49,$74,$9D,$13,$0E,$73,$E4,$5C,$BD,$15,$28,$CD
	.db $51,$63,$EF,$44,$21,$37,$73,$17,$BD,$09,$B1,$C4,$8C,$4F,$BA,$A9
	.db $0F,$22,$33,$FA,$22,$16,$55,$91,$CC,$E8,$1B,$8B,$47,$63,$32,$D3
	.db $CF,$A2,$31,$95,$C5,$4C,$1F,$73,$54,$97,$08,$33,$EF,$69,$5E,$31
	.db $A1,$CC,$3A,$46,$4D,$46,$84,$32,$AB,$EC,$D3,$95,$06,$CB,$AC,$6A
	.db $74,$75,$06,$6C,$B3,$09,$55,$11,$51,$A1,$CC,$BA,$75,$72,$56,$C9
	.db $34,$EB,$3A,$05,$39,$15,$C2,$6C,$FD,$2C,$78,$CF,$08,$B3,$2E,$33
	.db $68,$B3,$C0,$CC,$A2,$4F,$E0,$54,$68,$33,$F3,$B4,$CD,$9E,$51,$DD
	.db $CC,$7D,$8F,$7A,$44,$2D,$B3,$5D,$A5,$B9,$66,$BA,$CC,$7E,$56,$CA
	.db $A4,$E1,$32,$87,$3A,$C1,$5D,$81,$CA,$1C,$CA,$24,$77,$05,$4E,$73
	.db $68,$E5,$D2,$69,$B8,$CD,$A1,$4C,$68,$65,$C8,$30,$87,$16,$69,$33
	.db $83,$C8,$EC,$87,$59,$8F,$91,$46,$B3,$2E,$AD,$C4,$2A,$02,$CD,$22
	.db $A5,$88,$8A,$08,$34,$D3,$52,$C6,$E1,$B2,$D0,$8C,$6F,$AA,$98,$4B
	.db $22,$33,$FD,$AA,$6C,$53,$59,$CC,$F2,$09,$C7,$79,$40,$37,$AB,$23
	.db $E1,$95,$86,$D3,$EC,$5C,$65,$25,$07,$49,$73,$30,$36,$55,$2A,$34
	.db $CD,$C1,$78,$4F,$2B,$D1,$30,$7B,$5D,$D5,$2D,$44,$D3,$1C,$74,$4D
	.db $A7,$0A,$4D,$73,$50,$35,$5D,$2E,$38,$CC,$5E,$D7,$54,$88,$50,$33
	.db $7B,$E5,$E3,$1D,$44,$C8,$8C,$72,$8F,$A8,$3B,$21,$33,$EC,$D5,$A6
	.db $2A,$97,$CC,$A8,$F4,$AA,$1A,$5D,$32,$93,$54,$A7,$62,$AA,$D1,$4C
	.db $6A,$AF,$B2,$4B,$13,$B3,$B5,$5B,$39,$69,$48,$CD,$41,$C7,$58,$85
	.db $10,$31,$47,$ED,$29,$ED,$82,$D9,$1C,$4C,$26,$97,$8A,$46,$B3,$F7
	.db $D6,$E5,$E2,$10,$CD,$51,$45,$84,$5B,$FF,$FF
spx_maxwillnot

sps_maxbeback
	.db $FF,$FF
spx_maxbeback

sps_maxahhhh
	.db $FF,$FF
spx_maxahhhh

sps_maxohhhh
	.db $FF,$FF
spx_maxohhhh

sps_maxeeeeww
	.db $FF,$FF
spx_maxeeeeww


sps_maxactivate
	;jessactivated_S1.90.bin
	.db $00,$00,$05,$A8,$81,$BD,$BC,$39,$68,$A7,$A7,$91,$76,$A5,$68
	.db $BD,$16,$41,$D2,$5D,$BC,$5A,$E9,$84,$46,$5F,$0A,$EA,$13,$61,$08
	.db $FD,$BE,$54,$AC,$B0,$21,$F4,$47,$6B,$E1,$22,$94,$00,$02,$D0,$A4
	.db $70,$FA,$A3,$F2,$EC,$8E,$50,$EA,$D7,$C1,$A7,$2C,$02,$B1,$9F,$E7
	.db $AC,$52,$0B,$45,$7E,$19,$62,$4A,$3C,$92,$F8,$4D,$B0,$AA,$A8,$40
	.db $EE,$77,$C1,$B7,$2B,$85,$86,$DF,$3B,$DB,$9A,$24,$EB,$FE,$60,$74
	.db $73,$83,$8C,$F9,$9D,$C9,$9E,$0E,$A2,$E8,$37,$AA,$72,$D2,$0D,$43
	.db $02,$86,$10,$11,$BF,$4C,$55,$3B,$41,$8C,$FD,$DA,$56,$6F,$06,$50
	.db $FF,$FF
	
	;jessactivated_S1.50.bin
	; .db $00,$00,$05,$A8,$81,$A3,$6D,$39,$68,$67,$A4,$D1,$B6,$A4,$68
	; .db $BD,$16,$41,$CB,$5D,$BC,$5A,$E9,$44,$26,$71,$8A,$EA,$93,$59,$1A
	; .db $FD,$A1,$54,$5E,$58,$64,$F4,$27,$17,$5D,$19,$76,$D0,$12,$63,$44
	; .db $A5,$DB,$81,$00,$4C,$EF,$5C,$8E,$A8,$3C,$BB,$23,$94,$B9,$75,$F0
	; .db $69,$8F,$40,$E2,$E3,$A5,$AA,$CD,$42,$91,$5F,$86,$98,$54,$8F,$25
	; .db $7E,$13,$AC,$2B,$AA,$50,$F8,$6C,$F0,$ED,$4A,$A1,$E1,$F3,$CE,$B7
	; .db $26,$C5,$86,$3F,$18,$DD,$DA,$A0,$EA,$7E,$67,$6B,$A6,$53,$28,$FA
	; .db $8C,$AA,$9C,$0A,$23,$90,$80,$21,$44,$D4,$2F,$73,$D5,$4E,$10,$13
	; .db $BF,$76,$D5,$5B,$49,$14,$FF,$FF
spx_maxactivate

sps_maxhaha
	;jessha_S1.90.bin
	.db $00,$00,$CA,$28,$AE,$AB,$23,$A2,$94,$31,$9B,$4A,$57,$2B,$D3
	.db $8E,$E8,$BB,$DC,$7D,$74,$3B,$63,$AE,$F2,$C8,$D1,$E5,$08,$DB,$D2
	.db $BA,$8C,$A6,$2B,$F4,$4A,$F1,$28,$9A,$BE,$E4,$7C,$55,$3C,$8A,$01
	.db $9A,$B0,$24,$42,$B6,$35,$6E,$19,$15,$00,$10,$D0,$8C,$B7,$B1,$43
	.db $8B,$E0,$88,$D0,$E9,$0F,$B5,$DA,$BC,$4A,$95,$2F,$CC,$28,$AD,$09
	.db $9D,$7E,$BF,$B3,$39,$CB,$A8,$FB,$43,$AD,$10,$F3,$20,$EE,$2F,$3A
	.db $CA,$85,$4A,$B3,$BF,$E8,$EC,$54,$2B,$0D,$00,$E8,$2F,$2E,$CA,$CC
	.db $8D,$28,$C7,$97,$70,$8D,$30,$1B,$BE,$50,$BA,$CD,$BD,$54,$FA,$43
	.db $EE,$32,$8F,$49,$E9,$0F,$B5,$46,$BD,$4B,$B9,$DF,$9F,$4C,$A9,$34
	.db $6A,$FE,$D0,$32,$28,$5C,$2C,$FB,$93,$AF,$14,$11,$A1,$68,$4C,$A1
	.db $5C,$3C,$03,$03,$00,$10,$A0,$DA,$68,$F3,$87,$6D,$EE,$AD,$62,$C3
	.db $1F,$76,$84,$75,$1A,$0D,$7F,$D8,$99,$5A,$65,$D8,$FC,$A1,$74,$8B
	.db $78,$10,$F5,$C7,$54,$C1,$E6,$41,$D0,$12,$5D,$B7,$89,$05,$05,$00
	.db $00,$56,$17,$57,$6D,$EE,$E1,$CC,$1F,$52,$15,$7B,$04,$71,$BF,$BF
	.db $E6,$51,$2E,$D4,$FD,$FE,$7A,$58,$A7,$30,$F5,$FB,$5D,$21,$E1,$C1
	.db $D4,$1F,$5A,$BB,$A4,$1B,$16,$7F,$88,$15,$60,$1A,$56,$FC,$C9,$78
	.db $32,$7B,$61,$F1,$67,$1B,$C5,$A2,$66,$10,$50,$B9,$17,$FF,$FF
spx_maxhaha

sps_maxuggh
	;jess2_uggh_S0.90.bin
    .db $00,$AA,$90,$C3,$52,$2A,$2E,$B8,$5D,$57,$2B,$CE,$B8,$E8
	.db $76,$59,$7D,$38,$FC,$82,$DB,$F9,$B2,$C2,$C8,$82,$EE,$E0,$4F,$C3
	.db $26,$02,$86,$9D,$6F,$2F,$4A,$2F,$69,$76,$79,$24,$75,$B3,$A0,$DB
	.db $C5,$D6,$A2,$8D,$01,$6E,$97,$5B,$52,$26,$07,$86,$43,$4D,$2B,$AE
	.db $1C,$18,$4E,$BB,$C4,$64,$72,$A4,$39,$C5,$D3,$E4,$C9,$82,$E2,$F0
	.db $CD,$1B,$D5,$47,$13,$4D,$0C,$30,$EB,$08,$8D,$68,$30,$48,$BB,$DD
	.db $22,$FF,$FF
spx_maxuggh

; ;sps_indy_6C
; sps_maxback
		; .db $AD,$4F,$A6,$C6,$5D,$DA,$8C,$AE,$18,$9F,$74,$69,$3B,$DA,$62,$7D
		; .db $C2,$A4,$ED,$68,$AA,$D3,$31,$97,$B6,$A3,$6E,$4E,$C6,$42,$D6,$8D
		; .db $AA,$39,$69,$73,$6D,$DB,$EA,$E6,$79,$D4,$69,$6B,$6B,$9B,$A7,$B6
		; .db $C4,$6E,$AD,$EB,$11,$CB,$1A,$DA,$B5,$6E,$24,$2A,$6B,$70,$D7,$FA
		; .db $E6,$B1,$2C,$C9,$5B,$EB,$9B,$C5,$0A,$67,$EF,$A5,$AF,$06,$33,$92
		; .db $DD,$A7,$BE,$48,$CA,$4C,$4A,$9F,$86,$AA,$C8,$B3,$28,$7D,$18,$AB
		; .db $92,$34,$07,$E7,$6E,$AA,$9A,$C2,$02,$9C,$8B,$29,$71,$09,$13,$75
		; .db $2B,$66,$CF,$35,$53,$2D,$0D,$5A,$3C,$B5,$88,$08,$27,$6D,$E8,$DE
		; .db $1C,$82,$BA,$8D,$A1,$27,$37,$0C,$6C,$D7,$86,$1E,$CD,$30,$70,$19
		; .db $9A,$8A,$33,$C3,$C0,$B6,$78,$2A,$43,$B6,$62,$D2,$D4,$B6,$0D,$D9
		; .db $58,$70,$C9,$DA,$D6,$17,$13,$C9,$A9,$EB,$DA,$54,$AD,$97,$25,$B7
		; .db $6B,$53,$71,$D6,$96,$D2,$AE,$8D,$C5,$E9,$68,$69,$BB,$36,$15,$AF
		; .db $A3,$A9,$6D,$DB,$92,$A2,$0D,$87,$B7,$2D,$5B,$CC,$D6,$12,$D9,$06
		; .db $00,$01,$31,$04,$21,$20,$B9,$E4,$07,$FF,$FF
; spx_maxback

; sps_ughlow
; ;ug1_S35
	; .db $00,$00,$00
	; .db $8E,$E3,$3D,$4A,$23,$07,$25,$96,$97,$2A,$33,$1B
	; .db $98,$22,$99,$A2,$D4,$F2,$44,$62,$45,$CE,$52,$F7,$83,$89,$E5,$23
	; .db $4C,$2B,$07,$26,$8E,$4F,$0F,$A9,$1E,$98,$38,$D9,$22,$39,$FB,$60
	; .db $10,$45,$ED,$E4,$F4,$43,$4E,$16,$B3,$8C,$37,$06,$19,$8D,$DF,$70
	; .db $E9,$1C,$A0,$4C,$FE,$C5,$B2,$CB,$20,$73,$F8,$0F,$A7,$CC,$C0,$C4
	; .db $E5,$C7,$D4,$C6,$0D,$32,$93,$8F,$32,$2A,$2F,$44,$18,$E9,$AA,$D8
	; .db $F4,$24,$32,$7C,$4D,$C1,$CC,$41,$28,$E0,$5D,$45,$2B,$4C,$21,$8E
	; .db $FB,$10,$34,$AD,$8C,$9C,$30,$8D,$38,$34,$31,$00,$FF,$FF
; spx_ughlow

; sps_ughmid
; ;ug1_S30
	; .db $00,$00,$00
	; .db $8E,$E2,$3D,$4A,$23,$07,$25,$92,$97,$2A,$33,$1B
	; .db $98,$3C,$99,$A2,$D4,$F2,$44,$22,$45,$CE,$52,$F7,$83,$89,$E4,$23
	; .db $4C,$2B,$07,$26,$8A,$4F,$0F,$A9,$1E,$98,$28,$D9,$22,$39,$FB,$60
	; .db $60,$44,$ED,$E4,$F4,$43,$8E,$17,$B3,$8C,$37,$06,$19,$81,$DF,$70
	; .db $E9,$1C,$A0,$64,$FE,$C5,$B2,$CB,$20,$D3,$F8,$0F,$A7,$CC,$C0,$C4
	; .db $E4,$C7,$D4,$C6,$0D,$32,$99,$8F,$32,$2A,$2F,$44,$08,$E9,$AA,$D8
	; .db $F4,$24,$92,$7C,$4D,$C1,$CC,$41,$C8,$E1,$5D,$45,$2B,$4C,$21,$8A
	; .db $FB,$10,$34,$AD,$8C,$B4,$30,$8D,$38,$34,$31,$00,$FF,$FF
; spx_ughmid

; sps_ughhig
; ;ug1_S25
	; .db $00,$00,$00,
	; .db $0E,$E3,$3D,$4A,$23,$07,$25,$94,$97,$2A,$33,$1B
	; .db $98,$0C,$99,$A2,$D4,$F2,$44,$42,$45,$CE,$52,$F7,$83,$09,$E5,$23
	; .db $4C,$2B,$07,$26,$8C,$4F,$0F,$A9,$1E,$98,$30,$D9,$22,$39,$FB,$60
	; .db $20,$44,$ED,$E4,$F4,$43,$8E,$12,$B3,$8C,$37,$06,$19,$9A,$DF,$70
	; .db $E9,$1C,$A0,$38,$FE,$C5,$B2,$CB,$20,$13,$F8,$0F,$A7,$CC,$C0,$44
	; .db $E2,$C7,$D4,$C6,$0D,$32,$8E,$8F,$32,$2A,$2F,$44,$10,$E9,$AA,$D8
	; .db $F4,$24,$62,$7D,$4D,$C1,$CC,$41,$48,$E3,$5D,$45,$2B,$4C,$21,$8C
	; .db $FB,$10,$34,$AD,$8C,$84,$30,$8D,$38,$34,$31,$00,$FF,$FF
; spx_ughhig

; sps_suffocate
	; ;ug4_S0.40.wav
	; .db $C4,$A5,$3E,$37,$30,$9D,$08,$82,$A6,$AA,$34
	; .db $2B,$E9,$3C,$9E,$32,$A2,$AA,$44,$20,$79,$9C,$F4,$8A,$06,$C5,$A7
	; .db $79,$2A,$22,$6A,$16,$9F,$E6,$EE,$F4,$98,$D9,$7C,$EE,$6B,$D2,$7D
	; .db $41,$0B,$78,$EA,$B2,$8A,$09,$23,$D4,$BA,$CB,$D2,$17,$0E,$46,$9B
	; .db $4E,$A9,$A8,$D9,$78,$9E,$B6,$24,$2D,$62,$13,$79,$9B,$B6,$D0,$92
	; .db $2D,$E7,$65,$CB,$D2,$2B,$16,$4D,$A4,$4A,$0F,$1F,$98,$0C,$DE,$2B
	; .db $AC,$3C,$50,$31,$F9,$6C,$B7,$B6,$00,$C9,$E2,$AB,$D2,$D2,$0B,$16
	; .db $9B,$8F,$49,$4B,$2B,$11,$1C,$3A,$AB,$34,$A2,$50,$72,$79,$E9,$B4
	; .db $F4,$82,$C1,$E5,$AD,$5A,$35,$03,$06,$8F,$8F,$4C,$8D,$0C,$1C,$7C
	; .db $DE,$BA,$55,$A3,$A0,$F3,$E9,$C8,$D0,$28,$43,$2E,$A0,$A3,$5D,$3D
	; .db $0D,$B9,$80,$F6,$4A,$F1,$32,$60,$22,$DA,$BA,$C4,$D2,$80,$89,$68
	; .db $99,$16,$F6,$10,$2A,$A2,$A5,$97,$C8,$42,$AA,$88,$D4,$2E,$52,$0F
	; .db $A9,$22,$DA,$CA,$35,$53,$A0,$88,$C9,$A8,$46,$09,$83,$22,$C6,$3D
	; .db $83,$23,$05,$8A,$98,$E4,$2A,$D6,$30,$C0,$12,$14,$AB,$C8,$C2,$00
	; .db $49,$70,$0C,$E3,$8D,$00,$28,$01,$A3,$1C,$C3,$0D,$A1,$84,$EA,$74
	; .db $98,$A8,$88,$62,$E0,$CC,$31,$A7,$24,$8A,$A1,$8A,$66,$8B,$01,$28
	; .db $42,$36,$9C,$BC,$22,$A2,$08,$C4,$2A,$B2,$34,$88,$62,$60,$33,$29
	; .db $AA,$14,$A0,$94,$CB,$18,$65,$A9,$08,$FF,$FF
; spx_suffocate



;The Force will be with you!
sps_sw1	
	.db $04,$30,$3A,$BC,$2D,$D1,$17,$AB,$6E,$91,$B2,$07,$95,$A2,$1E
	.db $49,$14,$60,$94,$AB,$02,$B2,$37,$33,$40,$4A,$65,$06,$60,$5B,$7C
	.db $F5,$7E,$99,$68,$76,$E5,$35,$F8,$1E,$4E,$52,$93,$D7,$E0,$5B,$3A
	.db $4B,$4C,$59,$A3,$AF,$E9,$AC,$31,$65,$CD,$61,$44,$90,$D6,$94,$B5
	.db $C5,$E0,$4D,$5C,$95,$C7,$50,$C5,$BA,$A8,$C5,$4E,$C0,$02,$96,$09
	.db $98,$24,$2B,$00,$55,$54,$29,$20,$8B,$52,$01,$24,$5E,$5A,$1A,$E7
	.db $5B,$C5,$AA,$76,$9B,$43,$F4,$60,$AE,$C6,$6D,$09,$C9,$13,$B9,$1B
	.db $97,$D5,$67,$37,$96,$AA,$5D,$56,$CD,$2B,$D2,$2C,$71,$D8,$3D,$8B
	.db $CC,$F0,$24,$6D,$0A,$5E,$DC,$2A,$6B,$8F,$35,$38,$4E,$AF,$AC,$D5
	.db $56,$1F,$39,$BC,$AA,$56,$59,$7D,$B0,$50,$8D,$46,$69,$37,$2E,$CC
	.db $C5,$1A,$97,$D5,$67,$33,$95,$6C,$DC,$16,$9F,$25,$34,$AB,$F6,$58
	.db $63,$A2,$B0,$9A,$C8,$63,$0B,$91,$AD,$62,$AC,$94,$C3,$EB,$D0,$88
	.db $B2,$9D,$D6,$2C,$42,$3C,$2C,$4E,$F9,$82,$08,$8B,$2C,$DB,$0A,$80
	.db $35,$B3,$FC,$DE,$4A,$74,$A5,$25,$03,$D0,$62,$5D,$5E,$13,$3C,$33
	.db $2A,$52,$FA,$BC,$0D,$77,$CF,$84,$E1,$0F,$CA,$DD,$D5,$5B,$31,$40
	.db $45,$13,$06,$98,$E2,$42,$00,$A3,$C3,$E0,$01,$FF,$FF
spx_sw1

sps_sw2
    .db $AE,$8F,$D4,$47,$CD,$9C,$26,$4B,$C6,$49,$8A,$4E,$DD,$D4,$60,$A7
    .db $28,$DA,$F1,$50,$82,$ED,$E0,$AC,$28,$46,$89,$AC,$2A,$DC,$12,$2B
    .db $25,$D2,$AC,$50,$B7,$6D,$80,$52,$58,$87,$DE,$56,$08,$6B,$38,$39
    .db $5E,$11,$E2,$E9,$45,$B9,$04,$89,$B0,$59,$94,$15,$30,$7E,$A2,$A2
    .db $9E,$25,$35,$05,$CD,$2B,$8B,$46,$9C,$E1,$27,$2D,$AE,$35,$32,$4F
    .db $50,$94,$84,$D6,$28,$5C,$61,$A6,$EA,$A9,$2D,$69,$45,$83,$1B,$B3
    .db $44,$92,$11,$2E,$65,$2C,$92,$89,$47,$B8,$DC,$B8,$91,$D8,$0E,$C0
    .db $14,$CC,$01,$98,$9C,$38,$01,$47,$39,$15,$60,$48,$93,$A5,$54,$65
    .db $AE,$9E,$8A,$8F,$9C,$6C,$BA,$66,$2A,$3A,$72,$36,$5E,$E2,$66,$F3
    .db $88,$D5,$C5,$48,$9A,$ED,$23,$34,$17,$4D,$29,$B2,$0F,$DB,$9D,$17
    .db $A7,$3B,$39,$74,$F7,$11,$34,$1E,$FB,$D0,$AD,$9A,$4B,$77,$EC,$43
    .db $F7,$64,$26,$D9,$AD,$0E,$DB,$AD,$98,$5D,$C7,$3A,$C2,$70,$A2,$D8
    .db $69,$6B,$98,$8B,$27,$B3,$55,$AD,$E5,$6F,$6E,$4C,$14,$89,$57,$B4
    .db $A8,$B2,$4A,$C4,$3E,$41,$57,$62,$CA,$E9,$E8,$44,$4D,$71,$72,$96
    .db $CD,$15,$67,$A1,$49,$DE,$B6,$18,$C0,$B4,$97,$02,$A8,$8D,$38,$69
    .db $F5,$6A,$D4,$ED,$F0,$E4,$CD,$49,$E8,$B4,$CD,$53,$94,$C2,$1E,$5B
    .db $B2,$4E,$DE,$AA,$89,$56,$39,$3C,$65,$B1,$16,$91,$63,$25,$E4,$55
    .db $9B,$59,$84,$68,$06,$48,$E5,$49,$00,$29,$23,$10,$20,$71,$98,$00
    .db $0A,$67,$31,$40,$31,$6A,$2B,$6C,$41,$8D,$74,$1D,$9E,$A8,$79,$33
    .db $F6,$8D,$79,$E2,$16,$D4,$A4,$26,$E1,$49,$7B,$31,$C3,$EA,$58,$27
    .db $EB,$49,$0D,$BB,$6C,$AD,$BC,$27,$16,$AD,$92,$38,$CA,$11,$48,$3D
    .db $43,$52,$AA,$9A,$41,$F7,$72,$8B,$A1,$A9,$9A,$CC,$D2,$93,$30,$40
    .db $48,$1B,$D7,$75,$23,$2A,$E6,$76,$08,$C0,$65,$D4,$03,$86,$34,$29
    .db $FF,$FF
spx_sw2

; sps_indy_01
		; .db $AD,$4F,$A6,$C6,$5D,$DA,$8C,$AE,$18,$9F,$74,$69,$3B,$DA,$62,$7D
		; .db $C2,$A4,$ED,$68,$AA,$D3,$31,$97,$B6,$A3,$6E,$4E,$C6,$42,$D6,$8D
		; .db $AA,$39,$69,$73,$6D,$DB,$EA,$E6,$79,$D4,$69,$6B,$6B,$9B,$A7,$B6
		; .db $C4,$6E,$AD,$EB,$11,$CB,$1A,$DA,$B5,$6E,$24,$2A,$6B,$70,$D7,$FA
		; .db $E6,$B1,$2C,$C9,$5B,$EB,$9B,$C5,$0A,$67,$EF,$A5,$AF,$06,$33,$92
		; .db $DD,$A7,$BE,$48,$CA,$4C,$4A,$9F,$86,$AA,$C8,$B3,$28,$7D,$18,$AB
		; .db $92,$34,$07,$E7,$6E,$AA,$9A,$C2,$02,$9C,$8B,$29,$71,$09,$13,$75
		; .db $2B,$66,$CF,$35,$53,$2D,$0D,$5A,$3C,$B5,$88,$08,$27,$6D,$E8,$DE
		; .db $1C,$82,$BA,$8D,$A1,$27,$37,$0C,$6C,$D7,$86,$1E,$CD,$30,$70,$19
		; .db $9A,$8A,$33,$C3,$C0,$B6,$78,$2A,$43,$B6,$62,$D2,$D4,$B6,$0D,$D9
		; .db $58,$70,$C9,$DA,$D6,$17,$13,$C9,$A9,$EB,$DA,$54,$AD,$97,$25,$B7
		; .db $6B,$53,$71,$D6,$96,$D2,$AE,$8D,$C5,$E9,$68,$69,$BB,$36,$15,$AF
		; .db $A3,$A9,$6D,$DB,$92,$A2,$0D,$87,$B7,$2D,$5B,$CC,$D6,$12,$D9,$06
		; .db $00,$01,$31,$04,$21,$20,$B9,$E4,$07,$FF,$FF
; spx_indy_01

sps_rexhurry
    .db $FF,$FF
spx_rexhurry


sps_rexcomeon
    .db $FF,$FF
spx_rexcomeon

sps_rexouttah
    .db $FF,$FF
spx_rexouttah

sps_rexooof
    .db $FF,$FF
spx_rexooof

sps_rexooom
    .db $FF,$FF
spx_rexooom

sps_rexouch
    .db $FF,$FF
spx_rexouch

sps_rexuggh
    .db $FF,$FF
spx_rexuggh

sps_rexuhoh
    .db $FF,$FF
spx_rexuhoh

sps_rexumhmm
    .db $FF,$FF
spx_rexumhmm

sps_rexummm
    .db $FF,$FF
spx_rexummm

sps_rexwhoa
    .db $FF,$FF
spx_rexwhoa		

sps_rexwoohoo
    .db $FF,$FF
spx_rexwoohoo


#ENDIF
