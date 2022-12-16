;************************************************
; tw_exports - for defining paged variables 
; and method exports
;************************************************
    .TEXT ".TWEXPORTS."
;************************************************

;set up table base pointers
___extpages = $
___extaddrs = ___extpages + ___exports_max

 .org ___extaddrs+(___exports_max*2)
 
 
;*******************************************************************************
;* Paged Export Macros
;*******************************************************************************
___exports_max = 128d
___exports_adr = 0
___exports_cur = 0

#DEFINE EXTERNAL(mname)	\___exports_adr .set $
#DEFCONT 						\ .org ___extpages + ___exports_cur
#DEFCONT  						\mname = ___exports_cur
#DEFCONT						\ .db ___ROMPAGE
#DEFCONT						\ .org (___extaddrs + (___exports_cur*2))
#DEFCONT						\ .dw ___exports_adr
#DEFCONT						\___exports_cur .set ___exports_cur+1
#DEFCONT						\ .export mname

#DEFINE EXTCALL(label_index)	\ lda #label_index
#DEFCONT						\ jsr ___callext


;1.	Push the Current Page to the stack (stack + 1)
;2.	Find the new page number (byte) by looking up value in Page table by index (byte)
;3.	Change to the new page number
;4.	Find the new physical address (word) by lookup up value in Address table by index (byte)
;5.	Push the return address of the External handler (stack + 2)
;6.	Push the physical address onto stack (stack + 2)
;7.	RTS to ‘jump’ to paged external function
;8.	External Function terminates with an RTS which returns back to the handler
;9.	Pull pushed old Page number off the stack
;10.	Save the page number
;11.	RTS from external handler 

; A contains the index of the ext method call
; Valid range is only 0-127 (128 maximum external calls)
___callext
	pha		;Save the index on the stack
	tax
	lda	currpage	
	cmp ___extpages,X			;Get the new page number	
	ifne
		;different page, we need to save the current page
		pha			;Current page was already in A... Yea!
		lda #((___callext_ret_diff&$ff00)/$100)
        pha 
        lda #(___callext_ret_diff&$ff)
		pha
	else
		;same page, we don't need to push the current page
		;just the return handler
		lda #((___callext_ret_same&$ff00)/$100)
        pha 
        lda #(___callext_ret_same&$ff)
        pha
	endif
	;get back our working index
	pla
	asl A 		;words
	tax
	;now push the external method address
	lda ___extaddrs+1,X
	pha
	lda ___extaddrs,X
	pha
	;X contains the index still
	rts
	
___callext_ret_diff
	;we originated on a different page, pull it and write it
	pla 
	sta rompg
___callext_ret_same
	;was the same page, so we don't need to do anything except return
	rts
	
.export ___callext