; 10/25/18 TinyLoad for protoZ80SBC, Z80SBCLoad
; Derived from TinyLoad for Z280
; default I/O location is 0xF8 for status, 0xF9 for data
; status bits are D[0] for transmit empty, D[1] for receive ready.  D[7..2] are 0xF8 which is high 6 bits 
;   of the interrupt vector
; write 0x4 to 0xf8 to enable transmit empty interrupt
; write 0x0 to 0xf8 to disable transmit empty interrupt
; serial port initially configured in boot strap mode.  This is the boot strap code
; clear all memory to zero
; file load program fit in 256 bytes
; File is intel HEX format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Z80SBCLoad, Copyright (C) 2018 Hui-chien Shen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RxData  	equ 0f9h        	; CPLD UART receive register
TxData  	equ 0f9h        	; CPLD UART transmit register
RxStat  	equ 0f8h        	; CPLD UART transmitter status/control register
TxStat  	equ 0f8h        	; CPLD UART receiver status/control register

	nop
          LD SP,0FFFFH  	; initialize stack pointer to top of memory
	ld hl,SignOn$	; print the sign on message
	call strout

	xor a			; clear reg A
	ld hl,0ffffh		; clear memory, starting from the top
clrmem:
	ld (hl),a			; clear memory
	dec hl
	cp h			; reaching 100h?
	jr nz,clrmem

main:
	call cinq
	cp ':'			; intel load file starts with :
	jr z,fileload
	cp 'G'			; execute 
	jr nz,main
	call cout		; echo back 'G'
	ld a,' '		; put out a space
	call cout
	call GETHEX	; get starting address
	ld h,a
	call GETHEX
	ld l,a
	jp (hl)
fileload:
;	call cout		; echo back valid input character 
	call GETHEX	; get two ASCII char (byte count) into hex byte in reg A
	ld d,a			; save byte count to reg D
	ld b,a			; initialize the checksum
	call GETHEX	; get MSB of address
	ld h,a			; HL points to memory to be loaded
	add a,b			; accumulating checksum
	ld b,a			; checksum is kept in reg B
	call GETHEX	; get LSB of address
	ld l,a
	add a,b			; accumulating checksum
	ld b,a			; checksum is kept in reg B
	call GETHEX	; get the record type, 0 is data, 1 is end
	cp 0
	jr z,filesave
	cp 1				; end of file transfer?
	jp nz,unknown	; if not, print a 'U'
; end of the file load
	call GETHEX	; flush the line, get the last byte
	ld a,'X'		; mark the end with 'X'
	call cout
	ld a,10			; carriage return and line feed
	call cout
	ld a,13
	call cout
	jp main
;	jp 200h

; the assumption is the data is good and will be saved to the destination memory
filesave:
	add a,b			; accumulating checksum of record type
	ld b,a			; checksum is kept in reg B
filesav1:
	call GETHEX	; get a byte
	ld (hl),a		; save to destination
	add a,b			; accumulating checksum
	ld b,a			; checksum is kept in reg B
	inc hl
	dec d
	jr nz,filesav1
	call GETHEX	; get the checksum
	neg a			; 2's complement
	cp b				; compare to checksum accumulated in reg B
	jr nz,badload	; checksum not match, put '?'
	ld a,'.'		; checksum match, put '.'
filesav2:
	call cout
	jp main			; repeat until record end
badload:
	ld a,'?'		; checksum not match, put '?'
	jr filesav2
unknown:
	ld a,'U'		; put out a 'U' and wait for next record
	call cout
	jp main





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GETHEX -- Get byte from console as hex
;
;pre: none
;post: A register contains byte from hex input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GETHEX:
	push de		; save register 
        	CALL cinq
        	CALL ASCHEX
        	RLCA
        	RLCA
        	RLCA
        	RLCA
        	LD D,A
        	CALL cinq
        	CALL ASCHEX
        	OR D 
  	pop de			;restore register
        	RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ASCHEX -- Convert ASCII coded hex to nybble
;
;pre: A register contains ASCII coded nybble
;post: A register contains nybble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ASCHEX: 	SUB 30h
        	CP 0Ah
        	RET M
        	AND 5Fh
        	SUB 07h
        	RET
; string output
; output string pointed by HL until null terminator
strout:
	ld a,(hl)		;get next character
	cp 0		;null terminator?
	ret z		;return if null terminator
	call cout		; put out a character
	inc hl		; next char in the string
	jr strout
; output char in reg A
cout:
	push af		; save reg
cout1:
	in a,(0f8h)	; read status
	and 1		; TxEmpty is D[0]
	jr z,cout1	; wait for TxEmpty asserted
	pop af
	out (0f9h),a	; put out character
	ret
; input char into reg A
cin:
	in a,(0f8h)	;read status
	and 2		;data ready flag is in D[1]
	jr z,cin
	in a,(0f9h)	; read data
	call cout		; echo back
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CINQ -- Get a char from the console and no echo
;
;pre: console device is initialized
;post: received char is in A register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cinq:    
	in a,(0f8h)	;read status
	and 2		;data ready flag is in D[1]
	jr z,cinq
	in a,(0f9h)	; read data
	ret
        	RET


SignOn$: 	db 0ah,0dh,"Z80SBC Loader v0.4",0ah,0dh
	db "G xxxx to run",0ah,0dh,0

	db 0,0,0,0,0,0,0,0,0,0,0,0,0	; pad to exactly 256 byte
	




