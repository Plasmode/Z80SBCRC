; Basic monitor for Z80SBC based on ProtoRC rev 1
; 10/25/18 fork from ZZ80Mon
;  L ist memory in Intel Hex format
;  I in from port # in I/O page 0
;  O out to port # in I/O page 0
;  Display the ASCII characters along with HEX value
;  show the correct memory address with 'R' command

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ZMon, Copyright (C) 2018 Hui-chien Shen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RxData  	equ 0f9h        	; CPLD UART receive register
TxData  	equ 0f9h        	; CPLD UART transmit register
RxStat  	equ 0f8h        	; CPLD UART transmitter status/control register
TxStat  	equ 0f8h        	; CPLD UART receiver status/control register

CFdata   	equ 0C0h    	;CF data register
CFerr    	equ 0C1h    	;CF error reg
CFsectcnt equ 0C2h    	;CF sector count reg
CF07     	equ 0C3h   	;CF LA0-7
CF815    	equ 0C4h       	;CF LA8-15
CF1623   	equ 0C5h       	;CF LA16-23
CF2427   	equ 0C6h       	;CF LA24-27
CFstat   	equ 0C7h       	;CF status/command reg

	ORG 0b000h
	ld hl,400h	; ZMon is stored in 0x400 to 0xFFF
	ld de,0b400h	; destination where ZMon will run
	ld bc,0c00h	; copy 3K of program
	ldir
	jp 0b400h		; jump into ZMon

	ORG 0b400H
; This part of the program will be relocated to 0x400 post process		
	jr start
; variable area
testseed: ds 2		; RAM test seed value
addr3116	ds 2		; high address for Intel Hex format 4
RDsector	ds 1		; current RAM disk sector
RDtrack	ds 1		; current RAM disk track 
RDaddr	ds 2		; current RAM disk address 
;Initialization and sign-on message
start:
	ld sp,0bfffh	; initialize stack 

    	LD HL,signon$
        	CALL strout

	call readbsy	;;8 wait until busy flag is cleared
	ld a,0e0h		;;8 set up LBA mode
	out (CF2427),a	;;8
	ld a,1		;;8 set feature to 8-bit interface
	out (CFerr),a	;;8
	ld a,0efh		;;8 set feature command
	out (CFstat),a	;;8
	call readbsy	;;8 wait until busy flag is cleared




	ld hl,251		; initialize RAM test seed value
	ld (testseed),hl	; save it
clrRx:  
        	IN A,(RxStat)	; read on-chip UART receive status
        	AND 2				; data available?
        	jp z,CMD
        	IN A,(RxData)	; read clear the input buffer
	jr clrRx
;Main command loop
CMD:  	LD HL, PROMPT$
        	CALL strout
CMDLP1:
        	CALL cinq
	cp ':'		; Is this Intel load file?
	jp z,initload
	cp 0ah		; ignore line feed
	jp z,CMDLP1
	cp 0dh		; carriage return get a new prompt
	jp z,CMD
	CALL cout		; echo character
        	AND 5Fh
	cp 'H'		; help command
	jp z,HELP
        	CP A,'D'
        	JP Z,MEMDMP
        	CP A,'E'
        	JP Z,EDMEM
	cp a,'I'		; read data from specified I/O port in page 0
	jp z,INPORT
	cp a,'O'		; write data to specified I/O port in page 0
	jp z,OUTPORT
	cp a,'L'		; list memory as Intel Hex format
	jp z,LISTHEX
        	CP A,'G'
        	JP Z,go
	cp a,'R'		; read a CF sector
	jp z,READRD
	cp a,'Z'		; fill memory with zeros
	jp z,fillZ
	cp a,'F'		; fill memory with ff
	jp z,fillF
	cp a,'C'		; Copy to CF	
	jp z,COPYCF
	cp a,'T'		; testing RAM 
	jp z,TESTRAM
	cp 'B'		; boot CPM
	jp z,BootCPM
	cp 'X'		; clear RAMdisk directory at 0x80000
	jp z,format
what:
        	LD HL, what$
        	CALL strout
        	JP CMD
abort:
	ld hl,abort$	; print command not executed
	call strout
	jp CMD
; initialize for file load operation
initload:
	ld hl,0		; clear the high address in preparation for file load
	ld (addr3116),hl	; addr3116 modified with Intel Hex format 4 
; load Intel file
fileload:
	call GETHEXQ	; get two ASCII char (byte count) into hex byte in reg A
	ld d,a		; save byte count to reg D
	ld c,a		; save copy of byte count to reg C
	ld b,a		; initialize the checksum
	call GETHEXQ	; get MSB of address
	ld h,a		; HL points to memory to be loaded
	add a,b		; accumulating checksum
	ld b,a		; checksum is kept in reg B
	call GETHEXQ	; get LSB of address
	ld l,a
	add a,b		; accumulating checksum
	ld b,a		; checksum is kept in reg B
	call GETHEXQ	; get the record type, 0 is data, 1 is end
	cp 0
	jp z,filesave
	cp 1		; end of file transfer?
	jp z,fileend
	cp 4		; Extended linear address?
	jp nz,unknown	; if not, print a 'U'
; Extended linear address for greater than 64K
; this is where addr3116 is modified
	add a,b		; accumulating checksum of record type
	ld b,a		; checksum is kept in reg B
	ld a,d		; byte count should always be 2
	cp 2
	jp nz,unknown
	call GETHEXQ	; get first byte (MSB) of high address
	ld (addr3116+1),a	; save to addr3116+1
	add a,b		; accumulating checksum
	ld b,a		; checksum is kept in reg B
; Little Endian format.  MSB in addr3116+1, LSB in addr3116
	call GETHEXQ	; get the 2nd byte (LSB) of of high address
	ld (addr3116),a	; save to addr3116
	add a,b		; accumulating checksum
	ld b,a		; checksum is kept in reg B
	call GETHEXQ	; get the checksum
	neg a		; 2's complement
	cp b		; compare to checksum accumulated in reg B
	jp nz,badload	; checksum not match, put '?'
	ld a,'E'		; denote a successful Extended linear addr update
	jp filesav2
; end of the file load
fileend:
	call GETHEXQ	; flush the line, get the last byte
	ld a,'X'		; mark the end with 'X'
	call cout
	ld a,10			; carriage return and line feed
	call cout
	ld a,13
	call cout
	jp CMD
; the assumption is the data is good and will be saved to the destination memory
filesave:
	add a,b		; accumulating checksum of record type
	ld b,a		; checksum is kept in reg B
	ld ix,0c000h	; 0c000h is buffer for incoming data
filesavx:
	call GETHEXQ	; get a byte
;Z80SBC	ld (ix),a		; save to buffer
	ld (hl),a		;Z80SBC save data to destination
	add a,b		; accumulating checksum
	ld b,a		; checksum is kept in reg B
	inc ix
	inc hl		;Z80SBC 
	dec d
	jp nz,filesavx
	call GETHEXQ	; get the checksum
	neg a		; 2's complement
	cp b		; compare to checksum accumulated in reg B
	jp nz,badload	; checksum not match, put '?'
;	call DMAPage	; set page i/o reg to DMA
; use DMA to put data from buffer to location pointed by ehl
;	push hl		; destination RAM in hl, save it for now
;	ld b,0		; clear out MSB of reg BC, reg C contains the saved byte count
;	push bc		; DMA count is in reg BC, save it 
; set up DMA master control
;	ld c,DMActrl	; set up DMA master control
;	ld hl,0f0e0h	; software ready for dma0&1, no end-of-process, no links
;	outw (c),hl	; write DMA master control reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; set up DMA count register 
;	ld c,DMA3cnt	; setup count of 128 byte
;	pop hl		; transfer what was saved in bc into hl
;	outw (c),hl	; write DMA3 count reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; source buffer starts at 0xc000
;	ld c,DMA3srcH	; source is 0x1000
;	ld hl,0cfh		; A23..A12 are 0x00c		
;	outw (c),hl	; write DMA3 source high reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;	ld c,DMA3srcL	;
;	ld hl,0f000h	; A11..A0 are 0x0
;	outw (c),hl	; write DMA3 source low reg
;	db 0edh,0bfh	; op code for OUTW (C),HL	
; destination buffer is in e + hl (saved in stack right now)
;	ld c,DMA3dstH
;	ld a,(addr3116)	; get A23..A16 value into reg H
;	ld h,a		; 	
;	pop de		; restore saved hl into de

;lines marked with ;;mmu comment are added to check for physical page 0
; insert a test for physical page 0 (addr3116 equal 0 & upper nibble of reg D also zero)
;	or a		;;mmu reg A contains (addr3116)
;	jp nz,notpage0	;;mmu not physical page 0
;	ld a,d		;;mmu A31..A16 is zero, now examine A15..A12
;	and 0f0h		;;mmu mask off A11..A8
;	jp nz,notpage0	;;mmu not physical page 0
; the destination is physical page 0, substitue 0x3C000 instead
; put 3C000 in HL and jump to write dma3destination

;	ld l,0cfh		;;mmu
;	ld h,03h		;;mmu
;	jp do_dma3hi	;;mmu

;notpage0:
;	ld l,d		; move A15..A8 value
;	ld a,0fh		; force lowest nibble of DMA3dstH to 0xF
;	or l
;do_dma3hi:
;	outw (c),hl	; write DMA3 destination high reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;	ld c,DMA3dstL
;	ld h,d		; reg DE contain A15..A0 value
;	ld l,e
;	ld a,0f0h		; force highest nibble of DMA3dstL to 0xF
;	or h
;	outw (c),hl	; write DMA3 destination low reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; write DMA3 transaction description reg and start DMA
;	ld hl,8080h	; enable DMA3, burst, byte size, flowthrough, no interrupt
;			;  incrementing memory for source & destination
;	ld c,DMA3td	; setup DMA3 transaction descriptor reg
;	outw (c),hl	; write DMA3 transaction description reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;  DMA should start now

	ld a,'.'		; checksum match, put '.'
filesav2:
	call cout
	jp flushln	; repeat until record end
badload:
	ld a,'?'		; checksum not match, put '?'
	jp filesav2
unknown:
	ld a,'U'		; put out a 'U' and wait for next record
	call cout
flushln:
	call cinq		; keep on reading until ':' is encountered
	cp ':'
	jp nz,flushln
	jp fileload
; format CF drives directories unless it is RAM disk
; drive A directory is track 1, sectors 0-0x1F
; drive B directory is track 0x40, sectors 0-0x1F
; drive C directory is track 0x80, sectors 0-0x1F
; drive D directory is track 0xC0, sectors 0-0x1F
format:
	ld hl,clrdir$	; command message
	call strout
	call cin
	cp 'A'
	jp z,formatA	; fill track 1 sectors 0-0x1F with 0xE5
	cp 'B'
	jp z,formatB	; fill track 0x40 sectors 0-0x1F with 0xE5
	cp 'C'
	jp z,formatC	; fill track 0x80 sectors 0-0x1F with 0xE5
	cp 'D'
	jp z,formatD	; fill track 0xC0 sectors 0-0x1F with 0xE5
;	cp 'E'
;	jp z,ClearDir
	jp abort		; abort command if not in the list of options
formatA:
	ld de,100h	; start with track 1 sector 0
	jp doformat
formatB:
	ld de,4000h	; start with track 0x40 sector 0
	jp doformat
formatC:
	ld de,8000h	; start with track 0x80 sector 0
	jp doformat
formatD:
	ld de,0c000h	; start with track 0xC0 sector 0
doformat:
	ld hl,confirm$	; confirm command execution
	call strout
	call tstCRLF
	jp nz,abort	; abort command if not CRLF
	ld a,40h		; set Logical Address addressing mode
	out (CF2427),a
	xor a		; clear reg A
	out (CF1623),a	; MSB track is 0
	ld a,d		; reg D contains the track info
	out (CF815),a
	ld c,CFdata	; reg C points to CF data reg
	ld hl,0e5e5h	; value for empty directories
wrCFf:
	ld a,1		; write 1 sector
	out (CFsectcnt),a	; write to sector count with 1
	ld a,e		; write CPM sector
	cp 20h		; format sector 0-0x1F
	jp z,wrCFdonef	; done formatting
	out (CF07),a	; 
	ld a,30h		; write sector command
	out (CFstat),a	; issue the write sector command
wrdrqf:
	in a,(CFstat)	; check data request bit set before write CF data
	and 8		; bit 3 is DRQ, wait for it to set
	jp z,wrdrqf
	ld b,0h		; sector has 256 16-bit data
loopf:
;z80	db 0edh,0bfh	; op code for OUT[W] (C),HL
;	outw (c),hl
	out (c),h		;z80 writes 2 bytes to CF
	out (c),h	
	inc b
	jp nz,loopf
readbsyf:
; spin on CF status busy bit
	in a,(CFstat)	; read CF status 
	and 80h		; mask off all except busy bit
	jp nz,readbsyf

	inc e		; write next sector
	jp wrCFf
wrCFdonef:
	jp CMD

INPORT:
; read data from specified I/O port in page 0
; command format is "I port#"
; 
	ld hl,inport$	; print command 'I' prompt
	call strout
	call GETHEX	; get port # into reg A
	push bc		; save register
	ld c,a		; load port # in reg C
;	call ZeroPage	; The I/O port resides in page 0
	in b,(c)		; get data from port # into reg B
;	call UARTPage
	ld hl,invalue$
	call strout
	ld a,b
	call HEXOUT
	pop bc		; restore reg
	jp CMD
OUTPORT:
; write data to specified I/O port in page 0
; command format is "O value port#"
	ld hl,outport$	; print command 'O' prompt
	call strout
	call GETHEX	; get value to be output
	push bc		; save register

	ld b,a		; load value in reg B
	ld hl,outport2$	; print additional prompt for command 'O'
	call strout
	call GETHEX	; get port number into reg A
	ld c,a
;	call ZeroPage	; The I/O port resides in page 0
	out (c),b		; output data in regB to port in reg C

;	call UARTPage
	pop bc
	jp CMD
LISTHEX:
; list memory as Intel Hex format
; the purpose of command is to save memory as Intel Hex format to console
	ld hl,listhex$	; print command 'L' prompt
	call strout
	call ADRIN	; get address word into reg DE
	push de		; save for later use
	ld hl,listhex1$	; print second part of 'L' command prompt
	call strout
	call ADRIN	; get end address into reg DE
listhex1:
	ld hl,CRLF$	; put out a CR, LF	
	call strout
	ld c,10h		; each line contains 16 bytes
	ld b,c		; reg B is the running checksum
	ld a,':'		; start of Intel Hex record
	call cout
	ld a,c		; byte count
	call HEXOUT
	pop hl		; start address in HL
	call ADROUT	; output start address
	ld a,b		; get the checksum
	add a,h		; accumulate checksum
	add a,l		; accumulate checksum
	ld b,a		; checksum is kept in reg B
	xor a		
	call HEXOUT	; record type is 00 (data)
listhex2:
	ld a,(hl)		; get memory pointed by hl
	call HEXOUT	; output the memory value in hex
	ld a,(hl)		; get memory again
	add a,b		; accumulate checksum
	ld b,a		; checksum is kept in reg B
	inc hl
	dec c
	jp nz,listhex2
	ld a,b		; get the checksum
	neg a
	call HEXOUT	; output the checksum
; output 16 memory location, check if reached the end address (saved in reg DE)
; unsign compare: if reg A < reg N, C flag set, if reg A > reg N, C flag clear
	push hl		; save current address pointer
	ld a,h		; get MSB of current address
	cp d		; reg DE contain the end address
	jp nc,hexend	; if greater, output end-of-file record
	jp c,listhex1	; if less, output more record
; if equal, compare the LSB value of the current address pointer
	ld a,l		; now compare the LSB of current address
	cp e
	jp c,listhex1	; if less, output another line of Intel Hex
hexend:
; end-of-record is :00000001FF
	ld hl,CRLF$
	call strout
	ld a,':'		; start of Intel Hex record
	call cout
	xor a
	call HEXOUT	; output "00"
	xor a
	call HEXOUT	; output "00"
	xor a
	call HEXOUT	; output "00"
	ld a,1
	call HEXOUT	; output "01"
	ld a,0ffh
	call HEXOUT	; output "FF"

	pop hl		; clear up the stack

	jp CMD

; print help message
HELP:
	ld hl,HELP$	; print help message
	call strout
	jp CMD
; boot CPM
; copy program from LA9-LA26 (9K) to 0xDC00
; jump to 0xF200 after copy is completed.
BootCPM:
	ld hl,bootcpm$	; print command message
	call strout
	call cin		; get input
;	cp '1'		; '1' is user apps
;	jp z,bootApps
	cp '2'		; '2' is cpm2.2
	jp z,boot22
;	cp '3'		; '3' is cpm3, not implemented
;	jp z,boot3
	jp what

boot22:
	ld hl,confirm$	; CRLF to execute the command
	call strout
	call tstCRLF
	jp nz,abort	; abort command if no CRLF
;	call COUTdone	; wait for transmit done before switch page i/o reg
	ld a,0e0h		; set Logical Address addressing mode
	out (CF2427),a
	xor a		; clear reg A
	out (CF1623),a	; track 0
	out (CF815),a
	ld hl,0dc00h	; CPM starts from 0xDC00 to 0xFFFF
	ld c,CFdata	; reg C points to CF data reg
;	ld d,9		; read from LA 9 to LA27
	ld d,80h		; read from LA 0x80 to LA 0x92
readCPM1:
	ld a,1		; read 1 sector
	out (CFsectcnt),a	; write to sector count with 1
	ld a,d		; read CPM sector
;	cp 27		; between LA9 and LA26
	cp 92h		; between LA80h and LA91h
	jp z,goCPM	; done copying, execute CPM
	out (CF07),a	; 
	ld a,20h		; read sector command
	out (CFstat),a	; issue the read sector command
readdrqCPM:
	in a,(CFstat)	; check data request bit set before read CF data
	and 8		; bit 3 is DRQ, wait for it to set
	jp z,readdrqCPM
	ld b,0h		; sector has 256 16-bit data
	inir		;z80 read 256 bytes
	ld b,0h		;z80
	inir		;z80 read 256 bytes

;z80	db 0edh,92h	; op code for inirw input word and increment
;	inirw
	inc d		; read next sector
	jp readCPM1
goCPM:
	jp 0f200h		; BIOS starting address of CP/M


fillZ:
	ld hl,fill0$	; print fill memory with 0 message
	call strout
	ld b,0		; fill memory with 0
	jp dofill
fillF:
	ld hl,fillf$	; print fill memory with F message
	call strout
	ld b,0ffh		; fill memory with ff
dofill:
	ld hl,confirm$	; get confirmation before executing
	call strout
	call tstCRLF	; check for carriage return
	jp nz,abort
	ld hl,PROGEND	; start from end of this program
	ld a,0ffh		; end address in reg A
filla:
	ld (hl),b		; write memory location
	inc hl
	cp h		; reached 0xFF00?
	jp nz,filla	; continue til done
	cp l		; reached 0xFFFF?
	jp nz,filla
	ld hl,0b000h	; fill value from 0xB000 down to 0x0000
fillb:
	dec hl
	ld (hl),b		; write memory location with desired value
	ld a,h		; do until h=l=0
	or l
	jp nz,fillb
	jp CMD
; Read RAMdisk
; start from 0x1000 as track 0 sector 0
; each track is 128k, so there are total of 4 tracks
; each sector is 512 bytes, this is holdover from a CF sector
; use DMA to read a sector to 0x1000
READRD:
	ld hl,read$	; put out read command message
	call strout
	ld hl,track$	; enter track in hex value
	call strout
	call GETHEX	; get a byte of hex value as track
	ld (RDtrack),a	; save it 
;;	push af		; save track value in stack
	ld hl,sector$	; enter sector in hex value
	call strout
	call GETHEX	; get a byte of hex value as sector
	ld (RDsector),a	; save it
;;	push af		; save sector value in stack
READRD1:
	ld hl,1000h	; copy previous block to 2000h
	ld de,2000h
	ld bc,200h	; copy 512 bytes
	ldir		; block copy

; track 0 is 0x10000 base address plus 0x200*sector
; track 1 is 0x30000 base address plus 0x200*sector
; track 2 is 0x50000 base address plus 0x200*sector
; track 3 is 0x70000 base address plus 0x200*sector

;	call DMAPage	; set page i/o reg to DMA
; set up DMA master control
;	ld c,DMActrl	; set up DMA master control
;	ld hl,0f0e0h	; software ready for dma0&1, no end-of-process, no links
;	outw (c),hl	; write DMA master control reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; set up DMA count register 
;	ld c,DMA3cnt	; setup count of 512 bytes
;	ld hl,200h	
;	outw (c),hl	; write DMA3 count reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;;	pop af		; get sector value from stack
;;	push af		; save for later use
;	ld a,(RDsector)	; get sector value
;	add a		; 2*n, no need to worry about carry for this part of operation
;	or 0f0h		; set high nibble to all 1's
;	ld h,a		; forming source low address
;	ld l,0
;	ld c,DMA3srcL	;
;	outw (c),hl	; write DMA3 source low reg
;	db 0edh,0bfh	; op code for OUTW (C),HL

;;	pop af		; restore, now calculate the high 12-bit of source address
;	ld a,(RDsector)
;	ld h,0
;	ld l,a		; get the sector value into reg L
;	add hl,hl		; 16-bit add, carry bit goes into reg H
;	ld a,0fh		; force lowest nibble to all 1's
;	or l		; 
;	ld l,a		; reg L has the low address value
; now get the high address value into reg H:
;;	pop af		; get track number
;	ld a,(RDtrack)
;	add a		; track 0 ->0x10000, track 1 -> 0x30000, track 2 -> 0x50000, etc
;	add 1		; add the track offset
;	add h		; h may be 1 due to carry from hl+hl operation
;	ld h,a		; reg H has the high address value
;	ld c,DMA3srcH	; point to source high register
;	outw (c),hl	; write DMA3 source high reg
;	db 0edh,0bfh	; op code for OUTW (C),HL

; destination is 0x1000
;	ld c,DMA3dstH
;	ld hl,01fh	; A23..A12 are 0x001, low nibble is all 1's
;	outw (c),hl	; write DMA3 destination high reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;	ld c,DMA3dstL
;	ld hl,0f000h	; A11..A0 are 0x000, high nibble is all 1's
;	outw (c),hl	; write DMA3 destination low reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; write DMA3 transaction description reg and start DMA
;	ld hl,8080h	; enable DMA3, burst, byte size, flowthrough, no interrupt
;			;  incrementing memory for source & destination
;	ld c,DMA3td	; setup DMA3 transaction descriptor reg
;	outw (c),hl	; write DMA3 transaction description reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;  DMA should start now

;dumpdata:
;	ld d,32		; 32 lines of data
;	ld hl,1000h	; display 512 bytes of data
;dmpdata1:
;	push hl		; save hl
;	ld hl,CRLF$	; add a CRLF per line
;	call strout
;	pop hl		; hl is the next address to display
;	call DMP16TS	; display 16 bytes per line
;	dec d
;	jp nz,dmpdata1

;	ld hl,1000h	; compare with data block in 2000h
;	ld bc,200h
;	ld de,2000h
;blkcmp:
;	ld a,(de)		; get a byte from block in 2000h
;	inc de
;	cpi		; compare with corresponding data in 1000h
;	jp po,blkcmp1	; exit at end of block compare
;	jp z,blkcmp	; exit if data not compare
;	ld hl,notsame$	; send out message that data not same as previous read
;	call strout
;	jp chkRDmore
;blkcmp1:	
;	ld hl,issame$	; send out message that data read is same as before
;	call strout

;chkRDmore:
;	ld hl,RDmore$	; carriage return for next sector of data
;	call strout
;	call tstCRLF	; look for CRLF
;	jp nz,CMD		; 
;	ld hl,(RDsector)	; load track & sector as 16-bit value
;	inc hl		; increment by 1
;	ld (RDsector),hl	; save updated values
;	ld hl,track$	; print track & sector value
;	call strout
;	ld a,(RDtrack)
;	call HEXOUT
;	ld hl,sector$
;	call strout
;	ld a,(RDsector)
;	call HEXOUT

;	jp READRD1

;********************** CF read**************








	ld a,0e0h		; set Logical Address addressing mode
	out (CF2427),a
	ld a,1		; read 1 sector
	out (CFsectcnt),a	; write to sector count with 1
	ld a,0		; read first sector
	out (CF1623),a	; high byte of track is always 0
;Z80SBC	pop af		; restore the sector value
	ld a,(RDsector)	;Z80SBC get sector value
	out (CF07),a	; write sector
;Z80SBC	pop af		; restore the track value
	ld a,(RDtrack)
	out (CF815),a
;	ld a,0		; LA sector 0
;	out (CF07),a	; 
	ld a,20h		; read sector command
	out (CFstat),a	; issue the read sector command
readdrq:
	in a,(CFstat)	; check data request bit set before read CF data
	and 8		; bit 3 is DRQ, wait for it to set
	jp z,readdrq
	ld hl,1000h	; store CF data starting from 1000h
	ld c,CFdata	; reg C points to CF data reg
	ld b,0h		; sector has 256 16-bit data
	inir		;Z80SBC
	ld b,0h		;Z80SBC 2nd half of 512-byte sector
	inir
;Z80SBC	db 0edh,92h	; op code for inirw input word and increment
;	inirw
	ld hl,1000h	; compare with data block in 2000h
	ld bc,200h
	ld de,2000h
blkcmp:
	ld a,(de)		; get a byte from block in 2000h
	inc de
	cpi		; compare with corresponding data in 1000h
	jp po,blkcmp1	; exit at end of block compare
	jp z,blkcmp	; exit if data not compare
;	call UARTPage
	ld hl,notsame$	; send out message that data not same as previous read
	call strout
	jp dumpdata
blkcmp1:
;	call UARTPage	; initialize page i/o reg back to UART	
	ld hl,issame$	; send out message that data read is same as before
	call strout
dumpdata:
	ld d,32		; 32 lines of data
	ld hl,1000h	; display 512 bytes of data
dmpdata1:
	push hl		; save hl
	ld hl,CRLF$	; add a CRLF per line
	call strout
	pop hl		; hl is the next address to display
	call DMP16	; display 16 bytes per line
	dec d
	jp nz,dmpdata1

	jp CMD

readbsy:
; spin on CF status busy bit
	in a,(CFstat)	; read CF status 
	and 80h		; mask off all except busy bit
	jp nz,readbsy
	ret


;****************CF read*************






; Write CF
;  allowable parameters are '0' for boot sector & ZZMon, '1' for 32K apps, 
;   '2' for CPM2.2, '3' for CPM3
; Set page I/O to 0, afterward set it back to 0FEh
COPYCF:
	ld hl,copycf$	; print copy message
	call strout
	call cin		; get write parameters
	cp '0'
	jp z,cpboot
;	cp '1'
;	jp z,cpAPPS
	cp '2'
	jp z,CopyCPM2
;	cp '3'
;	jp z,CopyCPM3
	jp what		; error, abort command

	jp CMD
; test for CR or LF.  Echo back. return 0
tstCRLF:
	call cin		; get a character					
	cp 0dh		; if carriage return, output LF
	jp z,tstCRLF1
	cp 0ah		; if line feed, output CR 
	jp z,tstCRLF2
	ret
tstCRLF1:
	ld a,0ah		; put out a LF
	call cout
	xor a		; set Z flag
	ret
tstCRLF2:
	ld a,0dh		; put out a CR
	call cout
	xor a		; set Z flag
	ret

; write CPM to CF
; write data from 0xDC00 to 0xFFFF to CF LA128-LA146 (9K)
CopyCPM2:
	ld hl,0dc00h	; CPM starts from 0xDC00 to 0xFFFF
	ld de,8092h	; reg DE contains beginning sector and end sector values
	jp wrCF

cpboot:
; use DMA to copy itself into physical page 0
; the reason DMA is used is because physical page 0 is invisible due to MMU mapping
;  but DMA transfer between physical addresses bypassing the MMU
;	ld hl,confirm$	; carriage return to execute the program
;	call strout
;	call tstCRLF
;	jp nz,CMD		; abort command if not CR or LF
; Source is at 0xb000
;	call DMAPage	; set page i/o reg to DMA
; set up DMA master control
;	ld c,DMActrl	; set up DMA master control
;	ld hl,0f0e0h	; software ready for dma0&1, no end-of-process, no links
;	outw (c),hl	; write DMA master control reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; set up DMA count register 
;	ld c,DMA3cnt	; setup count of 4096 byte
;	ld hl,1000h	
;	outw (c),hl	; write DMA3 count reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; source buffer starts at 0xb000
;	ld c,DMA3srcH	; source is 0xb000
;	ld hl,0bfh	; A23..A12 are 0x00b, low nibble is all 1's		
;	outw (c),hl	; write DMA3 source high reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;	ld c,DMA3srcL	;
;	ld hl,0f000h	; A11..A0 are 0x0, high nibble is all 1's
;	outw (c),hl	; write DMA3 source low reg
;	db 0edh,0bfh	; op code for OUTW (C),HL	
; destination is 0x0
;	ld c,DMA3dstH
;	ld hl,0fh		; A23..A12 are 0x000, low nibble is all 1's
;	outw (c),hl	; write DMA3 destination high reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;	ld c,DMA3dstL
;	ld hl,0f000h	; A11..A0 are 0x0, high nibble is all 1's
;	outw (c),hl	; write DMA3 destination low reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
; write DMA3 transaction description reg and start DMA
;	ld hl,8080h	; enable DMA3, burst, byte size, flowthrough, no interrupt
;			;  incrementing memory for source & destination
;	ld c,DMA3td	; setup DMA3 transaction descriptor reg
;	outw (c),hl	; write DMA3 transaction description reg
;	db 0edh,0bfh	; op code for OUTW (C),HL
;  DMA should start now
	jp CMD

cpAPPS:
	ld hl,0		; Application starts from 0 to 0x7FFF
	ld de,407fh	; reg DE contains beginning sector and end sector values
wrCF:
	push hl		; save value
	ld hl,confirm$	; carriage return to execute the program
	call strout
	pop hl
	call tstCRLF
	jp nz,CMD	; abort command if not CR or LF
	ld a,0e0h		; set Logical Address addressing mode
	out (CF2427),a
	xor a		; clear reg A
	out (CF1623),a	; track 0
	out (CF815),a
	ld c,CFdata	; reg C points to CF data reg
wrCF1:
	ld a,1		; write 1 sector
	out (CFsectcnt),a	; write to sector count with 1
	ld a,d		; write CPM sector
	cp e		; reg E contains end sector value
	jp z,wrCFdone	; done copying, execute CPM
	out (CF07),a	; 
	ld a,30h		; write sector command
	out (CFstat),a	; issue the write sector command
wrdrq:
	in a,(CFstat)	; check data request bit set before write CF data
	and 8		; bit 3 is DRQ, wait for it to set
	jp z,wrdrq
	ld b,0h		; sector has 256 16-bit data
	otir		;z80 write 256 bytes
	ld b,0h		;z80
	otir		;z80 wrute 256 bytes
;Z80
;Z80	db 0edh,93h	; op code for otirw output word and increment
;	otirw
readbsyc:
; spin on CF status busy bit
	in a,(CFstat)	; read CF status 
	and 80h		; mask off all except busy bit
	jp nz,readbsyc

	inc d		; write next sector
	jp wrCF1
wrCFdone:
	jp CMD
	
TESTRAM:
; test memory from top of this program to 0xFFFE 
	ld hl,testram$	; print test ram message
	call strout
	ld hl,confirm$	; get confirmation before executing
	call strout
	call tstCRLF	; check for carriage return
	jp nz,abort
	ld iy,(testseed)	; a prime number seed, another good prime number is 211
TRagain:
	ld hl,PROGEND	; start testing from the end of this program
	ld de,137		; increment by prime number
TRLOOP:
	push iy		; bounce off stack
	pop bc
	ld (hl),c		; write a pattern to memory
	inc hl
	ld (hl),b
	inc hl
	add iy,de		; add a prime number
	ld a,0ffh		; compare h to 0xff
	cp h
	jp nz,TRLOOP	; continue until reaching 0xFFFE
	ld a,0feh		; compare l to 0xFE
	cp l
	jp nz,TRLOOP
	ld hl,0b000h	; test memory from 0xAFFF down to 0x0000
TR1LOOP:
	push iy
	pop bc		; bounce off stack
	dec hl
	ld (hl),b		; write MSB
	dec hl
	ld (hl),c		; write LSB
	add iy,de		; add a prime number
	ld a,h		; check h=l=0
	or l
	jp nz,TR1LOOP
	ld hl,PROGEND	; verify starting from the end of this program
	ld iy,(testseed)	; starting seed value
TRVER:
	push iy		; bounce off stack
	pop bc
	ld a,(hl)		; get LSB
	cp c		; verify
	jp nz,TRERROR
	inc hl
	ld a,(hl)		; get MSB
	cp b
	jp nz,TRERROR
	inc hl
	add iy,de		; next reference value
	ld a,0ffh		; compare h to 0xff
	cp h
	jp nz,TRVER	; continue verifying til end of memory
	ld a,0feh		; compare l to 0xFE
	cp l
	jp nz,TRVER
	ld hl,0b000h	; verify memory from 0xB000 down to 0x0000
TR1VER:
	push iy		; bounce off stack
	pop bc
	dec hl
	ld a,(hl)		; get MSB from memory
	cp b		; verify
	jp nz,TRERROR
	dec hl
	ld a,(hl)		; get LSB from memory
	cp c
	jp nz,TRERROR
	add iy,de
	ld a,h		; check h=l=0
	or l
	jp nz,TR1VER
	call SPCOUT	; a space delimiter
	ld a,'O'		; put out 'OK' message
	call cout
	ld a,'K'
	call cout
	ld (testseed),iy	; save seed value

	IN A,(RxStat)	; read on-chip UART receive status
        	AND 2				;;Z data available?
        	JP Z,TRagain	; no char, do another iteration of memory test
        	IN A,(RxData)	; save to reg A
        	OUT (TxData),A	; echo back
;	cp 'X'		; if 'X' or 'x', exit memory test
;	jp z,CMD
;	cp 'x'
;	jp nz,TRagain
	jp CMD
TRERROR:
	call SPCOUT	; a space char to separate the 'r' command
	ld a,'H'		; display content of HL reg
	call cout		; print the HL label
	ld a,'L'
	call cout
	call SPCOUT	
	call ADROUT	; output the content of HL 	
	jp CMD

;Get an address and jump to it
go:
	ld hl,go$		; print go command message
	call strout
        	CALL ADRIN
        	LD H,D
        	LD L,E
	push hl		; save go address
	ld hl,confirm$	; get confirmation before executing
	call strout
	call tstCRLF	; check for carriage return
	pop hl
	jp nz,abort
;	ld hl,CRLF$	; insert CRLF before executing
;	call strout
;	pop hl		; restore saved go address
	jp (hl)		; jump to address if CRLF

;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities from Glitch Works ver 0.1 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Copyright (C) 2012 Jonathan Chapman ;;;;;;;;;;;;;;;;;;

;Edit memory from a starting address until X is
;pressed. Display mem loc, contents, and results
;of write.
EDMEM:  	CALL SPCOUT
        	CALL ADRIN
        	LD H,D
        	LD L,E
ED1:    	LD A,13
        	CALL cout
        	LD A,10
        	CALL cout
        	CALL ADROUT
        	CALL SPCOUT
        	LD A,':'
        	CALL cout
        	CALL SPCOUT
        	CALL DMPLOC
        	CALL SPCOUT
        	CALL GETHEX
        	JP C,CMD
        	LD (HL),A
        	CALL SPCOUT
        	CALL DMPLOC
        	INC HL
        	JP ED1

;Dump memory between two address locations
MEMDMP: 	CALL SPCOUT
        	CALL ADRIN
        	LD H,D
        	LD L,E
        	LD C,10h
        	CALL SPCOUT
        	CALL ADRIN
MD1:    	LD A,13
        	CALL cout
        	LD A,10
        	CALL cout
        	CALL DMP16
        	LD A,D
        	CP H
        	JP M,CMD
        	LD A,E
        	CP L
        	JP M,MD2
        	JP MD1
MD2:    	LD A,D
        	CP H
        	JP NZ,MD1
        	JP CMD

DMP16TS:
; dump memory pointed by HL, but using the RDtrack & RDsector
;  as the address field
; compute physical address from sector & track
	push hl		; save reg
	ld hl,(RDsector)	; load current sector and track
	add hl,hl		; shift by one to get physical address
	inc h		; add track offset
; hl now contains A23..A8 of physical address.  A7..A0 are zero
	call ADROUT	; output A23..A8
	ld a,0
	call HEXOUT	; output A7..A0 which are zero
	pop hl		; restore reg
	jp DMP16D		; display the 16 data field

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DMP16 -- Dump 16 consecutive memory locations
;
;pre: HL pair contains starting memory address
;post: memory from HL to HL + 16 printed
;post: HL incremented to HL + 16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DMP16:  	CALL ADROUT
DMP16D:			; 16 consecutive data
        	CALL SPCOUT
        	LD A,':'
        	CALL cout
        	LD C,10h
	push hl		; save location for later use
DM1:    	CALL SPCOUT
        	CALL DMPLOC
        	INC HL		
        	DEC C
;        	RET Z
	jp nz,DM1
;        	JP DM1

; display the ASCII equivalent of the hex values
	pop hl		; retrieve the saved location
	ld c,10h		; print 16 characters
	call SPCOUT	; insert two space
	call SPCOUT	; 
dm2:
	ld a,(hl)		; read the memory location
	cp ' '
	jp m,printdot	; if lesser than 0x20, print a dot
	cp 7fh
	jp m,printchar
printdot:
; for value lesser than 0x20 or 0x7f and greater, print '.'
	ld a,'.'
printchar:
; output printable character
	call cout
	inc hl
	dec c
	ret z
	jp dm2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DMPLOC -- Print a byte at HL to console
;
;pre: HL pair contains address of byte
;post: byte at HL printed to console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DMPLOC: 	LD A,(HL)
        	CALL HEXOUT
        	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;HEXOUT -- Output byte to console as hex
;
;pre: A register contains byte to be output
;post: byte is output to console as hex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HEXOUT: 	PUSH BC
        	LD B,A
        	RRCA
        	RRCA
        	RRCA
        	RRCA
        	AND 0Fh
        	CALL HEXASC
        	CALL cout
        	LD A,B
        	AND 0Fh
        	CALL HEXASC
        	CALL cout
        	POP BC
        	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;HEXASC -- Convert nybble to ASCII char
;
;pre: A register contains nybble
;post: A register contains ASCII char
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HEXASC: 	ADD 90h
        	DAA
        	ADC A,40h
        	DAA
        	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ADROUT -- Print an address to the console
;
;pre: HL pair contains address to print
;post: HL printed to console as hex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ADROUT: 	LD A,H
        	CALL HEXOUT
        	LD A,L
        	CALL HEXOUT
        	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ADRIN -- Get an address word from console
;
;pre: none
;post: DE contains address from console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ADRIN:  	CALL GETHEX
        	LD D,A
        	CALL GETHEX
        	LD E,A
        	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GETHEX -- Get byte from console as hex
;
;pre: none
;post: A register contains byte from hex input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GETHEX: 	PUSH DE
        	CALL cin
        	CP 'X'
        	JP Z,GE2
	cp 'x'		; exit with lower 'x'
	jp z,GE2
        	CALL ASCHEX
        	RLCA
        	RLCA
        	RLCA
        	RLCA
        	LD D,A
        	CALL cin
        	CALL ASCHEX
        	OR D
GE1:    	POP DE
        	RET
GE2:    	SCF
        	JP GE1

; get hex without echo back
GETHEXQ:
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GOBYT -- Push a two-byte instruction and RET
;         and jump to it
;
;pre: B register contains operand
;pre: C register contains opcode
;post: code executed, returns to caller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GOBYT:  	LD HL,0000
        	ADD HL,SP
        	DEC HL
        	LD (HL),0C9h
        	DEC HL
        	LD (HL),B
        	DEC HL
        	LD (HL),C
        	JP (HL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SPCOUT -- Print a space to the console
;
;pre: none
;post: 0x20 printed to console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SPCOUT: 	LD A,' '
        	CALL cout
        	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;strout -- Print a null-terminated string
;
;pre: HL contains pointer to start of a null-
;     terminated string
;post: string at HL printed to console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strout: 	LD A,(HL)
        	CP 00
        	RET Z
        	CALL cout
        	INC HL
        	JP strout
;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities by Glitch Works ver 0.1 ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Copyright (C) 2012 Jonathan Chapman ;;;;;;;;;;;;;;;;;;

; input char into reg A
cin:
	in a,(0f8h)	;read status
	and 2		;data ready flag is in D[1]
	jr z,cin
	in a,(0f9h)	; read data
	call cout		; echo back
	ret
; get char from console without echo
cinq:    
	in a,(0f8h)	;read status
	and 2		;data ready flag is in D[1]
	jr z,cinq
	in a,(0f9h)	; read data
	ret
        	RET

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

MMUtbl	ds 32		; 16 pages of system MMU page descriptors

;PROGEND:	equ $		; end of the program
PROGEND:	equ 0c000h		; end of the program is above the stack

signon$:	db "Z80SBC Monitor v0.4 10/28/18", 13,10,0
;        	db "Format drives command",13,10,0 
PROMPT$:	db 13, 10, 10, ">", 0
what$:   	db 13, 10, "?", 0
CRLF$	db 13,10,0
confirm$	db " press Return to execute command",0
abort$	db 13,10,"command aborted",0
notdone$	db 13,10,"command not implemented",0
go$	db "o to address: 0x",0
track$	db " track:0x",0
sector$	db " sector:0x",0
read$	db "ead RAM disk",0
RDmore$	db 10,13,"carriage return for next sector, any other key for command prompt",10,13,0
notsame$	db 10,13,"Data NOT same as previous read",10,13,0
issame$	db 10,13,"Data same as previous read",10,13,0
inport$	db "nput from port ",0
invalue$	db 10,13,"Value=",0
outport$	db "utput ",0
outport2$	db " to port ",0
listhex$	db "ist memory as Intel Hex, start address=",0
listhex1$	db " end address=",0
fillf$	db "ill memory with 0xFF",10,13,0
fill0$	db "ero memory",10,13,0
testram$	db "est memory",10,13,0
copycf$	db "opy to RAM disk",10,13
	db "0--boot,",10,13
;	db "1--User Apps,",10,13
	db "2--CP/M2.2:",10,13
;	db "3--CP/M3: ",0
	db 0
clrdir$	db " clear disk directories",10,13
	db "A -- drive A,",10,13
	db "B -- drive B:",10,13
	db "C -- drive C,",10,13
	db "D -- drive D,",10,13	
;	db "E -- RAM drive: ",0
	db 0
bootcpm$	db "oot CP/M",10,13
;	db "1--User Apps,",10,13
	db "2--CP/M2.2:",10,13
;	db "3--CP/M3: ",0
	db 0
HELP$	db "elp",13,10
	db "G <addr> CR",13,10
	db "R <track> <sector>",13,10
	db "D <start addr> <end addr>",13,10
	db "I <port>",13,10
	db "O <value> <port>",13,10
	db "L <start addr> <end addr>",13,10
	db "Z CR",13,10
	db "F CR",13,10
	db "T CR",13,10
	db "E <addr>",13,10
	db "X <options> CR",13,10
	db "B <options> CR",13,10
	db "C <options> CR",0

	END


