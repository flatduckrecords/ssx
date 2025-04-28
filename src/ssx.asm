;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SAM Coupé SCREEN$ viewer for ZX Spectrum Next
; by Robert William Morrison age 41
; March/April 2025
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; System Variables
BORDCR:			equ $5C48

;; Z80 I/O
PORT_REGSELECT: equ $243B
REG_CPU:		equ $07		; CPU speed (RW)
REG_L2BANK:		equ $12		; Layer 2 bank (RW)
ULA:			equ $FE		; Keyboard (R) and border colour (W)

;; DOS hook codes
IDE_BANK:		equ $01BD	; Plus3DOS allocate memory
M_P3DOS:		equ $94		; esxDOS make Plus3DOS call
F_OPEN:			equ $9A		; esxDOS File Open
F_CLOSE:		equ $9B		; esxDOS File Close
F_SEEK:			equ $9F		; esxDOS File Seek
F_READ:			equ $9D		; esxDOS File Read
F_FSTAT:		equ $A1		; esxDOS File Status

;; Other constants
START:			equ $2000		; dot-command start address
Y_OFFSET		equ $20			; used for MODE 3 vertical centring
RESOLUTION_X    equ 640			; Hires width
RESOLUTION_Y    equ 256			; Hires height

	DEVICE ZXSPECTRUMNEXT		; enable Z80n extensions in SjASMPlus
	org START

		di						; interrupts will be off throughout

		ld (_cmdline),hl
		ld (_osiy),iy

		push bc
		push de
		push hl					; HL will be modified on return
		push ix					; if there's an error.
		push iy
		ex af,af'
		push af
		exx
		push bc
		push de
		push hl

		ld (_ossp),sp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYSTEM >>> IN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		; Increase CPU speed
		ld sp,$4000
		ld bc,PORT_REGSELECT
		ld a,REG_CPU
		out (c),a
		inc b
		in a,(c)
		and 3
		ld (_cpuspeed),a
		nextreg REG_CPU,$03

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get filespec from the command line arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		ld hl,(_cmdline)
		ld a,h
		or l
		jp z,noargs
		ld de,filespec
		ld b,255
parseparam:
		ld a,(hl)
		cp " "
		jr z,modehint
		cp ":"
		jr z,done
		or a
		jr z,done
		cp 13
		jr z,done
		bit 7,a
		jr nz,done
		ld (de),a
		inc de
		inc hl
		djnz parseparam
		jr done

; try to determine target screen mode 
; from the file name:
modehint:
		inc hl
		ld a,(hl)
		cp "3"
		jr nz,1f
		ld a,3
		ld (_mode),a
		jr done
1:		cp "4"
		jr nz,1f
		ld a,4
		ld (_mode),a
		jr done
1:		ld a,4
		ld (_return),a
		jp noargs
done:	ld a,(_mode)
		or a
		jr nz,2f
		dec de				;step back to last char
		ld a,(de)
		inc de
		cp "3"				; compare ASCII "3"
		jr nz,1f
		ld a,3				; set literal 3
		ld (_mode),a
		jr 2f
1:		cp "4"
		jr nz,2f
		ld a,4				; otherwise we'll assume MODE 4
		ld (_mode),a
2:		xor a
		ld (de),a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We will allocate an 8KB bank as a temporary buffer for file
; and page it in at MMU3 ($6000). We will use MMU 4&5 to access
; Layer 2 in 16KB chunks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		ld hl,$0001			; 1=allocate ZX memory
		exx					; parameters in alt registers
		ld de,IDE_BANK		; IDE_BANK
		ld c,7				; "usually 7, but 0 for some calls"
		rst 8				; hook code via Err RST
		db M_P3DOS			; +3DOS call
		jp nc,allocfail
		ld (_reserved),de

		ld bc,PORT_REGSELECT
		ld a,$53			; MMU3
		out (c),a
		inc b
		in a,(c)
		ld (_mmu3),a		; backup old value

		ld bc,PORT_REGSELECT
		ld a,$54			; MMU4
		out (c),a
		inc b				; $253B nextreg data
		in a,(c)
		ld (_mmu4),a		; backup old value

		ld bc,PORT_REGSELECT
		ld a,$55			; MMU5
		out (c),a
		inc b
		in a,(c)
		ld (_mmu5),a		; backup old value

		ld bc,PORT_REGSELECT
		ld a,$12			; L2 Bank
		out (c),a
		inc b
		in a,(c)
		add a
		ld (_l2bank),a		; curent bank pointer
		inc a
		ld (_l2bank0),a		; reference to starting bank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Begin paging etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;; bring in our reserved bank at $6000
		ld a,(_reserved)
		nextreg 83,a		; MMU3 // $6000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get filename and load the file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		ld a,"*"
		ld hl,filespec
		ld b,%00000001
		rst 8
		db F_OPEN

		jr nc,1f
		ld (_error),a
		jp fileerror

1:		ld (_file),a		; backup file handle
		ld hl,filestat
		rst 8
		db F_FSTAT			; DOS get file status (size etc)

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Check filesize			   ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		ld hl,(filestat+9)
		ld a,h
		add l
		jr z,1f				; if A<>0 then file is WAY too big
		ld a,1
		ld (_return),a		; error #1 "File WAY too big"
		jp filefail

1:		ld hl,(filestat+7)
		ld a,h
		cp $64				; total size should not exceed $6400
		jr c,1f
		ld a,1
		ld (_return),a		; error #1 "File too big"
		jp filefail

1:		ld bc,$6000
		sbc hl,bc
		jr nc,1f
		ld a,2
		ld (_return),a		; error #2 "File too small"
		jp filefail
1:		
		jr nz,1f			; skip header check if file is
		jr skiphdrchk		; exactly 24KB
1:
		ld a,(_file)
		ld hl,$6000
		ld bc,$0008			; get first 8 bytes of image data
		rst 8
		db F_READ

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Check for +3DOS header	   ;
		; (if found, ignore it)		   ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkhdr:
		ld hl,P3DOSH
		ld de,$6000
		ld b,8
1:		ld a,(de)
		cp (hl)
		jr nz,1f
		inc hl
		inc de
		djnz 1b

		; header match, seek file pointer to 128
		ld a,(_file)
		ld bc,0
		ld de,128
		ld l,0				; 0=set abs position, 1=relative, 2=backwards
		rst 8
		db F_SEEK
skiphdrchk:
		ld hl,$6000
		ld bc,8192
		jr 2f
1:		ld hl,$6000+8
		ld bc,8192-8			; get first 8KB of image data
2:		ld a,(_file)
		rst 8
		db F_READ

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Final MODE checks
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		ld a,(_mode)
		or a				; clear Fc and check for zero
		jr nz,2f			; skip ahead if mode already set
		ld hl,(filestat+7)	; get filesize from header
		ld bc,$6000			; less 24KB should leave
		sbc hl,bc			; just the palette metadata
		ld a,h				
		or a
		jr nz,1f			; skip ahead if HL>255
		ld a,l
		cp 4
		jr nz,1f			; skip ahead if a<>4
		ld a,3				; otherwise set MODE 3
		ld (_mode),a
		jr 2f				; done, skip ahead.
1:		ld a,4				; Default to MODE 4
		ld (_mode),a

2:		ld a,(_mode)			; check which MODE we 
		cp 3				; settled on, and set
		jr nz,1f			; the starting L2
		ld a,(_l2bank0)		; bank accordingly
		ld (_l2bank),a

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Page-in Layer 2 at $8000-$BFFF
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
1:		ld a,(_l2bank)
		nextreg 84,a
		inc a
		nextreg 85,a
		inc a
		ld (_l2bank),a

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Branch to main SCREEN$ routine
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		ld a,(_mode)
		cp 3
		jp z,m3routine ; -->
		cp 4
		jp z,m4routine ; -->
bmpdone:	; <-- execution will JP to here

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Get palette metadata
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		ld a,(_file)
		ld hl,$6000			; buffer address
		ld bc,1024			; get (up to) 1024 bytes
		rst 8
		db F_READ			; BC=bytes actually read

		ld a,c
		or b				; check for zero
		jr z,nopalette		; if zero, skip palette
checklineint:
		ld hl,$6000			; HL=start of buffer
		add hl,bc			; HL=end of buffered bytes
		dec hl				; HL=final byte that was read
		ld a,255			; we'll compare it to 255
		cp (hl)				; check for end-marker
		jr z,1f				; jump ahead if okay
		inc hl				; otherwise step forward and
		ld (hl),a			; set missing end-marker
1:		
		ld a,b
		or a
		jr nz,1f			; TODO :: upper limit of line ints?
		ld a,c				; (if B=0, C=total palette bytes)
		cp 42				; more than 41 means lint ints
		jp c,2f				; skip subroutine if not
1:		call lineints		; call subroutine if so.

2:		call palette
nopalette:
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Branch to Next Layer 2 mode
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		ld a,(_mode)
		cp 4
		jp z,l2_256x192	; -->
		cp 3
		jp z,l2_640x256	; -->
l2enabled:	; <-- execution will JP to here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Okay, the image is now being displayed!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		ld a,(_mmu5)	; We can page-out Layer 2 now
		nextreg 85,a	; since no more writes will
		ld a,(_mmu4)	; be needed. (Does not need to
		nextreg 84,a	; be paged-in for display).

		call anykey		; hold and wait for user keystroke

		ld bc,$123B 	; L2 control port
		xor a			; 0 = switch off layer 2
		out (c),a

		nextreg $62,0	; copr stop

		ld a,(BORDCR)	; restore BORDER from sysvar
		rra
		rra
		rra
		and 7
		out (ULA),a		; set border colour

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Tell DOS to close the file
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
filefail:	; <-- also land here on load failure (e.g not found)
		ld a,(_file)
		rst 8
		db F_CLOSE

fileerror:	; <-- land here if DOS error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Undo memory paging, prepare to shut down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		; restore $6000 memory mapping
		ld a,(_mmu3)
		nextreg 83,a

		; de-allocate reserved memory bank
		ld de,(_reserved)
		ld hl,3
		exx
		ld de,IDE_BANK
		ld c,7
		rst 8
		db M_P3DOS
		jr allocfail

noargs:
		ld (noargsdone+1),sp
		ld sp,0
		ld hl,USAGE
1:		ld a,(hl)
		or a
		jr z,allocfail
		rst $10
		inc hl
		jr 1b
noargsdone:
		ld sp,0
allocfail:
		ld a,(_cpuspeed)
		nextreg REG_CPU,a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYSTEM >>> OUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		ld sp,(_ossp)
		pop hl
		pop de
		pop bc
		pop af
		exx
		ex af,af'
		pop iy
		pop ix
		pop hl
		pop de
		pop bc

		;; Handle dot-command return code
		ld a,(_return)
		or a
		jr z,checkesx	; skip if zero (i.e. not a custom error)
		dec a
		add a
		ld hl,ERRORTAB	; fetch custom error from code
		add a,l
		ld l,a
		push de
		ld de,(hl)
		ex de,hl		; HL now points to custom error message
		pop de
		xor a
		scf
		jr exit

checkesx:
		ld a,(_error)	; check for esxDOS error
		or a
		jr z,exit
		scf				; set Fc to indicate error
exit:	ei
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   S U B R O U T I N E S   
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
nokey:
		xor a
		in a,(ULA)		; read keys
		cpl
		and 31
		jr nz,nokey
		ret
anykey:
		call nokey
		xor a
aklp:
		in a,(ULA)		; read keys
		cpl
		and 31
		jr z,aklp
		ret

;; page-in L2 banks and increment pointers
incrbank:
		ld a,(_l2bank)
		nextreg 84,a		; Layer 2 (Bank 20)
		inc a
		nextreg 85,a		; Layer 2 (Bank 21)
		inc a
		ld (_l2bank),a
		ret

;; DOS read 8K of file to memory $6000
readfile:
		ld a,(_file)
		ld hl,$6000
		ld bc,8192
		rst 8
		db F_READ
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MODE 3 loading and bank switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
m3routine:

		call l2erase	; Clear screen memory

		ld hl,$6000
		ld de,$8000 + Y_OFFSET
		call m3_bitmap		; column 1&2
		call incrbank

		ld hl,$6020
		ld de,$8000 + Y_OFFSET
		call m3_bitmap		; column 3&4
		call incrbank

		ld hl,$6040
		ld de,$8000 + Y_OFFSET
		call m3_bitmap		; column 5&6
		call incrbank

		ld hl,$6060
		ld de,$8000 + Y_OFFSET
		call m3_bitmap		; column 7&8

		call readfile
		ld a,(_l2bank0)
		ld (_l2bank),a
		call incrbank

		ld hl,$6000
		ld de,$8040 + Y_OFFSET
		call m3_bitmap		; column 1&2
		call incrbank

		ld hl,$6020
		ld de,$8040 + Y_OFFSET
		call m3_bitmap		; column 3&4
		call incrbank

		ld hl,$6040
		ld de,$8040 + Y_OFFSET
		call m3_bitmap		; column 5&6
		call incrbank

		ld hl,$6060
		ld de,$8040 + Y_OFFSET
		call m3_bitmap		; column 7&8

		call readfile
		ld a,(_l2bank0)
		ld (_l2bank),a
		call incrbank

		ld hl,$6000
		ld de,$8080 + Y_OFFSET
		call m3_bitmap		; column 1&2
		call incrbank

		ld hl,$6020
		ld de,$8080 + Y_OFFSET
		call m3_bitmap		; column 3&4
		call incrbank

		ld hl,$6040
		ld de,$8080 + Y_OFFSET
		call m3_bitmap		; column 5&6
		call incrbank

		ld hl,$6060
		ld de,$8080 + Y_OFFSET
		call m3_bitmap		; column 7&8

		jp bmpdone			; JP back to common branching point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ENABLE Layer 2 640 x 256
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
l2_640x256
		ld bc,$123B 		; L2 control port
		ld a,%00000010		; 2 = enable layer 2
		out (c),a
		NEXTREG $70, %00100000    ; 640x256 16 colour mode
		NEXTREG $1C, 1            ; Reset Layer 2 clip window reg index
		NEXTREG $18, 0
		NEXTREG $18, RESOLUTION_X / 4 - 1
		NEXTREG $18, 0
		NEXTREG $18, RESOLUTION_Y - 1
		jp l2enabled	; JP back to common branching point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ENABLE Layer 2 256 x 192
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
l2_256x192
		ld bc,$123B 		; L2 control port
		ld a,%00000010		; 2 = enable layer 2
		out (c),a
		NEXTREG $70, %00000000    ; 256x192 256 colour mode
		NEXTREG $1C, 1            ; Reset Layer 2 clip window reg index
		NEXTREG $18, 0
		NEXTREG $18, 255
		NEXTREG $18, 0
		NEXTREG $18, 191
		jp l2enabled	; JP back to common branching point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Process MODE 3 bitmap data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
m3_bitmap:
		ld c,0
		ld b,64
m3_lineloop:
		push bc
		ld b,32
		push de
		push hl

m3_pxloop:
		ld a,(hl)		; 76543210
		and %11000000	; 76000000
		rra				; 07600000
		rra				; 00760000
		ld c,a			; 00760000
		ld a,(hl)		; 76543210
		and %00110000	; 00540000	;TODO go left instead?
		rra				; 00054000
		rra				; 00005400
		rra				; 00000540
		rra				; 00000054
		or c			; 00760054

		ld (de),a
		inc D
		
		ld a,(hl)		; 76543210
		rla				; 65432107
		rla				; 54321078
		and %00110000	; 00320000
		ld c,a			; 00320000
		ld a,(hl)		; 76543210
		and %00000011	; 00000010
		or c			; 00320010
		ld (de),a

		inc d
		inc l

		djnz m3_pxloop
		pop hl
		pop de
		inc de

		ld a,128
		add a,l
		ld l,a
		jr nc, 1f
		inc h
1:			pop bc
		djnz m3_lineloop
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MODE 4 loading and bank switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
m4routine:
		call m4_bitmap

		; get second 8KB of image data
		call readfile
		call incrbank
		call m4_bitmap

		; get third 8KB of image data
		call readfile
		call incrbank
		call m4_bitmap

		jp bmpdone

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Process MODE 4 bitmap data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
m4_bitmap:
		ld hl,$6000
		ld de,$8000
		ld c,0
		ld b,32		; 32 lines per screen 3rd
m4_lineloop:
		push bc
		ld b,0		; 256 px per line
m4_pxloop:		
		xor a
		rld			; hi nibble in A
		ld (de),a
		inc de
		rld			; lo nibble in A
		ld (de),a
		inc de
		rld			; restore hl
		inc hl
		djnz m4_pxloop
		pop bc
		djnz m4_lineloop
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Process SAM palette data and set Layer 2 (palette 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
palette:
		nextreg $14,$E3			; reset transparency colour to default
								; (useful if running from command line)
		ld de, $6000
		nextreg $43,%00000000	; palette control: select ULA palette 1 for writing
		nextreg $40,$10  		; palette index 16 (i.e. `BORDER 0`)
		ld a,(de)				; a=SAM pal num for PEN 0
		add a					; a=CLUT offset
		ld hl,clut
		add a,l
		ld l,a
		ld a,(hl)
		nextreg $44,a			; RRRGGGBB
		inc hl
		ld a,(hl)
		nextreg $44,a			; PxxxxxxB
		xor a
		out (ULA),a				; set border black

		nextreg $43,%00010000	; palette control: Wr L2 palette 1
		nextreg $40,0  			; palette index

		ld a,(_mode)
		cp 3
		jp z,m3_copypal

m4_copypal
		ld b,16					; 16 colours
1:		ld a,(de)				; a=SAM pal num
		call palpoke
		inc de					; next pallet #
		djnz 1b
		ret

m3_copypal	; 4 colours but the middle two are swapped
		ld a,($6000)	; a=SAM pal #0
		call palpoke
		ld a,($6002)	; a=SAM pal #2
		call palpoke	;  note: swapped!
		ld a,($6001)	; a=SAM pal #1
		call palpoke	;  note: swapped!
		ld a,($6003)	; a=SAM pal #3
		call palpoke
		ret

palpoke:
		add a      		; a=CLUT offset
		ld hl,clut		; HL=CLUT base
		add a,l			
		ld l,a			; HL=CLUT+offset
		ld a,(hl)		;
		nextreg $44,a
		inc hl
		ld a,(hl)
		nextreg $44,a
		ret
lineints:
		nextreg $62,0	; copr stop
		nextreg $61,0	; copr addr 0
		ld de,$6000+40 	; start of int data
		ld a,(de)		; first value
		cp 255			; end marker?
		ret z			; ret if no lines

lineint:
		nextreg $60,218
		nextreg $60,a	; line
		inc de
		ld a,(de)
		nextreg $60,$40
		nextreg $60,a	; pal#
		inc de
		ld a,(de)
		add a
		ld hl,clut
		add l
		ld l,a
		ld a,(hl)
		nextreg $60,$44
		nextreg $60,a    ; RGB9 hi
		inc hl
		ld a,(hl)
		nextreg $60,$44
		nextreg $60,a    ; RGB9 lo
		inc de  ; colour2 (ignore)
		inc de  ; next line intrpt
		ld a,(de)
		cp 255	; end marker?
		jr nz,lineint

		; PALETTE RESET (IN V-BLANK?)
		ld de,$6000	; paltab
		ld b,16
		ld c,0
		nextreg $60,129		; wait for
		nextreg $60,0 		; line #256

linezero:
		ld a,c
		nextreg $60,$40		; palette index
		nextreg $60,a
		ld hl,clut
		ld a,(de)
		add a
		add l
		ld l,a
		ld a,(hl)
		nextreg $60,$44		; palette value
		nextreg $60,a
		inc hl
		ld a,(hl)
		nextreg $60,$44		; palette value
		nextreg $60,a
		inc c
		inc de
		djnz linezero

		nextreg $60,129		; Copper
		nextreg $60,128		; HALT
		nextreg $61,0		; Copr start
		nextreg $62,%11000000

		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_ossp:		dw 0		; SP backup
_osiy:		dw 0		; IY backup 
_mmu3:		db 0		; MMU3 backup
_mmu4:		db 0		; MMU4 backup
_mmu5:		db 0		; MMU5 backup
_l2bank:	db 18		; Location of Layer 2
_l2bank0:	db 0		; Layer 2 starting bank
_cpuspeed	db 0		; Previous CPU speed
_cmdline:	dw 0		; Reference to CMD line args
_reserved:	dw 0		; handle to our reserved RAM bank
_file		db 0		; esxDOS file handle
_error		db 0		; esxDOS error code
_return:	db 0		; On exit, return with the the carry flag reset if execution was successful.
_mode:		db 0		; SAM screen MODE (3 or 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants and strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
author:		dm "RM83"
clutgap:	ALIGN 256
clut:
;; Colour lookup table
;; SAM 8-Bit "GRBbrGRB" -> ZX Next RGB9
;; (Bright bit gives LSB of each channel
;; as recommended by SNG).
;; RRRGGGBB, PXXXXXXB
			db $00, $00
			db $01, $00
			db $40, $00
			db $41, $00
			db $08, $00
			db $09, $00
			db $48, $00
			db $49, $00
			db $24, $01
			db $25, $01
			db $64, $01
			db $65, $01
			db $2C, $01
			db $2D, $01
			db $6C, $01
			db $6D, $01
			db $02, $00
			db $03, $00
			db $42, $00
			db $43, $00
			db $0A, $00
			db $0B, $00
			db $4A, $00
			db $4B, $00
			db $26, $01
			db $27, $01
			db $66, $01
			db $67, $01
			db $2E, $01
			db $2F, $01
			db $6E, $01
			db $6F, $01
			db $80, $00
			db $81, $00
			db $C0, $00
			db $C1, $00
			db $88, $00
			db $89, $00
			db $C8, $00
			db $C9, $00
			db $A4, $01
			db $A5, $01
			db $E4, $01
			db $E5, $01
			db $AC, $01
			db $AD, $01
			db $EC, $01
			db $ED, $01
			db $82, $00
			db $83, $00
			db $C2, $00
			db $C3, $00
			db $8A, $00
			db $8B, $00
			db $CA, $00
			db $CB, $00
			db $A6, $01
			db $A7, $01
			db $E6, $01
			db $E7, $01
			db $AE, $01
			db $AF, $01
			db $EE, $01
			db $EF, $01
			db $10, $00
			db $11, $00
			db $50, $00
			db $51, $00
			db $18, $00
			db $19, $00
			db $58, $00
			db $59, $00
			db $34, $01
			db $35, $01
			db $74, $01
			db $75, $01
			db $3C, $01
			db $3D, $01
			db $7C, $01
			db $7D, $01
			db $12, $00
			db $13, $00
			db $52, $00
			db $53, $00
			db $1A, $00
			db $1B, $00
			db $5A, $00
			db $5B, $00
			db $36, $01
			db $37, $01
			db $76, $01
			db $77, $01
			db $3E, $01
			db $3F, $01
			db $7E, $01
			db $7F, $01
			db $90, $00
			db $91, $00
			db $D0, $00
			db $D1, $00
			db $98, $00
			db $99, $00
			db $D8, $00
			db $D9, $00
			db $B4, $01
			db $B5, $01
			db $F4, $01
			db $F5, $01
			db $BC, $01
			db $BD, $01
			db $FC, $01
			db $FD, $01
			db $92, $00
			db $93, $00
			db $D2, $00
			db $D3, $00
			db $9A, $00
			db $9B, $00
			db $DA, $00
			db $DB, $00
			db $B6, $01
			db $B7, $01
			db $F6, $01
			db $F7, $01
			db $BE, $01
			db $BF, $01
			db $FE, $01
			db $FF, $01
ERRORTAB:	dw ERR_BIG
			dw ERR_WEE
			dw ERR_END
			dw ERR_MDE

ERR_BIG:	dc "File is too big"
ERR_WEE:	dc "File is too small"
ERR_END:	dc "No end marker"
ERR_MDE:	dc "Parameter error"
P3DOSH:		dm "PLUS3DOS"
USAGE:		dm "SSX v1.0 by Robert Morrison",$0D
			dm "Display SAM Coupe SCREEN$",$0D,$0D
			dm " .ssx FILESPEC [MODE]",$0D,$0D
			dm "MODES:",$0D
			dm " 3   SAM Mode 3",$0D
			dm " 4   SAM Mode 4",$0D
			db 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; More subroutines (moved for ALIGNment)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
l2erase:
		; Clear all Layer 2 banks (9,10,11,12,13)
		; - only really needed for MODE 3 screens
		; becuase we're not filling all of it but
		; it will get displayed, so it needs to
		; be reset.
		ld (stk+1),sp		; backup the SP
		ld bc,PORT_REGSELECT
		ld a,REG_L2BANK
		out (c),a
		inc b				; $253B is NextREG data port
		in a,(c)
		add a				; 8K bank number is 16K number x2
		
		; We need 10x 8K banks for 640x256 mode
		; (Partially unrolled so we don't need 
		;  the stack for nested DJNZ loops).

		; First bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Second bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Third bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Forth bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Fifth bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Sixth bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Seventh bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Eighth bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Ninth bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		; Tenth bank
		nextreg 84,a		; Page bank in at $8000
		ld b,0
		ld d,b
		ld e,b
		ld sp,$A000			; SP = End of Bank
1:		defs 16,$D5			; 16x "PUSH DE" opcodes (moves 32 bytes)
		djnz 1b				; 256x loops (256 x 32 = 8192 bytes)
		inc a				; point to next bank

		ld a,(_l2bank0)
		nextreg 84,a		; page-in first bank again
stk:	ld sp,0				; restore the SP (self-modified)
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
filestat:		defs 11		; esxDOS file status buffer
filespec:		defs 256,0	; user-provided file name

	savebin		"build/ssx",START,$-START
	display		"start: ",START
	display		"stop:  ",clutgap
	display		"CLUT:  ",/H,clut," (alignment ",/D,clut-clutgap," bytes)"
	display		"end:   ",$
	display		"total: ",/D,$-START," bytes."
	end 		START
