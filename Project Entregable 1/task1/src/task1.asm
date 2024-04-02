.include "constants.inc"
.include "header.inc"

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
	LDA #$00
	STA $2005
	STA $2005
  RTI
.endproc

.import reset_handler

.export main
.proc main
  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes

  ; write sprite data
  LDX #$00
load_sprites:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$c0
  BNE load_sprites

	; write nametables
	; background tiles
	;Wood
	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ec
	STA PPUADDR
	LDX #$02
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$cc
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ed
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$cd
	STA PPUADDR
	STX PPUDATA

	; Glass
	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ee
	STA PPUADDR
	LDX #$03
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ce
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$cf
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ef
	STA PPUADDR
	STX PPUDATA

	; Rock

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ac
	STA PPUADDR
	LDX #$04
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ad
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$8c
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$8d
	STA PPUADDR
	STX PPUDATA
	
	; Floor1 

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$ae
	STA PPUADDR
	LDX #$09
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$af
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$8e
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$8f
	STA PPUADDR
	STX PPUDATA

	; Choco
	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$d0
	STA PPUADDR
	LDX #$06
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$d1
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$f0
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$f1
	STA PPUADDR
	STX PPUDATA

	; Candy Dots
	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$d2
	STA PPUADDR
	LDX #$08
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$d3
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$f2
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$f3
	STA PPUADDR
	STX PPUDATA

	; Caramel Wall
	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$90
	STA PPUADDR
	LDX #$05
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$91
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$b0
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$b1
	STA PPUADDR
	STX PPUDATA

	; Candy Floor
	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$b2
	STA PPUADDR
	LDX #$07
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$b3
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$93
	STA PPUADDR
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$22
	STA PPUADDR
	LDA #$92
	STA PPUADDR
	STX PPUDATA

	; finally, attribute table
	LDA PPUSTATUS
	LDA #$23
	STA PPUADDR
	LDA #$eb
	STA PPUADDR
	LDA #%11011000
	STA PPUDATA

	LDA PPUSTATUS
	LDA #$23
	STA PPUADDR
	LDA #$ec
	STA PPUADDR
	LDA #%11011110
	STA PPUDATA

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  JMP forever
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:
.byte $0f, $2d, $00, $10
.byte $0f, $17, $27, $07
.byte $0f, $27, $37, $17
.byte $0f, $20, $25, $31

.byte $0f, $2d, $10, $15
.byte $0f, $15, $01, $2B
.byte $0f, $19, $09, $29
.byte $0f, $19, $09, $29

sprites:
.byte $70, $01, $01, $80
.byte $70, $02, $01, $88
.byte $78, $11, $01, $80
.byte $78, $12, $01, $88

.byte $70, $03, $01, $70
.byte $70, $04, $01, $78
.byte $78, $13, $01, $70
.byte $78, $14, $01, $78

.byte $70, $05, $01, $60
.byte $70, $06, $01, $68
.byte $78, $15, $01, $60
.byte $78, $16, $01, $68 

.byte $70, $23, $01, $90
.byte $70, $24, $01, $98
.byte $78, $33, $01, $90
.byte $78, $34, $01, $98
;
.byte $80, $07, $01, $80
.byte $80, $08, $01, $88
.byte $88, $17, $01, $80
.byte $88, $18, $01, $88

.byte $80, $09, $01, $70
.byte $80, $0a, $01, $78
.byte $88, $19, $01, $70
.byte $88, $1a, $01, $78

.byte $80, $0b, $01, $60
.byte $80, $0c, $01, $68
.byte $88, $1b, $01, $60
.byte $88, $1c, $01, $68 

.byte $80, $21, $01, $90
.byte $80, $22, $01, $98
.byte $88, $31, $01, $90
.byte $88, $32, $01, $98
;
.byte $60, $0d, $01, $90
.byte $60, $0e, $01, $98
.byte $68, $1d, $01, $90
.byte $68, $1e, $01, $98

.byte $60, $25, $01, $70
.byte $60, $26, $01, $78
.byte $68, $35, $01, $70
.byte $68, $36, $01, $78

.byte $60, $27, $01, $60
.byte $60, $28, $01, $68
.byte $68, $37, $01, $60
.byte $68, $38, $01, $68 

.byte $60, $29, $01, $80
.byte $60, $2a, $01, $88
.byte $68, $39, $01, $80
.byte $68, $3a, $01, $88


.segment "CHR"
.incbin "character.chr"
