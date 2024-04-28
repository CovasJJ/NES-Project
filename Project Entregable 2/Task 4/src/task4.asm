; Task 4 JJC
.include "constants.inc"
.include "header.inc"

.segment "ZEROPAGE"
player_x: 					.res 1	
player_y: 					.res 1	

animation_tick: 	.res 1
animation_state:	 .res 1
player_dir:					.res 1
is_walking:         .res 1 	
player_offset:				.res 1	
ppuctrl_settings: .res 1
ppuscroller: .res 1
scrollcount: .res 1
mem_offset:		.res 1	
tile_offset: .res 1
currNTB: .res 1
curNMOff: .res 1
curTile: .res 1
mindex: .res 1
myb: .res 1
mxb: .res 1
lowbit: .res 1
highbit: .res 1
temp: .res 1
nxtlvl: .res 1
pad1: .res 1
tileIn: .res 1
counter: .res 1



.exportzp scrollcount, tileIn, counter, nxtlvl, temp, lowbit, highbit, myb, mxb, mindex, curTile, curNMOff, currNTB, tile_offset, player_x,player_y,animation_tick,animation_state,player_dir,player_offset,mem_offset, is_walking, pad1, ppuscroller

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

	; read controller
	JSR read_controller1

  ; update tiles *after* DMA transfer
	; and after reading controller state

  ; Move sprites away from screen before each frame.

  JSR move_sprites
	LDA #$00
  STA is_walking
  JSR update_player
  JSR next_level
	LDA ppuscroller
  CMP #255
  BNE set_scroll_positions

  LDA ppuctrl_settings
  EOR #%00000001
  STA ppuctrl_settings
  STA PPUCTRL
  LDA #00
  STA ppuscroller

set_scroll_positions:
  LDA ppuscroller
  STA PPUSCROLL
  LDA #$00
  STA PPUSCROLL

  RTI
.endproc

.import reset_handler

.proc read_controller1
  PHA
  TXA
  PHA
  PHP

  ; write a 1, then a 0, to CONTROLLER1
  ; to latch button states
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA CONTROLLER1

  LDA #%00000001
  STA pad1

get_buttons:
  LDA CONTROLLER1 ; Read next button's state
  LSR A           ; Shift button state right, into carry flag
  ROL pad1        ; Rotate button state from carry flag
                  ; onto right side of pad1
                  ; and leftmost 0 of pad1 into carry flag
  BCC get_buttons ; Continue until original "1" is in carry flag

  PLP
  PLA
  TAX
  PLA
  RTS
.endproc

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

  JSR genLvl1
  JSR genLvl1nm2


vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait


  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
	STA ppuctrl_settings
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  LDA nxtlvl
  CMP #$01
  BNE check2
  
  ; Set Lvl 2 (reset ppuctrl y ppumask)
  LDA #%00000000
  STA PPUCTRL
  STA PPUMASK
  LDA #$01
  STA player_x
  LDA #$0f
	STA player_y
  JSR genLvl2
  JSR genLvl2nm2
  LDA #$00
  STA nxtlvl
  LDA #$00
  STA ppuscroller
  LDA #$00
  STA scrollcount
  
  JMP vblankwait

; Set lvl 1
check2:
  LDA nxtlvl
  CMP #$02
  BNE loopever
  
  LDA PPUSTATUS
  LDA #%00000000
  STA PPUCTRL
  STA PPUMASK
  LDA #$01
  STA player_x
  LDA #$0f
	STA player_y
  JSR genLvl1
  JSR genLvl1nm2
  LDA #$00
  STA nxtlvl
  LDA #$00
  STA ppuscroller
  LDA #$00
  STA scrollcount
  
  JMP vblankwait

  
  loopever:
  JMP forever

.endproc

.proc move_sprites
    LDX #$00            
  loop:
  ; Set Y value of sprites to 255 in order to get them off the screen
    LDA #$ff
    STA $0200, X
    INX
    INX
    INX
    INX 
    ;Check if it already moved all sprites.
    CPX #$00
    BNE loop
    RTS
.endproc

; Attempt to draw all tiles
.proc genLvl1
  PHP  
  PHA 
  TXA
  PHA
  TYA
  PHA

  loopIndex:

    LDA mindex 
    LSR      
    LSR       
    STA myb     
    LDA mindex
    AND #$03    
    STA mxb     
    
   ; Obtain MYb by shifting Mindex two times to the right (/4). 
    LDA mindex
    LSR 
    LSR 
    STA myb

    ; Obtain MXb by Mindex %4 (AND 0x03)
    LDA mindex
    AND #%00000011
    STA mxb

    ; Obtain highbit by MYb >> 2 && 0x03
    LDA myb
    LSR
    LSR
    AND #%00000011
    STA highbit

    ; Obtain lowbit by MXb << 3 + MYb << 6
    LDA mxb
    ASL
    ASL
    ASL
    STA temp
    LDA myb
    ASL
    ASL
    ASL
    ASL
    ASL
    ASL
    CLC
    ADC temp  
    STA lowbit

    JSR drawlvl1NM1

    ; Increase m_index
    LDX mindex
    INX 
    STX mindex

    ; Check if in last megaindex
    LDX mindex
    CPX #$3c
    BNE loopIndex
    LDA #$00
    STA mindex
  
    ; Load Attribute Table for NM1 in Stage 1
    LDA PPUSTATUS
    LDA #$23
    STA PPUADDR
    LDA #$c0
    STA PPUADDR

    LDX #$00
    loopATR1:
    LDA lvl1nm1Atr, X
    STA PPUDATA
    INX
    CPX #$40
    BNE loopATR1
  

  PLA 
  TAY 
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc genLvl1nm2
  PHP  
  PHA 
  TXA
  PHA
  TYA
  PHA

  loopIndex:
    
  ; Obtain MYb by shifting Mindex two times to the right (/4). 
  LDA mindex
  LSR 
  LSR 
  STA myb

  ; Obtain MXb by Mindex %4 (AND 0x03)
  LDA mindex
  AND #%00000011
  STA mxb

  ; Obtain highbit by MYb >> 2 && 0x03
  LDA myb
  LSR
  LSR
  AND #%00000011
  STA highbit

  ; Obtain lowbit by MXb << 3 + MYb << 6
  LDA mxb
  ASL
  ASL
  ASL
  STA temp
  LDA myb
  ASL
  ASL
  ASL
  ASL
  ASL
  ASL
  CLC
  ADC temp  
  STA lowbit

  JSR drawlvl1NM2

  ; Increase m_index
  LDX mindex
  INX 
  STX mindex

  ; Check if in last megaindex
  LDX mindex
  CPX #$3c
  BNE loopIndex
  LDA #$00
  STA mindex
  
  ; Load Attribute Table for NM2 in Stage 1
  LDA PPUSTATUS
  LDA #$27
  STA PPUADDR
  LDA #$c0
  STA PPUADDR

  LDX #$00
  loopATR2:
  LDA lvl1nm2Atr, X
  STA PPUDATA
  INX
  CPX #$40
  BNE loopATR2
  

  PLA 
  TAY 
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

; Draw name tables 1 and 2
.proc drawlvl1NM1
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX #$00

  LDY mindex
  LDA lvl1nm1, Y
  STA currNTB

  loopMegaTile:

    LDA currNTB  
    AND #%11000000   
    STA curNMOff 

    JSR selectTilelvl1

    ; Top Left Tile (Lowbit)
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR   
    LDY lowbit
    STY PPUADDR   
    LDY tileIn
    STY PPUDATA


    ; Top Right Tile (Lowbit + 1 )
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR     
    LDA lowbit   
    CLC
    ADC #$01             
    STA PPUADDR
    LDA tileIn
    STA PPUDATA


    ; Bottom Left Tile (Lowbit + 32 = HEX $20)
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR   
    LDA lowbit
    CLC
    ADC #$20      
    STA PPUADDR   
    LDA tileIn
    STA PPUDATA


    ; Bottom Right tile (Lowbit + 33 = HEX $21) 
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR 
    LDA lowbit
    CLC
    ADC #$21    
    STA PPUADDR  
    LDA tileIn
    STA PPUDATA

    ; Shift current byte 2 times to the left to get the next megatile value.
    LDA currNTB
    ASL
    ASL
    STA currNTB


    ; Increase lowbit to move to the next megatile.
    LDA lowbit
    CLC
    ADC #$02
    STA lowbit

    ; Loop until we draw the four megatiles in the byte.
    INX                 
    CPX #$04            
    BNE loopMegaTile  


    ; Reset 0
    LDX #$00

  PLA 
  TYA
  PLA
  TAX 
  PLA 
  PLP 
  RTS 
.endproc

; Draw Lvl1 NM2 
.proc drawlvl1NM2
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX #$00

  LDY mindex
  LDA lvl1nm2, Y
  STA currNTB

  loopMegaTile:

    LDA currNTB  
    AND #%11000000   
    STA curNMOff 

    JSR selectTilelvl1

    ; Top Left Tile (Lowbit + 1)
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR   
    LDY lowbit
    STY PPUADDR   
    LDY tileIn
    STY PPUDATA


    ; Top Right Tile (Lowbit + 1 )
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR     
    LDA lowbit   
    CLC
    ADC #$01             
    STA PPUADDR
    LDA tileIn
    STA PPUDATA


    ; Bottom Left Tile (Lowbit + 32 = HEX $20)
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR   
    LDA lowbit
    CLC
    ADC #$20      
    STA PPUADDR   
    LDA tileIn
    STA PPUDATA


    ; Bottom Right tile (Lowbit + 33 = HEX $21) 
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR 
    LDA lowbit
    CLC
    ADC #$21    
    STA PPUADDR  
    LDA tileIn
    STA PPUDATA

    ; Shift current byte 2 times to the left to get the next megatile value.
    LDA currNTB
    ASL
    ASL
    STA currNTB


    ; Increase lowbit to move to the next megatile.
    LDA lowbit
    CLC
    ADC #$02
    STA lowbit

     ; Loop until we draw the four megatiles in the byte.
    INX                 
    CPX #$04            
    BNE loopMegaTile  
    ; Reset 0
    LDX #$00

  PLA 
  TYA
  PLA
  TAX 
  PLA 
  PLP 
  RTS 
.endproc

; Select Tile Index for Level 1
.proc selectTilelvl1
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  ; Check if background 00
  LDA curNMOff
  CMP #%00000000
  BEQ backgroundTile
  
  ; Check if steel 10
  LDA curNMOff
  CMP #%01000000
  BEQ steelTile
  ; Check if wood 01
  LDA curNMOff
  CMP #%10000000
  BEQ woodTile

  ; Check if glass 11
  LDA curNMOff
  CMP #%11000000
  BEQ glassTile

  ; Set tileIn to the desired tile index.
  backgroundTile:
  LDA #$00
  STA tileIn
  JMP tileSet

  steelTile:
  LDA #$01
  STA tileIn
  JMP tileSet

  woodTile:
  LDA #$02
  STA tileIn
  JMP tileSet

  glassTile: 
  LDA #$03
  STA tileIn
  JMP tileSet

  tileSet: 

  PLA 
  TYA
  PLA
  TAX 
  PLA 
  PLP 
  RTS
.endproc

; Select Tile Index for Level 2
.proc selectTilelvl2
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  ; Check if background 00
  LDA curNMOff
  CMP #%00000000
  BEQ backgroundTile
  
  ; Check if caramel 10
  LDA curNMOff
  CMP #%10000000
  BEQ caramelTile

  ; Check if chocolate 01
  LDA curNMOff
  CMP #%01000000
  BEQ chocoTile

  
  ; Check if candydots 11
  LDA curNMOff
  CMP #%11000000
  BEQ candyTile

  ; Set tileIn to the desired tile index.
  backgroundTile:
  LDA #$00
  STA tileIn
  JMP tileSet

  caramelTile:
  LDA #$05
  STA tileIn
  JMP tileSet

  chocoTile:
  LDA #$06
  STA tileIn
  JMP tileSet

  candyTile: 
  LDA #$08
  STA tileIn
  JMP tileSet

  tileSet: 

  PLA 
  TYA
  PLA
  TAX 
  PLA 
  PLP 
  RTS
.endproc

; Draw Level 2

.proc genLvl2
  PHP  
  PHA 
  TXA
  PHA
  TYA
  PHA

  loopIndex:

    LDA mindex 
    LSR      
    LSR       
    STA myb     
    LDA mindex
    AND #$03    
    STA mxb     
    
   ; Obtain MYb by shifting Mindex two times to the right (/4). 
    LDA mindex
    LSR 
    LSR 
    STA myb

    ; Obtain MXb by Mindex %4 (AND 0x03)
    LDA mindex
    AND #%00000011
    STA mxb

    ; Obtain highbit by MYb >> 2 && 0x03
    LDA myb
    LSR
    LSR
    AND #%00000011
    STA highbit

    ; Obtain lowbit by MXb << 3 + MYb << 6
    LDA mxb
    ASL
    ASL
    ASL
    STA temp
    LDA myb
    ASL
    ASL
    ASL
    ASL
    ASL
    ASL
    CLC
    ADC temp  
    STA lowbit

    JSR drawlvl2NM1

    ; Increase m_index
    LDX mindex
    INX 
    STX mindex

    ; Check if in last megaindex
    LDX mindex
    CPX #$3c
    BNE loopIndex
    LDA #$00
    STA mindex
  
    ; Load Attribute Table for NM1 in Stage 1
    LDA PPUSTATUS
    LDA #$23
    STA PPUADDR
    LDA #$c0
    STA PPUADDR

    LDX #$00
    loopATR1:
    LDA lvl2nm1Atr, X
    STA PPUDATA
    INX
    CPX #$40
    BNE loopATR1
  

  PLA 
  TAY 
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc drawlvl2NM1
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX #$00

  LDY mindex
  LDA lvl2nm1, Y
  STA currNTB

  loopMegaTile:

    LDA currNTB  
    AND #%11000000   
    STA curNMOff 

    JSR selectTilelvl2

    ; Top Left Tile (Lowbit + 1)
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR   
    LDY lowbit
    STY PPUADDR   
    LDY tileIn
    STY PPUDATA


    ; Top Right Tile (Lowbit + 1 )
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR     
    LDA lowbit   
    CLC
    ADC #$01             
    STA PPUADDR
    LDA tileIn
    STA PPUDATA


    ; Bottom Left Tile (Lowbit + 32 = HEX $20)
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR   
    LDA lowbit
    CLC
    ADC #$20      
    STA PPUADDR   
    LDA tileIn
    STA PPUDATA


    ; Bottom Right tile (Lowbit + 33 = HEX $21) 
    LDA highbit
    CLC 
    ADC #$20
    STA PPUADDR 
    LDA lowbit
    CLC
    ADC #$21    
    STA PPUADDR  
    LDA tileIn
    STA PPUDATA

    ; Shift current byte 2 times to the left to get the next megatile value.
    LDA currNTB
    ASL
    ASL
    STA currNTB


    ; Increase lowbit to move to the next megatile.
    LDA lowbit
    CLC
    ADC #$02
    STA lowbit

    ; Loop until we draw the four megatiles in the byte.
    INX                 
    CPX #$04            
    BNE loopMegaTile  


    ; Reset 0
    LDX #$00

  PLA 
  TYA
  PLA
  TAX 
  PLA 
  PLP 
  RTS 
.endproc

.proc genLvl2nm2
  PHP  
  PHA 
  TXA
  PHA
  TYA
  PHA

  loopIndex:

    LDA mindex 
    LSR      
    LSR       
    STA myb     
    LDA mindex
    AND #$03    
    STA mxb     
    
   ; Obtain MYb by shifting Mindex two times to the right (/4). 
    LDA mindex
    LSR 
    LSR 
    STA myb

    ; Obtain MXb by Mindex %4 (AND 0x03)
    LDA mindex
    AND #%00000011
    STA mxb

    ; Obtain highbit by MYb >> 2 && 0x03
    LDA myb
    LSR
    LSR
    AND #%00000011
    STA highbit

    ; Obtain lowbit by MXb << 3 + MYb << 6
    LDA mxb
    ASL
    ASL
    ASL
    STA temp
    LDA myb
    ASL
    ASL
    ASL
    ASL
    ASL
    ASL
    CLC
    ADC temp  
    STA lowbit

    JSR drawlvl2NM2

    ; Increase m_index
    LDX mindex
    INX 
    STX mindex

    ; Check if in last megaindex
    LDX mindex
    CPX #$3c
    BNE loopIndex
    LDA #$00
    STA mindex
  
    ; Load Attribute Table for NM2 in Stage 1
    LDA PPUSTATUS
    LDA #$27
    STA PPUADDR
    LDA #$c0
    STA PPUADDR

    LDX #$00
    loopATR2:
    LDA lvl2nm2Atr, X
    STA PPUDATA
    INX
    CPX #$40
    BNE loopATR2
  

  PLA 
  TAY 
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc drawlvl2NM2
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX #$00

  LDY mindex
  LDA lvl2nm2, Y
  STA currNTB

  loopMegaTile:

    LDA currNTB  
    AND #%11000000   
    STA curNMOff 

    JSR selectTilelvl2

    ; Top Left Tile (Lowbit + 1)
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR   
    LDY lowbit
    STY PPUADDR   
    LDY tileIn
    STY PPUDATA


    ; Top Right Tile (Lowbit + 1 )
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR     
    LDA lowbit   
    CLC
    ADC #$01             
    STA PPUADDR
    LDA tileIn
    STA PPUDATA


    ; Bottom Left Tile (Lowbit + 32 = HEX $20)
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR   
    LDA lowbit
    CLC
    ADC #$20      
    STA PPUADDR   
    LDA tileIn
    STA PPUDATA


    ; Bottom Right tile (Lowbit + 33 = HEX $21) 
    LDA highbit
    CLC 
    ADC #$24
    STA PPUADDR 
    LDA lowbit
    CLC
    ADC #$21    
    STA PPUADDR  
    LDA tileIn
    STA PPUDATA

    ; Shift current byte 2 times to the left to get the next megatile value.
    LDA currNTB
    ASL
    ASL
    STA currNTB


    ; Increase lowbit to move to the next megatile.
    LDA lowbit
    CLC
    ADC #$02
    STA lowbit

     ; Loop until we draw the four megatiles in the byte.
    INX                 
    CPX #$04            
    BNE loopMegaTile  

    
    ; Reset 0
    LDX #$00

  PLA 
  TYA
  PLA
  TAX 
  PLA 
  PLP 
  RTS 
.endproc

.proc next_level
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA pad1
  AND #BTN_B
  BEQ checkA

  LDA #$01
  STA nxtlvl

  JMP skip

  checkA:
  LDA pad1
  AND #BTN_A
  BEQ skip

  LDA #$02
  STA nxtlvl

  skip:

  PLA 
  TYA
  PLA
  TAX 
  PLA 
  PLP 
  RTS 
.endproc

.proc update_player
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA

  LDA animation_tick	
	CLC			
	ADC #$01   	
	STA animation_tick	

  LDA pad1        ; Load button presses
  AND #BTN_LEFT   ; Filter out all but Left
  BEQ check_right ; If result is zero, left not pressed

  DEC player_x  ; If the branch is not taken, move player left
  LDA #$27
  STA player_dir
  LDA #$01
  STA is_walking
  JMP done_checking

check_right:
  LDA pad1
  AND #BTN_RIGHT
  BEQ check_up
  INC player_x
  LDA #$21
  STA player_dir
  LDA #$01
  STA is_walking
  LDA scrollcount
  CMP #$ff
  BEQ done_checking

  INC ppuscroller
  INC scrollcount

  JMP done_checking

check_up:
  LDA pad1
  AND #BTN_UP
  BEQ check_down
  DEC player_y
  LDA #$07
  STA player_dir
  LDA #$01
  STA is_walking
  JMP done_checking
  
check_down:
  LDA pad1
  AND #BTN_DOWN
  BEQ done_checking
  INC player_y
  LDA #$01
  STA player_dir
  LDA #$01
  STA is_walking


  JMP done_checking

check_b:
  LDA pad1
  AND #BTN_B
  BEQ done_checking
  ; CHANGE LEVEL (Didnt work)
  ; LDA $01
  ; STA nxtlvl

not_moving:
  LDA #$00
  STA is_walking
  sta player_offset
  LDA #$01
  STA player_dir


done_checking:
  JSR animation
  

  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc animation 
	; save registers
	PHP
	PHA
	TXA
	PHA
	TYA	
	PHA	

  ; Check if player is walking
  LDA is_walking
  CMP #$00
  BEQ keepAnimation

	LDA animation_tick					
	CMP #$0a
	BCC skip_animation			; If the tick is less than 10, skip.				


	LDA #$00
	STA animation_tick				; Reset tick amount back to 0

	INC animation_state 		; Increase animation_state by 1.

   LDA player_offset
   CLC
   ADC #$02
   STA player_offset

  ; Check animation states
	LDA animation_state
  ; Since there are 4 states, if less than four keep drawing.
	CMP #$03
	BCC skip_animation

  ; If 4, reset/keep same animation

  keepAnimation:
  LDA #$00
  STA animation_state
	STA player_offset
			  
	skip_animation:
	JSR draw_player      

    
	PLA	
	TAY	
	PLA	
	TAX	
	PLA	
	PLP	
	RTS	
.endproc

.proc draw_player	
	; save registers
	PHP	
	PHA	
	TXA	
	PHA	
	TYA	
	PHA	

; Setting Up Location of Tiles with Offset
	LDA player_dir
	CLC
	ADC player_offset
	STA player_dir

  ; Loading X with the sprite offset for the location.
	LDX mem_offset			

	; Top Left Tile
	LDA player_y           ; Set Y
	STA $0200, X        
	LDA player_dir      ; Set Tile 
	STA $0201, X      
	LDA #$01            ; Set Palette (01)
	STA $0202, X        
	LDA player_x           ; Set X
	STA $0203, X  

	; Top Right Tile of Player
	LDA player_y          ; Set Y
	STA $0204, X    
	LDA player_dir  ; Set Tile  (+1 from top left)
	CLC
	ADC #$01     
	STA $0205, X        
	LDA #$01            ; Set Palette (01)
	STA $0206, X         
	LDA player_x           ; Set X (+8)
	CLC
	ADC #$08
	STA $0207, X 

	; Bottom Let Tile of Player
	LDA player_y         ; Set Y (+8)
	CLC
	ADC #$08
	STA $0208, X        
	LDA player_dir   ; Set Tile  ($10 or 16 from top left)
	CLC
	ADC #$10
	STA $0209, X      
	LDA #$01            ; Set Palette (01)
	STA $020A, X         
	LDA player_x           ; Set X
	STA $020B, X

	; Bottom Right Tile of Player
	LDA player_y		         ; Set Y (+8)
	CLC
	ADC #$08
	STA $020C, X        
	LDA player_dir       ; Set Tile  ($11 or 17 from top left)
	CLC
	ADC #$11
	STA $020D, X      
	LDA #$01                 ; Set Palette (01)
	STA $020E, X         
	LDA player_x			         ; Set X (+8)
	CLC
	ADC #$08
	STA $020F, X

  ; Increase the Offset for the location of Sprites.
	LDA mem_offset
	CLC
	ADC #$10
	STA mem_offset 			 

	
	PLA	
	TAY	
	PLA	
	TAX	
	PLA	
	PLP	
	RTS	
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

; Down here we store ALL the name tables and their attributes. 
; Level 1 Screen 1
lvl1nm1:
.byte %01010101, %01010101, %01010101, %01010101
.byte %00001000, %00000000, %00101100, %00000000
.byte %01001011, %10101010, %00101010, %10100010
.byte %01001011, %10100010, %00100000, %00100010
.byte %01001011, %11100000, %00100010, %00100000
.byte %01001010, %11100010, %10100010, %00100010
.byte %01000000, %00100010, %11110010, %00100000
.byte %01001010, %10100011, %11101010, %00100010
.byte %01001011, %11101010, %11111000, %00100010
.byte %01001111, %11000000, %00001011, %10100000
.byte %01001010, %10101010, %11101011, %11100010
.byte %01001000, %00001110, %00100011, %00100010
.byte %01000000, %10101011, %00000010, %00000000
.byte %01010101, %01010101, %01010101, %01010101
.byte %00000000, %00000000, %00000000, %00000000

; Level 1 Screen 1 Attributes
lvl1nm1Atr: 

.byte %00000000, %00010000, %00000000, %00000000, %01000000, %00110000, %00000000, %00000000
.byte %00000000, %11011101, %01010101, %01010101, %01000100, %01010101, %01010101, %01000100
.byte %00000000, %01011101, %01110111, %01000000, %01010100, %01000100, %01000100, %01000000
.byte %00000000, %01010000, %01010100, %11000100, %01111111, %01010100, %01000100, %01000000
.byte %00000000, %11111101, %00110111, %00000101, %00001111, %11010001, %01010100, %00000100
.byte %00000000, %00010101, %00000101, %01110101, %01000111, %11001101, %01000111, %01000100
.byte %00000000, %00000000, %00000101, %00001101, %00000000, %00000100, %00000000, %00000000
.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000


; Level 1 Screen 2 
lvl1nm2:
	.byte %01010101, %01010101, %01010101, %01010101
	.byte %00000000, %00101000, %00000000, %10101101
	.byte %00000000, %00101000, %00000000, %10100001
	.byte %10101110, %00001000, %10001000, %00000001
	.byte %10000010, %10001011, %10001010, %10100001
	.byte %10001010, %00001111, %10001000, %00000001
	.byte %00000000, %10001110, %10111000, %10101001
	.byte %10001000, %10000000, %10000011, %00000001
	.byte %10001010, %10101000, %10000010, %10100001
	.byte %00001111, %10101010, %10100010, %00000001
	.byte %10001000, %10000000, %00100010, %11101001
	.byte %10001000, %10111010, %00100010, %11000000
	.byte %00001000, %00000010, %00000010, %10101001
	.byte %01010101, %01010101, %01010101, %01010101
	.byte %00000000, %00000000, %00000000, %00000000

; Level 1 Screen 2 Attributes
lvl1nm2Atr: 

.byte %01010000, %00000000, %01000000, %00010000, %00000000, %00000000, %01010000, %00110000
.byte %01010000, %01110000, %00000100, %00010001, %00010000, %00010000, %00000101, $00000011
.byte %00010001, %01010100, %00000001, %11111101, %00010001, %00010101, %00000101, %00000000
.byte %00010000, %01010000, %00010001, %00000111, %00011101, %11000001, %00000101, %00000001
.byte %00000001, %11110101, %01010101, %01010001, %01010001, %01000100, %00000101, %00000000
.byte %00010001, %00010001, %11010001, %01010000, %01000100, %01000100, %00110111, %00000001
.byte %00000000, %00000001, %00000000, %00000100, %00000000, %00000100, %00000101, %00000001
.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000


; Level 2 Screen 1 
lvl2nm1:
	.byte %01010101, %01010101, %01010101, %01010101
	.byte %00000000, %10101000, %00001100, %10001100
	.byte %01001000, %00100000, %10101000, %10001000
  .byte %01001000, %00100000, %10101000, %10001000
	.byte %01001010, %00100010, %10111000, %00001000
	.byte %01001000, %00111110, %11111010, %10001010
	.byte %01001000, %10101010, %10001000, %00001000
	.byte %01001100, %10000000, %00001000, %10000000
	.byte %01001100, %00001010, %00101000, %10001010
	.byte %01001010, %10101010, %11101111, %11000010
	.byte %01000010, %00000010, %00101111, %11100010
	.byte %01001010, %00100010, %00101000, %10100010
	.byte %01000000, %00100000, %00001100, %10000000
	.byte %01010101, %01010101, %01010101, %01010101
	.byte %00000000, %00000000, %00000000, %00000000

; Level 2 Screen 1 Attributes
lvl2nm1Atr: 
  .byte %00001010, %00001010, %01011010, %00011010, %00001010, %00111010, %00011010, %00111010
  .byte %00100010, %00010001, %01000101, %00000001, %01010101, %00010001, %00010001, %00010001
  .byte %00100010, %00010101, %11000100, %01110100, %11111101, %01010001, %00010000, %01010001
  .byte %00100010, %00110001, %00010101, %00000101, %00000001, %00010001, %00010000, %00000001
  .byte %00100010, %01010011, %01010000, %01010101, %01110100, %11110001, %00110001, %01000101
  .byte %00100010, %01010100, %01000000, %01000100, %01000100, %00011111, %01010111, %01000100
  .byte %10100010, %10100000, %10100100, %10100000, %10100000, %10100011, %10100001, %10100000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000



; Level 2 Screen 2 
lvl2nm2:
	.byte %01010101, %01010101, %01010101, %01010101
	.byte %10000000, %10000000, %10000000, %00000001
  .byte %10000000, %10000000, %10000000, %00000001
	.byte %10001000, %10001000, %10001010, %10101101
	.byte %00000000, %10001000, %00000010, %00101001
	.byte %10101000, %00001000, %10100011, %00000001
	.byte %00001000, %10000000, %10000010, %00100001
	.byte %10001000, %10101010, %10100010, %00110001
	.byte %10001000, %11000010, %11110010, %10100001
	.byte %10000000, %10000010, %11101010, %00111101
	.byte %10001010, %10100010, %00000010, %00101101
	.byte %10000000, %10111110, %00100010, %00100001
	.byte %11001000, %00111011, %00100000, %00100000
	.byte %01010101, %01010101, %01010101, %01010101
	.byte %00000000, %00000000, %00000000, %00000000

; Level 2 Screen 2 Attributes
lvl2nm2Atr: 
  .byte %00011010, %00001010, %00011010, %00001010, %01011010, %00001010, %00011010, %10001010
  .byte %00010001, %00010000, %00010001, %00010000, %00010001, %01010000, %01010000, %10111000
  .byte %01010000, %00010000, %00000001, %00010001, %01010000, %11000100, %00000100, %10001001
  .byte %00010000, %00010001, %01010001, %01010000, %01010001, %01000100, %11000100, %10001000
  .byte %00010001, %00000001, %00010011, %01000100, %01111111, %01010100, %11000101, %10111000
  .byte %00010001, %00000101, %11010101, %01110100, %01000000, %01000100, %01000100, %10001011
  .byte %10100011, %10100001, %10101100, %10101101, %10100100, %10100000, %10100100, %10100000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000


.segment "CHR"
.incbin "character2.chr"
