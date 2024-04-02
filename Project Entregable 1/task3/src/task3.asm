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
mem_offset:		.res 1	
pad1: .res 1



.exportzp player_x,player_y,animation_tick,animation_state,player_dir,player_offset,mem_offset, is_walking, pad1

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


vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
	STA ppuctrl_settings
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  JMP forever
.endproc

.proc update_player
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA
	
	INC animation_tick	

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
  ; Since there are 3 moving states, if less than four keep drawing.
	CMP #$03
	BCC skip_animation

  ; If 4 = Last State, reset/keep same animation

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

.segment "CHR"
.incbin "character2.chr"
