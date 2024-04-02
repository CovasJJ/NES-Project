.include "constants.inc"
.include "header.inc"
.segment "ZEROPAGE"
	player_x: .res 1	
	player_y: .res 1	

	animation_tick: .res 1
	animation_state: .res 1
	player_dir:	.res 1	
	player_offset: .res 1	
	mem_offset:	.res 1	

.exportzp player_x,player_y,animation_tick,animation_state,player_dir,player_offset,mem_offset
.segment "CODE"
.proc irq_handler       
    RTI                 
.endproc

.proc nmi_handler       
    LDA #$00  
    STA OAMADDR 
                       
    LDA #$02 
    STA OAMDMA  

  ; Update tiles DMA transfer
	JSR tick_updater
	JSR set_players	

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
        LDA palettes, X     
        STA PPUDATA           
        INX                 
        CPX #$20            
        BNE load_palettes   
    
	vblankwait:         	; wait for another vblank before continuing
        BIT PPUSTATUS      	
        BPL vblankwait

        LDA #%10010000  	; turn on NMIs, sprites use first pattern table
        STA PPUCTRL       	
        LDA #%00011110  	; turn on screen
        STA PPUMASK       	

    forever:
        JMP forever
.endproc

.proc tick_updater 	
	; Add 1 to the tick value.	
	INC animation_tick	
.endproc

.proc set_players 
	; save registers
	PHP	
	PHA	
	TXA	
	PHA	
	TYA	
	PHA	

   ; Set player Down
   LDA #$01          		
   STA player_dir   	; Set dir to $01 (First tile of player Up)

  ; Load X and Y coordinates of player and then call the animation. 
  LDA #$a0           		
  STA player_x				
  LDA #$a0          		
  STA player_y
  JSR animation		

  ; Set player Up
  LDA #$07
  STA player_dir  	; Set dir to $07 (Tiles of player Up)

  ; Load X and Y coordinates of player and then call the animation.
  LDA #$80
  STA player_x			
  LDA #$a0 
  STA player_y		
  JSR animation		

  ; Set player Right
  LDA #$21          		
  STA player_dir   	; Set dir to $21 (Right Tiles)

  ; Load X and Y coordinates of player and then call the animation.
  LDA #$a0           		
  STA player_x			
  LDA #$80         		
  STA player_y		
  JSR animation		


  ; Set player Left
  LDA #$27          	
  STA player_dir		; Set dir to $27(Left Tiles)

  ; Load X and Y coordinates of player and then call the animaton.
  LDA #$80
  STA player_x				
  LDA #$80
  STA player_y
  JSR animation	

	
  PLA	
  TAY	
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

	LDA animation_tick					
	CMP #$14
	BNE skip_animation			; If the tick is not at the number 20, skip.				

	LDA #$00
	STA animation_tick				; Reset tick amount back to 0,
	
	INC animation_state 		; Increase animation_state by 1.

  ; Check animation states
	LDA animation_state
  ; Go to next states if animation_state is either 1 or 3.
	CMP #$01
	BEQ nextState		
	CMP #$03
	BEQ nextState	

  ; When animation_state reaches 5, go to resetState.
	CMP #$05
	BEQ resetState		
	JMP skip_animation

	resetState:
	LDA player_offset
	CLC
	; Reset back the offset to original direction
	; Set animation_state to 0.
	SBC #$03   
	STA player_offset
	LDA #$00
	STA animation_state 	
	JMP skip_animation

		
	nextState:
	LDA player_offset
	CLC
	ADC #$02    
  ; Skips two in order to reach the next set of tiles for the next state
	STA player_offset	
	
  ; Increments animation_state by 1
	INC animation_state 
			  
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

.segment "CHR"
.incbin "character2.chr"
