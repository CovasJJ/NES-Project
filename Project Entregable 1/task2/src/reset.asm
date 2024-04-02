.include "constants.inc"

.segment "ZEROPAGE"
.importzp player_x,player_y,animation_tick,animation_state,player_dir,player_offset,mem_offset

.segment "CODE"
.import main
.export reset_handler
.proc reset_handler     
    SEI                 
    CLD                 
    LDX #$40    
    STX $4017
    LDX #$FF
    TXS
    INX                 
    STX $2000           
    STX $2001           
    STX $4010           
    BIT $2002
	vblankwait:             
		BIT $2002
		BPL vblankwait

	vblankwait2:
		BIT $2002
		BPL vblankwait2

	LDA #$00				
	STA player_x
	STA player_y
  STA animation_tick
	STA animation_state	
	STA player_dir
	STA player_offset
	STA mem_offset
	

  JMP main				
.endproc
