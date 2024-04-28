.include "constants.inc"

.segment "ZEROPAGE"
.importzp scrollcount, counter, tileIn, nxtlvl, temp, highbit, lowbit, mindex, myb, mxb, curTile, curNMOff, tile_offset, currNTB, player_x,player_y,animation_tick,animation_state,player_dir,player_offset,mem_offset, ppuscroller,  is_walking, pad1

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


; initialize zero-page values
  LDA #$01
  STA player_dir

  LDA #$01
  STA player_x
  LDA #$0f
	STA player_y
  LDA #$ff
  STA ppuscroller
	LDA #$00
  STA scrollcount
  STA ppuscroller
  STA animation_tick 
  STA animation_state  
  STA player_offset 
  STA mem_offset 
  STA is_walking
  STA tile_offset
  STA curNMOff
  STA curTile
  STA mindex
  STA myb
  STA mxb
  STA lowbit
  STA highbit
  STA temp
  STA nxtlvl
  STA tileIn 
  STA currNTB
  STA counter


  
  JMP main
.endproc
