;; To assemble:
;;   64tass --nostart -Wall --output rogue.o rogue.asm

  * = $2000

  osrdch = $ffe0
  oswrch = $ffee
  osword = $fff1
  osbyte = $fff4

  tile_floor     = 224
  tile_corner_tl = 225
  tile_corner_tr = 226
  tile_corner_bl = 227
  tile_corner_br = 228
  tile_edge_h    = 229
  tile_edge_v    = 230

  screen_mode  = 4
  screen_width = 40

  map_width  = 80
  map_height = 31

  room_min_width  = 4
  room_min_height = 4

  ;; --------------------------------------------------------------------------
  ;; Zero page workspace.
  ;; --------------------------------------------------------------------------   
  temp    = $70
  ptr     = $71

  ;; --------------------------------------------------------------------------
  ;; Main program, entry point.
  ;; --------------------------------------------------------------------------
main
  jsr init
- jsr clear_map
  jsr generate_map
  ldx #0
  jsr draw_map
  jsr osrdch
  jmp -

  ;; --------------------------------------------------------------------------
  ;; Initializes the program.
  ;; --------------------------------------------------------------------------
init
  jsr init_vdu
  jsr srand
  rts

  ;; --------------------------------------------------------------------------
  ;; Initializes the VDU.
  ;; --------------------------------------------------------------------------
init_vdu
  ldx #0
- ldy data_vdu,x
  beq +
  inx

- lda data_vdu,x
  jsr oswrch
  inx  
  dey
  bne -
  jmp --

+ rts
  
  ;; --------------------------------------------------------------------------
  ;; VDU initialization commands.
  ;; Format is: first byte (=N), then N bytes. Repeat as desired, end with N=0.
  ;; --------------------------------------------------------------------------
data_vdu
  ; Set the screen MODE.
  .byte 2
  .byte 22, screen_mode

  ; Disable the cursor.
  .byte 10
  .byte 23, 1, 0, 0, 0, 0, 0, 0, 0, 0

  ; Floor tile.
  .byte 10
  .byte 23, tile_floor 
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00001000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  ; Top left corner tile.
  .byte 10
  .byte 23, tile_corner_tl
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00001111
  .byte %00011111
  .byte %00011100
  .byte %00011000
  .byte %00011000

  ; Top right corner tile.
  .byte 10
  .byte 23, tile_corner_tr
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %11110000
  .byte %11111000
  .byte %00111000
  .byte %00011000
  .byte %00011000

  ; Bottom left corner tile.
  .byte 10
  .byte 23, tile_corner_bl
  .byte %00011000
  .byte %00011000
  .byte %00011100
  .byte %00011111
  .byte %00001111
  .byte %00000000
  .byte %00000000
  .byte %00000000

  ; Bottom right corner tile.
  .byte 10
  .byte 23, tile_corner_br
  .byte %00011000
  .byte %00011000
  .byte %00111000
  .byte %11111000
  .byte %11110000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  ; Vertical edge tile.
  .byte 10
  .byte 23, tile_edge_v
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000

  ; Horizontal edge tile.
  .byte 10
  .byte 23, tile_edge_h
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %11111111
  .byte %11111111
  .byte %00000000
  .byte %00000000
  .byte %00000000

  ; Colour yellow on blue.
  .byte 12
  .byte 19, 1, 3, 0, 0, 0
  .byte 19, 0, 4, 0, 0, 0

  ; End of VDU data.
  .byte 0

  ;; -------------------------------------------------------------------------- 
  ;; Initial random number generator seed. Only four bytes are used, but we
  ;; reserve five so we can easily initialize it with the time.
  ;; -------------------------------------------------------------------------- 
seed
  .fill 5

  ;; --------------------------------------------------------------------------
  ;; Read system clock and store as random seed.
  ;; --------------------------------------------------------------------------  
srand
  lda #1
  ldx #<seed
  ldy #>seed
  jsr osword
  rts

  ;; --------------------------------------------------------------------------
  ;; Returns a 16-bit random value in A (high) and X (low).
  ;; Implements R(n + 1) = $01010101 * R(n) + $b3b3b3b3.
  ;; Credits to cc65 implementation.
  ;; --------------------------------------------------------------------------
rand
  clc
  lda seed + 0
  adc #$b3
  sta seed + 0
  adc seed + 1
  sta seed + 1
  adc seed + 2
  sta seed + 2
  tax
  adc seed + 3
  sta seed + 3
  rts
  
  ;; --------------------------------------------------------------------------
  ;; Return A % X, both single-byte values.
  ;; --------------------------------------------------------------------------
mod
  stx temp
  sec
- sbc temp
  bcs -
  adc temp
  rts

map
  .fill map_height * map_width

  ;; --------------------------------------------------------------------------
  ;; Clears the map by setting each tile to a space.
  ;; --------------------------------------------------------------------------
clear_map
  ; Make (ptr) point to the map.
  lda #<map
  sta ptr
  lda #>map
  sta ptr + 1

  ; Erase this many rows.
  ldx #map_height

  ; Erase one row.
- lda #' '
  ldy #map_width - 1
- sta (ptr),y
  dey
  bpl -

  ; Advance to next row.
  clc
  lda ptr
  adc #map_width
  sta ptr
  lda ptr + 1
  adc #0
  sta ptr + 1

  ; Erase next row.
  dex
  bne --

  rts

  ;; --------------------------------------------------------------------------
  ;; The type of a single room
  ;; --------------------------------------------------------------------------
room_t .struct
  x1 .byte 0
  y1 .byte 0
  x2 .byte 0
  y2 .byte 0
.ends

  ;; --------------------------------------------------------------------------
  ;; The nine rooms we have.
  ;; --------------------------------------------------------------------------
room0 .dstruct room_t
room1 .dstruct room_t
room2 .dstruct room_t
room3 .dstruct room_t
room4 .dstruct room_t
room5 .dstruct room_t
room6 .dstruct room_t
room7 .dstruct room_t
room8 .dstruct room_t
room9 .dstruct room_t

  ;; --------------------------------------------------------------------------
  ;; Generates a single random map.
  ;; --------------------------------------------------------------------------
generate_map
  ; Generate room0 y1.
  jsr rand
  ldx #map_height / 3 - room_min_height
  jsr mod
  sta room0.y1

  ; Generate room0 y2.
  jsr rand
  tay
  sec
  lda #map_height / 3
  sbc room0.y1
  sbc #room_min_height
  tax
  tya
  jsr mod
  clc
  adc room0.y1
  adc #room_min_height
  sta room0.y2

  ; Update the map with room0 left edge at column 0.
  lda #<map
  sta ptr
  lda #>map
  sta ptr + 1

  ; Need for (ptr),y operations.
  ldy #0

  ; Move to y1 by adding map_width y1 times.
  clc
  ldx room0.y1
  beq +
- lda ptr
  adc #map_width
  sta ptr
  lda ptr + 1
  adc #0
  sta ptr + 1
  dex
  bne -

  ; Store the top left corner tile.
+ lda #tile_corner_tl
  sta (ptr),y

  ; Remember where we started from.
  ldx room0.y1
  
  ; Move to the next row.
- clc
  lda ptr
  adc #map_width
  sta ptr
  lda ptr + 1
  adc #0
  sta ptr + 1
  
  ; Are we at y2 yet?
  inx
  cpx room0.y2
  beq +

  ; No, store a vertical edge tile.
  lda #tile_edge_v
  sta (ptr),y

  ; Next row.
  jmp -  

  ; Yes, store a bottom left corner tile.
+ lda #tile_corner_bl
  sta (ptr),y

  rts

  ;; --------------------------------------------------------------------------
  ;; Draws a single map starting at column X.
  ;; --------------------------------------------------------------------------
draw_map
  ; Make (ptr) point to column X of the map.
  stx temp
  clc
  lda #<map
  adc temp
  sta ptr
  lda #>map
  adc #0
  sta ptr + 1

  ; Move the cursor to the top left corner.
  lda #30
  jsr oswrch

  ; The number of rows to print.
  ldx #map_height

  ; Display a single row.
- ldy #0
- lda (ptr),y
  jsr oswrch
  iny
  cpy #screen_width
  bne -

  ; Print another row?
  dex
  beq +

  ; Move to the next row by adding map_width columns.
  clc
  lda ptr
  adc #map_width
  sta ptr
  lda ptr + 1
  adc #0
  sta ptr + 1

  ; Print another row.
  jmp --

+ rts 