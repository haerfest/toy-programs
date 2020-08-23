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

  screen_mode   = 0
  screen_width  = 80
  screen_height = 32

  map_width  = 80
  map_height = screen_height - 1

  room_cell_width  = map_width  / 3
  room_cell_height = map_height / 3

  room_min_width  = 4
  room_min_height = 4

  ;; --------------------------------------------------------------------------
  ;; Zero page workspace.
  ;; --------------------------------------------------------------------------   
  temp    = $70
  ptr     = $71
  ptr2    = $73
  ptr3    = $75

  ;; --------------------------------------------------------------------------
  ;; Main program, entry point.
  ;; --------------------------------------------------------------------------
main
  jsr init
- jsr map_clear
  jsr map_generate
  ldx #0
  jsr map_draw

  lda #31
  jsr oswrch
  lda #0
  jsr oswrch
  lda #31
  jsr oswrch
  lda #room_cell_width
  jsr byte_print
  lda #' '
  jsr oswrch
  lda #room_cell_height
  jsr byte_print

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
  ;; Returns an 8-bit random value in A.
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

  ;; --------------------------------------------------------------------------
  ;; Clears the map by setting each tile to a space.
  ;; --------------------------------------------------------------------------
map_clear
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
  x1     .byte 0
  y1     .byte 0
  x2     .byte 0
  y2     .byte 0
  width  .byte 0
  height .byte 0
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
map_generate
  lda #<room0
  sta ptr
  lda #>room0
  sta ptr + 1

.for cell_y in 0, 1, 2
.for cell_x in 0, 1, 2
  jsr room_generate

  ; Correct x1, y1, x2, y2 for the grid cell.
  ldy #room_t.x1
  lda (ptr),y
  clc
  adc #cell_x * room_cell_width
  sta (ptr),y

  ldy #room_t.x2
  lda (ptr),y
  clc
  adc #cell_x * room_cell_width
  sta (ptr),y

  ldy #room_t.y1
  lda (ptr),y
  clc
  adc #cell_y * room_cell_height
  sta (ptr),y

  ldy #room_t.y2
  lda (ptr),y
  clc
  adc #cell_y * room_cell_height
  sta (ptr),y

  ; Draw this room.
  jsr room_draw

  ; Move (ptr) to the next room.
  clc
  lda ptr
  adc #size(room_t)
  sta ptr
  lda ptr + 1
  adc #0
  sta ptr + 1
.next
.next

  rts

  ;; --------------------------------------------------------------------------
  ;; Generate a single random room. (ptr) needs to point to the room_t to
  ;; generate.
  ;; --------------------------------------------------------------------------
room_generate
  ; height := room_min_height + rand() % (room_cell_height - room_min_height)
  ldx #room_cell_height - room_min_height
  jsr rand
  jsr mod
  clc
  adc #room_min_height
  ldy #room_t.height
  sta (ptr),y

  ; y1 := rand() % (room_cell_height - height)
  lda #room_cell_height
  sec
  sbc (ptr),y
  tax
  jsr rand
  jsr mod
  ldy #room_t.y1
  sta (ptr),y

  ; y2 := y1 + height - 1
  ldy #room_t.height
  clc
  adc (ptr),y
  sec
  sbc #1
  ldy #room_t.y2
  sta (ptr),y

  ; width := room_min_width + rand() % (room_cell_width - room_min_width)
  ldx #room_cell_width - room_min_width
  jsr rand
  jsr mod
  clc
  adc #room_min_width
  ldy #room_t.width
  sta (ptr),y

  ; x1 := rand() % (room_cell_width - width)
  lda #room_cell_width
  sec
  sbc (ptr),y
  tax
  jsr rand
  jsr mod
  ldy #room_t.x1
  sta (ptr),y

  ; x2 := x1 + width - 1
  ldy #room_t.width
  clc
  adc (ptr),y
  sec
  sbc #1
  ldy #room_t.x2
  sta (ptr),y
  
  rts

room_tiles_row_top
  .byte tile_corner_tl, tile_edge_h, tile_corner_tr

room_tiles_row_center
  .byte tile_edge_v,    tile_floor,  tile_edge_v

room_tiles_row_bottom
  .byte tile_corner_bl, tile_edge_h, tile_corner_br

  ;; --------------------------------------------------------------------------
  ;; Draws a room_t, pointed at by (ptr), into the map.
  ;; --------------------------------------------------------------------------
room_draw
  ; Make (ptr2) point to the map.
  lda #<map
  sta ptr2
  lda #>map
  sta ptr2 + 1

  ; Offset to y1.
  ldy #room_t.y1
  lda (ptr),y

  ; Move to row y1 in the map by adding #map_width y1 times.
  beq +
  tay
- clc
  lda ptr2
  adc #map_width
  sta ptr2
  lda ptr2 + 1
  adc #0
  sta ptr2 + 1
  dey
  bne -

  ; Offset to x1.
+ ldy #room_t.x1

  ; Move to column x1 in the map by adding x1.
  clc
  lda ptr2
  adc (ptr),y
  sta ptr2
  lda ptr2 + 1
  adc #0
  sta ptr2 + 1

  ; Draw the top row into the map.
  clc
  lda #<room_tiles_row_top
  sta ptr3
  lda #>room_tiles_row_top
  sta ptr3 + 1
  jsr draw_room_row

  ; Fetch the height - 2.
  ldy #room_t.height
  lda (ptr),y
  sec
  sbc #2
  sta temp

  ; Draw the center rows.
  clc
  lda #<room_tiles_row_center
  sta ptr3
  lda #>room_tiles_row_center
  sta ptr3 + 1

- jsr draw_room_row
  dec temp
  bne -

  ; Draw the bottom row.
  clc
  lda #<room_tiles_row_bottom
  sta ptr3
  lda #>room_tiles_row_bottom
  sta ptr3 + 1
  jsr draw_room_row

  rts
  
  ;; --------------------------------------------------------------------------
  ;; Draws a single row for a room. The room is pointed at by (ptr), (ptr2)
  ;; already should point to the leftmost map location where the row is to
  ;; start, and (ptr3) points to the three tiles that make up this row.
  ;; Advances (ptr2) to the beginning of the next row.
  ;; --------------------------------------------------------------------------
draw_room_row
  ; Push the right and center tiles on the stack.
  ldy #2
  lda (ptr3),y
  pha
  dey
  lda (ptr3),y
  pha
  dey

  ; Store the top left corner tile.
  lda (ptr3),y
  sta (ptr2),y

  ; Store the horizontal edge tile width - 2 times.
  ldy #room_t.width
  lda (ptr),y
  tax
  dex
  dex
  ldy #1
  pla
- sta (ptr2),y
  iny
  dex
  bne -

  ; Store the top right corner tile.
  pla
  sta (ptr2),y

  ; Advance (ptr2) to the beginning of the next row.
  clc
  lda ptr2
  adc #map_width
  sta ptr2
  lda ptr2 + 1
  adc #0
  sta ptr2 + 1

  rts

  ;; --------------------------------------------------------------------------
  ;; Draws a single map starting at column X.
  ;; --------------------------------------------------------------------------
map_draw
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

  ;; --------------------------------------------------------------------------
  ;; Prints a room's coordinates. The room is pointed at by (ptr).
  ;; --------------------------------------------------------------------------
room_print
  pha
  tya
  pha

  ; Move the cursor to the last row.
  lda #31
  jsr oswrch
  lda #0
  jsr oswrch
  lda #screen_height - 1
  jsr oswrch

  ; A room has six byte values.
  ldy #0
- lda (ptr),y
  jsr byte_print
  lda #' '
  jsr oswrch
  iny
  cpy #6
  bne -

  pla
  tay
  pla
  rts
  
  ;; --------------------------------------------------------------------------
  ;; Prints a single byte in A.
  ;; --------------------------------------------------------------------------
byte_print
  pha
  txa
  pha

  ; Load A from the stack and push it once more.
  tsx
  inx
  inx
  lda $0100,x
  pha

  ; Print upper nibble.
  lsr a
  lsr a
  lsr a
  lsr a
  tax
  lda hex_chars,x
  jsr oswrch

  ; Print lower nibble.
  pla
  and #$0F
  tax
  lda hex_chars,x
  jsr oswrch

  pla
  tax
  pla
  rts

hex_chars
  .text "0123456789ABCDEF"

  ;; --------------------------------------------------------------------------
  ;; The map will be map_height * map_width bytes in size and generated on
  ;; the fly. No need to store that in the binary.
  ;; --------------------------------------------------------------------------
map
  .byte 0

