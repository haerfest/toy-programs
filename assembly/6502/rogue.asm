;; To assemble:
;;   64tass --nostart -Wall --output rogue.o rogue.asm

  * = $2000

  osrdch = $ffe0
  oswrch = $ffee
  osword = $fff1
  osbyte = $fff4

  tile_empty     = 0
  tile_floor     = 1
  tile_corner_tl = 2
  tile_corner_tr = 3
  tile_corner_bl = 4
  tile_corner_br = 5
  tile_edge_h    = 6
  tile_edge_v    = 7

  screen_mode  = 4
  screen_width = 40

  map_width  = 80
  map_height = 31

  ;; --------------------------------------------------------------------------
  ;; Zero page workspace.
  ;; --------------------------------------------------------------------------   
  temp    = $70
  src     = $71
  dst     = $73
  map_ptr = $75

main
  jsr init
  jsr generate_map
  jsr vsync
  ldx #0
  jsr draw_map
- jmp -

  ;; --------------------------------------------------------------------------
  ;; Wait for vertical sync.
  ;; --------------------------------------------------------------------------
vsync
  lda #19
  jsr osbyte
  rts
  
  ;; --------------------------------------------------------------------------
  ;; Initializes the program.
  ;; --------------------------------------------------------------------------
init
  ldx #0
- ldy vdu_data,x
  beq +
- inx
  lda vdu_data,x
  jsr oswrch
  dey  
  bne -
  inx
  jmp --
+ rts

  ;; --------------------------------------------------------------------------
  ;; The type of a room.
  ;; --------------------------------------------------------------------------
room_t .struct x, y, w, h
  x1 .byte \x
  y1 .byte \y
  x2 .byte \x + \w - 1
  y2 .byte \y + \h - 1
  w  .byte \w
  h  .byte \h
  .ends

  ;; --------------------------------------------------------------------------
  ;; An example room.
  ;; --------------------------------------------------------------------------
room .dstruct room_t, 5, 6, 30, 11

  ;; --------------------------------------------------------------------------
  ;; Draws the example room.
  ;; --------------------------------------------------------------------------
room_draw
  ; Top row.
  lda #tile_corner_tl
  jsr tile_load
  ldx room.x1
  ldy room.y1
  jsr tab
  jsr tile_draw

  lda #tile_edge_h
  jsr tile_load
  ldx room.w
- jsr tile_draw
  dex
  bne -

  lda #tile_corner_tr
  jsr tile_load
  jsr tile_draw

  ; Center rows.
  ldy room.y1
  iny
- ldx room.x1
  jsr tab

  lda #tile_edge_v
  jsr tile_load
  jsr tile_draw

  lda #tile_floor
  jsr tile_load
  ldx room.w
- jsr tile_draw
  dex
  bne -

  lda #tile_edge_v
  jsr tile_load
  jsr tile_draw

  iny
  cpy room.y2
  bne --

  ; Bottom row.
  lda #tile_corner_bl
  jsr tile_load
  ldx room.x1
  ldy room.y2
  jsr tab
  jsr tile_draw

  lda #tile_edge_h
  jsr tile_load
  ldx room.w
- jsr tile_draw
  dex
  bne -

  lda #tile_corner_br
  jsr tile_load
  jsr tile_draw

  rts

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
  sed
- sec
  sbc temp
  bcs -
  adc temp
  rts

  ;; --------------------------------------------------------------------------
  ;; VDU initialization commands.
  ;; Format is: first byte (=N), then N bytes. Repeat as desired, end with N=0.
  ;; --------------------------------------------------------------------------
vdu_data
  .byte 2, 22, screen_mode
  ; cursor off
  .byte 10, 23, 1, 0, 0, 0, 0, 0, 0, 0, 0
  ; done
  .byte 0

  ;; --------------------------------------------------------------------------
  ;; Sets (dst) up to point to character cell XY, by pointing it to
  ;; $5800 + Y * $140 + X * 8.
  ;; --------------------------------------------------------------------------
tab
  pha
  txa
  pha
  tya
  pha

  lda #$00
  sta dst
  lda #$58
  sta dst + 1

  ; Each row is offset $140 bytes, so add it Y times.
  cpy #0
  beq +
  clc
- lda dst
  adc #$40
  sta dst
  lda dst + 1
  adc #$01
  sta dst + 1
  dey
  bne -

  ; Each column is offset 8 bytes, so add it X times.
+ cpx #0
  beq +
  clc
- lda dst
  adc #$08
  sta dst
  lda dst + 1
  adc #$00
  sta dst + 1
  dex
  bne -

+ pla
  tay
  pla
  tax
  pla
  rts

  ;; --------------------------------------------------------------------------
  ;; Sets up (src) to point to a tile (in A).
  ;; --------------------------------------------------------------------------
tile_load
  sta temp

  pha
  txa
  pha

  asl temp
  ldx temp
  lda bitmaps,x
  sta src
  lda bitmaps + 1,x
  sta src + 1

  pla
  tax
  pla
  rts

  ;; --------------------------------------------------------------------------
  ;; Draws a tile, pointed to by (src), at the character cell pointed to by
  ;; (dst).
  ;; --------------------------------------------------------------------------
tile_draw
  pha
  tya
  pha

  ldy #0
.rept 8 
  lda (src),y
  sta (dst),y
  iny
.next

  ; Update pointer, maybe we want to write to the adjacent cell.
  clc
  lda dst
  adc #8
  sta dst
  lda dst + 1
  adc #0
  sta dst + 1

  pla
  tay
  pla
  rts

bitmaps
  .addr bitmap_tile_empty
  .addr bitmap_tile_floor
  .addr bitmap_tile_corner_tl
  .addr bitmap_tile_corner_tr
  .addr bitmap_tile_corner_bl
  .addr bitmap_tile_corner_br
  .addr bitmap_tile_edge_h
  .addr bitmap_tile_edge_v

bitmap_tile_empty
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
        
bitmap_tile_floor
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00001000
  .byte %00000000
  .byte %00000000
  .byte %00000000

bitmap_tile_corner_tl
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00001111
  .byte %00011111
  .byte %00011100
  .byte %00011000
  .byte %00011000

bitmap_tile_corner_tr
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %11110000
  .byte %11111000
  .byte %00111000
  .byte %00011000
  .byte %00011000

bitmap_tile_corner_bl
  .byte %00011000
  .byte %00011000
  .byte %00011100
  .byte %00011111
  .byte %00001111
  .byte %00000000
  .byte %00000000
  .byte %00000000

bitmap_tile_corner_br
  .byte %00011000
  .byte %00011000
  .byte %00111000
  .byte %11111000
  .byte %11110000
  .byte %00000000
  .byte %00000000
  .byte %00000000

bitmap_tile_edge_v
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000

bitmap_tile_edge_h
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %11111111
  .byte %11111111
  .byte %00000000
  .byte %00000000
  .byte %00000000

map
  .fill 32 * 80, tile_floor

  ;; --------------------------------------------------------------------------
  ;; Generates a single random map.
  ;; --------------------------------------------------------------------------
generate_map
  rts

  ;; --------------------------------------------------------------------------
  ;; Draws a single map starting at column X, since a map is 80 columns
  ;; wide but the screen is 40, so we only show columns X:X+40.
  ;; --------------------------------------------------------------------------
draw_map
  stx temp

  ; Start drawing in the top-left corner.
  ldx #0
  ldy #0
  jsr tab

  ; Make (map_ptr) point to the start of the map at column X.
  clc
  lda #<map
  adc temp
  sta map_ptr
  lda #>map
  adc #0
  sta map_ptr + 1

  ; Repeat for each row of tiles.
  ldx #map_height

  ; Draw a single row of tiles.
- ldy #0
- lda (map_ptr),y
  jsr tile_load
  jsr tile_draw
  iny
  cpy #screen_width
  bne -

  ; Move (map_ptr) to next row by adding map_width bytes.
  clc
  lda map_ptr
  adc #map_width
  sta map_ptr
  lda map_ptr + 1
  adc #0
  sta map_ptr + 1

  ; Check for another row to draw.
  dex
  bne --

  rts
  
  

  