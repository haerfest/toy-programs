  ;; assemble using 64tass:
  ;;
  ;; $ 64tass -Wall --nostart --list music.lst music.asm -o music.bin

  .cpu "6502"

  ptr  = $70
  dur  = $72

  * = $2000

  ;; setup a pointer to the song position
  lda #<song
  sta ptr
  lda #>song
  sta ptr+1

  ;; enable sound mode, select MODE 6
  lda #%10110010
  sta $fe07

  ;; load duration (dur) and note (x): 31 cycles
next
  ldy #0                        ; 2 cycles
  lda (ptr),y                   ; 5 cycles
  tax                           ; 2 cycles
  iny                           ; 2 cycles
  lda (ptr),y                   ; 5 cycles
  sta dur                       ; 3 cycles
  iny                           ; 2 cycles
  lda (ptr),y                   ; 5 cycles
  sta dur+1                     ; 3 cycles
  iny                           ; 2 cycles

  ;; advance pointer by three: 18 cycles
  clc                           ; 2 cycles
  lda ptr                       ; 3 cycles
  adc #3                        ; 2 cycles
  sta ptr                       ; 3 cycles
  lda ptr+1                     ; 3 cycles
  adc #0                        ; 2 cycles
  sta ptr+1                     ; 3 cycles

  ;; we're done when duration is zero: 11 cycles
  lda dur                       ; 3 cycles
  bne play                      ; 2-3 cycles
  lda dur+1                     ; 3 cycles
  beq done                      ; 2-3 cycles

  ;; start playing: 6 cycles
  ;; it took 60 cycles from loading the note to playing it
  ;; at 1 mhz that is 60 usec, which we ignore
play
  txa                           ; 2 cycles
  sta $fe06                     ; 4 cycles

  ;; wait for the duration, specified in milliseconds > 0
  ;; the OS is doing things in the background, and this small
  ;; delay seems to provide accurate enough timing
wait
  ldy #40                       ; 2 cycles
-
  dey                           ; 2 cycles
  bne -                         ; 2-3 cycles

  ;; decrement duration: 18 cycles
  sec                           ; 2 cycles
  lda dur                       ; 3 cycles
  sbc #1                        ; 2 cycles
  sta dur                       ; 3 cycles
  lda dur+1                     ; 3 cycles
  sbc #0                        ; 2 cycles
  sta dur+1                     ; 3 cyles

  ;; check whether duration reached zero: 12 cycles
  lda dur                       ; 3 cycles
  bne wait                      ; 2-3 cycles
  lda dur+1                     ; 3 cycles
  bne wait                      ; 2-3 cycles

  ;; fetch and play the next note: 3 cycles
  jmp next                      ; 3 cycles

  ;; done, disable sound mode
done
  lda #%10110110
  sta $fe07
  rts

  ;; song data:
  ;; - 1 byte note
  ;; - 2 bytes duration in msec
song
  ;; should run for 1:10 minutes
  .include "mi1-theme.asm"

  ;; end of song marker
  .byte 0
  .word 0
