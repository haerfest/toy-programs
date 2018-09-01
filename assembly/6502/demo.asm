  ;; Operating system calls.
  oswrch = $ffee  
  osbyte = $fff4

  ;; OSBYTE commands.
  osbyte_inkey = 129

  ;; VDU commands.
  vdu_change_mode = 22

  ;; INKEY key values.
  inkey_space_bar = -99

  * = $2000

start:
  lda #4                        ; switch to video mode 4
  jsr switch_mode
  jsr wait_space_bar            ; wait until space bar pressed
  rts

  ;; ---------------------------------------------------------------------------
  ;; Switches to the video mode specified in the A register.
  ;; ---------------------------------------------------------------------------
switch_mode:
  pha                           ; perform VDU 22,A
  lda #vdu_change_mode
  jsr oswrch
  pla
  jmp oswrch

  ;; ---------------------------------------------------------------------------
  ;; Wait until the space bar is pressed.
  ;; ---------------------------------------------------------------------------
wait_space_bar:
  lda #osbyte_inkey             ; read status of space bar key
  ldx #inkey_space_bar & $ff
  ldy #$ff
  jsr osbyte
  bcc wait_space_bar            ; carry will be set when pressed
  rts

  ;; ---------------------------------------------------------------------------
  ;; Scratch space.
  ;; ---------------------------------------------------------------------------
tmp:
  .byt 0
  
