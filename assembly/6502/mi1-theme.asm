  ;; Patch DosBox to generate this listing of notes:
  ;;
  ;; $ diff dosbox-0.74-3/src/hardware/timer.cpp ~/Codez/dosbox-0.74-3/src/hardware/timer.cp
  ;;
  ;; 191a192,206
  ;; > #define LOG_NOTES
  ;; > #ifdef LOG_NOTES
  ;; > 
  ;; > #define SILENCE 0  // 62 kHz, inaudible
  ;; > 
  ;; > static struct {
  ;; >   unsigned int  hz;
  ;; >   unsigned long tick;
  ;; >   float         timer_hz;
  ;; > } log_notes = {
  ;; >   PIT_TICK_RATE, 0, 1.f
  ;; > };
  ;; > 
  ;; > #endif
  ;; > 
  ;; 237a253,258
  ;; > #ifdef LOG_NOTES
  ;; >       if (p->mode == 3) {
  ;; >         // PC speaker is governed by this timer.
  ;; >         log_notes.timer_hz = 1000 / p->delay;
  ;; >       }
  ;; > #endif
  ;; 240a262,285
  ;; > #ifdef LOG_NOTES
  ;; >       if (p->mode == 3) {
  ;; >         // Square wave.
  ;; >         const unsigned hz = unsigned(PIT_TICK_RATE / (double) p->cntr);
  ;; >         if (hz != log_notes.hz) {
  ;; >           // Note changed: output previous note + duration.
  ;; >           const unsigned long ticks    = PIC_Ticks - log_notes.tick;
  ;; >           const unsigned int  msecs    = (unsigned int) (1000 * ticks / log_notes.timer_hz);
  ;; > 
  ;; >           // Acorn Electron specific.
  ;; >           const int           s_value  = 1000000 / (16 * log_notes.hz) - 1;
  ;; >           const int           s_capped = (s_value < 0)   ? SILENCE
  ;; >                                        : (s_value > 255) ? SILENCE
  ;; >                                        : s_value;
  ;; > 
  ;; >           printf("  .byte %5d  ; %u hz\n", s_capped, log_notes.hz);
  ;; >           printf("  .word %5u  ; ms\n", msecs);
  ;; > 
  ;; >           // Remember the new note's frequency and when it started.
  ;; >           log_notes.hz   = hz;
  ;; >           log_notes.tick = PIC_Ticks;
  ;; >         }
  ;; >       }
  ;; > #endif

  .byte    62  ; 988 hz
  .word    50  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   105  ; ms
  .byte     0  ; 1193182 hz
  .word   232  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte     0  ; 164 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word    54  ; ms
  .byte     0  ; 1193182 hz
  .word   447  ; ms
  .byte     0  ; 164 hz
  .word    16  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   376  ; ms
  .byte     0  ; 1193182 hz
  .word   194  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte     0  ; 146 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   646  ; ms
  .byte   212  ; 293 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte     0  ; 220 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   177  ; ms
  .byte     0  ; 1193182 hz
  .word   337  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   105  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word    54  ; ms
  .byte    93  ; 658 hz
  .word    67  ; ms
  .byte     0  ; 146 hz
  .word    54  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word    54  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte    62  ; 988 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   139  ; ms
  .byte     0  ; 1193182 hz
  .word   376  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   536  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 146 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   181  ; ms
  .byte     0  ; 1193182 hz
  .word   337  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   519  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte     0  ; 164 hz
  .word  1073  ; ms
  .byte     0  ; 1193182 hz
  .word   266  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   125  ; 494 hz
  .word    38  ; ms
  .byte    93  ; 658 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte    93  ; 658 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   608  ; ms
  .byte    83  ; 740 hz
  .word   747  ; ms
  .byte     0  ; 1193182 hz
  .word    21  ; ms
  .byte    93  ; 658 hz
  .word   532  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte     0  ; 130 hz
  .word    33  ; ms
  .byte    93  ; 658 hz
  .word  1301  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   354  ; ms
  .byte     0  ; 1193182 hz
  .word   160  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   621  ; ms
  .byte     0  ; 98 hz
  .word  1233  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   158  ; 392 hz
  .word    33  ; ms
  .byte   105  ; 587 hz
  .word   608  ; ms
  .byte   118  ; 523 hz
  .word   697  ; ms
  .byte     0  ; 196 hz
  .word    67  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   125  ; 494 hz
  .word   498  ; ms
  .byte     0  ; 196 hz
  .word    71  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   553  ; ms
  .byte   118  ; 523 hz
  .word   692  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte   118  ; 523 hz
  .word   519  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   498  ; ms
  .byte   125  ; 494 hz
  .word  1056  ; ms
  .byte     0  ; 1193182 hz
  .word   371  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   177  ; ms
  .byte     0  ; 1193182 hz
  .word   321  ; ms
  .byte     0  ; 82 hz
  .word    16  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    67  ; ms
  .byte     0  ; 82 hz
  .word   109  ; ms
  .byte   125  ; 494 hz
  .word    50  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   160  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word    54  ; ms
  .byte    62  ; 988 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   143  ; ms
  .byte     0  ; 1193182 hz
  .word   532  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   536  ; ms
  .byte    93  ; 658 hz
  .word    33  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   321  ; ms
  .byte     0  ; 1193182 hz
  .word   194  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   125  ; 494 hz
  .word    33  ; ms
  .byte    93  ; 658 hz
  .word  1305  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   608  ; ms
  .byte    83  ; 740 hz
  .word   713  ; ms
  .byte    93  ; 658 hz
  .word   608  ; ms
  .byte     0  ; 220 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 130 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word  1229  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte     0  ; 130 hz
  .word   392  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    67  ; ms
  .byte     0  ; 130 hz
  .word   768  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte     0  ; 196 hz
  .word    33  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   498  ; ms
  .byte    78  ; 784 hz
  .word   697  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    33  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   536  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   141  ; 440 hz
  .word    16  ; ms
  .byte    70  ; 880 hz
  .word  1322  ; ms
  .byte     0  ; 220 hz
  .word    71  ; ms
  .byte    70  ; 880 hz
  .word    33  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte     0  ; 220 hz
  .word    71  ; ms
  .byte   238  ; 261 hz
  .word   105  ; ms
  .byte     0  ; 1193182 hz
  .word   697  ; ms
  .byte    83  ; 740 hz
  .word   730  ; ms
  .byte     0  ; 146 hz
  .word   519  ; ms
  .byte     0  ; 220 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word    88  ; ms
  .byte     0  ; 1193182 hz
  .word   426  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    38  ; ms
  .byte   167  ; 370 hz
  .word    67  ; ms
  .byte    78  ; 784 hz
  .word   608  ; ms
  .byte    83  ; 740 hz
  .word   608  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte    93  ; 658 hz
  .word   642  ; ms
  .byte     0  ; 220 hz
  .word    33  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   464  ; ms
  .byte    83  ; 740 hz
  .word    38  ; ms
  .byte     0  ; 220 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   515  ; ms
  .byte    78  ; 784 hz
  .word   713  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte     0  ; 98 hz
  .word   570  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte    78  ; 784 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte    83  ; 740 hz
  .word   925  ; ms
  .byte     0  ; 123 hz
  .word   430  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 123 hz
  .word   143  ; ms
  .byte     0  ; 1193182 hz
  .word   371  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte    93  ; 658 hz
  .word   857  ; ms
  .byte     0  ; 164 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte     0  ; 164 hz
  .word    54  ; ms
  .byte     0  ; 1193182 hz
  .word   464  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   570  ; ms
  .byte    83  ; 740 hz
  .word   713  ; ms
  .byte    93  ; 658 hz
  .word   642  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte     0  ; 220 hz
  .word    38  ; ms
  .byte   212  ; 293 hz
  .word    67  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   608  ; ms
  .byte    78  ; 784 hz
  .word   768  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte     0  ; 98 hz
  .word   498  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   536  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word    16  ; ms
  .byte     0  ; 1193182 hz
  .word   659  ; ms
  .byte    83  ; 740 hz
  .word   735  ; ms
  .byte     0  ; 123 hz
  .word   532  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   697  ; ms
  .byte    93  ; 658 hz
  .word   713  ; ms
  .byte     0  ; 164 hz
  .word   536  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   570  ; ms
  .byte    83  ; 740 hz
  .word   713  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte    93  ; 658 hz
  .word   642  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   604  ; ms
  .byte    93  ; 658 hz
  .word   659  ; ms
  .byte     0  ; 1193182 hz
  .word   164  ; ms
  .byte     0  ; 130 hz
  .word   498  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    38  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   536  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   604  ; ms
  .byte    93  ; 658 hz
  .word  1339  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    67  ; ms
  .byte     0  ; 130 hz
  .word   342  ; ms
  .byte     0  ; 1193182 hz
  .word   177  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    21  ; ms
  .byte     0  ; 130 hz
  .word   443  ; ms
  .byte     0  ; 1193182 hz
  .word   105  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   553  ; ms
  .byte    93  ; 658 hz
  .word    21  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   105  ; 587 hz
  .word   747  ; ms
  .byte   118  ; 523 hz
  .word   608  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   125  ; 494 hz
  .word   519  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   253  ; 246 hz
  .word    38  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   692  ; ms
  .byte   118  ; 523 hz
  .word   680  ; ms
  .byte     0  ; 110 hz
  .word   515  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte     0  ; 220 hz
  .word    71  ; ms
  .byte   141  ; 440 hz
  .word    33  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   118  ; 523 hz
  .word   502  ; ms
  .byte     0  ; 220 hz
  .word    71  ; ms
  .byte   118  ; 523 hz
  .word    33  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte     0  ; 110 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word   232  ; ms
  .byte   125  ; 494 hz
  .word   752  ; ms
  .byte     0  ; 82 hz
  .word   460  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte     0  ; 82 hz
  .word   536  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 146 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte     0  ; 123 hz
  .word   642  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   126  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte    62  ; 988 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   143  ; ms
  .byte     0  ; 1193182 hz
  .word   392  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   464  ; ms
  .byte   125  ; 494 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word   177  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte     0  ; 220 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   337  ; ms
  .byte     0  ; 1193182 hz
  .word   177  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   414  ; ms
  .byte    93  ; 658 hz
  .word   143  ; ms
  .byte    58  ; 1046 hz
  .word   532  ; ms
  .byte     0  ; 1193182 hz
  .word   198  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   570  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   321  ; ms
  .byte     0  ; 1193182 hz
  .word   198  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   426  ; ms
  .byte     0  ; 1193182 hz
  .word   143  ; ms
  .byte    78  ; 784 hz
  .word   591  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte     0  ; 196 hz
  .word   680  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   536  ; ms
  .byte    58  ; 1046 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word   215  ; ms
  .byte    62  ; 988 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte    70  ; 880 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte    70  ; 880 hz
  .word   371  ; ms
  .byte     0  ; 1193182 hz
  .word   143  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    67  ; ms
  .byte    58  ; 1046 hz
  .word   287  ; ms
  .byte     0  ; 1193182 hz
  .word   287  ; ms
  .byte    62  ; 988 hz
  .word   587  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte     0  ; 98 hz
  .word   642  ; ms
  .byte    78  ; 784 hz
  .word    21  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    33  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 123 hz
  .word   536  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte    78  ; 784 hz
  .word   587  ; ms
  .byte     0  ; 146 hz
  .word   663  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    38  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte   141  ; 440 hz
  .word    38  ; ms
  .byte    70  ; 880 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word   177  ; ms
  .byte     0  ; 146 hz
  .word   591  ; ms
  .byte   105  ; 587 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   460  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 185 hz
  .word   570  ; ms
  .byte   105  ; 587 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte     0  ; 220 hz
  .word   675  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   502  ; ms
  .byte    62  ; 988 hz
  .word   697  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte    70  ; 880 hz
  .word   680  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   141  ; 440 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   519  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte    70  ; 880 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word   105  ; ms
  .byte    62  ; 988 hz
  .word   536  ; ms
  .byte     0  ; 1193182 hz
  .word   177  ; ms
  .byte   253  ; 246 hz
  .word    33  ; ms
  .byte     0  ; 164 hz
  .word   536  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   409  ; ms
  .byte     0  ; 1193182 hz
  .word   109  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 123 hz
  .word   498  ; ms
  .byte    78  ; 784 hz
  .word   608  ; ms
  .byte     0  ; 1193182 hz
  .word   160  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte     0  ; 98 hz
  .word   532  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    67  ; ms
  .byte    93  ; 658 hz
  .word   502  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   519  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    38  ; ms
  .byte    93  ; 658 hz
  .word   730  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   536  ; ms
  .byte   167  ; 370 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word   105  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte   158  ; 392 hz
  .word   371  ; ms
  .byte     0  ; 1193182 hz
  .word   143  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   502  ; ms
  .byte   141  ; 440 hz
  .word   553  ; ms
  .byte     0  ; 1193182 hz
  .word   232  ; ms
  .byte     0  ; 87 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   178  ; 349 hz
  .word    50  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   178  ; 349 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte     0  ; 110 hz
  .word   337  ; ms
  .byte     0  ; 1193182 hz
  .word   232  ; ms
  .byte    58  ; 1046 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte     0  ; 130 hz
  .word   519  ; ms
  .byte     0  ; 1193182 hz
  .word   194  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   178  ; 349 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   178  ; 349 hz
  .word    71  ; ms
  .byte     0  ; 174 hz
  .word   481  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte   178  ; 349 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   178  ; 349 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   178  ; 349 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte    58  ; 1046 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   646  ; ms
  .byte   118  ; 523 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte     0  ; 220 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   178  ; 349 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   178  ; 349 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   481  ; ms
  .byte    46  ; 1316 hz
  .word    71  ; ms
  .byte    62  ; 988 hz
  .word    71  ; ms
  .byte   158  ; 392 hz
  .word    33  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte    93  ; 658 hz
  .word    67  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word   604  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    50  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    54  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 123 hz
  .word   376  ; ms
  .byte     0  ; 1193182 hz
  .word   194  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte     0  ; 98 hz
  .word   409  ; ms
  .byte     0  ; 1193182 hz
  .word   232  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    50  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    50  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   392  ; ms
  .byte     0  ; 1193182 hz
  .word   177  ; ms
  .byte    83  ; 740 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word  1305  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    54  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    54  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte    83  ; 740 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    50  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte   167  ; 370 hz
  .word   232  ; ms
  .byte     0  ; 1193182 hz
  .word   287  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    54  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   587  ; ms
  .byte   158  ; 392 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word   232  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   177  ; ms
  .byte     0  ; 130 hz
  .word   392  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    67  ; ms
  .byte     0  ; 130 hz
  .word    38  ; ms
  .byte     0  ; 1193182 hz
  .word   481  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    33  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 155 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word    21  ; ms
  .byte    78  ; 784 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   646  ; ms
  .byte     0  ; 196 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word   143  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte   238  ; 261 hz
  .word   498  ; ms
  .byte   118  ; 523 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word   287  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   199  ; 311 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   199  ; 311 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   199  ; 311 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   430  ; ms
  .byte    52  ; 1175 hz
  .word    67  ; ms
  .byte   105  ; 587 hz
  .word    38  ; ms
  .byte    70  ; 880 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word    16  ; ms
  .byte    83  ; 740 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   177  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word    38  ; ms
  .byte   167  ; 370 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   359  ; ms
  .byte   212  ; 293 hz
  .word   481  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    50  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   167  ; 370 hz
  .word    50  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 220 hz
  .word   625  ; ms
  .byte    83  ; 740 hz
  .word    16  ; ms
  .byte     0  ; 1193182 hz
  .word   608  ; ms
  .byte     0  ; 185 hz
  .word   675  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   167  ; 370 hz
  .word    38  ; ms
  .byte   141  ; 440 hz
  .word    67  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   608  ; ms
  .byte    93  ; 658 hz
  .word    33  ; ms
  .byte     0  ; 1193182 hz
  .word  1305  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    50  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   167  ; 370 hz
  .word    50  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte    83  ; 740 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    50  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   519  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    50  ; ms
  .byte   105  ; 587 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word    38  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word    33  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word    50  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word    54  ; ms
  .byte    78  ; 784 hz
  .word    54  ; ms
  .byte     0  ; 164 hz
  .word    16  ; ms
  .byte    62  ; 988 hz
  .word    33  ; ms
  .byte     0  ; 164 hz
  .word   143  ; ms
  .byte    93  ; 658 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word   802  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte    93  ; 658 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   608  ; ms
  .byte   212  ; 293 hz
  .word    33  ; ms
  .byte    83  ; 740 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte     0  ; 220 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   498  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   105  ; ms
  .byte    93  ; 658 hz
  .word   946  ; ms
  .byte     0  ; 1193182 hz
  .word   232  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   232  ; ms
  .byte     0  ; 1193182 hz
  .word   287  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word   105  ; ms
  .byte     0  ; 98 hz
  .word   747  ; ms
  .byte   212  ; 293 hz
  .word    38  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   447  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   212  ; 293 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   158  ; 392 hz
  .word    38  ; ms
  .byte   105  ; 587 hz
  .word   604  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   118  ; 523 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   125  ; 494 hz
  .word    38  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte   125  ; 494 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   570  ; ms
  .byte   118  ; 523 hz
  .word   697  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   141  ; 440 hz
  .word   109  ; ms
  .byte   118  ; 523 hz
  .word   515  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   118  ; 523 hz
  .word    38  ; ms
  .byte   141  ; 440 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    67  ; ms
  .byte   125  ; 494 hz
  .word  1195  ; ms
  .byte     0  ; 1193182 hz
  .word    21  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   392  ; ms
  .byte     0  ; 1193182 hz
  .word   126  ; ms
  .byte   253  ; 246 hz
  .word   105  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word    88  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word    33  ; ms
  .byte    78  ; 784 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word    88  ; ms
  .byte    62  ; 988 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   194  ; ms
  .byte     0  ; 1193182 hz
  .word  1144  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte    93  ; 658 hz
  .word  1035  ; ms
  .byte     0  ; 1193182 hz
  .word   215  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte    78  ; 784 hz
  .word   608  ; ms
  .byte    83  ; 740 hz
  .word   608  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   553  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   519  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte    93  ; 658 hz
  .word  1090  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte     0  ; 196 hz
  .word    54  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    67  ; ms
  .byte     0  ; 130 hz
  .word   232  ; ms
  .byte     0  ; 1193182 hz
  .word   287  ; ms
  .byte     0  ; 196 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   697  ; ms
  .byte     0  ; 1193182 hz
  .word   587  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   118  ; 523 hz
  .word    38  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word    16  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   625  ; ms
  .byte   125  ; 494 hz
  .word    33  ; ms
  .byte    78  ; 784 hz
  .word   713  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   125  ; 494 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   515  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   158  ; 392 hz
  .word   109  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte    70  ; 880 hz
  .word  1339  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   118  ; 523 hz
  .word    33  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   141  ; 440 hz
  .word   105  ; ms
  .byte     0  ; 1193182 hz
  .word   498  ; ms
  .byte    83  ; 740 hz
  .word   730  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte     0  ; 146 hz
  .word   625  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   498  ; ms
  .byte    78  ; 784 hz
  .word    16  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   587  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte    83  ; 740 hz
  .word   591  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte    93  ; 658 hz
  .word   574  ; ms
  .byte     0  ; 1193182 hz
  .word   105  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   519  ; ms
  .byte     0  ; 220 hz
  .word    33  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   426  ; ms
  .byte     0  ; 1193182 hz
  .word   270  ; ms
  .byte    78  ; 784 hz
  .word   515  ; ms
  .byte     0  ; 98 hz
  .word   680  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   498  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   697  ; ms
  .byte    83  ; 740 hz
  .word   608  ; ms
  .byte     0  ; 123 hz
  .word   625  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   553  ; ms
  .byte    93  ; 658 hz
  .word   642  ; ms
  .byte     0  ; 164 hz
  .word   481  ; ms
  .byte     0  ; 1193182 hz
  .word   266  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   574  ; ms
  .byte    83  ; 740 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word   160  ; ms
  .byte    93  ; 658 hz
  .word   498  ; ms
  .byte     0  ; 1193182 hz
  .word   126  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   443  ; ms
  .byte     0  ; 1193182 hz
  .word   215  ; ms
  .byte    78  ; 784 hz
  .word   675  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte     0  ; 98 hz
  .word   481  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    67  ; ms
  .byte    78  ; 784 hz
  .word   519  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte    83  ; 740 hz
  .word   625  ; ms
  .byte     0  ; 123 hz
  .word   536  ; ms
  .byte     0  ; 1193182 hz
  .word   143  ; ms
  .byte   199  ; 311 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   199  ; 311 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte    93  ; 658 hz
  .word   675  ; ms
  .byte     0  ; 164 hz
  .word   502  ; ms
  .byte     0  ; 1193182 hz
  .word   194  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   125  ; 494 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   125  ; 494 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   570  ; ms
  .byte    83  ; 740 hz
  .word   697  ; ms
  .byte    93  ; 658 hz
  .word   608  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    50  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   105  ; 587 hz
  .word   587  ; ms
  .byte   167  ; 370 hz
  .word    54  ; ms
  .byte   141  ; 440 hz
  .word    54  ; ms
  .byte   105  ; 587 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   663  ; ms
  .byte    93  ; 658 hz
  .word   570  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte     0  ; 130 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word   194  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   502  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   697  ; ms
  .byte    93  ; 658 hz
  .word   659  ; ms
  .byte     0  ; 1193182 hz
  .word   143  ; ms
  .byte     0  ; 130 hz
  .word   426  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    54  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    50  ; ms
  .byte   118  ; 523 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    54  ; ms
  .byte     0  ; 98 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word   126  ; ms
  .byte   212  ; 293 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte   253  ; 246 hz
  .word    67  ; ms
  .byte   158  ; 392 hz
  .word   109  ; ms
  .byte     0  ; 1193182 hz
  .word    16  ; ms
  .byte   105  ; 587 hz
  .word   498  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word    38  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   570  ; ms
  .byte   141  ; 440 hz
  .word    16  ; ms
  .byte   118  ; 523 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte   188  ; 329 hz
  .word    71  ; ms
  .byte   125  ; 494 hz
  .word   570  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte   141  ; 440 hz
  .word   105  ; ms
  .byte   118  ; 523 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte   238  ; 261 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte   141  ; 440 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   659  ; ms
  .byte   125  ; 494 hz
  .word   625  ; ms
  .byte     0  ; 164 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte     0  ; 82 hz
  .word   325  ; ms
  .byte     0  ; 1193182 hz
  .word   194  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   392  ; ms
  .byte     0  ; 1193182 hz
  .word   105  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte     0  ; 164 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word    71  ; ms
  .byte     0  ; 82 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte   253  ; 246 hz
  .word    71  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   426  ; ms
  .byte     0  ; 1193182 hz
  .word    88  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   608  ; ms
  .byte    93  ; 658 hz
  .word   642  ; ms
  .byte     0  ; 1193182 hz
  .word   697  ; ms
  .byte   253  ; 246 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   519  ; ms
  .byte   253  ; 246 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    78  ; 784 hz
  .word   680  ; ms
  .byte    83  ; 740 hz
  .word   604  ; ms
  .byte     0  ; 1193182 hz
  .word    38  ; ms
  .byte    93  ; 658 hz
  .word   604  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte   105  ; 587 hz
  .word   515  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   642  ; ms
  .byte    93  ; 658 hz
  .word   591  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte     0  ; 130 hz
  .word   536  ; ms
  .byte     0  ; 1193182 hz
  .word   143  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    50  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte    93  ; 658 hz
  .word   519  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 130 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word   908  ; ms
  .byte     0  ; 130 hz
  .word   481  ; ms
  .byte     0  ; 1193182 hz
  .word    92  ; ms
  .byte   238  ; 261 hz
  .word    50  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    71  ; ms
  .byte     0  ; 1193182 hz
  .word   515  ; ms
  .byte   238  ; 261 hz
  .word    54  ; ms
  .byte   188  ; 329 hz
  .word    54  ; ms
  .byte   158  ; 392 hz
  .word    67  ; ms
  .byte     0  ; 1193182 hz
  .word   574  ; ms
  .byte    70  ; 880 hz
  .word   532  ; ms
  .byte     0  ; 1193182 hz
  .word   181  ; ms
  .byte   141  ; 440 hz
  .word    16  ; ms
  .byte    78  ; 784 hz
  .word   464  ; ms
  .byte     0  ; 1193182 hz
  .word   139  ; ms
  .byte    83  ; 740 hz
  .word    21  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    50  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   447  ; ms
  .byte     0  ; 1193182 hz
  .word    50  ; ms
  .byte     0  ; 220 hz
  .word    71  ; ms
  .byte   212  ; 293 hz
  .word    54  ; ms
  .byte   167  ; 370 hz
  .word    71  ; ms
  .byte    70  ; 880 hz
  .word   532  ; ms
  .byte    78  ; 784 hz
  .word   680  ; ms
  .byte     0  ; 1193182 hz
  .word    33  ; ms
  .byte    93  ; 658 hz
  .word   663  ; ms
  .byte     0  ; 185 hz
  .word    54  ; ms
  .byte     0  ; 220 hz
  .word    50  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte    83  ; 740 hz
  .word   519  ; ms
  .byte     0  ; 185 hz
  .word    50  ; ms
  .byte     0  ; 220 hz
  .word    54  ; ms
  .byte   212  ; 293 hz
  .word    71  ; ms
  .byte     0  ; 164 hz
  .word    16  ; ms
  .byte     0  ; 196 hz
  .word   287  ; ms
  .byte   253  ; 246 hz
  .word    33  ; ms
  .byte   158  ; 392 hz
  .word   215  ; ms
  .byte   188  ; 329 hz
  .word    16  ; ms
  .byte   125  ; 494 hz
  .word   304  ; ms
  .byte    93  ; 658 hz
  .word  1732  ; ms
