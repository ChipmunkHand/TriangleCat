#lang asi64
(require (for-syntax syntax/parse))
(set-emulator-program! emu "c64.prg")
(set-emulator-execute?! emu #t)
(set-emulator-breakpoints?! emu #t)
(if (eq? (system-type 'os) 'windows)
  (set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")
  (set-emulator-path! emu "/snap/bin/vice-jz.x64"))

(require threading)
(require racket/string racket/list racket/file)
(require racket/list)
(struct sprite-data (frame-count data) #:transparent)

(define (extract-sprite filename)
  (let* (;read file as a sequence of lines
         [lines (file->lines filename)]
         ;count the amount of frames by looking at lines that end with :
         [frames (length (filter (λ (s) (string-suffix? s ":")) lines))]

         ;; [colours (~>>
         ;;           lines
         ;;           (filter (λ (s) (string-prefix? s "// sprite")))
         ;;           (map (λ (s)
         ;;                  (let* ([sl (string-length s)]
         ;;                         [start (- sl 2)])
         ;;                    (substring s start))))
         ;;           (map (λ (s) (string->number s 16))))]
         

         ; extract the raw data as one big lump
         [data (~>>
                lines
                ; filter to .byte rows 
                (filter (λ (s) (string-prefix? s ".byte")))
                ; clean up text leaving raw hex values
                (map (λ (s) (string-replace s ".byte " "")))
                (map (λ (s) (string-replace s "$" "")))
                (map (λ (s) (string-split s ",")))
                ; flatten into one big list of numbers
                (flatten)
                ; parse hex 
                (map (λ (s) (string->number s 16))))])

    (sprite-data frames data)))



(define sprites
  (~>>
   (directory-list "../sprites" #:build? #t)
   (map path->string)
   (filter (λ (s) (string-suffix? s ".txt")))
   (map extract-sprite)))
(writeln sprites)
(C64{

     *= $0801
     ;autostart
     (data $0b $08 $01 $00 $9E $31 $32 $32 $38 $38 $00 $00 $00)
     
     *= $1000

    ; raw sprite data starts at $2000
    *= $2000
    (data
     (~>>
      sprites
      (map sprite-data-data)
      (flatten)))
    
    *= $3000

      ;enable all sprites
      lda @$FF
      sta $d015

      (for/fold ([data-index $80]
                 [sprite-pointer $07f8]
                 [code (list)])
                ([s sprites])
        (let ([new-code
               {
                lda @data-index
                sta sprite-pointer            
               }])    
          (values (+ (sprite-data-frame-count s) data-index)
                  (+ 1 sprite-pointer)
                  (cons new-code code))))
      
      
      ;position sprites
      (for ([x (in-range 8)])
        {
          lda @(+ $20 (* x 24))
          sta (+ $d000 (* x 2))  ; x
          lda @$D0
          sta (+ $d000 (+ 1 (* x 2))) ; y
        })

     ; turn on multicolour mode for all sprites
     ;; lda @$FF
     ;; sta $d01c 
     lda @$04 ; sprite multicolor 1
     sta $D025
     lda @$06 ; sprite multicolor 2
     sta $D026

 ;    jsr $1000
     (define delay $15)
     (define is-animating $58)
     (define current-frame $60)
     (define current-offset-lo $61)
     (define current-offset-hi $62)
     (define current-temp $63)

     ; set background colour to black
     lda @0
     sta $d021
     sta is-animating

     lda @ delay
     sta $42

:loop
     ; wait for the raster to hit the bottom of the screen
     lda $d012
     cmp @$ff
     bne loop-
     lda is-animating
     beq joy+
     jmp skip-joy+
     :joy
     jsr check-joy:
     :skip-joy
     ; decrease our delay by one
     ldx $42
     dex
     ; if it is zero, branch out
     beq change+
     ; otherwise store the new value and go back to waiting
     stx $42
     jmp loop-
:change

         ldx @ delay
         stx $42

         (for/fold ([base-offset $80]
                    [index 0]
                    [code (list)])                   
                   ([ s sprites])
           (let ([new-code
                  {
                   ;load sprite pointer value
                   ldx (+ $07f8 index)
                       ;is it on the final frame?
;                       break
                   cpx @(+ base-offset (- (sprite-data-frame-count s) 1))
                   bne skip+
                   ;reset to its first frame
                   ldx @base-offset
                   stx (+ $07f8 index)
                   jmp done+
                   :skip
                   ; move to next frame
                   inx
                   stx (+ $07f8 index)
                   :done                   
                   }])
             (values (+ base-offset (sprite-data-frame-count s))
                     (+ 1 index)
                     (cons new-code code))))
         
         jmp loop-

:check-joy
     ldy @2   ;move amount
     sty $45
     lda $dc00
     tax
     and @%00010000  ; fire
     bne next+
     lda @1
     ;clear animation params
     sta is-animating
     lda @$40
     sta current-offset-hi
     lda @0
     sta current-offset-lo
     lda @1
     sta current-frame

         rts
         ;; ldy @4
         ;; sty $45
:next         
         txa 
         and @%00000001  ;up
         bne next+
         lda $d001
         sec
         sbc $45
         sta $d001
:next
         txa
         and @%00000010  ; down
         bne next+
         lda $d001
         clc
         adc $45         
         sta $d001
:next
         txa
         and @%00000100  ; left
         bne next+
         lda $d000
         sec
         sbc $45
         sta $d000

:next
         txa
         and @%00001000  ; right
         bne done+
         lda $d000
         clc
         adc $45
         sta $d000
:done         
         rts



 })
