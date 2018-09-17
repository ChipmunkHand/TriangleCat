#lang asi64
(require (for-syntax syntax/parse))
(set-emulator-program! emu "c64.prg")
(set-emulator-execute?! emu #t)
(set-emulator-breakpoints?! emu #t)
(set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")
(require threading)
(require racket/string racket/list racket/file)
(require racket/list)

(struct sprite-data (frame-count colours data) #:transparent)

(define (extract-sprite filename)
  (let* ([lines (file->lines filename)]
         [frames (length (filter (λ (s) (string-suffix? s ":")) lines))]
         [colours (~>>
                   lines
                   (filter (λ (s) (string-prefix? s "// sprite")))
                   (map (λ (s)
                          (let* ([sl (string-length s)]
                                 [start (- sl 2)])
                            (substring s start))))
                   (map (λ (s) (string->number s 16))))]          
          
         [data (~>>
                lines
                (filter (λ (s) (string-prefix? s ".byte")))
                (map (λ (s) (string-replace s ".byte " "")))
                (map (λ (s) (string-replace s "$" "")))
                (map (λ (s) (string-split s ",")))
                (flatten)
                (map (λ (s) (string->number s 16))))])
    (sprite-data frames colours data)))
         
(define sprites
  (~>>
   (directory-list "..\\sprites" #:build? #t)
   (map path->string)
   (filter (λ (s) (string-suffix? s ".txt")))
   (map extract-sprite)))

(C64{
             
      *= $1000
         
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

:end-gen         
         lda @$FF
         sta $d01c 
         lda @$04 ; sprite multicolor 1
         sta $D025
         lda @$06 ; sprite multicolor 2
         sta $D026

         lda @0
         sta $d021


         (define delay $5)
         lda @ delay
         sta $42


         :loop
         lda $d012
         cmp @$ff
         bne loop-
         jsr check-joy:
         ldx $42
         dex
         beq change+
         stx $42
         jmp loop-
:change         
         ldx @ delay
         stx $42
         ;dont change unless some movement
         ;; lda $dc00
         ;; and @%00011111
         ;; cmp @%00011111
         ;; beq loop-

         (for/fold ([base-offset $80]
                    [index 0]
                    [code (list)])                   
                   ([ s sprites])
           (let ([new-code
                  {
                   ldx (+ $07f8 index) ;current sprite pointer
                   cpx @(+ base-offset (- (sprite-data-frame-count s) 1))
                   bne skip+
                   ldx @base-offset
                   stx (+ $07f8 index)
                   :skip
                   inx
                   stx (+ $07f8 index)
                   
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
         ldy @4
         sty $45
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

;;// sprite 1 / multicolor / color: $0e
*= $2000
(data
 (~>>
  sprites
  (map sprite-data-data)
  (flatten)))

 })
