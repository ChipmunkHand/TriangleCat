#lang asi64
(set-emulator-program! emu "tc.prg")
(set-emulator-execute?! emu #t)
(set-emulator-breakpoints?! emu #t)
(set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")

(require
 syntax/parse/define
 threading
 racket/string
 racket/list
 racket/file)

(require
 "global.rkt"
 "graphics.rkt"
 "charset.rkt"
 "tc-state.rkt"
 "remote-programmer.rkt"
 "psid.rkt"
 "spritemate.rkt"
 "maths.rkt"
 "collision.rkt")

(require (for-syntax syntax/parse))

(define (clear-mem start character)
  {	ldx @0
        lda @character            
 :loop	(for ([i '(0 1 2 3)])
           {sta (+ start (* i $100)) x})
       	dex
        bne loop- })




(C64{
   (hash-for-each sprites
    (λ (_ sprite)
      (begin
        (set-location (sprite-data-start-address sprite))
        (data (sprite-data-data sprite)))))   
*= $0801
   ;autostart 163844 ($4000)
   (data $0b $08 $01 $00 $9E $31 $36 $33 $38 $34 $00 $00 $00 $00)          
   (remote-programmer-main)
     
   *= $3000
   (for ([b (bytes->list  (file->bytes "C:\\Users\\juan\\Documents\\char-test.bin"))])
     (data b))

   ;(generate-charset)


*= $4000
   ; main program    
   (remote-programmer-init)

   (let ([colours (bytes->list (file->bytes "C:\\Users\\juan\\Documents\\char-test-att.bin"))])
     (for ([x (level-1)]
           [i (in-naturals)])     
       {lda @x
        sta (+ $0400 i)
        lda @(bitwise-and $f (list-ref colours x))
        sta (+ $d800 i)}))
         ;; (case x
         ;;   [(1) {lda @$e
         ;;          sta (+ $D800 i)}]
         ;;   [(2) {lda @$d
         ;;          sta (+ $D800 i)}]
         ;;   [(3) {lda @$d
         ;;          sta (+ $D800 i)}]
         ;;   [(4) {lda @$d
         ;;          sta (+ $D800 i)}]
         ;;   )
         ;; })
   
   ;(clear-mem $0400 0)
   ;enable sprite 1
   lda @%00000001
   sta $d015
   ;     lda @$35   ; turn off the BASIC and KERNAL
   ;     sta $01    

   lda @(get-sprite-start "standing")      
   sta $07f8
   sta tc-frame
   lda @state-standing
   sta tc-state
   lda @anim-standing
   sta tc-anim-type
   lda @54
   sta tc-angle-target
   lda @3     
   sta tc-vel
   lda @(get-sprite-start "target")      
   sta $07f9     

   ; chars at $1000
;   lda $d018
    lda @%0001_1100  ;chars $3000, screen $0400
;   ora @%0000_1110
   sta $d018

   ;we will put sprites at $2000, code starts at $3000

   ;; lda @$ff
   ;; sta $3801
   ;; sta $3802
   ;; sta $3803
   ;; sta $3804
   ;; sta $3805
   ;; sta $3806
   ;; sta $3807


   ;; lda @3
   ;; sta $690
   ;; sta $691
   ;; sta $692
   ;; sta $693
   ;; sta $694
   ;; sta $695
   
   ;; sta $540
   ;; sta $541
   ;; sta $542
   ;; sta $543
   ;; sta $544
   ;; sta $545   

   ;; sta $45D
   ;; sta $45E
   ;; sta $45F
   ;; sta $460
   ;; sta $461
   ;; sta $462

   
   
   ;; sta $450
   ;; sta $451
   ;; sta $452
   ;; sta $453
   
   ;; sta $6A5
   ;; sta $6A6
   ;; sta $6A7
   ;; sta $6A8
   ;; sta $6A9
   ;; sta $6AA


 ;;   (for ([x (in-range 40)])
;;      {sta (+ $0400 x)
;;       sta (+ $07C0 x)})

;;    (for ([x (in-range 24)])
;;      { sta (+ $0400 (* x 40))
;;        sta (+ $0400 (* x 40) 39)

;;        }     )
;; 
   
   ;position sprites
   (for ([x (in-range 8)])
     {
      lda @(+ $1F (* x 24))
      sta (+ $d000 (* x 2))  ; x
      lda @$D0
      sta (+ $d000 (+ 1 (* x 2))) ; y
       })

   lda $d000
   lda $d001
   
   ; turn on multicolour mode for all sprites
   lda @$FF
   sta $d01c

   lda @$04 ; sprite multicolor 1
   sta $D025

   
   lda @$06 ; sprite multicolor 2
   sta $D026


   ;multicolour chars
   lda $d016
   ora @$10
   sta $d016

   lda @3
   sta $d022 ;mc 1
   lda @8
   sta $d023 ;mc 1
   
   
   (create-vec tc-vec-vx-low  0)
   (create-vec tc-vec-vy-low  0)

   (create-vec tc-vec-x-low  50)
   (create-vec tc-vec-y-low  50   )

   lda @angle-change-delay
   sta tc-angle-change-delay
   lda @vel-change-delay
   sta tc-vel-change-delay

   
   
   lda @0
   sta $d01b  ; sprites in front of chars
   
   sta $d021
   sta $d020

   sta tc-angle-vxi
   sta tc-angle-vyi
   sta tc-vec-vy-low
   sta tc-vec-vy-high





;     lda @%00000001
;     sta $d010
   (init-psid acid-disco)

   ;todo: raster interupts. this'll do for now
:loop
   lda $d012
   cmp @$3A
   bne loop-
;   inc $d020
   (play-psid acid-disco)
;   inc $d020
   lda tc-state
   sta tc-prev-state
   jsr state-update:
   jsr global-physics:
;   inc $d020
   jsr collision-detection:
;   dec $d020
;   dec $d020
;   dec $d020
   ; wait for the raster to hit the bottom of the screen
:iloop
   lda $d012
   cmp @$ff
   bne iloop-
   inc $d020
   jsr programmer-check:   
   jsr render:
   jsr animate-new:
   dec $d020
   jmp loop-

   :global-physics
   
   (create-fractional-vec vec-temp-low 9)
   (add-16 tc-vec-vy-low vec-temp-low)

   ;todo: impose velocity caps on TC

   rts

   (graphics-code)
   (generate-collision-metadata-code)
   (state-machine-code)
   (collision-detection-code)
   
    
;sin/cos tables in 5 degreee increments
;/= $100
(define (deg->rad rad)
  (* (/ pi 180) rad))

:sine
(data
(for/list ([x (in-range 0 365 5)])
  (bitwise-and (exact-round (* (sin (deg->rad x)) 63)) #xFF)))

:cosine
(data
(for/list ([x (in-range 0 365 5)])
  (bitwise-and (exact-round (* (cos (deg->rad x)) 63)) #xFF)))

;/= $100
:screen-rows-lo
  (write-values
   (for/list ([i (in-range 25)])
     (lo-byte (+ (* i 40) $0400))))

:screen-rows-hi
  (write-values
   (for/list ([i (in-range 25)])
     (hi-byte (+ (* i 40) $0400))))



/= $100
(generate-collision-meta)


})

