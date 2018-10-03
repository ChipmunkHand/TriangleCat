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

(require (for-syntax syntax/parse))

(require "data.rkt")

(struct sprite-data
  (frame-count   
   start-address  ; raw data location
   start-offset   ; sprite pointer offset 
   end-offset     ; final frame
   colours        ; multicolours   
   data           ; actual data
   )  #:transparent)

(define (extract-sprite filename start-address start-offset)
  (let* (;read file as a sequence of lines
         [lines (file->lines filename)]
         ;count the amount of frames by looking at lines that end with :
         [frames (length (filter (λ (s) (string-suffix? s ":")) lines))]
         [colours (~>>
                   lines
                   (filter (λ (s) (string-prefix? s "// sprite")))
                   (map (λ (s)
                          (let* ([sl (string-length s)]
                                 [start (- sl 2)])
                            (substring s start))))
                   (map (λ (s) (string->number s 16))))]
         

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

    (sprite-data frames
                 start-address
                 start-offset
                 (+ start-offset (- frames 1))
                 colours
                 data)))

(define sprites
  (let ([files
         (~>>
          (directory-list "..\\sprites" #:build? #t)
          (map path->string)
          (filter (λ (s) (string-suffix? s ".txt"))))])
    (match-let-values
        ([(_ _ sprites)
          (for/fold ([address $2000]
                     [offset $80]
                     [sprites (list)])
                    ([filename files])
            (let
                ([short-name  ; extract file name (probably a nicer way to do this)
                  (~>
                   filename
                   (string-split "\\")
                   reverse
                   first
                   (string-replace ".txt" ""))]
                 [sprite (extract-sprite filename address offset)])
              ; build sprite data folding though the addresses and offsets for later use
              (values (+ address (* (sprite-data-frame-count sprite) $40))
                      (+ offset (sprite-data-frame-count sprite))
                      (cons (cons short-name sprite) sprites))))])
      ; hash by sprite name
      (make-hash sprites))))
        

(define (load-psid filename)
  (for ([b
         (~>
          filename
          file->bytes
          bytes->list
          (drop (+ 2 $7c)))])
    (write-value b)))
   
(hash-for-each sprites
  (λ (key sprite)
    (begin
      (writeln (format "fn:~a" key))
      (writeln (format "frames:~a" (sprite-data-frame-count sprite)))
      (writeln (format "address:~x" (sprite-data-start-address sprite)))
      (writeln (format "start:~x" (sprite-data-start-offset sprite))))))

(define (get-sprite-start name)
  (~>
   sprites
   (hash-ref name)
   sprite-data-start-offset))

(define (get-sprite-end name)
  (~>
   sprites
   (hash-ref name)
   sprite-data-end-offset))

(define (clear-mem start character)
  {	ldx @0
        lda @character            
 :loop	(for ([i '(0 1 2 3)])
           {sta (+ start (* i $100)) x})
       	dex
        bne loop- })

(define-syntax-parser expand-joy
  [(_ test #t target)
   #'{and @test
      bne skip+}]
    [(_ test #f target)
   #'{and @test
      beq skip+}])
            
(define-syntax-parser generate-joy-transitions
  [(_ ([test is-true? target (~optional extra #:defaults ([extra #'{}]))] ...))
   #'{lda $dc00
;      eor $FF
      tax      
      {
       (expand-joy test is-true? target) 
       ldx @target
       jsr change-state-
       extra
       rts
       :skip
       txa
       } ... }
   ])

;some constants
{
  ; joy masks
  joy-up    = %00000001
  joy-down  = %00000010
  joy-left  = %00000100         
  joy-right = %00001000
  joy-fire  = %00010000
  
  ; TC state machine states
  state-standing = $0
  state-walking-right = $1
  state-walking-left = $2
  state-crouching = $3
  state-angle-right = $4
  state-angle-left = $5
  state-velocity = $6
  state-jumping = $7
  state-skidding-right = $8
  state-skidding-left = $9 
  state-falling-right = $a 
  state-falling-left = $b
  state-dying = $c

  ; ZP addresses
  tc-table-frame = $f7
  tc-temp = $f8
  tc-vel = $f9
  tc-angle-target = $fa; where the target is pointing in crouch
  tc-traj-lo = $fb     ; pointers into trajectory tables
  tc-traj-hi = $fc
  tc-anim-delay = $fd
  tc-frame = $fe
  tc-state = $ff
  
  
}

(C64{
     (velocity-tables) ; from data.rkt
     (hash-for-each sprites
      (λ (_ sprite)
        (begin
          (set-location (sprite-data-start-address sprite))
          (data (sprite-data-data sprite)))))

;music 
*= $1000
    (load-psid "C:\\C64Music\\MUSICIANS\\A\\A-Man\\acid_disco.sid")    

; main program    
*= $3000
     (clear-mem $0400 32)
     ;enable sprite 1
     lda @%00000001
     sta $d015

     lda @(get-sprite-start "standing")      
     sta $07f8
     sta tc-frame
     lda @state-standing
     sta tc-state

     lda @4
     sta tc-angle-target
     sta tc-vel
     lda @(get-sprite-start "target")      
     sta $07f9


     
     
     ;position sprites
     (for ([x (in-range 8)])
       {
        lda @(+ $20 (* x 24))
            sta (+ $d000 (* x 2))  ; x
            lda @$D0
            sta (+ $d000 (+ 1 (* x 2))) ; y
            })

     ; turn on multicolour mode for all sprites
     lda @$FF
     sta $d01c

     lda @$04 ; sprite multicolor 1
     sta $D025
     lda @$06 ; sprite multicolor 2
     sta $D026

     lda @0
     sta $d01b  ; sprites in front of chars
     sta $d020
     sta $d021
  
 ;    jsr $1000  ; init music

:loop
     ; wait for the raster to hit the bottom of the screen
     lda $d012
     cmp @$ff
     bne loop-
     ; music
     inc $d020
;     jsr $1003
     jsr update+
     jsr animate+

     dec $d020
     jmp loop-


:update-target
     ;;place the target above the cat depending on its value
     lda $d000
     sta $d002
     ;todo: copy d010 x-bit  
     ldx tc-angle-target
     cpx @4    ; centre
     bne skip+
     lda $d001
     sec
     sbc @40
     sta $d003

:skip     
     rts
     
     (define (activate-target) {
       lda $d015
       ora @%00000010
       sta $d015
       jsr update-target-
       })
                              
     (define (deactivate-target) {
       lda $d015
       and @%11111101
       sta $d015
       
       })

     (define (prepare-jump) {
       ; load lookup table address based on angle and vel
       lda @1
       sta tc-table-frame
       ldx tc-angle-target
       cpx @4
       bne skip+
       lda @<trajectory-table-4:
       sta tc-traj-lo
       lda @>trajectory-table-4:
       sta tc-traj-hi
       jmp set-vel+       
    :skip


    :set-vel
       ; velocity
       ldx tc-vel
       cpx @4
       bne skip+
       inc tc-traj-hi

       lda @$80
       
       sta tc-traj-lo
       
    :skip       
     })
     
:animate
     ; the job of this is to simply cycle the current TC animation
     ; and nothing else.
     ldx tc-anim-delay
     beq change+
     dex
     stx tc-anim-delay
     rts     
:change
     ldy tc-state
     ldx frame-anim-speeds+ y
     stx tc-anim-delay
     ldx frame-end-offsets+ y
     cpx tc-frame
     beq reset+
     inc tc-frame
     jmp end+
:reset     
     ldx frame-start-offsets+ y
     stx tc-frame
:end
     ; copy to first sprite
     lda tc-frame
     sta $07f8
     rts


:change-state ; pass new state in x
     stx tc-state
     lda frame-start-offsets+ x
     sta tc-frame
     lda @0
     sta tc-anim-delay
     rts

     ; remember joy bits are 0 if pressed
:update     
     ; here the state machine is updated based on collisions and inputs
     ldy tc-state
     cpy @state-standing
     bne next-state+
     (generate-joy-transitions
      ([joy-right #t state-walking-right]
       [joy-left #t state-walking-left]
       [joy-down #t state-crouching
                 (activate-target)]))
     rts
:next-state     
     cpy @state-walking-right
     bne next-state+
     ;update walking right
     inc $d000
     inc $d000
     (generate-joy-transitions
      ([joy-left #t state-walking-left]
       [joy-right #f state-standing]))
     rts     
:next-state
     cpy @state-walking-left
     bne next-state+
     ;update walking left
     dec $d000
     dec $d000
     (generate-joy-transitions
      ([joy-right #t state-walking-right]
       [joy-left #f state-standing]))
     rts
:next-state
     cpy @state-crouching
     bne next-state+
     ;update crouching
     (generate-joy-transitions
      ([joy-fire #t state-velocity]
       [(bitwise-ior joy-right joy-down) #t state-angle-right]
       [(bitwise-ior joy-left joy-down) #t state-angle-left]
       [joy-right #t state-walking-right
                  (deactivate-target)]
       [joy-left #t state-walking-left
                 (deactivate-target)]
       [joy-down #f state-standing
                 (deactivate-target)]))
     rts

:next-state
     cpy @state-angle-left
     bne next-state+
     ;update angle left
     (generate-joy-transitions
      ([joy-fire #t state-velocity]
       [(bitwise-ior joy-right joy-down) #t state-angle-right]
       [(bitwise-ior joy-left joy-down) #t state-angle-left] ; todo - this should do nothing?
       [joy-down #t state-crouching]
       [joy-right #t state-walking-right
                  (deactivate-target)]
       [joy-left #t state-walking-left
                 (deactivate-target)]))
     rts

:next-state
     cpy @state-angle-right
     bne next-state+
     ;update angle right
     (generate-joy-transitions
      ([joy-fire #t state-velocity]
       [(bitwise-ior joy-right joy-down) #t state-angle-right]
       [(bitwise-ior joy-left joy-down) #t state-angle-left] ; todo - this should do nothing?
       [joy-down #t state-crouching]
       [joy-right #t state-walking-right
                  (deactivate-target)]
       [joy-left #t state-walking-left
                 (deactivate-target)]
))
     rts

:next-state
     cpy @state-velocity
     bne next-state+
     ;update vel
     (generate-joy-transitions
      ([joy-fire #f state-jumping
                 (begin
                   (deactivate-target)
                   (prepare-jump))]))
     rts

:next-state
     cpy @state-jumping
     bne next-state+
     jsr table-test+
     ;jumping, allow some sway if headed downwards.
     (generate-joy-transitions
      ([joy-fire #t state-standing]))
     rts

     
:next-state
     rts
    

:table-test
;     break
     ldy @0
     ; x delta appears at the first byte
     lda £ tc-traj-lo y
     ; move sprite x. check sign bit
     tax
     and @%10000000
     beq add+
     ; is negative, invert bits and sub
     txa
     eor @$ff
     sta tc-temp
     lda $d000
     sec
     sbc tc-temp
     sta $d000     
     jmp y+
:add    
     clc
     txa
     adc $d000
     sta $d000
:y
     
     ; load y delta
     ldy tc-table-frame
     lda £ tc-traj-lo y
     tax
     and @%10000000
     beq add+
     ; is negative, invert bits and sub
     txa
     eor @$ff
     sta tc-temp
     lda $d001
     sec
     sbc tc-temp
     sta $d001     
     jmp end+
:add     
     clc
     txa
     adc $d001
     sta $d001
     :end
     clc
     lda tc-table-frame
     adc @1
     bmi done+
     sta tc-table-frame
     rts
:done
     lda @state-standing
     sta tc-state
     rts


     
     states = (list "standing" "walking-right" "walking-left"
                     "crouching" "angle-right" "angle-left"
                     "velocity" "jumping" "skidding-right"
                     "skidding-left" "falling-right"
                     "falling-left" "dying")
:frame-start-offsets
      (for ([s states]) 
        (write-value (get-sprite-start s)))

:frame-end-offsets
      (for ([s states]) 
        (write-value (get-sprite-end s)))

:frame-anim-speeds      
     (data $30 $4 $4 $4 $4 $4 $4 $4 $4 $4 $4 $4 $4)
 })
