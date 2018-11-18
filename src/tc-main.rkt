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

(require "psid.rkt" "spritemate.rkt")
(require (for-syntax syntax/parse))
        
(define (zp-generator exceptions)
  (let* ([next-addr $ff]
         ;whitelist of addresses to not use
         [kernal (list $a0 $a1 $a2)]
         [exceptions (remove-duplicates (append exceptions kernal))])
    (λ ()
      (define (aux)
        (let ([addr next-addr])
          (set! next-addr (- next-addr 1))
          (if (set-member? exceptions addr)
              (aux)
              addr)))
      (aux))))

(define (clear-mem start character)
  {	ldx @0
        lda @character            
 :loop	(for ([i '(0 1 2 3)])
           {sta (+ start (* i $100)) x})
       	dex
        bne loop- })
    
(define-syntax-parser joy-branch
  [(_ #t) #'{bne joy-skip+}] 
  [(_ #f) #'{beq joy-skip+}])

(define-syntax-parser generate-joy-transitions 
  [(_ ([test is-pressed? target (~optional extra #:defaults ([extra #'{}]))] ...))
   #'{lda $dc00
      tax      
      {
       and @test
       (joy-branch is-pressed?) 
       ldx @target
       jsr change-state-
       extra
       rts
       :joy-skip
       txa
       } ... }
   ])

(define-syntax-parser generate-state-machine
  [(_ ([state-number
        update-code
        joy-cases] ...))
     #'{
        ldx tc-state            ; load current state
        lda state-machine-lo: x ; use lookup table and setup
        sta jump-vector-lo+     ; 16 bit address pointer
        lda state-machine-hi: x
        sta jump-vector-hi+
        jmp £ jump-vector-lo:    ; jump to target state
        
        ; write out the states 
        {
         ;set jump location
         (set-jump-source-current (format "state~a" state-number)) 
         update-code
         (generate-joy-transitions joy-cases)
         rts
        } ...

       (define jump-labels
         (~>>
          (list state-number ...)
          (map (λ (n) (format "state~a" n)))
          (map (λ (n) (find-closest-label n (here) '-)))))

;        (data 0 0 )
        :state-machine-lo
        (write-values (map lo-byte jump-labels))
        
        :state-machine-hi
        (write-values (map hi-byte jump-labels))

        :jump-vector-lo (data $FF)
        :jump-vector-hi (data $FF)

        }])


;some constants
{
  ; screen bits
  x-visible = $18 ; subtract for sprite->char indexing
  y-visible = $32

  ; joy masks
  joy-up    = %0000_0001
  joy-down  = %0000_0010
  joy-left  = %0000_0100         
  joy-right = %0000_1000
  joy-fire  = %0001_0000
  
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

  angle-change-delay = $4
  vel-change-delay = $14

  (define-syntax-parser zp
  [(_ name ...)
   #'{ (begin
         (define name (new-zp))
         (printf "~a = $~x\n" 'name name)
         ) ...}])

  acid-disco = (load-psid "C:\\C64Music\\MUSICIANS\\A\\a-man\\acid_disco.sid")
  new-zp = (zp-generator (psid-header-zp acid-disco))

  ; ZP addresses
  ; ---------------------------
  (zp ; assigned from $ff downwards
   screen-hi
   screen-lo
   tc-angle-vxi
   tc-angle-vyi
   ;store vectors little endian 
   tc-vec-x-high
   tc-vec-x-low
   tc-vec-y-high
   tc-vec-y-low
   tc-vec-vx-high
   tc-vec-vx-low
   tc-vec-vy-high
   tc-vec-vy-low
   tc-falling 
   tc-vel-change-delay 
   tc-angle-change-delay 
   tc-table-frame 
   tc-temp 
   tc-vel 
   tc-angle-target 
   tc-anim-delay 
   tc-frame 
   tc-state)
    
}

(C64{

   (hash-for-each sprites
    (λ (_ sprite)
      (begin
        (set-location (sprite-data-start-address sprite))
        (data (sprite-data-data sprite)))))

;music 

     
*= $3800
   (data
    ; 0
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    ;1
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %11111111
    ;2
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %11111111    
    %11111111
    ;3
    %00000000
    %00000000
    %00000000
    %00000000
    %00000000
    %11111111    
    %11111111    
    %11111111
    ;4
    %00000000
    %00000000
    %00000000
    %00000000
    %11111111
    %11111111    
    %11111111    
    %11111111

    ; block
    %00000000
    %11111111
    %11111111
    %11111111
    %11111111    
    %11111111    
    %11111111

    )
; main program    
*= $3000
   (clear-mem $0400 32)
;enable sprite 1
;break
   lda @%00000001
   sta $d015
   ;     lda @$35   ; turn off the BASIC and KERNAL
   ;     sta $01    

   lda @(get-sprite-start "standing")      
   sta $07f8
   sta tc-frame
   lda @state-standing
   sta tc-state

   lda @54
   sta tc-angle-target
   lda @3     
   sta tc-vel
   lda @(get-sprite-start "target")      
   sta $07f9     

   ; chars at $3800
   lda $d018
   ora @%00001110
   sta $d018

   ;; lda @$ff
   ;; sta $3801
   ;; sta $3802
   ;; sta $3803
   ;; sta $3804
   ;; sta $3805
   ;; sta $3806
   ;; sta $3807


   lda @5
   sta $690
   sta $691
   sta $692
   sta $693
   sta $694
   sta $695
   
   sta $540
   sta $541
   sta $542
   sta $543
   sta $544
   sta $545   

   sta $430
   sta $431
   sta $432
   sta $433
   sta $434
   sta $435

   sta $6A5
   sta $6A6
   sta $6A7
   sta $6A8
   sta $6A9
   sta $6B0

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

:loop
     ; wait for the raster to hit the bottom of the screen
     lda $d012
     cmp @$ff
     bne loop-
     ; music
     inc $d020
     (play-psid acid-disco)
     jsr update+
     jsr animate+
     dec $d020
     jmp loop-

:sprite-y-char-top
     lda $d001
     sec
     sbc @$32       
     lsr		
     lsr
     lsr
     tax
     lda screen-rows-lo: x
     sta screen-lo
     lda screen-rows-hi: x
     sta screen-hi
     rts

:sprite-y-char-bottom
     lda $d001
     sec
     sbc @(-$32 21)
     lsr		
     lsr
     lsr
     tax
     lda screen-rows-lo: x
     sta screen-lo
     lda screen-rows-hi: x
     sta screen-hi
     rts

:sprite-x-char-left     
     ; calculate the x chracter position of sprite 0
     ; taking into consideration the 9th bit
     ; returns the char index in A
     tmp1 = $28
     lda $d000
     sec            
     sbc @$18	;visible screen area starts at x=$18
     sta tmp1   
     lda $d010  ; load the msb
     and @%0000_0001
     lsr        ; cycle into carry
     lda tmp1
     ror	; rotate/shift 3 times 
     lsr
     lsr
     ; return in A
     rts
     
:sprite-char-collision
     ; this routine returns the character index
     ; where the sprite is currently located.
     ; pass in additional X and Y offsets in the X Y registers.
     ; this can then be used to check different parts of the sprite
     ; out even outside of the sprite.
     ; currently hardcoded to work with sprite 0 only
     jsr sprite-y-char-bottom:
     jsr sprite-x-char-left:
     tay
     lda £ screen-lo y
     rts
     
     
:update-target
     ;;place the target above the cat depending on its value
     ;todo: copy d010 x-bit
     lda @0
     sta tc-angle-vxi
     sta tc-angle-vyi
;break
     ldx tc-angle-target
     lda cosine+ x
     ; vx = velocity (2) * cos(angle)
     ; here we will load 32 * cos(angle)
     ; it is quckier to load the cos in as an integer
     ; and shift it down twice.
     ; if the msb is set then rotate in carry bits to maintain
     ; twos complement
     bmi x-neg+
     lsr
     lsr
     sta tc-angle-vxi
     jmp y+
:x-neg
     sec
     ror
     sec
     ror
     sta tc-angle-vxi
:y      
     clc
     ldx tc-angle-target
     lda sine+ x
     bmi y-neg+
     lsr
     lsr
     sta tc-angle-vyi
     jmp done+

:y-neg
     sec
     ror
     sec
     ror
     sta tc-angle-vyi

:done
     clc
     lda tc-angle-vxi
     bmi x-neg+
     adc $d000
     sta $d002
     bcc y+
     lda @%00000010
     ora $d010
     sta $d010
     jmp y+
:x-neg
     eor @$ff
     adc @1     
     sta $10
     lda $d000
     sec
     sbc $10
     sta $d002
     bcc y+
     lda @%1111_1101
     and $d010
     sta $d010

:y
     clc
     lda tc-angle-vyi
     bmi y-neg+
     adc $d001
     sta $d003
     rts

:y-neg     
     eor @$ff
     adc @1
     sta $10
     lda $d001
     sec
     sbc $10
     sta $d003
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

:prepare-jump
   ; calculate the velcoity vector for the jump
   ; we have two bytes; a sign bit, 9 whole bits
   ; and 6 fraction bits.
   ; the trig tables are calcuated to 6 bits of accuracy,
   ; if they are negative then fill in all the other  
   ; high bits with 1s to make the whole thing negative.
   ; this can then be multiplied for extra
   ; power

   ;initialze X component
   
   ldx tc-angle-target
   lda cosine: x
   sta tc-vec-vx-low
   sta $10
   bpl (+ (here) 4)
   lda @$ff
   (data $2c) 
   lda @0
   sta tc-vec-vx-high
   sta $11
   
   ; now multiply by tc-vel
   ldy tc-vel
:loop
   beq done+
   clc
   break
   lda $10
   adc tc-vec-vx-low
   sta tc-vec-vx-low
   lda $11
   adc tc-vec-vx-high
   sta tc-vec-vx-high
:skip2
   dey
   jmp loop-
:done      

   ;initialze Y component
   lda sine: x
   sta tc-vec-vy-low
   sta $10
   bpl (+ (here) 4)
   lda @$ff
   (data $2c) 
   lda @0
   sta tc-vec-vy-high
   sta $11
   

   ldy tc-vel
:loop
   beq done+
   clc
   lda $10
   adc tc-vec-vy-low
   sta tc-vec-vy-low
   lda $11
   adc tc-vec-vy-high
   sta tc-vec-vy-high
:skip
   dey
   jmp loop-
   :done 

   ; map the sprite to the current position vector
   ; (temporary until everything is based on it)

   lda @0
   sta tc-vec-x-low
   sta tc-vec-y-low

   
   ;we must factor in the 9th bit in d010
   ;we can cheat here since the last bit is the first
   ;sprite which is TC.
   lda $d010
   ror ; 9th bit in carry
   lda $d000
   ror
   ror tc-vec-x-low
   ror
   ror tc-vec-x-low
   sta tc-vec-x-high

   ;y is slightly easier
   lda $d001
   ror
   ror tc-vec-y-low
   ror
   ror tc-vec-y-low
   sta tc-vec-y-high
   
   rts

     
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


   
:update-jump
   ; add the velocity vector to the
   ; position vector. don't care about two's complement
   ; here, all will work out when projecting to the screen
   ; at the end.

   clc
   lda tc-vec-vx-low
   adc tc-vec-x-low
   sta tc-vec-x-low
   lda tc-vec-vx-high
   adc tc-vec-x-high
   sta tc-vec-x-high
   clc
   lda tc-vec-vy-low
   adc tc-vec-y-low
   sta tc-vec-y-low
   lda tc-vec-vy-high
   adc tc-vec-y-high
   sta tc-vec-y-high


   ; add gravity to y velocity
   clc
   lda tc-vec-vy-low
   adc @10
   sta tc-vec-vy-low
   bcc (+ (here) 3)
   inc tc-vec-vy-high
   
   ; project to screen co-ords
?=
   ;x
   lda tc-vec-x-high
   sta $10
   lda tc-vec-x-low
   ; drop the sign bit
   rol
   rol $10
   ; drop the 9th bit (into carry)
   rol
   rol $10
   lda $10
   sta $d000
   bcc zero+
   lda @1
   ora $d010
   sta $d010
   bcc y+
:zero
   lda @%1111_1110
   and $d010
   sta $d010
   =?
   ;y
:y lda tc-vec-y-high
   sta $10
   lda tc-vec-y-low
   rol
   rol $10
   rol
   rol $10
   lda $10
   sta $d001


   
   rts


; joy bits are 0 if pressed
:update     
; here the state machine is updated based on collisions and inputs
   lda tc-angle-target
   sta $402
   sta $400
   lda tc-state
   sta $400
   lda tc-state
   cmp @state-jumping
   bne skip+
   lda tc-vec-vy-high
   bmi skip+
   jsr sprite-char-collision:
   cmp @5
   bne skip+
   lda @0
   sta tc-vec-vy-high
   sta tc-vec-vy-low
   ldx @state-standing
   jsr change-state-
   rts

:skip
(generate-state-machine
 ([state-standing
   { }
   ([joy-right #t state-walking-right]
    [joy-left  #t state-walking-left]
    [joy-fire  #t state-crouching
               (activate-target)])]
               
  [state-walking-right
   {
    ; move sprite right
    lda $d000
    clc
    adc @2
    sta $d000
    bcc skip+
    lda @%00000001
    eor $d010
    sta $d010
:skip    
   }    
   ([joy-left  #t state-walking-left]
    [joy-right #f state-standing])]
  
  [state-walking-left
   {
    ; move sprite left
    lda $d000
    sec
    sbc @2
    sta $d000
    bcs skip+
    lda @%00000001
    eor $d010
    sta $d010
:skip    
    }
   ([joy-right #t state-walking-right]
    [joy-left  #f state-standing])]

  [state-crouching
   {}
   ([joy-down #t state-velocity]
    [joy-left #t state-angle-left
          { lda @0
            sta tc-angle-change-delay } ]
    [joy-right #t state-angle-right
          { lda @0
            sta tc-angle-change-delay }] 
    [joy-fire #f state-standing
               (deactivate-target)])]
  
  [state-angle-right
   {
     ldx tc-angle-change-delay
     bne skip+
     lda @angle-change-delay
     sta tc-angle-change-delay
     ; increase angle if we can
     ldy tc-angle-target
     cpy @70
     beq joy+
     iny
     sty tc-angle-target
     jsr update-target-
     jmp joy+
:skip
     dex
     stx tc-angle-change-delay
:joy     
   }
   ([joy-down #t state-velocity]
    [joy-left #t state-angle-left
          { lda @0
            sta tc-angle-change-delay }]
    [joy-fire #f state-standing
              (deactivate-target)]
    [joy-left #f state-crouching]
    )]

  [state-angle-left
   {
     ldx tc-angle-change-delay
     bne skip+
     lda @angle-change-delay
     sta tc-angle-change-delay
     ; decrease angle if we can
     ldy tc-angle-target
     cpy @38
     beq joy+
     dey
     sty tc-angle-target
     jsr update-target-
     jmp joy+
;; :reset
;;      lda @$48
;;      sta tc-angle-target
     jmp joy+
:skip
    dex
     stx tc-angle-change-delay

     
:joy     
   }
   ([joy-down #t state-velocity]
    [joy-right #t state-angle-right
               { lda @0                     
                 sta tc-angle-change-delay }]
    [joy-fire #f state-standing
              (deactivate-target)]
    [joy-right #f state-crouching])]

  [state-velocity
   {

    ldx tc-vel-change-delay
    bne skip+
    lda @vel-change-delay
    sta tc-vel-change-delay
    ldx tc-vel
    cpx @5
    beq zero+
    inx
    (data $2c)
:zero    
    ldx @1
:done
    stx tc-vel
    clc
    txa
;   adc @49
    sta $0404
    clc
    bcc joy+
:skip
   dex
   stx tc-vel-change-delay
:joy
   }
   ([joy-down #f state-jumping    
                 {
                    lda @vel-change-delay
                    sta tc-vel-change-delay
                    lda @0
                    sta tc-falling
                    (deactivate-target)
                    jsr prepare-jump:
                    }]
    [joy-down #f state-crouching])]
                     
  [state-jumping
   {
     ldx $d001
     cpx @$d1
     bcc cont+
     lda @$d0
     sta $d001
     ldx @state-standing
     jsr change-state:
     rts
:cont
     jsr update-jump:
     ;jumping, allow some sway if headed downwards.
     }
   (;[joy-fire #t state-standing]
    )]

  [state-skidding-left
   {}
   ()]

  [state-skidding-right
   {}
   ()]

  [state-falling-right
   {}
   ()]

  [state-falling-left
   {}
   ()]

  [state-dying
   {}
   ()]
  
   ))
    

     
   states = (list "standing" "walking-right" "walking-left"
                     "crouching" "angle-right" "angle-left"
                     "velocity" "jumping" "skidding-right"
                     "skidding-left" "falling-right"

                     "falling-left" "dying")
;/= $100
:frame-start-offsets
      (for ([s states]) 
        (write-value (get-sprite-start s)))

:frame-end-offsets
      (for ([s states]) 
        (write-value (get-sprite-end s)))

:frame-anim-speeds      
     (data $30 $4 $4 $4 $4 $4 $4 $1 $4 $4 $4 $4 $4)

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
     })


