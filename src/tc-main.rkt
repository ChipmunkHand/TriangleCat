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
         ;blacklist of addresses to not use
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


(define (add-16 target to-add)
  ; 16 bit little-endian numbers
  ; adds to-add onto target.
  ; pass in the low addresses.
  {
   clc
   lda to-add
   adc target
   sta target
   lda (+ to-add 1)
   adc (+ target 1)
   sta (+ target 1)
  })

(define (sub-16 target to-add)
  ; 16 bit little-endian numbers
  ; subs to-add from target.
  ; pass in the low addresses.
  {
   sec
   lda target
   sbc to-add
   sta target
   lda (+ target 1)
   sbc (+ to-add 1)
   sta (+ target 1)
   })

(define (copy-16 target source)
  ;copy the source to the target
  ;little endian, pass in the low address
  {
   lda source
   sta target
   lda (+ source 1)
   sta (+ target 1)
   })

(define (neg-16 target)
  ;in place twos complement negation
  ;clobbers vec-temp
  {
   lda target
   eor @$FF
   sta target
   lda (+ 1 target)
   eor @$FF
   sta (+ 1 target)
   (create-vec vec-temp-low 1)
   (add-16 target vec-temp-low)
  })

(define (vec/8 vec)
  ; destructive divide by 8 considering
  ; sign bit and 9th bit     
  {
   clc
   rol vec ; 2nd bit in
   rol (+ 1 vec) ; sign bit out
   rol vec ; 1st bit in
   rol (+ 1 vec) ; 9th bit out
   lda (+ 1 vec)
   ror
   lsr
   lsr
   })

(define (create-vec target value)
  ;creates a 16 bit vector at target (low address)
  ;uses 1 sign bit, 9 whole bits and 6 fractional.
  (if (integer? value) 
      (if (> value 0)
          (let* ([actual (arithmetic-shift value 6)]
                 [high   (arithmetic-shift (bitwise-and actual $FF00) -8)]
                 [low    (bitwise-and actual $FF)])             
            {
              (printf "value ~a actual ~a high ~a low ~a\n"value actual high low)
              lda @low
              sta target
              lda @high
              sta (+ target 1)
            })
          (error "create-vec doesnt yet support negative values"))
  (raise-argument-error 'create-vec "integer?" value)))
  

;some constants
{

  data-reg = $dd01

  ; screen bits
  x-visible = $18 ; subtract for sprite->char indexing
  y-visible = $32

  ; joy masks
  joy-up    = %0000_0001
  joy-down  = %0000_0010
  joy-left  = %0000_0100         
  joy-right = %0000_1000
  joy-fire  = %0001_0000

  ;directions
  top = 0
  right = 1
  bottom = 2
  left = 3
  
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
           (set-jump-source (format "~a" 'name) name)
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
   tc-state
   tc-vec-tempx-high
   tc-vec-tempx-low
   tc-vec-tempy-high
   tc-vec-tempy-low
   vec-temp-high
   vec-temp-low
   scratch-a
   scratch-b
   scratch-c
   scratch-d
   )
    
}

(C64{
   (hash-for-each sprites
    (λ (_ sprite)
      (begin
        (set-location (sprite-data-start-address sprite))
        (data (sprite-data-data sprite)))))

   *= $0801
;autostart
     (data $0b $08 $01 $00 $9E $31 $32 $32 $38 $38 $00 $00 $00 $00)
          
:programmer-check
         ;interrupt will be set on a negative transition
         ;indicating the pi has put the first byte of a program
         ;on the wire
         lda $dd0d
         and @%0001_0000
         ;if this bit is set then
         ;jump. reading this also clears it.
         bne cont+
         rts
:cont    inc $d020
         ; read control bytes
         start-lo = $40
         start-hi = $41
         total-pages = $43
         first-page-bytes = $44
         last-page-bytes = $45
         data-ptr-lo = $46
         data-ptr-hi = $47
         ;todo: write these as an indexed loop
         ;to save space
         (define (get loc) {
           lda data-reg
           sta loc
           jsr wait+
           })
         ;disable interrupts for loading
         cli
         (get start-lo)
         (get start-hi)
         (get data-ptr-lo)
         (get data-ptr-hi)
         (get total-pages)
         (get first-page-bytes)
         lda data-reg
         sta last-page-bytes
;         (get last-page-bytes)

         ;read/write first page bytes         
         ldx @0
         ldy @0
         lda first-page-bytes ;skip if zero
         beq main+ ;         
         ; read until end of page
      :next
         jsr wait+
         lda data-reg
         sta £ data-ptr-lo y
         iny
         cpy first-page-bytes
         bne next-

         ;move to next page
         inc data-ptr-hi
         lda @0
         sta data-ptr-lo

     :main
         ldy @0
         ldx total-pages
         beq last+         ;skip if zero
         ;copy whole pages
     :loop
         jsr wait+
         lda data-reg     
         sta £ data-ptr-lo y    
         iny
         bne loop-
         inc data-ptr-hi
         dex
         bne loop-              
     :last
        lda last-page-bytes
        beq done+
     :loop
        jsr wait+
        lda data-reg
        sta £ data-ptr-lo y        
        iny
        cpy last-page-bytes
        bne loop-
          
      :done
        ;re-enable interrupts, splat stack
        ;and jump to new execution point
        ;toggle pa2 to let the pi know we are done
        lda $dd00
        eor @%0000_0100
        sta $dd00
        ldx @$ff
      :delay
        dex
        bne delay-
        ldx @$ff
      :delay
        dex
        bne delay-
        ldx @$ff
        inc $0400
        ldx @$ff
        txs
        sei
        jmp £ start-lo 

     :wait
         ;toggle pa2 to let the pi know we are done
         lda $dd00
         eor @%0000_0100
         sta $dd00
         ;wait for pi to send a new byte
:inner   lda $dd0d
         and @%0001_0000                    
         beq inner-
         rts

     
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
            lda @0
         sta $dd03  ;pin all inputs

         ;pa2 is an output
         lda $dd02         
         ora @%0000_0100
         sta $dd02

         ;pa2 starts HIGH (inverted?)
         lda $dd00
         and @%1111_1011
         sta $dd00

         ;clear any pending int
         lda $dd0d

   (clear-mem $0400 32)
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
   lda $d012
   cmp @$40
   bne loop-
   inc $d020
   jsr collision-test:
   dec $d020
   ; wait for the raster to hit the bottom of the screen
:iloop
   lda $d012
   cmp @$ff
   bne iloop-
   jsr programmer-check-
   ; music
   dec $d020
   (play-psid acid-disco)
   jsr update+
   jsr animate+
   inc $d020
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
     sbc @(- $32 21)
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
     jsr sprite-y-char-bottom:  ; loads screen pointer
     jsr sprite-x-char-left:    ; loads offset
     tay
     lda £ screen-lo y          ; return value at 
     rts


:collision-direction
   ; a collision was found with the tile in A
   ; now we need to determine the direction it was hit from.
   ; returns 0 (top) 1 (right) 2 (bottom) 3 (left)
   
   ; to do this we will need TC's current and new map positions.
   ; we have ultimate X and Y of the new positions in scratch-a and scratch-b
   ; establish current positions.

   ; original X
   (copy-16 vec-temp-low tc-vec-x-low)
   (vec/8 vec-temp-low)
   sta scratch-c

   ; original Y
   (copy-16 vec-temp-low tc-vec-y-low)
   (vec/8 vec-temp-low)
   sta scratch-d

   ; Now we need to re-calcuate using just one component and see if the position
   ; (todo: we already calcualte this in the previous function so we can store it then.)
   ; does not change.  start with X
   (copy-16 vec-temp-low tc-vec-x-low)
   (add-16 vec-temp-low tc-vec-vx-low)
   (vec/8 vec-temp-low)      
   cmp scratch-c
   bne next+
   ;if these are equal, it must mean Y caused the collision.
   ;if VY is positive and TC is headed down across the screen, then the collision is the
   ;top of the platform, else bottom
   lda tc-vec-vy-high
   bmi neg+
   lda @top
   rts
:neg
   lda @bottom
   rts

:next
   ; same for Y 
   (copy-16 vec-temp-low tc-vec-y-low)
   (add-16 vec-temp-low tc-vec-vy-low)
   (vec/8 vec-temp-low)      
   cmp scratch-d
   bne next+
   ;if these are equal, it must mean X caused the collision.
   ;if VX is positive and TC is headed right across the screen, then the collision is the
   ;left of the platform, else right
   lda tc-vec-vx-high
   bmi neg+
   lda @left
   rts
:neg
   lda @right
   rts
   
:next
   ;if both caused a grid change then the smallest absolute vector wins
   (copy-16 tc-vec-tempx-low tc-vec-vx-low)
   lda tc-vec-tempx-high
   bmi neg+ 
   jmp skip+
:neg
   (neg-16 tc-vec-tempx-low)
:skip
   (copy-16 tc-vec-tempy-low tc-vec-vy-low)
   lda tc-vec-tempy-high
   bmi neg+
   jmp skip+
:neg
   (neg-16 tc-vec-tempy-low)
:skip
   ;compare high bytes
   lda tc-vec-tempx-high
   cmp tc-vec-tempy-high
   bcc less-than+     ; X < Y
   bne greater-than+ ; X > Y
   lda tc-vec-tempx-low
   cmp tc-vec-tempy-low
   bcs greater-than+  ; X > Y
:less-than
   ; X < Y, work out direction
   lda tc-vec-vx-high
   bmi neg+
   lda @left
   rts
:neg
   lda @right
   rts
   :greater-than
   ; X > Y, work out direction
   lda tc-vec-vy-high
   bmi neg+
   lda @top
   rts
:neg
   lda @bottom
   rts


   
:collision-test
   ; depending on the direction TC is going we will need to check
   ; up to 2 directions with 4 points on each side, 7 total (since one on a corner is shared)

   (copy-16 tc-vec-tempx-low tc-vec-x-low)
   (copy-16 tc-vec-tempy-low tc-vec-y-low)

   ; for the initial test we will only check if TC is going down and right.  Both are positive
   ; numbers in the vector.
   lda tc-vec-vx-high
   bpl next+
   rts
:next   
   lda tc-vec-vy-high 
   bpl next+
   rts
:next   
;   break
   
   ; first we take the "current position" and add the x and y vectors to it
   ; for this test that is the bottom centre of the sprite, so we need to add
   ; 24 to the Y and 12 to the X
   ; Y border is $32 pixels and X is $18 pixels which must always be subtracted
   ; that leaves us with subtracting ($32 - 24) 26 for Y and
   ; ($18 - 12) 12 for X

   (create-vec vec-temp-low 12)
   (sub-16 tc-vec-tempx-low vec-temp-low)

   (create-vec vec-temp-low 26)
   (sub-16 tc-vec-tempy-low vec-temp-low)

   (add-16 tc-vec-tempx-low tc-vec-vx-low)
   (add-16 tc-vec-tempy-low tc-vec-vy-low)
   
   ; test if there is a platform there.
   ; to do this we can divide by 8 (shift right 3)
   ; the vectors are stored with 9 biths of precision and a leading
   ; sign bit.  will need to drop the sign bit, shift in the remainig
   ; two bits leaving the highest bit in the carry ready to be rotated in
   
   ; for Y we need to rotate in the 8th bit and drop the sign
   (vec/8 tc-vec-tempy-low)
   tax 
;   sta scratch-b
   lda screen-rows-lo: x
   sta screen-lo
   lda screen-rows-hi: x
   sta screen-hi

   (vec/8 tc-vec-tempx-low)
 ;  sta scratch-a
   tay
   lda £ screen-lo y          ; return value at 
   sta scratch-a
   cmp @$20
   beq end+
   break
   jsr collision-direction:
   sta scratch-b

   ;lookup tile metadata
   ;; tax
   ;; lda tile-meta: x
   ;; sta scratch-c
   ;; lda @%0000_1000
   ;; bit scratch-c
   ;; beq skip+

   ; this flag is "invert y vector"
   (neg-16 tc-vec-vy-low)
   
:skip   
   
   dec $d021
;   inc $d021
   

   :end
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
   (add-16 tc-vec-vx-low $10)
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
   (add-16 tc-vec-vy-low $10)
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

   (add-16 tc-vec-x-low tc-vec-vx-low)
   (add-16 tc-vec-y-low tc-vec-vy-low)

   ; add gravity to y velocity
   clc
   lda tc-vec-vy-low
   adc @10
   sta tc-vec-vy-low
   bcc (+ (here) 3)
   inc tc-vec-vy-high
   
   ; project to screen co-ords

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
   sta $0402            ; indicate state machine on screen
   sta $0400
   lda tc-state
   sta $0400
   lda tc-state
   ;; cmp @state-jumping
   ;; bne skip+
   ;; lda tc-vec-vy-high
   ;; bmi skip+
   ;; jsr sprite-char-collision:
   ;; cmp @5
   ;; bne skip+
   ;; lda @0
   ;; sta tc-vec-vy-high
   ;; sta tc-vec-vy-low
   ;; ldx @state-standing
   ;; jsr change-state-
   ;; rts


:skip
(generate-state-machine
 ([state-standing
   { }
   ([joy-right #t state-walking-right ]
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




;; :tile-meta
;;   (data (for/list ([i (in-range 256)]) (lo-byte $FF)))



})

