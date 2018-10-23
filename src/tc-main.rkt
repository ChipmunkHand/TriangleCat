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
   
;; (hash-for-each sprites
;;   (λ (key sprite)
;;     (begin
;;       (writeln (format "fn:~a" key))
;;       (writeln (format "frames:~a" (sprite-data-frame-count sprite)))
;;       (writeln (format "address:~x" (sprite-data-start-address sprite)))
;;       (writeln (format "start:~x" (sprite-data-start-offset sprite))))))

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

(define-syntax-parser joy-branch
  [(_ #t) #'{bne skip+}] 
  [(_ #f) #'{beq skip+}])

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
       :skip
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

        (data 0 0 )
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
  
  ; ZP addresses

  ;screen lookup pointer addresses
  screen-lo = $B0
  screen-hi = $B1
  
  ; tc stuff
  tc-falling = $A4
  tc-vel-change-delay = $A5
  tc-angle-change-delay = $A6
  tc-table-frame = $A7
  tc-temp = $A8
  tc-vel = $A9
  tc-angle-target = $Aa; where the target is pointing in crouch
  tc-traj-lo = $Ab     ; pointers into trajectory tables
  tc-traj-hi = $Ac
  tc-anim-delay = $Ad
  tc-frame = $Ae
  tc-state = $Af
  
  
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

     lda @4
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
     
     ;position sprites
     (for ([x (in-range 8)])
       {
        lda @(+ $1F (* x 24))
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

     lda @angle-change-delay
     sta tc-angle-change-delay
     lda @vel-change-delay
     sta tc-vel-change-delay
     
     lda @0
     sta $d01b  ; sprites in front of chars
     
     sta $d021
     sta $d020
;break
;     lda @%00000001
;     sta $d010
     jsr $1000  ; init music

:loop
     ; wait for the raster to hit the bottom of the screen
     lda $d012
     cmp @$ff
     bne loop-
     ; music
     inc $d020
     ;jsr $1003
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
     ;;pace the target above the cat depending on its value
     ;todo: copy d010 x-bit
     ldx tc-angle-target
     cpx @5
     bcs pos+
     ; < 5 means we subtract the x
     lda $d000
     sec
     sbc target-offset-x+ x
     sta $d002
     jmp y+
     ; otherwise add it
:pos lda $d000
     clc
     adc target-offset-x+ x
     sta $d002
:y
     lda $d001
     sec
     sbc target-offset-y+ x
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

     (define (prepare-jump) {
       ; load lookup table address based on angle and vel
       lda @1
       sta tc-table-frame
       clc
       lda @>trajectory-table-0:              
       ldx tc-angle-target
       beq skip+  ; x was zero so we don;t need to add anything
:loop  adc @2
       dex
       bne loop-

:skip
       sta tc-traj-hi
       ; to work out the velocity there are only 4 cases so
       ; handle them explictly
       ldx tc-vel
       beq done+
       cpx @1         ; forward 80
       bne next+
       lda @$80
       sta tc-traj-lo
       rts
:next  cpx @2
       bne next+
       inc tc-traj-hi ; next bank
       lda @0
       sta tc-traj-lo
       rts
:next  cpx @3         ; next bank + 80
       inc tc-traj-hi ; next bank
       lda @$80
       sta tc-traj-lo
:done
       rts
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

; joy bits are 0 if pressed
:update     
; here the state machine is updated based on collisions and inputs
     lda tc-state
     sta $400
     lda tc-state
     cmp @state-jumping
     bne skip+
     lda tc-falling
     beq skip+
     jsr sprite-char-collision:     
     cmp @5
     bne skip+
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
     cpy @10
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
    [joy-left #f state-crouching]
    [joy-fire #f state-standing
               (deactivate-target)]
    )]

  [state-angle-left
   {
     ldx tc-angle-change-delay
     bne skip+
     lda @angle-change-delay
     sta tc-angle-change-delay
     ; decrease angle if we can
     ldy tc-angle-target
     beq joy+
     dey
     sty tc-angle-target
     jsr update-target-
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
    [joy-right #f state-crouching]
    [joy-fire #f state-standing
              (deactivate-target)])]

  [state-velocity
   {
    ldx tc-vel-change-delay
    bne skip+
    lda @vel-change-delay
    sta tc-vel-change-delay

    ldx tc-vel
    cpx @3
    beq zero+
    inx
    (data $2c)
:zero    
    ldx @0
:done
   stx tc-vel
   clc
   txa
;   adc @49
   sta $0402
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
                    (prepare-jump)
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
     jsr table-test+
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

    
:table-test
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
     clc
     adc @1
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
;break
lda @1
     sta tc-falling

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
     ldx @state-standing
     jsr change-state-
     lda @0
     sta tc-falling
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
     (data $30 $4 $4 $4 $4 $4 $4 $1 $4 $4 $4 $4 $4)

:target-offset-x  ; where to move the target for each angle position
                  ; the code knows < 4 is negative.     
     (data 12 9 6 3 0 3 6 10 12 15 17)

:target-offset-y   ; always negative     
     (data 15 17 18 19 20 19 18 17 15 12 9)

:screen-rows-lo
  (write-values
   (for/list ([i (in-range 25)])
     (lo-byte (+ (* i 40) $0400))))

:screen-rows-hi
  (write-values
   (for/list ([i (in-range 25)])
     (hi-byte (+ (* i 40) $0400))))
     })
