#lang asi64
(require
 threading
 syntax/parse/define
 "global.rkt"
 "maths.rkt"
 "spritemate.rkt")
(provide state-machine-code)
    
(define-syntax-parser joy-branch
  [(_ #t) #'{bne joy-skip+}] 
  [(_ #f) #'{beq joy-skip+}])

(define-syntax-parser generate-joy-transitions
  [(_ ([test is-pressed? to-execute] ...))
   #'{lda $dc00
      tax      
      {
       and @test
       (joy-branch is-pressed?) 
       to-execute
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
     (define (activate-target) {
       lda $d015
       ora @%00000010
       sta $d015       
       jsr update-target:
       })
                              
     (define (deactivate-target) {
       lda $d015
       and @%11111101
       sta $d015
       
       })

(define (state-machine-code)
  {
:state-update
   lda tc-angle-target
   sta $0402            ; indicate state machine on screen
   sta $0400
   lda tc-state
   sta $0400
   lda tc-state
   jsr update-target:
   ; first try to update a state based on what we can observe.
   ; first, if vy is +/- then TC is jumping / falling.  the platforms
   ; will clamp these values to zero when they are landed on so
   ; otherwise the player must have jumped or fell
   ; it is possible they are already jumping or falling, which is fine

   ; state =
   ;   if vy <> 0 then airborne
   ;   elif state = airborne then standing
   ;   else state
   lda tc-vec-vy-high
   bne next+
   lda tc-vec-vy-low
   bne next+
   lda tc-state
   cmp @state-airborne
   bne machine+
   lda @state-standing
   sta tc-state 
   jmp machine+
   :next
   break
   lda @state-airborne
   sta tc-state
   (deactivate-target)


   :machine

 (generate-state-machine
   ([state-standing
     { }
     ([joy-right #t
        {
         (create-fractional-vec vec-temp-low 10)
         (add-16 tc-vec-vx-low vec-temp-low)
        } ]
      [joy-left  #t
         {
         (create-fractional-vec vec-temp-low 10)
         (sub-16 tc-vec-vx-low vec-temp-low)
        } ]
      [joy-fire  #t
        {
         (activate-target)
         ldx @state-crouching
         jsr change-state+
          
                  } ])
     ]

    [state-crouching
     {}
     ([joy-down #t
        {ldx @state-velocity
         jsr change-state:
        }]
      [joy-left #t 
          { ldx tc-angle-change-delay
            bne skip+
            lda @angle-change-delay
            sta tc-angle-change-delay
            ; decrease angle if we can
            ldy tc-angle-target
            cpy @38
            beq joy+
            dey
            sty tc-angle-target
            jsr update-target:
            jmp joy+
          :skip
            dex
            stx tc-angle-change-delay
          :joy
            } ]
      [joy-right #t
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
               jsr update-target:
               jmp joy+
             :skip
               dex
               stx tc-angle-change-delay
             :joy
             }]
      [joy-fire #f
                {
                 ldx @state-standing
                 jsr change-state:
                 (deactivate-target)
                 }])]


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
   ([joy-down #f 
                 {
                    lda @vel-change-delay
                    sta tc-vel-change-delay
                    lda @0
                    sta tc-falling
                    (deactivate-target)
                    jsr prepare-jump:
                    }]
    [joy-down #f
              { ldx @state-crouching
                jsr change-state: }])]

    [state-airborne
     {}
     ([joy-right #t
        {
         (create-fractional-vec vec-temp-low 5)
         (add-16 tc-vec-vx-low vec-temp-low)
        } ]
      [joy-left  #t
         {
         (create-fractional-vec vec-temp-low 5)
         (sub-16 tc-vec-vx-low vec-temp-low)
        } ]
      [joy-fire  #t {} ])
]
    
    [state-dying
     {}
     ()]
    ))
   
   
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
   sta vec-temp-low
   sta $10
   bpl (+ (here) 4)
   lda @$ff
   (data $2c) 
   lda @0
   sta vec-temp-high
   sta $11
   
   ; now multiply by tc-vel
   ldy tc-vel
:loop
   beq done+
   (add-16 vec-temp-low $10)
:skip2
   dey
   jmp loop-
:done      
   (add-16 tc-vec-vx-low vec-temp-low)

   ;initialze Y component
   lda sine: x
   sta vec-temp-low
   sta $10
   bpl (+ (here) 4)
   lda @$ff
   (data $2c) 
   lda @0
   sta vec-temp-high
   sta $11
   ldy tc-vel
:loop
   beq done+
   (add-16 vec-temp-low $10)
:skip
   dey
   jmp loop-
   :done
   (add-16 tc-vec-vy-low vec-temp-low)
   rts



:change-state ; pass new state in x
     stx tc-state
     ;; lda frame-start-offsets+ x
     ;; sta tc-frame
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


;; (generate-state-machine
;;  ([state-standing
;;    { }
;;    ([joy-right #t state-walking-right ]
;;     [joy-left  #t state-walking-left]
;;     [joy-fire  #t state-crouching
;;                (activate-target)])]
               
;;   [state-walking-right
;;    {
;;     ; move sprite right
;;     lda $d000
;;     clc
;;     adc @2
;;     sta $d000
;;     bcc skip+
;;     lda @%00000001
;;     eor $d010
;;     sta $d010
;; :skip    
;;    }    
;;    ([joy-left  #t state-walking-left]
;;     [joy-right #f state-standing])]
  
;;   [state-walking-left
;;    {
;;     ; move sprite left
;;     lda $d000
;;     sec
;;     sbc @2
;;     sta $d000
;;     bcs skip+
;;     lda @%00000001
;;     eor $d010
;;     sta $d010
;; :skip    
;;     }
;;    ([joy-right #t state-walking-right]
;;     [joy-left  #f state-standing])]

;;   [state-crouching
;;    {}
;;    ([joy-down #t state-velocity]
;;     [joy-left #t state-angle-left
;;           { lda @0
;;             sta tc-angle-change-delay } ]
;;     [joy-right #t state-angle-right
;;           { lda @0
;;             sta tc-angle-change-delay }] 
;;     [joy-fire #f state-standing
;;                (deactivate-target)])]
  
;;   [state-angle-right
;;    {
;;      ldx tc-angle-change-delay
;;      bne skip+
;;      lda @angle-change-delay
;;      sta tc-angle-change-delay
;;      ; increase angle if we can
;;      ldy tc-angle-target
;;      cpy @70
;;      beq joy+
;;      iny
;;      sty tc-angle-target
;;      jsr update-target-
;;      jmp joy+
;; :skip
;;      dex
;;      stx tc-angle-change-delay
;; :joy     
;;    }
;;    ([joy-down #t state-velocity]
;;     [joy-left #t state-angle-left
;;           { lda @0
;;             sta tc-angle-change-delay }]
;;     [joy-fire #f state-standing
;;               (deactivate-target)]
;;     [joy-left #f state-crouching]
;;     )]

;;   [state-angle-left
;;    {
;;      ldx tc-angle-change-delay
;;      bne skip+
;;      lda @angle-change-delay
;;      sta tc-angle-change-delay
;;      ; decrease angle if we can
;;      ldy tc-angle-target
;;      cpy @38
;;      beq joy+
;;      dey
;;      sty tc-angle-target
;;      jsr update-target-
;;      jmp joy+
;; ;; :reset
;; ;;      lda @$48
;; ;;      sta tc-angle-target
;;      jmp joy+
;; :skip
;;     dex
;;      stx tc-angle-change-delay

     
;; :joy     
;;    }
;;    ([joy-down #t state-velocity]
;;     [joy-right #t state-angle-right
;;                { lda @0                     
;;                  sta tc-angle-change-delay }]
;;     [joy-fire #f state-standing
;;               (deactivate-target)]
;;     [joy-right #f state-crouching])]

;;   [state-velocity
;;    {

;;     ldx tc-vel-change-delay
;;     bne skip+
;;     lda @vel-change-delay
;;     sta tc-vel-change-delay
;;     ldx tc-vel
;;     cpx @5
;;     beq zero+
;;     inx
;;     (data $2c)
;; :zero    
;;     ldx @1
;; :done
;;     stx tc-vel
;;     clc
;;     txa
;; ;   adc @49
;;     sta $0404
;;     clc
;;     bcc joy+
;; :skip
;;    dex
;;    stx tc-vel-change-delay
;; :joy
;;    }
;;    ([joy-down #f state-jumping    
;;                  {
;;                     lda @vel-change-delay
;;                     sta tc-vel-change-delay
;;                     lda @0
;;                     sta tc-falling
;;                     (deactivate-target)
;;                     jsr prepare-jump:
;;                     }]
;;     [joy-down #f state-crouching])]
                     
;;   [state-jumping
;;    {
;;      ldx $d001
;;      cpx @$d1
;;      bcc cont+
;;      lda @$d0
;;      sta $d001
;;      ldx @state-standing
;;      jsr change-state:
;;      rts
;; :cont
;;      jsr update-jump:
;;      ;jumping, allow some sway if headed downwards.
;;      }
;;    (;[joy-fire #t state-standing]
;;     )]

;;   [state-skidding-left
;;    {}
;;    ()]

;;   [state-skidding-right
;;    {}
;;    ()]

;;   [state-falling-right
;;    {}
;;    ()]

;;   [state-falling-left
;;    {}
;;    ()]

;;   [state-dying
;;    {}
;;    ()]
  
;;    ))


     

   })
