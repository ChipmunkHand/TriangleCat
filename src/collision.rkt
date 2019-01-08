#lang asi64

(provide collision-detection-code)

(require (for-syntax racket/list racket/function  racket/syntax))
(require
 threading
 syntax/parse/define

 "global.rkt"
 "maths.rkt")


(begin-for-syntax
  (define labels (hash))
  (define (gen-label name)
    (if (hash-has-key? labels name)
        (let ([val (hash-ref labels name)])
          (hash-set labels name (+ val 1))
          (format "~a_~a" name (+ val 1)))
        (begin
          (hash-set labels name 0)
          (format "~a_~a" name 0)))))

; load X with the value before calling this
(define-syntax-parser ~switch-x
  [(_ ([case-number code] ...) default)
   (let ([done-label (gen-label "lut_done")])
     (with-syntax*
       ([done-source (string->symbol (format ":~a" done-label))]
        [done-target (string->symbol (format "~a+" done-label))]
        [lut-max (argmax identity (syntax->datum #'(case-number ...)))]
        [exists? (λ (n) (member n (syntax->datum #'(case-number ...))))])
       #'{
          lda lut-lo: x         ; use lookup table and setup
              sta jump-vector-lo+   ; 16 bit address pointer
              lda lut-hi: x
              sta jump-vector-hi+
              jmp £ jump-vector-lo+ ; jump to target state

              {
               (set-jump-source-current (format "lut~a" case-number)) 
               code
               jmp done-target
               } ...

               :lut-default
               default
               jmp done-target
                 
               (define jump-labels
                 (for/list ([index (in-range (+ lut-max 1))])
                   (if (exists? index) 
                       (find-closest-label (format "lut~a" index) (here) '-)
                       (find-closest-label ":lut-default" (here) '-))))

               :lut-lo
               (write-values (map lo-byte jump-labels))
               
               :lut-hi
               (write-values (map hi-byte jump-labels))

               :jump-vector-lo (data $FF)
               :jump-vector-hi (data $FF)
               
               done-source
               }))])

(define-syntax-parser ~and
  ;A will be 0 if all tests pass
  ;Success is always defined as 0
  [(_ test ...+ final)
   (let ([done-label (gen-label "and_done")])
   (with-syntax
     ([start-label  (gen-label "and_start")]
      [done-source (string->symbol (format ":~a" done-label))]
      [done-target (string->symbol (format "~a+" done-label))])
     #'{
     (set-jump-source-current start-label)        
     {test
      bne done-target
     } ...
     final
     done-source
     }))])

(define-syntax-parser ~or
  ;A will be 0 if any tests pass
  ;Success is always defined as 0
  [(_ test ...+ final)
   (let ([done-label (gen-label "or_done")])
   (with-syntax
     ([start-label  (gen-label "or_start")]
      [done-source (string->symbol (format ":~a" done-label))]
      [done-target (string->symbol (format "~a+" done-label))])
     #'{
     (set-jump-source-current start-label )        
     {test
      beq done-target      
      } ...
     final     
     done-source
     }))])

(define-syntax-parser ~if
  [(_ test true-code false-code)
   (let ([false-label (gen-label "if-false")]
         [end-label (gen-label "if-end")])         
   (with-syntax
     ([start-label  (gen-label "if_start")]
      [false-source (string->symbol (format ":~a" false-label))]
      [false-target (string->symbol (format "~a+" false-label))]
      [end-source (string->symbol (format ":~a" end-label))]
      [end-target (string->symbol (format "~a+" end-label))])
     #'{
        (set-jump-source-current  start-label)
        test
        bne false-target
        true-code
        jmp end-target
        false-source
        false-code
        end-source
      }))])



(define (collision-detection-code)
{
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
   break
   lda tc-vec-vx-high
   bmi neg+
   lda @left
   rts
:neg
   lda @right
   rts
   
:next
;if both caused a grid change then the smallest absolute vector wins
break
   (copy-16 tc-vec-tempx-low tc-vec-vx-low)
   lda tc-vec-tempx-high
   bmi neg+ 
   jmp skip+
:neg
   (vec/neg tc-vec-tempx-low vec-temp-low)
:skip
   (copy-16 tc-vec-tempy-low tc-vec-vy-low)
   lda tc-vec-tempy-high
   bmi neg+
   jmp skip+
:neg
   (vec/neg tc-vec-tempy-low vec-temp-low)
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

   
:perform-test
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
  sta scratch-b  ; Y pos
  lda screen-rows-lo: x
  sta screen-lo
  lda screen-rows-hi: x
  sta screen-hi

  (vec/8 tc-vec-tempx-low)
  sta scratch-a  ; x pos
  tay
  lda £ screen-lo y          ; return value at 

  rts
:collision-detection
  ;NEW ROUTINE

  ;perform the X and Y steps indiviually.
  ;  1.  Perform the Y calculations
  ;      a. determine direction of motion
  ;      b. create first of three origin points (downwards would be bottom left, bottom. bottom right)
  ;      c. apply vy to the origin point 
  ;      d. check for collision
  ;      e. if there is no collision, proceed to next origin (b)
  ;         if there is a collision, the direction is the opposite (up or down). 
  ;         determine tile metadata and apply clamping / velocity changes / whatever
  ;      f. after one collision or all three fails,  apply vy to y

  ;  2. Perform the X calculations, the same as above.

  ;///////////////////////////

; depending on the direction TC is going we will need to check
   ; up to 2 directions with 4 points on each side, 7 total (since one on a corner is shared)


   ; to work out which directions to check we will hard code each case since speed is of the essence
   ; we need a case for each possible direction, 8 in total. we can load these into 4 bits and
   ; jump via a lookup table for fastest speed.
break
   ;        X   |   Y  |  
   ; zero | 00  |  00  |
   ; pos  | 01  |  01  |
   ; neg  | 10  |  10  |

   ; combos (x << 2 || y)
   ; 0000   X = 0 && Y = 0   None
   ; 0100   X > 0 && Y = 0   Right
   ; 1000   X < 0 && Y = 0   Left
   ; 0001   X = 0 && Y > 0   Down
   ; 0010   X = 0 && Y < 0   Up
   ; 0101   X > 0 && Y > 0   Down Right
   ; 0110   X > 0 && Y < 0   Up Right
   ; 1001   X < 0 && Y > 0   Down Left
   ; 1010   X < 0 && Y < 0   Up Left

   lda tc-vec-vx-high
   bmi x-neg+
   bne x-pos+
   lda tc-vec-vx-low
   bne x-pos+
   lda @0
   beq done+
 :x-neg
   lda @%0000_1000
   bne done+
 :x-pos
   lda @%0000_0100
 :done
   sta tc-temp

   lda tc-vec-vy-high
   bmi y-neg+
   bne y-pos+
   lda tc-vec-vy-low
   bne y-pos+
   lda @0
   beq done+
 :y-neg
   lda @%0000_0010
   bne done+
 :y-pos
   lda @%0000_0001
 :done
   ora tc-temp
   sta tc-temp

   (define (test-point xoff yoff)
     ;pass in the offset co-ords from sprite location 0 0
     ;takes into consideration the borders for you.
     (let ([xoff (- $18 xoff)]
           [yoff (- $32 yoff)])
       {
         (copy-16 tc-vec-tempx-low tc-vec-x-low)
         (copy-16 tc-vec-tempy-low tc-vec-y-low)

         (create-vec vec-temp-low xoff)
         (sub-16 tc-vec-tempx-low vec-temp-low)
         (create-vec vec-temp-low yoff)
         (sub-16 tc-vec-tempy-low vec-temp-low)

         jsr perform-test:
         }))

   (define-syntax-parser gen-case
     #:datum-literals (top top-right right bottom-right bottom bottom-left left top-left)
     [(_ top)          #'(test-point 12 0)]
     [(_ top-right)    #'(test-point 24 0)]
     [(_ right)        #'(test-point 24 10)]
     [(_ bottom-right) #'(test-point 24 21)]
     [(_ bottom)       #'(test-point 12 21)]
     [(_ bottom-left)  #'(test-point 0 21)]
     [(_ left)         #'(test-point 0 12)]
     [(_ top-left)     #'(test-point 0 0)])

   
   (define-syntax-parser gen-cases
     [(_ cases ...)
      #'{
         {
          (gen-case cases)
          cmp @$20
          beq cont+
          jmp hit+
        :cont
          } ...
         }])
   
   tax
   (~switch-x 
   ([%0000 {nop}]
     [%0001 
      {
       :down
       (gen-cases
        bottom-right
        bottom
        bottom-right)
       }]
     [%0010 
      {
       :up
       (gen-cases
        top-left
        top
        top-right)
       }]
     [%0100 
      {
       :right
       (gen-cases
        top-right
        right
        bottom-right)
       }]
     [%1000 
      {
       :left
       (gen-cases
        bottom-left
        left
        top-left)
       }]

     [%0101 
      {
       :down-right
       (gen-cases
        bottom-left
        bottom
        bottom-right
        right
        top-right)

       }]

     [%0110 
      {
       :up-right
       (gen-cases
        top-left
        top
        top-right
        right
        bottom-right)
       }]

     [%1001
      {
       :down-left
       (gen-cases
        top-left
        left
        bottom-left
        bottom
        bottom-right)
       }]

     [%1010
      {
       :up-left
       (gen-cases
        top-right
        top
        top-left
        left
        bottom-left)
       }])
    {nop})
 

   jmp end+
   
   ;   break
   
   ; first we take the "current position" and add the x and y vectors to it
   ; for this test that is the bottom centre of the sprite, so we need to add
   ; 24 to the Y and 12 to the X
   ; Y border is $32 pixels and X is $18 pixels which must always be subtracted
   ; that leaves us with subtracting ($32 - 21) 29 for Y and
   ; ($18 - 12) 12 for X

:JOHN
   ;; (copy-16 tc-vec-tempx-low tc-vec-x-low)
   ;; (copy-16 tc-vec-tempy-low tc-vec-y-low)

   ;; (create-vec vec-temp-low 12)
   ;; (sub-16 tc-vec-tempx-low vec-temp-low)

   ;; (create-vec vec-temp-low 29)
   ;; (sub-16 tc-vec-tempy-low vec-temp-low)

   ;; (add-16 tc-vec-tempx-low tc-vec-vx-low)
   ;; (add-16 tc-vec-tempy-low tc-vec-vy-low)
   
   ;; ; test if there is a platform there.
   ;; ; to do this we can divide by 8 (shift right 3)
   ;; ; the vectors are stored with 9 biths of precision and a leading
   ;; ; sign bit.  will need to drop the sign bit, shift in the remainig
   ;; ; two bits leaving the highest bit in the carry ready to be rotated in
   
   ;; ; for Y we need to rotate in the 8th bit and drop the sign
   ;; (vec/8 tc-vec-tempy-low)
   ;; tax 
   ;; sta scratch-b  ; Y pos
   ;; lda screen-rows-lo: x
   ;; sta screen-lo
   ;; lda screen-rows-hi: x
   ;; sta screen-hi

   ;; (vec/8 tc-vec-tempx-low)
   ;; sta scratch-a  ; x pos
   ;; tay
   ;; lda £ screen-lo y          ; return value at 
;; sta scratch-a
;; (test-point 12 21)
;;    cmp @$20
;;    beq end+

:hit
   jsr collision-direction:
   sta scratch-b

   (~if
    (~or
     {cmp @0}
     {lda scratch-b
      cmp @2 })
    {
     
;     (vec/neg tc-vec-vy-low vec-temp-low)
     (create-vec tc-vec-vy-low 0)
  ;   (add-16 tc-vec-x-low tc-vec-vx-low)
    }
    {
     break
     (create-vec tc-vec-vx-low 0)
 ;    (add-16 tc-vec-y-low tc-vec-vy-low)
    })
    
   
   ;lookup tile metadata
   ;; tax
   ;; lda tile-meta: x
   ;; sta scratch-c
   ;; lda @%0000_1000
   ;; bit scratch-c
   ;; beq skip+

   ; this flag is "invert y vector"
   ;(vec/neg tc-vec-vy-low vec-temp-low)

   ;clamp to low / 8
;   rts

   
   :end

   
   (add-16 tc-vec-x-low tc-vec-vx-low)
   (add-16 tc-vec-y-low tc-vec-vy-low)
   
   rts



})
