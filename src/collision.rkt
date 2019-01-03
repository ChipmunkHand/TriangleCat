#lang asi64

(provide collision-detection-code)

(require
 "global.rkt"
 "maths.rkt")

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


   
:collision-detection
   ; depending on the direction TC is going we will need to check
   ; up to 2 directions with 4 points on each side, 7 total (since one on a corner is shared)

   (copy-16 tc-vec-tempx-low tc-vec-x-low)
   (copy-16 tc-vec-tempy-low tc-vec-y-low)

   ; for the initial test we will only check if TC is going down and right.  Both are positive
   ; numbers in the vector.
;;    lda tc-vec-vx-high
;;    bpl next+
;;    rts
;; :next   
;;    lda tc-vec-vy-high 
;;    bpl next+
;;    rts
;; :next  
   
;   break
   
   ; first we take the "current position" and add the x and y vectors to it
   ; for this test that is the bottom centre of the sprite, so we need to add
   ; 24 to the Y and 12 to the X
   ; Y border is $32 pixels and X is $18 pixels which must always be subtracted
   ; that leaves us with subtracting ($32 - 21) 29 for Y and
   ; ($18 - 12) 12 for X

   (create-vec vec-temp-low 12)
   (sub-16 tc-vec-tempx-low vec-temp-low)

   (create-vec vec-temp-low 29)
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
   sta scratch-b  ; Y pos
   lda screen-rows-lo: x
   sta screen-lo
   lda screen-rows-hi: x
   sta screen-hi

   (vec/8 tc-vec-tempx-low)
   sta scratch-a  ; x pos
   tay
   lda £ screen-lo y          ; return value at 
   sta scratch-a
   cmp @$20
   beq end+

   jsr collision-direction:
;   sta scratch-b

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
   (create-vec tc-vec-vy-low 0)

;   break
   ; todo: this can be better
   ;; lda @0
   ;; sta tc-vec-y-low
   ;; lda tc-vec-y-high
   ;; and @%1111_1110
   ;; sta tc-vec-y-high

   
   (add-16 tc-vec-x-low tc-vec-vx-low)

   
   rts
   


;   dec $d021
;   inc $d021
   
   
   :end

   (add-16 tc-vec-x-low tc-vec-vx-low)
   (add-16 tc-vec-y-low tc-vec-vy-low)
   
   rts



})
