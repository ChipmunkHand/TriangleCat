#lang asi64

(provide collision-detection-code)

(require (for-syntax racket/list racket/function  racket/syntax))
(require
 threading
 syntax/parse/define

 "global.rkt"
 "maths.rkt")


(define (collision-detection-code)
{
:perform-test
  (add-16 vec-originx-low tc-vec-vx-low)
  (add-16 vec-originy-low tc-vec-vy-low)
  ; test if there is a platform there.
  ; to do this we can divide by 8 (shift right 3)
  ; the vectors are stored with 9 biths of precision and a leading
  ; sign bit.  will need to drop the sign bit, shift in the remainig
  ; two bits leaving the highest bit in the carry ready to be rotated in

  ; for Y we need to rotate in the 8th bit and drop the sign
  (vec/8 vec-originy-low)
  tax 
  sta scratch-b  ; Y pos
  lda screen-rows-lo: x
  sta screen-lo
  lda screen-rows-hi: x
  sta screen-hi

  (vec/8 vec-originx-low)
  sta scratch-a  ; x pos
  tay
  lda £ screen-lo y          ; return value at screen pos
  rts


   ; try to find a collision from the three points
   (define (move-origin xoff yoff)
     ;pass in the offset co-ords from sprite location 0 0
     ;takes into consideration the borders for you.
     (let ([xoff (- $18 xoff)]
           [yoff (- $32 yoff)])
       {
         (copy-16 vec-originx-low tc-vec-x-low)
         (copy-16 vec-originy-low tc-vec-y-low)

         (create-vec vec-temp-low xoff)
         (sub-16 vec-originx-low vec-temp-low)

         (create-vec vec-temp-low yoff)
         (sub-16 vec-originy-low vec-temp-low)

         }))

   (define-syntax-parser gen-origin
     #:datum-literals (top top-right right bottom-right bottom bottom-left left top-left)
     [(_ top)          #'(move-origin 12 0)]
     [(_ top-right)    #'(move-origin 24 0)]
     [(_ right)        #'(move-origin 24 10)]
     [(_ bottom-right) #'(move-origin 24 21)]
     [(_ bottom)       #'(move-origin 12 21)]
     [(_ bottom-left)  #'(move-origin 0 21)]
     [(_ left)         #'(move-origin 0 12)]
     [(_ top-left)     #'(move-origin 0 0)])

   

   
:collision-detection

  ; 1.  Y calculations

  ; a.  determine direction of motion
   lda tc-vec-vy-high
   bmi y-neg+
   bne y-pos+
   lda tc-vec-vy-low
   bne y-pos+
   ;unlikely caes of Y being zero, skip   
   jmp x-calculations+
 :y-neg
   lda @%1
   bne done+
 :y-pos
   lda @%0000_0000
 :done
   sta tc-temp   ; 0 is positive (down) 1 is negative (up)
   bne up+
   jmp down+
 :up
   ;create origin point
   (gen-origin top)

   ;apply VY to origin
   (add-16 vec-originy-low tc-vec-vy-low)

   ;collision test
   jsr perform-test:
   cmp @$20
   beq next+
   jmp y-hit+
:next
   (gen-origin top-left)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp y-hit+
:next
   (gen-origin top-right)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp y-hit+
 :next
   jmp y-finish:

:down
   (gen-origin bottom)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp y-hit+
 
:next
   (gen-origin bottom-left)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp y-hit+
 
:next
   
   (gen-origin bottom-right)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp y-hit+
 
:next
   jmp y-finish:

   :y-hit

   lda tc-temp
   beq down+

;   (vec/neg tc-vec-vy-low vec-temp-low)
   jmp y-finish:
   :down
   ;for now we deaden vy
   (create-vec tc-vec-vy-low 0)
 

 :y-finish
  (add-16 tc-vec-y-low tc-vec-vy-low)

   
:x-calculations

 ; a.  determine direction of motion
   lda tc-vec-vx-high
   bmi x-neg+
   bne x-pos+
   lda tc-vec-vx-low
   bne x-pos+
   ;unlikely caes of Y being zero, skip   
   jmp collision-end+
 :x-neg
   lda @%1
   bne done+
 :x-pos
   lda @%0000_0000
 :done
   sta tc-temp   ; 0 is positive (right) 1 is negative (left)
   bne left+
   jmp right+
 :left
   ;create origin point
   (gen-origin left)

   ;apply VY to origin
   (add-16 vec-originx-low tc-vec-vx-low)

   ;collision test
   jsr perform-test:
   cmp @$20
   beq next+
   jmp x-hit+
:next
   (gen-origin top-left)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp x-hit+
:next
   (gen-origin bottom-left)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp x-hit+
:next
   jmp x-finish:

:right
   (gen-origin right)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp x-hit+
:next
   (gen-origin bottom-right)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp x-hit+
:next   
   (gen-origin top-right)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:
   cmp @$20
   beq next+
   jmp x-hit+
:next
   jmp x-finish:

:x-hit
   lda tc-temp
   beq right+
   (vec/neg tc-vec-vx-low vec-temp-low)
   jmp x-finish:
   :right
   ;for now we deaden vx
   (create-vec tc-vec-vx-low 0)

 

 :x-finish
  (add-16 tc-vec-x-low tc-vec-vx-low)

 
 :collision-end

  
   rts


})
