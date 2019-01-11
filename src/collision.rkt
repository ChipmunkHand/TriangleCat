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
   ;TODO:
   ; 2. clamping behaviour for X and Y.
   ;      will align TC's position with the nearest
   ;      tile boundary.  
   ;
   ; 3. metatadata system for tiles to determine collision effects
   
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

:apply-friction
   ;supply source vector in vec-temp
   lda tc-vec-vx-high
   bmi neg+
   lda tc-vec-vx-low
   bne pos+
   ;zero, exit early
   rts
 :neg
  (add-16 tc-vec-vx-low vec-temp-low)
  lda tc-vec-vx-high
  bmi done+
  (create-vec tc-vec-vx-low 0)
  rts
  :pos
  (sub-16 tc-vec-vx-low vec-temp-low)
  lda tc-vec-vx-high
  bpl done+
  (create-vec tc-vec-vx-low 0)
  rts
 :done 
  rts
:clamp-x

   ;temp
   (create-vec tc-vec-vx-low 0)
   rts

:clamp-y

   ;temp
   (create-vec tc-vec-vy-low 0)
   rts
   
   lda tc-vec-vy-high
   bmi neg+
   lda tc-vec-vy-low
   bne pos+
   rts
 :neg
   ; TC is going upwards, round 8 at current location
   ; and set the Y pos to this value + border (easy). Kill velocity.
   ;for now we deaden vy   
   (create-vec tc-vec-vy-low 0)
   
   rts
  :pos
   ;TX going down, find bottom of sprite + 8 then round
   ;subtract border and sprite height, set.
   ;dont if already 8 aligned
   (gen-origin bottom)
   ;; (create-vec vec-temp-low 8)
   ;; (add-16 vec-originy-low vec-temp-low)
   lda vec-originy-high
   ror
   lda vec-originy-low
   ror
   and @%1110_0000
   bne cont+
   rts

   (add-16 vec-originy-low tc-vec-vy-low)
   
   :cont
   
   ; round to nearest 8
   lda @0
   sta vec-originy-low
   lda vec-originy-high
   and @%1111_1110
   sta vec-originy-high

   ;add back the sprite height
   (create-vec vec-temp-low 37)
   (add-16 vec-originy-low vec-temp-low)
   ;replace TC pos
   (copy-16 tc-vec-y-low vec-originy-low)
   ;dead vy
   (create-vec tc-vec-vy-low 0)
  rts

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
   lda @2
   bne done+
 :y-pos
   lda @%0000_0000
 :done
   sta tc-temp   ; 0 is positive (down, hits top) 2 is negative (up, hits bottom)
   bne up+
   jmp down+
 :up
   ;create origin point
   (gen-origin top)

   ;apply VY to origin
   (add-16 vec-originy-low tc-vec-vy-low)

   ;collision test
   jsr perform-test:   
   beq next+
   jmp y-hit+
:next
   (gen-origin top-left)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:   
   beq next+
   jmp y-hit+
:next
   (gen-origin top-right)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:   
   beq next+
   jmp y-hit+
 :next
   jmp y-finish:

:down
   (gen-origin bottom)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:   
   beq next+
   jmp y-hit+
 
:next
   (gen-origin bottom-left)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:
   beq next+
   jmp y-hit+
 
:next
   
   (gen-origin bottom-right)
   (add-16 vec-originy-low tc-vec-vy-low)
   jsr perform-test:   
   beq next+
   jmp y-hit+
 
:next
   jmp y-finish:

   :y-hit
   sta tc-tile-hit
   ldx tc-temp ; direction
   lda tc-tile-hit
   jsr metadata-lookup:
   sta tc-temp

   ; process metadata flags.
   and @x-neg
   beq next+
;   break
   (vec/neg tc-vec-vx-low vec-temp-low)
:next   
   lda tc-temp
   and @y-neg
   beq next+
   (vec/neg tc-vec-vy-low vec-temp-low)
:next
   lda tc-temp
   and @clamp
   beq next+
   jsr clamp-y:
:next
   lda tc-temp
   and @%0000_0111   
   beq next+
   pha
;   lda tc-tile-hit
   ;special vectors.
   ldx @0
   jsr vector-lookup:
   ;first vector, returns high in A, low in Y
   sta vec-temp-high
   sty vec-temp-low
   ;if there is a second vector, it is a max/min
   ;that should be adhered to  (TODO)
   ;should probably have bit 9 indicate X or Y (todo)
   pla
   ldx @2
   jsr vector-lookup:
   sta tc-vec-tempx-low  ; this memory is not being used
   sty tc-vec-tempx-high
   bne special+
   tya
   beq friction+
   :special
   ;TODO: min max thing
   ;; sta (+ vec-temp-low 1)
   ;; sty vec-temp-low
   (add-16 tc-vec-vx-low vec-temp-low)
   jmp next+
   :friction
   jsr apply-friction:

:next  

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
   lda @3
   bne done+
 :x-pos
   lda @1
 :done
   sta tc-temp   ; 1 is positive (right) 3 is negative (left)
   cmp @1
   bne left+
   jmp right+
 :left
   ;create origin point
   (gen-origin left)

   ;apply VY to origin
   (add-16 vec-originx-low tc-vec-vx-low)

   ;collision test
   jsr perform-test:   
   beq next+
   jmp x-hit+
:next
   (gen-origin top-left)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:   
   beq next+
   jmp x-hit+
:next
   (gen-origin bottom-left)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:   
   beq next+
   jmp x-hit+
:next
   jmp x-finish:

:right
   (gen-origin right)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:   
   beq next+
   jmp x-hit+
:next
   (gen-origin bottom-right)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:   
   beq next+
   jmp x-hit+
:next   
   (gen-origin top-right)
   (add-16 vec-originx-low tc-vec-vx-low)
   jsr perform-test:   
   beq next+
   jmp x-hit+
:next
   jmp x-finish:

:x-hit
   sta tc-tile-hit
   ldx tc-temp
   jsr metadata-lookup:
   sta tc-temp
   ; process metadata flags.
   and @x-neg
   beq next+
   (vec/neg tc-vec-vx-low vec-temp-low)
:next   
   lda tc-temp
   and @y-neg
   beq next+
   (vec/neg tc-vec-vy-low vec-temp-low)
:next
   lda tc-temp
   and @clamp
   beq next+
   jsr clamp-x:
:next
   lda tc-temp
   and @%0000_0111
   beq next+
   pha
   ;special vectors.
   ldx @0
   jsr vector-lookup:
   ;first vector, returns high in A, low in Y
   sta vec-temp-high
   sty vec-temp-low
   ;if there is a second vector, it is a max/min
   ;that should be adhered to  (TODO)
   pla
   ldx @2
   jsr vector-lookup:
   sta tc-vec-tempx-low  ; this memory is not being used
   sty tc-vec-tempx-high
   bne special+
   tya
   beq friction+
   :special
   ;TODO: min max thing
   ;; sta (+ vec-temp-low 1)
   ;; sty vec-temp-low
   (add-16 tc-vec-vy-low vec-temp-low)

   jmp next+
   :friction
   jsr apply-friction:

:next  
 

 :x-finish
  (add-16 tc-vec-x-low tc-vec-vx-low)

 
 :collision-end

  
   rts


})
