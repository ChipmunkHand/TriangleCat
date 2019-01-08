#lang asi64

(provide (all-defined-out))

(require
 "global.rkt"
 "maths.rkt"
 "spritemate.rkt")
(define (graphics-code)
  {

     
:animate
     ; the job of this is to simply cycle the current TC animation
     ; and nothing else.
     lda tc-state
     cmp tc-prev-state
     beq cont+
     ldy tc-state
     cpy @state-airborne
     bne next+
     ldy @5
     jmp reset+
  :next     
     ldy @0
     jmp reset+
:cont     
     ldx tc-anim-delay
     beq change+
     dex
     stx tc-anim-delay
     rts     
 :change
     ldy tc-state
     cpy @state-airborne
     bne next+
     ldy @5
     beq done+
  :next     
     ldy @0
  :done
     ldx frame-anim-speeds+ y
     stx tc-anim-delay
     ldx frame-end-offsets+ y
     cpx tc-frame
     beq reset+
;     break
     inc tc-frame
     jmp end+
 :reset     
     ldx frame-start-offsets+ y
     stx tc-frame
  :end
      ; copy to first sprite
       ;;todo: this is totally broken now, just display the standing with no anims
     lda tc-frame
;     sta $07f8
     rts


:render
   ; display the sprite at the position vector
   (copy-16 vec-temp-low tc-vec-x-low)

   ;rotate out the sign bit, in the 9th bit then
   ;the 8th bit leving the 1st in the carry
   
   asl vec-temp-high  ; sign out
   asl vec-temp-low   ; 9th in 
   rol vec-temp-high  ; 1st bit out
   lda $d010
   bcc not-set+
   ; adjust bit in $d010 depending on carry
   ora @%0000_0001
   bcs next+
 :not-set
   and @%1111_1110
 :next
   sta $d010
   lda vec-temp-high
   sta $d000

   ; Y is a lot simpler !
   (copy-16 vec-temp-low tc-vec-y-low)
   asl vec-temp-high   ; sign bit out
   asl vec-temp-low    ; 9th bit in
   rol vec-temp-high   ; 1st bit out
   ; here we dont care about bit 9 since the
   ; Y limit is 240
   lda vec-temp-high
   sta $d001
   rts

   sprites = (list "standing" "walking-right" "walking-left"
                     "crouching"  "velocity" "airborne" "skidding-right"
                     "skidding-left" "dying")
;/= $100
:frame-start-offsets
      (for ([s sprites]) 
        (write-value (get-sprite-start s)))

:frame-end-offsets
      (for ([s sprites]) 
        (write-value (get-sprite-end s)))

:frame-anim-speeds      
     (data $30 $4 $4 $4 $4 $1 $4 $1 $4)


   })


  
