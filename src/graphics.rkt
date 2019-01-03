#lang asi64

(provide (all-defined-out))

(require "global.rkt")
(define (graphics-code)
  {

     
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

:render
   ; map the sprite to the current position vector
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
   ;TODO- target angle if in crouch/angle state
   })


  
