#lang asi64

(provide (all-defined-out))

(require
 "expression.rkt"
 "global.rkt"
 "maths.rkt"
 "spritemate.rkt")

(define (graphics-code)
  {

:animate-new
     ; looking at the velocties work out what animation should be playing

     ; if a crouching state, then crouching
     ; else
     ; +/- Y airborne
     ; else +/- X over top walking speed, skidding
     ; else +/- X walking

     ; if the animation is not the one currently playing, then change it
     ; otherwise continue playing animation.

     ; each animation has a separate frame delay which will need resetting.

    (~if
      (~or 
        {lda tc-state
         cmp @state-crouching}
        {lda tc-state
         cmp @state-velocity})
      {
        lda @anim-crouching
 ;       lda tc-anim-type
        sta tc-temp 
      }
      {;else
       (~if
        {lda tc-vec-vy-high
         bne done+
         lda tc-vec-vy-low
         :done }
        { ; vy = 0
         nop
         (~if
          {lda tc-vec-vx-high
           bne done+
           lda tc-vec-vx-low
           :done}
          { ;vx = 0
           nop
          }
          { ; vx <> 0           
           lda tc-vec-vx-high
           bmi neg+
           lda @anim-walking-right           
           jmp done+
           :neg
           lda @anim-walking-left
           jmp done+
           :done
           sta tc-temp
          })
         
        }
        { ; vy <> 0
         lda @anim-airborne
         sta tc-temp
        })
      })

    lda tc-anim-type
    cmp tc-temp
    beq update+
    ldy tc-temp
    sty tc-anim-type
    lda frame-anim-speeds+ y
    sta tc-anim-delay
    lda frame-start-offsets+ y
    sta tc-frame
    sta $07f8
    rts
    
:update
     ldx tc-anim-delay
     beq change+
     dex
     stx tc-anim-delay
     rts
:change
     ;reset anim speed
     ldy tc-anim-type
     ldx frame-anim-speeds+ y
     stx tc-anim-delay
     ;check for final frame
     ldx frame-end-offsets+ y
     cpx tc-frame
     beq reset+
     ;next frame
     inc tc-frame
     jmp end+
:reset
     ;first frame
     ldx frame-start-offsets+ y
     stx tc-frame
:end
     ; copy to first sprite
     lda tc-frame
     sta $07f8
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
     (data $30 $1 $1 $5 $1 $1 $1 $1 $1)


   })


  
