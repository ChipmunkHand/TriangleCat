#lang asi64

(provide generate-charset)
(require
 threading
 syntax/parse/define)

;metadata format
; 8 - negate x
; 7 - negate y
; 6 - clamp (direction context?)
; 5 - (maybe need clamp x and y)
; 4 - kill
; 3 - has friction
; 2 - has special
; 1 -

; each tile will need 4 of these, one for each direction.

; friction and special vectors are stored in a table
; that has 4 bytes for each entry - the vector itself
; and an optional max vector for the special vectors.

; therefore the tile lookup is tile number >> 3 + direction
; and the vector lookup is also tile nubmer >> 3 (+2 for max vector)

;finally there is a colour lookup table of tile -> colour.

(struct tile-metadata (flags vec1 vec2 colour) #:transparent)

(define-syntax-parser adjust-flag
  #:datum-literals (x-neg y-neg clamp kill vector)
  [(_ x-neg)   #'#x80]
  [(_ y-neg)   #'#x40]
  [(_ clamp)   #'#x20]
  ;[(_ y-neg)  #'#x10]
  [(_ kill)    #'#x08]
  [(_ vector)#'#x04]
  ;[(_ special) #'#x02]
;  [(_ y-neg)  #'#x1]
  )
  
(define-syntax-parser create-tile-metadata
  [(_ name colour (flags ...)
      (~optional vec1 #:defaults ([vec1 #'0]))
      (~optional vec2 #:defaults ([vec2 #'0])))
   #'(let ([meta-flags 0])
       (begin
         (set! meta-flags (bitwise-ior meta-flags (adjust-flag flags)))
         ...)
       (tile-metadata meta-flags vec1 vec2 colour))])

;(define yy (tile-metadata 0 0 0))      
(define xx (create-tile-metadata "test" 3 [x-neg y-neg]))

(define (tile-metadata-code)
  {
:metadata-lookup
   ;pass tile number in X

   rts

:vector-lookup
   ;pass tile number in X
   
   rts


   })

(define (generate-charset)
  {

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

    %11111111
    %11111111
    %11111111
    %11111111
    %11111111    
    %11111111    
    %11111111
    %11111111

    )
   })
