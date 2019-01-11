#lang asi64

(provide (all-defined-out))
(require
 "global.rkt"
 threading
 syntax/parse/define)

;collision metadata format
; 8 - negate x
; 7 - negate y
; 6 - clamp
; 5 - kill
; rest - 4 bit index of a special vector

; each tile will need 4 of these, one for each direction.

; friction and special vectors are stored in a table
; that has 4 bytes for each entry - the vector itself
; and an optional max vector for the special vectors.

; therefore the tile lookup is meta number >> 2 + direction
; and the vector lookup is also meta nubmer >> 2 (+2 for max vector)

(struct collision-metadata (flags vec1 vec2) #:transparent)

(define-syntax-parser adjust-flag
  #:datum-literals (x-neg y-neg clamp kill)
  [(_ x-neg)   #'#x80]
  [(_ y-neg)   #'#x40]
  [(_ clamp)   #'#x20]  
  [(_ kill)    #'#x10]
  ; the last 4 bits if set point
  ; at the collision vector table
  ; gives us only 16 custom vectors but it should be enough.
  )

(define-syntax-parser create-collision-metadata
  [(_ [(flags ...)
       (~optional vec1 #:defaults ([vec1 #'0]))
       (~optional vec2 #:defaults ([vec2 #'0]))])
   #'(let ([meta-flags 0])
       (begin
         (set! meta-flags (bitwise-ior meta-flags (adjust-flag flags)))
         ...)
       (collision-metadata meta-flags vec1 vec2))])

(define-syntax-parser create-collision-metadata-set
  [(_ [top-meta right-meta bottom-meta left-meta] ... )
   #'(let* ([data (list (list (create-collision-metadata top-meta)
                        (create-collision-metadata right-meta)
                        (create-collision-metadata bottom-meta)
                        (create-collision-metadata left-meta)) ...
                        )])
       data)])

(define meta-list
  (let* ([gen-fraction
          (λ (value)
            (bitwise-and value %00_111_111))]
         [gen-whole
          (λ (value)
            (let* ([masked (bitwise-and value %1_1111_1111)])
              (arithmetic-shift masked 6)))]
         [gen-whole-neg
          (λ (value)
            (bitwise-ior %1_000_0000_0000_0000 (gen-whole value)))]
         [normal-friction (gen-fraction 7)])

  (create-collision-metadata-set
   [;"icy-floor"       tile 1
    [(clamp) (gen-fraction 1)]
    [(clamp)]
    [()]
    [(clamp)]]
   [;"normal-floor"     2 ...
    [(clamp) normal-friction]
    [(clamp)]
    [()]
    [(clamp)]]
   [;"normal-floor"    3
    [(clamp) normal-friction]
    [(clamp)]
    [(clamp)]
    [(clamp)]]
   [;"normal-floor"   4
    [(clamp) normal-friction]
    [(clamp)]
    [(clamp)]
    [(clamp)]]
   [;"normal-floor"   5
    [(clamp) normal-friction]
    [(clamp)]
    [(clamp)]
    [(clamp)]]
   [;"normal-floor"  6
    [(clamp) normal-friction]
    [(clamp)]
    [(clamp)]
    [(clamp)]]
   [;"normal-floor"  7
    [(clamp) normal-friction]
    [(clamp)]
    [(clamp)]
    [(clamp)]]
   [;"normal-floor"  8
    [(clamp) normal-friction]
    [(clamp)]
    [(clamp)]
    [(clamp)]]
   [;"normal-floor"  9
    [(clamp) normal-friction]
    [(clamp)]
    [(clamp)]
    [(clamp)]]

   [;"bounce-floor"  10
    [(y-neg)]
    [(clamp)]
    [(y-neg)]
    [(clamp)]]
   [;"bounce-floor"  11
    [(y-neg)]
    [(clamp)]
    [(y-neg)]
    [(clamp)]]
   [;"bounce-floor"  12
    [(y-neg)]
    [(clamp)]
    [(y-neg)]
    [(clamp)]]
   [;"bounce-floor"  13
    [(y-neg)]
    [(clamp)]
    [(y-neg)]
    [(clamp)]]
   [;"bounce-floor"  14
    [(y-neg)]
    [(clamp)]
    [(y-neg)]
    [(clamp)]]
   [;"bounce-floor"  15
    [(y-neg)]
    [(clamp)]
    [(y-neg)]
    [(clamp)]]
   [;"spike-up"     16
    [(kill clamp)]
    [(kill clamp)]
    [(kill clamp)]
    [(kill clamp)]]
   [;"spike-down"     17
    [(kill)]
    [(kill)]
    [(kill)]
    [(kill)]]
   [;"conveyor-right"   18
    [(clamp) (gen-fraction 15)(gen-fraction 120)] ;todo: special vectors
    [(x-neg)]
    [(clamp)]
    [(x-neg)]]
   [;"conveyor-left"    19
    [(clamp)] ;todo: special vectors
    [(x-neg)]
    [(clamp)]
    [(x-neg)]]   
   [;"bounce-wall"   
    [(clamp)]
    [(x-neg)]
    [(clamp)]
    [(x-neg)]]
   
   )))



(define (generate-collision-metadata-code)
  {
:metadata-lookup
   ;pass tile number in A and the direction you want in X
   ;returns in A
   stx tc-temp
   asl
   asl
   clc
   adc tc-temp
   tax
   lda collision-flags: x
   rts

:vector-lookup
   ;pass vector number in A. pass 2 in X to retrieve the second vector
   ;returns high byte in A, low in Y
   stx tc-temp
   asl
   asl
   clc
   adc tc-temp
   tax
   lda collision-vectors: x ; lo
   tay
   inx
   lda collision-vectors: x ; hi
   rts
  })

 (define (generate-collision-meta)
  (let* ([vector-hash (make-hash)]
         [unique-vecs
          (~>>
           meta-list
           (flatten)
           (filter collision-metadata?)
           (map (λ (x) (cons (collision-metadata-vec1 x) (collision-metadata-vec2 x))))
           (filter (λ (x) (not (equal? x (cons 0 0)))))
           (remove-duplicates))]

         [fix-flags
          (match-lambda [(collision-metadata flags vec1 vec2)
            ;write the index into the bottom 3 bits
            (if (not (equal? vec1 0))
                (bitwise-ior flags
                             (hash-ref vector-hash (cons vec1 vec2)))
                flags)])])
    (begin
      ; index the unique vectors
      (for ([v unique-vecs]
            [i (in-naturals 1)])
        (when (not (hash-has-key? vector-hash v))
          (begin
            (printf "pair ~a has key ~a\n" v i)
            (hash-set! vector-hash v i))))
      {              
       :collision-flags
       (data 0 0 0 0) ; special case for tile 0
       (let ([meta meta-list])
         (for ([m meta]
               [i (in-naturals 1)])
           (match m
             [(list top right bottom left)
              (begin
                (data (fix-flags top) (fix-flags right)
                      (fix-flags bottom) (fix-flags left)))])))

       /=$100
       :collision-vectors
       (data 0 0 0 0) ;special case since 0 doesnt exist (0 signfies no vector in the flags)
       (let ([meta (hash->list vector-hash)])
         (for ([m (sort meta (λ (x y) (< (cdr x) (cdr y))))])
           (match m
             [(cons (cons vec1 vec2) _ )
                (data (bitwise-and vec1 #xFF) (bitwise-and (arithmetic-shift vec1 -8) #xFF)
                      (bitwise-and vec2 #xFF) (bitwise-and (arithmetic-shift vec2 -8) #xFF))])))
       

       })))


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

    (for/list ([x (in-range (- 256 1))])
      (list
       %11111111
       %11111111
       %11111111
       %11111111
       %11111111    
       %11111111    
       %11111111
       %11111111))
    )


   })


(define (level-1)
   (bytes->list  (file->bytes "C:\\Users\\juan\\Documents\\untitled.bin")))   
