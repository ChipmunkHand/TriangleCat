#lang asi64
(provide (all-defined-out))
(define (add-16 target to-add)
  ; 16 bit little-endian numbers
  ; adds to-add onto target.
  ; pass in the low addresses.
  {
   clc
   lda to-add
   adc target
   sta target
   lda (+ to-add 1)
   adc (+ target 1)
   sta (+ target 1)
  })

(define (sub-16 target to-add)
  ; 16 bit little-endian numbers
  ; subs to-add from target.
  ; pass in the low addresses.
  {
   sec
   lda target
   sbc to-add
   sta target
   lda (+ target 1)
   sbc (+ to-add 1)
   sta (+ target 1)
   })

(define (copy-16 target source)
  ;copy the source to the target
  ;little endian, pass in the low address
  {
   lda source
   sta target
   lda (+ source 1)
   sta (+ target 1)
   })

(define (vec/neg target temp)
  ;in place twos complement negation
  ;temp is the low byte of a clobberable 2 byte area  
  {
   lda target
   eor @$FF
   sta target
   lda (+ 1 target)
   eor @$FF
   sta (+ 1 target)
   (create-vec temp 1)
   (add-16 target temp)
  })

(define (vec/8 vec)
  ; destructive divide by 8 considering
  ; sign bit and 9th bit     
  {
   clc
   rol vec ; 2nd bit in
   rol (+ 1 vec) ; sign bit out
   rol vec ; 1st bit in
   rol (+ 1 vec) ; 9th bit out
   lda (+ 1 vec)
   ror
   lsr
   lsr
   })






(define (create-fractional-vec target value)
  ;creates a 16 bit vector at target (low address)
  ;uses 1 sign bit, 9 whole bits and 6 fractional.
  (cond
    [(integer? value)
     (if (> value -1)
         (let ([low  (bitwise-and value %00_111_111)])             
            {
              lda @low
              sta target
              lda @0
              sta (+ target 1)
            })
         (error "create-vec doesnt yet support negative values"))]
    [else  (raise-argument-error 'create-vec "integer?" value)]))

(define (create-vec target value)
  ;creates a 16 bit vector at target (low address)
  ;uses 1 sign bit, 9 whole bits and 6 fractional.
  (cond
    [(integer? value)
     (if (> value -1)
         (let* ([actual (arithmetic-shift value 6)]
                 [high   (arithmetic-shift (bitwise-and actual $FF00) -8)]
                 [low    (bitwise-and actual $FF)])             
            {
              lda @low
              sta target
              lda @high
              sta (+ target 1)
            })
         (error "create-vec doesnt yet support negative values"))]
    [else  (raise-argument-error 'create-vec "integer?" value)]))
  
