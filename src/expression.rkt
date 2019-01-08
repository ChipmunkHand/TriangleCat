#lang asi64

(require (for-syntax racket/list racket/function  racket/syntax))
(require
 syntax/parse/define)

(provide (all-defined-out))
(begin-for-syntax
  (define labels (make-hash))
  (define (gen-label name)
    (if (hash-has-key? labels name)
        (let ([val (hash-ref labels name)])
          (hash-set! labels name (+ val 1))
          (format "~a_~a" name (+ val 1)))
        (begin
          (hash-set! labels name 0)
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
      [false-target (string->symbol (format "~a:" false-label))]
      [end-source (string->symbol (format ":~a" end-label))]
      [end-target (string->symbol (format "~a:" end-label))])
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
