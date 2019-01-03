#lang asi64
(require syntax/parse/define)

(require "psid.rkt")

(provide (all-defined-out))

(define (zp-generator exceptions)
  (let* ([next-addr $ff]
         ;blacklist of addresses to not use
         [kernal (list $a0 $a1 $a2)]
         [exceptions (remove-duplicates (append exceptions kernal))])
    (λ ()
      (define (aux)
        (let ([addr next-addr])
          (set! next-addr (- next-addr 1))
          (if (set-member? exceptions addr)
              (aux)
              addr)))
      (aux))))

;some constants
{ 

  ; screen bits
  x-visible = $18 ; subtract for sprite->char indexing
  y-visible = $32

  ; joy masks
  joy-up    = %0000_0001
  joy-down  = %0000_0010
  joy-left  = %0000_0100         
  joy-right = %0000_1000
  joy-fire  = %0001_0000

  ;directions
  top = 0
  right = 1
  bottom = 2
  left = 3
  
  ; TC state machine states
  state-standing = $0
  state-walking-right = $1
  state-walking-left = $2
  state-crouching = $3
  state-angle-right = $4
  state-angle-left = $5
  state-velocity = $6
  state-jumping = $7
  state-skidding-right = $8
  state-skidding-left = $9 
  state-falling-right = $a 
  state-falling-left = $b
  state-dying = $c

  angle-change-delay = $4
  vel-change-delay = $14

  (define-syntax-parser zp
    [(_ name ...)
     #'{ (begin         
           (define name (new-zp))
           (printf "~a = $~x\n" 'name name)
           (set-jump-source (format "~a" 'name) name)
           ) ...}])

  acid-disco = (load-psid "C:\\C64Music\\MUSICIANS\\A\\a-man\\acid_disco.sid")
  new-zp = (zp-generator (psid-header-zp acid-disco))

  ; ZP addresses
  ; ---------------------------
  (zp ; assigned from $ff downwards
   screen-hi
   screen-lo
   tc-angle-vxi
   tc-angle-vyi
   ;store vectors little endian 
   tc-vec-x-high
   tc-vec-x-low
   tc-vec-y-high
   tc-vec-y-low
   tc-vec-vx-high
   tc-vec-vx-low
   tc-vec-vy-high
   tc-vec-vy-low
   tc-falling 
   tc-vel-change-delay 
   tc-angle-change-delay 
   tc-table-frame 
   tc-temp 
   tc-vel 
   tc-angle-target 
   tc-anim-delay 
   tc-frame 
   tc-state
   tc-vec-tempx-high
   tc-vec-tempx-low
   tc-vec-tempy-high
   tc-vec-tempy-low
   vec-temp-high
   vec-temp-low
   scratch-a
   scratch-b
   scratch-c
   scratch-d
   )
    
}
