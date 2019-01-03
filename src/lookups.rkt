#lang racket/base



(require racket/draw racket/class)



(define target (make-bitmap 30 30)) ; A 30x30 bitmap

(define dc (new bitmap-dc% [bitmap target]))

(send dc draw-rectangle 0 10 30 10)

(send dc draw-line 0 0 30 30)

(send dc draw-line 0 30 30 0)

target
;; #lang asi64
;; (set-emulator-program! emu "tc.prg")
;; (set-emulator-execute?! emu #f)
;; (set-emulator-breakpoints?! emu #t)
;; (set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")


;; (define new-zp
;;   (let ([next-addr $ff]
;;         ;whitelist of addresses to not use
;;         [exceptions (list $fe $fd)])
;;     (λ ()
;;       (define (aux)
;;         (let ([addr next-addr])
;;           (set! next-addr (- next-addr 1))
;;           (if (set-member? exceptions addr)
;;               (aux)
;;               addr)))
;;       (aux))))

;; (C64 {

;;       a = (new-zp) ; $ff
;;       b = (new-zp) ; $fc  
;;       c = (new-zp) ; $fb
      
;;       *= $1000
      
;;       lda a        ; $ff
;;       lda b        ; $fc
;;       lda c        ; $fb

;;       *= $3000

;; })

;; (require 2htdp/image)
;; (circle 30 "outline" "red")

;; (bitmap "C:\\Users\\juan\\AppData\\Local\\Temp\\racket-image-15404892651540489265907.png")

