#lang asi64

(provide (all-defined-out))

(define (remote-programmer-init)
{
    lda @0
    sta $dd03  ;pin all inputs

    ;pa2 is an output
    lda $dd02         
    ora @%0000_0100
    sta $dd02

    ;pa2 starts HIGH (inverted?)
    lda $dd00
    and @%1111_1011
    sta $dd00

    ;clear any pending int
    lda $dd0d

})

(define (remote-programmer-main)
{
:programmer-check
         data-reg = $dd01
         ;interrupt will be set on a negative transition
         ;indicating the pi has put the first byte of a program
         ;on the wire
         lda $dd0d
         and @%0001_0000
         ;if this bit is set then
         ;jump. reading this also clears it.
         bne cont+
         rts
:cont    inc $d020
         ; read control bytes
         start-lo = $40
         start-hi = $41
         total-pages = $43
         first-page-bytes = $44
         last-page-bytes = $45
         data-ptr-lo = $46
         data-ptr-hi = $47
         ;todo: write these as an indexed loop
         ;to save space
         (define (get loc) {
           lda data-reg
           sta loc
           jsr wait+
           })
         ;disable interrupts for loading
         cli
         (get start-lo)
         (get start-hi)
         (get data-ptr-lo)
         (get data-ptr-hi)
         (get total-pages)
         (get first-page-bytes)
         lda data-reg
         sta last-page-bytes
;         (get last-page-bytes)

         ;read/write first page bytes         
         ldx @0
         ldy @0
         lda first-page-bytes ;skip if zero
         beq main+ ;         
         ; read until end of page
      :next
         jsr wait+
         lda data-reg
         sta £ data-ptr-lo y
         iny
         cpy first-page-bytes
         bne next-

         ;move to next page
         inc data-ptr-hi
         lda @0
         sta data-ptr-lo

     :main
         ldy @0
         ldx total-pages
         beq last+         ;skip if zero
         ;copy whole pages
     :loop
         jsr wait+
         lda data-reg     
         sta £ data-ptr-lo y    
         iny
         bne loop-
         inc data-ptr-hi
         dex
         bne loop-              
     :last
        lda last-page-bytes
        beq done+
     :loop
        jsr wait+
        lda data-reg
        sta £ data-ptr-lo y        
        iny
        cpy last-page-bytes
        bne loop-
          
      :done
        ;re-enable interrupts, splat stack
        ;and jump to new execution point
        ;toggle pa2 to let the pi know we are done
        lda $dd00
        eor @%0000_0100
        sta $dd00
        ldx @$ff
      :delay
        dex
        bne delay-
        ldx @$ff
      :delay
        dex
        bne delay-
        ldx @$ff
        inc $0400
        ldx @$ff
        txs
        sei
        jmp £ start-lo 

     :wait
         ;toggle pa2 to let the pi know we are done
         lda $dd00
         eor @%0000_0100
         sta $dd00
         ;wait for pi to send a new byte
:inner   lda $dd0d
         and @%0001_0000                    
         beq inner-
         rts
})
