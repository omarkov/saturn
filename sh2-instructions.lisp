;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2004, Oliver Markovic <entrox@entrox.org> 
;;;   All rights reserved. 
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  o Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer. 
;;;  o Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution. 
;;;  o Neither the name of the author nor the names of the contributors may be
;;;    used to endorse or promote products derived from this software without
;;;    specific prior written permission. 
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :saturn)


(defmacro with-instruction-bindings ((format insn) &body body)
  (ecase format
    ;; xxxx xxxx xxxx xxxx
    (:zero `(progn ,@body))
    ;; xxxx nnnn xxxx xxxx
    (:n `(let ((n (ash (logand #x0f00 ,insn) -8)))
	   (declare ((unsigned-byte 4) n))
	   ,@body))
    ;; xxxx mmmm xxxx xxxx
    (:m `(let ((m (ash (logand #x0f00 ,insn) -8)))
	   (declare ((unsigned-byte 4) m))
	   ,@body))
    ;; xxxx nnnn mmmm xxxx
    (:nm `(let ((n (ash (logand #x0f00 ,insn) -8))
		(m (ash (logand #x00f0 ,insn) -4)))
	    (declare ((unsigned-byte 4) n m))
	    ,@body))
    ;; xxxx mmmm dddd xxxx
    (:md `(let ((m (ash (logand #x00f0 ,insn) -4))
		(d (logand #x000f ,insn)))
	    (declare ((unsigned-byte 4) m d))
	    ,@body))
    ;; xxxx xxxx nnnn dddd
    (:nd4 `(let ((n (ash (logand #x00f0 ,insn) -4))
		 (d (logand #x000f ,insn)))
	     (declare ((unsigned-byte 4) n d))
	     ,@body))
    ;; xxxx nnnn mmmm dddd
    (:nmd `(let ((n (ash (logand #x0f00 ,insn) -8))
		 (m (ash (logand #x00f0 ,insn) -4))
		 (d (logand #x000f ,insn)))
	     (declare ((unsigned-byte 4) n m d))
	     ,@body))
    ;; xxxx xxxx dddd dddd
    (:d `(let ((d (logand #x00ff ,insn)))
	   (declare ((unsigned-byte 8) d))
	   ,@body))
    ;; xxxx dddd dddd dddd
    (:d12 `(let ((d (logand #x0fff ,insn)))
	     (declare ((unsigned-byte 12) d))
	     ,@body))
    ;; xxxx nnnn dddd dddd
    (:nd8 `(let ((n (ash (logand #x0f00 ,insn) -8))
		 (d (logand #x00ff ,insn)))
	     (declare ((unsigned-byte 4) n)
		      ((unsigned-byte 8) d))
	     ,@body))
    ;; xxxx xxxx iiii iiii
    (:i `(let ((i (logand #x00ff ,insn)))
	   (declare ((unsigned-byte 8) i))
	   ,@body))
    ;; xxxx nnnn iiii iiii
    (:ni `(let ((n (ash (logand #x0f00 ,insn) -8))
		(i (logand #x00ff ,insn)))
	    (declare ((unsigned-byte 4) n)
		     ((unsigned-byte 8) i))
	    ,@body))))

(defmacro definsn (symbol (format opcode format-string) &body body)
  (let ((cpu (gensym "CPU"))
        (instruction (gensym "INSTRUCTION")))
    `(progn
       (setf (get ',symbol 'format) ,format
             (get ',symbol 'opcode) ,opcode
             (get ',symbol 'format-string) ,format-string)
       (macrolet ((register (x) `(aref (sh2-registers ,',cpu) ,x))
                  (set-bit (bit) `(set-status-bit ,',cpu ,bit))
                  (clear-bit (bit) `(clear-status-bit ,',cpu ,bit))
                  (bit-set? (bit) `(status-bit-set? ,',cpu ,bit)))
         (symbol-macrolet ((sr (sh2-sr ,cpu))
                           (gbr (sh2-gbr ,cpu))
                           (vbr (sh2-vbr ,cpu))
                           (mach (sh2-mach ,cpu))
                           (macl (sh2-macl ,cpu))
                           (pr (sh2-mach ,cpu))
                           (pc (sh2-pc ,cpu))
                           (delay-slot (sh2-delay-slot ,cpu)))
           (defun ,symbol (,cpu ,instruction)
             (declare (ignorable ,instruction)
                      (sh2 ,cpu) (fixnum ,instruction))
             (with-instruction-bindings (,format ,instruction)
               ,@body)))))))



;;; ADD (ADD Binary): Arithmetic Instruction

(definsn add (:nm #x300c "add~8Tr~D, r~D")
  (setf (register m) (+!32 (register m) (register n)))
  (incf!32 pc 2))

(definsn addi (:ni #x7000 "add~8T~D, r~D")
  (setf (register n) (+!32 (register n) (sign-extend i 8 32))) ; unsigned->signed?
  (incf!32 pc 2))


;;; ADDC (ADD with Carry): Arithmetic Instruction

(definsn addc (:nm #x300e "addc~8Tr~D, r~D")
  (let ((tmp0 (+!32 (register n) (register m)))
        (tmp1 (register n)))
    (setf (register n) (+!32 tmp1 (logand #x1 sr)))
    (if (> tmp0 tmp1)
        (set-bit $t)
        (clear-bit $t))
    (when (> tmp1 (register n))
      (set-bit $t)))
  (incf!32 pc 2))


;;; ADDV (ADD with V Flag Overflow Check): Arithmetic Instruction

(definsn addv (:nm #x300f "addv~8Tr~D, r~D")
  (let ((dest 0)
        (src 0)
        (ans 0))
    (when (< (unsigned->signed (register n) 32) 0)
      (setf dest 1))
    (when (< (unsigned->signed (register m) 32) 0)
      (setf src 1))
    (incf!32 src dest)
    (incf!32 (register n) (register m))
    (if (>= (unsigned->signed (register n) 32) 0)
        (setf ans 0)
        (setf ans 1))
    (incf!32 ans dest)
    (if (or (= src 0) (= src 2))
        (if (= ans 1)
            (set-bit $t)
            (clear-bit $t))
        (clear-bit $t)))
  (incf!32 pc 2))


;;; AND (AND Logical): Logic Operation Instruction

(definsn _and (:nm #x2009 "and~8Tr~D, r~D")
  (setf (register n) (logand (register n) (register m)))
  (incf!32 pc 2))

(definsn andi (:i #xc900 "and~8T~X, r0")
  (setf (register 0) (logand (register 0) i))
  (incf!32 pc 2))

(definsn andm (:i #xcd00 "and.b~8T~X, @(r0, GBR)")
  (let ((temp (logand (read-byte (+!32 (register 0) gbr)) i)))
    (write-byte (+!32 (register 0) gbr) temp))
  (incf!32 pc 2))


;;; BF (Branch if False): Branch Instruction

(definsn bf (:d #x8b00 "bf~8T~X")
  (let ((disp (unsigned->signed (sign-extend d 8 32) 32)))
    (if (bit-set? $t)
        (incf!32 pc 2)
        (incf!32 pc (+!32 (signed->unsigned (ash disp 1) 32) 4)))))


;;; BF/S (Branch if False with Delay Slot): Branch Instruction (SH-2)

(definsn bfs (:d #x8f00 "bf/s~8T~X")
  (let ((disp (unsigned->signed (sign-extend d 8 32) 32)))
    (if (bit-set? $t)
        (incf!32 pc 2)
        (progn
          (setf delay-slot (+!32 pc 2))
          (incf!32 pc (+!32 (signed->unsigned (ash disp 1) 32) 4))))))


;;; BRA (Branch): Branch Instruction

(definsn bra (:d12 #xa000 "bra~8T~X")
  (let ((disp (unsigned->signed (sign-extend d 12 32) 32)))
    (setf delay-slot (+!32 pc 2))
    (incf!32 pc (+!32 (signed->unsigned (ash disp 1) 32) 4))))


;;; BRAF (Branch Far): Branch Instruction (SH-2)

(definsn braf (:m #x0023 "braf~8T@r~D")
  (setf delay-slot (+!32 pc 2))
  (incf!32 pc (register m))) ; FIXME: is this correct wrt to sign?


;;; BSR (Branch to Subroutine): Branch Instruction

(definsn bsr (:d12 #xb000 "bsr~8T~X")
  (let ((disp (unsigned->signed (sign-extend d 12 32) 32)))
    (setf pr pc)
    (setf delay-slot (+!32 pr 2))
    (incf!32 pc (+!32 (signed->unsigned (ash disp 1) 32) 4))))


;;; BSRF (Branch to Subroutine Far): Branch Instruction (SH-2)

(definsn bsrf (:m #x0003 "bsrf~8T@r~D")
  (setf pr pc)
  (setf delay-slot (+ pr 2))
  (incf!32 pc (register m))) ; FIXME: is this correct wrt to the sign?


;;; BT (Branch if True): Branch Instruction

(definsn bt (:d #x8900 "bt~8T~X")
  (let ((disp (unsigned->signed (sign-extend d 8 32) 32)))
    (if (bit-set? $t)
        (incf!32 pc (+!32 (signed->unsigned (ash disp 1) 32) 4))
        (incf!32 pc 2))))


;;; BT/S (Branch if True with Delay Slot): Branch Instruction (SH-2)

(definsn bts (:d #x8d00 "bt/s~8T~X")
  (let ((disp (unsigned->signed (sign-extend d 8 32) 32)))
    (if (bit-set? $t)
        (progn
          (setf delay-slot (+!32 pc 2))
          (incf!32 pc (+!32 (signed->unsigned (ash disp 1) 32) 4)))
        (incf!32 pc 2))))


;;; CLRMAC (Clear MAC Register): System Control Instruction

(definsn clrmac (:zero #x0028 "clrmac")
  (setf mach 0
        macl 0)
  (incf!32 pc 2))


;;; CLRT (Clear T Bit): System Control Instruction

(definsn clrt (:zero #x0008 "clrt")
  (clear-bit $t)
  (incf!32 pc 2))


;;; CMP/cond (Compare Conditionally): Arithmetic Instruction

(definsn cmpeq (:nm #x3000 "cmp/eq~8Tr~D, r~D")
  (if (= (register n) (register m))
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn cmpge (:nm #x3003 "cmp/ge~8Tr~D, r~D")
  (if (>= (unsigned->signed (register n) 32)
          (unsigned->signed (register m) 32))
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn cmpgt (:nm #x3007 "cmp/gt~8Tr~D, r~D")
  (if (> (unsigned->signed (register n) 32)
         (unsigned->signed (register m) 32))
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn cmphi (:nm #x3006 "cmp/hi~8Tr~D, r~D")
  (if (> (register n) (register m))
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn cmphs (:nm #x3002 "cmp/hs~8Tr~D, r~D")
  (if (>= (register n) (register m))
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn cmppl (:n #x4015 "cmp/pl~8Tr~D")
  (if (> (unsigned->signed (register n) 32) 0)
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn cmppz (:n #x4011 "cmp/pz~8Tr~D")
  (if (>= (unsigned->signed (register n) 32) 0)
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn cmpstr (:nm #x200c "cmp/str~8Tr~D, r~D")
  (let ((temp (logxor (register n) (register m))))
    (if (and (not (zerop (logand #xff (ash temp -24))))
             (not (zerop (logand #xff (ash temp -16))))
             (not (zerop (logand #xff (ash temp -8))))
             (not (zerop (logand #xff temp))))
        (clear-bit $t)
        (set-bit $t)))
  (incf!32 pc 2))

(definsn cmpim (:i #x8800 "cmp/eq~8T~D, r0")
  (let ((imm (unsigned->signed (sign-extend i 8 32) 32)))
    (if (= imm (register 0)) ; FIXME: convert r0 to signed?
        (set-bit $t)
        (clear-bit $t)))
  (incf!32 pc 2))


;;; DIV0S (Divide Step 0 as Signed): Arithmetic Instruction

(definsn div0s (:nm #x2007 "div0s~8Tr~D, r~D")
  (if (= (logand #x80000000 (register n)) 0)
      (clear-bit $q)
      (set-bit $q))
  (if (= (logand #x80000000 (register m)) 0)
      (clear-bit $m)
      (set-bit $m))
  (if (eql (bit-set? $m) (bit-set? $q))
      (clear-bit $t)
      (set-bit $t))
  (incf!32 pc 2))


;;; DIV0U (Divide Step 0 as Unsigned): Arithmetic Instruction

(definsn div0u (:zero #x0019 "div0u")
  (clear-bit $m)
  (clear-bit $q)
  (clear-bit $t)
  (incf!32 pc 2))


;;; DIV1 (Divide Step 1): Arithmetic Instruction

(definsn div1 (:nm #x3004 "div1~8Tr~D, r~D")
  (declare (ignore n m))
  (error "DIV1 not implemented yet")
  (incf!32 pc 2))


;;; DMULS.L (Double-Length Multiply as Signed): Arithmetic Instruction (SH-2)

(definsn dmuls (:nm #x300d "dmuls.l~8Tr~D, r~D")
  (let ((result (* (unsigned->signed (register n) 32)
                   (unsigned->signed (register m) 32))))
    (when (< result 0)
      (incf result (ash 1 64)))
    (setf mach (ash result -32)
          macl (logand result #xffffffff)))
  (incf!32 pc 2))


;;; DMULU.L (Double-Length Multiply as Unsigned): Arithmetic Instruction (SH-2)

(definsn dmulu (:nm #x3005 "dmulu.l~8Tr~D, r~D")
  (let ((result (* (register n) (register m))))
    (setf mach (ash result -32)
          macl (logand result #xffffffff)))
  (incf!32 pc 2))


;;; DT (Decrement and Test): Arithmetic Instruction (SH-2)

(definsn dt (:n #x4010 "dt~8Tr~D")
  (decf!32 (register n))
  (if (= (register n) 0)
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))


;;; EXTS (Extend as Signed): Arithmetic Instruction

(definsn extsb (:nm #x600e "exts.b~8Tr~D, r~D")
  (setf (register n) (sign-extend (register m) 8 32))
  (incf!32 pc 2))

(definsn extsw (:nm #x600f "exts.w~8Tr~D, r~D")
  (setf (register n) (sign-extend (register m) 16 32))
  (incf!32 pc 2))


;;; EXTU (Extend as Unsigned): Arithmetic Instruction

(definsn extub (:nm #x600c "extu.b~8Tr~D, r~D")
  (setf (register n) (logand #xff (register m)))
  (incf!32 pc 2))

(definsn extuw (:nm #x600d "extu.w~8Tr~D, r~D")
  (setf (register n) (logand #xffff (register m)))
  (incf!32 pc 2))


;;; JMP (Jump): Branch Instruction

(definsn jmp (:m #x402b "jmp~8T@r~D")
  (setf delay-slot (+!32 pc 2))
  (setf pc (+!32 (register m) 4)))


;;; JSR (Jump to Subroutine): Branch Instruction

(definsn jsr (:m #x400b "jsr~8T@r~D")
  (setf pr pc)
  (setf delay-slot (+!32 pr 2))
  (setf pc (+!32 (register m) 4)))


;;; LDC (Load to Control Register): System Control Instruction

(definsn ldcsr (:m #x400e "ldc~8Tr~D, SR")
  (setf sr (logand #x3f3 (register m)))
  (incf!32 pc 2))

(definsn ldcgbr (:m #x401e "ldc~8Tr~D, GBR")
  (setf gbr (register m))
  (incf!32 pc 2))

(definsn ldcvbr (:m #x402e "ldc~8Tr~D, VBR")
  (setf vbr (register m))
  (incf!32 pc 2))

(definsn ldcmsr (:m #x4007 "ldc.l~8T@r~D+, SR")
  (setf sr (logand #x3f3 (read-long (register m))))
  (incf (register m) 4)
  (incf!32 pc 2))

(definsn ldcmgbr (:m #x4017 "ldc.l~8T@r~D+, GBR")
  (setf gbr (read-long (register m)))
  (incf (register m) 4)
  (incf!32 pc 2))

(definsn ldcmvbr (:m #x4027 "ldc.l~8T@r~D+, VBR")
  (setf vbr (read-long (register m)))
  (incf (register m) 4)
  (incf!32 pc 2))


;;; LDS (Load to System Register): System Control Instruction

(definsn ldsmach (:m #x400a "lds~8Tr~D, MACH")
  (setf mach (register m))
  (incf!32 pc 2))

(definsn ldsmacl (:m #x401a "lds~8Tr~D, MACL")
  (setf macl (register m))
  (incf!32 pc 2))

(definsn ldspr (:m #x402a "lds~8Tr~D, PR")
  (setf pr (register m))
  (incf!32 pc 2))

(definsn ldsmmach (:m #x4006 "lds.l~8T@r~D+, MACH")
  (setf mach (read-long (register m)))
  (incf!32 (register m) 4)
  (incf!32 pc 2))

(definsn ldsmmacl (:m #x4016 "lds.l~8T@r~D+, MACL")
  (setf macl (read-long (register m)))
  (incf!32 (register m) 4)
  (incf!32 pc 2))

(definsn ldsmpr (:m #x4026 "lds.l~8T@r~D+, PR")
  (setf pr (read-long (register m)))
  (incf!32 (register m) 4)
  (incf!32 pc 2))


;;; MAC.L (Multiply and Accumulate Long): Arithmetic Instruction (SH-2)

(definsn macl (:nm #x000f "mac.l~8T@r~D+, @r~D+")
  (declare (ignore n m))
  (error "MAC.L not implemented yet.")
  (incf!32 pc 2))


;;; MAC.W (Multiply and Accumulate Word): Arithmetic Instruction

(definsn macw (:nm #x400f "mac.w~8T@r~D+, @r~D+")
  (declare (ignore n m))
  (error "MAC.W not implemented yet.")
  (incf!32 pc 2))


;;; MOV (Move Data): Data Transfer Instruction

(definsn mov (:nm #x6003 "mov~8Tr~D, r~D")
  (setf (register n) (register m))
  (incf!32 pc 2))

(definsn movbs (:nm #x2000 "mov.b~8Tr~D, @r~D")
  (write-byte (register n) (ldb (byte 8 0) (register m)))
  (incf!32 pc 2))

(definsn movws (:nm #x2001 "mov.w~8Tr~D, @r~D")
  (write-word (register n) (ldb (byte 16 0) (register m)))
  (incf!32 pc 2))

(definsn movls (:nm #x2002 "mov.l~8Tr~D, @r~D")
  (write-long (register n) (register m))
  (incf!32 pc 2))

(definsn movbl (:nm #x6000 "mov.b~8T@r~D, r~D")
  (setf (register n) (sign-extend (read-byte (register m)) 8 32))
  (incf!32 pc 2))

(definsn movwl (:nm #x6001 "mov.w~8T@r~D, r~D")
  (setf (register n) (sign-extend (read-word (register m)) 16 32))
  (incf!32 pc 2))

(definsn movll (:nm #x6002 "mov.l~8T@r~D, r~D")
  (setf (register n) (read-long (register m)))
  (incf!32 pc 2))

(definsn movbm (:nm #x2004 "mov.b~8Tr~D, @-r~D")
  (decf!32 (register n))
  (write-byte (register n) (ldb (byte 8 0) (register m)))
  (incf!32 pc 2))

(definsn movwm (:nm #x2005 "mov.w~8Tr~D, @-r~D")
  (decf!32 (register n) 2)
  (write-byte (register n) (ldb (byte 16 0) (register m)))
  (incf!32 pc 2))

(definsn movlm (:nm #x2006 "mov.l~8Tr~D, @-r~D")
  (decf!32 (register n) 4)
  (write-long (register n) (register m))
  (incf!32 pc 2))

(definsn movbp (:nm #x6004 "mov.b~8T@r~D+, r~D")
  (setf (register n) (sign-extend (read-byte (register m)) 8 32))
  (when (/= (register n) (register m))
    (incf!32 (register m)))
  (incf!32 pc 2))

(definsn movwp (:nm #x6005 "mov.w~8T@r~D+, r~D")
  (setf (register n) (sign-extend (read-word (register m)) 16 32))
  (when (/= (register n) (register m))
    (incf!32 (register m) 2))
  (incf!32 pc 2))

(definsn movlp (:nm #x6006 "mov.l~8T@r~D+, r~D")
  (setf (register n) (read-long (register m)))
  (when (/= (register n) (register m))
    (incf!32 (register m) 4))
  (incf!32 pc 2))

(definsn movbs0 (:nm #x0004 "mov.b~8Tr~D, @(r0, r~D)")
  (write-byte (+!32 (register n) (register 0))
              (ldb (byte 8 0) (register m)))
  (incf!32 pc 2))

(definsn movws0 (:nm #x0005 "mov.w~8Tr~D, @(r0, r~D)")
  (write-word (+!32 (register n) (register 0))
              (ldb (byte 16 0) (register m)))
  (incf!32 pc 2))

(definsn movls0 (:nm #x0006 "mov.l~8Tr~D, @(r0, r~D)")
  (write-long (+!32 (register n) (register 0)) (register m))
  (incf!32 pc 2))

(definsn movbl0 (:nm #x000c "mov.b~8T@(r0, r~D), r~D")
  (setf (register n) (sign-extend (read-byte (+!32 (register m) (register 0))) 8 32))
  (incf!32 pc 2))

(definsn movwl0 (:nm #x000d "mov.w~8T@(r0, r~D), r~D")
  (setf (register n) (sign-extend (read-word (+!32 (register m) (register 0))) 16 32))
  (incf!32 pc 2))

(definsn mowll0 (:nm #x000e "mov.l~8T@(r0, r~D), r~D")
  (setf (register n) (read-long (+!32 (register m) (register 0))))
  (incf!32 pc 2))


;;; MOV (Move Immediate Data): Data Transfer Instruction

(definsn movi (:ni #xe000 "mov~8T~D, r~D")
  (setf (register n) (sign-extend i 8 32))
  (incf!32 pc 2))

(definsn movwi (:nd8 #x9000 "mov.w~8T@(#x~x, PC), r~D")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (setf (register n) (sign-extend (read-word (+!32 pc (ash disp 1))) 16 32)))
  (incf!32 pc 2))

(definsn movli (:nd8 #xd000 "mov.l~8T@(#x~x, PC), r~D")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (setf (register n) (read-long (+!32 (logand pc #xfffffffc) (ash disp 2)))))
  (incf!32 pc 2))


;;; MOV (Move Peripheral Data): Data Transfer Instruction

(definsn movblg (:d #xc400 "mov.b~8T@(#x~x, GBR), r0")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (setf (register 0) (sign-extend (read-byte (+!32 gbr disp)) 8 32)))
  (incf!32 pc 2))

(definsn movwlg (:d #xc500 "mov.w~8T@(#x~x, GBR), r0")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (setf (register 0) (sign-extend (read-word (+!32 gbr (ash disp 1))) 16 32)))
  (incf!32 pc 2))

(definsn movllg (:d #xc600 "mov.l~8T@(#x~x, GBR), r0")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (setf (register 0) (read-long (+!32 gbr (ash disp 2)))))
  (incf!32 pc 2))

(definsn movbsg (:d #xc000 "mov.b~8Tr0, @(#x~x, GBR)")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (write-byte (+!32 gbr disp) (ldb (byte 8 0) (register 0))))
  (incf!32 pc 2))

(definsn movwsg (:d #xc100 "mov.w~8Tr0, @(#x~x, GBR)")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (write-word (+!32 gbr (ash disp 1)) (ldb (byte 16 0) (register 0))))
  (incf!32 pc 2))

(definsn movlsg (:d #xc200 "mov.l~8Tr0, @(#x~x, GBR)")
  (let ((disp (logand (unsigned->signed d 8) #xff)))
    (write-long (+!32 gbr (ash disp 2)) (register 0)))
  (incf!32 pc 2))


;;; MOV (Move Structure Data): Data Transfer Instruction

(definsn movbs4 (:nd4 #x8000 "mov.b~8Tr0, @(#x~x, r~D)")
  (let ((disp (logand d #xf)))
    (write-byte (+!32 (register n) disp) (ldb (byte 8 0) (register 0))))
  (incf!32 pc 2))

(definsn movws4 (:nd4 #x8100 "mov.w~8Tr0, @(#x~x, r~D)")
  (let ((disp (logand d #xf)))
    (write-word (+!32 (register n) (ash disp 1)) (ldb (byte 16 0) (register 0))))
  (incf!32 pc 2))

(definsn movls4 (:nmd #x1000 "mov.l~8Tr~D, @(#x~x, r~D)")
  (let ((disp (logand d #xf)))
    (write-long (+!32 (register n) (ash disp 2)) (register m)))
  (incf!32 pc 2))

(definsn movbl4 (:md #x8400 "mov.b~8T@(#x~x, r~D), r0")
  (let ((disp (logand d #xf)))
    (setf (register 0) (sign-extend (read-byte (+!32 (register m) disp)) 8 32)))
  (incf!32 pc 2))

(definsn movwl4 (:md #x8500 "mov.w~8T@(#x~x, r~D), r0")
  (let ((disp (logand d #xf)))
    (setf (register 0) (sign-extend (read-word (+!32 (register m) (ash disp 1))) 16 32)))
  (incf!32 pc 2))

(definsn movll4 (:nmd #x5000 "mov.b~8T@(#x~x, r~D), r~D")
  (let ((disp (logand d #xf)))
    (setf (register n) (read-long (+!32 (register m) (ash disp 2)))))
  (incf!32 pc 2))


;;; MOVA (Move Effective Address): Data Transfer Instruction

(definsn mova (:d #xc700 "mova~8T@(#x~x, PC), r0")
  (let ((disp (logand d #xff)))
    (setf (register 0) (+!32 (logand pc #xfffffffc) (ash disp 2))))
  (incf!32 pc 2))


;;; MOVT (Move T Bit): Data Transfer Instruction

(definsn movt (:n #x0029 "movt~8Tr~D")
  (setf (register n) (logand sr #x1))
  (incf!32 pc 2))


;;; MUL.L (Multiply Long): Arithmetic Instruction (SH-2)

(definsn mull (:nm #x0007 "mul.l~8Tr~D, r~D")
  (let ((result (* (register n) (register m))))
    (setf macl (logand result #xffffffff)))
  (incf!32 pc 2))


;;; MULS.W (Multiply as Signed Word): Arithmetic Instruction

(definsn muls (:nm #x200f "muls.w~8Tr~D, r~D")
  (let ((result (* (unsigned->signed (logand (register n) #xffff) 16)
                   (unsigned->signed (logand (register m) #xffff) 16))))
    (setf macl (logand result #xffffffff)))
  (incf!32 pc 2))


;;; MULU.W (Multiply as Unsigned Word): Arithmetic Instruction

(definsn mulu (:nm #x200e "mulu.w~8Tr~D, r~D")
  (let ((result (* (logand (register n) #xffff) 
                   (logand (register m) #xffff))))
    (setf macl (logand result #xffffffff)))
  (incf!32 pc 2))


;;; NEG (Negate): Arithmetic Instruction

(definsn neg (:nm #x600b "neg~8Tr~D, r~D")
  (setf (register n) (-!32 0 (register m)))
  (incf!32 pc 2))


;;; NEGC (Negate with Carry): Arithmetic Instruction

(definsn negc (:nm #x600a "negc~8Tr~D, r~D")
  (let ((temp (unsigned->signed (-!32 0 (register m)) 32)))
    (setf (register n) (-!32 temp (logand sr #x1)))
    (if (< 0 temp)
        (set-bit $t)
        (clear-bit $t))
    (when (< temp (register n))
      (set-bit $t)))
  (incf!32 pc 2))


;;; NOP (No Operation): System Control Instruction

(definsn nop (:zero #x0009 "nop")
  (incf!32 pc 2))


;;; NOT (NOT Logical Complement): Logic Operation Instruction

(definsn _not (:nm #x6007 "not~8Tr~D, r~D")
  (setf (register n) (+ (lognot (register m)) (ash 1 32))) ; signed->unsigned
  (incf!32 pc 2))


;;; OR (OR Logical) Logic Operation Instruction

(definsn _or (:nm #x200b "or~8Tr~D, r~D")
  (setf (register n) (logior (register n) (register m)))
  (incf!32 pc 2))

(definsn ori (:i #xcb00 "or~8T~D, r~D")
  (setf (register 0) (logior (register 0) i))
  (incf!32 pc 2))

(definsn orm (:i #xcf00 "or.b~8T~D, @(r0, GBR)")
  (let ((temp (logior (sign-extend (read-byte (+ gbr (register 0))) 8 32) i)))
    (write-byte (+!32 gbr (register 0)) temp))
  (incf!32 pc 2))


;;; ROTCL (Rotate with Carry Left): Shift Instruction

(definsn rotcl (:n #x4024 "rotcl~8Tr~D")
  (let ((temp (if (= (logand (register n) #x80000000) 0) 0 1)))
    (setf (register n) (ash (register n) 1))
    (if (bit-set? $t)
        (setf (register n) (logior (register n) #x1))
        (setf (register n) (logand (register n) #xfffffffe)))
    (if (= temp 1)
        (set-bit $t)
        (clear-bit $t)))
  (incf!32 pc 2))


;;; ROTCR (Rotate with Carry Right): Shift Instruction

(definsn rotcr (:n #x4025 "rotcr~8Tr~D")
  (let ((temp (if (= (logand (register n) #x80000000) 0) 0 1)))
    (setf (register n) (ash (register n) -1))
    (if (bit-set? $t)
        (setf (register n) (logior (register n) #x80000000))
        (setf (register n) (logand (register n) #x7fffffff)))
    (if (= temp 1)
        (set-bit $t)
        (clear-bit $t)))
  (incf!32 pc 2))


;;; ROTL (Rotate Left): Shift Instruction

(definsn rotl (:n #x4004 "rotl~8Tr~D")
  (if (= (logand (register n) #x80000000) 0)
      (clear-bit $t)
      (set-bit $t))
  (setf (register n) (ash (register n) 1))
  (if (bit-set? $t)
      (setf (register n) (logior (register n) #x1))
      (setf (register n) (logand (register n) #xfffffffe)))
  (incf!32 pc 2))


;;; ROTR (Rotate Right): Shift Instruction

(definsn rotr (:n #x4025 "rotr~8Tr~D")
  (if (= (logand (register n) #x80000000) 0)
      (clear-bit $t)
      (set-bit $t))
  (setf (register n) (ash (register n) -1))
  (if (bit-set? $t)
      (setf (register n) (logior (register n) #x80000000))
      (setf (register n) (logand (register n) #x7fffffff)))
  (incf!32 pc 2))


;;; RTE (Return from Exception): System Control Instruction

(definsn rte (:zero #x002b "rte")
  (let ((temp pc))
    (setf pc (+!32 (read-long (register 15)) 4))
    (incf!32 (register 15) 4)
    (setf sr (logand (read-long (register 15)) #x3f3))
    (incf!32 (register 15) 4)
    (setf delay-slot (+!32 temp 2))))


;;; RTS (Return from Subroutine): Branch Instruction

(definsn rts (:zero #x000b "rts")
  (setf delay-slot (+!32 pc 2))
  (setf pc (+!32 pr 4)))


;;; SETT (Set T Bit): System Control Instruction

(definsn sett (:zero #x0018 "sett")
  (set-bit $t)
  (incf!32 pc 2))


;;; SHAL (Shift Arithmetic Left): Shift Instruction

(definsn shal (:n #x4020 "shal~8Tr~D")
  (if (= (logand (register n) #x80000000) 0)
      (clear-bit $t)
      (set-bit $t))
  (setf (register n) (ash (register n) 1))
  (incf!32 pc 2))


;;; SHAR (Shift Arithmetic Right): Shift Instruction

(definsn shar (:n #x4021 "shar~8Tr~D")
  (let ((temp (if (= (logand (register n) #x80000000) 0) 0 1)))
    (if (= (logand (register n) #x1) 0)
        (clear-bit $t)
        (set-bit $t))
    (setf (register n) (ash (register n) -1))
    (if (= temp 1)
        (setf (register n) (logior (register n) #x80000000))
        (setf (register n) (logand (register n) #x7fffffff))))
  (incf!32 pc 2))


;;; SHLL (Shift Logical Left): Shift Instruction

(definsn shll (:n #x4000 "shll~8Tr~D")
  (if (= (logand (register n) #x80000000) 0)
      (clear-bit $t)
      (set-bit $t))
  (setf (register n) (ash (register n) 1))
  (incf!32 pc 2))


;;; SHLLn (Shift Logical Left n Bits): Shift Instruction

(definsn shll2 (:n #x4008 "shll2~8Tr~D")
  (setf (register n) (ldb (byte 32 0) (ash (register n) 2)))
  (incf!32 pc 2))

(definsn shll8 (:n #x4018 "shll8~8Tr~D")
  (setf (register n) (ldb (byte 32 0) (ash (register n) 8)))
  (incf!32 pc 2))

(definsn shll16 (:n #x4028 "shll16~8Tr~D")
  (setf (register n) (ldb (byte 32 0) (ash (register n) 16)))
  (incf!32 pc 2))


;;; SHLR (Shift Logical Right): Shift Instruction

(definsn shlr (:n #x4001 "shlr~8Tr~D")
  (if (= (logand (register n) #x1) 0)
      (clear-bit $t)
      (set-bit $t))
  (setf (register n) (logand (ash (register n) -1) #x7fffffff))
  (incf!32 pc 2))


;;; SHLRn (Shift Logical Right n Bits): Shift Instruction

(definsn shlr2 (:n #x4009 "shlr2~8Tr~D")
  (setf (register n) (ash (register n) -2))
  (incf!32 pc 2))

(definsn shlr8 (:n #x4019 "shlr8~8Tr~D")
  (setf (register n) (ash (register n) -8))
  (incf!32 pc 2))

(definsn shlr16 (:n #x4029 "shlr16~8Tr~D")
  (setf (register n) (ash (register n) -16))
  (incf!32 pc 2))


;;; SLEEP (Sleep): System Control Instruction

(definsn _sleep (:zero #x001b "sleep")
  (error "SLEEP is not implemented yet."))


;;; STC (Store Control Register): System Control Instruction

(definsn stcsr (:n #x0002 "stc~8TSR, r~D")
  (setf (register n) sr)
  (incf!32 pc 2))

(definsn stcgbr (:n #x0012 "stc~8TGBR, r~D")
  (setf (register n) gbr)
  (incf!32 pc 2))

(definsn stcvbr (:n #x0022 "stc~8TVBR, r~D")
  (setf (register n) vbr)
  (incf!32 pc 2))

(definsn stcmsr (:n #x4003 "stc.l~8TSR, @-r~D")
  (decf!32 (register n) 4)
  (write-long (register n) sr)
  (incf!32 pc 2))

(definsn stcmgbr (:n #x4013 "stc.l~8TGBR, @-r~D")
  (decf!32 (register n) 4)
  (write-long (register n) gbr)
  (incf!32 pc 2))

(definsn stcmvbr (:n #x4023 "stc.l~8TVBR, @-r~D")
  (decf!32 (register n) 4)
  (write-long (register n) vbr)
  (incf!32 pc 2))


;;; STS (Store System Register): System Control Instruction

(definsn stsmach (:n #x000a "sts~8TMACH, r~D")
  (setf (register n) mach)
  (incf!32 pc 2))

(definsn stsmacl (:n #x001a "sts~8TMACL, r~D")
  (setf (register n) macl)
  (incf!32 pc 2))

(definsn stspr (:n #x002a "sts~8TPR, r~D")
  (setf (register n) pr)
  (incf!32 pc 2))

(definsn stsmmach (:n #x4002 "sts.l~8TMACH, @-r~D")
  (decf!32 (register n) 4)
  (write-long (register n) mach)
  (incf!32 pc 2))

(definsn stsmmacl (:n #x4012 "sts.l~8TMACL, @-r~D")
  (decf!32 (register n) 4)
  (write-long (register n) macl)
  (incf!32 pc 2))

(definsn stsmpr (:n #x4022 "sts.l~8TPR, @-r~D")
  (decf!32 (register n) 4)
  (write-long (register n) pr)
  (incf!32 pc 2))


;;; SUB (Subtract Binary): Arithmetic Instruction

(definsn sub (:nm #x3008 "sub~8Tr~D, r~D")
  (setf (register n) (-!32 (register n) (register m)))
  (incf!32 pc 2))


;;; SUBC (Subtract with Carry): Arithmetic Instruction

(definsn subc (:nm #x300a "subc~8Tr~D, r~D")
  (let ((tmp0 (register n))
        (tmp1 (-!32 (register n) (register m))))
    (setf (register n) (-!32 tmp1 (logand sr #x1)))
    (if (< tmp0 tmp1)
        (set-bit $t)
        (clear-bit $t))
    (when (< tmp1 (register n))
      (set-bit $t)))
  (incf!32 pc 2))


;;; SUBV (Subtract with V Flag Underflow Check): Arithmetic Instruction

(definsn subv (:nm #x300b "subv~8Tr~D, r~D")
  (let ((dest (if (>= (unsigned->signed (register n) 32) 0) 0 1))
        (src (if (>= (unsigned->signed (register m) 32) 0) 0 1)))
    (incf!32 src dest)
    (setf (register n) (-!32 (register n) (register m)))
    (let ((ans (if (>= (unsigned->signed (register n) 32)) 0 1)))
      (incf!32 ans dest)
      (if (= src 1)
          (if (= ans 1)
              (set-bit $t)
              (clear-bit $t))
          (clear-bit $t))))
  (incf!32 pc 2))


;;; SWAP (Swap Register Halves): Data Transfer Instruction

(definsn swapb (:nm #x6008 "swap.b~8Tr~D, r~D")
  (let ((temp0 (logand (register m) #xffff0000))
        (temp1 (ash (logand (register m) #xff) 8)))
    (setf (register n) (logior (logand (ash (register m) -8) #xff)
                               temp1
                               temp0)))
  (incf!32 pc 2))

(definsn swapw (:nm #x6009 "swap.w~8Tr~D, r~D")
  (let ((temp (logand (ash (register m) -16) #xffff)))
    (setf (register n) (logior (ash (register m) 16) temp)))
  (incf!32 pc 2))


;;; TAS (Test and Set): Logic Operation Instruction

(definsn tas (:n #x401b "tas.b~8T@r~D")
  (let ((temp (read-byte (register n))))
    (if (= temp 0)
        (set-bit $t)
        (clear-bit $t))
    (setf temp (logior temp #x80))
    (write-byte (register n) temp))
  (incf!32 pc 2))


;;; TRAPA (Trap Always): System Control Instruction

(definsn trapa (:i #xc300 "trapa~8T~D")
  (let ((imm (logand i #xff)))
    (decf (register 15) 4)
    (write-long (register 15) sr)
    (decf (register 15) 4)
    (write-long (register 15) (-!32 pc 2))
    (setf pc (+!32 (read-long (+!32 vbr (ash imm 2))) 4))))


;;; TST (Test Logical): Logic Operation Instruction

(definsn tst (:nm #x2008 "tst~8Tr~D, r~D")
  (if (= (logand (register n) (register m)) 0)
      (set-bit $t)
      (clear-bit $t))
  (incf!32 pc 2))

(definsn tsti (:i #xc800 "tst~8T~D, r0")
  (let ((temp (logand (register 0) (logand #xff i))))
    (if (= temp 0)
        (set-bit $t)
        (clear-bit $t)))
  (incf!32 pc 2))

(definsn tstm (:i #xcc00 "tst.b~8T~D, @(r0, GBR)")
  (let ((temp (read-byte (+ gbr (register 0)))))
    (if (= 0 (logand temp (logand #xff i)))
        (set-bit $t)
        (clear-bit $t)))
  (incf!32 pc 2))
 

;;; XOR (Exclusive OR Logical): Logic Operation Instruction

(definsn xor (:nm #x200a "xor~8Tr~D, r~D")
  (setf (register n) (logxor (register n) (register m)))
  (incf!32 pc 2))

(definsn xori (:i #xca00 "xor~8T~D, r0")
  (setf (register 0) (logxor (register 0) (logand #xff i)))
  (incf!32 pc 2))

(definsn xorm (:i #xce00 "xor.b~8T~D, @(r0, GBR)")
  (let ((temp (read-byte (+ gbr (register 0)))))
    (setf temp (logxor temp (logand #xff i)))
    (write-byte (+ gbr (register 0)) temp))
  (incf!32 pc 2))


;;; XTRCT (Extract): Data Transfer Instruction

(definsn xtrct (:nm #x200d "xtrct~8Tr~D, r~D")
  (setf (register n) (logior (logand (ash (register n) -16) #xffff)
                             (logand (ash (register m) 16) #xffff0000)))
  (incf!32 pc 2))


(defun instruction-format (insn)
  (get insn 'format))

(defun instruction-opcode (insn)
  (get insn 'opcode))

(defun instruction-format-string (insn)
  (get insn 'format-string))
      
(defun find-instruction (insn)
  (declare (fixnum insn)
           (optimize speed))
  (let ((a (ash (logand insn #xf000) -12))
        (b (ash (logand insn #x0f00) -8))
        (d (logand insn #x000f)))
    (let ((result (case a
                    (0 (case (logand insn #x00ff)
                         (2 'stcsr)
                         (3 'bsrf)
                         (4 'movbs0)
                         (5 'movws0)
                         (6 'movls0)
                         (7 'mull)
                         (8 'clrt)
                         (9 'nop)
                         (10 'stsmach)
                         (11 'rts)
                         (12 'movbl0)
                         (13 'movwl0)
                         (14 'mowll0)
                         (15 'macl)
                         (18 'stcgbr)
                         (24 'sett)
                         (25 'div0u)
                         (26 'stsmacl)
                         (27 '_sleep)
                         (34 'stcvbr)
                         (35 'braf)
                         (40 'clrmac)
                         (41 'movt)
                         (42 'stspr)
                         (43 'rte)))
                    (1 'movls4)
                    (2 (case d
                         (0 'movbs)
                         (1 'movws)
                         (2 'movls)
                         (4 'movbm)
                         (5 'movwm)
                         (6 'movlm)
                         (7 'div0s)
                         (8 'tst)
                         (9 '_and)
                         (10 'xor)
                         (11 '_or)
                         (12 'cmpstr)
                         (13 'xtrct)
                         (14 'mulu)
                         (15 'muls)))
                    (3 (case d
                         (0 'cmpeq)
                         (2 'cmphs)
                         (3 'cmpge)
                         (4 'div1)
                         (5 'dmulu)
                         (6 'cmphi)
                         (7 'cmpgt)
                         (8 'sub)
                         (10 'subc)
                         (11 'subv)
                         (12 'add)
                         (13 'dmuls)
                         (14 'addc)
                         (15 'addv)))
                    (4 (case (logand insn #x00ff)
                         (0 'shll)
                         (1 'shlr)
                         (2 'stsmmach)
                         (3 'stcmsr)
                         (4 'rotl)
                         (5 'rotr)
                         (6 'ldsmmach)
                         (7 'ldcmsr)
                         (8 'shll2)
                         (9 'shlr2)
                         (10 'ldsmach)
                         (11 'jsr)
                         (14 'ldcsr)
                         (15 'macw)
                         (16 'dt)
                         (17 'cmppz)
                         (18 'stsmmacl)
                         (19 'stcmgbr)
                         (21 'cmppl)
                         (22 'ldsmmacl)
                         (23 'ldcmgbr)
                         (24 'shll8)
                         (25 'shll8)
                         (26 'ldsmacl)
                         (27 'tas)
                         (30 'ldcgbr)
                         (32 'shal)
                         (33 'shar)
                         (34 'stsmpr)
                         (35 'stcmvbr)
                         (36 'rotcl)
                         (37 'rotcr)
                         (38 'ldsmpr)
                         (39 'ldcmvbr)
                         (40 'shll16)
                         (41 'shll16)
                         (42 'ldspr)
                         (43 'jmp)
                         (46 'ldcvbr)))
                    (5 'movll4)
                    (6 (case d
                         (0 'movbl)
                         (1 'movwl)
                         (2 'movll)
                         (3 'mov)
                         (4 'movbp)
                         (5 'movwp)
                         (6 'movlp)
                         (7 '_not)
                         (8 'swapb)
                         (9 'swapw)
                         (10 'negc)
                         (11 'neg)
                         (12 'extub)
                         (13 'extuw)
                         (14 'extsb)
                         (15 'extsw)))
                    (7 'addi)
                    (8 (case b
                         (0 'movbs4)
                         (1 'movws4)
                         (4 'movbl4)
                         (5 'movwl4)
                         (8 'cmpim)
                         (9 'bt)
                         (11 'bf)
                         (13 'bts)
                         (15 'bfs)))
                    (9 'movwi)
                    (10 'bra)
                    (11 'bsr)
                    (12 (case b
                          (0 'movbsg)
                          (1 'movwsg)
                          (2 'movlsg)
                          (3 'trapa)
                          (4 'movblg)
                          (5 'movwlg)
                          (6 'movllg)
                          (7 'mova)
                          (8 'tsti)
                          (9 'andi)
                          (10 'xori)
                          (11 'ori)
                          (12 'tstm)
                          (13 'andm)
                          (14 'xorm)
                          (15 'orm)))
                    (13 'movli)
                    (14 'movi))))
      (if result
          result
          (error "Illegal instruction ~X" insn)))))
