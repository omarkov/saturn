;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright (c) 2004, Oliver Markovic <entrox@entrox.org> 
;;;;   All rights reserved. 
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright notice,
;;;;    this list of conditions and the following disclaimer. 
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution. 
;;;;  o Neither the name of the author nor the names of the contributors may be
;;;;    used to endorse or promote products derived from this software without
;;;;    specific prior written permission. 
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :saturn)


(defstruct (sh2
             (:constructor %make-sh2))
  ;; 16 general purpose registers
  (registers (make-array 16
                         :element-type '(unsigned-byte 32)
                         :initial-element 0)
             :type (simple-array (unsigned-byte 32) (16)))

  ;; status register
  (sr #xf0 :type (unsigned-byte 32))
  ;; global base register
  (gbr 0 :type (unsigned-byte 32))
  ;; vector base register
  (vbr 0 :type (unsigned-byte 32))

  ;; high multiply and accumulate register
  (mach 0 :type (unsigned-byte 32))
  ;; low multiply and accumulate register
  (macl 0 :type (unsigned-byte 32))
  ;; procedure register
  (pr 0 :type (unsigned-byte 32))
  ;; program counter
  (pc 0 :type (unsigned-byte 32))

  ;; delay slot
  (delay-slot 0 :type (unsigned-byte 32))

  ;; on-chip register area
  (register-area (make-array #x200 :element-type '(unsigned-byte 8))
                 :type (simple-array (unsigned-byte 8) (#x200))))


(defun make-sh2 ()
  (%make-sh2))


(defun reset-cpu (cpu)
  (dotimes (i 16) (setf (aref (sh2-registers cpu) i) 0))
  (setf (sh2-sr cpu) #xf0
        (sh2-gbr cpu) 0
        (sh2-vbr cpu) 0
        (sh2-mach cpu) 0
        (sh2-macl cpu) 0
        (sh2-pr cpu) 0
        (sh2-delay-slot cpu) 0)
  (setf (sh2-pc cpu) (+ (read-long (sh2-vbr cpu)) 4)
        (aref (sh2-registers cpu) 15) (read-long (+ (sh2-vbr cpu) 4)))
  cpu)


;;; Status register

(defconstant $t #x1)
(defconstant $s #x2)
(defconstant $i0 #x10)
(defconstant $i1 #x20)
(defconstant $i2 #x40)
(defconstant $i3 #x80)
(defconstant $q #x100)
(defconstant $m #x200)

(defun set-status-bit (cpu bit)
  (setf (sh2-sr cpu) (logior bit (sh2-sr cpu))))

(defun clear-status-bit (cpu bit)
  (setf (sh2-sr cpu) (logand (lognot bit) (sh2-sr cpu))))

(defun status-bit-set? (cpu bit)
  (not (zerop (logand bit (sh2-sr cpu)))))


;;; Status dump

(defun print-registers (cpu stream)
  (format stream "General purpose registers:~%")
  (dotimes (i 16)
    (let ((register-value (aref (sh2-registers cpu) i)))
      (format stream " r~d: #x~X (~d)~%" i register-value register-value))))

(defun print-control-registers (cpu stream)
  (format stream "Control registers:~%")
  (let ((status-bits '()))
    (when (status-bit-set? cpu $t) (pushnew 't status-bits))
    (when (status-bit-set? cpu $s) (pushnew 's status-bits))
    (when (status-bit-set? cpu $i0) (pushnew 'i0 status-bits))
    (when (status-bit-set? cpu $i1) (pushnew 'i1 status-bits))
    (when (status-bit-set? cpu $i2) (pushnew 'i2 status-bits))
    (when (status-bit-set? cpu $i3) (pushnew 'i3 status-bits))
    (when (status-bit-set? cpu $q) (pushnew 'q status-bits))
    (when (status-bit-set? cpu $m) (pushnew 'm status-bits))

    (format stream " SR: #x~X ~a~%" (sh2-sr cpu) status-bits))

  (format stream " GBR: #x~X (~d)~%" (sh2-gbr cpu) (sh2-gbr cpu))
  (format stream " VBR: #x~X (~d)~%" (sh2-vbr cpu) (sh2-vbr cpu)))

(defun print-system-registers (cpu stream)
  (format stream "System registers:~%")
  (format stream " MACH: #x~X (~d)~%" (sh2-mach cpu) (sh2-mach cpu))
  (format stream " MACL: #x~X (~d)~%" (sh2-macl cpu) (sh2-macl cpu))
  (format stream " PR: #x~X (~d)~%" (sh2-pr cpu) (sh2-pr cpu))
  (format stream " PC: #x~X (~d)~%" (sh2-pc cpu) (sh2-pc cpu)))

(defun print-cpu-state (cpu &optional (stream *standard-output*))
  (print-registers cpu stream)
  (print-control-registers cpu stream)
  (print-system-registers cpu stream))



;;; Disassembly

(defun disassemble-instruction (insn)
  (let ((instruction (find-instruction insn)))
    (apply #'format
           nil
           (instruction-format-string instruction)
           (case (instruction-format instruction)
             (:zero (list))
             ((:n :m) (list (ash (logand #x0f00 insn) -8)))
             (:nm (list (ash (logand #x00f0 insn) -4)
                        (ash (logand #x0f00 insn) -8)))
             ((:md :nd4) (list (logand #x000f insn)
                               (ash (logand #x00f0 insn) -4)))
             (:nmd (list (logand #x000f insn)
                         (ash (logand #x00f0 insn) -4)
                         (ash (logand #x0f00 insn) -8)))
             (:d (list (logand #x00ff insn)))
             (:d12 (list (logand #x0fff insn)))
             (:nd8 (list (logand #x00ff insn)
                         (ash (logand #x0f00 insn) -8)))
             (:i (list (logand #x00ff insn)))
             (:ni (list (logand #x00ff insn)
                        (ash (logand #x0f00 insn) -8)))))))


(defun disassemble-memory (address count)
  (dotimes (i count)
    (format t
            "~8x: ~a~%"
            (+ address (* i 2))
            (disassemble-instruction (read-word (+ address (* i 2)))))))



;;; Execution

(defun fetch-instruction (cpu)
  (let ((delay (sh2-delay-slot cpu)))
    (if (= delay 0)
        (read-word (- (sh2-pc cpu) 4))
        (read-word (- delay 4)))))

(defun execute-instruction (cpu insn)
  (let ((instruction (find-instruction insn))
        (delay (sh2-delay-slot cpu)))
    (if (= delay 0)
        (funcall instruction cpu insn)
        (let ((pc (sh2-pc cpu)))
          (setf (sh2-pc cpu) delay)
          (funcall instruction cpu insn)
          (setf (sh2-pc cpu) pc
                (sh2-delay-slot cpu) 0)))
    instruction))

(defun sh2-step (cpu &optional (count 1))
  (dotimes (i count)
    (let ((instruction (fetch-instruction cpu)))
      (format t "~8x: ~a~%" (sh2-pc cpu) (disassemble-instruction instruction))
      (execute-instruction cpu instruction))))



;;; on-chip register area

;;; SCI

(define-register smr (:read-write #x00 #xfe00 8)
  (cks0 cks1 mp stop o/e pe chr ca))


(define-register brr :read-write #xff #xfe01 8)
(define-register scr :read-write #x00 #xfe02 8)
(define-register tdr :read-write #xff #xfe03 8)
(define-register ssr :read-write #x84 #xfe04 8)
(define-register rdr :read-only  #x00 #xfe05 8)
