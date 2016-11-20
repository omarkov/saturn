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


;; signed/unsigned handling

(declaim (inline unsigned->signed))
(defun unsigned->signed (value bits)
  (if (>= value (ash 1 (1- bits)))
      (- value (ash 1 bits))
      value))

(declaim (inline signed->unsigned))
(defun signed->unsigned (value bits)
  (if (< value 0)
      (+ value (ash 1 bits))
      value))

(declaim (inline sign-extend))
(defun sign-extend (value from to)
  (if (logbitp (1- from) value)
      (dpb (1- (ash 1 to)) (byte (- to from) from) value)
      (logand (lognot (ash 1 from)) value)))


;; addition and subtraction with overflow (unsigned 32-bit)

(declaim (inline +!32))
(defun +!32 (x y)
  (declare ((unsigned-byte 32) x y))
  (ldb (byte 32 0) (+ x y)))

(declaim (inline -!32))
(defun -!32 (x y)
  (declare ((unsigned-byte 32) x y))
  (ldb (byte 32 0) (- x y)))

(define-modify-macro incf!32 (&optional (delta 1))
  +!32)

(define-modify-macro decf!32 (&optional (delta 1))
  -!32)
