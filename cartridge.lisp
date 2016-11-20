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


;;; dummy reader/writer for now

(defun cartridge-read-byte (address)
  (format t "Cartridge: Read from #x~x.~%" address)
  #xff)

(defun cartridge-read-word (address)
  (format t "Cartridge: Read from #x~x.~%" address)
  #xffff)

(defun cartridge-read-long (address)
  (format t "Cartridge: Read from #x~x.~%" address)
  #xffffffff)


(defun cartridge-write-byte (address value)
  (format t "Cartridge: Write #x~x to #x~x.~%" value address)
  #xff)

(defun cartridge-write-word (address value)
  (format t "Cartridge: Write #x~x to #x~x.~%" value address)
  #xffff)

(defun cartridge-write-long (address value)
  (format t "Cartridge: Write #x~x to #x~x.~%" value address)
  #xffffffff)


;;; The CS0 and CS1 regions of the A-bus are mapped to the cartridge port.
;;; These areas are set up differently depending on what kind of cart is used.

(define-memory-region (#x02000000 #x03ffffff "A-Bus CS0")
    :byte-reader #'cartridge-read-byte
    :word-reader #'cartridge-read-word
    :long-reader #'cartridge-read-long
    :byte-writer #'cartridge-write-byte
    :word-writer #'cartridge-write-word
    :long-writer #'cartridge-write-long)

(define-memory-region (#x04000000 #x04ffffff "A-Bus CS1")
    :byte-reader #'cartridge-read-byte
    :word-reader #'cartridge-read-word
    :long-reader #'cartridge-read-long
    :byte-writer #'cartridge-write-byte
    :word-writer #'cartridge-write-word
    :long-writer #'cartridge-write-long)

