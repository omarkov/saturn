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


;;; Memory mapping

(defun unmapped-read-access (address)
  (error "Unmapped read access for #x~X." address))

(defun unmapped-write-access (address value)
  (error "Unmapped write access for #x~X with #x~X." address value))


(defstruct (memory-region
             (:constructor %make-memory-region))
  ;; start address
  (start 0 :type (unsigned-byte 27))
  ;; end address
  (end 0 :type (unsigned-byte 27))
  ;; region name
  (name "Unknown" :type string)

  ;; byte reading function
  (byte-reader #'unmapped-read-access :type function)
  ;; word reading function
  (word-reader #'unmapped-read-access :type function)
  ;; longword reading function
  (long-reader #'unmapped-read-access :type function)

  ;; byte writing function
  (byte-writer #'unmapped-write-access :type function)
  ;; word writing function
  (word-writer #'unmapped-write-access :type function)
  ;; longword writing function
  (long-writer #'unmapped-write-access :type function))



(defvar *memory-map* (make-array #x800 :initial-element (%make-memory-region)))


(defmacro define-memory-region ((start end description)
                                &key (byte-reader '#'unmapped-read-access)
                                     (word-reader '#'unmapped-read-access)
                                     (long-reader '#'unmapped-read-access)
                                     (byte-writer '#'unmapped-write-access)
                                     (word-writer '#'unmapped-write-access)
                                     (long-writer '#'unmapped-write-access))
  `(let ((region (%make-memory-region :start ,start :end ,end
                                      :name ,description
                                      :byte-reader ,byte-reader :byte-writer ,byte-writer
                                      :word-reader ,word-reader :word-writer ,word-writer
                                      :long-reader ,long-reader :long-writer ,long-writer)))
     (dotimes (i (1+ (ash (- ,end ,start) -16)))
       (let ((index (+ i (ash ,start -16))))
         (setf (aref *memory-map* index) region)))))




;; (define-memory-region #x00100000 #x0017ffff "SMPC")

;; (define-memory-region #x00300000 #x003fffff "Random data")
;; (define-memory-region #x00400000 #x007fffff "Always zero")
;; (define-memory-region #x00800000 #x00ffffff "Always 0001..0007")
;; (define-memory-region #x01000000 #x01ffffff "Always returns #xffff")
;; (define-memory-region #x05000000 #x057fffff "A-Bus Dummy")
;; (define-memory-region #x05800000 #x058fffff "A-Bus CS2")
;; (define-memory-region #x05900000 #x059fffff "Lockup when read")

;; (define-memory-region #x05b00000 #x05bfffff "SCSP registers")
;; (define-memory-region #x05c00000 #x05c7ffff "VDP1 VRAM")
;; (define-memory-region #x05c80000 #x05cfffff "VDP1 Framebuffer")
;; (define-memory-region #x05d00000 #x05d7ffff "VDP1 Registers")
;; (define-memory-region #x05d80000 #x05dfffff "Lockup when read")
;; (define-memory-region #x05e00000 #x05efffff "VDP2 VRAM")
;; (define-memory-region #x05f00000 #x05f7ffff "VDP2 CRAM")
;; (define-memory-region #x05f80000 #x05fbffff "VDP2 registers")
;; (define-memory-region #x05fc0000 #x05fdffff "Always returns #xe0000")
;; (define-memory-region #x05fe0000 #x05ffffff "SCU")




;;; Address calculation
;;;
;;; The SH7604 has a 32-bit address space. Of this, the lower 27 bits are mapped
;;; to the address bus, which give it a 128MB physical address space. Bits 28,27
;;; are not used, and bits 31-29 select special memory regions, which are as
;;; follows:
;;;
;;; 31 30 29
;;;  0  0  0 = Cache used (only if CE bit in CCR is set, otherwise no cache)
;;;  0  0  1 = Cache not used
;;;  0  1  0 = Associative purge area (Always returns $2312)
;;;  0  1  1 = Direct access to cache addresses (1K data, mirrored every 1K)
;;;  1  0  0 = Same as Cxxxxxxx
;;;  1  0  1 = Same as 2xxxxxxx
;;;  1  1  0 = Direct R/W access to cache data (4K data mirrored every 4K)
;;;  1  1  1 = On-chip registers

(defmacro with-mapped-region ((region mapped-address) address &body body)
  (let ((masked-address (gensym "MASKED-ADDRESS")))
    `(let* ((,masked-address (logand #x7ffffff ,address))
            (,mapped-address ,masked-address)
            (,region nil))
       (case (ash ,address -29)
         ;; ordinary memory access (with or without cache)
         ((0 1 5)
          (setf ,region (aref *memory-map* (ash ,masked-address -16)))
          (setf ,mapped-address (- ,masked-address (memory-region-start ,region))))
         ;; cache purge area
         (2 (setf ,region *purge-area-region*))
         ;; address array
         (3 (setf ,region *address-array-region*))
         ;; data array
         ((4 6) (setf ,region *data-array-region*))
         ;; on-chip area
         (7 (error "Tried accessing on-chip area")))
       ,@body)))



;;; External interface

(defun saturn::read-byte (address)
  "Read a byte (8 bit) from ADDRESS"
  (with-mapped-region (region mapped-address) address
    (funcall (memory-region-byte-reader region) mapped-address)))

(defun read-word (address)
  "Read a word (16 bit) from ADDRESS"
  (with-mapped-region (region mapped-address) address
    (funcall (memory-region-word-reader region) mapped-address)))

(defun read-long (address)
  "Read a byte (8 bit) from ADDRESS"
  (with-mapped-region (region mapped-address) address
    (funcall (memory-region-long-reader region) mapped-address)))



(defun saturn::write-byte (address byte)
  "Write a byte (8 bit) to ADDRESS"
  (with-mapped-region (region mapped-address) address
    (funcall (memory-region-byte-writer region) mapped-address byte)))

(defun write-word (address word)
  "Write a word (16 bit) to ADDRESS"
  (with-mapped-region (region mapped-address) address
    (funcall (memory-region-word-writer region) mapped-address word)))

(defun write-long (address long)
  "Write a long (32 bit) to ADDRESS"
  (with-mapped-region (region mapped-address) address
    (funcall (memory-region-long-writer region) mapped-address long)))




;;; Simple memory accesses
;;;
;;; The following functions implement a simple memory emulation for the
;;; regions that are not mapped to any specialized device. It consists
;;; of reading and writing to an array specialized on (UNSIGNED-BYTE 8).


(defun make-simple-memory (size)
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))


(defun simple-read-byte (array address)
  (aref array address))

(defun simple-read-word (array address)
  (let ((b0 (aref array address))
        (b1 (aref array (+ address 1))))
    (logior (ash b0 8) b1)))

(defun simple-read-long (array address)
  (let ((b0 (aref array address))
        (b1 (aref array (+ address 1)))
        (b2 (aref array (+ address 2)))
        (b3 (aref array (+ address 3))))
    (logior (ash b0 24) (ash b1 16) (ash b2 8) b3)))



(defun simple-write-byte (array address byte)
  (setf (aref array address) byte))


(defun simple-write-word (array address word)
  (let ((b0 (ash (logand word #xff00) -8))
        (b1 (logand word #xff)))
    (setf (aref array address) b0
          (aref array (+ address 1)) b1)))

(defun simple-write-long (array address long)
  (let ((b0 (ash (logand long #xff000000) -24))
        (b1 (ash (logand long #xff0000) -16))
        (b2 (ash (logand long #xff00) -8))
        (b3 (logand long #xff)))
      (setf (aref array address) b0
            (aref array (+ address 1)) b1
            (aref array (+ address 2)) b2
            (aref array (+ address 3)) b3)))


;; Convenience macros

(defmacro make-simple-reader (size array &key mirror)
  (assert (member size '(byte word long)))
  (let ((fn (intern (format nil "SIMPLE-READ-~A" size))))
    `(lambda (address)
       (,fn ,array ,(if mirror `(mod `,address ,mirror) 'address)))))

(defmacro make-simple-writer (size array &key mirror)
  (assert (member size '(byte word long)))
  (let ((fn (intern (format nil "SIMPLE-WRITE-~A" size))))
    `(lambda (address value)
       (,fn ,array ,(if mirror `(mod `,address ,mirror) 'address) value))))




;;; Definitions of the "simple" memory regions.
;;;
;;; Here are the definitions of the memory regions, which are not mapped
;;; to any specific device. The first three regions are special in that
;;; they are specially addressed by bits 31-29, which means they can't
;;; be referenced by *MEMORY-MAP* and must be selected by the macro
;;; WITH-MAPPED-REGION instead.


;;; CPU purge area. Always returns #x2312

(defvar *purge-area-region*
  (%make-memory-region
   :start #x0 :end #x2
   :name "CPU purge area"
   :byte-reader (lambda (a) (declare (ignore a)) #x23)
   :word-reader (lambda (a) (declare (ignore a)) #x2312)
   :long-reader (lambda (a) (declare (ignore a)) #x23122312)
   :byte-writer (lambda (a b)
                  (error "Illegal writer to purge area at #x~x: #x~x" a b))
   :word-writer (lambda (a b)
                  (error "Illegal writer to purge area at #x~x: #x~x" a b))
   :long-writer (lambda (a b)
                  (error "Illegal writer to purge area at #x~x: #x~x" a b))))


;;; CPU address array. 1K, mirrored every 1K

(defvar *address-array* (make-simple-memory #x400))
(defvar *address-array-region*
  (%make-memory-region
   :start #x0 :end #x400
   :name "CPU address array"
   :byte-reader (make-simple-reader byte *address-array* :mirror #x400)
   :word-reader (make-simple-reader word *address-array* :mirror #x400)
   :long-reader (make-simple-reader long *address-array* :mirror #x400)
   :byte-writer (make-simple-writer byte *address-array* :mirror #x400)
   :word-writer (make-simple-writer word *address-array* :mirror #x400)
   :long-writer (make-simple-writer long *address-array* :mirror #x400)))


;;; CPU data array. 4K, mirrored every 4K

(defvar *data-array* (make-simple-memory #x1000))
(defvar *data-array-region*
  (%make-memory-region
   :start #x0 :end #x1000
   :name "CPU data array"
   :byte-reader (make-simple-reader byte *data-array* :mirror #x400)
   :word-reader (make-simple-reader word *data-array* :mirror #x400)
   :long-reader (make-simple-reader long *data-array* :mirror #x400)
   :byte-writer (make-simple-writer byte *data-array* :mirror #x400)
   :word-writer (make-simple-writer word *data-array* :mirror #x400)
   :long-writer (make-simple-writer long *data-array* :mirror #x400)))





;;; Backup RAM (used to store save-states)

(defvar *backup-ram* (make-simple-memory #x10000))

(define-memory-region (#x00180000 #x001fffff "Backup RAM")
    :byte-reader (make-simple-reader byte *backup-ram* :mirror #x10000)
    :word-reader (make-simple-reader word *backup-ram* :mirror #x10000)
    :long-reader (make-simple-reader long *backup-ram* :mirror #x10000)
    :byte-writer (make-simple-writer byte *backup-ram* :mirror #x10000)
    :word-writer (make-simple-writer word *backup-ram* :mirror #x10000)
    :long-writer (make-simple-writer long *backup-ram* :mirror #x10000))


;;; Work RAM low

(defvar *work-ram-low* (make-simple-memory #x100000))

(define-memory-region (#x00200000 #x002fffff "Work RAM Low")
    :byte-reader (make-simple-reader byte *work-ram-low*)
    :word-reader (make-simple-reader word *work-ram-low*)
    :long-reader (make-simple-reader long *work-ram-low*)
    :byte-writer (make-simple-writer byte *work-ram-low*)
    :word-writer (make-simple-writer word *work-ram-low*)
    :long-writer (make-simple-writer long *work-ram-low*))


;;; 68k work RAM

(defvar *68k-work-ram* (make-simple-memory #x80000))

(define-memory-region (#x05a00000 #x05afffff "68000 Work RAM")
    :byte-reader (make-simple-reader byte *68k-work-ram*)
    :word-reader (make-simple-reader word *68k-work-ram*)
    :long-reader (make-simple-reader long *68k-work-ram*)
    :byte-writer (make-simple-writer byte *68k-work-ram*)
    :word-writer (make-simple-writer word *68k-work-ram*)
    :long-writer (make-simple-writer long *68k-work-ram*))


;;; Work RAM High

(defvar *work-ram-high* (make-simple-memory #x100000))

(define-memory-region (#x06000000 #x07ffffff "Work RAM High")
    :byte-reader (make-simple-reader byte *work-ram-high* :mirror #x100000)
    :word-reader (make-simple-reader word *work-ram-high* :mirror #x100000)
    :long-reader (make-simple-reader long *work-ram-high* :mirror #x100000)
    :byte-writer (make-simple-writer byte *work-ram-high* :mirror #x100000)
    :word-writer (make-simple-writer word *work-ram-high* :mirror #x100000)
    :long-writer (make-simple-writer long *work-ram-high* :mirror #x100000))
