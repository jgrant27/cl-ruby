;;
;; Copyright (c) 2010, Justin Grant <justin at imagine27 dot com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this list
;; of conditions and the following disclaimer.
;; Redistributions in binary form must reproduce the above copyright notice, this
;; list of conditions and the following disclaimer in the documentation and/or
;; other materials provided with the distribution.
;; Neither the name of the <ORGANIZATION> nor the names of its contributors may be
;; used to endorse or promote products derived from this software without specific
;; prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

;;;;
;;;; A 'compiler' for a subset of the Ruby language
;;;;




(require :meta-sexp)
(use-package :meta-sexp)


(declaim #+sbcl(sb-ext:muffle-conditions style-warning warning))


;; misc

(defun make-string-array()
  (make-array 0
              :element-type 'character
              :fill-pointer 0
              :adjustable t))




;;;; Delimiters
(defvar *white-space* '(#\Space #\Tab))
(defvar *line-terms* '(#\Newline #\;))


;;;; Ruby sub-set grammar rules (reads like pseudo BNF)

(defatom line-term? (c)
  (or (char= c #\;) (newline? c)))

(defrule string? (&aux char (str (make-string-array))) ()
  "\"" (vector-push-extend #\" str)
  (:* (:assign char
               (:and (:not "\"") (:type ascii?)))
      (vector-push-extend char str))
  "\"" (vector-push-extend #\" str)
  str)

(defrule numeric? (&aux char
                        (num (make-char-accum))) ()
  (:? (:assign char (:or #\- #\+))
      (vector-push-extend char num))
  (:+ (:assign char (:type digit?))
      (vector-push-extend char num))
  num)

(defrule literal? (&aux str) ()
  (:assign str (:rule (or string? numeric?)))
  (:eof)
  str)

(defrule identifier? (&aux char (str (make-string-array))) ()
  (:+ (:assign char (:or #\_ (:type alpha?)))
      (vector-push-extend char str))
  (:* (:assign char (:or #\_ (:type (or alpha? alnum?))))
      (vector-push-extend char str))
  (:eof)
  str)

(defrule do? () ()
  (:or "do" (:type line-term?)))

(defrule end? () ()
  "end"
  (:return ")"))

(defrule varname? () ()
  (:rule identifier?))

(defrule operation? () ()
  (:rule identifier?))

(defrule fname? () ()
  (:rule identifier?))

(defrule variable? (&aux val) ()
  (:assign val (:or (:rule varname?) "nil"))
  (:eof)
  val)

(defrule arglist? (&aux (identifier       (make-char-accum))
                        (rest-identifiers (make-char-accum))) ()
  (:+ (:not (:or #\, (:type white-space?)))
      (:char-push identifier))
  (:*
   (:* (:type white-space?))
   #\,
   (:* (:type white-space?))
   (:+ (:not (:eof))
       (:char-push rest-identifiers))
   (:* (:type white-space?))
   (:assign rest-identifiers
            (arglist? (create-parser-context rest-identifiers))))
  (:assign identifier (identifier? (create-parser-context identifier)))

  (format nil "~A ~A" identifier rest-identifiers))

(defrule arg-decl? (&aux (arglist (make-char-accum))) ()
  (:* (:type white-space?))
  (:* "(")
  (:* (:type white-space?))
  (:+ (:not #\))
      (:char-push arglist))
  (:* (:type white-space?))
  (:* ")")

  (:assign arglist
           (arglist?
            (create-parser-context
             (string-trim *white-space* arglist))))
  (string-trim *white-space* arglist))

(defrule args? (&aux (arg      (make-char-accum))
                     (rest-args (make-char-accum))) ()
  (:+ (:not (:or #\, (:type white-space?)))
      (:char-push arg))
  (:*
   (:* (:type white-space?))
   #\,
   (:* (:type white-space?))
   (:+ (:not (:eof))
       (:char-push rest-args))
   (:* (:type white-space?))
   (:assign rest-args (args? (create-parser-context rest-args))))
  (:assign arg (arg? (create-parser-context arg)))

  (format nil "~A ~A" arg rest-args))

(defrule call-args? (&aux args) ()
  (:assign args (:rule args?))
  (string-trim *white-space* args))

(defrule block-var? () ()
  (:rule variable?))

(defrule range? (&aux (start (make-char-accum))
                      (end   (make-char-accum))) ()
  (:+ (:not (:or #\. (:type (or white-space?))))
      (:char-push start))
  ".."
  (:+ (:not (:or (:eof) (:type (or white-space?))))
      (:char-push end))
  (list start end))

(defrule for-loop? (&aux range
                         (block-var (make-char-accum))
                         (expr      (make-char-accum))) ()
  (:* (:type white-space?))
  "for"
  (:+ (:type white-space?))
  (:+ (:not (:or "in" (:type (or white-space?))))
      (:char-push block-var))
  (:+ (:type white-space?))
  "in"
  (:+ (:type white-space?))
  (:+ (:not (:type (or line-term? white-space?)))
      (:char-push expr))
  (:* (:type white-space?))
  (:+ (:rule do?))

  (block-var? (create-parser-context block-var))
  (:assign range (expr? (create-parser-context expr)))

  (format nil "(loop for ~A from ~A to ~A do "
          block-var (first range) (second range)))

(defrule assign? (&aux (var (make-char-accum)) (arg (make-char-accum))) ()
  (:* (:type white-space?))
  (:+ (:not (:or "=" (:type (or white-space?))))
      (:char-push var))
  (:* (:type white-space?))
  "="
  (:* (:type white-space?))
  (:+ (:not (:or (:type line-term?) (:eof)))
      (:char-push arg))
  (:* (:type white-space?))
  (variable? (create-parser-context var))
  (:assign arg (arg? (create-parser-context (string-trim *white-space* arg))))
  (format nil "(setf ~A ~A)" var arg))

(defrule arithmetic? (&aux sign
                           op
                           (lhs (make-char-accum))
                           (rhs (make-char-accum))) ()
  (:* (:type white-space?))
  (:? (:assign sign (:or #\+ #\-)))
  (:+ (:not (:or #\* #\+ #\- (:type white-space?)))
      (:char-push lhs))
  (:* (:type white-space?))
  (:assign op (:or #\* #\+ #\-))
  (:* (:type white-space?))
  (:+ (:not (:or (:type line-term?) (:eof)))
      (:char-push rhs))
  (:* (:type white-space?))

  (:assign lhs (format nil "~A~A" (if sign sign "") lhs))
  (:assign lhs (arg? (create-parser-context lhs)))
  (:assign rhs
           (arg? (create-parser-context (string-trim *white-space* rhs))))
  (format nil "(~A ~A ~A)" op lhs rhs))

(defrule function-def? (&aux (fname    (make-char-accum))
                             (arg-decl (make-char-accum))) ()
  "def"
  (:+ (:type white-space?))
  (:+ (:not (:or #\( (:type ( white-space?))))
      (:char-push fname))
  (:* (:type white-space?))
  (:+ (:not (:or #\) (:type line-term?)))
      (:char-push arg-decl))
  (:* (:type white-space?))
  (fname? (create-parser-context fname))
  (:assign arg-decl
           (arg-decl? (create-parser-context arg-decl)))
  (format nil "(defun ~A(~A)~%" fname arg-decl))

(defrule function-call? (&aux (operation (make-char-accum))
                              (args      (make-char-accum))) ()
  (:+ (:not (:or #\( (:type white-space?)))
      (:char-push operation))
  (:* (:type white-space?))
  (:+
   (:+ "(")
   (:* (:type white-space?))
   (:+ (:not #\))
       (:char-push args))
   (:* (:type white-space?))
   (:+ ")"))
  (:* (:type white-space?))
  (:eof)

  (operation?
   (create-parser-context (string-trim *white-space* operation)))
  (:assign args
           (call-args?
            (create-parser-context (string-trim *white-space* args))))

  (format nil "(~A ~A)" operation args))

(defrule primary? () ()
  (:rule (or end?
             literal?
             variable?
             function-def?
             function-call?
             for-loop?
             )))

(defrule arg? () ()
  (:rule (or assign?
             range?
             arithmetic?
             primary? )))

(defrule expr? () ()
  (:rule arg?))

(defrule call? () ()
  (:rule function-call?))

(defrule stmt? () ()
  (:rule (or call? expr?)))

(defrule compstmt? (&aux (sval (make-char-accum))
                         (sval2 (make-char-accum))) ()
  (:* (:type white-space?))
  (:* (:type line-term?))
  (:* (:type white-space?))
  (:+ (:not (:or (:type line-term?) (:eof)))
      (:char-push sval))
  (:assign sval (stmt? (create-parser-context
                        (string-trim *white-space* sval))))
  (:*
   (:* (:type white-space?))
   (:* (:type line-term?))
   (:* (:type white-space?))
   (:+ (:not (:or (:eof)))
       (:char-push sval2))
   (:assign sval2 (compstmt? (create-parser-context
                              (string-trim (append *white-space* *line-terms*) sval2)))))
  (:* (:type white-space?))
  (:* (:type line-term?))
  (:* (:type white-space?))

  (format nil "  ~A~%~A" sval sval2))

(defrule ruby-program? (&aux prog) ()
  (:assign prog (:rule compstmt?))
  (:* (:type line-term? white-space?))
  (progn (when prog
           (format nil "~%(progn~%~A)~%" prog))))


;; utils

(defun compile-ruby(str)
  (let ((output (ruby-program? (create-parser-context str))))
    (when output
      (read-from-string output :preserving-whitespace))))

(defun run-ruby(prg-lst)
  (eval prg-lst))


;;;; tests

;; (load "/home/jgrant/cl-ruby/ruby.lisp")

(defun print-digits(number)
  (let ((resstr (format nil "~A" number)))
    (format t "~A...<~A digits>...~A~%"
            (subseq resstr 0 30)
            (- (length resstr) 60)
            (subseq resstr (- (length resstr) 30)))))

(defmacro make-timed-test (name
                           &key code call print-function
                           (return-result t))
  (let ((codecomp (gensym))
        (callcomp (gensym))
        (result (gensym)))
    `(defun ,name()
       (let* ((,codecomp  (compile-ruby ,code))
              (,callcomp (compile-ruby ,call))
              (,result (progn (run-ruby ,codecomp)
                              (format t "~A~%~A" ,codecomp ,callcomp)
                              (time (run-ruby ,callcomp)))))
         (when ,print-function
           (format t "~A" (apply ,print-function (list ,result))))
         (when ,return-result ,result)))))


(make-timed-test
 test-functions
 :code "def square (n)
          n * n
        end
         def cube (n)
          res = square(n) * n
          res
        end"
 :call "-3 * cube(-3)")
;; 81

(make-timed-test
 test-loops
 :code "def dec (n)
          n - 1
        end

        def loops (n)
          tot = 0
          for i in 1..n do
            for j in 1..i do
              tot = tot + dec(j)
            end
            tot = tot + i
          end
          tot
        end"
 :call "loops(100)")
;; 171700

(make-timed-test
 test-fib
 :code "def fib (n)
          curr = 0
          succ = 1
          presucc = nil
          for i in 1..n do
            presucc = succ
            succ = curr + succ
            curr = presucc
          end

          curr
        end"
 :call "fib(1000000)"
 :return-result nil
 :print-function 'print-digits)
;; 195328212870775773163201494759...<208928 digits>...719893411568996526838242546875

(make-timed-test
 test-fact
 :code "def fact (n)
          tot = 1

          for i in 1..n do
           tot = tot * i
          end

          tot
        end"
 :call "fact(100000)"
 :return-result nil
 :print-function 'print-digits)
;; 282422940796034787429342157802...<456514 digits>...000000000000000000000000000000



(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning warning))
