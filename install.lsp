;; ECL
;;install and wipe away the junk

(EXT:INSTALL-C-COMPILER)
;(PROCLAIM '(OPTIMIZE (SPEED 3) (SAFETY 3)))
 (PROCLAIM '(OPTIMIZE (DEBUG 0) (SPEED 0) (SAFETY 3)))
;(PROCLAIM '(OPTIMIZE (DEBUG 0) (SPEED 0) (SAFETY 3) (SPACE 0)))
;; (SETQ CUSTOM:*COMPILE-WARNINGS* NIL) 
;; (SETQ *COMPILE-VERBOSE* NIL)  

(IN-PACKAGE :CL-USER)
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(SETQ *language* "Common Lisp")
(SETQ *implementation* "Ecl")
(SETQ *release* "12.7.1-git")
(SETQ *port* 1.0)
(SETQ *porters* "Marko Kocic <marko@euptera.com>")
(SETQ *os* "Linux")

;(DEFUN function (X) (SYMBOL-FUNCTION (shen-maplispsym X)))

(DEFUN ecl-install (Filename)
  (LET* ((File (FORMAT NIL "~A.kl" Filename))
         (Read (read-in-kl File))
         (Intermediate (FORMAT NIL "~A.intermed" File))
         (Fas (FORMAT NIL "~A.fas" Intermediate))
         (Lsp (FORMAT NIL "~A.lsp" Intermediate))
         (Obj (FORMAT NIL "~A.o" Intermediate))
         (Write (write-out-kl Intermediate Read)))
    (boot Intermediate)
    (COMPILE-FILE (FORMAT NIL "~A.lsp" Intermediate) :SYSTEM-P T)
    (C:BUILD-FASL Fas :LISP-FILES (LIST Obj))
    (LOAD Fas)
    (DELETE-FILE Intermediate)
	(DELETE-FILE Lsp)
	(DELETE-FILE Fas)
;;    (DELETE-FILE File))
))


(DEFUN read-in-kl (File)
 (WITH-OPEN-FILE (In File :DIRECTION :INPUT)
   (kl-cycle (READ-CHAR In NIL NIL) In NIL 0)))
   
(DEFUN kl-cycle (Char In Chars State)
  (COND ((NULL Char) (REVERSE Chars))
        ((AND (MEMBER Char '(#\: #\; #\,) :TEST 'CHAR-EQUAL) (= State 0))
         (kl-cycle (READ-CHAR In NIL NIL) In (APPEND (LIST #\| Char #\|) Chars) State))
       ((CHAR-EQUAL Char #\") (kl-cycle (READ-CHAR In NIL NIL) In (CONS Char Chars) (flip State)))
        (T (kl-cycle (READ-CHAR In NIL NIL) In (CONS Char Chars) State))))

(DEFUN flip (State)
  (IF (ZEROP State)
      1
      0)) 

(COMPILE 'read-in-kl)
(COMPILE 'kl-cycle)   
(COMPILE 'flip)
   
(DEFUN write-out-kl (File Chars)
  (WITH-OPEN-FILE (Out File :DIRECTION :OUTPUT :IF-EXISTS :OVERWRITE :IF-DOES-NOT-EXIST :CREATE)
   (FORMAT Out "~{~C~}" Chars)))

(COMPILE 'write-out-kl)

(COMPILE-FILE "primitives.lsp" :SYSTEM-P T)
(C:BUILD-FASL "primitives" :LISP-FILES '("primitives.o"))
(LOAD "primitives.fas")
(DELETE-FILE "primitives.fas")

(COMPILE-FILE "backend.lsp" :SYSTEM-P T)
(C:BUILD-FASL "backend" :LISP-FILES '("backend.o"))
(LOAD "backend.fas")
(DELETE-FILE "backend.fas")

(DEFVAR kl-files '("toplevel" "core" "sys" "sequent" "yacc" "reader" "prolog" "track" "load" "writer" "macros" "declarations" "types" "t-star"))

(MAPC 'ecl-install kl-files)

(COMPILE-FILE "overwrite.lsp" :SYSTEM-P T)
(C:BUILD-FASL "overwrite" :LISP-FILES '("overwrite.o"))
(LOAD "overwrite.fas")
(DELETE-FILE "overwrite.fas")


(MAPC 'FMAKUNBOUND '(boot writefile openfile))

(DEFVAR obj-files 
  (APPEND
   (MAPCAR (LAMBDA (X) (FORMAT NIL "~A.kl.intermed.o" X)) kl-files)
   (LIST "primitives.o" "backend.o" "overwrite.o")))

(C:BUILD-PROGRAM "Shen" 
                 :LISP-FILES obj-files
                 :PROLOGUE-CODE NIL
                 :EPILOGUE-CODE '(shen-shen))

(MAPC 'DELETE-FILE obj-files)
(QUIT)
