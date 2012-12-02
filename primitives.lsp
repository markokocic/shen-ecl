
"                                                   The License
 
 The user is free to produce commercial applications with the software, to distribute these applications in source or binary  form, and to charge monies for them as he sees fit and in concordance with the laws of the land subject to the following  license.
 
 1. The license applies to all the software and all derived software and must appear on such.
 2. It is illegal to distribute the software without this license attached to it and use of the software implies agreement 
    with the license as such. It is illegal for anyone who is not the copyright holder to tamper with or change the license.
 3. Neither the names of Lambda Associates or the copyright holder may be used to endorse or promote products built using
     the software without specific prior written permission from the copyright holder.
 4. That possession of this license does not confer on the copyright holder any special contractual obligation towards the    user. That in no event shall the copyright holder be liable for any direct, indirect, incidental, special, exemplary or   consequential damages (including but not limited to procurement of substitute goods or services, loss of use, data, or    profits; or business interruption), however caused and on any theory of liability, whether in contract, strict liability   or tort (including negligence) arising in any way out of the use of the software, even if advised of the possibility of   such damage. 
5. It is permitted for the user to change the software, for the purpose of improving performance, correcting an error, or    porting to a new platform, and distribute the modified version of Shen (hereafter the modified version) provided the     resulting program conforms in all respects to the Shen standard and is issued under that title. The user must it clear   with his distribution that he/she is the author of the changes and what these changes are and why. 
6. Derived versions of this software in whatever form are subject to the same restrictions. In particular it is not          permitted to make derived copies of this software which do not conform to the Shen standard or appear under a different title.
7. It is permitted to distribute versions of Shen which incorporate libraries, graphics or other facilities which are not    part of the Shen standard.

For an explication of this license see http://www.lambdassociates.org/News/june11/license.htm which explains this license in full."

(SETQ *user-syntax-in* NIL)

(DEFMACRO if (X Y Z)
  `(LET ((*C* ,X))
       (COND ((EQ *C* 'true) ,Y) 
             ((EQ *C* 'false) ,Z)
             (T (ERROR "~S is not a boolean~%" *C*)))))

(DEFMACRO and (X Y) `(if ,X (if ,Y 'true 'false) 'false))

(DEFMACRO or (X Y) `(if ,X 'true (if ,Y 'true 'false)))

(DEFUN set (X Y) (SET X Y))

(DEFUN value (X) (SYMBOL-VALUE X))

(DEFUN simple-error (String) (ERROR String))

(DEFMACRO trap-error (X F) 
`(HANDLER-CASE ,X (ERROR (Condition) (FUNCALL ,F Condition))))

(DEFUN error-to-string (E) 
   (IF (TYPEP E 'CONDITION) 
       (FORMAT NIL "~A" E)
       (ERROR "~S is not an exception~%" E)))

(DEFUN cons (X Y) (CONS X Y))

(DEFUN hd (X) (CAR X))

(DEFUN tl (X) (CDR X))

(DEFUN cons? (X) (IF (CONSP X) 'true 'false))

(DEFUN intern (String) (INTERN (shen-process-intern String)))

(DEFUN shen-process-intern (S)
   (COND ((STRING-EQUAL S "") S)
         ((STRING-EQUAL (pos S 0) "#") 
          (cn "_hash1957" (shen-process-intern (tlstr S))))
         ((STRING-EQUAL (pos S 0) "'") 
          (cn "_quote1957" (shen-process-intern (tlstr S))))
         ((STRING-EQUAL (pos S 0) "`") 
          (cn "_backquote1957" (shen-process-intern (tlstr S))))
         ((STRING-EQUAL (pos S 0) "|") 
          (cn "bar!1957" (shen-process-intern (tlstr S))))
         (T (cn (pos S 0) (shen-process-intern (tlstr S))))))          

(DEFUN eval-kl (X) 
 (LET ((E (EVAL (shen-kl-to-lisp NIL X))))
      (IF (AND (CONSP X) (EQ (CAR X) 'defun))
          (COMPILE E) 
          E)))
                 
(DEFMACRO lambda (X Y) `(FUNCTION (LAMBDA (,X) ,Y)))

(DEFMACRO let (X Y Z) `(LET ((,X ,Y)) ,Z))

(DEFUN equal? (X Y) (IF (shen-ABSEQUAL X Y) 'true 'false))

(DEFUN shen-ABSEQUAL (X Y)
  (COND ((AND (CONSP X) (CONSP Y) (shen-ABSEQUAL (CAR X) (CAR Y))) 
         (shen-ABSEQUAL (CDR X) (CDR Y)))
        ((AND (STRINGP X) (STRINGP Y)) (STRING= X Y))
        ((AND (NUMBERP X) (NUMBERP Y)) (= X Y))
        ((AND (ARRAYP X) (ARRAYP Y)) (CF-VECTORS X Y (LENGTH X) (LENGTH Y)))
        (T (EQUAL X Y))))

(DEFUN CF-VECTORS (X Y LX LY) 
   (AND (= LX LY) 
        (CF-VECTORS-HELP X Y 0 (1- LX))))

(DEFUN CF-VECTORS-HELP (X Y COUNT MAX)
  (COND ((= COUNT MAX) (shen-ABSEQUAL (AREF X MAX) (AREF Y MAX)))
        ((shen-ABSEQUAL (AREF X COUNT) (AREF Y COUNT)) (CF-VECTORS-HELP X Y (1+ COUNT) MAX))
        (T NIL)))
  
(DEFMACRO freeze (X) `(FUNCTION (LAMBDA () ,X)))

(DEFUN absvector (N) (MAKE-ARRAY N :INITIAL-ELEMENT 'fail!))

(DEFUN absvector? (X) (IF (ARRAYP X) 'true 'false))

(DEFUN address-> (Vector N Value) (SETF (AREF Vector N) Value) Vector)

(DEFUN <-address (Vector N) (AREF Vector N))

(DEFUN n->string (N) (FORMAT NIL "~C" (CODE-CHAR N)))

(DEFUN string->n (S) (CHAR-CODE (CAR (COERCE S 'LIST))))
        
(DEFUN pr (X S) 
   (WRITE-STRING (shen-printer-routines X *printer*) S))

(DEFUN shen-printer-routines (X Routines)
  (IF (NULL Routines)
      X
      (shen-printer-routines (FUNCALL (CAR Routines) X) (CDR Routines))))

(DEFUN read-byte (S) 
  (IF (CHAR-STREAM? S) 
    (CHAR-CODE (READ-CHAR S)) 
    (READ-BYTE S NIL -1)))

(DEFUN CHAR-STREAM? (S) 
  (SUBTYPEP (STREAM-ELEMENT-TYPE S) 'CHARACTER))

(DEFUN open (Type String Direction)
   (LET ((Path (FORMAT NIL "~A~A" *home-directory* String)))
        (COND ((EQ Type 'file) (file-stream Path Direction))
              (T (ERROR "invalid stream type")))))

(DEFUN file-stream (String Direction)
     (COND ((EQ Direction 'in) (OPEN String :DIRECTION :INPUT :ELEMENT-TYPE 'UNSIGNED-BYTE))
           ((EQ Direction 'out) (OPEN String :DIRECTION :OUTPUT)) 
           (T (ERROR "invalid direction"))))

(DEFUN type (X MyType) (DECLARE (IGNORE MyType)) X)

(DEFUN close (Stream) (CLOSE Stream) NIL)

(DEFUN pos (X N) (COERCE (LIST (CHAR X N)) 'STRING))
        
(DEFUN tlstr (X) (SUBSEQ X 1))

(DEFUN cn (Str1 Str2) (CONCATENATE 'STRING Str1 Str2))

(DEFUN string? (S) (IF (STRINGP S) 'true 'false))

(DEFUN str (X) 
  (COND ((NULL X) (ERROR "[] is not an atom in Shen; str cannot convert it to a string.~%"))
        ((SYMBOLP X) 
         (shen-process-string (SYMBOL-NAME X)))
        ((NUMBERP X) 
         (shen-process-number (FORMAT NIL "~A" X)))
        ((STRINGP X) (FORMAT NIL "~S" X))
        ((STREAMP X) (FORMAT NIL "~A" X))
        ((FUNCTIONP X) (FORMAT NIL "~A" X)) 
        (T (ERROR "~S is not an atom, stream or closure; str cannot convert it to a string.~%" X))))

(DEFUN shen-process-number (S)
  (COND ((STRING-EQUAL S "") "")
        ((STRING-EQUAL (pos S 0) "d")
         (IF (STRING-EQUAL (pos S 1) "0")
             ""
             (cn "e" (tlstr S))))
        (T (cn (pos S 0) (shen-process-number (tlstr S))))))

(DEFUN shen-process-string (X)
   (COND ((STRING-EQUAL X "") X)
         ((AND (> (LENGTH X) 8) 
               (STRING-EQUAL X "_hash1957" :END1 9)) 
          (cn "#" (shen-process-string (SUBSEQ X 9))))
         ((AND (> (LENGTH X) 9) 
               (STRING-EQUAL X "_quote1957" :END1 10)) 
          (cn "'" (shen-process-string (SUBSEQ X 10))))
         ((AND (> (LENGTH X) 13) 
               (STRING-EQUAL X "_backquote1957" :END1 14)) 
          (cn "`" (shen-process-string (SUBSEQ X 14))))
         ((AND (> (LENGTH X) 7) 
               (STRING-EQUAL X "bar!1957" :END1 8))
          (cn "|" (shen-process-string (SUBSEQ X 8))))
         (T (cn (pos X 0) (shen-process-string (tlstr X)))))) 

(DEFUN get-time (Time) 
  (COND ((EQ Time 'run)
         (* 1.0 (/ (GET-INTERNAL-RUN-TIME) 
                   INTERNAL-TIME-UNITS-PER-SECOND)))
        ((EQ Time 'unix)
                     (- (GET-UNIVERSAL-TIME) 2208988800))
        (T (ERROR "get-time does not understand the parameter ~A~%" Time))))

(DEFUN multiply (X Y) 
  (* (double-precision X)(double-precision Y)))

(DEFUN add (X Y) 
  (+ (double-precision X)(double-precision Y)))

(DEFUN subtract (X Y) 
  (- (double-precision X)(double-precision Y)))

(DEFUN divide (X Y) 
  (LET ((Div (/ (double-precision X)
                (double-precision Y))))
                      (IF (INTEGERP Div)
                           Div
                          (* (COERCE 1.0 'DOUBLE-FLOAT) Div))))

(DEFUN double-precision (X)
  (IF (INTEGERP X) X (COERCE X 'DOUBLE-FLOAT)))

(DEFUN greater? (X Y) (IF (> X Y) 'true 'false))

(DEFUN less? (X Y) (IF (< X Y) 'true 'false))

(DEFUN greater-than-or-equal-to? (X Y) (IF (>= X Y) 'true 'false))

(DEFUN less-than-or-equal-to? (X Y) (IF (<= X Y) 'true 'false))

(SETQ *stinput* *STANDARD-INPUT*)

(SETQ *stoutput* *STANDARD-OUTPUT*)

(DEFUN number? (N) (IF (NUMBERP N) 'true 'false))