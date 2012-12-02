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

(DEFUN boot (File)
  (LET* ((SourceCode (openfile File))
         (ObjectCode (MAPCAR 
                       (FUNCTION (LAMBDA (X) (shen-kl-to-lisp NIL X))) SourceCode)))
        ;;(DELETE-FILE (FORMAT NIL "~A.lsp" File))
        (writefile (FORMAT NIL "~A.lsp" File) ObjectCode)))

(DEFUN writefile (File Out)
    (WITH-OPEN-FILE (OUTSTREAM File
                               :DIRECTION :OUTPUT 
                               :IF-EXISTS :OVERWRITE
                               :IF-DOES-NOT-EXIST :CREATE)
    (FORMAT OUTSTREAM "~%")
    (MAPC (FUNCTION (LAMBDA (X) (FORMAT OUTSTREAM "~S~%~%" X))) Out)
  File))

(DEFUN openfile (File)
 (WITH-OPEN-FILE (In File :DIRECTION :INPUT)
   (DO ((R T) (Rs NIL)) 
      ((NULL R) (NREVERSE (CDR Rs)))
       (SETQ R (READ In NIL NIL)) 
       (PUSH R Rs))))

(DEFUN shen-kl-to-lisp (V12 V13)
 (COND ((MEMBER V13 V12) V13)
  ((AND (CONSP V13) (EQ 'type (CAR V13)) (CONSP (CDR V13))
    (CONSP (CDR (CDR V13))) (NULL (CDR (CDR (CDR V13)))))
   (shen-kl-to-lisp V12 (CAR (CDR V13))))
  ((AND (CONSP V13) (EQ 'lambda (CAR V13)) (CONSP (CDR V13))
    (CONSP (CDR (CDR V13))) (NULL (CDR (CDR (CDR V13)))))
   (LET ((X14 (CDR V13)))
    (LET ((V (shen-rectify-t (CAR X14))))
     (CONS 'FUNCTION
      (CONS
       (CONS 'LAMBDA
        (CONS (CONS V NIL)
         (CONS
          (shen-kl-to-lisp (CONS V V12)
           (shen-rectify-t-body V (CAR X14) (CAR (CDR X14))))
          NIL)))
       NIL)))))
  ((AND (CONSP V13) (EQ 'let (CAR V13)) (CONSP (CDR V13))
    (CONSP (CDR (CDR V13))) (CONSP (CDR (CDR (CDR V13))))
    (NULL (CDR (CDR (CDR (CDR V13))))))
   (LET ((X15 (CDR V13)))
    (LET ((V (shen-rectify-t (CAR X15))))
     (CONS 'LET
      (CONS (CONS (CONS V (CONS (shen-kl-to-lisp V12 (CAR (CDR X15))) NIL)) NIL)
       (CONS
        (shen-kl-to-lisp (CONS V V12)
         (shen-rectify-t-body V (CAR X15) (CAR (CDR (CDR X15)))))
        NIL))))))
  ((AND (CONSP V13) (EQ 'defun (CAR V13)) (CONSP (CDR V13))
    (CONSP (CDR (CDR V13))) (CONSP (CDR (CDR (CDR V13))))
    (NULL (CDR (CDR (CDR (CDR V13))))))
   (LET ((X16 (CDR V13)))
    (CONS 'DEFUN
     (CONS (CAR X16)
      (CONS (CAR (CDR X16))
       (CONS (shen-kl-to-lisp (CAR (CDR X16)) (CAR (CDR (CDR X16)))) NIL))))))
  ((AND (CONSP V13) (EQ 'cond (CAR V13)))
   (CONS 'COND
    (MAPCAR #'(LAMBDA (C) (shen-cond_code V12 C)) (shen-insert-default (CDR V13)))))
  ((AND (CONSP V13) (MEMBER (CAR V13) V12))
   (shen-higher-order-code (CAR V13)
    (MAPCAR #'(LAMBDA (Y) (shen-kl-to-lisp V12 Y)) (CDR V13))))
  ((AND (CONSP V13) (CONSP (CAR V13)))
   (shen-higher-order-code (shen-kl-to-lisp V12 (CAR V13))
    (MAPCAR #'(LAMBDA (W) (shen-kl-to-lisp V12 W)) (CDR V13))))
  ((AND (CONSP V13) (SYMBOLP (CAR V13)))
   (shen-assemble-application (CAR V13)
    (MAPCAR #'(LAMBDA (Y) (shen-kl-to-lisp V12 Y)) (CDR V13))))
  ((NULL V13) NIL)
  ((SYMBOLP V13) (CONS 'QUOTE (CONS V13 NIL)))
  (T V13)))
  
(DEFUN shen-rectify-t (V22) (COND ((EQ V22 T) 'Var1957343) (T V22)))

(DEFUN shen-rectify-t-body (V23 V24 V25)
 (COND ((EQ V24 V23) V25) (T (SUBST V23 V24 V25))))  
  
(DEFUN shen-insert-default (V1)
 (BLOCK NIL
  (IF (NULL V1)
   (RETURN
    (CONS
     (CONS 'true (CONS (CONS 'ERROR (CONS "error: cond failure~%" NIL)) NIL))
     NIL))
   (TAGBODY
    (IF (CONSP V1)
     (LET ((Car8 (CAR V1)))
      (IF (CONSP Car8)
       (LET ((Cdr7 (CDR Car8)))
        (IF (EQ 'true (CAR Car8))
         (IF (CONSP Cdr7) (IF (NULL (CDR Cdr7)) (RETURN V1) (GO tag2))
          (GO tag2))
         (GO tag2)))
       (GO tag2))))
    tag2
    (IF (CONSP V1) (RETURN (CONS (CAR V1) (shen-insert-default (CDR V1))))
     (RETURN (ERROR "shen-insert-default has failed")))))))     
    
(DEFUN shen-higher-order-code (V3394 V3395)
 (CONS 'let
  (CONS 'Args
   (CONS (CONS 'LIST V3395)
    (CONS
     (CONS 'let
      (CONS 'NewF
       (CONS (CONS 'shen-maplispsym (CONS V3394 NIL))
        (CONS
         (CONS 'trap-error
          (CONS (CONS 'APPLY (CONS 'NewF (CONS 'Args NIL)))
           (CONS
            (CONS 'lambda
             (CONS 'E
              (CONS
               (CONS 'COND
                (CONS
                 (CONS (CONS 'shen-arity-error? (CONS V3394 (CONS 'Args NIL)))
                  (CONS
                   (CONS 'shen-funcall
                    (CONS
                     (CONS 'EVAL
                      (CONS (CONS 'shen-nest-lambda (CONS V3394 (CONS 'NewF NIL)))
                       NIL))
                     (CONS 'Args NIL)))
                   NIL))
                 (CONS
                  (CONS
                   (CONS 'EQ
                    (CONS 'NewF (CONS (CONS 'QUOTE (CONS 'or NIL)) NIL)))
                   (CONS
                    (CONS 'shen-funcall
                     (CONS
                      (CONS 'lambda
                       (CONS 'X1
                        (CONS
                         (CONS 'lambda
                          (CONS 'X2
                           (CONS (CONS 'or (CONS 'X1 (CONS 'X2 NIL))) NIL)))
                         NIL)))
                      (CONS 'Args NIL)))
                    NIL))
                  (CONS
                   (CONS
                    (CONS 'EQ
                     (CONS 'NewF (CONS (CONS 'QUOTE (CONS 'and NIL)) NIL)))
                    (CONS
                     (CONS 'shen-funcall
                      (CONS
                       (CONS 'lambda
                        (CONS 'X1
                         (CONS
                          (CONS 'lambda
                           (CONS 'X2
                            (CONS (CONS 'and (CONS 'X1 (CONS 'X2 NIL))) NIL)))
                          NIL)))
                       (CONS 'Args NIL)))
                     NIL))
                   (CONS
                    (CONS
                     (CONS 'EQ
                      (CONS 'NewF
                       (CONS (CONS 'QUOTE (CONS 'trap-error NIL)) NIL)))
                     (CONS
                      (CONS 'shen-funcall
                       (CONS
                        (CONS 'lambda
                         (CONS 'X1
                          (CONS
                           (CONS 'lambda
                            (CONS 'X2
                             (CONS (CONS 'trap-error (CONS 'X1 (CONS 'X2 NIL)))
                              NIL)))
                           NIL)))
                        (CONS 'Args NIL)))
                      NIL))
                    (CONS
                     (CONS
                      (CONS 'shen-bad-lambda-call? (CONS 'NewF (CONS 'Args NIL)))
                      (CONS (CONS 'shen-funcall (CONS 'NewF (CONS 'Args NIL))) NIL))
                     (CONS
                      (CONS T (CONS (CONS 'shen-relay-error (CONS 'E NIL)) NIL))
                      NIL)))))))
               NIL)))
            NIL)))
         NIL))))
     NIL)))))

(DEFUN shen-relay-error (E) (error-to-string (ERROR E)))

(DEFUN shen-funcall (V34 V35)
 (COND ((NULL V35) V34)
  ((CONSP V35) (shen-funcall (FUNCALL V34 (CAR V35)) (CDR V35)))
  (T (ERROR "shen-funcall has failed~%"))))

(DEFUN shen-arity-error? (V36 V37)
   (AND (SYMBOLP V36) (> (trap-error (arity V36) #'(LAMBDA (E) -1)) (LIST-LENGTH V37))))

(DEFUN shen-bad-lambda-call? (F Args) (AND (FUNCTIONP F) (NOT (= (LIST-LENGTH Args) 1))))

(DEFUN shen-nest-lambda (F NewF) (shen-nest-lambda-help NewF (trap-error (arity F) #'(LAMBDA (E) -1))))

(DEFUN shen-nest-lambda-help (V18 V19)
 (COND ((ZEROP V19) V18)
  (T
   (LET ((X (GENSYM "Y")))
    (CONS 'lambda
     (CONS X (CONS (shen-nest-lambda-help (shen-add-p V18 X) (1- V19)) NIL)))))))

(DEFUN shen-add-p (V41 V42)
 (COND ((CONSP V41) (APPEND V41 (CONS V42 NIL)))
  (T (CONS V41 (CONS V42 NIL)))))

(DEFUN shen-cond_code (V43 V44)
 (COND
  ((AND (CONSP V44) (CONSP (CDR V44)) (NULL (CDR (CDR V44))))
   (CONS (shen_lisp_test V43 (CAR V44))
    (CONS (shen-kl-to-lisp V43 (CAR (CDR V44))) NIL)))
  (T (ERROR "shen-cond_code has failed~%"))))

(DEFUN shen_lisp_test (V47 V48)
 (COND ((EQ 'true V48) T)
  ((AND (CONSP V48) (EQ 'and (CAR V48)))
   (CONS 'AND (MAPCAR #'(LAMBDA (X) (shen-wrap (shen-kl-to-lisp V47 X))) (CDR V48))))
  (T (shen-wrap (shen-kl-to-lisp V47 V48)))))

(DEFUN shen-wrap (V494)
 (COND
  ((AND (CONSP V494)
    (AND (EQ 'cons? (CAR V494))
     (AND (CONSP (CDR V494)) (NULL (CDR (CDR V494))))))
   (CONS 'CONSP (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'string? (CAR V494))
     (AND (CONSP (CDR V494)) (NULL (CDR (CDR V494))))))
   (CONS 'STRINGP (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'number? (CAR V494))
     (AND (CONSP (CDR V494)) (NULL (CDR (CDR V494))))))
   (CONS 'NUMBERP (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'empty? (CAR V494))
     (AND (CONSP (CDR V494)) (NULL (CDR (CDR V494))))))
   (CONS 'NULL (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'and (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494))))))))
   (CONS 'AND
    (CONS (shen-wrap (CAR (CDR V494))) (CONS (shen-wrap (CAR (CDR (CDR V494)))) NIL))))
  ((AND (CONSP V494)
    (AND (EQ 'or (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494))))))))
   (CONS 'OR
    (CONS (shen-wrap (CAR (CDR V494))) (CONS (shen-wrap (CAR (CDR (CDR V494)))) NIL))))
  ((AND (CONSP V494)
    (AND (EQ 'not (CAR V494))
     (AND (CONSP (CDR V494)) (NULL (CDR (CDR V494))))))
   (CONS 'NOT (CONS (shen-wrap (CAR (CDR V494))) NIL)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494)))
       (AND (NULL (CAR (CDR (CDR V494)))) (NULL (CDR (CDR (CDR V494)))))))))
   (CONS 'NULL (CONS (CAR (CDR V494)) NIL)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (NULL (CAR (CDR V494)))
       (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494)))))))))
   (CONS 'NULL (CDR (CDR V494))))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494)))
       (AND (CONSP (CAR (CDR (CDR V494))))
        (AND (CONSP (CDR (CAR (CDR (CDR V494)))))
         (AND (NULL (CDR (CDR (CAR (CDR (CDR V494))))))
          (AND (NULL (CDR (CDR (CDR V494))))
           (AND (EQ (SYMBOLP (CAR (CDR (CAR (CDR (CDR V494)))))) 'T)
            (EQ (CAR (CAR (CDR (CDR V494)))) 'QUOTE))))))))))
   (CONS 'EQ (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CAR (CDR V494)))
       (AND (CONSP (CDR (CAR (CDR V494))))
        (AND (NULL (CDR (CDR (CAR (CDR V494)))))
         (AND (CONSP (CDR (CDR V494)))
          (AND (NULL (CDR (CDR (CDR V494))))
           (AND (EQ (SYMBOLP (CAR (CDR (CAR (CDR V494))))) 'T)
            (EQ (CAR (CAR (CDR V494))) 'QUOTE))))))))))
   (CONS 'EQ (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CAR (CDR V494)))
       (AND (EQ 'fail (CAR (CAR (CDR V494))))
        (AND (NULL (CDR (CAR (CDR V494))))
         (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494)))))))))))
   (CONS 'EQ (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494)))
       (AND (CONSP (CAR (CDR (CDR V494))))
        (AND (EQ 'fail (CAR (CAR (CDR (CDR V494)))))
         (AND (NULL (CDR (CAR (CDR (CDR V494)))))
          (NULL (CDR (CDR (CDR V494)))))))))))
   (CONS 'EQ (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494)))
       (AND (NULL (CDR (CDR (CDR V494)))) (STRINGP (CAR (CDR V494))))))))
   (CONS 'EQUAL (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494)))
       (AND (NULL (CDR (CDR (CDR V494)))) (STRINGP (CAR (CDR (CDR V494)))))))))
   (CONS 'EQUAL (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'equal? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494))))))))
   (CONS 'shen-ABSEQUAL (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'shen-+string? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CAR (CDR V494)))
       (AND (EQ 'tlstr (CAR (CAR (CDR V494))))
        (AND (CONSP (CDR (CAR (CDR V494))))
         (AND (NULL (CDR (CDR (CAR (CDR V494)))))
          (NULL (CDR (CDR V494))))))))))
   (CONS 'NOT
    (CONS
     (CONS 'STRING-EQUAL (CONS (LIST 'tlstr (CAR (CDR (CAR (CDR V494))))) (CONS "" NIL)))
     NIL)))
  ((AND (CONSP V494)
    (AND (EQ 'shen-pvar? (CAR V494))
     (AND (CONSP (CDR V494)) (NULL (CDR (CDR V494))))))
   (CONS 'AND
    (CONS (CONS 'ARRAYP (CDR V494))
     (CONS (CONS 'NOT (CONS (CONS 'STRINGP (CDR V494)) NIL))
      (CONS
       (CONS 'EQ
        (CONS (CONS 'AREF (CONS (CAR (CDR V494)) (CONS 0 NIL)))
         (CONS (CONS 'QUOTE (CONS 'shen-pvar NIL)) NIL)))
       NIL)))))
  ((AND (CONSP V494)
    (AND (EQ 'tuple? (CAR V494))
     (AND (CONSP (CDR V494)) (NULL (CDR (CDR V494))))))
   (CONS 'AND
    (CONS (CONS 'ARRAYP (CDR V494))
     (CONS (CONS 'NOT (CONS (CONS 'STRINGP (CDR V494)) NIL))
      (CONS
       (CONS 'EQ
        (CONS (CONS 'AREF (CONS (CAR (CDR V494)) (CONS 0 NIL)))
         (CONS (CONS 'QUOTE (CONS 'shen-tuple NIL)) NIL)))
       NIL)))))
  ((AND (CONSP V494)
    (AND (EQ 'greater? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494))))))))
   (CONS '> (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'greater-than-or-equal-to? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494))))))))
   (CONS '>= (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'less? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494))))))))
   (CONS '< (CDR V494)))
  ((AND (CONSP V494)
    (AND (EQ 'less-than-or-equal-to? (CAR V494))
     (AND (CONSP (CDR V494))
      (AND (CONSP (CDR (CDR V494))) (NULL (CDR (CDR (CDR V494))))))))
   (CONS '<= (CDR V494)))
  (T (CONS 'shen-wrapper (CONS V494 NIL)))))                    

(DEFUN shen-wrapper (V50)
 (COND ((EQ 'true V50) T) ((EQ 'false V50) NIL)
  (T (ERROR "boolean expected: not ~S~%" V50))))

(DEFUN shen-assemble-application (V4815 V4816)
 (BLOCK NIL
  (TAGBODY
   (IF (EQ 'hd V4815)
    (IF (CONSP V4816)
     (IF (NULL (CDR V4816)) (RETURN (CONS 'CAR V4816)) (GO tag4819))
     (GO tag4819)))
   tag4819
   (TAGBODY
    (IF (EQ 'tl V4815)
     (IF (CONSP V4816)
      (IF (NULL (CDR V4816)) (RETURN (CONS 'CDR V4816)) (GO tag4822))
      (GO tag4822)))
    tag4822
    (TAGBODY
     (IF (EQ 'cons V4815)
      (IF (CONSP V4816)
       (LET ((Cdr4829 (CDR V4816)))
        (IF (CONSP Cdr4829)
         (IF (NULL (CDR Cdr4829)) (RETURN (CONS 'CONS V4816)) (GO tag4825))
         (GO tag4825)))
       (GO tag4825)))
     tag4825
     (TAGBODY
      (IF (EQ 'append V4815)
       (IF (CONSP V4816)
        (LET ((Cdr4834 (CDR V4816)))
         (IF (CONSP Cdr4834)
          (IF (NULL (CDR Cdr4834)) (RETURN (CONS 'APPEND V4816)) (GO tag4830))
          (GO tag4830)))
        (GO tag4830)))
      tag4830
      (TAGBODY
       (IF (EQ 'reverse V4815)
        (IF (CONSP V4816)
         (IF (NULL (CDR V4816)) (RETURN (CONS 'REVERSE V4816)) (GO tag4835))
         (GO tag4835)))
       tag4835
       (TAGBODY
        (IF (EQ 'if V4815)
         (IF (CONSP V4816)
          (LET ((Cdr4844 (CDR V4816)))
           (IF (CONSP Cdr4844)
            (LET ((Cdr4843 (CDR Cdr4844)))
             (IF (CONSP Cdr4843)
              (IF (NULL (CDR Cdr4843))
               (RETURN (CONS 'IF (CONS (shen-wrap (CAR V4816)) Cdr4844)))
               (GO tag4838))
              (GO tag4838)))
            (GO tag4838)))
          (GO tag4838)))
        tag4838
        (TAGBODY
         (IF (EQ '+ V4815)
          (IF (CONSP V4816)
           (LET ((Car4853 (CAR V4816)) (Cdr4854 (CDR V4816)))
            (TAGBODY
             (IF (EQL 1 Car4853)
              (IF (CONSP Cdr4854)
               (IF (NULL (CDR Cdr4854)) (RETURN (CONS '1+ Cdr4854))
                (GO tag4847))
               (GO tag4847)))
             tag4847
             (IF (CONSP Cdr4854)
              (IF (EQL 1 (CAR Cdr4854))
               (IF (NULL (CDR Cdr4854)) (RETURN (CONS '1+ (CONS Car4853 NIL)))
                (GO tag4845))
               (GO tag4845))
              (GO tag4845))))
           (GO tag4845)))
         tag4845
         (TAGBODY
          (IF (EQ '- V4815)
           (IF (CONSP V4816)
            (LET ((Cdr4860 (CDR V4816)))
             (IF (CONSP Cdr4860)
              (IF (EQL 1 (CAR Cdr4860))
               (IF (NULL (CDR Cdr4860))
                (RETURN (CONS '1- (CONS (CAR V4816) NIL))) (GO tag4855))
               (GO tag4855))
              (GO tag4855)))
            (GO tag4855)))
          tag4855
          (TAGBODY
           (IF (EQ 'value V4815)
            (IF (CONSP V4816)
             (LET ((Car4869 (CAR V4816)))
              (IF (CONSP Car4869)
               (LET ((Cdr4868 (CDR Car4869)))
                (IF (CONSP Cdr4868)
                 (IF (NULL (CDR Cdr4868))
                  (IF (NULL (CDR V4816))
                   (IF (EQ (CAR Car4869) 'QUOTE) (RETURN (CAR Cdr4868))
                    (GO tag4861))
                   (GO tag4861))
                  (GO tag4861))
                 (GO tag4861)))
               (GO tag4861)))
             (GO tag4861)))
           tag4861
           (TAGBODY
            (IF (EQ 'set V4815)
             (IF (CONSP V4816)
              (LET ((Car4896 (CAR V4816)) (Cdr4897 (CDR V4816)))
               (IF (CONSP Car4896)
                (LET ((Car4894 (CAR Car4896)) (Cdr4895 (CDR Car4896)))
                 (IF (CONSP Cdr4895)
                  (LET ((Car4893 (CAR Cdr4895)))
                   (IF (NULL (CDR Cdr4895))
                    (IF (CONSP Cdr4897)
                     (LET ((Car4891 (CAR Cdr4897)) (Cdr4892 (CDR Cdr4897)))
                      (IF (CONSP Car4891)
                       (LET ((Car4889 (CAR Car4891)) (Cdr4890 (CDR Car4891)))
                        (TAGBODY
                         (IF (EQ '1+ Car4889)
                          (IF (CONSP Cdr4890)
                           (IF (NULL (CDR Cdr4890))
                            (IF (NULL Cdr4892)
                             (IF (EQUAL (CAR Cdr4890) Car4893)
                              (IF (EQ Car4894 'QUOTE)
                               (RETURN (CONS 'INCF Cdr4890)) (GO tag4877))
                              (GO tag4877))
                             (GO tag4877))
                            (GO tag4877))
                           (GO tag4877)))
                         tag4877
                         (IF (EQ '1- Car4889)
                          (IF (CONSP Cdr4890)
                           (IF (NULL (CDR Cdr4890))
                            (IF (NULL Cdr4892)
                             (IF (EQUAL (CAR Cdr4890) Car4893)
                              (IF (EQ Car4894 'QUOTE)
                               (RETURN (CONS 'DECF Cdr4890)) (GO tag4870))
                              (GO tag4870))
                             (GO tag4870))
                            (GO tag4870))
                           (GO tag4870))
                          (GO tag4870))))
                       (GO tag4870)))
                     (GO tag4870))
                    (GO tag4870)))
                  (GO tag4870)))
                (GO tag4870)))
              (GO tag4870)))
            tag4870
            (RETURN
             (LET ((NewF (shen-maplispsym V4815)))
              (LET ((Arity (trap-error (arity V4815) #'(LAMBDA (E) -1))))
               (IF
                (OR (= Arity (LENGTH V4816))
                 (= Arity -1))
                (CONS NewF V4816)
                (CONS 'shen-funcall
                 (CONS (shen-nest-lambda V4815 NewF)
                  (CONS (CONS 'LIST V4816) NIL)))))))))))))))))))

(DEFUN shen-maplispsym (V53)
 (COND ((EQ '= V53) 'equal?) 
       ((EQ '> V53) 'greater?)
       ((EQ '< V53) 'less?) 
       ((EQ '>= V53) 'greater-than-or-equal-to?)
       ((EQ '<= V53) 'less-than-or-equal-to?) 
       ((EQ '+ V53) 'add)
       ((EQ '- V53) 'subtract) 
       ((EQ '/ V53) 'divide) 
       ((EQ '* V53) 'multiply)
       (T V53)))
