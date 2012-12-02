(DEFUN shen-pvar? (X) (IF (AND (ARRAYP X) (NOT (STRINGP X)) (EQ (AREF X 0) 'shen-pvar))
                          'true
                          'false))

(DEFUN shen-lazyderef (X ProcessN)
   (IF (AND (ARRAYP X) (NOT (STRINGP X)) (EQ (AREF X 0) 'shen-pvar))
       (LET ((Value (shen-valvector X ProcessN)))
            (IF (EQ Value 'shen--null-)
                X
                (shen-lazyderef Value ProcessN)))
       X))
                                    
(DEFUN shen-valvector (Var ProcessN)
  (AREF (AREF shen-*prologvectors* ProcessN) (AREF Var 1)))               

(DEFUN shen-unbindv (Var N)
  (LET ((Vector (AREF shen-*prologvectors* N)))
       (SETF (AREF Vector (AREF Var 1)) 'shen--null-)))

(DEFUN shen-bindv (Var Val N)
   (LET ((Vector (AREF shen-*prologvectors* N)))
        (SETF (AREF Vector (AREF Var 1)) Val)))

(DEFUN shen-copy-vector-stage-1 (V2828 V2829 V2830 V2831)
 (COND ((= V2831 V2828) V2830)
  (T
   (shen-copy-vector-stage-1 (1+ V2828) V2829
    (address-> V2830 V2828 (<-address V2829 V2828)) V2831))))

(DEFUN shen-copy-vector-stage-2 (V2835 V2836 V2837 V2838)
 (COND ((= V2836 V2835) V2838)
  (T
   (shen-copy-vector-stage-2 (1+ V2835) V2836 V2837
    (address-> V2838 V2835 V2837)))))

(DEFUN shen-newpv (N)
  (LET ((Count+1 (1+ (THE INTEGER (AREF shen-*varcounter* N))))
        (Vector (AREF shen-*prologvectors* N)))
       (SETF (AREF shen-*varcounter* N) Count+1)
       (IF (= (THE INTEGER Count+1) (THE INTEGER (limit Vector))) 
           (shen-resizeprocessvector N Count+1)
           'skip)
       (shen-mk-pvar Count+1)))

(DEFUN vector-> (Vector N X)
  (IF (ZEROP N) 
      (ERROR "cannot access 0th element of a vector~%")
      (address-> Vector N X)))

(DEFUN <-vector (Vector N)
  (IF (ZEROP N) 
      (ERROR "cannot access 0th element of a vector~%")
       (let VectorElement (AREF Vector N)
          (IF (EQ VectorElement (fail))
              (ERROR "vector element not found~%")
              VectorElement))))

(DEFUN variable? (X)
 (IF (AND (SYMBOLP X) (NOT (NULL X)) (UPPER-CASE-P (CHAR (SYMBOL-NAME X) 0)))
     'true 
     'false))

(DEFUN shen-+string? (X) (IF (AND (STRINGP X) (NOT (STRING-EQUAL X "")))
                            'true
                            'false))

(DEFUN thaw (F) (FUNCALL F))