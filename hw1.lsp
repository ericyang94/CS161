; PAD takes any single integer N and returns the Nth Padovan number using recursion
; PAD returns 1 if N == 1, 2, or 3

(defun PAD (N)
    (if (or (= N 0) (= N 1) (= N 2) )
      1
    (+ (PAD (- N 2)) (PAD (- N 3)))))

; SUMS takes any single integer N and returns the number of additions for
; PAD to compute the Nth Padovan number
; SUMS uses the same recursion as PAD but just counts each addition by adding 1

(defun SUMS (N)
    (if (or (= N 0) (= N 1) (= N 2) )
      0
    (+ (SUMS (- N 2)) (SUMS (- N 3)) 1)))

; ANON takes a single argument tree TREE and returns an anonymized tree with the
; same structure but all symbols and numbers are replaced with question marks

(defun ANON (btree)
  (cond
   ((null btree) nil)
   ((atom btree)
    (if (atom btree) '? btree))
   (t (mapcar #'(lambda (x)
                  (ANON x))
              btree))))
