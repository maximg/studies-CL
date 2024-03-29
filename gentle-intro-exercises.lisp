
(setf nerd-states
      '((sleeping                   . eating)
        (eating                     . waiting-for-a-computer)
        (waiting-for-a-computer     . programming)
        (programming                . debugging)
        (debugging                  . sleeping)))

(defun nerdus (state)
  (cdr (assoc state nerd-states)))

(defun sleepless-nerd (state)
  (cond ((equal state 'debugging) 'eating)
        (T (nerdus state))))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

;;; destructively chop everything after the first element
(defun chop (x)
  (when x
    (setf (cdr x) nil))
  x)

(defun check-all-odd (x)
  (do ((y x (cdr y)))
      ((null y) T)
    (when (evenp (first y))
      (return nil))))

(defun find-largest (list-of-int)
  (do* ((y list-of-int (rest y))
        (element (first y) (first y))
        (largest element))
      ((null y) largest)
    (when (> element largest)
      (setf largest element))))

;;; chapter 11

(defun complement-base (x)
  (rest (assoc x '((a . t)
                   (t . a)
                   (g . c)
                   (c . g)))))

(defun complement-strand (strand)
  (do* ((x strand (rest x))
        (base (first x) (first x))
        (result nil))
      ((null x) (nreverse result))
    (setf result (cons (complement-base base) result))))

(defun make-double (strand)
  (do* ((x strand (rest x))
        (base (first x)(first x))
        (result nil))
      ((null x) (reverse result))
    (setf result (cons (list base (complement-base base)) result))))

;; bug: does not reset counts on successive calls
(defun count-bases (dna)
  (do* ((result '((a . 0)
                  (t . 0)
                  (g . 0)
                  (c . 0)))
        (y dna (rest y))
        (elem (first y) (first y)))
      ((null y) result)
    (labels ((count-base (base)
               (incf (cdr (assoc base result)))))
      (cond ((atom elem) (count-base elem))
            (t (count-base (first elem))
              (count-base (second elem)))))))

(defmacro set-nil (var)
  `(setq ,var nil))

(defmacro simple-rotatef (x y)
  `(let ((x-val ,x) (y-val ,y))
    (setq ,x y-val)
    (setq ,y x-val)))
