;;;
;;; sudoku.scm:
;;; A really basic sudoku solver. :-)
;;;
(define mensa-jun13-puzzle #(7 0 5 0 0 4 0 0 9
                             0 3 0 0 0 0 2 0 0
                             0 1 8 0 9 0 0 0 0
                             8 0 0 9 0 0 0 2 0
                             1 0 3 4 0 6 7 0 8
                             0 7 0 0 0 3 0 0 6
                             0 0 0 0 3 0 4 6 0
                             0 0 1 0 0 0 0 9 0
                             6 0 0 8 0 0 5 0 1))

(define GRID_WIDTH 9)
(define GRID_HEIGHT 9)
(define GRID_AREA (* GRID_WIDTH GRID_HEIGHT))

(define incomplete-cells
  (lambda (grid)
    (let next ((index 0) (cur '()))
      (if (= index GRID_AREA)
          cur
          (next (+ index 1)
                (if (= (vector-ref grid index) 0)
                    (cons index cur)
                    cur))))))

(define-syntax when
  (syntax-rules ()
    ((_ pred body1 body2 ...)
      (if pred
          (begin
            body1
            body2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ pred body1 body2 ...)
     (if (not pred)
         (begin
           body1
           body2 ...)))))

(define-syntax memo-define
  (syntax-rules ()
    ((_ name fn)
     (define name
       (let ((memory (make-hash-table)))
         (lambda (n)
           (or (hash-ref memory n))
               (hash-set! memory n (fn n))))))))

(memo-define vertical-neighbours
  (lambda (rel)
    (let next ((n (modulo rel GRID_WIDTH))
               (cur '()))
               (if (< n GRID_AREA)
                   (next (+ n GRID_WIDTH)
                         (if (= n rel)
                             cur
                             (cons n cur)))
                   cur))))

(memo-define horizontal-neighbours
  (lambda (rel)
    (let ((lim (+ (* (truncate (/ rel GRID_WIDTH)) GRID_WIDTH) GRID_WIDTH)))
      (let next ((n (* (truncate (/ rel GRID_WIDTH)) GRID_WIDTH))
                 (cur '()))
        (if (< n lim)
            (next (+ n 1)
                  (if (= n rel)
                      cur
                      (cons n cur)))
            cur)))))

(memo-define adjacent-neighbours
  (lambda (rel)
    (let* ((startx (* (truncate (/ (modulo rel GRID_WIDTH) 3)) 3))
           (starty (* (truncate (/ rel (* GRID_WIDTH 3))) 3))
           (endx (+ startx 3))
           (endy (+ starty 3)))
      (let next-y ((y starty))
        (if (< y endy)
            (append (let next-x ((x startx) (cur '()))
                      (if (< x endx)
                          (let ((n (+ (* y GRID_WIDTH) x)))
                            (next-x (+ x 1)
                                    (if (= n rel)
                                        cur
                                        (cons n cur))))
                          cur))
                    (next-y (+ y 1)))
            '())))))

(define do-cells
  (lambda (grid cells fn)
    (unless (null? cells)
      (fn (vector-ref grid (car cells)))
      (do-cells grid (cdr cells) fn))))

(define possible-values
  (lambda (grid n)
    (let ((possible (make-vector 9 #t)))
      (do-cells grid
                (append (vertical-neighbours n)
                        (horizontal-neighbours n)
                        (adjacent-neighbours n))
                (lambda (val)
                  (unless (= val 0)
                    (vector-set! possible (- val 1) #f))))
      (let next ((vals (vector->list possible))
                 (n 1)
                 (res '()))
        (if (null? vals)
            res
            (next (cdr vals)
                  (+ n 1)
                  (if (car vals)
                      (cons n res)
                      res)))))))

(define options
  (lambda (grid)
    (map (lambda (cell)
           (list cell (possible-values grid cell)))
         (incomplete-cells grid))))

(define simplest-options
  (lambda (grid)
    (sort (options grid)
          (lambda (a b)
            (< (length (cadr a))
               (length (cadr b)))))))

(define try-solve
  (lambda (grid pos val)
    (let ((newgrid (vector-copy grid)))
      (vector-set! newgrid pos val)
      (solve newgrid))))

(define solve
  (lambda (grid)
    (let ((opts (simplest-options grid)))
      (if (null? opts)
          grid
          (let* ((first-opt-cell (caar opts))
                 (first-opt-vals (cadar opts))
                 (first-opt-nvals (length first-opt-vals)))
            (cond ((= first-opt-nvals 0) #f)
                  ((= first-opt-nvals 1)
                   (try-solve grid
                              first-opt-cell
                              (car first-opt-vals)))
                  (else
                   (let try-next ((try-vals first-opt-vals))
                     (if (null? try-vals)
                         #f
                         (or (try-solve grid first-opt-cell (car try-vals))
                             (try-next (cdr try-vals))))))))))))

(define display-puzzle
  (lambda (puzzle)
    (let next ((n 0))
      (if (< n GRID_AREA)
          (begin
            (when (> n 0)
              (when (= (modulo n 3) 0)
                (display " "))
              (when (= (modulo n 9) 0)
                (newline))
              (when (= (modulo n 27) 0)
                (newline)))
            (display " ")
            (display (vector-ref puzzle n))
            (next (+ n 1)))
          (newline)))))

(display-puzzle (solve mensa-jun13-puzzle))
