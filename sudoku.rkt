#lang racket

;--------------------------------------------------------------------------------------------
;--------------------------------------PROGRAM-----------------------------------------------
;--------------------------------------------------------------------------------------------

(define (my-append lst x)                    ; took from proseminares
  (if (null? lst)
      (cons x null)
      (cons (car lst) (my-append (cdr lst) x))))

;--------------------------------------------------------------------------------------------
(define (rem-first-column b res)             ;remove first column
  (if (null? b)
      res
      (rem-first-column (cdr b) (my-append res (cdr (car b))))))

;--------------------------------------------------------------------------------------------
(define (set-elem matrix y x num)           ; change element in matrix at given location 
  (for/list ([row matrix] [i (length matrix)])       ; internet helped with for/list
    (for/list ([original row] [j (length row)])
      (if (and (= x i) (= y j))             
          num
          original))))

;--------------------------------------------------------------------------------------------
(define (can-row? matrix row num)            ; check if 'num' can be in given row
  (if (zero? row)
      (if (null? matrix)
          #t
          (if (list? (car matrix))
              (if (= num (caar matrix))
                  #f
                  (can-row? (cdr (car matrix)) 0 num))
              (if (= num (car matrix))
                  #f
                  (can-row? (cdr matrix) 0 num))))
      (if (= 1 row)
          (can-row? (cadr matrix) 0 num)
          (can-row? (cdr matrix) (- row 1) num))))
  
;--------------------------------------------------------------------------------------------
(define (can-col? matrix column num)        ; check if 'num' can be in given column
  (cond
    ((not (zero? column))  (can-col? (rem-first-column matrix '()) (- column 1) num))
    ((null? matrix) #t)
    ((= num (car (car matrix))) #f)
    (#t (can-col? (cdr matrix) 0 num))))

;--------------------------------------------------------------------------------------------
(define (is-in-line? line num acum)         ; check if 'num' is in first 3 elements of line
  (cond
    ((zero? acum) #f)
    ((= (car line) num) #t)
    (#t (is-in-line? (cdr line) num (- acum 1)))))

;--------------------------------------------------------------------------------------------
(define (is-in-square? matrix num acum)     ; check if 'num' is 3x3 square from [0, 0]
  (cond
    ((zero? acum) #f)
    ((is-in-line? (car matrix) num 3) #t)
    (#t (is-in-square? (cdr matrix) num (- acum 1)))))
      
;--------------------------------------------------------------------------------------------
(define (can-be-in-squere? matrix row col num)  ; place 3x3 square at position (col, row) into front of matrix 
  (if (zero? row)                               ; so i can easily use is-in-square?
      (if (zero? col)
          (not (is-in-square? matrix num 3))
          (can-be-in-squere? (rem-first-column matrix '()) 0 (- col 1) num))
      (can-be-in-squere? (cdr matrix) (- row 1) col num)))

;--------------------------------------------------------------------------------------------
(define (possible? matrix row column num)    ; returns if num can be placed in given position
  (and
   (can-row? matrix row num)
   (and
    (can-col? matrix column num)
    (can-be-in-squere? matrix (* (exact-floor (/ row 3)) 3) (* (exact-floor (/ column 3)) 3) num))))

;--------------------------------------------------------------------------------------------
(define (index matrix row col)               ; returns element in matrix at given position
  (if (zero? row)
      (if (zero? col)
          (if (list? (car matrix))
              (caar matrix)
              (car matrix))
          (if (list? (car matrix))
              (index (cdar matrix) 0 (- col 1))
              (index (cdr matrix) 0 (- col 1))))
      (if (= 1 row)
           (index (cadr matrix) 0 col)
           (index (cdr matrix) (- row 1) col))))

;--------------------------------------------------------------------------------------------
(define (fill-sudoku solved row column tmp)   ; try every possible option, tmp -> number that is being tried to be filled in sudoku if possible
  (cond
    ((= 9 row) solved) ; success
    ((= 9 column) (fill-sudoku solved (+ 1 row) 0 1)) ; end of row 
    ((not (zero? (index solved row column))) (fill-sudoku solved row (+ 1 column) 1)) ; already filled
    ((= 10 tmp) #f)
    ((possible? solved row column tmp)
                (let ((predict (fill-sudoku (set-elem solved column row tmp) row (+ 1 column) 1)))
                        (if predict
                            predict
                            (fill-sudoku solved row column (+ 1 tmp)))))
    (#t  (fill-sudoku solved row column (+ 1 tmp)))))
;--------------------------------------------------------------------------------------------
(define (check-elems line)                   ; check every element 
  (cond
    ((null? line) #t)
    ((not (number? (car line))) #f)
    ((< (car line) 0 ) #f)
    ((< 9 (car line)) #f)
    (#t (check-elems (cdr line)))))


;--------------------------------------------------------------------------------------------
(define (check-input matrix acum)           ; check whole matrix 
  (cond
        ((and (null? matrix) (= acum 9)) #t)    ;good
        ((null? matrix) #f)
        ((not (= 9 (length (car matrix)))) #f)
        ((not (check-elems (car matrix))) #f)
        (#t (check-input (cdr matrix) (+ 1 acum)))))
        

;--------------------------------------------------------------------------------------------
(define (solve-sudoku matrix)                ; start at (0, 0) with first possible number 1
  (if (not (check-input matrix 0))
      #f
      (fill-sudoku matrix 0 0 1)))
