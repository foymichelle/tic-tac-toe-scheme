#lang racket

;; Display board fns
(define init-board 
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

(define (char->str char)
  (cond
    ((number? char) (number->string char))
    ((symbol? char) (symbol->string char))
    (else "")))

(define (display-cell cell)
  (string-append " " (char->str cell) " "))

(define (display-row row)
  (string-append* (add-between (map display-cell row) "|")))

(define (display-board board)
  (begin
    (newline)
    (display (string-append* (add-between (map display-row board) "\n---+---+---\n")))
    (newline)))

;; Make a move on board fns
(define (valid-move? cell board)
  (let ((valid-cells (free-cells board)))
    (cond
      ((not (number? cell)) #f)
      ((not (member cell valid-cells)) #f)
      (else #t))))

(define (replace-row token cell row)
  (cond
    ((null? row) '())
    ((eq? cell (car row)) (cons token (cdr row)))
    (else (cons (car row) (replace-row token cell (cdr row))))))

(define (make-move token cell board)
  (if (valid-move? cell board)
      (map (lambda (row) (replace-row token cell row)) board)
      '()))

;; Player interaction fns
(define (free-cell? cell board)
  (let ((member-space (lambda (row) (member cell row))))
    (ormap member-space board)))

(define (select-cell)
  (begin
    (display "Select a cell (1-9): ")
    (string->number (read-line))))

(define (x-play board)
  (let ((answer (select-cell)))
    (make-move 'x answer board)))

;; Computer play fns
(define (free-cells board)
  (let ((board-list (flatten board)))
    (if (null? board-list)
      '()
      (if (number? (car board-list))
          (cons (car board-list) (free-cells (cdr board-list)))
          (free-cells (cdr board-list))))))

(define (choose-random board)
  (let ((cells (free-cells board)))
    (list-ref cells (random (length cells)))))
    
(define (next-move-winner token board)
  (filter (lambda (pos) (if (winner? (make-move token pos board)) #t #f)) (free-cells board)))

(define (o-play board)
  (let ((check-x-win (next-move-winner 'x board))
        (check-o-win (next-move-winner 'o board)))
    (cond
      ((not (empty? check-o-win)) (make-move 'o (car check-o-win) board))
      ((not (empty? check-x-win)) (make-move 'o (car check-x-win) board))
      (else (make-move 'o (choose-random board) board)))))

;; Check winner fns
(define (all-equal? seq)
  (foldl (lambda (x y) (if (eq? x y) x #f)) (car seq) (cdr seq))) 

(define (winner? board)
  (or
   (all-equal? (first board))
   (all-equal? (second board))
   (all-equal? (third board))
   (all-equal? (map first board))
   (all-equal? (map second board))
   (all-equal? (map third board))
   (all-equal? (list (first (first board))
                           (second (second board)) 
                           (third (third board))))
   (all-equal? (list (third (first board)) 
                           (second (second board)) 
                           (first (third board))))))

;; Check for draw
(define (draw? board)
  (empty? (free-cells board)))

;; Main fn
(define (start)
  (begin
    (display "Welcome to Tic Tac Toe! You are player X.")
    (play init-board)))

(define (play board)
  (begin
    (display-board board)
    (let ((x-board (x-play board)))
      (cond
        ((empty? x-board)
         (begin (display "Invalid input. Try again.")
                (play board)))
        ((winner? x-board)
         (begin
           (display-board x-board)
           (display "X wins!")))
        ((draw? x-board)
         (begin
           (display-board x-board)
           (display "It's a draw!")))
        (else (let ((o-board (o-play x-board)))
                (cond
                  ((winner? o-board)
                   (begin
                     (display-board o-board)
                     (display "O wins!")))
                  ((draw? o-board)
                   (begin
                     (display-board o-board)
                     (display "It's a draw!")))
                  (else (play o-board)))))))))