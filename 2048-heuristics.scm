#!/usr/bin/env guile
!#

;;; 2048 Game with Basic Heuristics
;;; Based on "Composition of Basic Heuristics for the Game 2048"
;;; by Kohler, Migler & Khosmood (2019)

(use-modules (srfi srfi-1)   ; List operations
             (srfi srfi-43)  ; Vector operations  
             (ice-9 match)   ; Pattern matching
             (ice-9 format)  ; Formatted output
             (srfi srfi-11)) ; Let-values

;;; Board representation: 4x4 grid as vector of vectors
;;; 0 represents empty cells

(define (make-board)
  "Create empty 4x4 board"
  (vector (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)
          (vector 0 0 0 0)))

(define (board-ref board row col)
  "Get value at position"
  (vector-ref (vector-ref board row) col))

(define (board-set! board row col val)
  "Set value at position"
  (vector-set! (vector-ref board row) col val))

(define (board-copy board)
  "Deep copy of board"
  (vector-map (lambda (i row) (vector-copy row)) board))

(define (board->list board)
  "Convert board to list of lists"
  (vector->list (vector-map vector->list board)))

(define (print-board board)
  "Pretty print the board"
  (newline)
  (vector-for-each
   (lambda (i row)
     (vector-for-each
      (lambda (j val)
        (format #t "~4d " (if (zero? val) 0 val)))
      row)
     (newline))
   board)
  (newline))

;;; Core game mechanics

(define (slide-row row direction)
  "Slide and merge a single row. Direction: 'left or 'right"
  (let* ((vals (vector->list row))
         (non-zero (filter (lambda (x) (not (zero? x))) vals))
         (merged (merge-adjacent non-zero))
         (padded (case direction
                   ((left) (append merged (make-list (- 4 (length merged)) 0)))
                   ((right) (append (make-list (- 4 (length merged)) 0) merged)))))
    (list->vector padded)))

(define (merge-adjacent lst)
  "Merge adjacent equal values in list"
  (match lst
    (() '())
    ((x) (list x))
    ((x y . rest)
     (if (= x y)
         (cons (* 2 x) (merge-adjacent rest))
         (cons x (merge-adjacent (cons y rest)))))))

(define (move-board board direction)
  "Apply move in given direction, return new board and score"
  (let ((new-board (board-copy board))
        (score 0))
    (case direction
      ((left right)
       (do ((i 0 (+ i 1))) ((= i 4))
         (let* ((row (vector-ref new-board i))
                (old-row (vector-copy row))
                (new-row (slide-row row direction)))
           (vector-set! new-board i new-row)
           ;; Calculate score from merges
           (do ((j 0 (+ j 1))) ((= j 4))
             (let ((new-val (vector-ref new-row j)))
               (when (and (not (zero? new-val))
                         (not (member new-val (vector->list old-row))))
                 (set! score (+ score new-val))))))))
      
      ((up down)
       ;; Transpose, move, transpose back
       (let ((transposed (transpose-board new-board)))
         (do ((i 0 (+ i 1))) ((= i 4))
           (let* ((row (vector-ref transposed i))
                  (new-row (slide-row row (if (eq? direction 'up) 'left 'right))))
             (vector-set! transposed i new-row)))
         (set! new-board (transpose-board transposed)))))
    
    (values new-board score)))

(define (transpose-board board)
  "Transpose the board matrix"
  (let ((new-board (make-board)))
    (do ((i 0 (+ i 1))) ((= i 4))
      (do ((j 0 (+ j 1))) ((= j 4))
        (board-set! new-board j i (board-ref board i j))))
    new-board))

(define (board-equal? b1 b2)
  "Check if two boards are equal"
  (let ((result #t))
    (do ((i 0 (+ i 1))) ((or (= i 4) (not result)))
      (do ((j 0 (+ j 1))) ((or (= j 4) (not result)))
        (when (not (= (board-ref b1 i j) (board-ref b2 i j)))
          (set! result #f))))
    result))

(define (valid-move? board direction)
  "Check if move changes the board"
  (let-values (((new-board score) (move-board board direction)))
    (not (board-equal? board new-board))))

(define (get-valid-moves board)
  "Get list of valid moves"
  (filter (lambda (dir) (valid-move? board dir))
          '(up down left right)))

;;; Heuristic evaluators

(define (count-empty board)
  "Count empty cells (heuristic: maximize)"
  (let ((count 0))
    (vector-for-each
     (lambda (i row)
       (vector-for-each
        (lambda (j val)
          (when (zero? val)
            (set! count (+ count 1))))
        row))
     board)
    count))

(define (calculate-monotonicity board)
  "Calculate monotonicity score (higher is better)"
  (let ((score 0))
    ;; Check all 4 orientations
    (do ((rotation 0 (+ rotation 1))) ((= rotation 4))
      (let ((rotated (rotate-board board rotation))
            (corner-score 0))
        ;; Check rows
        (do ((row 0 (+ row 1))) ((= row 4))
          (do ((col 0 (+ col 1))) ((= col 3))
            (let ((v1 (board-ref rotated row col))
                  (v2 (board-ref rotated row (+ col 1))))
              (when (and (not (zero? v1)) (not (zero? v2)) (>= v1 v2))
                (set! corner-score (+ corner-score 1))))))
        ;; Check columns  
        (do ((col 0 (+ col 1))) ((= col 4))
          (do ((row 0 (+ row 1))) ((= row 3))
            (let ((v1 (board-ref rotated row col))
                  (v2 (board-ref rotated (+ row 1) col)))
              (when (and (not (zero? v1)) (not (zero? v2)) (>= v1 v2))
                (set! corner-score (+ corner-score 1))))))
        (set! score (max score corner-score))))
    score))

(define (rotate-board board times)
  "Rotate board 90 degrees clockwise 'times' times"
  (if (zero? times)
      board
      (rotate-board (rotate-90 board) (- times 1))))

(define (rotate-90 board)
  "Rotate board 90 degrees clockwise"
  (let ((new-board (make-board)))
    (do ((i 0 (+ i 1))) ((= i 4))
      (do ((j 0 (+ j 1))) ((= j 4))
        (board-set! new-board j (- 3 i) (board-ref board i j))))
    new-board))

(define (calculate-uniformity board)
  "Calculate uniformity score (sum of cubes of tile counts)"
  (let ((counts (make-hash-table)))
    (vector-for-each
     (lambda (i row)
       (vector-for-each
        (lambda (j val)
          (unless (zero? val)
            (hash-set! counts val
                      (+ 1 (hash-ref counts val 0)))))
        row))
     board)
    (hash-fold (lambda (key count sum)
                 (+ sum (expt count 3)))
               0 counts)))

;;; Strategy composition

(define (evaluate-move board move evaluator)
  "Evaluate a move using given heuristic"
  (let-values (((new-board score) (move-board board move)))
    (case evaluator
      ((empty) (count-empty new-board))
      ((monotonicity) (calculate-monotonicity new-board))
      ((uniformity) (calculate-uniformity new-board))
      ((greedy) score)
      (else 0))))

(define (apply-strategy board strategy)
  "Apply strategy (list of evaluators) to choose best move"
  (let ((moves (get-valid-moves board)))
    (if (null? moves)
        #f  ; Game over
        (let loop ((evaluators strategy)
                   (candidates moves))
          (if (or (null? (cdr evaluators)) (= 1 (length candidates)))
              ;; Terminal evaluator or single candidate
              (if (eq? (car evaluators) 'random)
                  (list-ref candidates (random (length candidates)))
                  (car candidates))
              ;; Apply current evaluator
              (let* ((evaluator (car evaluators))
                     (scores (map (lambda (move)
                                   (cons move (evaluate-move board move evaluator)))
                                 candidates))
                     (best-score (apply max (map cdr scores)))
                     (best-moves (map car (filter (lambda (p) (= (cdr p) best-score))
                                                 scores))))
                (loop (cdr evaluators) best-moves)))))))

;;; Spawn new tiles

(define (get-empty-cells board)
  "Get list of empty cell positions"
  (let ((empty '()))
    (do ((i 0 (+ i 1))) ((= i 4))
      (do ((j 0 (+ j 1))) ((= j 4))
        (when (zero? (board-ref board i j))
          (set! empty (cons (cons i j) empty)))))
    empty))

(define (add-random-tile! board)
  "Add a random 2 (90%) or 4 (10%) to empty cell"
  (let ((empty (get-empty-cells board)))
    (unless (null? empty)
      (let* ((pos (list-ref empty (random (length empty))))
             (val (if (< (random 10) 9) 2 4)))
        (board-set! board (car pos) (cdr pos) val)))))

;;; Game runner

(define (play-game strategy max-moves)
  "Play a game using given strategy"
  (let ((board (make-board))
        (score 0)
        (moves 0))
    ;; Initial tiles
    (add-random-tile! board)
    (add-random-tile! board)
    
    (let loop ()
      (when (< moves max-moves)
        (print-board board)
        (format #t "Score: ~a  Moves: ~a~%" score moves)
        
        (let ((move (apply-strategy board strategy)))
          (if move
              (begin
                (let-values (((new-board move-score) (move-board board move)))
                  (set! board new-board)
                  (set! score (+ score move-score))
                  (set! moves (+ moves 1)))
                (add-random-tile! board)
                (loop))
              (begin
                (format #t "Game Over! Final Score: ~a~%" score)
                score)))))))

;;; Example usage

(define (demo)
  "Demo the best strategies from the paper"
  (format #t "Testing EMR strategy (Empty, Monotonicity, Random)~%")
  (format #t "========================================~%")
  (play-game '(empty monotonicity random) 100)
  
  ;; Uncomment to test other strategies:
  ;; (play-game '(greedy empty monotonicity random) 100)
  ;; (play-game '(monotonicity random) 100)
  )

;; Run demo only if this script is executed directly (not loaded)
(when (and (> (length (command-line)) 0) 
           (string-suffix? "2048-heuristics.scm" (car (command-line))))
  (demo))
