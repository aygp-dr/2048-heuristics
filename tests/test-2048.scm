#!/usr/bin/env guile3
!#

;;; Unit tests for 2048 Heuristics
;;; Uses SRFI-64 for testing framework

(use-modules (srfi srfi-64)  ; Test framework
             (srfi srfi-1)   ; List operations
             (srfi srfi-43)  ; Vector operations
             (ice-9 match))  ; Pattern matching

;; Load the main program
(load "../2048-heuristics.scm")

;; Configure test suite
(test-begin "2048-heuristics")

;;; Board creation and manipulation tests

(test-group "board-creation"
  (let ((board (make-board)))
    (test-assert "Board is a vector" (vector? board))
    (test-eqv "Board has 4 rows" 4 (vector-length board))
    (test-assert "Each row is a vector" 
                 (vector-every vector? board))
    (test-eqv "Each row has 4 columns" 4 
              (vector-length (vector-ref board 0)))
    (test-assert "All cells are initially 0"
                 (vector-every 
                  (lambda (i row) 
                    (vector-every zero? row))
                  board))))

(test-group "board-access"
  (let ((board (make-board)))
    (board-set! board 1 2 8)
    (test-eqv "Can set and get cell value" 8 (board-ref board 1 2))
    (test-eqv "Other cells remain 0" 0 (board-ref board 0 0))))

(test-group "board-copy"
  (let* ((board (make-board))
         (copy-board #f))
    (board-set! board 1 2 8)
    (set! copy-board (board-copy board))
    (test-assert "Copied board is equal" (board-equal? board copy-board))
    (test-assert "But not the same object" (not (eq? board copy-board)))
    (board-set! copy-board 0 0 4)
    (test-assert "Changes to copy don't affect original" 
                 (zero? (board-ref board 0 0)))))

;;; Core game mechanics tests

(test-group "slide-row"
  (let ((row (vector 0 2 2 0)))
    (test-equal "Slide left" #(2 2 0 0) (slide-row row 'left))
    (test-equal "Slide right" #(0 0 2 2) (slide-row row 'right))))

(test-group "merge-adjacent"
  (test-equal "Empty list" '() (merge-adjacent '()))
  (test-equal "Single value" '(2) (merge-adjacent '(2)))
  (test-equal "Equal values" '(4) (merge-adjacent '(2 2)))
  (test-equal "Non-equal values" '(2 4) (merge-adjacent '(2 4)))
  (test-equal "Multiple pairs" '(4 8) (merge-adjacent '(2 2 4 4)))
  (test-equal "Mixed values" '(2 8 4) (merge-adjacent '(2 4 4 4))))

(test-group "move-board"
  (let ((board (make-board)))
    ;; Set up test board
    ;; 0 2 0 0
    ;; 0 2 0 0
    ;; 0 0 0 0
    ;; 0 0 0 0
    (board-set! board 0 1 2)
    (board-set! board 1 1 2)
    
    ;; Test left move
    (let-values (((new-board score) (move-board board 'left)))
      (test-eqv "Left move shifts values" 2 (board-ref new-board 0 0))
      (test-eqv "Left move shifts values" 2 (board-ref new-board 1 0)))
    
    ;; Test right move
    (let-values (((new-board score) (move-board board 'right)))
      (test-eqv "Right move shifts values" 2 (board-ref new-board 0 3))
      (test-eqv "Right move shifts values" 2 (board-ref new-board 1 3)))
    
    ;; Test up move
    (let-values (((new-board score) (move-board board 'up)))
      (test-eqv "Up move shifts values" 4 (board-ref new-board 0 1))
      (test-eqv "Up move merges equal values" 0 (board-ref new-board 1 1)))
    
    ;; Test down move
    (let-values (((new-board score) (move-board board 'down)))
      (test-eqv "Down move shifts values" 4 (board-ref new-board 3 1))
      (test-eqv "Down move merges equal values" 0 (board-ref new-board 0 1)))))

(test-group "valid-move"
  (let ((board (make-board)))
    ;; Empty board has no valid moves
    (test-assert "Empty board has no valid moves"
                 (null? (get-valid-moves board)))
    
    ;; Add a single tile
    (board-set! board 0 0 2)
    (test-assert "Board with single tile has right and down moves"
                 (lset= eq? (get-valid-moves board) '(right down)))
    
    ;; Add a second tile
    (board-set! board 3 3 2)
    (test-assert "Board with two tiles has all moves"
                 (lset= eq? (get-valid-moves board) '(up down left right)))))

;;; Heuristic evaluator tests

(test-group "count-empty"
  (let ((board (make-board)))
    (test-eqv "Empty board has 16 empty cells" 16 (count-empty board))
    (board-set! board 0 0 2)
    (test-eqv "Board with one tile has 15 empty cells" 15 (count-empty board))))

(test-group "calculate-monotonicity"
  (let ((board (make-board)))
    ;; Create a monotonic board (values decrease from top-left)
    (board-set! board 0 0 16)
    (board-set! board 0 1 8)
    (board-set! board 1 0 8)
    (board-set! board 1 1 4)
    
    (test-assert "Monotonic board has good score" 
                 (> (calculate-monotonicity board) 0))))

(test-group "calculate-uniformity"
  (let ((board (make-board)))
    ;; Board with all different values
    (board-set! board 0 0 2)
    (board-set! board 0 1 4)
    (board-set! board 1 0 8)
    (board-set! board 1 1 16)
    (let ((diverse-score (calculate-uniformity board)))
      
      ;; Board with uniform values
      (board-set! board 0 0 2)
      (board-set! board 0 1 2)
      (board-set! board 1 0 2)
      (board-set! board 1 1 2)
      (let ((uniform-score (calculate-uniformity board)))
        (test-assert "Uniform board scores higher than diverse board"
                     (> uniform-score diverse-score))))))

;;; Strategy tests

(test-group "evaluate-move"
  (let ((board (make-board)))
    ;; Setup board for testing
    (board-set! board 0 0 2)
    (board-set! board 0 1 2)
    
    (test-assert "Empty evaluator works"
                 (number? (evaluate-move board 'left 'empty)))
    (test-assert "Monotonicity evaluator works"
                 (number? (evaluate-move board 'left 'monotonicity)))
    (test-assert "Uniformity evaluator works"
                 (number? (evaluate-move board 'left 'uniformity)))
    (test-assert "Greedy evaluator works"
                 (number? (evaluate-move board 'left 'greedy)))))

(test-group "apply-strategy"
  (let ((board (make-board)))
    ;; Setup board for testing
    (board-set! board 0 0 2)
    (board-set! board 0 1 2)
    
    (test-assert "EMR strategy returns valid move"
                 (member (apply-strategy board '(empty monotonicity random))
                         '(up down left right)))))

;;; Random tile tests

(test-group "add-random-tile"
  (let ((board (make-board)))
    (add-random-tile! board)
    (test-assert "Random tile was added"
                 (not (= 16 (count-empty board))))
    
    (let ((empty-before (count-empty board)))
      (add-random-tile! board)
      (test-eqv "Another random tile was added"
                (- empty-before 1) (count-empty board)))))

;; Finish test suite
(test-end "2048-heuristics")

;; Display test summary
(test-exit)