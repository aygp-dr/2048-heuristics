#!/usr/bin/env guile
!#

;;; 2048 Interactive Strategy Comparison
;;; Allows interactive comparison of different strategies

(use-modules (ice-9 format)
             (ice-9 readline)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-11))

;; Load the main game implementation
(load "2048-heuristics.scm")

;;; Interactive comparison functionality

(define available-strategies
  '(("1" . (random))
    ("2" . (empty random))
    ("3" . (monotonicity random))
    ("4" . (empty monotonicity random))
    ("5" . (greedy empty monotonicity random))
    ("6" . (uniformity random))
    ("7" . (greedy random))
    ("C" . custom)))

(define (get-strategy-name strategy)
  "Get a descriptive name for a strategy"
  (cond
   ((equal? strategy '(random)) "Random")
   ((equal? strategy '(empty random)) "Empty → Random (ER)")
   ((equal? strategy '(monotonicity random)) "Monotonicity → Random (MR)")
   ((equal? strategy '(empty monotonicity random)) "Empty → Monotonicity → Random (EMR)")
   ((equal? strategy '(greedy empty monotonicity random)) "Greedy → Empty → Monotonicity → Random (GEMR)")
   ((equal? strategy '(uniformity random)) "Uniformity → Random (UR)")
   ((equal? strategy '(greedy random)) "Greedy → Random (GR)")
   (else (format #f "Custom: ~a" strategy))))

(define (display-menu)
  "Display the strategy selection menu"
  (format #t "~%2048 Strategy Comparison~%")
  (format #t "======================~%~%")
  (format #t "Available strategies:~%")
  (format #t "  1) Random~%")
  (format #t "  2) Empty → Random (ER)~%")
  (format #t "  3) Monotonicity → Random (MR)~%")
  (format #t "  4) Empty → Monotonicity → Random (EMR)~%")
  (format #t "  5) Greedy → Empty → Monotonicity → Random (GEMR)~%")
  (format #t "  6) Uniformity → Random (UR)~%")
  (format #t "  7) Greedy → Random (GR)~%")
  (format #t "  C) Custom Strategy~%")
  (format #t "~%")
  (format #t "Commands:~%")
  (format #t "  R) Run comparison between strategies~%")
  (format #t "  Q) Quit~%")
  (format #t "~%"))

(define (prompt-for-strategy)
  "Prompt user to select a strategy"
  (let ((choice (read-line)))
    (cond
     ((eof-object? choice) #f)
     ((string-ci=? choice "q") #f)
     ((string-ci=? choice "c")
      (format #t "Enter custom strategy (e.g., empty monotonicity random): ")
      (let ((custom-input (read-line)))
        (if (eof-object? custom-input)
            #f
            (let ((parts (string-split custom-input #\space)))
              (map string->symbol parts)))))
     (else
      (let ((strategy (assoc-ref available-strategies choice)))
        (if strategy
            (if (eq? strategy 'custom)
                (begin
                  (format #t "Enter custom strategy (e.g., empty monotonicity random): ")
                  (let ((custom-input (read-line)))
                    (if (eof-object? custom-input)
                        #f
                        (let ((parts (string-split custom-input #\space)))
                          (map string->symbol parts)))))
                strategy)
            (begin
              (format #t "Invalid choice. Please try again.~%")
              (prompt-for-strategy))))))))

(define (select-strategies)
  "Select strategies to compare"
  (format #t "~%Select first strategy (1-7, C, or Q to quit): ")
  (let ((strategy1 (prompt-for-strategy)))
    (if strategy1
        (begin
          (format #t "Selected: ~a~%" (get-strategy-name strategy1))
          (format #t "~%Select second strategy (1-7, C, or Q to quit): ")
          (let ((strategy2 (prompt-for-strategy)))
            (if strategy2
                (begin
                  (format #t "Selected: ~a~%" (get-strategy-name strategy2))
                  (cons strategy1 strategy2))
                #f)))
        #f)))

(define (run-visible-game strategy max-moves)
  "Run a game with visible output, return final score and max tile"
  (let ((board (make-board))
        (score 0)
        (moves 0))
    ;; Initial tiles
    (add-random-tile! board)
    (add-random-tile! board)
    
    (format #t "~%Starting game with strategy: ~a~%" 
            (get-strategy-name strategy))
    (format #t "=============================================~%")
    
    (let loop ()
      (print-board board)
      (format #t "Score: ~a  Moves: ~a~%" score moves)
      (format #t "Strategy: ~a~%" (get-strategy-name strategy))
      
      (format #t "Press Enter to continue, 'q' to quit: ")
      (let ((input (read-line)))
        (cond
         ((or (eof-object? input) (string-ci=? input "q"))
          (values score (find-max-tile board) moves))
         (else
          (if (or (>= moves max-moves)
                  (null? (get-valid-moves board)))
              (begin
                (format #t "~%Game over!~%")
                (values score (find-max-tile board) moves))
              (let ((move (apply-strategy board strategy)))
                (if move
                    (begin
                      (format #t "Selected move: ~a~%" move)
                      (let-values (((new-board move-score) (move-board board move)))
                        (set! board new-board)
                        (set! score (+ score move-score))
                        (set! moves (+ moves 1)))
                      (add-random-tile! board)
                      (loop))
                    (begin
                      (format #t "~%Game over! No valid moves.~%")
                      (values score (find-max-tile board) moves))))))))))

(define (find-max-tile board)
  "Find the maximum tile value on the board"
  (let ((max-val 0))
    (vector-for-each
     (lambda (i row)
       (vector-for-each
        (lambda (j val)
          (when (> val max-val)
            (set! max-val val)))
        row))
     board)
    max-val))

(define (compare-strategies-interactive strategy1 strategy2)
  "Run interactive comparison between two strategies"
  (format #t "~%Comparing strategies:~%")
  (format #t "  1. ~a~%" (get-strategy-name strategy1))
  (format #t "  2. ~a~%" (get-strategy-name strategy2))
  (format #t "~%")
  
  ;; First strategy
  (format #t "Running first strategy...~%")
  (let-values (((score1 max-tile1 moves1) 
                (run-visible-game strategy1 2000)))
    
    ;; Second strategy
    (format #t "~%Running second strategy...~%")
    (let-values (((score2 max-tile2 moves2) 
                  (run-visible-game strategy2 2000)))
      
      ;; Print comparison
      (format #t "~%~%Results:~%")
      (format #t "========~%~%")
      (format #t "Strategy             | Score | Max Tile | Moves~%")
      (format #t "----------------------|-------|----------|------~%")
      (format #t "~22a | ~5d | ~8d | ~5d~%" 
              (get-strategy-name strategy1) score1 max-tile1 moves1)
      (format #t "~22a | ~5d | ~8d | ~5d~%" 
              (get-strategy-name strategy2) score2 max-tile2 moves2)
      (format #t "~%"))))

(define (main-loop)
  "Main interactive loop"
  (let loop ()
    (display-menu)
    (format #t "Enter command (R to run, Q to quit): ")
    (let ((command (read-line)))
      (cond
       ((eof-object? command) 'done)
       ((string-ci=? command "q") 'done)
       ((string-ci=? command "r")
        (let ((strategies (select-strategies)))
          (if strategies
              (compare-strategies-interactive (car strategies) (cdr strategies))
              (format #t "Comparison cancelled.~%"))
          (loop)))
       (else
        (format #t "Invalid command. Please try again.~%")
        (loop))))))

;; Enable readline history
(activate-readline)

;; Run the interactive comparison
(format #t "~%Welcome to 2048 Strategy Comparison~%")
(format #t "==================================~%")
(format #t "This tool lets you interactively compare different~%")
(format #t "AI strategies for playing the 2048 game.~%~%")

(main-loop)

(format #t "~%Goodbye!~%")
(exit 0)