#!/usr/bin/env guile
!#

;;; 2048 Strategy Analysis
;;; Comparative analysis of different heuristic strategies for 2048
;;; Based on "Composition of Basic Heuristics for the Game 2048"

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-26)
             (ice-9 textual-ports))

;; Load the main game implementation
(load "2048-heuristics.scm")

;;; Strategy Analysis Functions

(define (run-silent-game strategy max-moves)
  "Run a game without printing, return final score and max tile"
  (let ((board (make-board))
        (score 0)
        (moves 0))
    ;; Initial tiles
    (add-random-tile! board)
    (add-random-tile! board)
    
    (let loop ()
      (if (or (>= moves max-moves)
              (null? (get-valid-moves board)))
          (values score (find-max-tile board) moves)
          (let ((move (apply-strategy board strategy)))
            (if move
                (begin
                  (let-values (((new-board move-score) (move-board board move)))
                    (set! board new-board)
                    (set! score (+ score move-score))
                    (set! moves (+ moves 1)))
                  (add-random-tile! board)
                  (loop))
                (values score (find-max-tile board) moves)))))))

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

(define (analyze-strategy strategy games max-moves)
  "Run multiple games with a strategy and collect statistics"
  (let ((scores '())
        (max-tiles '())
        (move-counts '()))
    
    (format #t "Analyzing strategy: ~a~%" strategy)
    (format #t "Running ~a games...~%" games)
    
    (do ((i 0 (+ i 1))) ((= i games))
      (when (zero? (modulo i 10))
        (format #t ".")
        (force-output))
      (let-values (((score max-tile moves) (run-silent-game strategy max-moves)))
        (set! scores (cons score scores))
        (set! max-tiles (cons max-tile max-tiles))
        (set! move-counts (cons moves move-counts))))
    
    (newline)
    
    ;; Calculate statistics
    (let* ((avg-score (/ (apply + scores) games))
           (avg-max-tile (/ (apply + max-tiles) games))
           (avg-moves (/ (apply + move-counts) games))
           (max-score (apply max scores))
           (max-tile (apply max max-tiles))
           
           ;; Count distribution of max tiles
           (tile-distribution (fold (lambda (tile dist)
                                     (hash-set! dist tile
                                               (+ 1 (hash-ref dist tile 0)))
                                     dist)
                                   (make-hash-table)
                                   max-tiles))
           
           ;; Generate distribution percentage
           (tile-percentages
            (let ((result '()))
              (hash-for-each 
               (lambda (tile count)
                 (set! result 
                       (cons (cons tile 
                                  (* 100.0 (/ count games)))
                             result)))
               tile-distribution)
              (sort result (lambda (a b) (< (car a) (car b))))))
           
           ;; Score distribution
           (score-bins
            (let ((bins (make-hash-table)))
              (for-each
               (lambda (score)
                 (let ((bin (* 500 (ceiling (/ score 500)))))
                   (hash-set! bins bin
                             (+ 1 (hash-ref bins bin 0)))))
               scores)
              (sort (hash-map->list cons bins) 
                    (lambda (a b) (< (car a) (car b))))))
           
           ;; Calculate success rates
           (reach-128 (/ (count (lambda (t) (>= t 128)) max-tiles) games))
           (reach-256 (/ (count (lambda (t) (>= t 256)) max-tiles) games))
           (reach-512 (/ (count (lambda (t) (>= t 512)) max-tiles) games))
           (reach-1024 (/ (count (lambda (t) (>= t 1024)) max-tiles) games)))
      
      ;; Print results
      (format #t "Results for strategy: ~a~%" strategy)
      (format #t "=================================~%")
      (format #t "Average score: ~a~%" avg-score)
      (format #t "Maximum score: ~a~%" max-score)
      (format #t "Average moves: ~a~%" avg-moves)
      (format #t "Average max tile: ~a~%" avg-max-tile)
      (format #t "Maximum tile reached: ~a~%" max-tile)
      (format #t "~%")
      
      (format #t "Tile distribution:~%")
      (for-each
       (lambda (pair)
         (format #t "  ~a: ~,1f%~%" (car pair) (cdr pair)))
       tile-percentages)
      (format #t "~%")
      
      (format #t "Success rates:~%")
      (format #t "  Reached 128: ~,1f%~%" (* 100 reach-128))
      (format #t "  Reached 256: ~,1f%~%" (* 100 reach-256))
      (format #t "  Reached 512: ~,1f%~%" (* 100 reach-512))
      (format #t "  Reached 1024: ~,1f%~%" (* 100 reach-1024))
      (format #t "~%")
      
      ;; Return a summary for comparison
      (list (cons 'strategy strategy)
            (cons 'avg-score avg-score)
            (cons 'max-score max-score)
            (cons 'avg-moves avg-moves)
            (cons 'max-tile max-tile)
            (cons 'reach-128 reach-128)
            (cons 'reach-256 reach-256)
            (cons 'reach-512 reach-512)
            (cons 'tile-distribution tile-percentages)
            (cons 'score-distribution score-bins)))))

(define (compare-strategies strategies games max-moves)
  "Compare multiple strategies"
  (let ((results '()))
    (for-each
     (lambda (strategy)
       (let ((stats (analyze-strategy strategy games max-moves)))
         (set! results (cons stats results))))
     strategies)
    
    ;; Print comparison table
    (format #t "~%Strategy Comparison Summary~%")
    (format #t "=========================~%")
    (format #t "Strategy       | Avg Score | Max Tile | Reach 256 | Reach 512~%")
    (format #t "---------------|-----------|----------|-----------|----------~%")
    
    (for-each
     (lambda (result)
       (format #t "~15a | ~9,1f | ~8a | ~9,1f% | ~9,1f%~%"
               (assq-ref result 'strategy)
               (assq-ref result 'avg-score)
               (assq-ref result 'max-tile)
               (* 100 (assq-ref result 'reach-256))
               (* 100 (assq-ref result 'reach-512))))
     (reverse results))
    
    ;; Generate data file for external plotting
    (with-output-to-file "strategy-results.dat"
      (lambda ()
        (format #t "# Strategy comparison data~%")
        (format #t "# Strategy AvgScore MaxTile Reach128 Reach256 Reach512~%")
        (for-each
         (lambda (result)
           (format #t "~a ~a ~a ~a ~a ~a~%"
                   (assq-ref result 'strategy)
                   (assq-ref result 'avg-score)
                   (assq-ref result 'max-tile)
                   (* 100 (assq-ref result 'reach-128))
                   (* 100 (assq-ref result 'reach-256))
                   (* 100 (assq-ref result 'reach-512))))
         (reverse results))))
    
    ;; Return results for further processing
    results))

;;; Run the analysis

(define strategies-to-test
  '("random" 
    (random)
    "ER"
    (empty random)
    "MR" 
    (monotonicity random)
    "EMR"
    (empty monotonicity random)
    "GEMR"
    (greedy empty monotonicity random)))

;; Run 50 games for each strategy with max 2000 moves
(define results (compare-strategies strategies-to-test 50 2000))

;; Exit
(exit 0)