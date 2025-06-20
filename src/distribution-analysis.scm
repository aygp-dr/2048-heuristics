#!/usr/bin/env guile3
!#

;;; Distribution Analysis for 2048 Heuristics
;;; Runs multiple games and analyzes score distribution

(load "2048-heuristics.scm")
(use-modules (ice-9 format)
             (srfi srfi-1))  ; For list operations

;; Save original demo function and disable it
(define original-demo demo)
(set! demo (lambda () #f))

;;; Distribution tracking

(define (run-games strategy num-games max-moves)
  "Run multiple games and collect statistics"
  (let ((scores '())
        (moves-counts '())
        (highest-tiles '())
        (game-count 0))
    
    (format #t "Running ~a games with strategy ~a (max ~a moves each)...~%~%" 
            num-games strategy max-moves)
    
    (do ((i 0 (+ i 1))) ((= i num-games))
      ;; Progress indicator
      (when (zero? (modulo i 10))
        (format #t "Game ~a/~a...~%" (+ i 1) num-games))
      
      (let ((board (make-board))
            (score 0)
            (moves 0))
        
        ;; Initialize board
        (add-random-tile! board)
        (add-random-tile! board)
        
        ;; Play game
        (let loop ()
          (when (< moves max-moves)
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
                    ;; Game over, collect stats
                    (set! scores (cons score scores))
                    (set! moves-counts (cons moves moves-counts))
                    (set! highest-tiles (cons (find-highest-tile board) highest-tiles))
                    (set! game-count (+ game-count 1))
                    #f)))))
        
        ;; If game not over yet, collect stats anyway
        (when (= moves max-moves)
          (set! scores (cons score scores))
          (set! moves-counts (cons moves moves-counts))
          (set! highest-tiles (cons (find-highest-tile board) highest-tiles))
          (set! game-count (+ game-count 1)))))
    
    ;; Return statistics
    (list (reverse scores) 
          (reverse moves-counts) 
          (reverse highest-tiles))))

(define (find-highest-tile board)
  "Find the highest tile value on the board"
  (let ((highest 0))
    (vector-for-each
     (lambda (i row)
       (vector-for-each
        (lambda (j val)
          (when (> val highest)
            (set! highest val)))
        row))
     board)
    highest))

(define (display-histogram values min-val max-val num-buckets)
  "Display a simple ASCII histogram"
  (let* ((range (- max-val min-val))
         (bucket-size (max 1 (ceiling (/ range num-buckets))))
         (buckets (make-vector num-buckets 0)))
    
    ;; Sort values into buckets
    (for-each
     (lambda (val)
       (let ((bucket-index (min (- num-buckets 1)
                               (quotient (- val min-val) bucket-size))))
         (vector-set! buckets bucket-index 
                     (+ 1 (vector-ref buckets bucket-index)))))
     values)
    
    ;; Display histogram
    (format #t "~%Distribution (bucket size: ~a):~%" bucket-size)
    (format #t "~%Value Range            | Count | Histogram~%")
    (format #t "------------------------+-------+----------~%")
    
    (do ((i 0 (+ i 1))) ((= i num-buckets))
      (let* ((bucket-count (vector-ref buckets i))
             (bucket-min (+ min-val (* i bucket-size)))
             (bucket-max (+ bucket-min bucket-size -1))
             (scale-factor (/ 50.0 (apply max (vector->list buckets))))
             (bar-length (inexact->exact (round (* bucket-count scale-factor)))))
        
        (format #t "~5d - ~5d          | ~5d | ~a~%" 
                bucket-min bucket-max bucket-count 
                (make-string bar-length #\#))))
    
    (format #t "~%")))

(define (analyze-distribution strategy num-games max-moves)
  "Run games and analyze the resulting distribution"
  (let* ((results (run-games strategy num-games max-moves))
         (scores (car results))
         (moves (cadr results))
         (highest-tiles (caddr results))
         (avg-score (/ (apply + scores) num-games))
         (avg-moves (/ (apply + moves) num-games))
         (max-score (apply max scores))
         (min-score (apply min scores))
         (std-dev-score (standard-deviation scores avg-score))
         (sorted-scores (sort scores <)))
    
    (format #t "~%~%Results for strategy: ~a~%" strategy)
    (format #t "================================================~%")
    (format #t "Games played:          ~a~%" num-games)
    (format #t "Average score:         ~a~%" (inexact->exact (round avg-score)))
    (format #t "Score standard dev:    ~a~%" (inexact->exact (round std-dev-score)))
    (format #t "Minimum score:         ~a~%" min-score)
    (format #t "Maximum score:         ~a~%" max-score)
    (format #t "Average moves:         ~a~%" (inexact->exact (round avg-moves)))
    (format #t "Median score:          ~a~%" (list-ref sorted-scores (quotient num-games 2)))
    
    ;; Distribution of scores
    (display-histogram scores min-score max-score 10)
    
    ;; Distribution of moves
    (display-histogram moves (apply min moves) (apply max moves) 10)
    
    ;; Distribution of highest tiles
    (let ((tile-counts (make-hash-table)))
      (for-each
       (lambda (tile)
         (hash-set! tile-counts tile
                   (+ 1 (hash-ref tile-counts tile 0))))
       highest-tiles)
      
      (format #t "~%Highest tile distribution:~%")
      (format #t "Tile  | Count | Percentage~%")
      (format #t "------+-------+-----------~%")
      
      (let ((sorted-tiles (sort (hash-map->list cons tile-counts) 
                              (lambda (a b) (< (car a) (car b))))))
        (for-each
         (lambda (pair)
           (let ((tile (car pair))
                 (count (cdr pair)))
             (format #t "~5d | ~5d | ~8,2f%%~%" 
                     tile count (* 100.0 (/ count num-games)))))
         sorted-tiles)))
    
    ;; Write results to CSV for further analysis
    (with-output-to-file (string-append "strategy-" 
                                       (symbol->string (car strategy)) 
                                       "-results.csv")
      (lambda ()
        (format #t "game,score,moves,highest_tile~%")
        (do ((i 0 (+ i 1))) ((= i num-games))
          (format #t "~a,~a,~a,~a~%" 
                  (+ i 1) (list-ref scores i) (list-ref moves i) (list-ref highest-tiles i)))))))

(define (standard-deviation values mean)
  "Calculate standard deviation"
  (let ((variance (/ (apply + (map (lambda (x) (expt (- x mean) 2)) values))
                   (length values))))
    (sqrt variance)))

(define (main)
  ;; Analyze EMR strategy with 100 games, 500 max moves
  (analyze-distribution '(empty monotonicity random) 100 500)
  
  ;; Other strategies for comparison
  (analyze-distribution '(monotonicity random) 100 500)
  (analyze-distribution '(empty random) 100 500)
  (analyze-distribution '(greedy empty monotonicity random) 100 500)
  (analyze-distribution '(random) 100 500))

;; Run the analysis
(main)