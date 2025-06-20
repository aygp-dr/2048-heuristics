#!/usr/bin/env guile
!#

(load "2048-heuristics.scm")

(define strategies
  '(("EMR" . (empty monotonicity random))
    ("GEMR" . (greedy empty monotonicity random))
    ("MR" . (monotonicity random))
    ("ER" . (empty random))
    ("Random" . (random))))

(define (run-benchmark runs-per-strategy max-moves)
  (format #t "Running benchmark: ~a runs per strategy, ~a max moves~%~%"
          runs-per-strategy max-moves)
  
  (for-each
   (lambda (strategy-pair)
     (let ((name (car strategy-pair))
           (strategy (cdr strategy-pair)))
       (format #t "Testing ~a strategy: ~a~%" name strategy)
       (let ((scores '()))
         (do ((i 0 (+ i 1))) ((= i runs-per-strategy))
           (format #t "  Run ~a/~a... " (+ i 1) runs-per-strategy)
           (force-output)
           (let ((score (play-game strategy max-moves)))
             (set! scores (cons score scores))
             (format #t "Score: ~a~%" score)))
         (let* ((total (apply + scores))
                (avg (/ total runs-per-strategy))
                (sorted (sort scores >))
                (median (list-ref sorted (quotient runs-per-strategy 2))))
           (format #t "  Average: ~a~%" avg)
           (format #t "  Median: ~a~%" median)
           (format #t "  Max: ~a~%" (car sorted))
           (format #t "  Min: ~a~%~%" (car (reverse sorted)))))))
   strategies))

;; Run benchmark
(run-benchmark 10 1000)
