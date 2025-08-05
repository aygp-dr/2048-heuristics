#!/usr/local/bin/guile \
!#

;;; Performance Benchmarking System for 2048 Heuristics
;;; Measures execution time, memory usage, and strategy effectiveness

(use-modules (ice-9 time)
             (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-19))  ; Time operations

;; Load the main game
(load "../2048-heuristics.scm")

;; Benchmark configuration
(define benchmark-runs 10)      ; Number of runs per strategy
(define warmup-runs 2)          ; Warmup runs before benchmarking

;; Timing utilities
(define (current-milliseconds)
  (let ((time (current-time)))
    (+ (* 1000 (time-second time))
       (quotient (time-nanosecond time) 1000000))))

(define (measure-time thunk)
  (let ((start (current-milliseconds)))
    (let ((result (thunk)))
      (cons result (- (current-milliseconds) start)))))

;; Memory measurement (approximation)
(define (measure-memory thunk)
  (gc)  ; Force garbage collection
  (let ((start-stats (gc-stats)))
    (let ((result (thunk)))
      (gc)
      (let ((end-stats (gc-stats)))
        (cons result 
              (- (assq-ref end-stats 'heap-total-allocated)
                 (assq-ref start-stats 'heap-total-allocated)))))))

;; Benchmark a single game
(define (benchmark-game strategy)
  (let-values (((time-result time-ms) 
                (measure-time 
                 (lambda () 
                   (play-with-strategy (make-board) strategy 0)))))
    (let ((final-score (cadr time-result))
          (moves (caddr time-result))
          (max-tile (cadddr time-result)))
      (list 'score final-score
            'moves moves
            'max-tile max-tile
            'time-ms time-ms))))

;; Benchmark a strategy multiple times
(define (benchmark-strategy strategy-name strategy runs)
  (format #t "\nBenchmarking ~a strategy...\n" strategy-name)
  
  ;; Warmup
  (format #t "Warming up...")
  (for-each
   (lambda (i)
     (play-with-strategy (make-board) strategy 0)
     (display "."))
   (iota warmup-runs))
  (newline)
  
  ;; Actual benchmarks
  (let ((results '()))
    (format #t "Running ~d benchmarks...\n" runs)
    (for-each
     (lambda (i)
       (format #t "  Run ~2d: " (+ i 1))
       (let ((result (benchmark-game strategy)))
         (set! results (cons result results))
         (format #t "Score: ~5d, Time: ~4dms\n" 
                 (cadr (assq 'score result))
                 (cadr (assq 'time-ms result)))))
     (iota runs))
    
    ;; Calculate statistics
    (let* ((scores (map (lambda (r) (cadr (assq 'score r))) results))
           (times (map (lambda (r) (cadr (assq 'time-ms r))) results))
           (moves (map (lambda (r) (cadr (assq 'moves r))) results))
           (max-tiles (map (lambda (r) (cadr (assq 'max-tile r))) results))
           (avg-score (/ (apply + scores) runs))
           (avg-time (/ (apply + times) runs))
           (avg-moves (/ (apply + moves) runs))
           (max-score (apply max scores))
           (min-score (apply min scores)))
      
      (list 'strategy strategy-name
            'avg-score avg-score
            'max-score max-score
            'min-score min-score
            'avg-time-ms avg-time
            'avg-moves avg-moves
            'max-tiles (sort max-tiles >)
            'runs runs))))

;; Compare multiple strategies
(define (compare-strategies strategies)
  (let ((results '()))
    (for-each
     (lambda (strategy-pair)
       (let ((name (car strategy-pair))
             (strategy (cdr strategy-pair)))
         (set! results 
               (cons (benchmark-strategy name strategy benchmark-runs) 
                     results))))
     strategies)
    (reverse results)))

;; Display comparison results
(define (display-comparison results)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                  BENCHMARK RESULTS                           ║\n")
  (display "╠══════════════════════════════════════════════════════════════╣\n")
  (format #t "║ Strategy         │ Avg Score │ Max Score │ Avg Time │ Moves ║\n")
  (display "╟──────────────────┼───────────┼───────────┼──────────┼───────╢\n")
  
  (for-each
   (lambda (result)
     (format #t "║ ~16a │ ~9,1f │ ~9d │ ~7,1fms │ ~5,0f ║\n"
             (cadr (assq 'strategy result))
             (cadr (assq 'avg-score result))
             (cadr (assq 'max-score result))
             (cadr (assq 'avg-time-ms result))
             (cadr (assq 'avg-moves result))))
   (sort results (lambda (a b) 
                  (> (cadr (assq 'avg-score a))
                     (cadr (assq 'avg-score b))))))
  
  (display "╚══════════════════════════════════════════════════════════════╝\n"))

;; Detailed strategy analysis
(define (analyze-strategy strategy-name strategy detailed-runs)
  (format #t "\nDetailed Analysis: ~a\n" strategy-name)
  (display "═══════════════════════════════════════\n")
  
  (let ((results '()))
    ;; Run detailed benchmarks
    (for-each
     (lambda (i)
       (format #t "\nGame ~d:\n" (+ i 1))
       (let* ((board (make-board))
              (start-time (current-milliseconds))
              (game-result (play-with-strategy board strategy 0))
              (end-time (current-milliseconds))
              (score (cadr game-result))
              (moves (caddr game-result))
              (max-tile (cadddr game-result)))
         
         (format #t "  Score: ~d\n" score)
         (format #t "  Moves: ~d\n" moves)
         (format #t "  Max tile: ~d\n" max-tile)
         (format #t "  Time: ~dms\n" (- end-time start-time))
         (format #t "  Score/move: ~,2f\n" (/ score moves))
         
         (set! results (cons (list score moves max-tile 
                                  (- end-time start-time))
                            results))))
     (iota detailed-runs))
    
    ;; Display statistics
    (let* ((scores (map car results))
           (moves (map cadr results))
           (max-tiles (map caddr results))
           (times (map cadddr results))
           (avg-score (/ (apply + scores) detailed-runs))
           (stddev-score (sqrt (/ (apply + 
                                         (map (lambda (s) 
                                               (expt (- s avg-score) 2))
                                             scores))
                                 detailed-runs))))
      
      (display "\nStatistics:\n")
      (display "───────────\n")
      (format #t "Average score: ~,1f ± ~,1f\n" avg-score stddev-score)
      (format #t "Score range: ~d - ~d\n" (apply min scores) (apply max scores))
      (format #t "Average moves: ~,1f\n" (/ (apply + moves) detailed-runs))
      (format #t "Average time: ~,1fms\n" (/ (apply + times) detailed-runs))
      (display "\nMax tile distribution:\n")
      (let ((tile-counts (make-hash-table)))
        (for-each
         (lambda (tile)
           (hash-set! tile-counts tile 
                     (+ 1 (hash-ref tile-counts tile 0))))
         max-tiles)
        (for-each
         (lambda (tile)
           (format #t "  ~4d: ~d times (~,1f%)\n" 
                   tile 
                   (hash-ref tile-counts tile)
                   (* 100.0 (/ (hash-ref tile-counts tile) detailed-runs))))
         (sort (hash-map->list (lambda (k v) k) tile-counts) >))))))

;; Main benchmark suite
(define (run-benchmark-suite)
  (display "\n🎮 2048 Heuristics Performance Benchmark Suite 🎮\n")
  (display "================================================\n")
  
  ;; Define strategies to benchmark
  (let ((strategies
         (list (cons "Random" 'random)
               (cons "Greedy" 'greedy)
               (cons "Empty" 'empty)
               (cons "Monotonicity" 'monotonicity)
               (cons "Uniformity" 'uniformity)
               (cons "EMR" 'emr)
               (cons "EMU" 'emu)
               (cons "GEMR" 'gemr)
               (cons "Expectimax" 'expectimax))))
    
    ;; Quick comparison
    (let ((comparison-results (compare-strategies strategies)))
      (display-comparison comparison-results))
    
    ;; Detailed analysis of top strategies
    (display "\n")
    (analyze-strategy "EMR (Empty→Monotonicity→Random)" 'emr 5)
    (display "\n")
    (analyze-strategy "GEMR (Greedy→Empty→Monotonicity→Random)" 'gemr 5)))

;; Command-line interface
(define (main args)
  (cond
    ((member "--quick" args)
     (set! benchmark-runs 5)
     (run-benchmark-suite))
    ((member "--full" args)
     (set! benchmark-runs 20)
     (run-benchmark-suite))
    ((member "--help" args)
     (display "Usage: benchmark.scm [OPTIONS]\n")
     (display "Options:\n")
     (display "  --quick   Run quick benchmark (5 games per strategy)\n")
     (display "  --full    Run full benchmark (20 games per strategy)\n")
     (display "  --help    Show this help message\n"))
    (else
     (run-benchmark-suite))))

;; Run benchmarks
(main (command-line))