#!/usr/bin/env guile3
!#

;;; Generate data files for gnuplot from strategy results
(use-modules (ice-9 regex)
             (ice-9 ftw)
             (ice-9 rdelim))

;; Find CSV files with strategy results
(define (find-strategy-files)
  (filter (lambda (file) 
            (string-match "strategy-.*-results.csv" file))
          (scandir "." string<?)))

;; Extract strategy name from filename
(define (filename->strategy-name filename)
  (let ((match (string-match "strategy-([^-]+)-results.csv" filename)))
    (if match
        (match:substring match 1)
        "unknown")))

;; Read CSV file and calculate statistics
(define (read-csv-file filename)
  (let ((scores '())
        (moves '())
        (highest-tiles '()))
    (with-input-from-file filename
      (lambda ()
        (let ((header (read-line)))  ; Skip header
          (let loop ((line (read-line)))
            (unless (eof-object? line)
              (let* ((fields (string-split line #\,))
                     (score (string->number (list-ref fields 1)))
                     (move-count (string->number (list-ref fields 2)))
                     (highest-tile (string->number (list-ref fields 3))))
                (set! scores (cons score scores))
                (set! moves (cons move-count moves))
                (set! highest-tiles (cons highest-tile highest-tiles))
                (loop (read-line))))))))
    (list (reverse scores) (reverse moves) (reverse highest-tiles))))

;; Calculate statistics
(define (calculate-stats data)
  (let* ((scores (car data))
         (moves (cadr data))
         (highest-tiles (caddr data))
         (avg-score (/ (apply + scores) (length scores)))
         (avg-moves (/ (apply + moves) (length moves)))
         (efficiency (/ avg-score avg-moves))
         (tile-counts (make-hash-table)))
    
    ;; Count highest tiles
    (for-each
     (lambda (tile)
       (hash-set! tile-counts tile
                 (+ 1 (hash-ref tile-counts tile 0))))
     highest-tiles)
    
    ;; Calculate percentages
    (let ((tile-percentages '()))
      (hash-for-each
       (lambda (tile count)
         (set! tile-percentages 
               (cons (cons tile 
                          (/ (* 100.0 count) (length highest-tiles)))
                     tile-percentages)))
       tile-counts)
      
      (list avg-score avg-moves efficiency (sort tile-percentages
                                               (lambda (a b) (< (car a) (car b))))))))

;; Process all strategy files
(define (process-all-files)
  (let ((files (find-strategy-files))
        (all-stats '()))
    
    (for-each
     (lambda (file)
       (let* ((strategy (filename->strategy-name file))
              (data (read-csv-file file))
              (stats (calculate-stats data)))
         (set! all-stats (cons (cons strategy stats) all-stats))))
     files)
    
    ;; Sort by strategy name
    (reverse all-stats)))

;; Write data for average scores
(define (write-scores-data stats)
  (with-output-to-file "scores.dat"
    (lambda ()
      (for-each
       (lambda (stat)
         (format #t "~a ~a~%" 
                 (car stat) 
                 (cadr stat)))  ; avg-score
       stats))))

;; Write data for average moves
(define (write-moves-data stats)
  (with-output-to-file "moves.dat"
    (lambda ()
      (for-each
       (lambda (stat)
         (format #t "~a ~a~%" 
                 (car stat) 
                 (caddr stat)))  ; avg-moves
       stats))))

;; Write data for efficiency
(define (write-efficiency-data stats)
  (with-output-to-file "efficiency.dat"
    (lambda ()
      (for-each
       (lambda (stat)
         (format #t "~a ~a~%" 
                 (car stat) 
                 (cadddr stat)))  ; efficiency
       stats))))

;; Write data for highest tiles
(define (write-tiles-data stats)
  ;; First, find all tile values across all strategies
  (let ((all-tiles '()))
    (for-each
     (lambda (stat)
       (let ((tile-percentages (car (cddddr stat))))
         (for-each
          (lambda (tp)
            (unless (member (car tp) all-tiles)
              (set! all-tiles (cons (car tp) all-tiles))))
          tile-percentages)))
     stats)
    
    (set! all-tiles (sort all-tiles <))
    
    ;; Now write the data file
    (with-output-to-file "tiles.dat"
      (lambda ()
        ;; Write header
        (format #t "Tile")
        (for-each
         (lambda (stat)
           (format #t " ~a" (car stat)))
         stats)
        (newline)
        
        ;; Write data for each tile value
        (for-each
         (lambda (tile)
           (format #t "~a" tile)
           (for-each
            (lambda (stat)
              (let* ((tile-percentages (car (cddddr stat)))
                     (matching (assoc tile tile-percentages)))
                (if matching
                    (format #t " ~,2f" (cdr matching))
                    (format #t " 0"))))
            stats)
           (newline))
         all-tiles)))))

;; Main function
(define (main)
  (let ((stats (process-all-files)))
    (write-scores-data stats)
    (write-moves-data stats)
    (write-efficiency-data stats)
    (write-tiles-data stats)
    (format #t "Data files generated successfully.~%")))

;; Run the main function
(main)