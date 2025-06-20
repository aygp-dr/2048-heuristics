#!/usr/bin/env guile
!#

(load "2048-heuristics.scm")
(use-modules (ice-9 format))

(define (visualize-decision board strategy)
  "Show how strategy makes decisions"
  (format #t "~%Current board:~%")
  (print-board board)
  
  (let ((moves (get-valid-moves board)))
    (format #t "Valid moves: ~a~%~%" moves)
    
    ;; Show evaluation for each heuristic
    (for-each
     (lambda (evaluator)
       (format #t "~a scores:~%" evaluator)
       (for-each
        (lambda (move)
          (let ((score (evaluate-move board move evaluator)))
            (format #t "  ~a: ~a~%" move score)))
        moves)
       (newline))
     strategy)
    
    ;; Show final decision
    (let ((chosen (apply-strategy board strategy)))
      (format #t "Strategy ~a chooses: ~a~%~%" strategy chosen)
      chosen)))

;; Demo
(let ((board (make-board)))
  (add-random-tile! board)
  (add-random-tile! board)
  (visualize-decision board '(empty monotonicity random)))
