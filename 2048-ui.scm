#!/usr/bin/env guile3
!#

;;; Simple terminal UI for 2048 game
;;; Provides a more interactive experience than the basic demo

(use-modules (ice-9 readline)    ; Line editing support
             (ice-9 format)      ; Formatted output
             (ice-9 threads)     ; Sleep function
             (srfi srfi-1)       ; List operations
             (srfi srfi-43))     ; Vector operations

;; Load main game logic
(load "2048-heuristics.scm")

;; Import 2048 game functionality
(define demo (lambda () #f))

;; Load the main program code
(load "2048-heuristics.scm")

;; Remove the auto-run demo
(set! demo (lambda () #f))

;;; Terminal UI helpers

(define (clear-screen)
  "Clear the terminal screen"
  (display "\x1B[2J\x1B[H"))

(define (color-text str color)
  "Apply ANSI color to text"
  (let ((color-code (case color
                      ((black) "30")
                      ((red) "31")
                      ((green) "32")
                      ((yellow) "33")
                      ((blue) "34")
                      ((magenta) "35")
                      ((cyan) "36")
                      ((white) "37")
                      (else "37"))))
    (string-append "\x1B[" color-code "m" str "\x1B[0m")))

(define (bold-text str)
  "Make text bold"
  (string-append "\x1B[1m" str "\x1B[0m"))

(define (tile-color val)
  "Choose color based on tile value"
  (cond
   ((= val 0) 'black)
   ((= val 2) 'blue)
   ((= val 4) 'green)
   ((= val 8) 'cyan)
   ((= val 16) 'magenta)
   ((= val 32) 'red)
   ((= val 64) 'yellow)
   ((= val 128) 'blue)
   ((= val 256) 'green)
   ((= val 512) 'cyan)
   ((= val 1024) 'magenta)
   ((= val 2048) 'red)
   (else 'white)))

(define (display-board-pretty board)
  "Display board with colors and formatting"
  (newline)
  (display "┌──────┬──────┬──────┬──────┐") (newline)
  (vector-for-each
   (lambda (i row)
     (display "│")
     (vector-for-each
      (lambda (j val)
        (let ((val-str (if (zero? val) "      " (format #f " ~4d  " val))))
          (display (color-text val-str (tile-color val)))
          (display "│")))
      row)
     (newline)
     (if (< i 3)
         (begin
           (display "├──────┼──────┼──────┼──────┤")
           (newline))
         (begin
           (display "└──────┴──────┴──────┴──────┘")
           (newline))))
   board)
  (newline))

(define (display-help)
  "Show controls and help"
  (display (bold-text "2048 Game Controls:")) (newline)
  (display "  w or ↑: Move Up") (newline)
  (display "  s or ↓: Move Down") (newline)
  (display "  a or ←: Move Left") (newline)
  (display "  d or →: Move Right") (newline)
  (display "  h: Show this help") (newline)
  (display "  q: Quit game") (newline)
  (display "  ai: Make AI move") (newline)
  (display "  auto [n]: Let AI play n moves") (newline)
  (newline))

;;; Game controller

(define (play-interactive-game)
  "Play game with keyboard input"
  (let ((board (make-board))
        (score 0)
        (moves 0)
        (game-over #f))
    
    ;; Initialize board with two random tiles
    (add-random-tile! board)
    (add-random-tile! board)
    
    ;; Strategy for AI moves
    (define ai-strategy '(empty monotonicity random))
    
    ;; Process a move
    (define (make-move direction)
      (when (valid-move? board direction)
        (let-values (((new-board move-score) (move-board board direction)))
          (set! board new-board)
          (set! score (+ score move-score))
          (set! moves (+ moves 1))
          (add-random-tile! board)
          (when (null? (get-valid-moves board))
            (set! game-over #t)))))
    
    ;; AI auto-play
    (define (auto-play count)
      (let loop ((remaining count))
        (when (and (> remaining 0) (not game-over))
          (let ((move (apply-strategy board ai-strategy)))
            (if move
                (begin
                  (make-move move)
                  (display-game-state)
                  (sleep 0.5)  ; Pause to show the move
                  (loop (- remaining 1)))
                (set! game-over #t))))))
    
    ;; Display current state
    (define (display-game-state)
      (clear-screen)
      (display (bold-text "2048 Game with Heuristics")) (newline)
      (display-board-pretty board)
      (format #t "Score: ~a  Moves: ~a~%" score moves)
      (if game-over
          (display (color-text "Game Over!" 'red))
          (display "Enter move (w/a/s/d, h for help): ")))
    
    ;; Main game loop
    (let loop ()
      (display-game-state)
      (unless game-over
        (let ((input (readline)))
          (cond
           ;; Movement
           ((member input '("w" "W" "up" "UP" "k"))
            (make-move 'up))
           ((member input '("s" "S" "down" "DOWN" "j"))
            (make-move 'down))
           ((member input '("a" "A" "left" "LEFT" "h"))
            (make-move 'left))
           ((member input '("d" "D" "right" "RIGHT" "l"))
            (make-move 'right))
           
           ;; Help
           ((member input '("h" "H" "help" "HELP" "?"))
            (display-help))
           
           ;; AI moves
           ((string=? input "ai")
            (let ((move (apply-strategy board ai-strategy)))
              (when move
                (make-move move))))
           
           ;; Auto-play
           ((string-prefix? "auto" input)
            (let* ((parts (string-split input #\space))
                   (count (if (= (length parts) 2)
                              (string->number (cadr parts))
                              10)))
              (auto-play count)))
           
           ;; Quit
           ((member input '("q" "Q" "quit" "QUIT" "exit" "EXIT"))
            (display "Thanks for playing!") (newline)
            (set! game-over #t))
           
           ;; Invalid input
           (else
            (display "Invalid input. Use w/a/s/d for moves, h for help.") (newline)
            (sleep 1))))
        (loop)))))

;; Start game
(display-help)
(play-interactive-game)