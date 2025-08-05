#!/usr/local/bin/guile \
!#

;;; Save/Load functionality for 2048 game states
;;; Allows saving and resuming games

(use-modules (ice-9 format)
             (ice-9 textual-ports)
             (ice-9 pretty-print)
             (srfi srfi-11))

;; Load the main game
(load "../2048-heuristics.scm")

;; Save directory
(define save-dir (string-append (getenv "HOME") "/.2048-saves"))

;; Ensure save directory exists
(define (ensure-save-dir)
  (unless (file-exists? save-dir)
    (mkdir save-dir)))

;; Game state structure
(define (make-game-state board score moves strategy)
  (list 'game-state
        (list 'board (board->list board))
        (list 'score score)
        (list 'moves moves)
        (list 'strategy strategy)
        (list 'timestamp (current-time))))

(define (board->list board)
  "Convert board vector to list for serialization"
  (map vector->list (vector->list board)))

(define (list->board lst)
  "Convert list back to board vector"
  (list->vector (map list->vector lst)))

;; Save game state
(define (save-game-state filename board score moves strategy)
  (ensure-save-dir)
  (let ((save-path (string-append save-dir "/" filename ".scm"))
        (state (make-game-state board score moves strategy)))
    (call-with-output-file save-path
      (lambda (port)
        (pretty-print state port)))
    (format #t "Game saved to ~a\n" save-path)
    save-path))

;; Load game state
(define (load-game-state filename)
  (let ((save-path (string-append save-dir "/" filename ".scm")))
    (if (file-exists? save-path)
        (let* ((state (call-with-input-file save-path read))
               (board-data (cadr (assq 'board state)))
               (score (cadr (assq 'score state)))
               (moves (cadr (assq 'moves state)))
               (strategy (cadr (assq 'strategy state))))
          (values (list->board board-data) score moves strategy))
        (error "Save file not found" save-path))))

;; List saved games
(define (list-saved-games)
  (ensure-save-dir)
  (let ((files (scandir save-dir 
                       (lambda (f) 
                         (string-suffix? ".scm" f)))))
    (if (null? files)
        (display "No saved games found.\n")
        (begin
          (display "Saved games:\n")
          (display "════════════\n")
          (for-each
           (lambda (file)
             (let* ((path (string-append save-dir "/" file))
                    (state (call-with-input-file path read))
                    (score (cadr (assq 'score state)))
                    (moves (cadr (assq 'moves state)))
                    (time (cadr (assq 'timestamp state))))
               (format #t "~a - Score: ~d, Moves: ~d\n"
                      (string-drop-right file 4) ; Remove .scm extension
                      score moves)))
           (sort files string<?))))))

;; Auto-save functionality
(define (auto-save-filename)
  (string-append "autosave-" 
                 (number->string (current-time))
                 "-" 
                 (number->string (random 1000))))

;; Resume game with save/load support
(define (play-with-saves strategy)
  "Play game with save/load support"
  (display "1. New Game\n")
  (display "2. Load Game\n")
  (display "Choice: ")
  
  (let ((choice (read)))
    (let-values (((board score moves saved-strategy)
                  (if (= choice 2)
                      (begin
                        (list-saved-games)
                        (display "\nEnter save name: ")
                        (let ((name (symbol->string (read))))
                          (load-game-state name)))
                      (values (make-board) 0 0 strategy))))
      
      ;; Use saved strategy if loading
      (let ((active-strategy (if (= choice 2) saved-strategy strategy)))
        
        ;; Initialize new game
        (when (= choice 1)
          (add-random-tile! board)
          (add-random-tile! board))
        
        ;; Game loop with save support
        (let loop ()
          (print-board board)
          (format #t "Score: ~a  Moves: ~a\n" score moves)
          (display "Commands: move (m), save (s), quit (q): ")
          
          (let ((cmd (read)))
            (case cmd
              ((m move)
               (let ((move (apply-strategy board active-strategy)))
                 (if move
                     (begin
                       (let-values (((new-board move-score) (move-board board move)))
                         (set! board new-board)
                         (set! score (+ score move-score))
                         (set! moves (+ moves 1)))
                       (add-random-tile! board)
                       (loop))
                     (begin
                       (format #t "Game Over! Final Score: ~a\n" score)
                       ;; Auto-save on game over
                       (save-game-state (auto-save-filename) board score moves active-strategy)))))
              
              ((s save)
               (display "Save name: ")
               (let ((name (symbol->string (read))))
                 (save-game-state name board score moves active-strategy))
               (loop))
              
              ((q quit)
               (display "Save before quitting? (y/n): ")
               (when (eq? (read) 'y)
                 (display "Save name: ")
                 (let ((name (symbol->string (read))))
                   (save-game-state name board score moves active-strategy)))
               (display "Thanks for playing!\n"))
              
              (else
               (display "Invalid command. Use 'm' for move, 's' for save, 'q' for quit.\n")
               (loop)))))))))

;; Command-line interface
(define (main args)
  (cond
    ((member "--list" args)
     (list-saved-games))
    ((member "--load" args)
     (let ((idx (list-index (lambda (x) (string=? x "--load")) args)))
       (if (< (+ idx 1) (length args))
           (let ((filename (list-ref args (+ idx 1))))
             (let-values (((board score moves strategy) (load-game-state filename)))
               (format #t "Loaded game: Score ~d, Moves ~d\n" score moves)
               (print-board board)))
           (error "No filename provided for --load"))))
    ((member "--help" args)
     (display "Usage: save-load.scm [OPTIONS]\n")
     (display "Options:\n")
     (display "  --list         List all saved games\n")
     (display "  --load FILE    Load and display a saved game\n")
     (display "  --help         Show this help message\n")
     (display "\nWithout options, starts interactive game with save/load support\n"))
    (else
     ;; Default: play with EMR strategy
     (play-with-saves '(empty monotonicity random)))))

;; Run main
(main (command-line))