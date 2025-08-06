;;; 2048-heuristics.el --- Emacs configuration for 2048 Heuristics Scheme development

;;; Commentary:
;; This configuration sets up Emacs for Scheme development with Guile 3,
;; including Geiser, Paredit, and project-specific settings.

;;; Code:

;; Package initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install required packages if not present
(defvar required-packages
  '(geiser
    geiser-guile
    paredit
    rainbow-delimiters
    company))

(dolist (pkg required-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Load packages
(require 'geiser)
(require 'geiser-guile)
(require 'paredit)
(require 'rainbow-delimiters)
(require 'company)

;; Project configuration
(defvar project-name (or (getenv "PROJECT_NAME") "2048-heuristics"))
(defvar project-root (or (getenv "PROJECT_ROOT") default-directory))

;; Geiser configuration for Guile 3
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
(setq geiser-default-implementation 'guile)
(setq geiser-guile-load-path (list project-root))

;; Auto-complete configuration
(add-hook 'geiser-mode-hook 'company-mode)
(add-hook 'geiser-repl-mode-hook 'company-mode)

;; Paredit for structured editing
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)

;; Rainbow delimiters for better parentheses visualization
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)

;; Org mode configuration for literate programming
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

;; Set Scheme implementation for Org Babel
(setq org-babel-scheme-command "guile3")

;; Project-specific keybindings
(global-set-key (kbd "C-c C-p") 'geiser-mode-switch-to-repl-and-enter)
(global-set-key (kbd "C-c C-k") 'geiser-compile-current-buffer)
(global-set-key (kbd "C-c C-r") 'geiser-eval-region)
(global-set-key (kbd "C-c C-e") 'geiser-eval-last-sexp)
(global-set-key (kbd "C-c C-d") 'geiser-doc-symbol-at-point)

;; Custom functions for 2048 development
(defun 2048-run-game ()
  "Run the 2048 heuristics game."
  (interactive)
  (geiser-load-file (concat project-root "/2048-heuristics.scm"))
  (geiser-eval-region (point-min) (point-max)))

(defun 2048-run-tests ()
  "Run the 2048 test suite."
  (interactive)
  (geiser-load-file (concat project-root "/tests/test-2048.scm")))

(defun 2048-benchmark ()
  "Run the 2048 benchmark suite."
  (interactive)
  (geiser-load-file (concat project-root "/src/benchmark.scm")))

;; Set up project-specific menu
(easy-menu-define 2048-menu scheme-mode-map "2048 Heuristics"
  '("2048"
    ["Run Game" 2048-run-game t]
    ["Run Tests" 2048-run-tests t]
    ["Run Benchmark" 2048-benchmark t]
    "---"
    ["Switch to REPL" geiser-mode-switch-to-repl t]
    ["Compile Buffer" geiser-compile-current-buffer t]))

;; Automatically start Geiser when opening Scheme files
(add-hook 'scheme-mode-hook 'geiser-mode)

;; Set default directory to project root
(setq default-directory project-root)

;; Load main project file if it exists
(let ((main-file (concat project-root "/2048-heuristics.scm")))
  (when (file-exists-p main-file)
    (find-file main-file)))

;; Start Geiser REPL
(geiser-repl-startup 'guile)

;; Display welcome message
(message "2048 Heuristics development environment loaded. Project: %s" project-name)

(provide '2048-heuristics)
;;; 2048-heuristics.el ends here