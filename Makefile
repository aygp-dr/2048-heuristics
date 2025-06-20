# Makefile for 2048 Heuristics Simulator
# Requires GNU Guile 3.0+

# Configuration
GUILE := guile3
GUILD := guild3
GUILE_MIN_VERSION := 3.0
SCRIPT := 2048-heuristics.scm
COMPILED := 2048-heuristics.go
INSTALL_DIR := $(HOME)/.local/bin
PROGRAM_NAME := 2048-heuristics

# Colors for output
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m # No Color

# Default target
.DEFAULT_GOAL := help

.PHONY: help
help: ## Show this help message
	@echo "$(GREEN)2048 Heuristics Simulator - Available targets:$(NC)"
	@echo ""
	@awk 'BEGIN {FS = ":.*##"} /^[a-zA-Z_-]+:.*?##/ { \
		printf "  $(YELLOW)%-15s$(NC) %s\n", $$1, $$2 \
	}' $(MAKEFILE_LIST) | sort
	@echo ""
	@echo "$(GREEN)Examples:$(NC)"
	@echo "  make deps      # Check dependencies"
	@echo "  make build     # Compile the script"
	@echo "  make run       # Run the simulator"

.PHONY: deps
deps: ## Check if Guile 3 is installed and verify version
	@echo "$(GREEN)Checking dependencies...$(NC)"
	@command -v $(GUILE) >/dev/null 2>&1 || { \
		echo "$(RED)Error: Guile is not installed$(NC)"; \
		echo "Install with: sudo apt-get install guile-3.0 (Debian/Ubuntu)"; \
		echo "           or: brew install guile (macOS)"; \
		exit 1; \
	}
	@echo -n "Found Guile version: "
	@$(GUILE) --version | head -n1
	@VERSION=$$($(GUILE) --version | head -n1 | awk '{print $$NF}'); \
	MAJOR=$$(echo $$VERSION | cut -d. -f1); \
	if [ "$$MAJOR" -lt "3" ]; then \
		echo "$(RED)Error: Guile 3.0 or higher required (found $$VERSION)$(NC)"; \
		exit 1; \
	fi
	@command -v $(GUILD) >/dev/null 2>&1 || { \
		echo "$(YELLOW)Warning: guild not found (compilation unavailable)$(NC)"; \
	}
	@echo "$(GREEN)✓ All dependencies satisfied$(NC)"

.PHONY: build
build: $(COMPILED) ## Compile the Scheme script to bytecode

$(COMPILED): $(SCRIPT)
	@echo "$(GREEN)Building $(SCRIPT)...$(NC)"
	@if command -v $(GUILD) >/dev/null 2>&1; then \
		$(GUILD) compile -O2 \
			-W unsupported-warning \
			-W unused-variable \
			-W arity-mismatch \
			$(SCRIPT) -o $(COMPILED) && \
		echo "$(GREEN)✓ Compiled to $(COMPILED)$(NC)"; \
	else \
		echo "$(YELLOW)guild not available - script will run interpreted$(NC)"; \
		touch $(COMPILED); \
	fi

.PHONY: run
run: ## Run the 2048 simulator
	@if [ -f $(COMPILED) ] && [ -s $(COMPILED) ]; then \
		echo "$(GREEN)Running compiled version...$(NC)"; \
		GUILE_AUTO_COMPILE=0 $(GUILE) -c "(load-compiled \"$(COMPILED)\")"; \
	else \
		echo "$(GREEN)Running interpreted version...$(NC)"; \
		GUILE_AUTO_COMPILE=0 $(GUILE) $(SCRIPT); \
	fi

.PHONY: run-interactive
run-interactive: ## Run Guile REPL with the game loaded
	@echo "$(GREEN)Starting interactive REPL...$(NC)"
	@echo "Try: (play-game '(empty monotonicity random) 50)"
	@GUILE_AUTO_COMPILE=0 $(GUILE) -l $(SCRIPT) --

.PHONY: run-strategy
run-strategy: ## Run with custom strategy (use STRATEGY env var)
	@if [ -z "$(STRATEGY)" ]; then \
		echo "$(RED)Error: Specify strategy with STRATEGY variable$(NC)"; \
		echo "Example: make run-strategy STRATEGY=\"monotonicity random\""; \
		exit 1; \
	fi
	@echo "$(GREEN)Running with strategy: $(STRATEGY)$(NC)"
	@$(GUILE) -c "(load \"$(SCRIPT)\") (play-game '($(STRATEGY)) 100)"

.PHONY: run-analysis
run-analysis: ## Run performance analysis for all strategies
	@echo "$(GREEN)Running strategy analysis...$(NC)"
	@bin/run-analysis

.PHONY: plot-results
plot-results: ## Generate plots from analysis results
	@echo "$(GREEN)Generating plots from analysis results...$(NC)"
	@bin/generate-plots

.PHONY: benchmark
benchmark: build ## Run performance benchmark with multiple strategies
	@echo "$(GREEN)Running benchmark suite...$(NC)"
	@echo "#!/usr/bin/env guile" > benchmark.scm
	@echo '!#' >> benchmark.scm
	@cat $(SCRIPT) >> benchmark.scm
	@echo "" >> benchmark.scm
	@echo "(define (benchmark-strategy name strategy runs)" >> benchmark.scm
	@echo "  (format #t \"~%Testing ~a...~%\" name)" >> benchmark.scm
	@echo "  (let ((scores '()))" >> benchmark.scm
	@echo "    (do ((i 0 (+ i 1))) ((= i runs))" >> benchmark.scm
	@echo "      (set! scores (cons (play-game strategy 1000) scores)))" >> benchmark.scm
	@echo "    (let ((avg (/ (apply + scores) runs)))" >> benchmark.scm
	@echo "      (format #t \"Average score over ~a runs: ~a~%\" runs avg))))" >> benchmark.scm
	@echo "" >> benchmark.scm
	@echo "(benchmark-strategy \"EMR\" '(empty monotonicity random) 5)" >> benchmark.scm
	@echo "(benchmark-strategy \"MR\" '(monotonicity random) 5)" >> benchmark.scm
	@echo "(benchmark-strategy \"Random\" '(random) 5)" >> benchmark.scm
	@$(GUILE) benchmark.scm
	@rm -f benchmark.scm

.PHONY: install
install: build ## Install the script to ~/.local/bin
	@echo "$(GREEN)Installing to $(INSTALL_DIR)...$(NC)"
	@mkdir -p $(INSTALL_DIR)
	@echo "#!/usr/bin/env guile" > $(INSTALL_DIR)/$(PROGRAM_NAME)
	@echo '!#' >> $(INSTALL_DIR)/$(PROGRAM_NAME)
	@cat $(SCRIPT) >> $(INSTALL_DIR)/$(PROGRAM_NAME)
	@chmod +x $(INSTALL_DIR)/$(PROGRAM_NAME)
	@echo "$(GREEN)✓ Installed to $(INSTALL_DIR)/$(PROGRAM_NAME)$(NC)"
	@echo "Make sure $(INSTALL_DIR) is in your PATH"

.PHONY: uninstall
uninstall: ## Remove installed script
	@echo "$(GREEN)Uninstalling...$(NC)"
	@rm -f $(INSTALL_DIR)/$(PROGRAM_NAME)
	@echo "$(GREEN)✓ Uninstalled$(NC)"

.PHONY: clean
clean: ## Remove compiled bytecode and temporary files
	@echo "$(GREEN)Cleaning build artifacts...$(NC)"
	@rm -f $(COMPILED)
	@rm -f benchmark.scm
	@echo "$(GREEN)✓ Clean complete$(NC)"

.PHONY: test
test: build ## Run basic tests
	@echo "$(GREEN)Running tests...$(NC)"
	@$(GUILE) -c "(load \"$(SCRIPT)\") \
		(format #t \"Testing board creation...~%\") \
		(let ((b (make-board))) \
			(add-random-tile! b) \
			(add-random-tile! b) \
			(print-board b) \
			(format #t \"Empty cells: ~a~%\" (count-empty b)) \
			(format #t \"Valid moves: ~a~%\" (get-valid-moves b)) \
			(format #t \"~%✓ Basic tests passed~%\"))"

.PHONY: watch
watch: ## Watch for changes and rebuild
	@command -v inotifywait >/dev/null 2>&1 || { \
		echo "$(RED)Error: inotifywait not found$(NC)"; \
		echo "Install with: sudo apt-get install inotify-tools"; \
		exit 1; \
	}
	@echo "$(GREEN)Watching for changes...$(NC)"
	@while true; do \
		inotifywait -q -e modify $(SCRIPT); \
		$(MAKE) build; \
	done

.PHONY: debug
debug: ## Run with debug output
	@echo "$(GREEN)Running in debug mode...$(NC)"
	@bin/debug-2048

.PHONY: profile
profile: build ## Run with profiling enabled
	@echo "$(GREEN)Running profiler...$(NC)"
	@$(GUILE) --no-auto-compile -c "\
		(use-modules (statprof)) \
		(load \"$(SCRIPT)\") \
		(statprof \
			(lambda () \
				(play-game '(empty monotonicity random) 100)) \
			#:count-calls? #t)"

.PHONY: repl
repl: ## Start a REPL (alias for run-interactive)
	@$(MAKE) run-interactive

.PHONY: all
all: deps build test ## Run all checks, build, and test

.PHONY: info
info: ## Show system and project information
	@echo "$(GREEN)System Information:$(NC)"
	@echo "  OS: $$(uname -s) $$(uname -r)"
	@echo "  Guile: $$( $(GUILE) --version | head -n1 )"
	@echo ""
	@echo "$(GREEN)Project Information:$(NC)"
	@echo "  Script: $(SCRIPT)"
	@echo "  Compiled: $(COMPILED)"
	@echo "  Install path: $(INSTALL_DIR)/$(PROGRAM_NAME)"

# Prevent make from trying to rebuild this Makefile
Makefile: ;