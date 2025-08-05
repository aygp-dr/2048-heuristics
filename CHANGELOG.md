# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-08-05

### Added
- **Interactive CLI mode** (`bin/play-2048`) - Play 2048 in the terminal with:
  - Real-time colored board display
  - WASD and vim-style (HJKL) controls
  - Undo functionality with history
  - Score and move tracking
  - New game and quit options
- **Performance benchmarking system** (`src/benchmark.scm`) with:
  - Time and memory measurement
  - Statistical analysis (average, min, max, stddev)
  - Strategy comparison tables
  - Detailed per-game analysis
  - Quick (--quick) and full (--full) benchmark modes
- **Expectimax AI strategy** - Advanced lookahead algorithm that:
  - Evaluates expected values of moves
  - Considers tile spawn probabilities (90% for 2, 10% for 4)
  - Uses depth-3 search by default
  - Combines empty cells, monotonicity, and uniformity heuristics
- **Save/Load functionality** (`src/save-load.scm`) featuring:
  - Save game states to disk
  - Resume previously saved games
  - List all saved games with scores
  - Auto-save on game over
  - Command-line options for save management

### Changed
- Updated Makefile with new targets:
  - `make benchmark` - Run standard benchmarks
  - `make benchmark-quick` - Quick 5-game benchmark
  - `make benchmark-full` - Full 20-game benchmark
- Enhanced `apply-strategy` to support both symbolic strategies and advanced AI
- Added `play-with-strategy` function for complete game simulation

### Fixed
- Fixed test import issue by adding SRFI-11 for `let-values` support
- All 42 tests now pass successfully
- Resolved test framework compatibility issues

### Technical Improvements
- Better code organization with modular components
- Enhanced strategy evaluation system
- Improved game state management
- More robust error handling

## [0.1.0] - 2025-08-05

### Added
- Complete 2048 game implementation in GNU Guile Scheme
- Multiple AI heuristic strategies: Empty, Monotonicity, Uniformity, Greedy, Random
- Hierarchical strategy composition framework (EMR, GEMR, etc.)
- Comprehensive test suite with 42 test cases using SRFI-64
- Statistical analysis tools for strategy comparison
- Makefile with comprehensive build and test targets
- Binary scripts for easy analysis execution
- Cross-platform support (Linux, FreeBSD, macOS)
- Gnuplot visualization support for analysis results
- Interactive comparison tools for strategy evaluation

### Fixed
- Test suite now passes all 42 tests reliably
- Fixed slide-row behavior tests to match actual game mechanics
- Resolved script loading conflicts during testing
- Fixed FreeBSD bash compatibility issues
- Corrected SRFI-64 test framework integration
- Eliminated duplicate demo execution on script load

### Changed
- Reorganized project structure with proper bin/, src/, tests/ directories
- Improved error handling and user feedback
- Enhanced documentation with installation guides
- Consolidated duplicate script files

### Technical Details
- Requires GNU Guile 3.0+
- Optional gnuplot dependency for visualizations
- Supports analysis of 50+ games per strategy
- Implements research findings from Kohler, Migler & Khosmood (2019)
- Validates EMR (Empty → Monotonicity → Random) as optimal strategy