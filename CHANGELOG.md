# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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