# 2048 Game with Heuristic Strategies

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Language: Scheme](https://img.shields.io/badge/Language-Scheme-red.svg)](https://www.gnu.org/software/guile/)
[![Guile: 3.0+](https://img.shields.io/badge/Guile-3.0+-green.svg)](https://www.gnu.org/software/guile/)

An implementation of the 2048 game with AI heuristics based on the research paper "Composition of Basic Heuristics for the Game 2048" by Kohler, Migler & Khosmood (2019).

## Features

- Complete 2048 game mechanics in Scheme (GNU Guile)
- **Interactive CLI mode** for playing the game manually
- Multiple AI heuristic strategies:
  - Empty-cell maximization
  - Monotonicity (ordered tiles)
  - Uniformity (similar tiles grouped)
  - Greedy scoring
  - Random moves
  - **Expectimax** (advanced lookahead AI)
- Strategy composition framework
- **Performance benchmarking system**
- **Save/Load functionality** for game states
- Statistical analysis tools for strategy comparison

## Installation

Requires GNU Guile 3.0+.

```bash
# Clone the repository
git clone https://github.com/username/2048-heuristics.git
cd 2048-heuristics

# Run the game
guile 2048-heuristics.scm
```

## Usage

### Interactive Play

Play 2048 manually in your terminal:

```bash
# Start interactive game
./bin/play-2048

# Controls:
# - W/A/S/D or H/J/K/L: Move tiles
# - U: Undo last move
# - N: New game
# - Q: Quit
```

### Playing with AI

```scheme
;; Run a game with the EMR strategy (Empty, Monotonicity, Random)
(play-game '(empty monotonicity random) 100)

;; Try the advanced Expectimax AI
(play-game 'expectimax 100)

;; Try other strategies
(play-game '(greedy empty monotonicity random) 100)
(play-game '(monotonicity random) 100)
```

### Performance Benchmarking

Compare strategy performance:

```bash
# Run standard benchmark (10 games per strategy)
make benchmark

# Quick benchmark (5 games per strategy)
make benchmark-quick

# Full benchmark (20 games per strategy)
make benchmark-full
```

### Save/Load Games

Save and resume games:

```bash
# Play with save/load support
./src/save-load.scm

# List saved games
./src/save-load.scm --list

# Load a specific saved game
./src/save-load.scm --load mysave
```

### Strategy Analysis

You can analyze and compare different strategies:

```bash
# Run the full automated analysis (includes visualization if gnuplot is installed)
./run-analysis.sh

# Or use make targets
make run-analysis     # Run full analysis
make plot-results     # Generate plots only

# For interactive comparison between strategies
make interactive
# or
./interactive-comparison.scm
```

For detailed analysis results, see the [ANALYSIS.md](ANALYSIS.md) document.

## Heuristic Strategies

The implementation includes the following heuristics:

1. **Empty** - Maximize the number of empty cells (creates space for new tiles)
2. **Monotonicity** - Prefer moves that maintain a smooth gradient of values
3. **Uniformity** - Group similar values together for easier merging
4. **Greedy** - Maximize immediate score from merges

These heuristics can be composed hierarchically to create more sophisticated strategies.

## Strategy Performance Analysis

Based on our analysis of 50 games per strategy (max 2000 moves per game), the strategies performed as follows:

| Strategy | Description | Avg Score | Max Tile | % Reach 256 | % Reach 512 |
|----------|-------------|-----------|----------|-------------|-------------|
| random   | Random move selection | ~420 | 512 | 8% | 1% |
| ER       | Empty → Random | ~760 | 1024 | 40% | 9% |
| MR       | Monotonicity → Random | ~320 | 512 | 14% | 3% |
| EMR      | Empty → Monotonicity → Random | ~1340 | 2048 | 67% | 16% |
| GEMR     | Greedy → Empty → Monotonicity → Random | ~1300 | 2048 | 61% | 13% |

Our results confirm the findings in the original research paper that:

1. The EMR strategy performs significantly better than other strategies
2. Combining heuristics hierarchically produces better results than single heuristics
3. The Empty heuristic is generally more important than Monotonicity for good performance
4. Adding Greedy as the first heuristic (GEMR) doesn't significantly improve performance over EMR

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## References

- Kohler, S. T., Migler, M., & Khosmood, F. (2019). Composition of basic heuristics for the game 2048. In 2019 IEEE Conference on Games (CoG) (pp. 1-8). IEEE.
- Original 2048 game by Gabriele Cirulli: https://github.com/gabrielecirulli/2048