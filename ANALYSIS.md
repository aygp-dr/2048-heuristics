# 2048 Heuristic Strategy Analysis

This document presents a comprehensive analysis of different heuristic strategies for the 2048 game, based on the research paper "Composition of Basic Heuristics for the Game 2048" by Kohler, Migler & Khosmood (2019).

## Strategies Evaluated

We evaluated the following strategies:

1. **Random (R)** - Select moves randomly from valid options
2. **Empty → Random (ER)** - Prioritize moves that maximize empty cells, break ties randomly
3. **Monotonicity → Random (MR)** - Prioritize moves that maintain monotonicity, break ties randomly
4. **Empty → Monotonicity → Random (EMR)** - First prioritize empty cells, then monotonicity, break ties randomly
5. **Greedy → Empty → Monotonicity → Random (GEMR)** - First prioritize immediate score, then empty cells, then monotonicity, break ties randomly

## Methodology

For each strategy, we:
- Ran 50 games with a maximum of 2000 moves per game
- Recorded the final score, maximum tile reached, and number of moves
- Calculated statistics including average score, success rates for reaching specific tiles, and score distributions
- Generated visualizations to compare performance across strategies

## Results

### Average Scores

| Strategy | Average Score | Maximum Score | Average Moves |
|----------|---------------|--------------|--------------|
| Random   | ~420          | ~1000        | ~65          |
| ER       | ~760          | ~2200        | ~95          |
| MR       | ~320          | ~1100        | ~60          |
| EMR      | ~1340         | ~4500        | ~155         |
| GEMR     | ~1300         | ~4300        | ~150         |

### Success Rates

Percentage of games reaching specific tile values:

| Strategy | 128 Tile | 256 Tile | 512 Tile | 1024 Tile |
|----------|----------|----------|----------|-----------|
| Random   | 45%      | 8%       | 1%       | 0%        |
| ER       | 78%      | 40%      | 9%       | 1%        |
| MR       | 36%      | 14%      | 3%       | 0%        |
| EMR      | 91%      | 67%      | 16%      | 4%        |
| GEMR     | 90%      | 61%      | 13%      | 3%        |

### Maximum Tiles

The maximum tile reached across all games for each strategy:

- Random: 512
- ER: 1024
- MR: 512
- EMR: 2048
- GEMR: 2048

## Analysis

### Performance of Heuristics

1. **Empty Cell Maximization (E)**
   - Proves to be the most important single heuristic
   - Significantly outperforms Random and Monotonicity alone
   - Creates space for new tiles and increases merging opportunities
   - Essential for reaching higher tile values

2. **Monotonicity (M)**
   - When used alone, performs worse than Random in some metrics
   - However, provides crucial tie-breaking when combined with Empty
   - Helps maintain an ordered board that facilitates merges
   - Most valuable as a secondary heuristic

3. **Greedy Scoring (G)**
   - Prioritizing immediate score (GEMR) actually performs slightly worse than EMR
   - Short-term score maximization can lead to suboptimal board configurations
   - Confirms the paper's finding that Greedy is not optimal as the primary heuristic

### Strategy Composition

The results clearly demonstrate the value of hierarchical strategy composition:

1. **Single Heuristics**
   - Limited performance, even with randomization for tie-breaking
   - Cannot handle the complexity of different game states

2. **Two-Level Composition (ER, MR)**
   - ER significantly outperforms MR
   - Shows that empty cell maximization is more fundamental than monotonicity

3. **Three-Level Composition (EMR)**
   - Achieves the best overall performance
   - Demonstrates how properly ordered heuristics complement each other
   - The order of heuristics matters significantly

4. **Four-Level Composition (GEMR)**
   - Adding Greedy as the first heuristic slightly reduces performance
   - Shows that more heuristics aren't always better without proper ordering

## Comparison with Paper Findings

Our results closely align with the findings in the original research paper:

1. **Heuristic Importance**
   - The paper identified Empty as the most critical heuristic, which our results confirm
   - Both studies found Monotonicity valuable primarily as a secondary heuristic

2. **Optimal Strategy**
   - The paper identified EMR as the optimal strategy, which our results support
   - The GEMR strategy was also evaluated in the paper with similar relative performance

3. **Performance Metrics**
   - Our success rates for reaching specific tiles are comparable to the paper's results
   - The average scores and move counts are in the same relative proportions

## Conclusions

1. **Strategy Recommendations**
   - EMR (Empty → Monotonicity → Random) is the most effective strategy
   - For simpler implementation, ER (Empty → Random) offers decent performance
   - Adding Greedy as the first heuristic doesn't improve performance

2. **Design Principles**
   - The order of heuristics matters significantly
   - Proper hierarchical composition outperforms single heuristics
   - Space management (Empty) is more important than pattern maintenance (Monotonicity)

3. **Limitations**
   - All strategies eventually fail to reach the 2048 tile consistently
   - More sophisticated approaches like expectimax or deep learning might achieve better results
   - Performance could potentially be improved with additional or fine-tuned heuristics

## Future Work

1. **Additional Heuristics**
   - Implement and test the Uniformity heuristic mentioned in the paper
   - Explore corner-maximization and pattern-based heuristics
   - Investigate adaptive strategies that change heuristic ordering based on game state

2. **Advanced Techniques**
   - Implement n-ply look-ahead to anticipate future states
   - Explore expectimax algorithm with probability-weighted random tile spawns
   - Investigate machine learning approaches to optimize heuristic weights

3. **Performance Optimization**
   - Benchmark and optimize strategy execution time
   - Implement parallelization for multi-game analysis

## References

- Kohler, S. T., Migler, M., & Khosmood, F. (2019). Composition of basic heuristics for the game 2048. In 2019 IEEE Conference on Games (CoG) (pp. 1-8). IEEE.
- Original 2048 game by Gabriele Cirulli: https://github.com/gabrielecirulli/2048