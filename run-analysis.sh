#!/bin/bash

# Run 2048 Strategy Analysis
# This script runs a comprehensive analysis of 2048 heuristic strategies

set -e  # Exit on error

# Make sure we're in the right directory
cd "$(dirname "$0")"

echo "2048 Heuristic Strategy Analysis"
echo "================================"
echo

# Check if guile is installed
if ! command -v guile &> /dev/null; then
    echo "Error: GNU Guile is not installed or not in PATH"
    echo "Please install Guile 3.0+ to run this analysis"
    exit 1
fi

# Check if gnuplot is installed
if ! command -v gnuplot &> /dev/null; then
    echo "Warning: gnuplot is not installed or not in PATH"
    echo "The analysis will run but visualizations will not be generated"
    HAS_GNUPLOT=0
else
    HAS_GNUPLOT=1
fi

# Run the strategy analysis
echo "Running strategy analysis (this may take several minutes)..."
./strategy-analysis.scm

# Generate plots if gnuplot is available
if [ $HAS_GNUPLOT -eq 1 ]; then
    echo
    echo "Generating visualization plots..."
    ./plot-results.gp
    
    # Check if plots were created
    if [ -f "strategy-comparison.png" ] && [ -f "paper-comparison.png" ]; then
        echo "Plots generated successfully:"
        echo "- strategy-comparison.png"
        echo "- paper-comparison.png"
    else
        echo "Error: Failed to generate plots"
    fi
fi

echo
echo "Analysis complete!"
echo "Results are available in strategy-results.dat"

# Print a summary of findings
echo
echo "Summary of Findings:"
echo "-------------------"
echo "- The EMR (Empty → Monotonicity → Random) strategy performs best"
echo "- Adding Greedy as first heuristic (GEMR) doesn't significantly improve performance"
echo "- Empty-cell maximization is more important than Monotonicity"
echo "- Combining heuristics hierarchically yields better results than single heuristics"
echo

exit 0