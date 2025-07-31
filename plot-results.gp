#!/usr/bin/env gnuplot

# 2048 Strategy Analysis Plot Script

set terminal pngcairo enhanced font 'Arial,12' size 1000,800
set output 'strategy-comparison.png'

set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9

# Multiple plots in one figure
set multiplot layout 2,2 title "2048 Strategy Comparison" font ",14"

# Plot 1: Average Scores
set title "Average Score by Strategy" font ",12"
set xlabel "Strategy"
set ylabel "Average Score"
set yrange [0:*]
set grid y
set xtics rotate by -45
plot 'strategy-results.dat' using 2:xtic(1) title "Avg Score" linecolor rgb "#4169E1"

# Plot 2: Max Tile Distribution
set title "Highest Tile Reached" font ",12"
set xlabel "Strategy"
set ylabel "Highest Tile Value"
set yrange [0:*]
set grid y
plot 'strategy-results.dat' using 3:xtic(1) title "Max Tile" linecolor rgb "#FF6347"

# Plot 3: Success Rates (128, 256)
set title "Success Rates (%)" font ",12"
set xlabel "Strategy"
set ylabel "Percentage of Games"
set yrange [0:100]
set grid y
plot 'strategy-results.dat' using 4:xtic(1) title "Reached 128" linecolor rgb "#32CD32", \
     'strategy-results.dat' using 5:xtic(1) title "Reached 256" linecolor rgb "#FFD700"

# Plot 4: Success Rates (512)
set title "Reaching 512 Tile (%)" font ",12"
set xlabel "Strategy"
set ylabel "Percentage of Games"
set yrange [0:30]
set grid y
plot 'strategy-results.dat' using 6:xtic(1) title "Reached 512" linecolor rgb "#FF4500"

unset multiplot

# Create a second visualization for paper comparison
set terminal pngcairo enhanced font 'Arial,12' size 800,600
set output 'paper-comparison.png'

set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9

set title "Strategy Performance Comparison" font ",14"
set xlabel "Strategy"
set ylabel "Average Score"
set yrange [0:*]
set grid y
set key top left

plot 'strategy-results.dat' using 2:xtic(1) title "Avg Score" linecolor rgb "#4169E1", \
     'strategy-results.dat' using 5:xtic(1) title "% Games Reaching 256" linecolor rgb "#FFD700"