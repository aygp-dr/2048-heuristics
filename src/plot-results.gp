#!/usr/bin/gnuplot
#
# Plot results from the strategy distribution analysis
#

# Set terminal type and output file
set terminal png size 1200,900 enhanced font "Helvetica,12"
set output "strategy-comparison.png"

# Set global styles
set style data histogram
set style histogram clustered gap 1
set style fill solid 0.7 border -1
set boxwidth 0.9

# Set multiplot layout
set multiplot layout 2,2 title "2048 Strategy Comparison" font ",14"

# Plot 1: Average Score Comparison
set title "Average Score by Strategy" font ",12"
set xlabel "Strategy"
set ylabel "Score"
set yrange [0:1500]
set grid y
plot 'scores.dat' using 2:xtic(1) title "Average Score" linecolor rgb "#008080"

# Plot 2: Average Moves Comparison
set title "Average Moves by Strategy" font ",12"
set xlabel "Strategy"
set ylabel "Moves"
set yrange [0:300]
set grid y
plot 'moves.dat' using 2:xtic(1) title "Average Moves" linecolor rgb "#800080"

# Plot 3: Highest Tile Distribution
set title "Highest Tile Distribution" font ",12"
set xlabel "Highest Tile Value"
set ylabel "Percentage of Games"
set yrange [0:100]
set grid y
set style histogram rowstacked
plot 'tiles.dat' using 2:xtic(1) title "EMR" linecolor rgb "#008000", \
     '' using 3 title "MR" linecolor rgb "#800000", \
     '' using 4 title "ER" linecolor rgb "#000080", \
     '' using 5 title "GEMR" linecolor rgb "#808000", \
     '' using 6 title "Random" linecolor rgb "#800080"

# Plot 4: Score Efficiency (Score per Move)
set title "Score Efficiency (Score per Move)" font ",12"
set xlabel "Strategy"
set ylabel "Score/Move"
set yrange [0:6]
set grid y
set style histogram
plot 'efficiency.dat' using 2:xtic(1) title "Efficiency" linecolor rgb "#008080"

unset multiplot