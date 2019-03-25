Pandoc test

pandoc -f markdown+lhs assignment.lhs -V papersize:a4 -V fontsize:12pt -V geometry:margin=1in -o assignment.pdf