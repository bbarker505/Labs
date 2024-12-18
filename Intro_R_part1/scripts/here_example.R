# Load the R package 'here'
library(here)

# Relative path to a file called "file.txt"
# Notice that the full path is returned in the console
here("Intro_R_part1", "files", "file.txt")

# Load the file
readLines(here("Intro_R_part1", "files", "file.txt"))