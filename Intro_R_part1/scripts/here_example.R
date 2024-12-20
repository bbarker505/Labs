# Load the R package 'here'
library(here)

# Relative path to a file called "file.txt"
# Notice that the full path is returned in the console
here("Intro_R_part1", "files", "file.txt")

# Load the file
readLines(here("Intro_R_part1", "files", "file.txt"))

# This is the faster and easier way to access this file than:
readLines("/home/barkebri/Labs/Intro_R_part1/files/file.txt")

# Also, you'd have to change the absolute path every time you move files
# For example, if you copy labs from this course to your own computer and
# don't use relative paths, you'd have to change every single line of code
# where you import files. 