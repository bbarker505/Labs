# Load the R package 'here'
library(here)

# Relative path to a file called "file.txt"
# Notice that the full path is returned in the console
here("Intro_R_part1", "files", "file.txt")

# Load the file using a relative path
readLines(here("Intro_R_part1", "files", "file1.txt"))

# You can also load a file using an absolute path
# But this method is more inefficient 
readLines("/home/barkebri/Labs/Intro_R_part1/files/file2.txt")