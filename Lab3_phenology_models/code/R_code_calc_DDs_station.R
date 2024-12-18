# R script to calculate daily degree-days and degree day accumulation 
# over time using daily minumum and maximum temperature (Tmin, Tmax) data
# from a single weather station. 
# Inputs: Tmin and Tmax data in a tab delimited text file
# Output: Plot of degree-day accumulation over the dates of input rasters
# Running script: type "Rscript R_code_calc_DDs_station.R" 

# Set directory 
dir <- "C:/Users/barkebri/Documents/Teaching/HORT499.599/Wint23/Labs/Lab2"

# Load the required R libraries
#library(here)
library(ggplot2)
library(dplyr)

#Set the lower developmental threshold (LDT) in Celsius
LDT <- 10 

# Import and format weather data
# First line is skipped because it's descriptive info (not data)
weath_data <- read.table(paste0(dir, "/weather_data/CRVO21.txt"), skip = 1)
names(weath_data) <- c("month", "day", "tmax", "tmin", "prec", "dd50")

# Loop through each row of weather data, calculating degree days for each day
# (dd_today) and then accumulating them over all days (DDaccum)
# Create an empty data frame to store accumulated DDs
out_all <- data.frame(matrix(ncol = 4, nrow = 0))
names(out_all) <- c("month", "day", "dd_today", "dd_accum")

# Number of days for daily time step
number_of_days <- nrow(weath_data)

# Initialize an object for degree-day accumulation
dd_accum <- 0

# Step through days of year
for (i in 1:number_of_days) {
  
  # Tmin and Tmax for day of year, but convert from degrees F to C
  tmax <- (weath_data$tmax[i] - 32) * 5/9
  tmin <- (weath_data$tmin[i] - 32) * 5/9
  
  # Use simple average DDs, more complex formulas are available and preferable
  dd_today <- (tmax + tmin/2) - LDT
  
  # Can't have negative degree-days (change to 0)
  if (dd_today < 0) {
    dd_today <- 0
  }
  
  # Use "dd_today" as first entry if day = 1, or else accumulate off previous day
  #if (dd_accum == 0) {
  #  dd_accum <- dd_today
  #} else {
    dd_accum <- dd_accum + dd_today
  #}
  
  # Put results for day in a data frame
  out_i <- data.frame("doy" = i,
                      "month" = weath_data$month[i],
                      "month_day" = weath_data$day[i],
                      "dd_today" = dd_today,
                      "dd_accum" = dd_accum)
  
  # Attach results for day to all results 
  out_all <- bind_rows(out_all, out_i) 
  
}

# Create and save a simple plot

# New set of x-axis labels (dates vs. day of year)
days <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365)
x_labels <- paste(c(unique(out_all$month), 1), 1, sep = "-")

# Plot
dd_plot <- ggplot(data = out_all, aes(x = doy, y = dd_accum)) +
  geom_line() + # for trend lines, time series, etc.
  theme_bw() + # simple black and white plot theme
  ylab("Cumulative DDs  (LDT = 10C)") + # y-axis title
  xlab("Day of year - 2021") + # x-axis title
  # Both axes start at 0, x-axis uses custom breaks and labels
  scale_x_continuous(expand = c(0, 0), breaks = days, labels = x_labels) +
  # Make the y-axis 100 units larger than the max value in data
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(out_all$dd_accum) + 100))

# Save plot
ggsave(dd_plot, 
       filename = paste0(dir, "/DDaccum_R_Rscript.png"),
       height = 3, width = 5, units = c('in'), device = "png", dpi = 300)
