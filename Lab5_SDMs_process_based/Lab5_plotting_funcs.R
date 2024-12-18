#### Functions for Lab 5: Species distribution modeling
#### Ecological Systems Modeling (Winter 2024)
#### Author: Brittany Barker

## Map production in ggplot requires specifying plot.height and plot.width
# These need to be dynamic because extents have different aspect ratios, 
# which results warped looking maps
# Calculate bounding box (xmin, xmax, ymin, ymax) of extent 
extent <- c(-125.0, -111.02, 31.06, 49.4)
coord <- coord_quickmap(xlim = c(extent[1], extent[2]), 
                        ylim = c(extent[3], extent[4]), expand = FALSE)
asp <- coord$aspect(list(x.range = c(extent[1], extent[2]), 
                         y.range = c(extent[3], extent[4]))) # aspect ratio

# Adjust base_size for ggplot2 (font size) according to aspect ratio
if (asp >= 1.7) {
  base_size <- 10.5
  legend_units <- 1.4
} else if (asp >= 1.5 & asp < 1.7) {
  base_size <- 9.5
  legend_units <- 1.3
} else if (asp >= 1.2 & asp < 1.5) {
  base_size <- 8.5 
  legend_units <- 1.2
} else if (asp < 1.2 & asp >= 0.6) {
  base_size <- 7
  legend_units <- 1
} else if (asp < 0.6 & asp >= 0.45) {
  base_size <- 5.7
  legend_units <- 0.6
} else if (asp < 0.45) {
  base_size <- 5.2
  legend_units <- asp
}

# States feature for plots
states <- ne_states(returnclass = "sf") %>%
  filter(geonunit == "United States of America" & !name %in% c("Alaska", "Hawaii")) %>%
  st_cast("MULTILINESTRING") %>%
  st_crop(ext(extent))

# Theme to use for plots
mytheme <- theme(legend.text = element_text(size = rel(1.5)), 
                 legend.title = element_text(size = rel(1.5), face = "bold"),
                 legend.position = "right", 
                 legend.justification = "left",
                 legend.margin = margin(t = 0, r = 0.10, b = 0, l = 0.10, unit = "cm"),
                 legend.key.width = unit(legend_units, "line"), 
                 legend.key.height = unit(legend_units, "line"),
                 plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5), 
                 plot.margin = margin(t = 0.05, r = 0.25, b = 0.05, l = 0.25, unit = "cm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_rect("white"), panel.border = element_blank(),
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank())

##### (1). Convert raster into a data frame for plotting ####
ConvDF <- function(rast) {
  df <- terra::as.data.frame(rast, xy = TRUE)
  colnames(df) <- c("x", "y", "value")
  return(df)
}

#### (2). Cut_bins: classify data into bins for plotting ####
# Classify data (= df) so it can be visualized in categories 
# (e.g., 1-10, 11-20, 21-30) 
Cut_bins <- function(df, breaks) {
  df$value_orig <- df$value # Keep old value so can sort factors against it
  # Round up max value to highest number divisible by 10
  df$value[df$value == max(df$value)] <- 10 * ceiling(max(df$value)/10)
  # Cut values into bins and format results
  df2 <- df %>% mutate(value = cut_interval(df$value, n = breaks),
                       value = gsub("[()]|\\[|\\]", "", value),
                       bin1 = ceiling(as.numeric(str_split_fixed(value, ",", 2)[,1])),
                       bin2 = ceiling(as.numeric(str_split_fixed(value, ",", 2)[,2])),
                       value = paste(bin1, bin2, sep = "-"))
  return(df2)
}

#### (3). Base_map: base map for summary plots ####
# Base features used for all summary (PNG) maps in "PlotMap" function
# The "coord_quickmap" function allows the state/region of interest to be plotted
# geom_raster is faster than geom_tile
# Input data are in a data frame (= df) format
Base_map <- function(output_df) {
  # Base map plot
  p <- ggplot() + 
    geom_raster(data = output_df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = states) 
}

#### Plotting for DDtotal, NumGen, ColdEXCL, HeatEXCL, and AllEXCL
#### (4.) PlotMap ####
PlotMap <- function(r, yr, type) {
  
  # Convert raster to data frame
  df <- terra::as.data.frame(r, xy = TRUE)
  colnames(df) <- c("x", "y", "value")
  
  # Need to enforce a rule for wrapping title and subtitle on plot
  # The title and subtitle go off of page for some states (VT...any else?)
  if (asp > 1.55) {
    titl_width <- 45
  } else if (asp >= 1.4 & asp < 1.55) {
    titl_width <- 55
  } else {
    titl_width <- 65
  }
  
  #### * DDtotal ####
  if (type == "Total DDs") {
    
    # Create plot separately for rasters where all DDtotal values = 0  
    if (all(df$value == 0)) {
      df$value <- factor(df$value)
      # If there are any non-zero values, then cut values into bins
    } else {
      df <- Cut_bins(df, 10)  
      df$value <- factor(df$value, 
                         levels = unique(df$value[order(df$value_orig)]))
    }
    
    # DDtotal plot
    p <- Base_map(df) +       
      scale_fill_viridis_d(type, option = "C", direction = -1) +
      labs(title = str_wrap(paste(spp, type, yr), width = titl_width)) +
     theme_map(base_size = base_size) + 
      mytheme
    
    #### * NumGen ####
  } else if (type == "Number of generations") {
    
    # Value must be a factor for plotting
    df$value <- factor(df$value)
    
    # NumGen plot
    p <- Base_map(df) +       
      scale_fill_viridis_d(type, option = "H") +
      labs(title = str_wrap(paste(spp, type, yr), width = titl_width)) +
      guides(fill = guide_legend(title = "No. gens")) +
     # theme_map(base_size = base_size) + 
      mytheme
    
    #### * Stress exclusion ####
  } else if (grepl("exclusion", type)) {
    
    # Value must be a factor for plotting
    df$value <- factor(df$value)
    
    # Re-assign values (0, -1, -2) to their corresponding description
    df <- df %>%
      mutate(value = ifelse(value == -2, "excl.-severe", 
                            ifelse(value == -1, "excl.-moderate", 
                                   ifelse(value == 0, "not excluded",NA)))) %>%
      mutate(value = factor(value, levels = c("excl.-severe", "excl.-moderate", "not excluded")))
    
    # Color palette
    # -2 = sev. strs. excl., -1 = mod. strs. excl., 0 = no strs. excl.
    pal <- c("gray30", "gray70", "green2")
    
    # Stress exclusion plot
    p <- Base_map(df) +       
      scale_fill_manual(values = pal, name = "Stress exclusion") +
      labs(title = str_wrap(paste(spp, type, yr), width = titl_width)) +
      theme_map(base_size = base_size) + 
      mytheme
  }
  
  # Save plot
  fl_name <- paste0(spp, "_", gsub(" ", "_", type), "_", yr, ".png")
  ggsave(p, file = paste0(output_dir, "/", fl_name), 
         height = asp * 6, width = asp * 6, units = c('in'), dpi = 300)
}

### Plots for cold and heat stress accumulation

#### (5). Format values for climate stress plotting ####
# Deal with 0 vs non-0 values when plotting cold and heat stress unit rasters
# Only create bins (0-10, etc...) if there are non-zero values in the data 
Stress_Val_Conv <- function(x) {
  if (all(x$value == 0)) {
    x2 <- x
  } else {
    x2 <- Cut_bins(x, 10)
  }
  # Need to fix bins if all data are < 10 
  if (all(x2$value_orig < 10)) {
    x2$value <- "0-10"
  }
  x2$value <- factor(x2$value, 
                     levels = unique(x2$value[order(x2$value_orig)]))
  return(x2)
}

#### (9). PlotMap_stress ####
PlotMap_stress <- function(rast, year, type, max1, max2) {
  
  # Format stress units values
  df <- ConvDF(rast)
  df$value_orig <- df$value
  df2 <- Stress_Val_Conv(df) # Properly formats values
  
  # Need to wrap title
  if (asp > 1.55) {
    titl_width <- 45
  } else {
    titl_width <- 55
  }
  
  # Create contours from raster values greater than limit 1 and limit 2, if 
  # raster values are greater than max1 and/or max2
  max1_c <- tryCatch(
    {
      # Convert SpatVector to sf object
      max1_c <- as.contour(rast > max1)
      max1_c <- st_as_sf(max1_c)
    },
    error = function(e) {
      max1_c <- 0 
    } )
  max2_c <- tryCatch(
    {
      # Convert SpatVector to sf object
      max2_c <- as.contour(rast > max2) 
      max2_c <- st_as_sf(max2_c)
    },
    error = function(e) {
      max2_c <- 0 
    } )
  
  # If all values are 0 or are less than 10 (if stress limit < 10), then don't 
  # include contours (must include this code or it will throw an error)
  if (all(df$value == 0 | all(df$value < 10 & all(df$value < max1)))) {
    p <- Base_map(df2) +
      scale_fill_manual(values = c("#5E4FA2"), name = type) +
      labs(title = str_wrap(paste(spp, type, yr), width = titl_width)) +
     # theme_map(base_size = base_size) + 
      mytheme 
    
    # If any values are greater than 0, but not greater than max1, then don't 
    # plot either countour (must include or it will thrown an error)
  } else if (any(df$value > 0) & is.numeric(max1_c)) {
    p <- Base_map(df2) +
      scale_fill_brewer(palette = "Spectral", direction = -1, 
                        name = str_wrap(type, width = 15)) +
      labs(title = str_wrap(paste(spp, type, yr), width = titl_width)) +
      theme_map(base_size = base_size) + 
      mytheme
    
    # If any values are greater than limit 1 but less than limit 2, then 
    # plot countour line for just limit 1
  } else if (any(df$value > 0) & !is.numeric(max1_c) & is.numeric(max2_c)) { 
    # max1_c is class "SpatialLinesDataFrame" but max2_c is class "numeric" 
    # (max2_c = 0)
    p <- Base_map(df2) +
      scale_fill_brewer(palette = "Spectral", direction = -1, 
                        name = str_wrap(type, width = 15)) +
      scale_color_manual(name = "Stress Limits", 
                         values = c("Stress limit 1" = "magenta")) +
      geom_sf(data = max1_c, aes(color = "Stress limit 1"), lwd = 0.15) +
      labs(title = str_wrap(paste(spp, type, yr), width = titl_width)) +
      theme_map(base_size = base_size) + 
      mytheme 
    
    # If any values are greater than limit1 and limit2, then plot 
    # countour lines for both limit1 and limit2
  } else if (!is.numeric(max1_c) & !is.numeric(max2_c)) {
    p <- Base_map(df2) +
      scale_fill_brewer(palette = "Spectral", direction = -1, 
                        name = str_wrap(type, width = 15)) +
      scale_color_manual(name = "Stress Limits", 
                         values = c("Stress limit 1" = "magenta", 
                                    "Stress limit 2" = "mediumblue")) +
      geom_sf(data = max1_c, aes(color = "Stress limit 1"), lwd = 0.15) +
      geom_sf(data = max2_c, aes(color = "Stress limit 2"), lwd = 0.15) +
      labs(title = str_wrap(paste(spp, type, yr), width = titl_width)) +
      theme_map(base_size = base_size) + 
      mytheme 
  }
  
  # Save plot
  fl_name <- paste0(spp, "_", gsub(" ", "_", type), "_", yr, ".png")
  ggsave(p, file = here(output_dir, fl_name), 
         height = asp * 7, width = asp * 7, units = c('in'), dpi = 300)
  
}

