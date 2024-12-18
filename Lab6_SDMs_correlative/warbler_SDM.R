library(here)
library(dismo)
library(terra)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)

sf_use_s2(FALSE)


# Theme to use for plots
mytheme <- theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_rect("white"), panel.border = element_blank(),
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank())

# Environmental layers
env_data_current <- rast(here("Lab6_SDMs_correlative", "layers", "warbler", "env_current_na.grd"))
env_data_forecast <- rast(here("Lab6_SDMs_correlative", "layers", "warbler", "env_forecast_na.grd"))

# States feature for plots
north_america <- ne_countries(scale = 10, type = "countries", returnclass = "sf")  %>%
  st_cast("MULTILINESTRING") %>%
  st_crop(ext(env_data_current)) 
states <- ne_states(returnclass = "sf") %>%
  st_cast("MULTILINESTRING") %>%
  st_crop(ext(env_data_current)) 

hooded_warb_data <- read.csv(
  here("Lab6_SDMs_correlative", "locations", "hooded_warb_locations.csv"))
head(hooded_warb_data)

ext <- extent(c(xmin=-170.449222,xmax=-48.984379,ymin=24.590273,ymax=71.333694))


plot(env_data_current$tmin)
plot(env_data_current$precip)

hooded_warb_data <- env_data_current %>% 
  extract(select(hooded_warb_data, lon, lat)) %>% 
  bind_cols(hooded_warb_data)  %>% 
  mutate(present = as.factor(present))

ggplot(hooded_warb_data, aes(x = tmin, y = precip, color = present)) +
  geom_point() +
  scale_color_manual(values=c("0" = "gray70", "1" = "forestgreen")) +
  theme_bw()

ggplot() +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  geom_point(data = hooded_warb_data, aes(x = lon, y = lat), size = 0.3, color = "red")

# GLM - 1 variable
hooded_warb_data <- hooded_warb_data %>% 
  mutate(present_num = as.numeric(as.character(present)))

# GLM for Tmin only
log_regr_model_tmin <- glm(
  present ~ tmin, 
  family= binomial(link="logit"), 
  data = hooded_warb_data)

# x-axis
xtmin <-seq(-100, 225, 25)

# Prediction
ytmin <- predict(
  log_regr_model_tmin, 
  list(tmin=xtmin),type="response")


xy_tmin <- data.frame(x=xtmin, y=ytmin)

ggplot(hooded_warb_data, aes(x = tmin, y = present_num)) +
  geom_point() +
  geom_line(data=xy_tmin, aes(x=x,y=y)) +
  xlab("Tmin") + 
  ylab("Probability") +
  theme_bw()

# GLM for precip only
log_regr_model_precip <- glm(
  present ~ precip, 
  family= binomial(link="logit"), 
  data = hooded_warb_data)

# X-axis
xprecip <-seq(0, 175, 25)

# Prediction
yprecip <- predict(
  log_regr_model_precip, 
  list(precip=xprecip),type="response")


xy_precip <- data.frame(x=xprecip, y=yprecip)

ggplot(hooded_warb_data, aes(x = precip, y = present_num)) +
  geom_point() +
  geom_line(data=xy_precip, aes(x=x,y=y)) +
  xlab("Precip") + 
  ylab("Probability") +
  theme_bw()

# GLM - both Tmin and Precip
log_regr_model_both <- glm(
  present ~ tmin + precip,
  family = binomial(link = "logit"),
  data = hooded_warb_data)

# X-axis
xboth <- seq(0, 175, 25)

# Prediction
yboth <- predict(
  log_regr_model_both, 
  list(tmin=xboth, precip=xboth), type="response")
xy_both <- data.frame(x=xboth, y=yboth)


ggplot(hooded_warb_data, aes(x = precip, y = present_num)) +
  geom_point() +
  geom_line(data=xy_both, aes(x=x,y=y)) +
  xlab("Combo (Tmin, Precip)") + 
  ylab("Probability") +
  theme_bw()

logistic_regr_model <- log_regr_model_both
predictions_rast <- predict(env_data_current, logistic_regr_model, type = "response")

predictions <- as.data.frame(predictions_rast, xy = TRUE) %>%
  rename("probability" = "lyr1")
present_loc <- select(filter(hooded_warb_data, present == 1), lon, lat)

p <- ggplot() +
  geom_raster(data = predictions, aes(x = x, y = y, fill = probability)) +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  scale_fill_viridis_c(option = "turbo", limits = c(0,1)) +
  ggtitle("GLM - Probability of Hooded Warbler Presence") +
  mytheme
p

p + 
  geom_point(data=present_loc, aes(x = lon, y = lat), shape = 1)


predictions_gt0.5 <- filter(predictions_df, probability > 0.5)

p_gt0.5 <- ggplot() +
  geom_tile(data = predictions_gt0.5,
              aes(x = x, y = y, fill = probability)) +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  scale_fill_viridis_c(option = "turbo", limits = c(0,1)) +
  ggtitle("GLM - Probability of Hooded Warbler Presence > 0.5") +
  mytheme
p_gt0.5

# Split warbler dataset by presence vs. absence
presence_data <- filter(hooded_warb_data, present ==  1)
absence_data <- filter(hooded_warb_data, present == 0)

# Evaluate predictions using ROC
evaluation <- evaluate(presence_data, absence_data, logistic_regr_model)
plot(evaluation, 'ROC')

# Threshold to define presence vs. absence 
thresh <- threshold(evaluation, stat = 'prevalence')

# Retain only predictions above the estimated threshold
predictions_gtThrs <- filter(predictions, probability > thresh)

# Proability of presence using a threshold value of 0.5
p_gtThrs <- ggplot() +
  geom_tile(data = predictions_gtThrs,
            aes(x = x, y = y, fill = probability)) +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  scale_fill_viridis_c(option = "turbo", limits = c(0,1)) +
  ggtitle("GLM - Probability of Hooded Warbler Presence > 0.2") +
  mytheme
p_gtThrs

# Predict presence under future climatic conditions
# Raster result
forecasts_rast <- predict(env_data_forecast, logistic_regr_model, type = "response")
# Conver to data frame
forecasts <- as.data.frame(forecasts_rast, xy = TRUE) %>%
  rename("probability" = "lyr1")

# Plot of Probability of Hooded Warbler Presence under future climate
p_forecasts <- ggplot() +
  geom_tile(data = forecasts,
            aes(x = x, y = y, fill = probability)) +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  scale_fill_viridis_c(option = "turbo", limits = c(0,1)) +
  labs(title = "GLM - Probability of Hooded Warbler Presence in 50 years") +
  mytheme
p_forecasts

# Calculate predicted - forecasted
#i.e., probability under current climate - probability under future climate
differences <- forecasts %>%
  bind_cols(., select(predictions, "probability_pred" = probability)) %>%
  mutate(difference = probability - probability_pred)

# Plot differences 
p_diff <- ggplot() +
  geom_tile(data = differences, aes(x = x, y = y, fill = difference)) +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  scale_fill_viridis_c(option = "turbo", limits = c(-0.75,0.5)) +
  labs(title ="GLM - Change in Probability of Hooded Warbler Presence (Future - Current)") +
  mytheme
p_diff

# Plot of differences with warbler presences
p_diff + 
  geom_point(data=present_loc, aes(x = lon, y = lat), shape = 1)

# Extract environmental data for points and conduct MESS analysis
# Binning data as factors gives a nicer looking plot
xy <- select(presence_data, lon, lat)
ref_pts <- extract(env_data_current, xy) %>%
  select(tmin, precip)

# Conduct MESS analysis
# Negative values indicate that at least one variable has a value which is outside the range of the environmental range of the training data points
# Positive values indicate that environmental conditions are similar to the training data points
# Older "raster" package required - doesn't seem like dismo has updated to "terra"
mess_out <- dismo::mess(raster::stack(env_data_forecast), ref_pts, full = FALSE) 
mess_out_df <- as.data.frame(mess_out, xy = TRUE)

# Plot the MESS results
ggplot() +
  geom_tile(data = mess_out_df, aes(x = x, y = y, fill = mess)) +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  scale_fill_gradient2(low = "midnightblue", mid = "white", high = "red") +
  labs(title = "MESS Analysis - Model Uncertainty Associated With Extrapolation") +
  mytheme

# Pixels for which at least one environmental variable has a value outside the range of values in presence records (calibration data), indicating model extrapolation
# These are negative values
extrapolation_areas <- mess_out < 0
extrapolation_areas_df <- as.data.frame(extrapolation_areas, xy = TRUE) %>%
  rename("extrapolation" = layer) %>%
  filter(extrapolation == 1)
  
# Mask out geographic areas where model extrapolation occurs - these are non-analogous climates and predictions shoudln't be interpreted
ggplot() +
  geom_tile(data = forecasts,
            aes(x = x, y = y, fill = probability)) +
  geom_tile(data = extrapolation_areas_df, aes(x = x, y = y), fill = "gray80") +
  geom_sf(data = north_america, linewidth = 0.1, color = "gray20") +
  geom_sf(data = states, linewidth = 0.1, color = "gray20") +
  scale_fill_viridis_c(option = "turbo", limits = c(0,1)) +
  labs(title = "GLM - Probability of Hooded Warbler Presence in 50 years",
       subtitle = "Predictions in Areas With no Extrapolation Errors") +
  mytheme
  
  
# Exercise 2
boxb_data <- read.csv(here("Lab6_SDMs_correlative", "locations", "Cps_Eurasia_locations.csv"))
boxb_data <- as.matrix(boxb_data)
eur_clim_data <- raster::stack(rast(list.files(
  here("Lab6_SDMs_correlative", "layers", "boxb", "predictors"), 
  pattern = ".asc$", full.names = TRUE)))
world_clim_data <- raster::stack(rast(list.files(
  here("Lab6_SDMs_correlative", "layers", "boxb", "projection"), 
  pattern = ".asc$", full.names = TRUE)))

test <- bioclim(eur_clim_data, boxb_data)


#or
v <- extract(eur_clim_data, boxb_data)
bc <- bioclim(v)
p1 <- predict(eur_clim_data, bc)
p2 <- predict(world_clim_data, bc)

## Maxent
set.seed(0)
group <- kfold(boxb_data, 3)
pres_train <- boxb_data[group != 1, ]
pres_test <- boxb_data[group == 1, ]
m <- maxent(eur_clim_data, pres_train)
p3 <- predict(m, eur_clim_data)
plot(p3)
points(boxb_data)
