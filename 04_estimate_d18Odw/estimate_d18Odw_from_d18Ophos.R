
# this script will convert bioapatite phosphate d18O values to estimates of d18O of drinking water
# equivalent to the Z1 step in Pryor et al. 2014

#### load libraries ####
library(ggplot2)
library(dplyr)

# read the d18Odw-d18Ophos calibration data

dwcal <- read.csv("02_calibration_data/Pryor_Equus_d18Odw_d18Ophos_calibration_data.csv")

head(dwcal)

#### initial plot ####

ggplot(dwcal, aes(x = d18Odw, y = d18Ophos))+
  theme_bw()+
  geom_point(size = 3)+
  xlim(-20, 5)+
  ylim(0, 30)

#### calculate first interim terms for OLS fit uncertainty ####

dwcal_errors <- dwcal %>%
  mutate(xxbar = d18Odw - mean(dwcal$d18Odw),
         xxbar2 = (d18Odw - mean(dwcal$d18Odw))^2, 
         yybar = d18Ophos - mean(dwcal$d18Ophos),
         yybar2 = yybar^2, 
         yybar_by_xxbar = yybar*xxbar)


#### computed variables ####

n <- length(dwcal$d18Ophos) # number of calibration data points

a <- sum(dwcal_errors$yybar_by_xxbar)/sum(dwcal_errors$xxbar2) # OLS slope

xbar <- mean(dwcal_errors$d18Odw) # mean of d18Odw (x)

ybar <- mean(dwcal_errors$d18Ophos) # mean of d18Ophos (y)

b <- ybar - (a * xbar) # OLS intercept

r2 <- sum(dwcal_errors$yybar_by_xxbar)^2/sum(dwcal_errors$xxbar2)/sum(dwcal_errors$yybar2) 

sum_xxbar2 <- sum(dwcal_errors$xxbar2)

#### more interim terms for OLS fit ####

dwcal_errors <- dwcal_errors %>%
  mutate(yaxb2 = (d18Ophos - (a * d18Odw) - b)^2)

#### calculate error terms ####

sigest <- sqrt(sum(dwcal_errors$yaxb2)/(n - 2)) # estimate of the standard deviation of the natural variability in ε

delta_a <- sigest/sqrt(sum_xxbar2) # estimate of the uncertainty of the slope

delta_bbar <- sum(sigest/sqrt(n)) # estimate of the uncertainty in the fit at x = xbar

#### error curves for the plot ####

error_curves <- data.frame(x = seq(from = min(dwcal_errors$d18Odw), to = max(dwcal_errors$d18Odw), by = 1)) %>%
  mutate(y = a*(x - xbar) + ybar, 
         dyfit = sigest * sqrt(1/n + (x - xbar)^2/sum_xxbar2), # 1 s.d. uncertainty on the OLS fit
         dytot = sigest * sqrt(1 + 1/n + (x - xbar)^2/sum_xxbar2)) # estimation uncertainty of d18Odw from d18Ophos



#### plot with error curves ####

ggplot()+
  theme_bw()+
  geom_ribbon(data = error_curves, aes(x = x, ymin = y - dytot, ymax = y + dytot), color = NA, fill = "red", alpha = 0.3)+
  geom_ribbon(data = error_curves, aes(x = x, ymin = y - dyfit, ymax = y + dyfit), color = NA, fill = "magenta", alpha = 0.3)+
  geom_line(data = dwcal, aes(x = d18Odw, y = a * d18Odw + b), color = "black", lwd = 1)+
  geom_point(data = dwcal, aes(x = d18Odw, y = d18Ophos), size = 3)+
  xlim(-20, 5)+
  ylim(0, 30)

#### calibrate example data ####


z1_input <- read.csv("03_input_to_calibrate/equus_example_d18O_input.csv")

z1_input

# calibrate each d18Ophos point individually

d18Odw_est_individual <- z1_input %>%
  mutate(est_d18Odw_i = round(xbar + (d18Ophos - ybar)/a, 1), 
         est_d18O_error_i = round((sigest/a)*sqrt(1 + 1/n + (d18Ophos - ybar)^2/sum_xxbar2), 1))

# calculate mean d18Odw estimate (grouped by layer)

d18Odw_est_group <- z1_input %>%
  add_count(layer, name = "m") %>%
  group_by(site, taxon, layer, m) %>%
  summarise(mean_d18Ophos = round(mean(d18Ophos, na.rm = TRUE), 1)) %>%
  mutate(est_d18Odw_group = round(xbar + (mean_d18Ophos - ybar)/a, 1), 
         est_d18Odw_group_error = round((sigest/a)*sqrt(1/m + 1/n + (mean_d18Ophos - ybar)^2/a^2/sum_xxbar2), 2))


#### export calibrated data ####

write.csv(d18Odw_est_individual, file = "04_estimate_d18Odw/output/individual_d18Odw_estimates.csv", row.names = FALSE)

write.csv(d18Odw_est_group, file = "04_estimate_d18Odw/output/layer_d18Odw_estimates.csv", row.names = FALSE)








