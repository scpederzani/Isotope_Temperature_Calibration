
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





