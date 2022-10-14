
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

#### calculate first interim terms for OSL fit uncertainty ####

dwcal %>%
  mutate(xxbar = d18Odw - mean(dwcal$d18Odw),
         xxbar_sqr = (d18Odw - mean(dwcal$d18Odw))^2, 
         yybar = d18Ophos - mean(dwcal$d18Ophos),
         yybar_sqr = yybar^2, 
         yybar_by_xxbar = yybar*xxbar, 
         yaxb_sqr)
  

