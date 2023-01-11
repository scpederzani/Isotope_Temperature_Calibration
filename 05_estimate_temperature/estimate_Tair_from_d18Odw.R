# this script will convert water d18O values to estimates of air temperature
# equivalent to the Z2 step in Pryor et al. 2014

#### load libraries ####
library(ggplot2)
library(dplyr)

# read the Tair-d18Oprecip calibration data

tcal <- read.csv("02_calibration_data/Pryor_Europe_Tair_d18Oprecip_calibration_data.csv")

head(tcal)

#### initial plot ####

ggplot2::ggplot(tcal, ggplot2::aes(x = Tair, y = d18Oprecip))+
  ggplot2::theme_bw()+
  ggplot2::geom_point(size = 3)+
  ggplot2::xlim(5, 15)+
  ggplot2::ylim(-12, -5)

#### calculate first interim terms for OLS fit uncertainty ####

tcal_errors <- tcal |>
  dplyr::mutate(ttbar = Tair - mean(tcal$Tair),
                ttbar2 = ttbar^2, 
                xxbar = d18Oprecip - mean(tcal$d18Oprecip), # for now following Pryor variable names
                xxbar2 = xxbar^2, # where x is d18Owater throughout, but might change later
                xxbar_by_ttbar = xxbar*ttbar) # because it means x is now on the y-axis here







