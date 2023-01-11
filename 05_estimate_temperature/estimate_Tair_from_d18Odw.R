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


#### computed variables ####

nt <- length(tcal$d18Oprecip) # number of calibration data points

at <- sum(tcal_errors$xxbar_by_ttbar)/sum(tcal_errors$ttbar2) # OLS slope

tbar <- mean(tcal_errors$Tair) # mean of Tair

xbar <- mean(tcal_errors$d18Oprecip) # mean of d18Oprecip

bt <- xbar - (at * tbar) # OLS intercept

r2 <- sum(tcal_errors$xxbar_by_ttbar)^2/sum(tcal_errors$ttbar2)/sum(tcal_errors$xxbar2) 

sum_ttbar2 <- sum(tcal_errors$ttbar2)

#### more interim terms for OLS fit ####

tcal_errors <- tcal_errors |>
  dplyr::mutate(xatb2 = (d18Oprecip - (at * Tair) - bt)^2) 
# calculated as (d18Oprecip - (at * ttbar) - xbar)^2 in spreadsheet, but gives the same result

#### calculate error terms ####

sigest <- sqrt(sum(tcal_errors$xatb2)/(nt - 2)) # estimate of the standard deviation of the natural variability in ε

delta_a <- sigest/sqrt(sum_ttbar2) # estimate of the uncertainty of the slope

delta_bbar <- sum(sigest/sqrt(nt)) # estimate of the uncertainty in the fit at t = tbar

#### error curves for the plot ####

error_curves <- data.frame(t = seq(from = min(tcal_errors$Tair), 
                                   to = max(tcal_errors$Tair), by = 0.1)) |>
  dplyr::mutate(x = at*(t - tbar) + xbar, # note wrong notation in excel sheet cell
                dxfit = sigest * sqrt(1/nt + (t - tbar)^2/sum_ttbar2), # 1 s.d. uncertainty on the OLS fit
                dxtot = sigest * sqrt(1 + 1/nt + (t - tbar)^2/sum_ttbar2)) # estimation uncertainty of d18Odw from d18Ophos
# note incorrect notation but correct calculation of formulas in the x, dxfit and dxtot excel cells

#### plot with error curves ####

cal_uncertainty_plot <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_ribbon(data = error_curves, ggplot2::aes(x = t, ymin = x - dxtot, ymax = x + dxtot), 
                       color = NA, fill = "red", alpha = 0.3)+
  ggplot2::geom_ribbon(data = error_curves, ggplot2::aes(x = t, ymin = x - dxfit, ymax = x + dxfit), 
                       color = NA, fill = "magenta", alpha = 0.3)+
  ggplot2::geom_line(data = tcal, ggplot2::aes(x = Tair, y = at * Tair + bt),
                     color = "black", lwd = 1)+
  ggplot2::geom_point(data = tcal, ggplot2::aes(x = Tair, y = d18Oprecip), size = 3)+
  ggplot2::xlim(5, 15)+
  ggplot2::ylim(-12, -5)

cal_uncertainty_plot






