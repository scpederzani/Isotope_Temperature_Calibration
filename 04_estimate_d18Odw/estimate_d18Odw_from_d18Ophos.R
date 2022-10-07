
# this script will convert bioapatite phosphate d18O values to estimates of d18O of drinking water
# equivalent to the Z1 step in Pryor et al. 2014

# read the d18Odw-d18Ophos calibration data

dwcal <- read.csv("02_calibration_data/Pryor_Equus_d18Odw_d18Ophos_calibration_data.csv")

head(dwcal)
