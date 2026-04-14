# 
# 
# library(tidyverse)
# library(dplyr)
# library(labelled)

# Load Sample Data ------------------------------------------------------------#


data <- read_csv("Raw Data/hh_mod_h.csv") %>%
  rename(
    rCSILessQlty = hh_h02a,
    rCSIBorrow = hh_h02e,
    rCSIMealNb = hh_h02c,
    rCSIMealSize = hh_h02b,
    rCSIMealAdult    = hh_h02d,
    hhid=HHID
  ) %>%
  select(hhid, rCSILessQlty, rCSIBorrow, rCSIMealNb, rCSIMealSize , rCSIMealAdult)
# Label rCSI relevant variables -----------------------------------------------#

var_label(data$rCSILessQlty)  <- "Relied on less preferred and less expensive food"
var_label(data$rCSIBorrow)    <- "Borrowed food or relied on help from a relative or friend"
var_label(data$rCSIMealNb)    <- "Reduce number of meals eaten in a day"
var_label(data$rCSIMealSize)  <- "Limit portion size of meals at meal times"
var_label(data$rCSIMealAdult) <- "Restricted consumption by adults for small children to eat"

# Calculate rCSI --------------------------------------------------------------# 

rCSI_data <- data %>% mutate(rcsi_fs = rCSILessQlty + 
                          (rCSIBorrow * 2) + 
                          rCSIMealNb + 
                          rCSIMealSize + 
                          (rCSIMealAdult * 3))

var_label(rCSI_data$rcsi_fs)          <- "Reduced coping strategies index (rCSI)"

rCSI_data <- na.omit(rCSI_data)
