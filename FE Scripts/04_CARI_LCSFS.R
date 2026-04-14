library(tidyverse)

# Read the data
data <- read.csv("Raw Data/hh_mod_u.csv")

# Step 1: Create fs per row based on conditions
data <- data %>%
  rowwise() %>%
  mutate(fs = case_when(
    5 %in% c(hh_u04a, hh_u04b, hh_u04c) & any(c(hh_u04a, hh_u04b, hh_u04c) %in% c(1, 17, 8, 10)) ~ 2,
    5 %in% c(hh_u04a, hh_u04b, hh_u04c) & any(c(hh_u04a, hh_u04b, hh_u04c) %in% c(11, 14, 15, 9)) ~ 3,
    5 %in% c(hh_u04a, hh_u04b, hh_u04c) & any(c(hh_u04a, hh_u04b, hh_u04c) %in% c(13)) ~ 4,
    TRUE ~ 1
  )) %>%
  ungroup()

# Step 2: Get the maximum fs per HHID
fs_summary <- data %>%
  group_by(HHID) %>%
  summarise(lcsfs_fs = max(fs, na.rm = TRUE)) %>%
  rename(hhid=HHID)

colSums(is.na(fs_summary))
range(fs_summary$lcsfs_fs)
