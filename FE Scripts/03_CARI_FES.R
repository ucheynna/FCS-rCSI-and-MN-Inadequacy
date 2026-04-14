# library(tidyverse)
# Expressed monthly 
food_exp <- read.csv("Raw Data/hh_mod_g1.csv") %>%
  select(HHID,case_id, hh_g05) %>%
  group_by(HHID) %>%
  mutate(hh_g05=hh_g05 *30/7) %>%
  summarise(total_food_exp = sum(hh_g05, na.rm = TRUE)) %>%
  rename(hhid = HHID)

# Read and summarize hh_mod_i2.csv
non_food_exp1 <- read.csv("Raw Data/hh_mod_i2.csv") %>%
  select(HHID, hh_i06) %>%
  group_by(HHID) %>%
  summarise(total_non_food_exp1 = sum(hh_i06, na.rm = TRUE))

# Read, adjust, and summarize hh_mod_i1.csv
non_food_exp2 <- read.csv("Raw Data/hh_mod_i1.csv") %>%
  select(HHID, hh_i03) %>%
  mutate(hh_i03 = hh_i03 * 30 / 7) %>%  # Convert weekly to monthly
  group_by(HHID) %>%
  summarise(total_non_food_exp2 = sum(hh_i03, na.rm = TRUE))

# Read, adjust, and summarize hh_mod_i1.csv
non_food_exp3 <- read.csv("Raw Data/hh_mod_j.csv") %>%
  select(HHID, hh_j03) %>%
  mutate(hh_j03 = hh_j03 /3) %>%  # Convert tri monthly to monthly
  group_by(HHID) %>%
  summarise(total_non_food_exp3 = sum(hh_j03, na.rm = TRUE))

# Read, adjust, and summarize hh_mod_i1.csv
non_food_exp4 <- read.csv("Raw Data/hh_mod_k1.csv") %>%
  select(HHID, hh_k03) %>%
  mutate(hh_k03 = hh_k03 /12) %>%  # Convert annual to monthly
  group_by(HHID) %>%
  summarise(total_non_food_exp4 = sum(hh_k03, na.rm = TRUE))

# Merge all four non-food expenditure datasets
non_food_exp <- full_join(non_food_exp1, non_food_exp2, by = "HHID") %>%
  full_join(non_food_exp3, by = "HHID") %>%
  full_join(non_food_exp4, by = "HHID") %>%
  mutate(
    total_non_food_exp = rowSums(select(., total_non_food_exp1, total_non_food_exp2, total_non_food_exp3, total_non_food_exp4), na.rm = TRUE)
  )


# Optional: rename HHID to hhid if needed
non_food_exp <- non_food_exp %>%
  rename(hhid = HHID)


expenditure <- food_exp %>%
  left_join(non_food_exp, by = "hhid") %>%
  mutate(
    perc_spent = total_food_exp / (total_food_exp + total_non_food_exp) * 100,
    FES = case_when(
      perc_spent < 50 ~ 1,
      perc_spent >= 50 & perc_spent <65 ~ 2,
      perc_spent >= 65 & perc_spent <75 ~ 3,
      perc_spent >= 75 ~ 4,
      TRUE ~ NA_real_  # Handles cases that don't match any condition
    )
  )

hist(expenditure$perc_spent, breaks=50, main=" ", xlab="% Food Expenditure")
median(expenditure$perc_spent, na.rm=TRUE)

colSums(is.na(expenditure))
expenditure <- na.omit(expenditure)





