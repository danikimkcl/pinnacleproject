# Open libraries
library(here)       # for easy file referencing by using the top-level directory of a file project
library(labelled)   # for var_label() to get variable labels
library(openxlsx)   # for write.xlsx()
library(readxl)     # for read_excel()
library(tidyverse)

# Source codes
source(here::here("Script", "Functions", "Data process", "func_open_and_process_data.R"))
source(here::here("Script", "Functions", "Data process", "func_compute_pd_variables.R"))

# Open dataset
path = here::here("Data", "raw data", "baseline survey", "prevalence_survey_data_1_3.dta")
data = open_and_process_data(path, "dta") # this function opens a Stata dataset (dta) and converts Stata's labelled variables into factors

# Open mortality data
path = here::here("Data", "raw data", "fu folder from Matthew", "specific outcomes", "mortality", "mortality_outcome1_1.dta")
mortality_outcome = open_and_process_data(path, "dta")

# Process Parkinsonism (`pnism`) and PD (`JorgePD`) variables
data = compute_pd_variables(data)

# Create a PD variable that combines the algorithm-based dx (`JorgePD`) and self-reported dx (`PARK`)
data = data %>% 
  mutate(JorgePD_or_selfPD = case_when(JorgePD==1 | PARK=="probable" | PARK=="certain" ~ 1, 
                                       JorgePD==0 | PARK=="no" ~ 0, 
                                       TRUE ~ NA_real_))

# Merge in mortality data
## Create houseid2
mortality_outcome = mortality_outcome %>% 
  mutate(centreid_num = case_when(centreid == "Cuba" ~ 10,
                                  centreid == "DR" ~ 20,
                                  centreid == "Peru (urban)" ~ 30,
                                  centreid == "Peru (rural)" ~ 40,
                                  centreid == "Venezuela" ~ 50,
                                  centreid == "Mexico (urban)" ~ 60,
                                  centreid == "Mexico (rural)" ~ 70,
                                  centreid == "China (urban)" ~ 100,
                                  centreid == "China (rural)" ~ 110,
                                  centreid == "India (urban)" ~ 120,
                                  centreid == "Puerto Rico" ~ 200)) %>% 
  mutate(HOUSEID = stringr::str_pad(HOUSEID, 4, pad="0")) %>% 
  mutate(houseid2 = as.numeric(paste0(centreid_num, HOUSEID)))

## Merge vital status data with baseline data
data = data %>% 
  left_join(select(mortality_outcome, houseid2, PARTICID, dead2, censor_days, censor_yr, vital_ascertained), by = c("houseid2", "PARTICID"))

# Save data
saveRDS(data, here::here("Data", "processed data", "prevalence_survey_data_pd.rds"))
write.xlsx(data, here::here("Data", "processed data", "prevalence_survey_data_pd.xlsx"))