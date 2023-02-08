testr = mydata %>% 
  mutate(combined = paste(pnism, cdem1066)) %>% 
  mutate(combined = factor(case_when(pnism==0 & cdem1066=="non-case" ~ "0 non-case",
                                     pnism==0 & cdem1066=="case" ~ "0 case",
                                     pnism==1 & cdem1066=="non-case" ~ "1 non-case",
                                     pnism==1 & cdem1066=="case" ~ "1 case",
                                     TRUE ~ NA_character_))) %>% 
  select(houseid2, PARTICID, pnism, cdem1066, combined, NEO14G, NEO6AU, NEO6AL, NEO6BU, NEO6BL, NEO4E, NEO4F)

testst = haven::read_dta(here::here("Data", "raw data", "testdata.dta"))

testst = testst %>% 
  select(houseid2, PARTICID, pnism, cdem1066, combined, NEO14G, NEO6AU, NEO6AL, NEO6BU, NEO6BL, NEO4E, NEO4F) %>% 
  mutate(cdem1066 = factor(if_else(cdem1066==0, "non-case", "case"), levels = c("non-case", "case")),
         combined = case_when(combined == 1 ~ "0 non-case",
                              combined == 2 ~ "0 case",
                              combined == 3 ~ "1 non-case",
                              combined == 4 ~ "1 case",
                              TRUE ~ "NA"))

temp = testst %>% 
  select_if(is.labelled) %>%                            # select labelled variables
  mutate(across(everything(), ~haven::as_factor(.x)))   # convert these into factors

testst = testst %>% 
  select_if(~!is.labelled(.)) %>%    # deselect labelled variables
  bind_cols(temp)    

identical(testr, testst)
identical(testr[,3], testst[,3]) # pnism different
identical(testr[,4], testst[,4]) # cdem1066 different (but actually equal)
identical(testr[,5], testst[,5]) # combined different
identical(testr[,6:12], testst[,6:12]) # rest equal
# number of Parkinsonism is equal, but number of missing and non-cases is different.
# more NAs in Stata (1081) than R (1005) 

testing = testr %>% 
  select(houseid2, PARTICID, pnism_r = pnism, cdem1066_r = cdem1066, combined_r = combined) %>% 
  left_join(select(testst, houseid2, PARTICID, pnism_s = pnism, cdem1066_s = cdem1066, combined_s = combined, everything()), by = c("houseid2", "PARTICID"))

# select data with discrepancy

temp = testing %>% 
  mutate(diff = if_else(combined_r==combined_s, 0, 1),
         diff2 = if_else(pnism_r==pnism_s, 0, 1, 22)) %>% 
  filter(diff==1) %>% 
  filter(!diff2==0) %>% 
  filter(!pnism_r==1) # no differences when pnism==1

temp = testing %>% 
  mutate(diff = if_else(combined_r==combined_s, 0, 1)) %>% 
  filter(diff==1)