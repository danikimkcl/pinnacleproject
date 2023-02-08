library(tidyverse)

### Compute variables related to Parkinson's disease
### i.e. Parkinsonism, Parkinson disease, and combined variable of Parkinsonism and dementia

### Note: this function only works on 10/66 datasets that have been processed using func_open_and_process_dta.R

compute_pd_variables = function(mydata){
  
  output = mydata %>% 
    
    ## Parkinsonism
    
    # 1. Index individuals with bradykinesia (NEO14G) AND rigidity (NEO6AU/AL/BU/AL)
    mutate(pnism1 = if_else(((NEO14G=="somewhat slow movements" | NEO14G=="very slow movements") & (NEO6AU=="slightly increased tone" | NEO6AU=="much increased tone" |NEO6AL=="slightly increased tone" |NEO6AL=="much increased tone" | NEO6BU=="slightly increased tone" |NEO6BU=="much increased tone" | NEO6BL=="slightly increased tone" | NEO6BL=="much increased tone")), 1, 0)) %>%  
    mutate(pnism1 = if_else(is.na(NEO14G) & (is.na(NEO6AU) | NEO6AU=="normal tone") & (is.na(NEO6AL) | NEO6AL=="normal tone") & (is.na(NEO6BU) | NEO6BU=="normal tone") & (is.na(NEO6BL) | NEO6BL=="normal tone"), NA_real_, pnism1)) %>% 
    # Stata treats missing data differently (removes observations in the calculation when there is missing data), so the above line of codes does that in R
    
    # 2. Index individuals with bradykinesia (NEO14G) AND resting tremor (NEO4E/F)
    mutate(pnism2 = if_else(((NEO14G=="somewhat slow movements" | NEO14G=="very slow movements") & ((!NEO4E=="no tremor") | (!NEO4F=="no tremor"))), 1, 0)) %>% 
    
    # 3. Index individuals with either #1 or #2
    mutate(pnism = if_else(pnism1==1 | pnism2==1, 1, pnism1)) %>% 
    select(-pnism1, -pnism2) %>% 

    ## Parkinson's disease
    mutate(JorgePD = if_else(pnism==1 & (NOE1=="normal upgaze" & NEO14E=="normal gait" & PCVA=="no"), 1, 0)) %>% 
    mutate(JorgePD = if_else(is.na(pnism) & (is.na(NOE1) | is.na(NEO14E) | is.na(PCVA)), NA_real_, JorgePD)) %>% 
      
    ## Combined dementia and Parkinsonism variables
    mutate(combined = paste(pnism, cdem1066),
          combined = factor(if_else(grepl("NA", combined), NA_character_, combined),
                            levels = c("0 non-case", "1 non-case", "0 case", "1 case"),
                            labels = c("Control", "Parkinsonism", "Dementia", "Parkinsonism and dementia")))

  return(output)

}