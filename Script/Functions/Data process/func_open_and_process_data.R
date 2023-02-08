
## Open and process Stata data

open_and_process_data = function(path, data_type){
  
  library(haven)      # for opening Stata dta files and converting relevant variables into factors as_factor()
  library(here)       # for easy file referencing by using the top-level directory of a file project
  library(tidyverse)

  # Open data
  if (data_type == "dta"){
   
    data = haven::read_dta(path)
    # here::here("Data", "raw data", "baseline survey", "prevalence_survey_data_1_3.dta")
    # using here() means that file paths can be created from any physical place as long as fold structure is equivalent
     
  } else if (data_type == "sav"){
    
    data = haven::read_sav(path)
    
  } else if (data_type == "spss"){
    
    data = haven::read_spss(path)
    
  } else {}

  # Process data
  test = data %>% 
    select_if(is.labelled) %>%                            # select labelled variables
    mutate(across(everything(), ~haven::as_factor(.x)))   # convert these into factors
    # use haven::as_factor, which uses the labels imported from the Stata data to convert into factor levels
  
  # Merge data
  output = data %>% 
    select_if(~!is.labelled(.)) %>%    # deselect labelled variables
    bind_cols(test)                    # bind factor variables

  return(output)
  
}