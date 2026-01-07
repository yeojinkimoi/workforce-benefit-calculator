library(pacman)
p_load(tidyverse, readr, stringr, tidyr)

current_directory <- getwd()

load(paste0(current_directory,"/prd_parameters/benefit.parameters.rdata"))

stateinctaxData <- stateinctaxData %>% 
  mutate(
    UT_Phaseout = if_else(
      stateName == "Utah", 0.013, UT_Phaseout
    )
  )

# stateinctaxData %>% 
#   filter(stateName == "Utah") %>% 
#   count(UT_Phaseout)

save(list = ls(), file=file.path(current_directory, "prd_parameters/benefit.parameters.rdata"))

