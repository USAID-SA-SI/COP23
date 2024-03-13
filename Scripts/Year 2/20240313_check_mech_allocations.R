# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Check PSNUxIM mechs by age/sex
# REF ID:   99a7a46e 
# LICENSE:  MIT
# DATE:     2024-03-13
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  library(tameDP)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

# folder

data_folder <- "Data/Datapack"
  
  ref_id <- "99a7a46e"

# IMPORT ------------------------------------------------------------------
  
  psnu_filepath <- data_folder %>% 
    return_latest("PSNUxIM_SouthAfrica_20240313_082102")

# MUNGE -------------------------------------------------------------------

 df_psnu <- tame_dp(psnu_filepath, type = "PSNUxIM", map_names = TRUE)
  
  df_pivot <- df_psnu %>% 
    group_by(psnu, fiscal_year, mech_code, prime_partner_name, indicator, target_age_2024, sex) %>% 
    summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop") 
  
df_pivot %>%
  arrange(indicator) %>% 
  relocate(fiscal_year, .before = psnu) %>% 
  write_csv("Dataout/20240313_check_mech_allocations.csv")
  