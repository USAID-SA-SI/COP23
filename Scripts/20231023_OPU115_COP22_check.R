# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  COP22 OPU 115
# REF ID:   b3b7eacf 
# LICENSE:  MIT
# DATE:     2023-10-23
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  #filepath
filepath <- si_path() %>%
  return_latest("^MER_.*_PSNU_IM_FY21.*_S.*.zip$")

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "b3b7eacf"

# IMPORT ------------------------------------------------------------------
  
df_msd <- read_psd(filepath)


# 1. USAID KP UPDATE

  df_msd %>% 
    filter(funding_agency == "USAID",
           fiscal_year == 2023,
           indicator != "KP_PREV",
           str_detect(standardizeddisaggregate, "KeyPop"),
           mech_code %in% c("86131", "86132", "82199")) %>% 
    count(indicator, standardizeddisaggregate) %>% View()
    group_by(operatingunit, fiscal_year, funding_agency, mech_code) %>% 
    summarise(across(c(targets), sum, na.rm = TRUE)) 
  
  
  df_msd %>% 
    filter(funding_agency == "USAID",
           fiscal_year == 2023,
           indicator == "KP_PREV",
           standardizeddisaggregate == "Total Numerator",
           #str_detect(standardizeddisaggregate, "KeyPop"),
           mech_code %in% c("86131", "86132", "82199")) %>% 
    # count(indicator, standardizeddisaggregate) %>% View()
    group_by(operatingunit, fiscal_year, funding_agency, mech_code) %>% 
    summarise(across(c(targets), sum, na.rm = TRUE)) 
