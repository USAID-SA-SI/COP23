# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Prepare PLHIV and TX_CURR cascade tab input data
# REF ID:   e3470184 
# LICENSE:  MIT
# DATE:     2024-01-29
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

# Grab metadata
data_folder <- "Data"

#read MSD
file_path <- si_path() %>% 
  return_latest("South Africa") 

#read subnat
df_nat_subnat <- si_path() %>% 
  return_latest("SUBNAT") %>% 
  read_psd()

#google id for DSP list
g_id <- '1Xrdf7ZHavhFn_Tc1CwnaUx3haIgEbWN5aWI1I7ryJpw'
df_dsp <- read_sheet(g_id, col_types = "c")


# IMPORT ------------------------------------------------------------------

#import pepfar indicators dataset
df_pepfar_indic <- data_folder %>% 
  return_latest("pepfar_datapack_indicators_2024") %>% 
  read_csv()

#NEW NAOMI

df_pepfar_indic_new <- data_folder %>% 
  return_latest("pepfar_datapack_indicators_2024_02_01") %>% 
  read_csv()

#import indicators dataset
df_indic <- data_folder %>% 
  return_latest("indicators.csv") %>% 
  read_csv()

#import MSD
df_msd <- read_psd(file_path)

#read subnat
df_nat_subnat <- si_path() %>% 
  return_latest("SUBNAT") %>% 
  read_psd()

# MUNGE -------------------------------------------------------------------

#prep DSP mapping file
dsp_table <- df_dsp %>% 
  janitor::clean_names() %>% 
  filter(dsp == "Yes") %>% 
  mutate(shortname = str_remove(dspid, mechanism_id)) %>% 
  mutate(shortname = str_replace(shortname, "Mofutsanyane", "Mofutsanyana")) %>% 
  rename(funding_agency = agency_lookback,
         mech_code = mechanism_id) %>% 
  clean_agency() %>% 
  select(shortname, mech_code, funding_agency) 

# GRAB UIDS from subnat
df_uid <- df_nat_subnat %>% 
  filter(operatingunit == "South Africa",
         fiscal_year == 2024) %>% 
  mutate(psnu = str_replace(psnu, "Mofutsanyane", "Mofutsanyana")) %>%
  mutate(shortname = str_replace(psnu, "District Municipality", "DM")) %>% 
  mutate(shortname = str_replace(shortname, "Metropolitan Municipality", "MM")) %>% 
  count(psnu, psnuuid, shortname)

#Columns:
  #1: PSNU [psnuuid]
  #2: shortname
  #3: sex
  #4: line id
  #5: tst_age
  #6: funding_agency
  #7: FY23Q4 TX_CURR
  #8: PLHIV Attend: CY2025Q3 10 year band


df_tx <- df_msd %>% 
  clean_agency() %>% 
  filter(funding_agency %in% c("USAID", "CDC", "DEDUP"),
         indicator == "TX_CURR",
         fiscal_year == 2023,
         snuprioritization != "5 - Centrally Supported",
         standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
  group_by(fiscal_year, psnu, psnuuid, target_age_2024, snuprioritization, sex, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == "FY23Q4") %>% 
  mutate(new_psnu = str_c(psnu, " [", psnuuid, "]")) %>% 
  rename(fy23q4_txcurr = value) %>% 
  left_join(df_uid %>% select(c(psnuuid, shortname)), by = c("psnuuid")) %>% 
  select(new_psnu, psnu, psnuuid, shortname, target_age_2024,snuprioritization, sex, fy23q4_txcurr)

df_plhiv <- df_pepfar_indic %>% 
  mutate(age = str_remove(age, "=\"\"")) %>% 
  mutate(age = str_remove(age, "\"\"")) %>%
  filter(indicator_code == "PLHIV_Attend.T") %>%
  mutate(indicator_code = "plhiv_attend") %>% 
  group_by(indicator_code, psnu, psnu_uid, age, sex, calendar_quarter) %>% 
  summarise(across(starts_with("value"), sum, na.rm = T), .groups = "drop") %>% 
  rename(plhiv_attend_cy2025Q3 = value) 

df_final <- df_tx %>% 
  rename(age = target_age_2024) %>% 
  filter(age != "Unknown Age") %>% 
  tidylog::full_join(df_plhiv %>% select(-c(indicator_code, calendar_quarter)),
                     by = c("psnu", "psnuuid" = "psnu_uid",  "age", "sex")) %>% 
  filter(!is.na(fy23q4_txcurr)) %>% 
  mutate(line_id = str_c(shortname, "|", age, "|", sex)) %>% 
 # count(age, sex, shortname, line_id) %>% 
  select(new_psnu, shortname, sex, line_id, age, fy23q4_txcurr, plhiv_attend_cy2025Q3)

# NEW estimates

df_plhiv_new <- df_pepfar_indic_new %>% 
  mutate(age = str_remove(age, "=\"\"")) %>% 
  mutate(age = str_remove(age, "\"\"")) %>%
  filter(indicator_code == "PLHIV_Attend.T") %>%
  mutate(indicator_code = "plhiv_attend") %>% 
  group_by(indicator_code, psnu, psnu_uid, age, sex, calendar_quarter) %>% 
  summarise(across(starts_with("value"), sum, na.rm = T), .groups = "drop") %>% 
  rename(plhiv_attend_cy2025Q3 = value) 


df_final_new <- df_tx %>% 
  rename(age = target_age_2024) %>% 
  filter(age != "Unknown Age") %>% 
  tidylog::full_join(df_plhiv_new %>% select(-c(indicator_code, calendar_quarter)),
                     by = c("psnu", "psnuuid" = "psnu_uid",  "age", "sex")) %>% 
  filter(!is.na(fy23q4_txcurr)) %>% 
  mutate(line_id = str_c(shortname, "|", age, "|", sex)) %>% 
  # count(age, sex, shortname, line_id) %>% 
  select(new_psnu, shortname, sex, line_id, age, fy23q4_txcurr, plhiv_attend_cy2025Q3)

df_final <- dsp_table %>% 
  select(-mech_code) %>% 
  distinct() %>% 
  tidylog::left_join(df_final, by = c("shortname")) %>% 
  distinct() %>% 
  filter(!is.na(new_psnu))

df_final_new <- dsp_table %>% 
  select(-mech_code) %>% 
  distinct() %>% 
  tidylog::left_join(df_final_new, by = c("shortname")) %>% 
  distinct() %>% 
  filter(!is.na(new_psnu))


new_plhiv <- df_final_new %>% mutate(total_plhiv = sum(plhiv_attend_cy2025Q3)) %>% pull(total_plhiv) %>% unique()
old_plhiv <- df_final %>% mutate(total_plhiv = sum(plhiv_attend_cy2025Q3)) %>% pull(total_plhiv) %>% unique()



#old
df_final <- df_final %>% 
  rename(prev_naomi_plhiv2025 = plhiv_attend_cy2025Q3)

#new
df_final_new %>% 
  rename(new_naomi_plhiv2025 = plhiv_attend_cy2025Q3) %>% 
  select(line_id, funding_agency, new_naomi_plhiv2025) %>% 
  left_join(df_final %>% select(line_id, funding_agency, prev_naomi_plhiv2025)) %>% 
  mutate(diff =prev_naomi_plhiv2025 -  new_naomi_plhiv2025)







today <- lubridate::today()

write_csv(df_final, glue("Dataout/{today}_cascade_tool_input_v4.csv"))


#### check

psnu_msd <- df_final %>% count(shortname) %>% pull(shortname)
  
psnu_dsp <- dsp_table %>% count(shortname) %>% pull(shortname)


setdiff(psnu_dsp, psnu_msd)
setdiff(psnu_msd, psnu_dsp)

