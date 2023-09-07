# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  COP22 OPU #1 check
# REF ID:   c461cfab 
# LICENSE:  MIT
# DATE:     2023-08-18
# UPDATED:  2203-09-06

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(tameDP)


# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()

df_msd <- si_path() %>% 
  return_latest("MER_Structured_Datasets_PSNU_IM_FY21-24_20230811_v1_1_South Africa") %>% 
  read_psd()

# 1. USAID DREAMS Shifts: PP PREV targets for 15-19 shift from one IP to another

df_msd %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("PP_PREV"),
         fiscal_year == 2023,
         standardizeddisaggregate == "Age/Sex",
         # ageasentered == "15-19"
         # ,
          mech_code %in% c("160611", "80008")
         ) %>% 
  group_by(operatingunit,funding_agency, mech_code, prime_partner_name, ageasentered) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) 

opu_df %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("PP_PREV"),
         fiscal_year == 2023,
         ageasentered == "15-19",
         mech_code %in% c("160611", "80008")) %>% 
  group_by(operatingunit,funding_agency, mech_code, prime_partner_name, ageasentered) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) 



# 2. USAID DREAMS Shift: OVC_SERV preventive for NW province

ovc_check <- df_msd %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("OVC_SERV"),
         fiscal_year == 2023,
         snu1 == "nw North West Province",
         #ageasentered == "10-14",
         standardizeddisaggregate == "Age/Sex/Preventive",
         mech_code %in% c("14631", '81904', "86129")) %>% 
  group_by(operatingunit,funding_agency, mech_code, trendscoarse) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  rename(msd_targets = targets)

opu_df %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("OVC_SERV"),
         fiscal_year == 2023,
         standardizeddisaggregate == "Age/Sex/Preventive",
         mech_code %in% c("14631", '81904', "86129")) %>% 
  #count(standardizeddisaggregate, otherdisaggregate)
  group_by(operatingunit,funding_agency, mech_code, ageasentered) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  rename(opu_targets = targets) %>% 
  left_join(ovc_check, by = c("operatingunit", "funding_agency", "mech_code", "ageasentered")) %>% 
  mutate(diff = msd_targets - opu_targets)


# State Community Grant:
  # shift targets to West coast dummy district


opu_df %>% 
  filter(
    funding_agency == "USAID",
         indicator %in% c("OVC_HIVSTAT"),
         #mech_code == "11500",
         fiscal_year == 2023) %>% 
  #count(standardizeddisaggregate, otherdisaggregate)
  group_by(operatingunit,funding_agency, mech_code, prime_partner_name) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE))

df_msd %>% 
  filter(
    #funding_agency == "USAID",
    indicator %in% c("OVC_HIVSTAT"),
    mech_code == "11500",
    fiscal_year == 2023) %>% 
  #count(standardizeddisaggregate, otherdisaggregate)
  group_by(operatingunit,funding_agency, mech_code, prime_partner_name) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE))

#general checks

df_msd %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2023,
         standardizeddisaggregate == "Age/Sex",
         indicator == "PP_PREV") %>% 
  group_by(fiscal_year, indicator,
          # @, snuprioritization,
           trendscoarse
  ) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>%
  pivot_wider(names_from = "snuprioritization", values_from = targets) %>% 
  View()


df_msd %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2023,
         standardizeddisaggregate %in% c("Age/Sex/ProgramStatusCaregiver", "Age/Sex/ProgramStatus", "Age/Sex/DREAMS", "Age/Sex/Preventive")) %>% 
  group_by(fiscal_year, indicator,
           snuprioritization,
           trendscoarse
  ) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>%
  pivot_wider(names_from = "snuprioritization", values_from = targets) %>% 
  View()