# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Review TX_CURR in western cape for 81894 and Anova
# REF ID:   205654f8 
# LICENSE:  MIT
# DATE:     2024-02-07
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
library(mindthegap)

# GLOBAL VARS ---------------------------------------------------

data_folder <- "Data"


# IMPORT --------------------------------------------------------

#pull MSD - site level
df_site <- si_path() %>% 
  return_latest("Site") %>% 
  read_psd() %>% 
  filter(fiscal_year == 2023)

#pull NDOH file from data folder
df_ndoh <- data_folder %>% 
  return_latest("non PEPFAR MER") %>% 
  read_excel(sheet = "TX_CURR_Q4")

# MUNGE ----------------------------------------------------------------------

# first, look across 81894 and 70310 for total TX_CURR
df_site %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
           mech_code %in% c("70310", "81894"),
         psnu == "wc City of Cape Town Metropolitan Municipality") %>% 
  group_by(psnu, indicator, mech_code) %>% 
  summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") 

# then, look across all mechs in MSD and grab TX_CURR for WC by site
df_tx_wc <- df_site %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
       #  mech_code %in% c("70310", "81894"),
         psnu == "wc City of Cape Town Metropolitan Municipality") %>% 
  group_by(psnu, indicator, mech_code, facility) %>% 
  summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") 


# check which sites report data for 81894 not but 70310
  # 4 sites
df_tx_wc %>% 
  pivot_wider(names_from = "mech_code", values_from = "cumulative") %>%
  filter(!is.na(`81894`) & is.na(`70310`)) %>% View()

#pull out 81894 MSD results for join
msd_ndoh <- df_tx_wc %>% 
  filter(mech_code == "81894")

#in NDOH file, get TX_CURR by site and join 81894 MSD results
# filter to see which facilities have different results in MER
df_ndoh %>% 
  filter(District == "wc City of Cape Town Metropolitan Municipality") %>% 
  group_by(District, Facility) %>% 
  summarise(across(c(Total), sum, na.rm = TRUE), .groups = "drop") %>% 
  left_join(msd_ndoh, by = c("District" = "psnu", "Facility" = "facility")) %>%
  mutate(diff = Total - cumulative) %>% 
  select(Facility, Total, cumulative, diff) %>% 
  rename(msd_ndoh_tx = cumulative,
         ndoh_tx = Total) %>% 
  filter(diff != 0) %>% View()

# anova sites that do not exist in NDOH file

#pull anova sites from mer
anova_sites <- df_tx_wc %>% 
  filter(mech_code == "70310") %>% 
  pull(facility)

#pull ndoh sites from ndoh file
ndoh_sites <- df_ndoh %>% 
  filter(District == "wc City of Cape Town Metropolitan Municipality") %>% 
  group_by(District, Facility) %>% 
  summarise(across(c(Total), sum, na.rm = TRUE), .groups = "drop") %>% 
  pull(Facility)

#what sites are in ANOVA in MSD but not NDOH file?
anova_diff_sites <- setdiff(anova_sites, ndoh_site)

#tx_curr for anova sites that are not in NDOH file
df_tx_wc %>% 
  filter(facility %in% anova_diff_sites) %>%
  pivot_wider(names_from = "mech_code", values_from = "cumulative") %>%
  View()

#also check what sites are in NDOH file but not ANOVA MER
setdiff(ndoh_site, anova_sites)



