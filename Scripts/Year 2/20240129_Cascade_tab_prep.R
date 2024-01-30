# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
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

# IMPORT ------------------------------------------------------------------
  
   #import pepfar indicators dataset
 df_pepfar_indic <- data_folder %>% 
     return_latest("pepfar_datapack_indicators_2024") %>% 
     read_csv()
   
   #import indicators dataset
   df_indic <- data_folder %>% 
     return_latest("indicators.csv") %>% 
     read_csv()
   
   #import MSD
   df_msd <- read_psd(filepath)

# MUNGE -------------------------------------------------------------------

   #grab PLHIV attend from indicators dataset - collapse age bands + adjust PSNU
  df_indic_plhiv <- df_indic %>%
     filter(indicator == "plhiv_attend",
            area_level_label == "District",
            sex== "both",
            calendar_quarter == "CY2025Q3",
            age_group_label %in% c("<1", "1-4", "5-9", "10-14", "15-24", "25-34", "35-49", "50+")) %>% 
     mutate(age_group_label = case_when(age_group_label == "1-4" ~ "01-09",
                                        age_group_label == "5-9" ~ "01-09",
                                        age_group_label == "<1" ~ "<01",
                                        TRUE ~ age_group_label)) %>% 
     group_by(indicator, area_name, age_group_label, calendar_quarter) %>% 
     summarise(across(starts_with("mean"), sum, na.rm = T), .groups = "drop") %>% 
     rename(val_df_indicator = mean) %>% 
     mutate(area_name = case_when(
       area_name %in% c("Buffalo City MM") ~ "ec Buffalo City MM",
       area_name %in% c("Cape Town MM") ~ "wc City of Cape Town MM",
       area_name %in% c("Sarah Baartman DM") ~ "ec Sarah Baartman DM",
       area_name %in% c("Amathole DM") ~ "ec Amathole DM",
       area_name %in% c("C Hani DM") ~ "ec Chris Hani DM",
       area_name %in% c("Joe Gqabi DM") ~ "ec Joe Gqabi DM",
       area_name %in% c("O Tambo DM") ~ "ec Oliver Tambo DM",
       area_name %in% c("A Nzo DM") ~ "ec Alfred Nzo DM",
       area_name %in% c("Ekurhuleni MM") ~ "gp Ekurhuleni MM",
       area_name %in% c("eThekwini MM") ~ "kz eThekwini MM",
       area_name %in% c("Xhariep DM") ~ "fs Xhariep DM",
       area_name %in% c("Lejweleputswa DM") ~ "fs Lejweleputswa DM",
       area_name %in% c("T Mofutsanyana DM") ~ "fs Thabo Mofutsanyana DM",
       area_name %in% c("Fezile Dabi DM") ~ "fs Fezile Dabi DM",
       area_name %in% c("Sedibeng DM") ~ "gp Sedibeng DM",
       area_name %in% c("West Rand DM") ~ "gp West Rand DM",
       area_name %in% c("Johannesburg MM") ~ "gp City of Johannesburg MM",
       area_name %in% c("Ugu DM") ~ "kz Ugu DM",
       area_name %in% c("uMgungundlovu DM") ~ "kz uMgungundlovu DM",
       area_name %in% c("Uthukela DM") ~ "kz Uthukela DM",
       area_name %in% c("Umzinyathi DM") ~ "kz Umzinyathi DM",
       area_name %in% c("Amajuba DM") ~ "kz Amajuba DM",
       area_name %in% c("Zululand DM") ~ "kz Zululand DM",
       area_name %in% c("Umkhanyakude DM") ~ "kz Umkhanyakude DM",
       area_name %in% c("King Cetshwayo DM" ) ~ "kz King Cetshwayo DM",
       area_name %in% c("iLembe DM") ~ "kz iLembe DM",
       area_name %in% c("Harry Gwala DM") ~ "kz Harry Gwala DM",
       area_name %in% c("Mopani DM") ~ "lp Mopani DM",
       area_name %in% c("Vhembe DM") ~ "lp Vhembe DM",
       area_name %in% c("Capricorn DM") ~ "lp Capricorn DM",
       area_name %in% c("Waterberg DM") ~ "lp Waterberg DM",
       area_name %in% c("Sekhukhune DM") ~ "lp Sekhukhune DM",
       area_name %in% c("Mangaung MM") ~ "fs Mangaung MM",
       area_name %in% c("G Sibande DM") ~ "mp Gert Sibande DM",
       area_name %in% c("Nkangala DM") ~ "mp Nkangala DM",
       area_name %in% c("Ehlanzeni DM") ~ "mp Ehlanzeni DM",
       area_name %in% c("Namakwa DM") ~ "nc Namakwa DM",
       area_name %in% c("Pixley ka Seme DM") ~ "nc Pixley ka Seme DM",
       area_name %in% c("ZF Mgcawu DM") ~ "nc Zwelentlanga Fatman Mgcawu DM",
       area_name %in% c("Frances Baard DM") ~ "nc Frances Baard DM",
       area_name %in% c("J T Gaetsewe DM") ~ "nc John Taolo Gaetsewe DM",
       area_name %in% c("N Mandela Bay MM") ~ "ec Nelson Mandela Bay Municipality",
       area_name %in% c("Bojanala Platinum DM") ~ "nw Bojanala Platinum DM",
       area_name %in% c("Ngaka Modiri Molema DM") ~ "nw Ngaka Modiri Molema DM",
       area_name %in% c("Ruth Segomotsi Mompati DM") ~ "nw Dr Ruth Segomotsi Mompati DM",
       area_name %in% c("Dr K Kaunda DM") ~ "nw Dr Kenneth Kaunda DM",
       area_name %in% c("Tshwane MM") ~ "gp City of Tshwane MM",
       area_name %in% c("West Coast DM") ~ "wc West Coast DM",
       area_name %in% c("Cape Winelands DM") ~ "wc Cape Winelands DM",
       area_name %in% c("Overberg DM") ~ "wc Overberg DM",
       area_name %in% c("Garden Route DM") ~ "wc Garden Route DM",
       area_name %in% c("Central Karoo DM") ~ "wc Central Karoo DM",
       TRUE ~ area_name))
   
   #first test - compare 10 year age band PLHIV attend from pepfar indicators df to roll up
   test1_plhiv <- df_pepfar_indic %>% 
     mutate(age = str_remove(age, "=\"\"")) %>% 
     mutate(age = str_remove(age, "\"\"")) %>%
     filter(indicator_code == "PLHIV_Attend.T") %>%
     mutate(indicator_code = "plhiv_attend") %>% 
     group_by(indicator_code, psnu, age, calendar_quarter) %>% 
     summarise(across(starts_with("value"), sum, na.rm = T), .groups = "drop") %>%
     mutate(psnu = str_replace(psnu, "District Municipality", "DM"),
            psnu = str_replace(psnu, "Metropolitan Municipality", "MM"),
            psnu = str_replace(psnu, "Mofutsanyane", "Mofutsanyana")) %>% 
     rename(val_df_indic_pepfar = value) %>% 
     left_join(df_indic_plhiv, by = c("indicator_code" = "indicator", "psnu" = "area_name","age" = "age_group_label", "calendar_quarter")) %>% 
     mutate(diff = val_df_indic_pepfar - val_df_indicator)
   
   
   # TX_CURR: column roll up of old age bands and new fy24 target age bands
   df_old_bands <- df_msd %>% 
     clean_agency() %>% 
     filter(funding_agency %in% c("USAID", "CDC"),
            indicator == "TX_CURR",
            fiscal_year == 2023,
            standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
     group_by(fiscal_year, psnu, age_2019, indicator) %>% 
     summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
     reshape_msd() %>% 
     filter(period == "FY23Q4") %>% 
     mutate(age_2019 = case_when(age_2019 == "01-04" | age_2019 == "05-09" ~ "01-09",
                                 age_2019 == "15-19" | age_2019 =="20-24" ~ "15-24",
                                 age_2019 == "25-29" | age_2019 == "30-34" ~ "25-34",
                                 age_2019 == "35-39" | age_2019 == "40-44" | age_2019 == "45-49" ~ "35-49",
                                 TRUE ~ age_2019)) %>% 
     group_by(period, psnu, age_2019, indicator) %>% 
     summarise(across(starts_with("value"), sum, na.rm = T), .groups = "drop") %>% 
     rename(age = age_2019,
            val_old_band = value)
     
   
   test2_txcurr <- df_msd %>% 
     clean_agency() %>% 
     filter(funding_agency %in% c("USAID", "CDC"),
            indicator == "TX_CURR",
            fiscal_year == 2023,
            standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
     group_by(fiscal_year, psnu, target_age_2024, indicator) %>% 
     summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
     reshape_msd() %>% 
     filter(period == "FY23Q4") %>% 
     select(-period_type) %>% 
     rename(age = target_age_2024,
            val_new_band = value) %>% 
     left_join(df_old_bands) %>% 
     mutate(diff = val_new_band - val_old_band)
   
   
   


   