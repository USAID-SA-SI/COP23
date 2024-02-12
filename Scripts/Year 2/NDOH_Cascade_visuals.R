# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   d28730a1 
# LICENSE:  MIT
# DATE:     2024-02-05
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

    data_folder <- "Data"
    
    df_cscd <- data_folder %>% 
      return_latest("CSV_Cascade_TSTGrossAgeSex_2024-02-05_13h50") %>% 
      read_csv()

    #rounding function
    clean_number <- function(x, digits = 0){
      dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                       x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                       x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                       TRUE ~ glue("{x}"))
    }  
    
    source <- "Naomi 2023 calibrated to Thembisa 4.7 + FY23Q4c MSD"
    
# MUNGE -------------------------------------------------------------------
    
  # Overall TX_CURR targets over time
      # district
      # coarse age / sex
    
    df_cscd <-  df_cscd %>% 
      mutate(coarse_age = ifelse(tst_age %in% c("01-09", "10-14"), "<15", "15+")) 
    
    
   df_tx <-  df_cscd %>% 
      select(short_name, sex, tst_age, `TST LineID`, Agency, `TX_CURR||2023|CY2023Q3`, `COP23T|TX_CURR`, `COP24T|TX_CURR`, `COP25T|TX_CURR`)
   
   df_tx_psnu_viz <- df_tx %>% 
      janitor::clean_names() %>% 
      rename(`FY23 (Result)` = tx_curr_2023_cy2023q3,
             `FY24 (Target)` = cop23t_tx_curr,
             `FY25 (Target)` = cop24t_tx_curr,
             `FY26 (Target)` = cop25t_tx_curr) %>% 
      pivot_longer(cols = c(`FY23 (Result)`:`FY26 (Target)`), names_to = "indicator") %>% 
      group_by(short_name, indicator) %>% 
      summarise(across(c(value), sum, na.rm = TRUE), .groups = "drop") 
   
   df_tx_agesex_viz <- df_tx %>% 
     janitor::clean_names() %>% 
     rename(`FY23 TX_CURR Result` = tx_curr_2023_cy2023q3,
            `FY24 TX_CURR Target` = cop23t_tx_curr,
            `FY25 TX_CURR Target` = cop24t_tx_curr,
            `FY26 TX_CURR Target` = cop25t_tx_curr) %>% 
     pivot_longer(cols = c(`FY23 TX_CURR Result`:`FY26 TX_CURR Target`), names_to = "indicator") %>% 
     group_by(sex, coarse_age, indicator) %>% 
     summarise(across(c(value), sum, na.rm = TRUE), .groups = "drop")
   
   
   # art cov
   
   df_cov <-  df_cscd %>% 
     select(short_name, sex, coarse_age, `TST LineID`, Agency,
            `TX_CURR||2023|CY2023Q3`, `plhiv_attend|Sept_2023|2025|CY2025Q3`,
            `COP23T|TX_CURR`, `COP24T|TX_CURR`, `COP25T|TX_CURR`)
   
  df_psnu_cov <- df_cov %>% 
     janitor::clean_names() %>% 
     group_by(short_name) %>% 
     summarise_if(is.double, sum, na.rm = TRUE) %>% 
     mutate(fy23_art_cov = tx_curr_2023_cy2023q3 /plhiv_attend_sept_2023_2025_cy2025q3,
            fy24_art_cov = cop23t_tx_curr/plhiv_attend_sept_2023_2025_cy2025q3,
            fy25_art_cov = cop24t_tx_curr/plhiv_attend_sept_2023_2025_cy2025q3,
            fy26_art_cov = cop25t_tx_curr/plhiv_attend_sept_2023_2025_cy2025q3) %>% 
     select(c(short_name,plhiv_attend_sept_2023_2025_cy2025q3, contains("art_cov"))) %>% 
    rename(`Sept 2023` = fy23_art_cov,
           `Sept 2024` = fy24_art_cov,
           `Sept 2025` = fy25_art_cov,
           `Sept 2026` = fy26_art_cov) %>% 
    pivot_longer(cols = `Sept 2023`:`Sept 2026`, names_to = "indicator")
  
 df_agesex_cov <-  df_cov %>% 
    janitor::clean_names() %>% 
   # group_by(sex, coarse_age) %>% 
    mutate(pop = glue("{sex} {coarse_age}")) %>% 
    mutate(pop = ifelse(str_detect(pop, "<15"), "<15", pop)) %>% 
    group_by(pop) %>% 
   #summarise(across(c(value), sum, na.rm = TRUE), .groups = "drop") %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    mutate(fy23_art_cov = tx_curr_2023_cy2023q3 /plhiv_attend_sept_2023_2025_cy2025q3,
           fy24_art_cov = cop23t_tx_curr/plhiv_attend_sept_2023_2025_cy2025q3,
           fy25_art_cov = cop24t_tx_curr/plhiv_attend_sept_2023_2025_cy2025q3,
           fy26_art_cov = cop25t_tx_curr/plhiv_attend_sept_2023_2025_cy2025q3) %>% 
    select(c(pop, contains("art_cov"))) %>% 
   rename(`Sept 2023` = fy23_art_cov,
          `Sept 2024` = fy24_art_cov,
          `Sept 2025` = fy25_art_cov,
          `Sept 2026` = fy26_art_cov) %>% 
   pivot_longer(cols = `Sept 2023`:`Sept 2026`, names_to = "indicator")
 
 
 # growth rate
 
 df_growth <- df_cscd %>%
   select(short_name, sex, coarse_age, `TST LineID`, Agency,
          `TX_CURR||2023|CY2023Q3`, `TX_CURR||2022|CY2022Q32`, `COP23T|TX_CURR`, `COP24T|TX_CURR`, `COP25T|TX_CURR`) 
 
 
df_growth_psnu <-  df_growth %>% 
   janitor::clean_names() %>% 
   group_by(short_name) %>% 
   summarise_if(is.double, sum, na.rm = TRUE) %>% 
   mutate(cop22_abs_diff = tx_curr_2023_cy2023q3-tx_curr_2022_cy2022q32,
          cop23_abs_diff = cop23t_tx_curr  - tx_curr_2023_cy2023q3,
          cop24_abs_diff = cop24t_tx_curr - cop23t_tx_curr,
          cop25_abs_diff = cop25t_tx_curr -cop24t_tx_curr) %>% 
   mutate(cop22_growth = cop22_abs_diff / tx_curr_2022_cy2022q32,
          cop23_growth = cop23_abs_diff /tx_curr_2023_cy2023q3,
          cop24_growth = cop24_abs_diff / cop23t_tx_curr,
          cop25_growth = cop25_abs_diff/ cop24t_tx_curr) %>% 
   select(c(short_name, contains("abs_diff"), contains("growth"))) %>% 
   pivot_longer(cols = cop22_abs_diff:cop25_growth, names_to = "indicator", values_to = "value") %>% 
   mutate(indic = str_remove(indicator, "cop\\d+_"),
          fy = str_extract(indicator, "cop\\d+")) %>% 
   select(-c(indicator)) %>% 
   pivot_wider(names_from = "indic")


df_growth_agesex <-  df_growth %>% 
  janitor::clean_names() %>% 
  mutate(pop = glue("{sex} {coarse_age}")) %>% 
  mutate(pop = ifelse(str_detect(pop, "<15"), "<15", pop)) %>% 
  group_by(pop) %>% 
  #summarise(across(c(value), sum, na.rm = TRUE), .groups = "drop") %>% 
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  mutate(cop22_abs_diff = tx_curr_2023_cy2023q3-tx_curr_2022_cy2022q32,
         cop23_abs_diff = cop23t_tx_curr  - tx_curr_2023_cy2023q3,
         cop24_abs_diff = cop24t_tx_curr - cop23t_tx_curr,
         cop25_abs_diff = cop25t_tx_curr -cop24t_tx_curr) %>% 
  mutate(cop22_growth = cop22_abs_diff / tx_curr_2022_cy2022q32,
         cop23_growth = cop23_abs_diff /tx_curr_2023_cy2023q3,
         cop24_growth = cop24_abs_diff / cop23t_tx_curr,
         cop25_growth = cop25_abs_diff/ cop24t_tx_curr) %>% 
  select(c(pop, contains("abs_diff"), contains("growth"))) %>% 
  pivot_longer(cols = cop22_abs_diff:cop25_growth, names_to = "indicator", values_to = "value") %>% 
  mutate(indic = str_remove(indicator, "cop\\d+_"),
         fy = str_extract(indicator, "cop\\d+")) %>% 
  select(-c(indicator)) %>% 
  pivot_wider(names_from = "indic")
 
 
 cop25 <- "= SUM('COP25T|TX_CURR'-'COP24T|TX_CURR')/'COP24T|TX_CURR'"
cop_24 <-  "SUM('COP24T|TX_CURR'-'COP23T|TX_CURR') /'COP23T|TX_CURR'"
cop23 <-  "= SUM('COP23T|TX_CURR'-'TX_CURR||2023|CY2023Q3')/'TX_CURR||2023|CY2023Q3"
cop22 <- " = SUM('TX_CURR||2023|CY2023Q3'-'TX_CURR||2022|CY2022Q32')/SUM('TX_CURR||2022|CY2022Q32')"
   
   
    
# VIZ -------------------------------------------------------------------
 
 metro_name <- c("gp City of Johannesburg MM", "kz eThekwini MM", "gp Ekurhuleni MM", )
      
   # PNSU tx_curr over time
   df_tx_psnu_viz %>% 
     mutate(fill_color = ifelse(str_detect(indicator, "Result"), scooter_med, usaid_blue)) %>% 
     ggplot(aes(x = indicator, y = value, fill = fill_color)) + 
     geom_col() +
     facet_wrap(~fct_reorder(short_name, desc(value)), nrow = 4) +
     coord_flip() + 
     si_style_xgrid() +
     scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) + 
     geom_text(aes(y = value,
                   label = clean_number(value),
                   color = fill_color),
               family = "Source Sans Pro",
               hjust = -0.2,
               size = 10/.pt) +
     scale_fill_identity() +
     scale_color_identity() +
     labs(title = "TX_CURR Targets over time by district" %>% toupper(),
          subtitle = "FY23 Results, FY24-FY26 Targets", 
          x = NULL, y = NULL)

    
   si_save("Graphics/01_txcurr_psnu_new.svg")
   
   
   # age / sex tx_curr over time
   df_tx_agesex_viz %>%
     mutate(pop = glue("{sex} {coarse_age}")) %>% 
     mutate(pop = ifelse(str_detect(pop, "<15"), "<15", pop)) %>% 
     group_by(indicator, pop) %>% 
     summarise(across(c(value), sum, na.rm = TRUE), .groups = "drop") %>% 
     mutate(fill_color = ifelse(str_detect(indicator, "Result"), scooter_med, usaid_blue)) %>% 
     ggplot(aes(x = indicator, y = value, fill = fill_color)) + 
     geom_col() +
     facet_wrap(~fct_reorder(pop, desc(value))) +
     coord_flip() + 
     si_style_xgrid() +
     scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) + 
     geom_text(aes(y = value,
                   label = clean_number(value, 1),
                   color = fill_color),
               family = "Source Sans Pro",
               hjust = -0.2,
               size = 12/.pt) +
     scale_fill_identity() +
     scale_color_identity() +
     labs(title = "TX_CURR Targets over time by coarse age and sex" %>% toupper(),
          subtitle = "FY23 Results, FY24-FY26 Targets", 
          x = NULL, y = NULL)
   
   si_save("Graphics/02_txcurr_agesex.svg")
   
   
    
  # ART Coverage lollipop
   # df_agesex_cov %>% 
   #   mutate(pop = glue("{sex} {coarse_age}")) %>% 
   #   ggplot(aes(value, indicator)) +
   #   geom_path(color = "gray50") +
   #   geom_point(size = 8, fill = "white") +
   #   geom_point(aes(fill = pop, color = "white"), shape = 21, size = 8) +
   #   geom_text(aes(label = percent(value, 1)), family = "Source Sans Pro", 
   #             color = trolley_grey, hjust = -0.8, na.rm = TRUE) +
   #   facet_wrap(~sex) +
   #   scale_fill_manual(values = c("Male <15" = genoa_light,"Male 15+" = genoa,
   #                                "Female 15+" = moody_blue, "Female <15" = moody_blue_light)) +
   #   scale_color_identity() +
   #   scale_x_continuous(label = percent) +
   #   si_style() +
   #   labs(x = NULL, y = NULL,
   #        title = "ART Coverage by coarse age and sex" %>% toupper(),
   #        subtitle = "FY23-FY26")
   
   
   # ART cov bar chart
   
   df_psnu_cov %>% 
     mutate(fill_color = ifelse(str_detect(indicator, "2023"), "#5CD5BC", "#00392A")) %>% 
     ggplot(aes(x = indicator, y = value, fill = fill_color)) + 
     geom_col() +
     facet_wrap(~fct_reorder(short_name, desc(plhiv_attend_sept_2023_2025_cy2025q3)), nrow = 4) +
     coord_flip() + 
     si_style_nolines() +
     scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) + 
     geom_text(aes(y = value,
                   label = percent(value, 1),
                   color = fill_color),
               family = "Source Sans Pro",
               hjust = -0.2,
               size = 10/.pt) +
     scale_fill_identity() +
     scale_color_identity() +
    # scale_color_identity() +
     scale_y_continuous(label = percent) +
     labs(x = NULL, y = NULL,
          title = "ART Coverage by district" %>% toupper(),
          subtitle = "FY23-FY26",
          caption = glue("Source: {source}")) +
     theme(axis.text.x = element_blank())
   
   si_save("Graphics/03_art_cov_psnu.svg")
   
   
   df_agesex_cov %>% 
     # mutate(pop = glue("{sex} {coarse_age}")) %>% 
     # mutate(pop = ifelse(str_detect(pop, "<15"), "<15", pop)) %>% 
     # group_by(indicator, pop) %>% 
     # summarise(across(c(value), sum, na.rm = TRUE), .groups = "drop") %>% 
     mutate(fill_color = ifelse(str_detect(indicator, "2023"), "#5CD5BC", "#00392A")) %>% 
     ggplot(aes(x = indicator, y = value, fill = fill_color)) + 
     geom_col() +
     facet_wrap(~pop, nrow = 3) +
     coord_flip() + 
     si_style_nolines() +
     scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) + 
     geom_text(aes(y = value,
                   label = percent(value, 1),
                   color = fill_color),
               family = "Source Sans Pro",
               hjust = -0.5,
               size = 10/.pt) +
     scale_fill_identity() +
     scale_color_identity() +
     # scale_color_identity() +
     scale_y_continuous(label = percent) +
     labs(x = NULL, y = NULL,
          title = "ART Coverage by coarse age and sex" %>% toupper(),
          subtitle = "FY23-FY26",
          caption = glue("{source}")) +
     theme(axis.text.x = element_blank())
   
   si_save("Graphics/04_art_cov_agesex.svg")
   
  # Table for growth rates + abs #
   
df_growth_agesex %>% 
  mutate(fy = str_to_upper(fy)) %>% 
     # mutate(pop = glue("{sex} {coarse_age}")) %>% 
     # mutate(pop = ifelse(str_detect(pop, "<15"), "<15", pop)) %>% 
     # group_by(indicator, pop) %>% 
     # summarise(across(c(value), sum, na.rm = TRUE), .groups = "drop") %>% 
     mutate(fill_color = ifelse(str_detect(fy, "22"), scooter_med, usaid_blue)) %>% 
     ggplot(aes(x = fy, y = abs_diff, fill = fill_color)) + 
     geom_col() +
     facet_wrap(~pop) +
    # coord_flip() + 
     si_style_nolines() +
     scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) + 
     geom_text(aes(y = abs_diff,
                   label = clean_number(abs_diff, 1),
                   color = fill_color),
               family = "Source Sans Pro",
              #hjust = -0.2,
               vjust = -0.5,
               size = 12/.pt) +
  geom_text(aes(y = abs_diff,
                label = glue("Growth rate:
                             {percent(growth, 1)}"),
                color = "white"),
            family = "Source Sans Pro",
            #hjust = -0.2,
            vjust = 2,
            size = 10/.pt) +
     scale_fill_identity() +
     scale_color_identity() +
  labs(x = NULL, y = NULL,
       title = "Annual Growth Rates" %>% toupper(),
       subtitle = "FY23-FY26",
       caption = glue("{source}"))

si_save("Graphics/05_growth_bar_agesex.svg")
  
vline_agesex <- df_growth_agesex %>% 
  mutate(group =1) %>% 
  mutate(fill_color = ifelse(str_detect(fy, "22"), scooter_med, usaid_blue)) %>% 
  ggplot(aes(fy, growth, group = group,
             color = fill_color, fill = fill_color)) +
  geom_blank(aes(y = 1.1 * growth)) +
  geom_line(size = 1.5) +
  geom_point(shape = 21, size = 10, stroke = 2) +
  scale_fill_identity() +
  scale_color_identity() +
  facet_wrap(~pop) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label = percent(growth, 1)), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  si_style_ygrid() +
  labs(x = NULL, y = NULL,
       title = "Annual Growth Rates" %>% toupper(),
       subtitle = "FY23-FY26",
       caption = glue("{source}")) +
  theme(axis.text.x = element_blank())

vline_agesex / vbar_agesex

si_save("Graphics/05_growth_rate_agesex_full.svg")


#PSNU

neg_net_new_psnu <- df_growth_psnu %>% 
  filter(abs_diff < 0) %>% 
  pull(short_name)


df_growth_psnu %>% 
  mutate(fy = str_to_upper(fy)) %>% 
  mutate(fill_color = ifelse(str_detect(fy, "22"), scooter_med, usaid_blue)) %>% 
 # filter(short_name %ni% neg_net_new_psnu) %>% 
  ggplot(aes(x = fy, y = abs_diff, fill = fill_color)) + 
  geom_col() +
  facet_wrap(~fct_reorder(short_name, desc(abs_diff)), nrow = 4) +
  # coord_flip() + 
  si_style_nolines() +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) + 
  geom_text(aes(y = abs_diff,
                label = clean_number(abs_diff, 1),
                color = fill_color),
            family = "Source Sans Pro",
            #hjust = -0.2,
            vjust = -0.5,
            size = 12/.pt) +
  geom_text(aes(y = abs_diff,
                label = glue("{percent(growth, 1)}"),
                color = "white"),
            family = "Source Sans Pro",
            #hjust = -0.2,
            vjust = 2,
            size = 10/.pt) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = NULL, y = NULL,
       title = "Annual Growth Rates" %>% toupper(),
       subtitle = "FY23-FY26",
       caption = glue("{source}"))

si_save("Graphics/06_growth_bar_psnu.svg")


vline_agesex / vbar_agesex


