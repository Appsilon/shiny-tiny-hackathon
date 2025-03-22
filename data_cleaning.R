# this script prepares the data downloaded from the online dashboard

library(tidyverse)
library(readxl)

# load the data sets

report_by_report_type <- read_excel("data/report-type.xlsx") %>% 
  filter(Year != "Total Reports") %>%
  select(!`Report Type`) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = !c(Year, `Total Reports`), names_to = "report_type", values_to = "case_count")

report_by_age_group <- read_excel("data/age_group.xlsx")  %>% 
  filter(Year != "Total Reports") %>% 
  select(!`Age Group`) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = !c(Year, `Total Reports`), names_to = "age_group", values_to = "case_count")

report_by_reporter_region <- read_excel("data/reporter-region.xlsx") %>% 
  filter(Year != "Total Reports") %>%
  select(!`Reporter Region`) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = !c(Year, `Total Reports`), names_to = "reporter_region", values_to = "case_count")


report_by_reporter <- read_excel("data/reporter.xlsx") %>% 
  filter(Year != "Total Reports") %>% 
  select(!Reporter) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = !c(Year, `Total Reports`), names_to = "reporter", values_to = "case_count")

report_by_seriousness <- read_excel("data/seriousness.xlsx") %>% 
  filter(Year != "Total Reports") %>% 
  select(!Seriousness) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = !c(Year, `Total Reports`), names_to = "seriousness", values_to = "case_count")

report_by_sex <- read_excel("data/sex.xlsx") %>% 
  filter(Year != "Total Reports") %>% 
  select(!Sex) %>% 
  pivot_longer(cols = !c(Year, `Total Reports`), names_to = "sex", values_to = "case_count")





