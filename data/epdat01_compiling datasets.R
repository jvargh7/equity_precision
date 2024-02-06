

library(readxl)
library(tidyverse)

#path_ep_folder comes from .Rprofile

# Loading Excel files into R
read_excel_workbook <- function(path) {
  sheets <- excel_sheets(path) 
  map_df(sheets, ~ read_excel(path, sheet = .x), .id = "sheet_index")
}

# Reviewer 1 ----------

r1_phase12 <- read_excel_workbook(paste0(path_ep_folder,"/working/raw/Abstract Review_Aamna_Phase 1 and 2_2023-01-25.xlsx")) %>% 
  dplyr::filter(!is.na(precision_medicine))


r1_phase3 <- read_excel_workbook(paste0(path_ep_folder,"/working/raw/Abstract Review_Aamna_Phase 3_2023-05-12.xlsx")) %>% 
  dplyr::filter(!is.na(precision_medicine))


r1_phase4 <- read_excel_workbook(paste0(path_ep_folder,"/working/raw/Abstract Review_Aamna_Phase 4_2023-05-12.xlsx")) %>% 
  dplyr::filter(!is.na(precision_medicine))

r1_compiled <- bind_rows(r1_phase12,
                         r1_phase3,
                         r1_phase4)

# Reviewer 2 --------

# The variable names start with r2_ for the 4 key labels

# I have added an example below for 'r2_phase1'
r2_phase1 <- read_excel_workbook(paste0(path_ep_folder,"/working/raw/Abstract Review_Sophia_Phase 1_2023-02-01.xlsx")) %>% 
  dplyr::filter(!is.na(r2_precision_medicine)) %>% 
  # Recode the columns using R itself - do not edit the excel sheet
  mutate(across(r2_precision_medicine:r2_primary_study,.fns = function(x) case_when(x == 1 ~ "Yes",
                                                                              x == 0 ~ "No",
                                                                              TRUE ~ NA_character_))) %>% 
# Rename the columns using R itself - do not edit the excel sheet.
  rename(gbd_region = region)


r2_phase2 <- read_excel_workbook(paste0(path_ep_folder,"/working/raw/Abstract Review_Sophia_Phase 2_2023-02-01.xlsx")) %>% 
  dplyr::filter(!is.na(r2_precision_medicine))


r2_phase34 <- read_excel_workbook(paste0(path_ep_folder,"/working/raw/Abstract Review_Sophia_Phase 3 and Phase 4_2023-05-19.xlsx")) %>% 
  dplyr::filter(!is.na(r2_precision_medicine)) 



# This dataset won't generate correctly 
# The issue is that column names are different between different excel workbooks and worksheets
# You would need to go back and correct the column names for the three workbooks and different worksheets using R
r2_compiled <- bind_rows(r2_phase1,
                         r2_phase2,
                         r2_phase34)


