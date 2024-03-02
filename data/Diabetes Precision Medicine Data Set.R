# Install packages
library(readxl)
library(tidyverse)

# Define the path to the data folder
path_ep_folder <- "/Users/aamnasoniwala/Library/CloudStorage/OneDrive-SharedLibraries-EmoryUniversity/Varghese, Jithin Sam - Global Equity in Diabetes Precision Medicine Research/working/raw"
# path_ep_folder <- "C:/Cloud/OneDrive - Emory University/Papers/Global Equity in Diabetes Precision Medicine Research/working/raw"

# Loading Excel files into R
read_excel_workbook <- function(path) {
  sheets <- excel_sheets(path) 
  map_df(sheets, ~ read_excel(path, sheet = .x), .id = "sheet_index")
}


# Reviewer 1

r1_phase12 <- read_excel_workbook(paste0(path_ep_folder,"/Abstract Review_Aamna_Phase 1 and 2_2023-01-25.xlsx")) %>% 
  dplyr::filter(!is.na(precision_medicine))


r1_phase3 <- read_excel_workbook(paste0(path_ep_folder,"/Abstract Review_Aamna_Phase 3_2023-05-12.xlsx")) %>% 
  dplyr::filter(!is.na(precision_medicine))


r1_phase4 <- read_excel_workbook(paste0(path_ep_folder,"/Abstract Review_Aamna_Phase 4_2023-05-12.xlsx")) %>% 
  dplyr::filter(!is.na(precision_medicine))


r1_compiled <- bind_rows(r1_phase12,
                         r1_phase3,
                         r1_phase4)

# Reviewer 2

r2_phase1 <- read_excel_workbook(paste0(path_ep_folder,"/Abstract Review_Sophia_Phase 1_2023-02-01.xlsx")) %>% 
  dplyr::filter(!is.na(r2_precision_medicine)) %>% 
  # Recode the columns using R
  mutate(across(r2_precision_medicine:r2_primary_study,.fns = function(x) case_when(x == 1 ~ "Yes",
                                                                                    x == 0 ~ "No",
                                                                                    TRUE ~ NA_character_)))  


r2_phase2 <- read_excel_workbook(paste0(path_ep_folder,"/Abstract Review_Sophia_Phase 2_2023-02-01.xlsx")) %>% 
  dplyr::filter(!is.na(r2_precision_medicine)) 


r2_phase34 <- read_excel_workbook(paste0(path_ep_folder,"/Abstract Review_Sophia_Phase 3 and Phase 4_2023-05-19.xlsx")) %>% 
  dplyr::filter(!is.na(r2_precision_medicine)) 


# Rename columns for r2
r2_phase2 <- r2_phase2 %>% rename(gbd_region = region)
r2_phase34 <- r2_phase34 %>% rename(title = Title)


r2_compiled <- bind_rows(r2_phase1,
                         r2_phase2,
                         r2_phase34)

# Merging r1 and r2
install.packages("dplyr")
library(dplyr)

merged_r1r2 <- full_join(r1_compiled, r2_compiled, by = "gbd_region", relationship = "many-to-many")

