

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
# Rename columns for r2
r2_phase2 <- r2_phase2 %>% rename(gbd_region = region)
r2_phase34 <- r2_phase34 %>% rename(title = Title)


# You would need to go back and correct the column names for the three workbooks and different worksheets using R
r2_compiled <- bind_rows(r2_phase1,
                         r2_phase2,
                         r2_phase34) %>% 
  dplyr::select(sheet_index,PMID,PMCID,title,abstract,gbd_region,
                starts_with("r2"), # This is a way to select variables easily
                random_screen
                ) %>% 
  mutate(gbd_region = case_when(gbd_region == "Latin America" ~ "Latin America and Caribbean",
                                gbd_region == "South East Asia" ~ "South East Asia and Pacific Islands",
                                gbd_region == "Middle East" ~ "Middle East and North Africa",
                                TRUE ~ gbd_region))


complete_dataset <- full_join(
  r1_compiled,
  r2_compiled %>% 
    dplyr::select(PMID,gbd_region,contains("r2")),
  by = c("PMID","gbd_region")
) %>% 
  mutate(across(one_of(c("precision_medicine","is_diabetes","correct_population","primary_study",
                         "r2_precision_medicine","r2_is_diabetes","r2_correct_population","r2_primary_study")),
                .fns = function(x) case_when(x %in% c("Yes","yes") ~ "Yes",
                                             x %in% c("no","No") ~ "No",
                                             x %in% c("no?") ~ "Unsure",
                                             TRUE ~ NA_character_)))

# Identifying rows where everything matches 
matched_rows <- complete_dataset %>% 
  dplyr::filter(precision_medicine == r2_precision_medicine,
                is_diabetes == r2_is_diabetes,
                correct_population == r2_correct_population,
                primary_study == r2_primary_study) %>% 
  mutate(h_precision_medicine = precision_medicine,
         h_is_diabetes = is_diabetes,
         h_correct_population = correct_population,
         h_primary_study = primary_study)

# Identifying other rows
unmatched_rows <- complete_dataset %>% 
  anti_join(matched_rows,
            by = c("PMID","gbd_region"))

# Reading in JV's reviews
r3_reviews <- read_excel(paste0(path_ep_folder,"/working/raw/Abstract Review_Jithin_Phase 1 and 2_2023-01-03.xlsx")) %>% 
  dplyr::filter(!is.na(r3_precision_medicine)) %>% 
  rename(gbd_region = region)  %>% 
  mutate(gbd_region = case_when(gbd_region == "Latin America" ~ "Latin America and Caribbean",
                                gbd_region == "South East Asia" ~ "South East Asia and Pacific Islands",
                                gbd_region == "Middle East" ~ "Middle East and North Africa",
                                TRUE ~ gbd_region)) %>% 
  # Recode the columns using R itself - do not edit the excel sheet
  mutate(across(r3_precision_medicine:r3_primary_study,.fns = function(x) case_when(x == 1 ~ "Yes",
                                                                                    x == 0 ~ "No",
                                                                                    TRUE ~ NA_character_)))

unmatched_rows_reviewed <- unmatched_rows %>% 
  inner_join(r3_reviews %>% 
               dplyr::select(PMID,gbd_region,contains("r3")),
             by = c("PMID","gbd_region")) %>% 
  mutate(h_precision_medicine = r3_precision_medicine,
         h_is_diabetes = r3_is_diabetes,
         h_correct_population = r3_correct_population,
         h_primary_study = r3_primary_study)

other_r3_reviews <- anti_join(r3_reviews,
                              unmatched_rows_reviewed,
                              by = c("PMID","gbd_region"))

unmatched_rows_pending <- anti_join(unmatched_rows,
                              unmatched_rows_reviewed,
                              by = c("PMID","gbd_region"))

matched_rows %>% 
  dplyr::select(sheet_index,PMID,Title,Authors,
                Citation,'First Author',
                'Journal/Book','Publication Year','Create Date',
                'PMCID','NIHMS ID','DOI',gbd_region,abstract,
                starts_with("h_")) %>% 
writexl::write_xlsx(.,path=paste0(path_ep_folder,"/working/cleaned/Matched Abstracts between Aamna and Sophia.xlsx"))


unmatched_rows_reviewed %>% 
  dplyr::select(sheet_index,PMID,Title,Authors,
                Citation,'First Author',
                'Journal/Book','Publication Year','Create Date',
                'PMCID','NIHMS ID','DOI',gbd_region,abstract,
                starts_with("h_")) %>% 
  writexl::write_xlsx(.,path=paste0(path_ep_folder,"/working/cleaned/Unmatched Abstracts reviewed by Jithin.xlsx"))

unmatched_rows_pending %>%  
  writexl::write_xlsx(.,path=paste0(path_ep_folder,"/working/cleaned/Unmatched Abstracts pending Review.xlsx"))
