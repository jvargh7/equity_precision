
source(".Rprofile")
# Set all column names to: PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study

source("preprocessing/eppre01_original fall 2022 review.R")


# Aamna : Spring 2023 ------
r1_df_phase1and2 = map_dfr(c("South Asia","East Asia","Latin America","Middle East","South East Asia"),
                function(s){
                  # Renamed Precision Medicine Abstract Review_2023-01-25_V2 --> Abstract Review_Aamna_Phase 1 and 2_2023-01-25
                  readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Abstract Review_Aamna_Phase 1 and 2_2023-01-25.xlsx"),sheet=s) %>%
                    dplyr::rename(
                      publication_year = 'Publication Year',
                      title = "Title",
                      abstract = "abstract") %>% 
                    dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
                    mutate(region = s,
                           reviewer = 1)
                  
                }) %>% 
  dplyr::filter(!is.na(precision_medicine) | !is.na(is_diabetes) | !is.na(correct_population) | !is.na(primary_study)) %>% 
  mutate(strata = case_when(PMID %in% r0_df$PMID ~ "Fall 2022",
                            TRUE ~ "Spring 2023"))


r1_df_phase3 = map_dfr(c("Central Asia","Central and Eastern Europe","Sub Saharan Africa"),
                           function(s){
                             readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Abstract Review_Aamna_Phase 3_2023-05-12.xlsx"),sheet=s) %>%
                               dplyr::rename(
                                 publication_year = 'Publication Year',
                                 title = "Title",
                                 abstract = "abstract") %>% 
                               dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
                               mutate(region = s,
                                      reviewer = 1)
                             
                           }) %>% 
  dplyr::filter(!is.na(precision_medicine) | !is.na(is_diabetes) | !is.na(correct_population) | !is.na(primary_study)) %>% 
  mutate(strata = "Summer 2023")


r1_df_phase4 = map_dfr(c("USA","Western Europe"),
                       function(s){
                         readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Abstract Review_Aamna_Phase 4_2023-05-12.xlsx"),sheet=s) %>%
                           dplyr::rename(
                             publication_year = 'Publication Year',
                             title = "Title",
                             abstract = "abstract") %>% 
                           dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
                           mutate(region = s,
                                  reviewer = 1)
                         
                       }) %>% 
  dplyr::filter(!is.na(precision_medicine) | !is.na(is_diabetes) | !is.na(correct_population) | !is.na(primary_study)) %>% 
  mutate(strata = "Summer 2023")


r1_df <- bind_rows(r1_df_phase1and2,
                   r1_df_phase3,
                   r1_df_phase4)

# Sophia: Spring 2023 --------

# This was taken from dataset_of_screens_2023-01-03 (Previously dataset_of_screens)
# We lost the original data but had access to the cleaned one for Phase 1 of screening by Sophia
r2_df_phase1 = readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Abstract Review_Sophia_Phase 1_2023-02-01.xlsx"))  %>% 
  mutate_at(vars(starts_with("r2_")), function(x) case_when(x == 1 ~ "yes",
                                                            x == 0 ~ "no",
                                                            TRUE ~ NA_character_))
# Renamed Precision Medicine Abstract for R2_2023-02-01_Sophia_screened --> Abstract Review_Sophia_Phase 2_2023-05-10

r2_df_phase2 = readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Abstract Review_Sophia_Phase 2_2023-02-01.xlsx")) %>% 
  dplyr::select(-one_of("...11"))
r2_df_phase3and4 = map_dfr(c("Central Asia","Central and Eastern Europe","Sub Saharan Africa","USA","Western Europe"),
                           function(s){
                             readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Abstract Review_Sophia_Phase 3 and Phase 4_2023-05-19.xlsx"),
                                                sheet=s) %>%
                               dplyr::rename(
                                 publication_year = 'Publication Year',
                                 title = "Title",
                                 abstract = "abstract") %>% 
                               dplyr::select(PMID, PMCID, publication_year, title, abstract, 
                                             r2_precision_medicine,r2_is_diabetes,r2_correct_population,r2_primary_study) %>% 
                               mutate(region = s,
                                      reviewer = 1)
                             
                           }) %>% 
  dplyr::filter(!is.na(r2_precision_medicine) | !is.na(r2_is_diabetes) | !is.na(r2_correct_population) | !is.na(r2_primary_study)) %>% 
  mutate(strata = "Summer 2023")

r2_df <- bind_rows(r2_df_phase1,
                   r2_df_phase2,
                   r2_df_phase3and4)

# First selection Abstracts for Sophia ------------

r2_df_fall22 = r2_df %>% 
  dplyr::filter(PMID %in% r0_df$PMID)

table(!is.na(r2_df_fall22$r2_precision_medicine))
# FALSE  TRUE 
# 690   502 
r2_df_spring23 = r2_df %>% 
  dplyr::filter(!PMID %in% r0_df$PMID)

table(!is.na(r2_df_spring23$r2_precision_medicine))
# FALSE  TRUE 
# 269   228 


# Using Aamna's screens as a benchmark, how many were matching from Sophia's ---------

dataset_of_screens <- r1_df %>% 
  dplyr::select(strata,PMID,PMCID,title,abstract,region,precision_medicine,is_diabetes,correct_population,primary_study) %>% 
  rename_at(vars(precision_medicine,is_diabetes,correct_population,primary_study),~paste0("r1_",.)) %>% 
  left_join(r2_df %>% 
              dplyr::select(PMID,region,r2_precision_medicine,r2_is_diabetes,r2_correct_population,r2_primary_study),
            by = c("PMID","region")) %>% 
  mutate_at(vars(matches("^(r1|r2)")),function(x) case_when(x %in% c("Yes","yes","y","Y") ~ 1,
                                                            x %in% c("No","no","n","N") ~ 0,
                                                            TRUE ~ NA_real_)) %>% 
  
  mutate(match_precision_medicine = case_when(r1_precision_medicine == r2_precision_medicine ~ 1,
                                              r1_precision_medicine != r2_precision_medicine ~ 0,
                                              is.na(r1_precision_medicine) | is.na(r2_precision_medicine) ~ NA_real_
  ),
  match_is_diabetes = case_when(r1_is_diabetes == r2_is_diabetes ~ 1,
                                r1_is_diabetes != r2_is_diabetes ~ 0,
                                is.na(r1_is_diabetes) | is.na(r2_is_diabetes) ~ NA_real_
  ),
  
  match_correct_population = case_when(r1_correct_population == r2_correct_population ~ 1,
                                       r1_correct_population != r2_correct_population ~ 0,
                                       is.na(r1_correct_population) | is.na(r2_correct_population) ~ NA_real_
  ),
  
  match_primary_study = case_when(r1_primary_study == r2_primary_study ~ 1,
                                  r1_primary_study != r2_primary_study ~ 0,
                                  is.na(r1_primary_study) | is.na(r2_primary_study) ~ NA_real_
  )
  
  )

r3_df <- readxl::read_excel(paste0(path_ep_folder,"/working/dataset of screens_2023-01-03_R3.xlsx")) %>% 
  dplyr::select(PMID, starts_with("r3_"))




dataset_of_screens %>% 
  dplyr::filter(!is.na(r1_precision_medicine) | !is.na(r2_precision_medicine)) %>% 
  mutate(any_difference = case_when(match_precision_medicine == 0 |
                                      match_is_diabetes == 0 |
                                      match_correct_population == 0 |
                                      match_primary_study == 0 ~ "Different",
                                    TRUE ~ "Same")) %>% 
  # Missing records 
  mutate(r1_missing = case_when(is.na(r1_precision_medicine) |
                                  is.na(r1_is_diabetes) | 
                                  is.na(r1_correct_population) |
                                  is.na(r1_primary_study) ~ "Missing",
                                TRUE ~ ""),
         r2_missing = case_when(is.na(r2_precision_medicine) |
                                  is.na(r2_is_diabetes) | 
                                  is.na(r2_correct_population) |
                                  is.na(r2_primary_study) ~ "Missing",
                                TRUE ~ "")
         ) %>% 
  
  
  left_join(r3_df,
            by = "PMID") %>% 
  writexl::write_xlsx(.,path=paste0(path_ep_folder,"/working/dataset of screens_",Sys.Date(),".xlsx"))





sum(!is.na(dataset_of_screens$match_precision_medicine)) 

sum(dataset_of_screens$match_precision_medicine == 1 & dataset_of_screens$match_is_diabetes == 1 &
      dataset_of_screens$match_correct_population == 1 & dataset_of_screens$match_primary_study == 1,na.rm = TRUE)

sum(!is.na(dataset_of_screens$r1_precision_medicine) | !is.na(dataset_of_screens$r2_precision_medicine))
