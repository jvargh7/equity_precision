
source(".Rprofile")
# Set all column names to: PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study

source("preprocessing/eppre_original fall 2022 review.R")


# Aamna : Spring 2023 ------
r1_df = map_dfr(sheet_names,
                function(s){
                  
                  readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract Review_2023-01-25_V2.xlsx"),sheet=s) %>%
                    dplyr::rename(
                                  publication_year = 'Publication Year',
                                  title = "Title",
                                  abstract = "abstract") %>% 
                    dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
                    mutate(region = s,
                           reviewer = 1)
                  
                }) %>% 
  mutate(strata = case_when(PMID %in% r0_df$PMID ~ "Fall 2022",
                            TRUE ~ "Spring 2023"))

# Sophia: Spring 2023 --------
r2_df = map_dfr(sheet_names,
                function(s){
                  
                  readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract for R2_2023-02-01_Sophia_screened.xlsx"),sheet=s) %>% 
                    dplyr::rename(
                      publication_year = 'Publication Year',
                      title = "Title",
                      abstract = "abstract")   %>% 
                    dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
                    mutate(region = s,
                           reviewer = 2)
                  
                }) %>% 
  mutate(strata = case_when(PMID %in% r0_df$PMID ~ "Fall 2022",
                            TRUE ~ "Spring 2023"))


# First selection Abstracts for Sophia ------------

r2_df_fall22 = r2_df %>% 
  dplyr::filter(PMID %in% r0_df$PMID)

table(!is.na(r2_df_fall22$precision_medicine))
# FALSE  TRUE 
# 690   502 
r2_df_spring23 = r2_df %>% 
  dplyr::filter(!PMID %in% r0_df$PMID)

table(!is.na(r2_df_spring23$precision_medicine))
# FALSE  TRUE 
# 269   228 


# Using Aamna's screens as a benchmark, how many were matching from Sophia's ---------

dataset_of_screens <- r1_df %>% 
  dplyr::select(strata,PMID,PMCID,title,abstract,region,precision_medicine,is_diabetes,correct_population,primary_study) %>% 
  rename_at(vars(precision_medicine,is_diabetes,correct_population,primary_study),~paste0("r1_",.)) %>% 
  left_join(r2_df %>% 
              dplyr::select(PMID,region,precision_medicine,is_diabetes,correct_population,primary_study) %>% 
              rename_at(vars(precision_medicine,is_diabetes,correct_population,primary_study),~paste0("r2_",.)),
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

dataset_of_screens %>% 
  dplyr::filter(!is.na(r1_precision_medicine) | !is.na(r2_precision_medicine)) %>% 
  writexl::write_xlsx(.,path=paste0(path_ep_folder,"/working/dataset of screens.xlsx"))

sum(!is.na(dataset_of_screens$match_precision_medicine))

sum(dataset_of_screens$match_precision_medicine == 1 & dataset_of_screens$match_is_diabetes == 1 &
      dataset_of_screens$match_correct_population == 1 & dataset_of_screens$match_primary_study == 1,na.rm = TRUE)
  
sum(!is.na(dataset_of_screens$r1_precision_medicine) | !is.na(dataset_of_screens$r2_precision_medicine))
