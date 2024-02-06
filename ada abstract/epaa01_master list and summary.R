rm(list=ls());gc();source(".Rprofile")

source("preprocessing/eppre01_original fall 2022 review.R")
# Adapted from eppre05_combining reviews after resolution.R -------

screens <- read_xlsx(path=paste0(path_ep_folder,"/working/dataset of screens_2023-06-12.xlsx")) %>% 
  mutate(ada_precision_medicine = case_when(!is.na(r3_precision_medicine) ~ r3_precision_medicine,
                                            TRUE ~ r1_precision_medicine),
         ada_is_diabetes = case_when(!is.na(r3_is_diabetes) ~ r3_is_diabetes,
                                     TRUE ~ r1_is_diabetes),
         ada_correct_population = case_when(!is.na(r3_correct_population) ~ r3_correct_population,
                                            TRUE ~ r1_correct_population),
         
         ada_primary_study = case_when(!is.na(r3_primary_study) ~ r3_primary_study,
                                       TRUE ~ r1_primary_study)
  ) %>% 
  mutate(review_strata = case_when(!is.na(r3_precision_medicine) ~ "R3_Completed",
                                   TRUE ~ "R1_Only")) %>% 
  dplyr::select(strata,region,PMID,review_strata,starts_with("ada")) %>% 
  mutate(outcome = rowSums(.[,c("ada_precision_medicine","ada_is_diabetes","ada_correct_population","ada_primary_study")])) %>% 
  mutate(outcome = case_when(outcome == 4 ~ 1,
                             TRUE ~ 0))


phase1and2 = readRDS(paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract Review_2023-01-25.RDS"))  %>% 
  bind_rows() %>% 
  mutate(strata = case_when(PMID %in% r0_df$PMID ~ "Fall 2022",
                            TRUE ~ "Spring 2023")) 

phase3 = readRDS(paste0(path_ep_folder,"/references/PubMed Queries/Phase 3 Precision Medicine Abstract Review_2023-05-12.RDS"))  %>% 
  bind_rows() %>% 
  mutate(strata = "Summer 2023") 

phase4 = readRDS(paste0(path_ep_folder,"/references/PubMed Queries/Phase 4 Precision Medicine Abstract Review_2023-05-19.RDS"))  %>% 
  bind_rows() %>% 
  mutate(strata = "Summer 2023") 

ada_master_list = bind_rows(phase1and2,
          phase3,
          phase4) %>% 
  left_join(screens,
            by=c("strata","gbd_region"="region","PMID")) %>% 
  mutate(fall = case_when(strata == "Fall 2022" ~ 1,
                          TRUE ~ 0),
         sampled = case_when(!is.na(ada_precision_medicine) ~ 1,
                             !is.na(ada_is_diabetes) ~ 1,
                             TRUE ~ 0),
         review_category = case_when(review_strata == "Matched" ~ 1,
                                     review_strata == "R1_Only" ~ 2,
                                     review_strata == "R2_Only" ~ 3,
                                     review_strata == "R3_Completed" ~ 4,
                                     TRUE ~ 0)) %>% 
  ungroup() 




ada_summary_master = ada_master_list %>% 
  group_by(sampled,strata,gbd_region,review_strata,outcome) %>% 
  tally()

ada_master_list %>% 
  dplyr::filter(sampled == 1) %>% 
  group_by(gbd_region) %>% 
  tally()

saveRDS(ada_master_list,paste0(path_ep_folder,"/writing global equity/ada abstract/ada master list of reviews.RDS"))
saveRDS(ada_summary_master,paste0(path_ep_folder,"/writing global equity/ada abstract/ada summary of reviews by strata.RDS"))
