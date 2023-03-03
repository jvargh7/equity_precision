source(".Rprofile")

matched_reviews <- readxl::read_excel(path=paste0(path_ep_folder,"/working/dataset of screens_R3.xlsx")) %>% 
  dplyr::filter(is.na(r3_precision_medicine)) %>% 
  dplyr::filter(!is.na(r1_precision_medicine), !is.na(r2_precision_medicine)) %>% 
  dplyr::select(strata,PMID,PMCID,title,abstract,region,contains("r1_"))

r1_reviews <- readxl::read_excel(path=paste0(path_ep_folder,"/working/dataset of screens_R3.xlsx")) %>% 
  dplyr::filter(is.na(r3_precision_medicine)) %>% 
  dplyr::filter(!is.na(r1_precision_medicine), is.na(r2_precision_medicine)) %>% 
  dplyr::select(strata,PMID,PMCID,title,abstract,region,contains("r1_"))

r2_reviews <- readxl::read_excel(path=paste0(path_ep_folder,"/working/dataset of screens_R3.xlsx")) %>% 
  dplyr::filter(is.na(r3_precision_medicine)) %>% 
  dplyr::filter(is.na(r1_precision_medicine), !is.na(r2_precision_medicine)) %>% 
  dplyr::select(strata,PMID,PMCID,title,abstract,region,contains("r2_"))

r3_reviews <- readxl::read_excel(path=paste0(path_ep_folder,"/working/dataset of screens_R3.xlsx")) %>% 
  dplyr::filter(!is.na(r3_precision_medicine)) %>% 
  dplyr::select(strata,PMID,PMCID,title,abstract,region,contains("r3_"))


combined_reviews <- bind_rows(
  matched_reviews %>% 
    rename_at(vars(contains("r1_")),~str_replace(.,"r1_","")) %>% 
    mutate(review_strata = "Matched"),
  
  r1_reviews %>% 
    rename_at(vars(contains("r1_")),~str_replace(.,"r1_","")) %>% 
    mutate(review_strata = "R1_Only"),
  
  r2_reviews %>% 
    rename_at(vars(contains("r2_")),~str_replace(.,"r2_","")) %>% 
    mutate(review_strata = "R2_Only"),
  
  r3_reviews %>% 
    rename_at(vars(contains("r3_")),~str_replace(.,"r3_","")) %>% 
    mutate(review_strata = "R3_Completed")
  
  
)

source("preprocessing/eppre_original fall 2022 review.R")



master_list = map_dfr(sheet_names,
                              function(s){
                                
                                readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract Review_2023-01-25_V2.xlsx"),sheet=s) %>%
                                  dplyr::rename(
                                    publication_year = 'Publication Year',
                                    title = "Title",
                                    abstract = "abstract") %>% 
                                  dplyr::select(PMID, PMCID, publication_year) %>% 
                                  mutate(region = s)
                                
                              }) %>% 
  mutate(strata = case_when(PMID %in% r0_df$PMID ~ "Fall 2022",
                            TRUE ~ "Spring 2023")) %>% 
  left_join(combined_reviews %>% dplyr::select(-PMCID),
            by=c("strata","region","PMID")) %>% 
  mutate(fall = case_when(strata == "Fall 2022" ~ 1,
                          TRUE ~ 0),
         sampled = case_when(!is.na(precision_medicine) ~ 1,
                             !is.na(is_diabetes) ~ 1,
                             TRUE ~ 0),
         review_category = case_when(review_strata == "Matched" ~ 1,
                                     review_strata == "R1_Only" ~ 2,
                                     review_strata == "R2_Only" ~ 3,
                                     review_strata == "R3_Only" ~ 4,
                                     TRUE ~ 0)) %>% 
  mutate(outcome = rowSums(.[,c("precision_medicine","is_diabetes","correct_population","primary_study")])) %>% 
  mutate(outcome = case_when(outcome == 4 ~ 1,
                             TRUE ~ 0))




summary_master = master_list %>% 
  group_by(sampled,strata,region,review_strata,outcome) %>% 
  tally()

saveRDS(master_list,paste0(path_ep_folder,"/working/master list of reviews.RDS"))
saveRDS(summary_master,paste0(path_ep_folder,"/working/summary of reviews by strata.RDS"))

