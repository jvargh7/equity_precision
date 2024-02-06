# Fall 2022 --------


# Renamed to Precision Medicine Abstract Review_R1orig --> Abstract Review_Aamna_Phase 1_2022-Fall
r0_df = map_dfr(c("South Asia","East Asia","Latin America"),
                function(s){
                  readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Abstract Review_Aamna_Phase 1_2022-Fall.xlsx"),sheet=s) %>% 
                    dplyr::rename(precision_medicine = "Precision Medicine",
                                  publication_year = 'Publication Year',
                                  title = "Title",
                                  abstract = "Abstract",
                                  is_diabetes = Diabetes,
                                  correct_population = s,
                                  primary_study = "Primary Study or Secondary Data Analysis (vs Systematic Review/ Perspectives)"
                    ) %>% 
                    dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
                    mutate(region = s,
                           reviewer = 1)
                  
                }) %>% 
  dplyr::filter(!is.na(precision_medicine) | !is.na(is_diabetes) | !is.na(correct_population) | !is.na(primary_study))
