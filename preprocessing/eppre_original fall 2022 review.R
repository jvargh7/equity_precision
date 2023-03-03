# Fall 2022 --------
r0_df = map_dfr(sheet_names[1:3],
                function(s){
                  readxl::read_excel(paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract Review_R1orig.xlsx"),sheet=s) %>% 
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
                  
                })