list_of_datasets <- readRDS(paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract Review_2023-01-25.RDS"))


cleaned_list <- map(list_of_datasets,
                    function(df){
                      
                      if(nrow(df)>100){
                        c = df %>% 
                          dplyr::filter(previously_screened == "Yes"|(previously_screened == "No" & random_screen == 1))
                      } else{c = df}
                      
                      c %>% 
                        ungroup() %>% 
                        dplyr::select(PMID:primary_study) %>% 
                        mutate_at(vars(precision_medicine:primary_study),function(x) "")
                      
                      
                    })
library(openxlsx)

write.xlsx(cleaned_list, file = paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract for R2_",Sys.Date(),".xlsx"))
