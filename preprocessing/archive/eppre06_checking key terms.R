source("preprocessing/ep_variables.R")

master_list <- readRDS(paste0(path_ep_folder,"/working/master list of reviews.RDS"))


matched_abstracts <- map_dfr(1:nrow(master_list),
                             function(x){
                               
                               t = master_list[x,]$title;
                               a = master_list[x,]$abstract;
                               
                               s = paste0(t,"\n",a);
                               
                               out = map_dfc(c(pm_variables,dm_variables),
                                             function(w){
                                               is_match = agrep(w,s);
                                               
                                               data.frame(V1 = ifelse(identical(is_match,integer(0)),0,1)) %>% 
                                                 rename_at(vars(V1),~w) %>% 
                                                 return(.)
                                               
                                             }
                                             
                               )
                               
                               out %>% 
                                 mutate(PMID = master_list[x,]$PMID) %>% 
                                 return(.)
                               
                             })

saveRDS(matched_abstracts,paste0(path_ep_folder,"/working/review terms present.RDS"))


master_list %>% 
  left_join(matched_abstracts,by="PMID") %>% 
  dplyr::filter(!is.na(precision_medicine)) %>% 
  group_by(sampled,strata,region,review_strata,genom) %>% 
  tally() %>% 
saveRDS(.,paste0(path_ep_folder,"/working/summary genom.RDS"))
