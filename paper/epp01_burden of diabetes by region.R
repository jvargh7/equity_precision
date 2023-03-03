
burden <- readRDS(paste0(path_ep_folder,"/working/diabetes estimates.RDS"))

burden %>% 
  group_by(Region) %>% 
  summarize_at(vars(-Country),~round(sum(.,na.rm=TRUE)/10^6,2)) %>% 
  write_csv(.,"paper/burden by region.csv")


burden %>% 
  dplyr::filter(!is.na(Country)) %>% 
  group_by(Region) %>% 
  mutate_at(vars(Y2021,Y2030,Y2045,DALY,YLL,YLD),~paste0(round(.*100/sum(.),1),"%"))  %>% 
  write_csv(.,"paper/burden by country.csv")

