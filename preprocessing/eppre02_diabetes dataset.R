
idf <- readxl::read_excel("data/Diabetes Estimates IDF and GBD.xlsx",sheet="IDF Diabetes Atlas") %>% 
  dplyr::filter(!is.na(Y2021)) %>% 
  mutate(Country = case_when(Country == "Hong Kong" ~ "China",
                             TRUE ~ Country)) %>% 
  group_by(Region,Country) %>% 
  summarize_all(~sum(.))
gbd <- readxl::read_excel("data/Diabetes Estimates IDF and GBD.xlsx",sheet="Global Burden of Disease latest") %>% 
  dplyr::filter(!is.na(DALY))

full_join(idf,
          gbd,
          by=c("Region","Country")) %>% 
  saveRDS(.,paste0(path_ep_folder,"/working/diabetes estimates.RDS"))
