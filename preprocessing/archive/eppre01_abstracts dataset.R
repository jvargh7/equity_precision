# <-- This is for commenting
# Add path to folder from Mac
# %>%: This is a pipe operator. Cmd + Shift + M 
# The pipe operator is used to pass datasets from one step to the next step 

source("preprocessing/ep_variables.R")

south_asians <- readxl::read_excel("data/Precision Medicine Abstract Review.xlsx",
                              sheet = "South Asians") %>% 
  # Format for rename is new_column = original column
  # OR "new column" = original_column
  # OR "new_column" = "original_column"
  dplyr::rename(precision_medicine = "Precision Medicine",
                publication_year = 'Publication Year',
                title = "Title",
                abstract = "Abstract",
                is_diabetes = Diabetes,
                correct_population = "South Asians",
                primary_study = "Primary Study or Secondary Data Analysis (vs Systematic Review/ Perspectives)"
  ) %>% 
  dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
  mutate(source_population = "South Asians",
         weight = 1)

latin_americans <- readxl::read_excel("data/Precision Medicine Abstract Review.xlsx",
                                   sheet = "Latin Americans")  %>% 

  dplyr::rename(precision_medicine = "Precision Medicine",
                publication_year = 'Publication Year',
                title = "Title",
                abstract = "Abstract",
                is_diabetes = Diabetes,
                correct_population = "Latin Americans",
                primary_study = "Primary Study or Secondary Data Analysis (vs Systematic Review/ Perspectives)"
  ) %>% 
  dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study) %>% 
  mutate(source_population = "Latin Americans",
         weight = 1)

east_asians <- readxl::read_excel("data/Precision Medicine Abstract Review.xlsx",
                                      sheet = "East Asians") %>% 
  dplyr::rename(precision_medicine = "Precision Medicine",
                publication_year = 'Publication Year',
                title = "Title",
                abstract = "Abstract",
                is_diabetes = Diabetes,
                random_screen = 'Random Screen',
                correct_population = "East Asians",
                primary_study = "Primary Study or Secondary Data Analysis (vs Systematic Review/ Perspectives)"
  ) %>% 
  dplyr::select(PMID, PMCID, publication_year, title, abstract, precision_medicine,is_diabetes,correct_population,primary_study,random_screen) %>% 
  mutate(source_population = "East Asians")

weights = nrow(east_asians)/table(east_asians$random_screen)

east_asians <- east_asians %>% 
  dplyr::filter(random_screen == 1) %>% 
  mutate(weight = case_when(!is.na(precision_medicine) ~ weights["1"],
                            TRUE ~ NA_real_)) %>% 
  dplyr::select(-random_screen)


# Matching abstracts --------
pooled <- bind_rows(south_asians,
          east_asians,
          latin_americans)

# https://astrostatistics.psu.edu/su07/R/html/base/html/agrep.html
matched_abstracts <- map_dfr(1:nrow(pooled),
                             function(x){
                               
                               t = pooled[x,]$title;
                               a = pooled[x,]$abstract;
                               
                               s = paste0(t,"\n",a);
                               
                               out = map_dfc(c(countries_east_asia,countries_latin_america,countries_south_asia,pm_variables,dm_variables),
                                       function(w){
                                         is_match = agrep(w,s);
                                         
                                         data.frame(V1 = ifelse(identical(is_match,integer(0)),0,1)) %>% 
                                           rename_at(vars(V1),~w) %>% 
                                           return(.)
                                         
                                       }
                                       
                                       )
                               
                               out %>% 
                                 mutate(PMID = pooled[x,]$PMID) %>% 
                                 return(.)
                               
                             })


abstracts_df <- bind_cols(pooled,
                          matched_abstracts %>% mutate_at(vars(-PMID),~as.numeric(.))%>% 
                            dplyr::select(-PMID))
saveRDS(abstracts_df,paste0(path_ep_folder,"/working/abstracts_df.RDS"))
