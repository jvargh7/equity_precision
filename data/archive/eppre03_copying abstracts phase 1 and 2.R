path_ep_pubmed_queries <- paste0(path_ep_folder,"/references/PubMed Queries/Refined Queries 2023-01-24_AS")

south_asians = read_csv(paste0(path_ep_pubmed_queries,"/SOUTH ASIA/South Asia CSV 2023-24-01_AS.csv"))
east_asians = read_csv(paste0(path_ep_pubmed_queries,"/EAST ASIA/East Asia CSV 2023-24-01_AS.csv"))
latin_america = read_csv(paste0(path_ep_pubmed_queries,"/LATIN AMERICA & CARIBBEAN/Latin American & Caribbean CSV 2023-24-01_AS.csv"))
middle_east = read_csv(paste0(path_ep_pubmed_queries,"/MIDDLE EAST AND NORTH AFRICA/MENA CSV 2023-24-01_AS.csv"))
southeast_asians = read_csv(paste0(path_ep_pubmed_queries,"/SOUTHEAST ASIA & PACIFIC ISLANDS/Southeast Asia and Pacific Islands CSV 2023-24-01_AS.csv"))

pubmed_extracts = bind_rows(south_asians %>% mutate(gbd_region = "South Asia"),
          east_asians %>% mutate(gbd_region = "East Asia"),
          latin_america %>% mutate(gbd_region = "Latin America and Caribbean"),
          middle_east %>% mutate(gbd_region = "Middle East and North Africa"),
          southeast_asians %>% mutate(gbd_region = "South East Asia and Pacific Islands"))


previously_reviewed_abstracts <- readRDS(paste0(path_ep_folder,"/working/abstracts_df.RDS")) %>% 
  dplyr::select(PMID, precision_medicine,is_diabetes,correct_population,primary_study)

library(xml2)

# library(furrr)
# options(future.globals.maxSize= (1*1024*1024)^3) #1GB
# # https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
# plan(multisession, workers = 3)

# https://academia.stackexchange.com/questions/67103/is-there-any-api-service-to-retrieve-abstract-of-a-journal-article
# https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_EFetch_
# Reading abstracts from PubMed ----------
start_time <- Sys.time()
extracted_abstracts = map_dfr(pubmed_extracts$PMID,
                       function(p){
                         address = paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",p,"&retmode=XML&rettype=abstract");
                         abstract = tryCatch({as_list(read_xml(address))$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Abstract %>% 
                             unlist() %>% 
                             paste0(.,collapse="") %>% 
                             as.character()},
                             error = function(e){NA});
                         data.frame(PMID = p,
                                    abstract = abstract) %>% 
                           return(.)

                       })
end_time <- Sys.time()
end_time - start_time

# Merging metadata ----------
merged_extracts = pubmed_extracts %>% 
  left_join(extracted_abstracts,
            by="PMID") %>% 
  distinct(PMID,.keep_all = TRUE)

# Linking abstracts to metadata --------
sa_df = merged_extracts %>% dplyr::filter(gbd_region == "South Asia") %>% 
  left_join(previously_reviewed_abstracts,
            by = "PMID") %>% 
  distinct(PMID,.keep_all = TRUE) %>% 
  mutate(previously_screened = case_when(!is.na(precision_medicine) ~ "Yes",
                                         TRUE ~ "No")) %>% 
  group_by(previously_screened) %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

ea_df = merged_extracts %>% dplyr::filter(gbd_region == "East Asia") %>% 
  left_join(previously_reviewed_abstracts,
            by = "PMID") %>% 
  distinct(PMID,.keep_all = TRUE) %>% 
  mutate(previously_screened = case_when(!is.na(precision_medicine) ~ "Yes",
                                         TRUE ~ "No")) %>% 
  group_by(previously_screened) %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

la_df = merged_extracts %>% dplyr::filter(gbd_region == "Latin America and Caribbean") %>% 
  left_join(previously_reviewed_abstracts,
            by = "PMID") %>% 
  distinct(PMID,.keep_all = TRUE) %>% 
  mutate(previously_screened = case_when(!is.na(precision_medicine) ~ "Yes",
                                         TRUE ~ "No")) %>% 
  group_by(previously_screened) %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

me_df = merged_extracts %>% dplyr::filter(gbd_region == "Middle East and North Africa") %>% 
  left_join(previously_reviewed_abstracts,
            by = "PMID") %>% 
  mutate(previously_screened = case_when(!is.na(precision_medicine) ~ "Yes",
                                         TRUE ~ "No")) %>% 
  group_by(previously_screened) %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

sea_df = merged_extracts %>% dplyr::filter(gbd_region == "South East Asia and Pacific Islands") %>% 
  left_join(previously_reviewed_abstracts,
            by = "PMID") %>% 
  mutate(previously_screened = case_when(!is.na(precision_medicine) ~ "Yes",
                                         TRUE ~ "No")) %>% 
  group_by(previously_screened) %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

# https://stackoverflow.com/questions/27713310/easy-way-to-export-multiple-data-frame-to-multiple-excel-worksheets
library(openxlsx)
list_of_datasets <- list("South Asia" = sa_df, 
                         "East Asia" = ea_df,
                         "Latin America" = la_df,
                         "Middle East" = me_df,
                         "South East Asia" = sea_df)

saveRDS(list_of_datasets,paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract Review_",Sys.Date(),".RDS"))

write.xlsx(list_of_datasets, file = paste0(path_ep_folder,"/references/PubMed Queries/Precision Medicine Abstract Review_",Sys.Date(),".xlsx"))
