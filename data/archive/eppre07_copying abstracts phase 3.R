path_ep_pubmed_queries <- paste0(path_ep_folder,"/references/PubMed Queries/Refined Queries 2023-01-24_AS")

central_asians = read_csv(paste0(path_ep_pubmed_queries,"/CENTRAL ASIA/csv-1AND2AND3-set (3).csv"))
centraleast_europe = read_csv(paste0(path_ep_pubmed_queries,"/EASTERN & CENTRAL EUROPE/csv-1AND2AND3-set (3).csv"))
subsaharan_africa = read_csv(paste0(path_ep_pubmed_queries,"/SUB-SAHARAN AFRICA/csv-1AND2AND3-set (3).csv"))

pubmed_extracts = bind_rows(central_asians %>% mutate(gbd_region = "Central Asia"),
                            centraleast_europe %>% mutate(gbd_region = "Central and Eastern Europe"),
                            subsaharan_africa %>% mutate(gbd_region = "Sub Saharan Africa"))


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
ca_df = merged_extracts %>% dplyr::filter(gbd_region == "Central Asia")  %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

cee_df = merged_extracts %>% dplyr::filter(gbd_region == "Central and Eastern Europe") %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

ssa_df = merged_extracts %>% dplyr::filter(gbd_region == "Sub Saharan Africa") %>% 
  mutate(random_screen = rbinom(n(),1,prob=100/n()))

# https://stackoverflow.com/questions/27713310/easy-way-to-export-multiple-data-frame-to-multiple-excel-worksheets
library(openxlsx)
list_of_datasets <- list("Central Asia" = ca_df, 
                         "Central and Eastern Europe" = cee_df,
                         "Sub Saharan Africa" = ssa_df)

saveRDS(list_of_datasets,paste0(path_ep_folder,"/references/PubMed Queries/Phase 3 Precision Medicine Abstract Review_",Sys.Date(),".RDS"))

write.xlsx(list_of_datasets, file = paste0(path_ep_folder,"/references/PubMed Queries/Phase 3 Precision Medicine Abstract Review_",Sys.Date(),".xlsx"))
