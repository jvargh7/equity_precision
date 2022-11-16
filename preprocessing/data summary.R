# library(readxl)
# library(dplyr)
# require(readxl)
# <-- This is for commenting
# Add path to folder from Mac
path_review <- "C:/code/external/equity_precision/data"
# paste0(path_review,"/Precision Medicine Abstract Review.xlsx")
south_asians <- readxl::read_excel(paste0(path_review,"/Precision Medicine Abstract Review.xlsx"),
                              sheet = "South Asians")

latin_americans <- readxl::read_excel(paste0(path_review,"/Precision Medicine Abstract Review.xlsx"),
                                   sheet = "Latin Americans")

east_asians <- readxl::read_excel(paste0(path_review,"/Precision Medicine Abstract Review.xlsx"),
                                      sheet = "East Asians")


# Renaming variables
colnames(south_asians)
colnames(latin_americans)
colnames(east_asians)

# %>%: This is a pipe operator. Cmd + Shift + M 
# The pipe operator is used to pass datasets from one step to the next step 
south_asians <- south_asians %>% 
  # Format for rename is new_column = original column
  # OR "new column" = original_column
  # OR "new_column" = "original_column"
  dplyr::rename(precision_medicine = "Precision Medicine",
                diabetes = Diabetes,
                south_asians = "South Asians",
                primary_study = "Primary Study or Secondary Data Analysis (vs Systematic Review/ Perspectives)"
                ) %>% 
  dplyr::select(PMID, precision_medicine,diabetes,south_asians,primary_study)

colnames(south_asians)
  

latin_americans <- latin_americans %>% 
  # Format for rename is new_column = original column
  # OR "new column" = original_column
  # OR "new_column" = "original_column"
  dplyr::rename(precision_medicine = "Precision Medicine",
                diabetes = Diabetes,
                latin_americans = "Latin Americans",
                random_screen = "Random Screen",
                primary_study = "Primary Study or Secondary Data Analysis (vs Systematic Review/ Perspectives)"
  ) %>% 
  dplyr::select(PMID, random_screen, precision_medicine,diabetes,latin_americans,primary_study)

colnames(latin_americans)

east_asians <- east_asians %>% 
  # Format for rename is new_column = original column
  # OR "new column" = original_column
  # OR "new_column" = "original_column"
  dplyr::rename(precision_medicine = "Precision Medicine",
                diabetes = Diabetes,
                east_asians = "East Asians",
                random_screen = "Random Screen",
                primary_study = "Primary Study or Secondary Data Analysis (vs Systematic Review/ Perspectives)"
  ) %>% 
  dplyr::select(PMID, random_screen, precision_medicine,diabetes,east_asians,primary_study) %>% 
  dplyr::filter(random_screen==1)

colnames(east_asians)


# How many are South Asians and How many are Precision Medicine?
with(south_asians,table(precision_medicine,south_asians))

