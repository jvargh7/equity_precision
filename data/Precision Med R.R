# Installing packages
install.packages("tidyverse")
library(tidyverse)

install.packages("readxl")
library(readxl)

install.packages("purrr")
library(purrr)

install.packages("dplyr")
library(dplyr)

# Loading Excel files into R
read_excel_workbook <- function(path) {
  sheets <- excel_sheets(path) 
  map_df(sheets, ~ read_excel(path, sheet = .x), .id = "sheet_name")
}

directory_path <- "/Users/aamnasoniwala/Library/CloudStorage/OneDrive-SharedLibraries-EmoryUniversity/Varghese, Jithin Sam - Global Equity in Diabetes Precision Medicine Research/references/PubMed Queries/R Studio 2024 Data Set"

excel_files <- list.files(directory_path, pattern = "\\.xlsx$", full.names = TRUE)

all_data <- map_df(excel_files, read_excel_workbook, .id = "file_name")







