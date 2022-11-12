knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Set paths
PROJECT_DIR <- rprojroot::find_rstudio_root_file()
RAW_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "raw_data")
DERIVED_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "derived_data")
FIG_DIR <- file.path(PROJECT_DIR, "analysis", "figures")

library(furrr)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
source(file.path(PROJECT_DIR, "R", "get_segment_data.R"))
plan(sequential)

files_path <- list.files(file.path(RAW_DATA_DIR, "mri_data", "analyzed"), 
                         pattern = "*.mat", full.names = TRUE)

dillacs <- future_map_dfr(files_path, get_segment_data, .progress = TRUE)


dillacs <- dillacs %>%
    mutate(image_id = unlist(image_id)) %>%
    rename(id = "image_id")

demo <- read.csv(file.path(RAW_DATA_DIR, "demographics.csv"),
                 stringsAsFactors = FALSE)

demo <- demo %>% 
    mutate(id = as.character(id),
           lbbb1_control0 = factor(lbbb1_control0,
                                   levels = list("Control", "LBBB")),
           Gender = factor(Gender,
                           levels = list("F", "M")))


dillacs <- left_join(dillacs, demo, by = "id")

saveRDS(dillacs, file.path(DERIVED_DATA_DIR, "cleaned_data.Rds"))
