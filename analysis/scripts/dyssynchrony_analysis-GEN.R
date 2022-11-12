knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(purrr)
library(tidyr)
library(dplyr)
library(furrr)

plan(multicore)

# Set paths
PROJECT_DIR <- rprojroot::find_rstudio_root_file()
RAW_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "raw_data")
DERIVED_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "derived_data")
FIG_DIR <- file.path(PROJECT_DIR, "analysis", "figures")

source(file.path(PROJECT_DIR, "R/strain_cure.R"))
source(file.path(PROJECT_DIR, "R/strain_ssi.R"))

dillacs <- readRDS(file.path(DERIVED_DATA_DIR, "cleaned_data.Rds"))

segment_order <-
    c("anteroseptal",
      "inferoseptal",
      "inferior",
      "inferolateral",
      "anterolateral",
      "anterior")

dillacs$cure <- 
    map2_dbl(dillacs$sax_segmentcirc_mid,
             dillacs$avc_tfind,
             ~strain_cure(
                 select(.x, -time), 
                 limit = .y,
                 order = segment_order
                 )
             )

dillacs$ssi <-
    pmap_dbl(
        list(data = map(dillacs$sax_segmentcirc_mid, select, -time),
             time = map(dillacs$sax_segmentcirc_mid, select, time),
             time_avo = dillacs$avo_tfind,
             time_avc = dillacs$avc_tfind),
        strain_ssi,
        septal_segments = c("anteroseptal", "inferoseptal"),
        lateral_segments = c("anterolateral", "inferolateral")
    )


saveRDS(dillacs, file.path(DERIVED_DATA_DIR, "dyssynchrony_data.Rds"))
