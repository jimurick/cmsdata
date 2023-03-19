

library(tidyverse)
library(magrittr)
library(readxl)
library(data.table)

dirs <- list(
  zip = "zips",
  rdata = "rdata",
  table = "tables"
)

wrong_dash  <- "\U2013"
wrong_apost <- "\U2019"

fix_dashes      <- function(x) { gsub(wrong_dash,  "-", x) }
fix_apostrophes <- function(x) { gsub(wrong_apost, "'", x) }
fix_non_ascii   <- function(x) { fix_dashes(fix_apostrophes(x)) }

fix_utf <- function(x) {
  gsub("\x92", "'", gsub("\x96", "-", gsub("\x94", "", x)))
}

get_unzipped_files_by_pattern <- function(file_re) {
  all_year_dirs <- list.dirs(dirs$zip, recursive = FALSE)
  dir_names <- str_extract(all_year_dirs, "20[0-2][0-9]")
  all_year_dirs <- setNames(all_year_dirs, dir_names)
  f <- function(dirname) {
    all_files <- list.files(path = dirname, recursive = TRUE, full.names = TRUE)
    all_files[grepl(file_re, all_files, ignore.case = TRUE)]
  }
  unlist(lapply(all_year_dirs, f))
}

