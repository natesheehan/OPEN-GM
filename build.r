## ---------------------------
##
## Script name: build.r
##
## Purpose of script: Complete a reproducible build for project or load data
##
## Author: Nathaneal Sheehan
##
## Date Created: 2021-10-22
##
## Email: nathanaelsheehan@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
reproducible = FALSE

if (reproducible == FALSE) {
  source("code/set-up.r")
  main_df = readRDS("data/main_df.rds")
} else {
  source("code/utils.r")
  source("code/pkgs.r")
  source("code/owid-data.r")
  source("code/data-wrangle.r")
  source("code/visualise-database-landscape.R")
  source("code/ena-corpus-analysis.r")
  source("code/gisaid-corpus-analysis.r")
}
