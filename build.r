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
  main_df = readRDS("raw-data/main_df.rds")
} else {
  source("code/set-up.r")
  source("code/owid-data.r")
  source("code/data-wrangle.r")
}

