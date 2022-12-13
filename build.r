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
  source("code/set-up.r")
  source("code/owid-data.r")
  source("code/data-wrangle.r")
  source("code/plot-continent-landscape.r")
  source("code/plot-temporal-submissions.r")
  source("code/plot-treemap.r")
  source("code/ena-corpus-analysis.r")
  source("code/gisaid-corpus-analysis.r")
}

unsd = read.csv("../../Downloads/UNSD — Methodology.csv",sep = ";") %>% select(Sub.region.Name,ISO.alpha3.Code) %>%rename(iso_code = ISO.alpha3.Code)
main_df = right_join(main_df,unsd)
income_groups = read.csv("raw-data/income-groups.csv") %>% rename(iso_code = Code)
main_df = right_join(main_df,income_groups)
rm(income_groups,unsd)
