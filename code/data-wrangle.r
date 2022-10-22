## ---------------------------
##
## Script name: data-wrangle.r
##
## Purpose of script: Download and transform metadata from GISAID, Covid-19 Data Portal and NCBI
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-03-21
##
## Copyleft (c) Nathanael Sheehan, 2022
## Email: nathanaelsheehan@gmail.co.uk
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------


#################################################################
##             Covid-19 Data Platform Monthly Data             ##
#################################################################

embl = readr::read_tsv("raw-data//embl-sequence-metadata.tsv")
embl = embl |>
  select(collection_date, country) |>
  dplyr::mutate(collection_date = paste0(
    substr(collection_date, 1, 4),
    "-",
    substr(collection_date, 5, 6),
    "-",
    substr(collection_date, 7, 8)
  )) |>
  dplyr::mutate(collection_date = lubridate::ymd(collection_date)) |>
  dplyr::filter(collection_date > '2020-01-01') |>
  dplyr::mutate(wy = isodate(collection_date)) |>
  group_by(wy, country) |>
  dplyr::select(-c(collection_date)) |>
  dplyr::summarise(Count = n()) |>
  dplyr::mutate(
    country = case_when(
      country == "West Bank" ~ "Palestine",
      country == "Viet Nam" ~ "Vietnam",
      TRUE ~ country
    )
  ) |>
  dplyr::rename(C19DP.weekly.submissions = Count)

embl$CD19DP.total.Submissions = ave(embl$C19DP.weekly.submissions,
                                    embl$country,
                                    FUN = cumsum)


write_rds(embl, "data/embl.RDS")



#######################################################################################################
##                                    GISAID Global Monthly Data                                     ##
##  GISAID monthly metadata submissions downloaded from https://www.epicov.org/epi3/frontend#5dc229  ##
#######################################################################################################

gisaid = as.data.frame(
  readr::read_tsv("raw-data/gisaid_variants_statistics.tsv") |>
    dplyr::rename(Date = `Week prior to`) |>
    dplyr::rename(country = Country) |>
    dplyr::mutate(Date = lubridate::ymd(Date)) |>
    dplyr::mutate(wy = isodate(Date)) |>
    dplyr::select(c(
      country, wy, `Submission Count`, `Total per Country and Week`
    )) |>
    dplyr::rename(GISAID.weekly.submissions = `Submission Count`) |>
    dplyr::rename(GISAID.total.submissions = `Total per Country and Week`)

)

write_rds(gisaid, "data/gisaid.RDS")


#################################################################
##                          Join data                          ##
#################################################################

main_df = left_join(gisaid, embl, by = c("country", "wy")) |> left_join(jh_covid_data, by =
                                                                          c("country", "wy")) |>
  mutate("Genomes per confirmed cases (GISAID)" = GISAID.total.submissions / cases) |>
  mutate("Genomes per confirmed cases (C19DP)" =  CD19DP.total.Submissions / cases) |>
  mutate("Genomes per confirmed full vaccine (GISAID)" = GISAID.total.submissions / Doses_admin) |>
  mutate("Genomes per confirmed full vaccine (C19DP)" =  CD19DP.total.Submissions / Doses_admin) |>
  {
    \(.) {
      replace(., is.na(.), 0)
    }
  }()

#
# ##################################################################
# ##                           GeneBank                           ##
# ##################################################################
#
#
# ncbi = read.csv("../../Downloads/sequences(6).csv")
#
# genebank = read.csv("../../Downloads/sequences.csv") |>
#   select(Country, Release_Date) |>
#   mutate(Date = as.Date(Release_Date)) |>
#   mutate(Date = substr(Date, start = 1, stop = 7)) |>
#   group_by(Date, Country) |>
#   summarise(genebank.monthly.submissions = n()) |>
#   mutate(
#     Country = case_when(
#       Country == "West Bank" ~ "Palestine",
#       Country == "Viet Nam" ~ "Vietnam",
#       TRUE ~ Country
#     )
#   )
# genebank$genebank.total.Submissions = ave(genebank$genebank.monthly.submissions,
#                                           genebank$Country,
#                                           FUN = cumsum)
#
# write_rds(genebank, "data/genebank.RDS")
#
#
