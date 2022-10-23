## ---------------------------
##
## Script name: owid-data.r
##
## Purpose of script: Load and transform owid covid data from daily to weekly & dropping unneeded columns.
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-10-23
##
## Copyleft (c) Nathanael Sheehan, 2022
## Email: nathanaelsheehan@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

owid = read.csv("../../Downloads/owid-covid-data.csv")

owid_temp = owid |>
  # remove counties and agggregated data
  dplyr::filter(
    location != "World" |
      location != "Africa" |
      location != "Asia" |
      location != "Europe" |
      location != "North America" |
      location != "South America" |
      location != "Oceania"
  ) |>
  dplyr::select(1:6) |>
  dplyr::select(-c("total_cases")) |>
  dplyr::mutate(date = lubridate::ymd(date)) |>
  dplyr::mutate(wy = isodate(date)) |>
  dplyr::group_by(iso_code, continent, location, wy) |>
  dplyr::summarise_if(is.numeric, sum)

owida_temp$total_cases = ave(owida_temp$new_cases,
                             owida_temp$location,
                             FUN = cumsum)

owid_temp2 = owid |>
  # remove counties and agggregated data
  dplyr::filter(
    location != "World" |
      location != "Africa" |
      location != "Asia" |
      location != "Europe" |
      location != "North America" |
      location != "South America" |
      location != "Oceania"
  ) |>
  dplyr::select(1:4,48:67) |>
  dplyr::mutate(date = lubridate::ymd(date)) |>
  dplyr::mutate(wy = isodate(date)) |>
  dplyr::group_by(iso_code, continent, location, wy) |>
  dplyr::summarise_if(is.numeric, mean)

owid = right_join(owid_temp,owid_temp2)
