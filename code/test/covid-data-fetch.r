## ---------------------------
##
## Script name: data-wrangle.r
##
## Purpose of script: Download and transform Covid-19 cases, deaths, testing and vaccines data
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

##################################################################
##                        Covid-19 cases                        ##
##################################################################
jh_global_covid = fetch_data(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                             path = "data/covid-jh.csv" ## fetch latest github data
) |>
  dplyr::select(-c(Lat, Long)) |> # be gone lat long
  dplyr::rename(Country = 2) |>
  dplyr::mutate(Country = ifelse(Province.State == "French Guiana" & Country == "France", "French Guiana", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "French Polynesia" & Country == "France", "French Polynesia", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Guadeloupe" & Country == "France", "Guadeloupe", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Martinique" & Country == "France", "Martinique", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Mayotte" & Country == "France", "Mayotte", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "New Caledonia" & Country == "France", "New Caledonia", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Reunion" & Country == "France", "Reunion", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Saint Barthelemy" & Country == "France", "Saint Barthelemy", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Saint Pierre and Miquelon" & Country == "France", "Saint Pierre and Miquelon", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "St Martin" & Country == "France", "St Martin", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Wallis and Futuna" & Country == "France", "Wallis and Futuna", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Curacao" & Country == "Netherlands", "Curacao", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Sint Maarten" & Country == "Netherlands", "	Sint Maarten", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Aruba" & Country == "Netherlands", "Aruba", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Bonaire, Sint Eustatius and Saba" & Country == "Netherlands", "Bonaire, Sint Eustatius and Saba", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Anguilla" & Country == "United Kingdom", "Anguilla", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Bermuda" & Country == "United Kingdom", "Bermuda", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "British Virgin Islands" & Country == "United Kingdom", "British Virgin Islands", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Cayman Islands" & Country == "United Kingdom", "Cayman Islands", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Channel Islands" & Country == "United Kingdom", "Channel Islands", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Gibraltar" & Country == "United Kingdom", "Gibraltar", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Montserrat" & Country == "United Kingdom", "Montserrat", Country)) |>
  dplyr::mutate(Country = ifelse(Province.State == "Turks and Caicos Islands" & Country == "United Kingdom", "Turks and Caicos Islands", Country)) |>
  dplyr::select(-c(Province.State)) |>
  tidyr::pivot_longer(!Country, names_to = "Date", values_to = "cases") |>
  dplyr::mutate(Date = paste0(sub('.', '', Date))) |>
  dplyr::mutate(Country = case_when(
    Country == "US" ~ "USA",
    Country == "The Bahamas" ~ "Bahamas",
    Country == "Taiwan*" ~ "Taiwan",
    Country == "Korea, South" ~ "South Korea",
    Country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
    Country == "Congo (Brazzaville)" ~ "Republic of the Congo",
    Country == "Czechia" ~ "Czech Republic",
    Country == "Wallis and Futuna" ~ "Wallis and Futuna Islands",
    Country == "Bonaire, Sint Eustatius and Saba" ~ "Bonaire",
    TRUE ~ Country
  )) |> dplyr::mutate(Date = mdy(Date)) |>  mutate(Date = as.Date(Date)) |>
  dplyr::mutate(wy = isodate(Date))

jh_global_covid = aggregate(cases ~ Country + wy, jh_global_covid, sum)

# Vaccines ----------------------------------------------------------------
jh_vaccine = fetch_data(url = "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv",
                        path = "data/js-vac.csv") |>
  dplyr::select(Province_State,Country_Region, Doses_admin,People_at_least_one_dose,Date) |>
  dplyr::rename(Country = Country_Region) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "French Guiana" &
        Country == "France",
      "French Guiana",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "French Polynesia" &
        Country == "France",
      "French Polynesia",
      Country
    )
  ) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "Guadeloupe" &
      Country == "France",
    "Guadeloupe",
    Country
  )) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "Martinique" &
      Country == "France",
    "Martinique",
    Country
  )) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "Mayotte" &
      Country == "France",
    "Mayotte",
    Country
  )) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "New Caledonia" &
        Country == "France",
      "New Caledonia",
      Country
    )
  ) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "Reunion" &
      Country == "France",
    "Reunion",
    Country
  )) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Saint Barthelemy" &
        Country == "France",
      "Saint Barthelemy",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Saint Pierre and Miquelon" &
        Country == "France",
      "Saint Pierre and Miquelon",
      Country
    )
  ) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "St Martin" &
      Country == "France",
    "St Martin",
    Country
  )) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Wallis and Futuna" &
        Country == "France",
      "Wallis and Futuna",
      Country
    )
  ) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "Curacao" &
      Country == "Netherlands",
    "Curacao",
    Country
  )) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Sint Maarten" &
        Country == "Netherlands",
      "	Sint Maarten",
      Country
    )
  ) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "Aruba" &
      Country == "Netherlands",
    "Aruba",
    Country
  )) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Bonaire, Sint Eustatius and Saba" &
        Country == "Netherlands",
      "Bonaire, Sint Eustatius and Saba",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Anguilla" &
        Country == "United Kingdom",
      "Anguilla",
      Country
    )
  ) |>
  dplyr::mutate(Country = ifelse(
    Province_State == "Bermuda" &
      Country == "United Kingdom",
    "Bermuda",
    Country
  )) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "British Virgin Islands" &
        Country == "United Kingdom",
      "British Virgin Islands",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Cayman Islands" &
        Country == "United Kingdom",
      "Cayman Islands",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Channel Islands" &
        Country == "United Kingdom",
      "Channel Islands",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Gibraltar" &
        Country == "United Kingdom",
      "Gibraltar",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Montserrat" &
        Country == "United Kingdom",
      "Montserrat",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = ifelse(
      Province_State == "Turks and Caicos Islands" &
        Country == "United Kingdom",
      "Turks and Caicos Islands",
      Country
    )
  ) |>
  dplyr::mutate(
    Country = dplyr::case_when(
      Country == "US" ~ "USA",
      Country == "The Bahamas" ~ "Bahamas",
      Country == "Taiwan*" ~ "Taiwan",
      Country == "Korea, South" ~ "South Korea",
      Country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
      Country == "Congo (Brazzaville)" ~ "Republic of the Congo",
      Country == "Czechia" ~ "Czech Republic",
      Country == "Wallis and Futuna" ~ "Wallis and Futuna Islands",
      Country == "Bonaire, Sint Eustatius and Saba" ~ "Bonaire",
      TRUE ~ Country
    )
  ) |>
  dplyr::select(-c(Province_State)) |>
  dplyr::mutate(Date = as.Date(Date)) |>
  dplyr::mutate(wy = isodate(Date))

jh_vaccine = aggregate(Doses_admin ~ Country + wy, jh_vaccine, sum)

jh_covid_data = dplyr::left_join(jh_global_covid,jh_vaccine, by=c("Country", "wy")) |>
  dplyr::rename(country = Country)

rm(jh_vaccine,jh_global_covid)
