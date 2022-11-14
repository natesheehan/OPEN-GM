## ---------------------------
##
## Script name: plot.r
##
## Purpose of script: visualise results
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

textcol = "white"
continent_loop = as.data.frame(unique(main_df$continent))


# GISAID ------------------------------------------------------------------
for (i in 1:nrow(continent_loop)) {
  plot_df = main_df  |>
    # convert state to factor and reverse order of levels
    mutate(country = factor(country, levels = rev(sort(unique(
      country
    ))))) |>
    # dplyr::filter(`Genomes per confirmed cases % (GISAID)` != Inf) |>
    # dplyr::filter(`Genomes per confirmed cases % (GISAID)` < 100) |>
    dplyr::filter(continent == continent_loop$`unique(main_df$continent)`[i]) |>
    dplyr::mutate(gpnc_gisaid = ifelse(gpnc_gisaid == Inf, 99998, gpnc_gisaid)) |> # missing case data
    dplyr::mutate(gpnc_gisaid = ifelse(is.na(gpnc_gisaid), 999998, gpnc_gisaid))  |> # missing seqeunce data
    dplyr::mutate(gpnc_gisaid = ifelse(gpnc_gisaid == 0, 9999998, gpnc_gisaid)) |> # missing seqeunce data
    # create a new variable from count
    mutate(countfactor = cut(
      gpnc_gisaid,
      breaks = c(0,
                 1,
                 2,
                 3,
                 4,
                 5,
                 20550,
                 99999,
                 999999,
                 max(gpnc_gisaid, na.rm = F)),
      labels = c(
        "0-1%",
        "1-2%",
        "2-3%",
        "3-4%",
        "4-5%",
        ">5%",
        "Missing Case Data",
        "Missing Case and Sequence Data",
        "Missing Sequence Data"
      )
    )) |>
    # change level order
    mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor))))

  ggplot(plot_df , aes(x = wy, y = country, fill = countfactor)) +
    #add border white colour of line thickness 0.25
    geom_bin_2d(colour = "white", size = 0.4) +
    guides(fill = guide_legend(title = "Percent of sequenced cases \nper epidemiological week")) +
    labs(
      x = "Epidemiological Week",
      y = "Country",
      title = paste0(
        "SARS-CoV2 sequences shared with GISAID in ",
        continent_loop$`unique(main_df$continent)`[i]
      ),
      caption = "Percent of reported cases (Our World In Data) that were sequenced and shared to the GISAID database per\n epidemiological week between December 19th 2019 and October 19th 2022. GISAID Metadata was accessed and used \nfollowing their terms of use. Epidemiological data was linked via joining Our World in Data on Covid-19 accessed on 24th October 2022."
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_grey(base_size = 10) +
    scale_fill_manual(
      values = c(
        "gray12",
        "gray47",
        "gray96",
        "#845ec2",
        "#d65db1" ,
        "#ff6f91",
        "#ff9671",
        "#ffc75f",
        "#f9f871",
        "#ddf1da"
      ),
      na.value = "grey90"
    ) + theme_landscape()

  ggsave(
    paste0(
      "plots/GISAID/",
      continent_loop$`unique(main_df$continent)`[i],
      " Sequence Landscape.png"
    ),
    dpi = 320,
    width = 18,
    height = 12,
    limitsize = FALSE
  )
}


# The Covid-19 Data Portal ------------------------------------------------
for (i in 1:nrow(continent_loop)) {
  plot_df = main_df  |>
    # convert state to factor and reverse order of levels
    mutate(country = factor(country, levels = rev(sort(unique(
      country
    ))))) |>
    # dplyr::filter(`Genomes per confirmed cases % (GISAID)` != Inf) |>
    # dplyr::filter(`Genomes per confirmed cases % (GISAID)` < 100) |>
    dplyr::filter(continent == continent_loop$`unique(main_df$continent)`[i]) |>
    dplyr::mutate(gpnc_embl = ifelse(gpnc_embl == Inf, 99998, gpnc_embl)) |> # missing case data
    dplyr::mutate(gpnc_embl = ifelse(is.na(gpnc_embl), 999998, gpnc_embl))  |> # missing seqeunce data
    dplyr::mutate(gpnc_embl = ifelse(gpnc_embl == 0, 9999998, gpnc_embl)) |> # missing seqeunce data
    # create a new variable from count
    mutate(countfactor = cut(
      gpnc_embl,
      breaks = c(0,
                 1,
                 2,
                 3,
                 4,
                 5,
                 20550,
                 99999,
                 999999,
                 max(gpnc_embl, na.rm = F)),
      labels = c(
        "0-1%",
        "1-2%",
        "2-3%",
        "3-4%",
        "4-5%",
        ">5%",
        "Missing Case Data",
        "Missing Case and Sequence Data",
        "Missing Sequence Data"
      )
    )) |>
    # change level order
    mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor))))

  ggplot(plot_df , aes(x = wy, y = country, fill = countfactor)) +
    #add border white colour of line thickness 0.25
    geom_bin_2d(colour = "white", size = 0.4) +
    guides(fill = guide_legend(title = "Percent of sequenced cases \nper epidemiological week")) +
    labs(
      x = "Epidemiological Week",
      y = "Country",
      title = paste0(
        "SARS-CoV2 sequences shared with The Covid-19 Data Portal in ",
        continent_loop$`unique(main_df$continent)`[i]
      ),
      caption = "Percent of reported cases (Our World In Data) that were sequenced and shared to the Covid-19 Data portal database per\n epidemiological week between December 19th 2019 and October 19th 2022. GISAID Metadata was accessed and used \nfollowing their terms of use. Epidemiological data was linked via joining Our World in Data on Covid-19 accessed on 24th October 2022."
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_grey(base_size = 10) +
    scale_fill_manual(
      values = c(
        "gray12",
        "gray47",
        "gray96",
        "#845ec2",
        "#d65db1" ,
        "#ff6f91",
        "#ff9671",
        "#ffc75f",
        "#f9f871",
        "#ddf1da"
      ),
      na.value = "grey90"
    ) + theme_landscape()

  ggsave(
    paste0(
      "plots/EMBL/",
      continent_loop$`unique(main_df$continent)`[i],
      " Sequence Landscape.png"
    ),
    dpi = 320,
    width = 18,
    height = 12,
    limitsize = FALSE
  )
}
