## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-10-26
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

library(ddplo)
continent = as.data.frame(unique(main_df$continent)) |> dplyr::rename(continent = 1)

for (i in 1:nrow(continent)) {
  d = main_df |>
    filter(continent == continent[i]) |>
    barChartRace(
      x = "GISAID.total.submissions",
      y = "country",
      time = "wy",
      ytitle = "Country",
      xtitle = "Count (n submissions)",
      title = "Global Sequence Submissions to GISAID EpiCov",
      paddingWidth = 0.1,
      xFontSize = 10,
      yFontSize = 10,
      xticks = 12,
      xtitleFontSize = 14,
      ytitleFontSize = 14,
      titleFontSize = 22,
      stroke = "black",
      strokeWidth = NULL,
      font = "gochi",
      bgcol = "#cf2e2e",
      panelcol = "#fcb900",
      xgridlinecol = "#8ed1fc",
      opacity = 1,
      timeLabel = TRUE,
      timeLabelOpts = list(
        size = 28,
        prefix = "",
        suffix = "",
        xOffset = 0.5,
        yOffset = 1
      ),
      width = NULL,
      height = NULL
    )
  htmlwidgets::saveWidget(as_widget(d),
                          paste0(
                            "plots/GISAID/bar-chart-",
                            tolower(continent$continent[i]),
                            ".html"
                          ))
}


for (i in 1:nrow(continent)) {
  d = main_df |>
    filter(continent == continent[i]) |>
    barChartRace(
      x = "CD19DP.total.submissions",
      y = "country",
      time = "wy",
      ytitle = "Country",
      xtitle = "Count (n submissions)",
      title = "Global Seqeuence Submissions to The Covid-19 Data Portal",
      paddingWidth = 0.1,
      xFontSize = 10,
      yFontSize = 10,
      xticks = 12,
      xtitleFontSize = 14,
      ytitleFontSize = 14,
      titleFontSize = 22,
      stroke = "black",
      strokeWidth = NULL,
      font = "gochi",
      bgcol = "#cf2e2e",
      panelcol = "#fcb900",
      xgridlinecol = "#8ed1fc",
      opacity = 1,
      timeLabel = TRUE,
      timeLabelOpts = list(
        size = 28,
        prefix = "",
        suffix = "",
        xOffset = 0.5,
        yOffset = 1
      ),
      width = NULL,
      height = NULL
    )
  htmlwidgets::saveWidget(as_widget(d), paste0("plots/EMBL/bar-chart-", tolower(continent$continent[i]), ".html"))
}
