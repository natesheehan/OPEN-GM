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


main_df %>%
  filter(continent == "Africa") %>%
  barChartRace(
    x = "GISAID.total.submissions",
    y = "country",
    time = "wy",
    ytitle = "Country",
    xtitle = "Count (n submissions)",
    title = "Global GISAID EpiCov Database Submissions",
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
