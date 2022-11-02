## ---------------------------
##
## Script name: set-up.r
##
## Purpose of script: Install packages needed for project and set up helper functions.
##
## Author: Nathaneal Sheehan
##
## Date Created: 2021-03-20
##
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
##################################################################
##                       Helper Functions                       ## thanx for helpin
##################################################################
# (1) install and require packages
pacman = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# (2_) fetch data
fetch_data = function(url,path) {
  url = url
  path = path
  download.file(url,path)
  read.csv(path)
}
# format date function into week and year
isodate = function (x = Sys.Date()) {
  xday = ISOdate(year(x), month(x), day(x), tz = tz(x))
  dn = 1 + (wday(x) + 5)%%7
  nth = xday + ddays(4 - dn)
  jan1 = ISOdate(year(nth), 1, 1, tz = tz(x))
  return(sprintf("%s/%02d", format(nth, "%y"), 1 + (nth - jan1)%/%ddays(7)))
}

# plot dark theme
dark_theme = function() {
  theme(
    # add border 1)
    panel.border = element_rect(
      colour = "slategrey",
      fill = NA,
      linetype = 2
    ),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    panel.grid.major.x = element_line(
      colour = "#cf2e2e",
      linetype = 3,
      size = 0.5
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(
      colour = "white",
      face = "italic",
      family = "ub"
    ),
    axis.title = element_text(colour = "white", family = "ub"),
    axis.ticks = element_line(colour = "white"),
    plot.background = element_rect(fill = "#cf2e2e"),
    plot.title = element_text(family = "ub", hjust = .5, size = 16),
    plot.subtitle = element_text(family = "ub", hjust = .5, size = 12),
    legend.background = element_rect(fill = "#cf2e2e"),
    legend.text  = element_text(color = "white", family = "ub", size = 10),
    legend.key = element_rect(fill = "#cf2e2e"),
    # legend at the bottom 6)
    legend.position = "bottom"
  )
}
options(scipen=999) # Turn off scientific notation

# font_add_google("Ubuntu", "ub") # font for plots

#################################################################
##                           Library                           ##
#################################################################
pkgs = c("tidyverse",# data cleaning
         "bibliometrix",
         "tidyr",
         "stringr",
         "tidytext",
         "quanteda",
         "treemapify",
         "stm",
         "lubridate"

         ) # stick plots together

pacman(pkgs)
rm(pkgs)


