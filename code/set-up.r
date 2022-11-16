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
theme_landscape = function(){
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 18
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 18,
      face = "bold"
    ),
    legend.key.height = grid::unit(1.4, "cm"),
    legend.key.width = grid::unit(0.8, "cm"),
    axis.text.x = element_text(
      size = 6,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      vjust = 0.2,
      hjust = 0,
      colour = textcol,
      size = 10
    ),
    axis.ticks = element_line(size = 0.4),
    plot.caption = element_text(colour = textcol, size = 10),
    axis.title = element_text(
      size = 12,
      face = "bold",
      colour = textcol,
      hjust = 0.1
    ),
    panel.border = element_blank(),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 0.5,
      linetype = "solid"
    ),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
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


