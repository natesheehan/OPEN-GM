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

network_stat_df = function(network){
  v = data.frame(
    "size"=network$network$networkSize,
    "density"=network$network$networkDensity,
    "transitivity"=network$network$networkTransitivity,
    "diameter"=network$network$networkDiameter,
    "distance"=network$network$networkCentrDegree,
    "avgpath"=network$network$NetworkAverPathLeng
  )
  return(v)
}

split_author_matrix = function(col){
  # create list of individual authors for each paper
  pub_auths = sapply(M$Funder.Country, function(x)
    strsplit(as.character(x), split = ";"))
  pub_auths = lapply(pub_auths, trimws)
  # for each paper, form a data frame of unique author pairs
  auth_pairs = lapply(pub_auths, function(x) {
    z  = expand.grid(x, x, stringsAsFactors = FALSE)
    z[z$Var1 < z$Var2, ]
  })
  # combine list of matrices for each paper into one data frame
  auth_pairs = do.call(rbind, auth_pairs)
  # count papers for each author pair
  auth_count = aggregate(paste(Var1, Var2)  ~ Var1 + Var2 , data = auth_pairs, length)
  colnames(auth_count) = c("datum1", "datum2", "count")

  return(auth_count)
}

# Edited from the biblometrix package with the first line removed in order to allow igraph functionlity
net2VOSviewerigraph <- function(net, vos.path = NULL){

  V(net)$id <- V(net)$name

  if (is.null(vos.path)) {
    vos.path <- getwd()
  }
  if (sum(dir(vos.path) %in% "VOSviewer.jar") == 0) {
    cat(
      paste(
        "VOSviewer.jar does not exist in the path",
        vos.path,
        "\n\nPlese download it from https://www.vosviewer.com/download",
        "\n(Java version for other systems)\n"
      )
    )
  }
  else{
    netfile <- paste(vos.path, "/", "vosnetwork.net", sep = "")
    VOScommand <- paste("java -jar ",
                        vos.path,
                        "/",
                        "VOSviewer.jar -pajek_network ",
                        netfile,
                        sep = "")
    write.graph(graph = net,
                file = netfile,
                format = "pajek")
    system(VOScommand, wait = FALSE)
  }

}

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


