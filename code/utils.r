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

# pacman - library management ---------------------------------------------
#' Takes a list of packages and installs and loads them in parrael
#'
#' @param pkg A list of packages
#' @examples
#' packman(pkg)
#'
pacman = function(pkg) {
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# fetch data - quick api tool ---------------------------------------------
#' Downloads data to a defined path
#'
#' @param url A url to download data from
#' @param path A path on the local machine to save the file
#' @examples
#' fetch_data(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",path = "data/covid-jh.csv")
#'
fetch_data = function(url, path) {
  url = url
  path = path
  download.file(url, path)
  read.csv(path)
}

# isodate- week to year function ------------------------------------------
#' Formats a date from week and year to iso format
#'
#' @examples
#' isodate("22/11")
#'
isodate = function (x = Sys.Date()) {
  xday = ISOdate(year(x), month(x), day(x), tz = tz(x))
  dn = 1 + (wday(x) + 5) %% 7
  nth = xday + ddays(4 - dn)
  jan1 = ISOdate(year(nth), 1, 1, tz = tz(x))
  return(sprintf("%s/%02d", format(nth, "%y"), 1 + (nth - jan1) %/% ddays(7)))
}

# tree plot - ggplot2 theme -----------------------------------------------
#' Tree plot ggplot2 theme
#'
#' @examples
#' + theme_tree()
#'
theme_tree = function() {
  theme(
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 14
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 6,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      vjust = 0.2,
      colour = textcol,
      size = 14
    ),
    axis.ticks = element_line(size = 0.4),
    plot.caption = element_text(colour = textcol, size = 10),
    axis.title = element_text(
      size = 14,
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
    panel.grid.major = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 22,
      face = "bold",
      vjust = 0.9
    )
  )
}

# K-elbow plot - ggplot2 theme -------------------------------------------
#' K-elbow plot ggplot2 theme
#'
#' @examples
#' + theme_temporal()
#'
theme_k = function() {
  theme(
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 14
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
      face = "bold"
    ),
    legend.key.height = grid::unit(0.8, "cm"),
    legend.key.width = grid::unit(0.2, "cm"),
    axis.text.x = element_text(
      size = 14,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = textcol
    ),
    axis.text.y = element_text(
      vjust = 0.2,
      colour = textcol,
      size = 14
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
    plot.background = element_rect(fill = "gray12"),
    legend.background = element_rect(fill = "gray12"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
  )
}

# temporal plot - ggplot2 theme -------------------------------------------
#' Temporal plot ggplot2 theme
#'
#' @examples
#' + theme_temporal()
#'
theme_temporal = function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(
      colour = textcol,
      face = "italic",
      size = 14
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
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
      colour = textcol,
      size = 14
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
    panel.grid.major = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "gray12"),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
  )
}

# landscape plot - ggplot2 theme ------------------------------------------
#' Landscape plot ggplot2 theme
#'
#' @examples
#' + theme_landscape()
#'
theme_landscape = function() {
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
      hjust = 0,
      colour = textcol,
      size = 8
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

# network_stat_df - generate network stats --------------------------------
#' Create a dataframe of network statistics of a given graph
#'
#' @param network A igraph network object
#' @examples
#' network_stat_df(author_colab)
#'
network_stat_df = function(network) {
  v = data.frame(
    "size" = network$network$networkSize,
    "density" = network$network$networkDensity,
    "transitivity" = network$network$networkTransitivity,
    "diameter" = network$network$networkDiameter,
    "distance" = network$network$networkCentrDegree,
    "avgpath" = network$network$NetworkAverPathLeng
  )
  return(v)
}

# split_author_matrix = Create colab matrix -------------------------------
#' Create a dataframe of network statistics of a given graph
#'
#' @param network A igraph network object
#' @examples
#' network_stat_df(author_colab)
#'
split_author_matrix = function(col_name) {
  # create list of individual authors for each paper
  V = M[, c(col_name)]
  pub_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))
  pub_auths = lapply(pub_auths, trimws)
  # for each paper, form a data frame of unique author pairs
  auth_pairs = lapply(pub_auths, function(x) {
    z  = expand.grid(x, x, stringsAsFactors = FALSE)
    z[z$Var1 < z$Var2,]
  })
  # combine list of matrices for each paper into one data frame
  auth_pairs = do.call(rbind, auth_pairs)
  # count papers for each author pair
  auth_count = aggregate(paste(Var1, Var2)  ~ Var1 + Var2 , data = auth_pairs, length)
  colnames(auth_count) = c("datum1", "datum2", "weight")

  return(auth_count)
}

# build_networks - create data files for network plots --------------------
#' Save graphs for author,geography and institution colab and author,keyword,abstract,title co-occurrence
#'
#' @param data A biblographic dataframe
#' @param path A path save files
#' @examples
#' build_networks(M, "data/networks/gisaid/")
#'
build_networks = function(data, path) {
  message("Building collaboration networks")
  ##################################################################
  ##                    Collaboration Networks                    ##
  ##################################################################

  # generate network
  author_colab = biblioNetwork(M,
                               analysis = "collaboration",
                               network = "authors",
                               sep = ";")
  message("Author network complete")
  institution_colab = biblioNetwork(M,
                                    analysis = "collaboration",
                                    network = "universities",
                                    sep = ";")
  message("University network complete")
  geog_colab = biblioNetwork(M,
                             analysis = "collaboration",
                             network = "countries",
                             sep = ";")
  message("Geography network complete")
  institution_co_oc = split_author_matrix("AU_UN") |> igraph::graph_from_data_frame()
  message("Institution network complete")
  funding_co_oc = split_author_matrix("FU") |> igraph::graph_from_data_frame()
  message("Funding network complete")
  funding_group_co_oc = split_author_matrix("Funder.Group") |> igraph::graph_from_data_frame()
  message("Funding group network complete")
  funding_country_co_oc = split_author_matrix("Funder.Country") |> igraph::graph_from_data_frame()
  message("Funding country network complete")

  saveRDS(author_colab, paste0(path, "author_colab.rds"))
  saveRDS(institution_colab, paste0(path, "institution_colab.rds"))
  saveRDS(geog_colab, paste0(path, "geog_colab.rds"))
  saveRDS(institution_co_oc, paste0(path, "institution_co_ocs.rds"))
  saveRDS(funding_co_oc, paste0(path, "funding_co_ocs.rds"))
  saveRDS(funding_group_co_oc,
          paste0(path, "funding_group_co_ocs.rds"))
  saveRDS(funding_country_co_oc,
          paste0(path, "funding_country_keywords_co_ocs.rds"))

  # calculate network statistics
  message("Calculating colab network stats")
  author_colab_stats = networkStat(author_colab) |> network_stat_df()
  institution_colab_stats = networkStat(institution_colab) |> network_stat_df()
  geog_colab_stats = networkStat(geog_colab) |> network_stat_df()
  inititution_co_ocs_stats = networkStat(institution_co_oc) |> network_stat_df()
  funding_co_ocs_stats = networkStat(funding_co_oc) |> network_stat_df()
  funding_group_co_ocs_stats = networkStat(funding_group_co_oc) |> network_stat_df()
  funding_country_keywords_co_ocs_stats = networkStat(funding_country_co_oc) |> network_stat_df()

  category = c(
    "author",
    "institution",
    "geography",
    "insitution",
    "funding",
    "funding-group",
    "funding country"
  )

  colab_stats = rbind(
    author_colab_stats,
    institution_colab_stats,
    geog_colab_stats,
    inititution_co_ocs_stats,
    funding_co_ocs_stats,
    funding_group_co_ocs_stats,
    funding_country_keywords_co_ocs_stats
  ) |>
    dplyr::mutate(category = category)

  saveRDS(colab_stats,
          paste0(path, "colab_network_stats.rds"))
  # remove redundant vars
  rm(author_colab_stats,
     institution_colab_stats,
     geog_colab_stats)

  #################################################################
  ##                   Co-occurrences Networks                   ##
  #################################################################
  message("Building co-occurrences networks")
  author_co_ocs = biblioNetwork(M,
                                analysis = "co-occurrences",
                                network = "authors",
                                sep = ";")
  message("Author network complete")
  journals_co_ocs = biblioNetwork(M,
                                  analysis = "co-occurrences",
                                  network = "sources",
                                  sep = ";")
  message("Journal network complete")
  keywords_co_ocs = biblioNetwork(M,
                                  analysis = "co-occurrences",
                                  network = "keywords",
                                  sep = ";")
  message("Keyword network complete")
  author_keywords_co_ocs = biblioNetwork(M,
                                         analysis = "co-occurrences",
                                         network = "author_keywords",
                                         sep = ";")
  message("Author keyword network complete")
  saveRDS(author_co_ocs, paste0(path, "author_co_ocs.rds"))
  saveRDS(journals_co_ocs, paste0(path, "journals_co_ocs.rds"))
  saveRDS(keywords_co_ocs, paste0(path, "keywords_co_ocs.rds"))
  saveRDS(author_keywords_co_ocs,
          paste0(path, "author_keywords_co_ocs.rds"))

  # calculate network statistics
  message("Calculating colab network stats")
  author_co_ocs_stats = networkStat(author_co_ocs) |> network_stat_df()
  journals_co_ocs_stats = networkStat(journals_co_ocs) |> network_stat_df()
  keywords_co_ocs_stats = networkStat(keywords_co_ocs) |> network_stat_df()
  author_keywords_co_ocs_stats = networkStat(author_keywords_co_ocs) |> network_stat_df()

  category = c("author", "journal", "keywords", "autho-keywords")

  co_oc_stats = rbind(
    author_co_ocs_stats,
    journals_co_ocs_stats,
    keywords_co_ocs_stats,
    author_keywords_co_ocs_stats
  ) |>
    dplyr::mutate(category = category)

  saveRDS(co_oc_stats, paste0(path, "co_oc_network_stats.rds"))
  # remove redundant vars
  rm(
    author_co_ocs_stats,
    journals_co_ocs_stats,
    keywords_co_ocs_stats,
    author_keywords_co_ocs_stats
  )
}

# Plot biblometrix using VOSViewer ----------------------------------------
#' Plot igraph collaboration network using VOSViewer
#'
#' @param network A igraph network object
#' @param vos A path to VOS on local machine
#' @examples
#' plot_colab_network(author_colab, "../VosViewer)
#'
plot_colab_network = function(network, vos) {
  # plot in igraph
  net_author = networkPlot(
    network,
    n = 100,
    type = "auto",
    size = 10,
    size.cex = T,
    halo = TRUE,
    edgesize = 3,
    labelsize = 1
  )
  # plot in vosviewer
  ## Repulsion 0, attraction 10 OR -1, 1, method: strength link, font: sans serif
  if (vos == TRUE) {
    net2VOSviewer(net_author, vos.path = "VOSviewer/")
  } else {
    print("Network plotted!")
  }
}

# Plot biblometrix using VOSViewer ----------------------------------------
#' Plot igraph collaboration network using VOSViewer
#'
#' @param net A igraph network object
#' @param vos.path A path to VOS on local machine
#' @examples
#' plot_colab_network(author_colab, "../VosViewer)
#'
#'@details
#'Edited from the biblometrix package with the first line removed in order to allow igraph functionlity

viewerigraph = function(net, vos.path = NULL) {
  V(net)$id = V(net)$name

  if (is.null(vos.path)) {
    vos.path = getwd()
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
    netfile = paste(vos.path, "/", "vosnetwork.net", sep = "")
    VOScommand = paste("java -jar ",
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


# Round DF----------------------------------------
#' Round all numeric values in dataframe by specified amount
#'
#' @param df a dataframe
#' @param digits the number to round by
#' @examples
#' round_df(df)
#'
round_df = function(df, digits) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] = round(df[,nums], digits = digits)

  (df)
}

# Plot MCP and SCP----------------------------------------
#' Rlot country distribution of multi cited papers vs single cited papers
#'
#' @param data a dataframe
#' @param db the name of the database to save files for
#' @examples
#' plot_mcp_scp(S,"GISAID)
#'
plot_mcp_scp = function(data,db){
  Country = S$MostProdCountries$Country
  SCP = S$MostProdCountries$SCP
  MCP = S$MostProdCountries$MCP
  Articles = S$MostProdCountries$Articles
  Ratio = S$MostProdCountries$MCP_Ratio

  data = as.data.frame(cbind(Country, SCP, MCP, Articles,Ratio)) |>
    arrange(desc(Articles)) |>
    pivot_longer(c(SCP, MCP)) |>
    mutate(value = as.numeric(value)) |>
    mutate(Articles = as.numeric(Articles)) |>
    mutate(collaboration = name)

  ggplot(data[1:40, ] |>   {
    \(.) {
      replace(., is.na(.), 0)
    }
  }(), aes(
    fill = collaboration,
    y = value,
    x = reorder(Country, Articles),

  )) +
    geom_bar(position = "stack", stat = "unique") +
    labs(title = "Leading 20 countries mentioning GISAID in scientific publications")
  xlab("Country") +
    ylab("No. Documents") +
    coord_flip() + theme_landscape() + theme(axis.text.x = element_text(size  = 8))

  ggsave(
    paste0(
      "plots/",
      db,"/mcp-scp.png"
    ),
    dpi = 320,
    width = 18,
    height = 12,
    limitsize = FALSE
  )
}


# Calculate network statistics from igraph----------------------------------------
#' Plot igraph collaboration network using VOSViewer
#'
#' @param g An igraph network object
#' @examples
#' all_indices(g)
#'
all_indices =  function(g) {
  res = matrix(0, igraph::vcount(g), 35)
  res[, 1] = V(g)$name
  res[, 2] = as.numeric(igraph::degree(g))
  res[, 3] = as.numeric(igraph::betweenness(g))
  res[, 4] = as.numeric(igraph::closeness(g))
  res[, 5] = as.numeric(igraph::eigen_centrality(g)$vector)
  res[, 6] = as.numeric(1 / igraph::eccentricity(g))
  res[, 7] = as.numeric(igraph::subgraph_centrality(g))

  A = get.adjacency(g, sparse = F)
  res[, 8] = as.numeric(sna::flowbet(A))
  res[, 9] = as.numeric(sna::loadcent(A))
  res[, 10] = as.numeric(sna::gilschmidt(A))
  res[, 11] = as.numeric(sna::infocent(A))
  res[, 12] = as.numeric(sna::stresscent(A))
  res[, 13] = as.numeric(1 / centiserve::averagedis(g))
  res[, 14] = as.numeric(centiserve::barycenter(g))
  res[, 15] = as.numeric(centiserve::closeness.currentflow(g))
  res[, 16] = as.numeric(centiserve::closeness.latora(g))
  res[, 17] = as.numeric(centiserve::closeness.residual(g))
  res[, 18] = as.numeric(centiserve::communibet(g))
  res[, 19] = as.numeric(centiserve::crossclique(g))
  res[, 20] = as.numeric(centiserve::decay(g))
  res[, 21] = as.numeric(centiserve::diffusion.degree(g))
  res[, 22] = as.numeric(1 / centiserve::entropy(g))
  res[, 23] = as.numeric(centiserve::geokpath(g))
  res[, 24] = as.numeric(centiserve::laplacian(g))
  res[, 25] = as.numeric(centiserve::leverage(g))
  res[, 26] = as.numeric(centiserve::lincent(g))
  res[, 27] = as.numeric(centiserve::lobby(g))
  res[, 28] = as.numeric(centiserve::markovcent(g))
  res[, 29] = as.numeric(centiserve::mnc(g))
  res[, 20] = as.numeric(centiserve::radiality(g))
  res[, 31] = as.numeric(centiserve::semilocal(g))
  res[, 32] = as.numeric(1 / centiserve::topocoefficient(g))
  res[, 33] = as.numeric(CINNA::dangalchev_closeness_centrality(g))
  res[, 34] = as.numeric(CINNA::harmonic_centrality(g))
  res[, 35] =as.numeric( 1 / CINNA::local_bridging_centrality(g))
  res = as.data.frame(res)
  res[, c(2:35)] = sapply(res[, c(2:35)], as.numeric)
  res = round_df(res,3)
  colnames(res) = c("name","degree", "betweenness",
                    "closeness", "eigen_centrality", "eccentricity",
                    "subgraph_centrality", "flowbet", "loadcent",
                    "gilschmidt", "infocent", "stresscent",
                    "averagedis", "barycenter", "closeness.currentflow",
                    "closeness.latora", "closeness.residual", "communibet",
                    "crossclique", "decay", "diffusion.degree",
                    "entropy", "geokpath", "laplacian",
                    "leverage", "lincent", "lobby",
                    "markovcent", "mnc", "radiality",
                    "semilocal", "topocoefficient", "dangalchev_closeness_centrality",
                    "harmonic_centrality", "local_bridging_centrality")
  return(res)

}


# Calculate indices for individual networks----------------------------------------
#' Save an RDS file containing network stats
#'
#' @param network_path The path to the network
#' @examples
#' calc_network_stats("data/networks/gisaid/")
calc_network_stats = function(network_path) {
  files = as.data.frame(grep(
    list.files(path = network_path),
    pattern = 'stats',
    invert = TRUE,
    value = TRUE
  ))
  for (i in 1:nrow(files)) {
    tryCatch(
      expr = {
        message(paste0("Reading network: "), files[i])
        file = readRDS(paste0(network_path, files[i]))
        file = igraph::as.undirected(file)

      },
      error = function(e) {
        message("Looks like this network needs to be made into an igraph object")
        file = igraph::graph_from_adjacency_matrix(file)
        file = igraph::as.undirected(file)

      },
      finally = {
        message(paste0("Calculating network stats for", files[i]))
        all_stats = all_indices(file)
        saveRDS(all_stats,
                paste0(network_path, "/network_stats/", files[i]))
      }
    )
  }
}

