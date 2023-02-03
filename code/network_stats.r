# Merge summary statistics for networks
gisaid_networks = rbind(
  readRDS("data/networks/gisaid/co_oc_network_stats.rds"),
  readRDS("data/networks/gisaid/colab_network_stats.rds")
)
gisaid_networks[, c(1:6)] = sapply(gisaid_networks[, c(1:6)], as.numeric)
gisaid_networks = round_df(gisaid_networks, 3)
saveRDS(gisaid_networks,
        "data/networks/gisaid/gisaid_network_stats.rds")

ena_networks = rbind(
  readRDS("data/networks/ena/co_oc_network_stats.rds"),
  readRDS("data/networks/ena/colab_network_stats.rds")
)
gisaid_networks[, c(1:6)] = sapply(gisaid_networks[, c(1:6)], as.numeric)
gisaid_networks = round_df(gisaid_networks, 3)
saveRDS(gisaid_networks,
        "data/networks/gisaid/gisaid_network_stats.rds")



files = grep(
  list.files(path = "data/networks/gisaid"),
  pattern = 'stats',
  invert = TRUE,
  value = TRUE
)
for (i in files) {
  tryCatch(
    expr = {
      message(paste0("Reading network: "), files[5])
      file = readRDS(paste0("data/networks/gisaid/", files[5]))
      file = igraph::as.undirected(file)

    },
    error = function(e) {
      message("Looks like this network needs to be made into an igraph object")
      file = igraph::graph_from_adjacency_matrix(file)
      file = igraph::as.undirected(file)

    },
    finally = {
      all_stats = all_indices(file)
      saveRDS(all_stats,paste0("data/networks/gisaid/network_stats/",file))
    }
  )
}

