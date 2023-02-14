## ---------------------------
##
## Script name: network_stats.r
##
## Purpose of script: run network statistics for each network
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-02-07
##
## Copyleft (c) Nathanael Sheehan, 2023
## Email: nathanaelsheehan@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
# Merge summary statistics for networks -----------------------------------

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
ena_networks[, c(1:6)] = sapply(ena_networks[, c(1:6)], as.numeric)
ena_networks = round_df(ena_networks, 3)
saveRDS(gisaid_networks,
        "data/networks/ena/ena_network_stats.rds")

# Run stats ---------------------------------------------------------------
calc_network_stats("data/networks/gisaid")
calc_network_stats("data/networks/ena/")
