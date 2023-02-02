## ---------------------------
##
## Script name: GS-LIT-NETWORK.r
##
## Purpose of script: create biblographic network on genomic survelience literature
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-10-25
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes: Data was collected on October the 25th using the WEB OF SCIENCE data downloader interface. The interface was queryed to return publications
##   between December 1st 2019 and October 1st 2022 with the search queries of "Genomic survlience" OR "Genome Survelience". The texts where then filtered to o
##   only include texts focusing on the SARS genome, rather than any other GS strategy which has grown in recent years.
## ---------------------------
source("build.r")
set.seed(999)
textcol = "yellow"
# Read data
file = "../../Downloads/Dimensions-Publication-2022-11-07_16-52-16.csv"
# Convert bibtext to dataframe
M = bibliometrix::convert2df(file = file,
                             dbsource = "dimensions",
                             format = "csv") |>
  dplyr::filter(PY != is.na(PY)) |>
  dplyr::filter(AB != is.na(AB)) |>
  dplyr::mutate(Year = substr(PY, 1, 4)) |>
  dplyr::filter(Year > 2019) |>
  dplyr::filter(Year < 2023) |>
  dplyr::select(-c(Year))

# Conduct a biblio analysis of dataframe using the bibliometrix package
results = bibliometrix::biblioAnalysis(M, sep = ";")
options(width = 100)
S = summary(object = results, k = 50, pause = FALSE)

Country = S$MostProdCountries$Country
SCP = S$MostProdCountries$SCP
MCP = S$MostProdCountries$MCP
Articles = S$MostProdCountries$Articles

data = as.data.frame(cbind(Country, SCP, MCP, Articles)) |>
  arrange(desc(Articles)) |>
  pivot_longer(c(SCP, MCP)) |>
  mutate(value = as.numeric(value)) |>
  mutate(Articles = as.numeric(Articles)) |>
  rename(collaboration = name)

ggplot(data[1:100, ] |>   {
  \(.) {
    replace(., is.na(.), 0)
  }
}(), aes(
  fill = collaboration,
  y = value,
  x = reorder(Country, Articles)
)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Leading 50 countries mentioning The Covid-19 Data Portal \nin scientific publications", caption  = "Publications containing the search query ‘The Covid-19 Data Portal’ OR 'European Nucleotide Archive' \nwere accessed via the Dimensions Analytics API and filtered to include publications between January 1st 2019 \nand October 1st 2021 which contain the phrase 'covid-19' OR 'sars-cov-2' in the full text.\nSCP: Single Country Publication. MCP: Multi Country Publication") +
  xlab("Country") +
  ylab("No. Documents") +
  coord_flip() + theme_landscape()

ggsave(
  paste0(
    "plots/EMBL/mcp-scp.png"
  ),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)

##################################################################
##                    Collaboration Networks                    ##
##################################################################

# generate network
author_colab = biblioNetwork(M,
                             analysis = "collaboration",
                             network = "authors",
                             sep = ";")

institution_colab = biblioNetwork(M,
                                  analysis = "collaboration",
                                  network = "universities",
                                  sep = ";")

geog_colab = biblioNetwork(M,
                                  analysis = "collaboration",
                                  network = "countries",
                                  sep = ";")

saveRDS(author_colab,"data/networks/ena/author_colab.rds")
saveRDS(institution_colab,"data/networks/ena/institution_colab.rds")
saveRDS(geog_colab,"data/networks/ena/geog_colab.rds")

# calculate network statistics
author_colab_stats = networkStat(author_colab) |> network_stat_df()
institution_colab_stats = networkStat(institution_colab) |> network_stat_df()
geog_colab_stats = networkStat(geog_colab) |> network_stat_df()

category = c("author", "institution", "geography")

colab_stats = rbind(author_colab_stats,institution_colab_stats,geog_colab_stats) |>
  dplyr::mutate(category = category)

saveRDS(colab_stats,"data/networks/ena/network_stats.rds")
# remove redundant vars
rm(author_colab_stats,institution_colab_stats,geog_colab_stats)

#################################################################
##                   Co-occurrences Networks                   ##
#################################################################

author_co_ocs = biblioNetwork(M,
                              analysis = "co-occurrences",
                              network = "authors",
                              sep = ";")

journals_co_ocs = biblioNetwork(M,
                            analysis = "co-occurrences",
                            network = "sources",
                            sep = ";")

keywords_co_ocs = biblioNetwork(M,
                               analysis = "co-occurrences",
                               network = "keywords",
                               sep = ";")

author_keywords_co_ocs = biblioNetwork(M,
                              analysis = "co-occurrences",
                              network = "author_keywords",
                              sep = ";")


institution_co_oc = split_author_matrix("AU_UN") |> igraph::graph_from_data_frame()
funding_co_oc = split_author_matrix("FU") |> igraph::graph_from_data_frame()
funding_group_co_oc = split_author_matrix("Funder.Group") |> igraph::graph_from_data_frame()
funding_country_co_oc = split_author_matrix("Funder.Country") |> igraph::graph_from_data_frame()

saveRDS(author_co_ocs,"data/networks/ena/author_co_ocs.rds")
saveRDS(journals_co_ocs,"data/networks/ena/journals_co_ocs.rds")
saveRDS(keywords_co_ocs,"data/networks/ena/keywords_co_ocs.rds")
saveRDS(author_keywords_co_ocs,"data/networks/ena/author_keywords_co_ocs.rds")
saveRDS(institution_co_oc,"data/networks/ena/institutionr_co_ocs.rds")
saveRDS(funding_co_oc,"data/networks/ena/funding_co_ocs.rds")
saveRDS(funding_group_co_oc,"data/networks/ena/funding_group_co_ocs.rds")
saveRDS(funding_country_co_oc,"data/networks/ena/funding_country_keywords_co_ocs.rds")

# calculate network statistics
author_co_ocs_stats = networkStat(author_co_ocs) |> network_stat_df()
journals_co_ocs_stats = networkStat(journals_co_ocs) |> network_stat_df()
keywords_co_ocs_stats = networkStat(keywords_co_ocs) |> network_stat_df()
author_keywords_co_ocs_stats = networkStat(author_keywords_co_ocs) |> network_stat_df()

inititution_co_ocs_stats = networkStat(institution_co_oc) |> network_stat_df()
funding_co_ocs_stats = networkStat(funding_co_oc) |> network_stat_df()
funding_group_co_ocs_stats = networkStat(funding_group_co_oc) |> network_stat_df()
funding_country_keywords_co_ocs_stats = networkStat(funding_country_co_oc) |> network_stat_df()

category = c("author", "journal", "keywords","autho-keywords","insitution","funding","funding-group","funding country")

co_oc_stats = rbind(author_co_ocs_stats,journals_co_ocs_stats,keywords_co_ocs_stats,
                    author_keywords_co_ocs_stats,inititution_co_ocs_stats,
                    funding_co_ocs_stats,funding_group_co_ocs_stats,
                    funding_country_keywords_co_ocs_stats) |>
  dplyr::mutate(category = category)
saveRDS(co_oc_stats,"data/networks/ena/co_oc_network_stats.rds")
# remove redundant vars
rm(author_co_ocs_stats,journals_co_ocs_stats,keywords_co_ocs_stats,
   author_keywords_co_ocs_stats)

#### PLOT AND VISUALSE NETWORKS WITH VOSVIEWER AND IGRAPH

plot_colab_network(author_co_ocs, vos = TRUE)

##################################################################
##                        Topic Modelling                      ##
##################################################################

M = M |> dplyr::filter(AB != "")
kens = M$AB |>
  stringr::str_to_lower() |>
  stringr::str_remove_all("ena") |>
  stringr::str_remove_all("european") |>
  stringr::str_remove_all("nucleotide") |>
  stringr::str_remove_all("archive") |>
  stringr::str_remove_all("sars-cov-2") |>
  quanteda::tokens(
    what = "word",
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE
  ) |>
  quanteda::tokens_tolower() |>
  quanteda::tokens_remove(stopwords("english"))

#applying relative pruning
dfm = quanteda::dfm_trim(dfm(kens),
                         docfreq_type = "prop", verbose = TRUE)

# convert to stm format
dfm_stm = quanteda::convert(dfm, to = "stm")

# Statistically find the best number of k ---------------------------------
K = c(4, 6, 8, 10, 12, 14, 16, 18, 20)
fit = stm::searchK(dfm_stm$documents,
                   dfm_stm$vocab,
                   K = K,
                   verbose = TRUE)

plot = data.frame(
  "K" = K,
  "Residuals" = unlist(fit$results$residual),
  "Exclusivity" = unlist(fit$results$exclus),
  "Held-Out-Likelihood" = unlist(fit$results$heldout),
  "Semantic Coherence" = unlist(fit$results$semcoh),
  "Bound" = unlist(fit$results$bound),
  "Lower Bound" = unlist(fit$results$lbound)
)
plot = reshape2::melt(plot, id = c("K"))

jpeg("plots/EMBL/topic-fit-ENA.jpeg",
     width = 800,
     height = 800)
ggplot(plot, aes(K, value, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Number of topics K",
       title = "Statistical fit of models with different K \nThe Covid-19 Data Portal Corpus") +  theme(legend.position =
                                                                        "none",
                                                                      plot.title = element_text(size = 11)) +
  theme_k()
dev.off()

# Set optimal number of K based on fit
k = 6
M$d = as.numeric(M$PY)
model = stm::stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  prevalence =  ~ d,
  K = k,
  data = M,
  verbose = TRUE
)
# plot topics
jpeg("plots/EMBL/topics-ENA.jpeg",
     width = 800,
     height = 800)
par(bg = "black", col = "white",col.axis = 'yellow', col.lab = 'red')
plot(model, main = "Top Topics ENA")
dev.off()

# Plot effect of topic per year
effect = stm::estimateEffect(formula =  ~ d,
                             stmobj = model,
                             metadata = M)
labels = stm::labelTopics(model, 1:k)
jpeg("plots/EMBL/effect-ENA.jpeg",
     width = 800,
     height = 800)
par(mfcol = c(2, 3),col.main = "white")
for (i in 1:k) {
  plot(
    effect,
    "d",
    method = "continuous",
    topics = i,
    model,
    main = paste0(labels$prob[i, 1:3], collapse = ", ")
  )
}
dev.off()
