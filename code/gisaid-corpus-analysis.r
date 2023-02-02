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
##   only include texts focusing on the SARS genome, rather than any other GS strategy which has grown in recent years.t
## ---------------------------

# Data Prep ---------------------------------------------------------------
set.seed(999)
textcol = "yellow"
# Read data
file = "../../Downloads/Dimensions-Publication-2022-11-04_03-22-22.csv"
# Convert bibtext to dataframe
M = bibliometrix::convert2df(file = file,
                             dbsource = "dimensions",
                             format = "csv") |>
  dplyr::filter(PY != is.na(PY)) |>
  dplyr::filter(AB != is.na(AB)) |>
  dplyr::mutate(Year = substr(PY, 1, 4)) |>
  dplyr::filter(Year > 2019) |>
  dplyr::select(-c(Year))
# Conduct a biblio analysis of dataframe using the bibliometrix package
results = bibliometrix::biblioAnalysis(M, sep = ";")
options(width = 100)
S = summary(object = results, k = 50, pause = FALSE)
#readRDS( "data/gisaid-corpus-analysis.rds")

#### BUILD NETWORKS
build_networks(M, "data/networks/gisaid/")
#### PLOT AND VISUALSE NETWORKS WITH VOSVIEWER AND IGRAPH

#plot_colab_network(author_colab, vos = TRUE)

plot_mcp_scp = function(data,db){
  Country = S$MostProdCountries$Country
  SCP = S$MostProdCountries$SCP
  MCP = S$MostProdCountries$MCP
  Articles = S$MostProdCountries$Articles

  data = as.data.frame(cbind(Country, SCP, MCP, Articles)) |>
    arrange(desc(Articles)) |>
    pivot_longer(c(SCP, MCP)) |>
    mutate(value = as.numeric(value)) |>
    mutate(Articles = as.numeric(Articles)) |>
    mutate(collaboration = name)

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
    labs(title = paste0("Leading 50 countries mentioning",db,"\nin scientific publications"), caption  = "Publications containing the search query ‘The Covid-19 Data Portal’ OR 'European Nucleotide Archive' \nwere accessed via the Dimensions Analytics API and filtered to include publications between January 1st 2019 \nand October 1st 2021 which contain the phrase 'covid-19' OR 'sars-cov-2' in the full text.\nSCP: Single Country Publication. MCP: Multi Country Publication") +
    xlab("Country") +
    ylab("No. Documents") +
    coord_flip() + theme_landscape()

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





##################################################################
##                        Topic Modelling                      ##
################

# Topic-Modelling ----------------------------------------------------------

M = M |> dplyr::filter(AB != "")

kens = M$AB |>
  stringr::str_to_lower() |>
  stringr::str_remove_all("gisaid") |>
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

jpeg("plots/gisaid/topic-fit-gisaid.jpeg",
     width = 800,
     height = 800)
ggplot(plot, aes(K, value, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Number of topics K",
       title = "Statistical fit of models with different K") +  theme(legend.position =
                                                                        "none",
                                                                      plot.title = element_text(size = 11)) +
  theme_k()
dev.off()

# Set optimal number of K based on fit
k = 5
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
jpeg("plots/gisaid/topics-gisaid.jpeg",
     width = 800,
     height = 800)
par(bg = "black", col = "white",col.axis = 'yellow', col.lab = 'red')
plot(model, main = "Top Topics GISAID")
dev.off()

# Plot effect of topic per year
effect = stm::estimateEffect(formula =  ~ d,
                             stmobj = model,
                             metadata = M)
labels = stm::labelTopics(model, 1:k)
jpeg("plots/gisaid/effect-gisaid.jpeg",
     width = 800,
     height = 800)
par(mfcol = c(2, 3))
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
