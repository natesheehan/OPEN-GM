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
S = summary(object = results, k = 100, pause = FALSE)
saveRDS(S, "data/gisaid-corpus-analysis.rds")

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

ggplot(data[1:100, ], aes(
  fill = collaboration,
  y = value,
  x = reorder(Country, Articles)
)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Studies citing GISAID", caption  = "Biblographic data was accessed by querying 'GISAID'' the dimensions.ai API between January first 2020 and October 1st 2021\nSCP: Single Country Publication. MCP: Multi Country Publication") +
  xlab("Country") +
  ylab("No. Documents") +
  coord_flip() + theme_landscape()

# Co-word Analysis through Keyword co-occurrences

NetMatrix <-
  biblioNetwork(M,
                analysis = "co-occurrences",
                network = "keywords",
                sep = ";")
summary(networkStat(NetMatrix))
# Main statistics about the network
#
# Size                                  2715
# Density                               0.021
# Transitivity                          0.162
# Diameter                              4
# Degree Centralization                 0.918
# Average path length                   2.012

net = bibliometrix::networkPlot(
  NetMatrix,
  normalize = "association",
  n = 50,
  Title = "Keyword Co-occurrences \nGISAID",
  type = "fruchterman",
  size.cex = TRUE,
  size = 20,
  remove.multiple = F,
  edgesize = 10,
  labelsize = 5,
  label.cex = TRUE,
  label.n = 30,
  edges.min = 2,
  label.color = FALSE
)

net2VOSviewer(net, vos.path = "VOSviewer/")

# Author collaboration network
NetMatrix <-
  biblioNetwork(M,
                analysis = "collaboration",
                network = "authors",
                sep = ";")
summary(networkStat(NetMatrix))

net = networkPlot(
  NetMatrix,
  n = 50,
  Title = "Author collaboration \nGISAID",
  type = "auto",
  size = 10,
  size.cex = T,
  edgesize = 3,
  labelsize = 1
)
net2VOSviewer(net, vos.path = "VOSviewer/")

# Education collaboration network
NetMatrix <-
  biblioNetwork(M,
                analysis = "collaboration",
                network = "universities",
                sep = ";")
summary(networkStat(NetMatrix))

# Main statistics about the network
#
# Size                                  5469
# Density                               0.005
# Transitivity                          0.351
# Diameter                              9
# Degree Centralization                 0.133
# Average path length                   3.259

net = networkPlot(
  NetMatrix,
  n = 50,
  Title = "Institution collaboration\n(GISAID)",
  type = "auto",
  size = 4,
  size.cex = F,
  edgesize = 3,
  labelsize = 1
)
net2VOSviewer(net, vos.path = "VOSviewer/")

# Country collaboration
NetMatrix <-
  biblioNetwork(M,
                analysis = "collaboration",
                network = "countries",
                sep = ";")
summary(networkStat(NetMatrix))
# Main statistics about the network
#
# Size                                  153
# Density                               0.255
# Transitivity                          0.658
# Diameter                              4
# Degree Centralization                 0.547
# Average path length                   1.821
net = networkPlot(
  NetMatrix,
  n = dim(NetMatrix)[1],
  Title = "Country collaboration",
  type = "circle",
  size = 10,
  size.cex = T,
  edgesize = 1,
  labelsize = 0.6,
  cluster = "none"
)
net2VOSviewer(net, vos.path = "VOSviewer/")



# Topic-Modelling ----------------------------------------------------------
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
K = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
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
