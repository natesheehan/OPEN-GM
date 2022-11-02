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
textcol = "grey40"
# Read data
file = (
  "../../Downloads/savedrecs(7).bib"
)
# Convert bibtext to dataframe
M = bibliometrix::convert2df(file = file,
                             dbsource = "wos",
                             format = "bibtex") |> dplyr::filter(PY != is.na(PY)) |> dplyr::filter(AB != is.na(AB))
# Conduct a biblio analysis of dataframe using the bibliometrix package
results = bibliometrix::biblioAnalysis(M, sep = ";")
options(width = 100)
S = summary(object = results, k = 100, pause = FALSE)
saveRDS(S,"data/gisaid-corpus-analysis.rds")

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
# jpeg("plots/LIT-REVIEW/model-fit.jpeg")
# # plot fit
# plot(fit)
# dev.off()v

plot = data.frame("K" = K,
                   "Residuals" = unlist(fit$results$residual),
                   "Exclusivity" = unlist(fit$results$exclus),
                   "Held-Out-Likelihood" = unlist(fit$results$heldout),
                   "Semantic Coherence" = unlist(fit$results$semcoh),
                   "Bound" = unlist(fit$results$bound),
                   "Lower Bound" = unlist(fit$results$lbound))
plot = reshape2::melt(plot, id=c("K"))

jpeg("plots/LIT-REVIEW/topic-fit-gisaid.jpeg", width = 800, height = 800)
ggplot(plot, aes(K, value, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~variable,scales = "free_y") +
  labs(x = "Number of topics K",
       title = "Statistical fit of models with different K") +  theme(
         legend.position="none",
         plot.title = element_text(size=11)
       ) +
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
    ))
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
jpeg("plots/LIT-REVIEW/topics-gisaid.jpeg", width = 800, height = 800)
plot(model, main = "Top Topics GISAID")
dev.off()

# Plot effect of topic per year
effect = stm::estimateEffect(formula =  ~ d,
                             stmobj = model,
                             metadata = M)
labels = stm::labelTopics(model, 1:k)
jpeg("plots/LIT-REVIEW/effect-gisaid.jpeg", width = 800, height = 800)
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

# #### NETWORKS
# A = cocMatrix(M, Field = "SO", sep = ";")
# sort(Matrix::colSums(A), decreasing = TRUE)[1:5]
#
