## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Nathanael Sheehan
##
## Date Created: 2023-02-03
##
## Copyleft (c) Nathanael Sheehan, 2023
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
set.seed(999)
db = "GISAID"

##################################################################
##                        Network Build                         ##

if (db == "GISAID") {
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
  S = summary(object = results,
              k = 50,
              pause = FALSE)
  oa = as.data.frame(table(M$Open.Access)) |> dplyr::rename("Access Type" = Var1)
  knitr::kable(oa, caption = "GISAID Publications") |> kableExtra::kable_classic()

  #### BUILD NETWORKS
  plot_mcp_scp(S, "gisaid")
  build_networks(M, "data/networks/gisaid/")
  #### PLOT AND VISUALSE NETWORKS WITH VOSVIEWER AND IGRAPH
} else {
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
  S = summary(object = results,
              k = 50,
              pause = FALSE)
  oa = as.data.frame(table(M$Open.Access)) |> dplyr::rename("Access Type" = Var1)
  knitr::kable(oa, caption = "The Covid-19 Data Portal Publications") |> kableExtra::kable_classic()

  #### BUILD NETWORKS
  plot_mcp_scp(S, "EMBL")
  build_networks(M, "data/networks/ena/")
}


#plot_colab_network(author_colab, vos = TRUE)

##################################################################
##                        Topic Modelling                      ##


if (db == "GISAID") {
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
    facet_wrap( ~ variable, scales = "free_y") +
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
  par(
    bg = "black",
    col = "white",
    col.axis = 'yellow',
    col.lab = 'red'
  )
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

} else {
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
    facet_wrap( ~ variable, scales = "free_y") +
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
  par(
    bg = "black",
    col = "white",
    col.axis = 'yellow',
    col.lab = 'red'
  )
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
  par(mfcol = c(2, 3), col.main = "white")
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

}
