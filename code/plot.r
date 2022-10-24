## ---------------------------
##
## Script name: plot.r
##
## Purpose of script: visualise results
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-10-23
##
## Copyleft (c) Nathanael Sheehan, 2022
## Email: nathanaelsheehan@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

plot_df = main_df  |>
  # convert state to factor and reverse order of levels
  mutate(country = factor(country, levels = rev(sort(unique(
    country
  ))))) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` != Inf) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` < 100) |>
  dplyr::filter(continent == "Europe") |>
  # create a new variable from count
  mutate(countfactor = cut(
    `Genomes per confirmed cases % (C19DP)`,
    breaks = c(
      0,
      1,
      2,
      3,
      4,
      5,
      max(`Genomes per confirmed cases % (GISAID)`, na.rm = F)
    ),
    labels = c("0-1%", "1-2%", "2-3%", "3-4%", "4-5%", ">5%")
  )) |>
  # change level order
  mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor))))


textcol = "white"
ggplot(plot_df , aes(x = wy, y = country, fill = countfactor)) +
  #add border white colour of line thickness 0.25
  geom_bin_2d(colour = "white", size = 0.2) +
  guides(fill = guide_legend(title = "Percent of sequenced cases \nper epidemiological week")) +
  labs(x = "Epidemiological Week", y = "Country", title = paste0("SARS-CoV2 sequences shared with GISAID in ",plot_df$continent[1])) +
  #scale_x_discrete(expand=c(0, 0), breaks=c("20/01", "20/26", "21/01", "21/26", "22/01", "22/26"))+
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "Blues")),
                    na.value = "black") +
  labs(caption = "Percent of reported cases (Our World In Data) that were sequenced and shared to the GISAID database per\n epidemiological week between December 19th 2019 and October 19th 2022. GISAID Metadata was accessed and used \nfollowing their terms of use. Epidemiological data was linked via joining Our World in Data on Covid-19 accessed on 24th October 2022.") +
  theme_grey(base_size = 10) +
  # facet_grid(.~continent) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(colour = textcol, face = "italic", size = 14),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 14,
      face = "bold"
    ),
    legend.key.height = grid::unit(0.8, "cm"),
    legend.key.width = grid::unit(0.2, "cm"),
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
