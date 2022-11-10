## ---------------------------
##
## Script name: plot-treemap.r
##
## Purpose of script: Plot global distribution of submissions
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-11-02
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
library(ggpubr)


tree_df = main_df |> dplyr::filter(wy == "22/39")
textcol = "grey40"
a = ggplot(
  tree_df,
  aes(
    area = GISAID.total.submissions,
    fill = GISAID.total.submissions,
    label = country,
    subgroup = continent
  )
) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(
    place = "centre",
    grow = TRUE,
    alpha = 0.25,
    colour = "black",
    fontface = "italic"
  ) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 15,
    grow = TRUE
  ) +  theme(
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
    legend.background = element_rect(fill = "gray12"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
  ) +   labs(title = "\nGlobal SARS-CoV-2 cases sequenced and shared with the GISAID \n\n",
             caption = "\nGISAID Metadata: https://www.epicov.org/")

b = ggplot(
  tree_df,
  aes(
    area = CD19DP.total.submissions,
    fill = CD19DP.total.submissions,
    label = country,
    subgroup = continent
  )
) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(
    place = "centre",
    grow = TRUE,
    alpha = 0.25,
    colour = "black",
    fontface = "italic"
  ) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 15,
    grow = TRUE
  ) +  theme(
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
    legend.background = element_rect(fill = "gray12"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )
  ) +   labs(title = "\nGlobal SARS-CoV-2 cases sequenced and shared with the Covid-19 Data Portal \n\n",
             caption = "\nThe Covid-19 Data Portal Metadata: https://www.covid19dataportal.org/")

ggarrange(a, b)

ggsave(
  paste0("plots/presentation/treemap.png"),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)
