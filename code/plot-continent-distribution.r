## ---------------------------
##
## Script name: plot-continent-distribution.r
##
## Purpose of script: create violon plot of distribution per continent
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-10-25
##
## Email: nathanaelsheehan@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
# sample size

gisaid = readRDS("data/gisaid.RDS")
#GISAID
gisaid = gisaid  |>
  right_join(owid) |>
  left_join(sample_size)

sample_size = gisaid |> group_by(continent) |> summarize(num=n())

gisaid |> mutate(myaxis = paste0(continent, "\n", "n=", num)) |>
  filter(GISAID.weekly.submissions != is.na(GISAID.weekly.submissions)) %>%
  ggplot( aes(x=myaxis, y=GISAID.weekly.submissions, fill=continent)) +
  geom_jitter(col = "red") +
  coord_flip() +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs(title = "Culmative samples and overall percentage of sequenced cases in \nthe GISAID platform per continent",
       y = "% of sequenced cases per epidemiological week",
       x = "Continent") +
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

ggsave(
  paste0(
    "plots/GISAID/continent violin.png"
  ),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)

# The coivid 19 data portal
embl = readRDS("data/embl.RDS")

embl = embl  |>
  left_join(owid)

sample_size = embl |> group_by(continent) |> summarize(num=n())

embl |> mutate(myaxis = paste0(continent, "\n", "n=", num)) |>
  filter(C19DP.weekly.submissions != is.na(C19DP.weekly.submissions)) %>%
  ggplot( aes(x=myaxis, y=C19DP.weekly.submissions, fill=continent)) +
  geom_jitter(aes(colour = continent)) +
  geom_violin(width = 1.4) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs(title = "Culmative samples and overall percentage of sequenced cases in \nThe Covid-19 Data Portal per continent",
       y = "% of sequenced cases per epidemiological week",
       x = "Continent") +
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
    )) + coord_flip()

ggsave(
  paste0(
    "plots/GISAID/continent violin.png"
  ),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)
