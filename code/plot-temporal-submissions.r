## ---------------------------
##
## Script name: plot-temporal-submissions.r
##
## Purpose of script: plot temporal submissions
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-11-01
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
gisaid = readRDS("data/gisaid.RDS")
embl = readRDS("data/embl.RDS")

gisaid_temporal_subs = gisaid |>
  filter(wy < "22/38") |>
  group_by(wy) |>
  summarise(sum_gisaid = sum(GISAID.weekly.submissions)) |>
  arrange(wy)

cd19dp_temporal_subs = embl |>
  filter(wy < "22/38") |>
  group_by(wy) |>
  summarise(sum_cd19dp = sum(C19DP.weekly.submissions)) |>
  arrange(wy)


gisaid_temporal_subs_av = gisaid |>
  filter(wy < "22/38") |>
  group_by(wy) |>
  summarise(av_gisaid = mean(GISAID.weekly.submissions)) |>
  arrange(wy)

cd19dp_temporal_subs_av = embl |>
  filter(wy < "22/38") |>
  group_by(wy) |>
  summarise(av_cd19dp = mean(C19DP.weekly.submissions)) |>
  arrange(wy)



temporal_sub_all = right_join(gisaid_temporal_subs,cd19dp_temporal_subs)
temporal_sub_all_av = right_join(gisaid_temporal_subs_av,cd19dp_temporal_subs_av)

textcol = "white"

ggplot(data=temporal_sub_all) +
  geom_line(aes(x=wy,y=sum_gisaid, group=1, color='GISAID Monthly Total')) +
  geom_line(aes(x=wy,y=sum_cd19dp, group=1, color ='CD19DP Monthly Total')) +
  # annotate("text", x = 7, y = 60000, label = "WHO Declares\n Pandemic") +
  # annotate("text", x = 17, y = 6000, label = "AstraZeneca's \nVaccine Authorised") +
  # annotate("text", x = 18, y = 600000, label = "EBI Open letter") +
  # annotate("text", x = 27, y = 700000, label = "Global Covid-19 Deaths \nPass Five Million") +
  scale_colour_manual("",
                      breaks = c("GISAID Monthly Total", "CD19DP Monthly Total","Genebank Monthly Total"),
                      values = c("green", "blue")) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(x = "Date",
       y = "Sequence Submissions",
       title = "Monthly Total SARS-CoV-2 Sequence Submissions",
       caption = "\nMonthly totals of global SARS-CoV-2 cases sequenced and shared on the GISAID \nand Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ ") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  theme(
    legend.position = "bottom",
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
    legend.background = element_rect(fill = "transparent"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    ))

ggsave(
  paste0(
    "plots/temporal_sum.png"
  ),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)

ggplot(data=temporal_sub_all_av) +
  geom_line(aes(x=wy,y=av_gisaid, group=1, color='GISAID Monthly Total')) +
  geom_line(aes(x=wy,y=av_cd19dp, group=1, color ='CD19DP Monthly Total')) +
  # annotate("text", x = 7, y = 60000, label = "WHO Declares\n Pandemic") +
  # annotate("text", x = 17, y = 6000, label = "AstraZeneca's \nVaccine Authorised") +
  # annotate("text", x = 18, y = 600000, label = "EBI Open letter") +
  # annotate("text", x = 27, y = 700000, label = "Global Covid-19 Deaths \nPass Five Million") +
  scale_colour_manual("",
                      breaks = c("GISAID Monthly Total", "CD19DP Monthly Total","Genebank Monthly Total"),
                      values = c("green", "blue")) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(x = "Date",
       y = "Sequence Submissions",
       title = "Monthly Mean SARS-CoV-2 Sequence Submissions",
       caption = "\nMonthly mean of global SARS-CoV-2 cases sequenced and shared on the GISAID \nand Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ ") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  theme(
    legend.position = "bottom",
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
    ))
ggsave(
  paste0(embl$
    "plots/temporal_average.png"
  ),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)


###### LOG plot
## ---------------------------
##
## Script name: plot-temporal-submissions.r
##
## Purpose of script: plot temporal submissions
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-11-01
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

ggplot(data=temporal_sub_all) +
  geom_line(aes(x=wy,y=sum_gisaid, group=1, color='GISAID Monthly Total')) +
  geom_line(aes(x=wy,y=sum_cd19dp, group=1, color ='CD19DP Monthly Total')) +
  # annotate("text", x = 7, y = 60000, label = "WHO Declares\n Pandemic") +
  # annotate("text", x = 17, y = 6000, label = "AstraZeneca's \nVaccine Authorised") +
  # annotate("text", x = 18, y = 600000, label = "EBI Open letter") +
  # annotate("text", x = 27, y = 700000, label = "Global Covid-19 Deaths \nPass Five Million") +
  scale_colour_manual("",
                      breaks = c("GISAID Monthly Total", "CD19DP Monthly Total","Genebank Monthly Total"),
                      values = c("green", "blue")) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(x = "Date",
       y = "Sequence Submissions",
       title = "Monthly Total SARS-CoV-2 Sequence Submissions",
       caption = "\nMonthly totals (log) of global SARS-CoV-2 cases sequenced and shared on the GISAID and Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ ") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  theme(
    legend.position = "bottom",
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
    legend.background = element_rect(fill = "transparent"),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      size = 18,
      face = "bold",
      vjust = 0.9
    )) +   scale_y_continuous(trans='log2')

ggsave(
  paste0(
    "plots/temporal_sum_log.png"
  ),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)

ggplot(data=temporal_sub_all_av) +
  geom_line(aes(x=wy,y=av_gisaid, group=1, color='GISAID Monthly Total')) +
  geom_line(aes(x=wy,y=av_cd19dp, group=1, color ='CD19DP Monthly Total')) +
  # annotate("text", x = 7, y = 60000, label = "WHO Declares\n Pandemic") +
  # annotate("text", x = 17, y = 6000, label = "AstraZeneca's \nVaccine Authorised") +
  # annotate("text", x = 18, y = 600000, label = "EBI Open letter") +
  # annotate("text", x = 27, y = 700000, label = "Global Covid-19 Deaths \nPass Five Million") +
  scale_colour_manual("",
                      breaks = c("GISAID Monthly Total", "CD19DP Monthly Total","Genebank Monthly Total"),
                      values = c("green", "blue")) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(x = "Date",
       y = "Sequence Submissions",
       title = "Monthly Mean SARS-CoV-2 Sequence Submissions",
       caption = "\nMonthly mean (log) of global SARS-CoV-2 cases sequenced and shared on the GISAID and Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ ") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + theme(
         legend.position="none",
         plot.title = element_text(size=11)
       ) +
  theme(
    legend.position = "bottom",
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
    )) +  scale_y_continuous(trans='log2')
ggsave(
  paste0(
    "plots/temporal_average_log.png"
  ),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)
rm(gisaid,embl)
