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


# Load data & create plot dataframes ---------------------------------------------------------------
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

temporal_sub_all = right_join(gisaid_temporal_subs, cd19dp_temporal_subs)
temporal_sub_all_av = right_join(gisaid_temporal_subs_av, cd19dp_temporal_subs_av)



# Plot total --------------------------------------------------------------
textcol = "white"

# SUM
ggplot(data = temporal_sub_all) +
  geom_line(aes(
    x = wy,
    y = sum_gisaid,
    group = 1,
    color = 'GISAID Monthly Total'
  )) +
  geom_line(aes(
    x = wy,
    y = sum_cd19dp,
    group = 1,
    color = 'CD19DP Monthly Total'
  )) +
  scale_colour_manual(
    "",
    breaks = c("GISAID Monthly Total",
               "CD19DP Monthly Total"),
    values = c("seagreen1", "cyan")
  ) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(
    x = "Date",
    y = "Sequence Submissions",
    title = "Monthly Total SARS-CoV-2 Sequence Submissions",
    caption = "\nMonthly totals of global SARS-CoV-2 cases sequenced and shared on the GISAID \nand Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ "
  ) + theme_temporal()

ggsave(
  paste0("plots/temporal_sum.png"),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)

# MEAN
ggplot(data = temporal_sub_all_av) +
  geom_line(aes(
    x = wy,
    y = av_gisaid,
    group = 1,
    color = 'GISAID Monthly Total'
  )) +
  geom_line(aes(
    x = wy,
    y = av_cd19dp,
    group = 1,
    color = 'CD19DP Monthly Total'
  )) +
  scale_colour_manual(
    "",
    breaks = c("GISAID Monthly Total",
               "CD19DP Monthly Total"),
    values = c("seagreen1", "cyan")
  ) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(
    x = "Date",
    y = "Sequence Submissions (Logged)",
    title = "Monthly Mean SARS-CoV-2 Sequence Submissions",
    caption = "\nMonthly mean of global SARS-CoV-2 cases sequenced and shared on the GISAID \nand Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ "
  ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_temporal()

ggsave(
  paste0("plots/temporal_average.png"),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)


# Plot log ----------------------------------------------------------------

# SUM

ggplot(data = sum_gis_c19) +
  geom_point(aes(
    x = wy,
    y = sum_gisaid,
    group = Sub.region.Name,
    color = 'GISAID Monthly Total'
  )) +
  # geom_line(aes(
  #   x = wy,
  #   y = sum_cd19dp,
  #   group = Sub.region.Name,
  #   color = 'CD19DP Monthly Total'
  # )) +
  scale_colour_manual(
    "",
    breaks = c(
      "GISAID Monthly Total",
      "CD19DP Monthly Total",
      "Genebank Monthly Total"
    ),
    values = c("seagreen1", "cyan")
  ) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(
    x = "Date",
    y = "Sequence\nSubmissions \n(log2n)",
    title = "Monthly Total SARS-CoV-2 Sequence Submissions",
    caption = "\nMonthly totals (log2n) of global SARS-CoV-2 cases sequenced and shared on the GISAID and Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ "
  ) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15)) +
  scale_y_continuous(trans = 'log2')

ggsave(
  paste0("plots/temporal_sum_log.png"),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)

# MEAN

ggplot(data = temporal_sub_all_av) +
  geom_line(aes(
    x = wy,
    y = av_gisaid,
    group = 1,
    color = 'GISAID Monthly Total'
  )) +
  geom_line(aes(
    x = wy,
    y = av_cd19dp,
    group = 1,
    color = 'CD19DP Monthly Total'
  )) +
  scale_colour_manual(
    "",
    breaks = c(
      "GISAID Monthly Total",
      "CD19DP Monthly Total"
    ),
    values = c("cyan", "purple")
  ) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(
    x = "Date",
    y = "Sequence Submissions",
    title = "Monthly Mean SARS-CoV-2 Sequence Submissions",
    caption = "\nMonthly mean (log) of global SARS-CoV-2 cases sequenced and shared on the GISAID and Covid-19 Data Platform database between December 2019 and October 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ "
  ) + theme_temporal() + scale_y_continuous(trans = 'log2')


