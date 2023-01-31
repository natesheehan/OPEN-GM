## ---------------------------
##
## Script name:
##
## Purpose of script: Visulations for landscape plot
## Author: Nathanael Sheehan
##
## Date Created: 2022-12-13
##
## Copyleft (c) Nathanael Sheehan, 2022
##
## ---------------------------
##
## Notes:
## Plot 1: Treemap, Temporal Regional Submissions, Submissions as a percent of Covid cases
## Methods:
## Platforms: GISAID, The covid-19 portal
## Data:
## ---------------------------

# Treemap -----------------------------------------------------------------

tree_df = main_df |> dplyr::filter(wy == "22/39")

a = ggplot(
  tree_df,
  aes(
    area = GISAID.total.submissions,
    fill = GISAID.total.submissions,
    label = country,
    subgroup = Sub.region.Name
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
  ) +   labs(title = "a)",
             caption = "\nGISAID Metadata: https://www.epicov.org/") + theme_tree()  +
  scale_fill_viridis_c()

d = ggplot(
  tree_df,
  aes(
    area = CD19DP.total.submissions,
    fill = CD19DP.total.submissions,
    label = country,
    subgroup = Sub.region.Name
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
  ) +  labs(title = "C)",
            caption = "\nThe Covid-19 Data Portal Metadata: https://www.covid19dataportal.org/") +
  theme_tree() +
  scale_fill_viridis_c()


# Temporal Submissions (Regions) ------------------------------------------

# Sort data into weekly sums
gis_main_df = main_df |>
  filter(wy < "22/38") |>
  group_by(wy,Sub.region.Name) |>
  summarise(sum_gisaid = sum(GISAID.weekly.submissions)) |>
  mutate(percentage_gis = sum_gisaid / sum(sum_gisaid))

c19_main_df = main_df |>
  filter(wy < "22/38") |>
  group_by(wy,Sub.region.Name) |>
  summarise(sum_cd19dp = sum(C19DP.weekly.submissions)) |>
  mutate(percentage_c19 = sum_cd19dp / sum(sum_cd19dp))

sum_gis_c19 = right_join(gis_main_df,c19_main_df)
sum_gis_c19$t = as.numeric(stringr::str_remove(sum_gis_c19$wy, "/"))
sum_gis_c19$f = factor(sum_gis_c19$Sub.region.Name,      # Reordering group factor levels
                       levels = paste(unique(sum_gis_c19$Sub.region.Name)))

# GISAID
b = ggplot(sum_gis_c19, aes(x=t, y=(sum_gisaid), fill=f)) +
  geom_smooth() + theme_landscape() + coord_trans() + labs(title = "B)")

# The covid-19 data portal
e = ggplot(sum_gis_c19, aes(x=t, y=(sum_cd19dp), fill=f)) +
  geom_smooth() + theme_landscape() + coord_trans() + labs(title = "E)")


# Plot landscape ----------------------------------------------------------

# GISAID
plot_df = main_df  |>
  filter(country != is.na(country)) |>
  filter(Sub.region.Name != is.na(Sub.region.Name)) |>
  # convert state to factor and reverse order of levels
  mutate(country = factor(country, levels = rev(sort(unique(
    country
  ))))) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` != Inf) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` < 100) |>
  dplyr::mutate(gpnc_gisaid = ifelse(gpnc_gisaid == Inf, 99998, gpnc_gisaid)) |> # missing case data
  dplyr::mutate(gpnc_gisaid = ifelse(is.na(gpnc_gisaid), 999998, gpnc_gisaid))  |> # missing seqeunce data
  dplyr::mutate(gpnc_gisaid = ifelse(gpnc_gisaid == 0, 9999998, gpnc_gisaid)) |> # missing seqeunce data
  # create a new variable from count
  mutate(countfactor = cut(
    gpnc_gisaid,
    breaks = c(0,
               1,
               2,
               3,
               4,
               5,
               20550,
               99999,
               999999,
               max(gpnc_gisaid, na.rm = F)),
    labels = c(
      "0-1%",
      "1-2%",
      "2-3%",
      "3-4%",
      "4-5%",
      ">5%",
      "Missing Case Data",
      "Missing Case and Sequence Data",
      "Missing Sequence Data"
    )
  )) |>
  # change level order
  mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor))))

plot_df$f = factor(plot_df$Income.group,      # Reordering group factor levels
                   levels = paste(unique(plot_df$Income.group)))
c = ggplot(plot_df,
           aes(x = wy, y = iso_code, fill = countfactor)) +
  #add border white colour of line thickness 0.25
  geom_bin_2d(colour = "white", size = 0.6) +
  facet_grid(rows = vars(f), scales = "free_y", switch = "y", space = "free_y") +
  guides(fill = guide_legend(title = "Percent of sequenced cases \nper epidemiological week")) +
  labs(
    x = "Epidemiological Week",
    y = "Country",
    title = paste0("B)"),
    caption = "Percent of reported cases (Our World In Data) that were sequenced and shared to the GISAID database per\n epidemiological week between December 19th 2019 and October 19th 2022. GISAID Metadata was accessed and used \nfollowing their terms of use. Epidemiological data was linked via joining Our World in Data on Covid-19 accessed on 24th October 2022."
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_grey(base_size = 10) +
  scale_fill_manual(
    values = c(
      "gray12",
      "gray47",
      "gray96",
      "#845ec2",
      "#d65db1" ,
      "#ff6f91",
      "#ff9671",
      "#ffc75f",
      "#f9f871",
      "#ddf1da"
    ),
    na.value = "grey90"
  ) + theme_landscape() +   theme(
    panel.spacing = unit(0, units = "cm"),
    # removes space between panels
    strip.text.y = element_text(angle = 270, face = "bold",size = 14),
    strip.placement = "outside"
  )

# The covid-19 data portal
plot_df = main_df  |>
  # convert state to factor and reverse order of levels
  mutate(country = factor(country, levels = rev(sort(unique(
    country
  ))))) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` != Inf) |>
  # dplyr::filter(`Genomes per confirmed cases % (GISAID)` < 100) |>
  dplyr::mutate(gpnc_embl = ifelse(gpnc_embl == Inf, 99998, gpnc_embl)) |> # missing case data
  dplyr::mutate(gpnc_embl = ifelse(is.na(gpnc_embl), 999998, gpnc_embl))  |> # missing seqeunce data
  dplyr::mutate(gpnc_embl = ifelse(gpnc_embl == 0, 9999998, gpnc_embl)) |> # missing seqeunce data
  # create a new variable from count
  mutate(countfactor = cut(
    gpnc_embl,
    breaks = c(0,
               1,
               2,
               3,
               4,
               5,
               20550,
               99999,
               999999,
               max(gpnc_embl, na.rm = F)),
    labels = c(
      "0-1%",
      "1-2%",
      "2-3%",
      "3-4%",
      "4-5%",
      ">5%",
      "Missing Case Data",
      "Missing Case and Sequence Data",
      "Missing Sequence Data"
    )
  )) |>
  # change level order
  mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor))))

plot_df$f = factor(plot_df$Income.group,      # Reordering group factor levels
                   levels = paste(unique(plot_df$Income.group)))

f = ggplot(plot_df , aes(x = wy, y = iso_code, fill = countfactor)) +
  #add border white colour of line thickness 0.25
  geom_bin_2d(colour = "white", size = 0.4) +
  facet_grid(rows = vars(f), scales = "free_y", switch = "y", space = "free_y") +
  theme(
    panel.spacing = unit(0, units = "cm"),
    # removes space between panels
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  guides(fill = guide_legend(title = "Percent of sequenced cases \nper epidemiological week")) +
  labs(
    x = "Epidemiological Week",
    y = "Country",
    title = paste0("D)"),
    caption = "Percent of reported cases (Our World In Data) that were sequenced and shared to the Covid-19 Data portal database per\n epidemiological week between December 19th 2019 and October 19th 2022. GISAID Metadata was accessed and used \nfollowing their terms of use. Epidemiological data was linked via joining Our World in Data on Covid-19 accessed on 24th October 2022."
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_grey(base_size = 10) +
  scale_fill_manual(
    values = c(
      "gray12",
      "gray47",
      "gray96",
      "#845ec2",
      "#d65db1" ,
      "#ff6f91",
      "#ff9671",
      "#ffc75f",
      "#f9f871",
      "#ddf1da"
    ),
    na.value = "grey90"
  ) + theme_landscape() + theme_landscape() +   theme(
    panel.spacing = unit(0, units = "cm"),
    # removes space between panels
    strip.text.y = element_text(angle = 270, face = "bold",size = 14),
    strip.placement = "outside"
  )


# Plot and Save -----------------------------------------------------------

# GISAID
  ggarrange(a, c,d,f )

ggsave(
  paste0("plots/GISAID/",
         " Sequence-Landscape.png"),
  dpi = 320,
  width = 32,
  height = 32,
  limitsize = FALSE
)

# GISAID
ggarrange(
  f,                # First row with line plot
  # Second row with box and dot plots
  ggarrange(e, d, ncol = 2, labels = c("D", "E")),
  nrow = 2,
  labels = "F"       # Label of the line plot
)

ggsave(
  paste0("plots/EMBL/",
         " Sequence-Landscape.png"),
  dpi = 320,
  width = 32,
  height = 32,
  limitsize = FALSE
)

rm(plot_df,c19_main_df,gis_main_df,sum_gis_c19,tree_df)
rm(a,b,c,d,e,f)
