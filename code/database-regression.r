# Regression between two databases ----------------------------------------
gis_main_df_europe = main_df |>
  filter(wy < "22/38") |>
  group_by(wy,Sub.region.Name) |>
  summarise(sum_gisaid = sum(GISAID.weekly.submissions)) |>
  mutate(percentage_gis = sum_gisaid / sum(sum_gisaid))

c19_main_df_europe = main_df |>
  filter(wy < "22/38") |>
  group_by(wy,Sub.region.Name) |>
  summarise(sum_cd19dp = sum(C19DP.weekly.submissions)) |>
  mutate(percentage_c19 = sum_cd19dp / sum(sum_cd19dp))

sum_gis_c19 = right_join(gis_main_df_europe,c19_main_df_europe)
sum_gis_c19$t = as.numeric(stringr::str_remove(sum_gis_c19$wy, "/"))
sum_gis_c19$f = factor(sum_gis_c19$Sub.region.Name,      # Reordering group factor levels
                       levels = paste(unique(sum_gis_c19$Sub.region.Name)))

regions = as.data.frame(unique(sum_gis_c19$Sub.region.Name))
colnames(regions) = "region"

# model_by_income_group <- lm(sum_cd19dp ~ sum_gisaid,  data=sum_gis_c19)
# anova_results <- anova(model_by_income_group)
#
# ll = sum_gis_c19 %>% select(wy,Sub.region.Name,sum_gisaid,sum_cd19dp) %>% group_by(Sub.region.Name,wy)
# result_by_group_and_week <- summarise(ll %>% na.omit(), cor = cor(sum_gisaid, sum_cd19dp))
#
ggscatter(
  sum_gis_c19 %>% mutate(
    sum_gisaid = log(sum_gisaid),
    sum_c19dp = log(sum_cd19dp)
  ),
  x = "sum_gisaid",
  y = "sum_c19dp",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "kendall",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions",
  title = "Global",
  ellipse = TRUE, mean.point = TRUE,
  star.plot = TRUE
)

vv=main_df |>   filter(wy == "22/38")

fit <- lm(GISAID.total.submissions ~ CD19DP.total.submissions + log(gdp_per_capita) + Sub.region.Name, data = vv)
summ(fit)


p1 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Latin America and the Caribbean") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p2 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Northern America") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p3 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "South-eastern Asia") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)

p4 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Eastern Asia") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p5 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Central Asia") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p6 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Western Asia") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)

p7 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Southern Asia") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p8 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Northern Europe") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)

p9 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Southern Europe") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p10 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Western Europe") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p11 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Eastern Europe") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p12 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Australia and New Zealand") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p13 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Micronesia") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p14 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Polynesia") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p15 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Northern Africa") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)
p16 = ggscatter(
  sum_gis_c19 %>%
    filter(Sub.region.Name == "Sub-Saharan Africa") %>%
    mutate(
      sum_gisaid = log(sum_gisaid),
      sum_cd19dp = log(sum_cd19dp)
    ),
  x = "sum_gisaid",
  y = "sum_cd19dp",
  add = "reg.line",
  facet.by = "Sub.region.Name",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  xlab = "GISAID total submissions",
  ylab = "ENA total submissions"
)



ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p15,p16,p0,nrow = 4,ncol = 4) |>
  annotate_figure(bottom = text_grob("Pearson correlation coefficient",
                                     color = "black", face = "bold", size = 16)) + theme_landscape()

ggsave(
  paste0("plots/submission-correlation.png"),
  dpi = 320,
  width = 24,
  height = 32,
  limitsize = FALSE
 )
#
# # Shapiro-Wilk normality test
# #
# # data:  temporal_sub_all$sum_gisaid
# # W = 0.74337, p-value = 0.00000000000001837
# # Shapiro-Wilk normality test for wt
# shapiro.test(temporal_sub_all$sum_cd19dp) # => p = 0.09
# # Shapiro-Wilk normality test
# #
# # data:  temporal_sub_all$sum_cd19dp
# # W = 0.83316, p-value = 0.00000000002076
#
# # mpg
# ggqqplot(temporal_sub_all$sum_gisaid, ylab = "sum_cd19dp")
# # wt
# z
# res <- cor.test(sum_gis_c19$sum_gisaid, sum_gis_c19$sum_cd19dp,
#                 method = "pearson")
# res
#
# b = ggscatter(temporal_sub_all_av %>% mutate(av_gisaid = log(av_gisaid), av_cd19dp = log(av_cd19dp)), x = "av_gisaid", y = "av_cd19dp",
#               add = "reg.line", conf.int = TRUE,
#               cor.coef = TRUE, cor.method = "pearson",
#               xlab = "Mean Weekly Sumbmissions to the Gisaid database", ylab = " Mean ENA")
#
#
# rm(gisaid, embl)
