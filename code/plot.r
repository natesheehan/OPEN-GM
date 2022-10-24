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

test = main_df |> filter(`Genomes per confirmed cases % (GISAID)` != Inf) |> filter(`Genomes per confirmed cases % (GISAID)` < 100)
m4 <- test %>%
  # convert state to factor and reverse order of levels
  mutate(country=factor(country, levels=rev(sort(unique(country))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(`Genomes per confirmed cases % (GISAID)`, breaks=c( 0, 1, 2, 3, 4, 5, max(`Genomes per confirmed cases % (GISAID)`, na.rm=T)),
                         labels=c("0-1", "1-2", "2-3", "3-4", "4-5", ">5"))) %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor), levels=rev(levels(countfactor))))

textcol = "black"
ggplot(m4 |> dplyr::filter(continent == "Europe"), aes(x=lubridate::ym(wy), y=country, fill=countfactor))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white", size=0.2)+
  guides(fill=guide_legend(title="% of Sequences\n Submitted by Cases"))+
  labs(x="Year/Week", y="Country", title="Landscape of Country based Sequence Sharing")+
  scale_fill_manual(values=c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#ddf1da"), na.value = "grey90")+
  scale_y_discrete(expand=c(0, 0))+
  scale_fill_manual(values=rev(brewer.pal(7, "Spectral")), na.value="grey90")+
  theme_grey(base_size=10)+
  labs(caption = "H. Wickham. ggplot2: Elegant Graphics for Data Analysis Springer-Verlag New York, 2009.") +
  # facet_grid(.~continent) +
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0, "cm")),
        legend.text=element_text(colour=textcol, size=10, face="bold"),
        legend.key.height=grid::unit(0.8, "cm"),
        legend.key.width=grid::unit(0.2, "cm"),
        axis.text.x=element_text(size=10, colour=textcol),
        axis.text.y=element_text(vjust=0.2, colour=textcol, size = 8),
        axis.ticks=element_line(size=0.4),
        axis.title = element_text(size = 10, face="bold"),
        panel.border=element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = "transparent",
                                        size = 0.5, linetype = "solid"),
        plot.background=element_rect(fill = "gray88"),
        legend.background = element_rect(fill = "gray88"),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title=element_text(colour=textcol, hjust=0, size=18, face="bold")
  )


