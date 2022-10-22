
ggplot(main_df, aes(x=lubridate::ym(wy), y=country, fill=cases))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white", size=0.2)+
  guides(fill=guide_legend(title="Cases per\n100,000 people"))+
  labs(x="", y="", title="Incidence of Covid-19 around the Globe")+
  theme_grey(base_size=10)+
  theme(legend.position="right", legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0, "cm")),
        legend.text=element_text(colour=textcol, size=7, face="bold"),
        legend.key.height=grid::unit(0.8, "cm"),
        legend.key.width=grid::unit(0.2, "cm"),
        axis.text.x=element_text(size=10, colour=textcol),
        axis.text.y=element_text(vjust=0.2, colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title=element_text(colour=textcol, hjust=0, size=14, face="bold")
  )



ggplot(main_df, aes(x=wy, y=country, fill=Genomes per confirmed cases (GISAID)))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white", size=0.2)+
  guides(fill=guide_legend(title="Cases per\n100,000 people"))+
  labs(x="", y="", title="Incidence of Covid-19 around the Globe")+
  scale_y_di

#coord_fixed()+
theme_grey(base_size=10)+
  theme(legend.position="right", legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0, "cm")),
        legend.text=element_text(colour=textcol, size=7, face="bold"),
        legend.key.height=grid::unit(0.8, "cm"),
        legend.key.width=grid::unit(0.2, "cm"),
        axis.text.x=element_text(size=10, colour=textcol),
        axis.text.y=element_text(vjust=0.2, colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title=element_text(colour=textcol, hjust=0, size=14, face="bold")
  )
