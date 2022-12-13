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




ggarrange(a, b,v, ncol = 1, nrow=3 )

ggsave(
  paste0("plots/presentation/treemap.png"),
  dpi = 320,
  width = 18,
  height = 12,
  limitsize = FALSE
)
