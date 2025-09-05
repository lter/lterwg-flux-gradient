library(tidyverse)

Sites.Summary$LAI.se <- Sites.Summary$LAI.sd / sqrt(Sites.Summary$LAI.years)
Sites.Summary$CHM.se <- Sites.Summary$CHM.sd / sqrt(Sites.Summary$CHM.years)


library(ggrepel)

# Plot
ggplot(Sites.Summary, aes(x = LAI.mean, y = CHM.mean, color = NDVI.mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = CHM.mean - CHM.se, ymax = CHM.mean + CHM.se), width = 0.01, alpha=0.5) +
  geom_errorbarh(aes(xmin = LAI.mean - LAI.se, xmax = LAI.mean + LAI.se), height = 0.01, alpha=0.5) +
  geom_text_repel(aes(label = site), size = 3) +
  theme_classic() +
  labs(
    x = "LAI Mean",
    y = "CHM Mean",
    color = "NDVI Mean"
  )