library(ggthemes)

ggplot(Sites.Summary, aes(x = CHM.mean, y = LAI.mean)) +
  geom_hex(bins = 10) +
  scale_fill_viridis_c() +
  labs(x = "Canopy Height", y = "LAI")+
  theme_minimal()