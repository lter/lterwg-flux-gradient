

ridgeplot(
   data_file= min9.FG.WP.list,
   gas = 'CO2',
   EC_0_bin = 2,
   EC_flux_bins = 3,
   sd_scalar = 1,
   facet_heights = TRUE
 )

ggplot(min9.FG.WP.list, aes(x = CO2)) +
  geom_density_ridges(scale = 1, rel_min_height = 0.01) +
  theme_ridges() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) +
  labs(title = "CO2 Emissions from Min9.FG.WP.list",
       subtitle = "EC_0_bin = 2, EC_flux_bins = 3, sd_scalar = 1",
       caption = "Data from min9.FG.WP.list")
