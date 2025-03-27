# Select relevant columns for heatmap
aop_matrix <- Sites.Summary %>%
  select(site, NDVI.mean, CHM.mean, EVI.mean, LAI.mean, PRI.mean, SAVI.mean) %>%
  column_to_rownames("site") %>%
  scale()  # z-score

# Convert to long for ggplot heatmap
aop_long <- as.data.frame(aop_matrix) %>%
  rownames_to_column("site") %>%
  pivot_longer(-site, names_to = "metric", values_to = "value")

ggplot(aop_long, aes(x = metric, y = site, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "AOP Site Summary Heatmap", x = "Metric", y = "Site")