library(tidyverse)
library(factoextra)  # for PCA + clustering viz
library(ggplot2)

# Select structure variables
structure_vars <- Sites.Summary %>%
  select(site, CHM.mean, CHM.sd, LAI.mean, LAI.sd) %>%
  drop_na()

# Scale and run PCA
structure_scaled <- structure_vars %>%
  column_to_rownames("site") %>%
  scale()

pca_result <- prcomp(structure_scaled, center = TRUE, scale. = TRUE)

# Choose # clusters (elbow method)
fviz_nbclust(structure_scaled, kmeans, method = "wss")

# Run clustering (e.g., k = 3)
set.seed(123)
km_res <- kmeans(structure_scaled, centers = 3, nstart = 25)

# Visualize PCA with clusters
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = factor(km_res$cluster),
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Cluster") +
  labs(title = "PCA of Canopy Structure with K-means Clustering")