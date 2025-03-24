# Load the necessary library
library(ggplot2)

# Example data: flux between levels, with multiple fluxes from the same source
flux_data <- data.frame(
  from = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5),
  to = c(2, 3, 4, 5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6),
  flux = c(5, 10, 8, 12, 15, 8, 6, 12, 7, 12, 7, 13, 9, 13, 10)
)

# Set the stack positions based on the source level
flux_data$stack_position <- flux_data$from

# Plot with stacked vertical arrows
ggplot(flux_data) +
  geom_segment(aes(x = stack_position, xend = stack_position, 
                   y = from, yend = to, 
                   size = flux),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               lineend = "round", linejoin = "round") +
  scale_size_continuous(range = c(0.5, 3)) +  # Arrow size proportional to flux
  theme_minimal() +
  labs(x = "Source Levels", y = "Tower Levels", 
       title = "Carbon Flux Between Tower Levels",
       size = "Flux") +
  theme(legend.position = "right") +
  scale_y_continuous(breaks = 1:6, limits = c(1, 6)) +  # Ensure proper y-axis breaks
  scale_x_continuous(breaks = 1:5, limits = c(1, 5))  # X-axis based on source levels
