# Create the plot function
create_scatterplot <- function(data, selected_species, selected_island) {
  # Filter the data for specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species, island == selected_island)
  # Create scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
  ) +
    geom_point() +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = paste("Penguin Bill Dimensions -", selected_species, "-", selected_island)
    )

  return(plot)
}

# Color the plot by species (without island)
color_scatterplot <- function(data, selected_species, color) {
  # Filter the data for specified species
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species)

  # Create scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm)
  ) +
    geom_point(color = color) +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = selected_species
    ) +
    theme(legend.position = "none")

  return(plot)
}

# Create custom theme
yan_holtz_theme <- function() {
  theme_ipsum() +
    theme(
      plot.title = element_text(color = "#69b3a2", size = 18, face = "bold"),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7)
    )
}
