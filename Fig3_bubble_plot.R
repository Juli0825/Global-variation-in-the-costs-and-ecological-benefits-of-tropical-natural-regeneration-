# Bubble plot for total natural regeneration area vs holistic hotspots area
# This script creates a bubble plot (Figure 3 in the manuscript) showing the relationship between total natural regeneration 
# potential and holistic hotspot areas across countries. Both axes use log10 scale for visualisation.
# Bubble size represents the proportion of holistic hotspots in total potential natural regeneration area.

#----------------------------------------------------------------------
# STEP 0: Data Source Information
#----------------------------------------------------------------------
# The input file "bubble.csv" is derived from "results/country_cat.xlsx" which was created 
# using the main analysis in PNR_cost_benefit_categories.R script.
#
# Preprocessing steps:
# 1. Added a column called 'total_area' which is the sum of all 12 categories for each country
# 2. Converted to CSV format

# Load necessary libraries
library(ggplot2) 
library(tidyverse)
library(viridis)
library(ggrepel)

#----------------------------------------------------------------------
# STEP 1: Load and prepare the data
#----------------------------------------------------------------------
# Load the cleaned CSV file
cleaned_lmics <- read_csv("data/bubble.csv")

# Calculate additional columns for analysis and visualisation
cleaned_lmics <- cleaned_lmics %>%
  mutate(
    # Calculate proportion of Category 4 (holistic hotspots - high benefit, low cost) 
    proportion_category_4 = Category_4_km2 / total_area * 100,
    # Convert to thousands of km² for more readable scales
    total_km2x1000 = total_area / 1000,
    # Convert proportion to decimal for bubble sizing
    prop_cat4x100 = proportion_category_4 / 100,
    # Convert Category 4 to thousands of km²
    cat4x1000 = Category_4_km2 / 1000
  )

# Filter countries with less than 100 km² of holistic hotspot area (Cat4)
filtered_lmics <- cleaned_lmics %>%
  filter(Category_4_km2 >= 100)

#----------------------------------------------------------------------
# STEP 2: Create the bubble plot
#----------------------------------------------------------------------
bubble <- ggplot(filtered_lmics, 
                 aes(x = total_km2x1000, 
                     y = cat4x1000, 
                     size = proportion_category_4, 
                     label = NAME_0)) +
  # Set axis limits (using log scale)
  coord_cartesian(xlim = c(.5, 2500), ylim = c(.1, 400), clip = 'on') +
  
  # Create bubbles with dark green borders and lighter fill
  geom_point(shape = 21, 
             colour = "darkgreen", 
             fill = '#008000', 
             stroke = 1.2, 
             alpha = 0.7) +
  
  # Scale size based on area, remove legend for size
  scale_size_area(max_size = 14, guide = "none") +
  
  # Add country name labels
  geom_text(size = 3, vjust = 1.5, hjust = 1) +
  
  # Use logarithmic scale for x-axis with appropriate breaks
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::label_number()) +
  
  # Use logarithmic scale for y-axis with appropriate breaks
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::label_number()) +
  
  # Add logarithmic tick marks
  annotation_logticks() +
  
  # Use black-and-white theme for cleaner appearance
  theme_bw() +
  
  # Set plot labels
  labs(
    title = "Total natural regeneration potential versus holistic hotspot (countries having >100 km² holistic hotspot)",
    x = "Total potential for natural regeneration area (km²/1000)", 
    y = "Holistic hotspot area (km²/1000)"
  ) +
  
  # Format text elements
  theme(
    plot.title = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "right"
  )

#----------------------------------------------------------------------
# STEP 3: Display and save the plot
#----------------------------------------------------------------------
# Display the plot in R viewer
print(bubble)

# Save as high-resolution PDF for further editing if needed
ggsave("results/bubble_plot.pdf", plot = bubble, width = 8, height = 6, units = "in", dpi = 400)
