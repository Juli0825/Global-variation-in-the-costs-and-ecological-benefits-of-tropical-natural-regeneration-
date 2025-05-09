# Circular barplot for visualising cost-benefit categories by country
# This script creates a circular barplot (Figure 4 in the manuscript) showing the distribution of 12 cost-benefit categories
# across different countries, organised by tropical realms.

#----------------------------------------------------------------------
# STEP 0: Data Source Information
#----------------------------------------------------------------------
# The input file "all_cat_barplot.csv" can be found in the data folder.
# This file is derived from "results/country_cat.xlsx" which was created 
# by PNR_cost_benefit_categories.R script.
#
# Preprocessing steps performed on the original country_cat.xlsx:
# 1. Values were transformed using log10(x/1000 + 1) scale to improve visualisation
# 2. Countries were arranged by tropical realms (Neotropics, Afrotropics, Indomalayan tropics)
#    according to the CONTINENT column
# 3. Within each realm, countries were ordered from smallest to largest total area
#    (sum of all 12 categories)
# 4. Column names were standardised to: individual, group, value1, value2, ..., value12
#    where "individual" = country name, "group" = realm name, and value1-12 = categories
# 5. Save the preprocessed Excel file into CSV format (comma-separated values) 

# Load necessary libraries
library(tidyverse)
library(viridis)

#----------------------------------------------------------------------
# STEP 1: Load and prepare the data
#----------------------------------------------------------------------
# Read data, excluding any unnamed columns
data <- read.csv("data/all_cat_barplot.csv", header = TRUE)[, 1:14]  # Only take first 14 columns

# Transform data from wide to long format for plotting
data <- data %>% gather(
  key = "observation", 
  value = "value", 
  c("value1", "value2", "value3", "value4", "value5",
    "value6", "value7", "value8", "value9", "value10", "value11", "value12"), 
  factor_key = TRUE
)

#----------------------------------------------------------------------
# STEP 2: Set up factor levels to maintain ordering
#----------------------------------------------------------------------
# Maintain grouping by tropical realms
data$group <- factor(data$group)

# Keep the manual ordering of countries (from smallest to largest area within each realm)
data$individual <- factor(data$individual, levels = unique(data$individual))

#----------------------------------------------------------------------
# STEP 3: Add empty bars to create spacing between tropical realms
#----------------------------------------------------------------------
empty_bar <- 3  # Number of empty bars between groups
nObsType <- nlevels(as.factor(data$observation))  # Number of categories (12)

# Create dataframe for empty spaces
to_add <- data.frame(matrix(NA, empty_bar * nlevels(data$group) * nObsType, ncol(data)))
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each = empty_bar * nObsType)

# Combine with main data and arrange
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)

# Add an id for each bar in the circular plot
data$id <- rep(seq(1, nrow(data) / nObsType), each = nObsType)

#----------------------------------------------------------------------
# STEP 4: Prepare label data for country names
#----------------------------------------------------------------------
# Calculate total height for each country for label positioning
label_data <- data %>% 
  group_by(id, individual) %>% 
  summarize(tot = sum(value, na.rm = TRUE))

# Calculate angle for each label to display around the circle
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar

# Adjust text alignment and angle for readability
label_data$hjust <- ifelse(angle < -90, 1, 0)  # Horizontal justification
label_data$angle <- ifelse(angle < -90, angle + 180, angle)  # Adjust text angle

#----------------------------------------------------------------------
# STEP 5: Define a custom colour palette for the 12 categories, which are the same as Figure 2 in the manuscript
#----------------------------------------------------------------------
value1 <- "#d3b7f1"  
value2 <- "#bc92d9"  
value3 <- "#be71a4" 
value4 <- "#a14bef"  
value5 <- "#bacde1"  
value6 <- "#5882b9"  
value7 <- "#233d7f"  
value8 <- "#000000"  
value9 <- "#faa9b9"  
value10 <- "#ff72a0" 
value11 <- "#f40550" 
value12 <- "#ae0002" 

#----------------------------------------------------------------------
# STEP 6: Create the circular barplot
#----------------------------------------------------------------------
p <- ggplot(data) +      
  geom_bar(aes(x = as.factor(id), y = value, fill = observation), stat = "identity") +
  scale_fill_manual(values = c(value1, value2, value3, value4, value5, value6,
                               value7, value8, value9, value10, value11, value12)) +
  ylim(-4, 50) +  # Set y-axis limits
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm") 
  ) +
  coord_polar() +  # Transform to polar coordinates for circular layout
  geom_text(
    data = label_data, 
    aes(x = id, y = tot + 2, label = individual, hjust = hjust), 
    color = "black", 
    fontface = "bold", 
    alpha = 0.7, 
    size = 1.8,  # Text size for country labels
    angle = label_data$angle, 
    inherit.aes = FALSE
  )

#----------------------------------------------------------------------
# STEP 7: Display the plot in R viewer
#----------------------------------------------------------------------
print(p)

#----------------------------------------------------------------------
# STEP 8: Save the plot as a high-resolution image
#----------------------------------------------------------------------
jpeg(filename = 'results/circular_barplot.jpg', height = 18, width = 18, units = c("in"), res = 1000)
print(p)
dev.off()
