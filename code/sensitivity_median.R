# Sensitivity analysis using median-based classification for divided biodiversity and carbon values
# This script examines the effect of using median-based benefit classification instead of natural breaks

# Load required libraries
library(terra)
library(scales)
library(raster)
library(sp)
library(dplyr)
library(sf)
library(ggplot2)
library(exactextractr)
library(writexl)

#----------------------------------------------------------------------
# STEP 1: Load preprocessed data 
#----------------------------------------------------------------------
# This step loads a preprocessed RData file containing the cost and benefit of natural regeneration potential dataframe.
# The complete data preparation workflow (raster processing, variable extraction, etc.) 
# can be found in PNR_cost_benefit_categories.R script

# Load the saved data file with preprocessed variables
load("results/pnr_df.RData")

# Define Mollweide CRS for consistent projection
mollweide_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

#----------------------------------------------------------------------
# STEP 2: Calculate natural regeneration costs
#----------------------------------------------------------------------
# Uses the same cost calculation as the main script
pnr_df$nrcosts <- ((1-pnr_df$PNR_score)*pnr_df$establishment) + pnr_df$landcosts

#----------------------------------------------------------------------
# STEP 3: Categorise data using median-based classification
#----------------------------------------------------------------------
# Divide nrcosts into 3 quantile-based categories (Low, Medium, High)
pnr_df <- pnr_df %>%
  mutate(
    nrcost_category = ntile(nrcosts, 3),  # Create 3 quantile bins
    nrcost_category = recode(nrcost_category, `1` = "Low", `2` = "Medium", `3` = "High")
  )

# Calculate the median biodiversity and carbon within each nrcost_category
nrcost_medians <- pnr_df %>%
  group_by(nrcost_category) %>%
  summarise(
    median_bio = median(bio, na.rm = TRUE),
    median_carbon = median(carbon, na.rm = TRUE)
  )

# Join the medians back to the original dataframe
pnr_df <- pnr_df %>%
  left_join(nrcost_medians, by = "nrcost_category")

# Categorise biodiversity and carbon based on the median values within each nrcost_category
pnr_df <- pnr_df %>%
  mutate(
    bio_category = if_else(bio <= median_bio, "Low", "High"),
    carbon_category = if_else(carbon <= median_carbon, "Low", "High"),
    
    # Create combined categories for bio and carbon
    nrcostsub_category = case_when(
      bio_category == "Low" & carbon_category == "Low" ~ "Both Low",
      bio_category == "High" & carbon_category == "High" ~ "Both High",
      bio_category == "Low" & carbon_category == "High" ~ "Biodiversity Low, Carbon High",
      bio_category == "High" & carbon_category == "Low" ~ "Biodiversity High, Carbon Low"
    )
  )

#----------------------------------------------------------------------
# STEP 4: Create category codes and rasterise
#----------------------------------------------------------------------
# Define numeric codes for each combination of cost category and benefit subcategory
pnr_df <- pnr_df %>%
  mutate(
    category_code = case_when(
      nrcost_category == "Low" & nrcostsub_category == "Both Low" ~ 1,
      nrcost_category == "Low" & nrcostsub_category == "Biodiversity Low, Carbon High" ~ 2,
      nrcost_category == "Low" & nrcostsub_category == "Biodiversity High, Carbon Low" ~ 3,
      nrcost_category == "Low" & nrcostsub_category == "Both High" ~ 4,
      nrcost_category == "Medium" & nrcostsub_category == "Both Low" ~ 5,
      nrcost_category == "Medium" & nrcostsub_category == "Biodiversity Low, Carbon High" ~ 6,
      nrcost_category == "Medium" & nrcostsub_category == "Biodiversity High, Carbon Low" ~ 7,
      nrcost_category == "Medium" & nrcostsub_category == "Both High" ~ 8,
      nrcost_category == "High" & nrcostsub_category == "Both Low" ~ 9,
      nrcost_category == "High" & nrcostsub_category == "Biodiversity Low, Carbon High" ~ 10,
      nrcost_category == "High" & nrcostsub_category == "Biodiversity High, Carbon Low" ~ 11,
      nrcost_category == "High" & nrcostsub_category == "Both High" ~ 12
    )
  ) %>%
  # Remove intermediate columns but keep median values for reference
  select(-bio_category, -carbon_category)

# Convert category_code to integer
pnr_df$category_code <- as.integer(pnr_df$category_code)

# Convert the dataframe to an sf object
pnr_sf <- st_as_sf(pnr_df, coords = c("Longitude", "Latitude"), crs = mollweide_crs)

# Define the raster template
raster_template <- raster(extent(pnr_sf), resolution = 884.8194, crs = mollweide_crs)

# Rasterise the data based on the category_code field
median_raster <- rasterize(pnr_sf, raster_template, field = "category_code") 

# Ensure integer values
median_raster <- calc(median_raster, fun = function(x) { as.integer(x) })

# Save the raster
writeRaster(median_raster, filename = "results/median_classification.tif", 
            format = "GTiff", datatype = "INT1U", overwrite = TRUE)

#----------------------------------------------------------------------
# STEP 5: Calculate country-level statistics
#----------------------------------------------------------------------
# Load country boundaries
gdam <- st_read("data/Statistics/gdam.shp")

# Record start time for performance tracking
start_time <- Sys.time()
cat("Starting country statistics calculation at:", as.character(start_time), "\n")

# Project shapefile to Mollweide
gadm_moll <- st_transform(gdam, crs = mollweide_crs)

# Calculate the area per raster cell (in km²)
res_x <- res(median_raster)[1]  # Resolution in X direction (meters)
res_y <- res(median_raster)[2]  # Resolution in Y direction (meters)
cell_area_km2 <- (res_x * res_y) / 1e6  # Convert square meters to square kilometers

# Create an empty data frame to store results
median_results <- data.frame(
  GID_0 = character(), 
  NAME_0 = character(), 
  CONTINENT = character(), 
  Category_1_km2 = numeric(), 
  Category_2_km2 = numeric(),
  Category_3_km2 = numeric(), 
  Category_4_km2 = numeric(),
  Category_5_km2 = numeric(), 
  Category_6_km2 = numeric(),
  Category_7_km2 = numeric(), 
  Category_8_km2 = numeric(),
  Category_9_km2 = numeric(), 
  Category_10_km2 = numeric(),
  Category_11_km2 = numeric(), 
  Category_12_km2 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each country in the GADM shapefile
for (x in 1:nrow(gadm_moll)) {
  # Extract the geometry for each country
  country_polygon <- st_geometry(gadm_moll[x, ])
  
  # Initialise a vector to store the area for each category (1 to 12)
  category_areas <- numeric(12)
  
  # Loop through each category value in the raster (1 to 12)
  for (i in 1:12) {
    # Use exact_extract to calculate the area for the given category (i) within each country
    cell_count_list <- exact_extract(median_raster, country_polygon, function(values, coverage_fraction) {
      # Filter out NA values and count only valid cells for category 'i'
      valid_values <- values[!is.na(values)]
      return(sum(valid_values == i))
    })
    
    # Sum the results from all polygons
    total_cell_count <- sum(unlist(cell_count_list))
    
    # Calculate the total area in km² for the category
    category_areas[i] <- total_cell_count * cell_area_km2
  }
  
  # Append the result to the results data frame
  new_row <- data.frame(
    GID_0 = gadm_moll$GID_0[x],
    NAME_0 = gadm_moll$NAME_0[x],
    CONTINENT = gadm_moll$CONTINENT[x],
    Category_1_km2 = category_areas[1],
    Category_2_km2 = category_areas[2],
    Category_3_km2 = category_areas[3],
    Category_4_km2 = category_areas[4],
    Category_5_km2 = category_areas[5],
    Category_6_km2 = category_areas[6],
    Category_7_km2 = category_areas[7],
    Category_8_km2 = category_areas[8],
    Category_9_km2 = category_areas[9],
    Category_10_km2 = category_areas[10],
    Category_11_km2 = category_areas[11],
    Category_12_km2 = category_areas[12]
  )
  
  # Append to the final results data frame
  median_results <- rbind(median_results, new_row)
}

# Save the results to an Excel file
write_xlsx(median_results, "results/median_classification_statistics.xlsx")

# Record end time
end_time <- Sys.time()
cat("Finished country statistics calculation at:", as.character(end_time), "\n")
cat("Total time:", difftime(end_time, start_time, units = "mins"), "minutes\n")
