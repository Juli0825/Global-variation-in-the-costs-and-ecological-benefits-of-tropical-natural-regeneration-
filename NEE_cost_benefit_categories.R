# This script is for generating the cost and ecological benefit of natural regeneration potential categories

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
library(classInt)

# Define Mollweide CRS
mollweide_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

#----------------------------------------------------------------------
# STEP 1: Load and prepare potential for natural regeneration (PNR) data
#----------------------------------------------------------------------
# Load the potential for natural regeneration file
pnr_raster <- raster("data/predregen_natural_v1_1km_moll.tif")

# Convert PNR raster to points and create data frame
pnr_points <- rasterToPoints(pnr_raster, spatial = TRUE)
pnr_coord <- coordinates(pnr_points)
pnr_values <- as.data.frame(pnr_points)[, 1]
pnr_df <- data.frame(Longitude = pnr_coord[, 1], Latitude = pnr_coord[, 2], PNR_score = pnr_values)

# Remove cells with PNR_score of 0
pnr_df <- pnr_df[pnr_df$PNR_score != 0, ]
pnr_df_coords <- pnr_df[, 1:2]

#----------------------------------------------------------------------
# STEP 2: Process benefits variables (carbon and biodiversity)
#----------------------------------------------------------------------
# Process carbon sequestration data
carbon_raster <- raster("data/Benefits/Cook_Patton_Carbon/sequestration_rate__mean__aboveground__full_extent__Mg_C_ha_yr.tif")
carbon_raster_reprojected <- projectRaster(carbon_raster, crs = mollweide_crs, method = 'ngb')
carbon_raster_resamp <- resample(carbon_raster_reprojected, pnr_raster, method = 'bilinear')
pnr_df$carbon <- raster::extract(carbon_raster_resamp, pnr_df_coords)

# Process biodiversity data
bio_raster <- raster("data/Benefits/Combined_THR_SR_2024/Combined_THR_SR_2024.tif")
bio_rast_reprojected <- projectRaster(bio_raster, crs = mollweide_crs, method = 'ngb')
bio_raster_resamp <- resample(bio_rast_reprojected, pnr_raster, method = 'bilinear')
pnr_df$bio <- raster::extract(bio_raster_resamp, pnr_df_coords)

#----------------------------------------------------------------------
# STEP 3: Process feasibility variables (land and establishment costs)
#----------------------------------------------------------------------
# Process land opportunity cost
all_cost <- raster("data/Feasibility/Vincent_opportunity_cost/All_cost.tif")
all_cost_reprojected <- projectRaster(all_cost, crs = mollweide_crs, method = 'ngb')
all_cost_resamp <- resample(all_cost_reprojected, pnr_raster, method = 'bilinear')
pnr_df$landcosts <- raster::extract(all_cost_resamp, pnr_df_coords)

# Process establishment (restoration) cost
raster_stack <- stack("data/Feasibility/Vincent_establishment_cost/03_implementation_cost.tif")
estab_raster <- raster_stack[[1]]
estab_rast_reprojected <- projectRaster(estab_raster, crs = mollweide_crs, method = 'ngb')
estab_reprojected_resamp <- resample(estab_rast_reprojected, pnr_raster, method = 'bilinear')
pnr_df$establishment <- raster::extract(estab_reprojected_resamp, pnr_df_coords)

# Clean up the data frame - remove NA values
pnr_df <- na.omit(pnr_df)

# Calculate cost for natural regeneration
pnr_df$nrcosts <- ((1-pnr_df$PNR_score)*pnr_df$establishment) + pnr_df$landcosts

#----------------------------------------------------------------------
# STEP 4: Calculate natural breaks for biodiversity and carbon
#----------------------------------------------------------------------
# Calculate Jenks natural breaks for biodiversity with 2 classes
bio_jenks <- classIntervals(pnr_df$bio, n = 2, style = "jenks")
bio_break <- bio_jenks$brks[2]  # Middle breakpoint

# Calculate Jenks natural breaks for carbon with 2 classes
carbon_jenks <- classIntervals(pnr_df$carbon, n = 2, style = "jenks")
carbon_break <- carbon_jenks$brks[2]  # Middle breakpoint

# Save the break values for reference
cat("Biodiversity break value:", bio_break, "\n")
cat("Carbon break value:", carbon_break, "\n")

#----------------------------------------------------------------------
# STEP 5: Categorise data based on costs, biodiversity, and carbon
#----------------------------------------------------------------------
# Divide nrcost into 3 categories using quantiles (Low, Medium, High)
pnr_df <- pnr_df %>%
  mutate(
    nrcost_category = ntile(nrcosts, 3),
    nrcost_category = recode(nrcost_category, `1` = "Low", `2` = "Medium", `3` = "High")
  )

# Categorise biodiversity and carbon into low/high
pnr_df <- pnr_df %>%
  mutate(
    bio_category = if_else(bio <= bio_break, "Low", "High"),
    carbon_category = if_else(carbon <= carbon_break, "Low", "High"),
    
    # Create combined categories
    nrcostsub_category = case_when(
      bio_category == "Low" & carbon_category == "Low" ~ "Both Low",
      bio_category == "High" & carbon_category == "High" ~ "Both High",
      bio_category == "Low" & carbon_category == "High" ~ "Biodiversity Low, Carbon High",
      bio_category == "High" & carbon_category == "Low" ~ "Biodiversity High, Carbon Low"
    )
  )

# Define numeric codes for each combination
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
  select(-bio_category, -carbon_category) # Remove intermediate columns

# Ensure category_code is integer
pnr_df$category_code <- as.integer(pnr_df$category_code)

#----------------------------------------------------------------------
# STEP 6: Create and save the categorised raster
#----------------------------------------------------------------------
# Convert the dataframe to an sf object
pnr_sf <- st_as_sf(pnr_df, coords = c("Longitude", "Latitude"), crs = mollweide_crs)

# Define the raster template
raster_template <- raster(extent(pnr_sf), resolution = 884.8194, crs = mollweide_crs)

# Rasterise the data based on the category_code field
cnr_raster <- rasterize(pnr_sf, raster_template, field = "category_code") 

# Convert raster values to integer
cnr_raster <- calc(cnr_raster, fun = function(x) { as.integer(x) })

# Save the raster as a TIFF file
writeRaster(cnr_raster, filename = "results/new_cnr_natubreaks.tif", 
            format = "GTiff", datatype = "INT1U", overwrite = TRUE)

#----------------------------------------------------------------------
# STEP 7: Calculate statistics by country
#----------------------------------------------------------------------
# Load the shapefile with country and continent information
gdam <- st_read("data/Statistics/gdam.shp")

# Project shapefile to Mollweide
gadm_moll <- st_transform(gdam, crs = mollweide_crs)

# Calculate the area per raster cell (in km²)
res_x <- res(cnr_raster)[1]  # Resolution in X direction (meters)
res_y <- res(cnr_raster)[2]  # Resolution in Y direction (meters)
cell_area_km2 <- (res_x * res_y) / 1e6  # Convert square meters to square kilometers

# Create an empty data frame to store results
developingnrc_results <- data.frame(
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
    cell_count_list <- exact_extract(cnr_raster, country_polygon, function(values, coverage_fraction) {
      # Filter out NA values and count only valid cells for category 'i'
      valid_values <- values[!is.na(values)]  # Remove NAs
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
  developingnrc_results <- rbind(developingnrc_results, new_row)
}

# Save the results to an Excel file
write_xlsx(developingnrc_results, "results/natural_breaks_statistics.xlsx")