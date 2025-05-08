# Global-variation-in-the-costs-and-ecological-benefits-of-tropical-natural-regeneration-
This repo contains the code used to analyse and map the spatial patterns of  cost and ecological benefits of natural tropical forest regeneration. The analysis categorises areas into 12 distinct groups based on cost levels and benefit combinations.

Repository Structure
code/: Contains all R scripts used for analysis and generating figures in the manuscript and appendices
data/: Contains resulting raster layers
Scripts
PNR_cost_benefit_categories.R: The main analysis script that processes input rasters, extracts values, calculates costs, and creates the 12-category classification system. This script generates the primary outputs used by all other scripts. It also creates Figure 2 in the manuscript and Appendix 1.

Fig3_bubble_plot.R: Creates a bubble plot comparing total natural regeneration potential against holistic hotspot area (Category 4) across countries. It creates Figure 3 in the manuscript.

Fig4_circular_barplot.R: Generates a circular barplot showing the distribution of all 12 categories across countries, organised by tropical realms (Neotropics, Afrotropics, and Indomalayan tropics). It creates Figure 4 in the manuscript.

sensitivity_opportunity_cost.R: Performs a sensitivity analysis by excluding land opportunity costs from the natural regeneration cost calculation, then recategorises areas using the same classification approach as the main analysis. It creates Appendix 3.

sensitivity_median.R: Conducts a sensitivity analysis using a different classification method for biodiversity and carbon. Instead of Jenks natural breaks, this script uses median values within each cost category to classify benefits as high or low. It creates Appendix 4.

Data Files
PNR_categories.tif: Main raster output containing the 12 cost-benefit categories
all_cat_barplot.csv: Preprocessed data for the circular barplot, with values transformed to log10 scale
sensitivity_median.tif: Raster output from the median-based classification sensitivity analysis
sensitivity_opp.tif: Raster output from the opportunity cost sensitivity analysis
Note on Data Availability: The input datasets used in this study are not included in this repo as they are extremely large and can be downloaded from the citations provided in the manuscript. All input datasets are publicly accessible. The data uploaded here represent the main results from this study. For additional results, please refer to the manuscript and its appendices.

Running the Code
The main workflow is:

First run PNR_cost_benefit_categories.R to generate the core classification
Then run Fig3_bubble_plot.R and Fig4_circular_barplot.R
If desired, run the sensitivity analysis scripts (sensitivity_opportunity_cost.R and sensitivity_median.R)

Citations
If using this code or results, please cite: https://doi.org/10.21203/rs.3.rs-6482620/v1
