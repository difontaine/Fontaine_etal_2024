# Fontaine etal. 2024
Associated code and input files used for Fontaine et al. manuscript published in Limnology and Oceanography. NPP rates (discrete and integrated) have been previsouly published on Environmental Data Iniative: https://doi.org/10.6073/pasta/95dde1f0c5bb3da0cac9bc47be2526b0

## Files:

Chapter2_FigureGeneration.R: Main script used to create figures for this manuscript. The figures are commented out based on figure name in manuscript

Get the "GEBCO_2023.nc" shapefile for the map from here: https://www.gebco.net/data_and_products/gridded_bathymetry_data/ and make sure to properly cite it. I used the 2023 version and make sure to download the "sub-ice topo/bathy" version.

en668_stations.csv: station list used in the map.

int_pp_corrected_011924.csv

pp_discrete_averaged_temp_filter_011924.csv

pp_depths.csv

integrated_chl_fromEDI_0524.csv")
stn_info <- read_csv("Processed_data_outputs/station_info.csv"

/integrated_nutrients_033024_euphotic.csv")

#need to get temperature data
temp_data <- read_csv("Processed_data_outputs/discrete_envi_data_all_bottles.csv"

"npp_mld_stations.csv" -- MLD data

incubation_light.csv

"buoyancy_freq.csv": buoy freq data

PLEMENTAL FIGURE 1: DARK CORRECTED NPP VS UNCORRECTED
pp_1 <- read_csv("NPP_data_package/For_EDI/npp_discrete_version1.csv")
pp_2 <- read_csv("NPP_data_package/For_EDI/npp_discrete_version2.csv") for dark corrected comparison plot

/pp_discrete_averaged_notemp_filter.csv") --- for discrete data temp check

_csv("historical_pp.csv"): historical data from marmap

Processed_data_outputs/nutrient_data_qced_033024.csv") nutrient data Qced from EDI
bottles_grouped_complete <- read_csv("niskin_groups_fixed.csv")
