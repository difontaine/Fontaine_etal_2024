# Fontaine etal. 2024
Associated code and input files used for Fontaine et al. manuscript published in Limnology and Oceanography. NPP rates (discrete and integrated) have been previsouly published on Environmental Data Iniative (EDI): https://doi.org/10.6073/pasta/95dde1f0c5bb3da0cac9bc47be2526b0

## Files:

Chapter2_FigureGeneration.R: Main script used to create figures for this manuscript. The figures are commented out based on figure name in manuscript

### 1. Map files

1.1 Get the "GEBCO_2023.nc" shapefile for the map from here: https://www.gebco.net/data_and_products/gridded_bathymetry_data/ and select the "sub-ice topo/bathy" version. If using for publication, make sure to properly cite it. I used the 2023 version for this manuscript. 

1.2 en668_stations.csv: station list to get latitude/longitude for points in map.

### 2. Quality control input data

2.1 int_pp_corrected_011924.csv: Integrated NPP rates after quality control steps outlined in Supplemental Info of manuscript.

2.2 pp_discrete_averaged_temp_filter_011924.csv: Discrete NPP rates after quality control steps outlined in Supplemental Info of manuscript.

2.3 integrated_chl_fromEDI_0524.csv: Integrated chlorophyll-_a_data after quality control steps outlined in Supplemental Info of manuscript.

2.4 integrated_nutrients_033024_euphotic.csv: Euphotic-zone integrated nutrient concentrations after quality control.

2.5 nutrient_data_qced_033024.csv: Discrete nutrient concentrations after quality control.

### 3. Environmental Data 

3.1 discrete_envi_data_all_bottles.csv: Discrete environmental data compiled from bottle files

3.2 npp_mld_stations.csv: Mixed-layer depth data by cruise/cast

3.3 incubation_light.csv: Average PAR per incubation

3.4 buoyancy_freq.csv: Buoyancy frequency data

### 4. Other files needed for data merging, etc.

4.1 pp_depths.csv: Depth categories for each cruise/cast combination

4.2 station_info.csv: Associated stations for each cruise/cast

4.3 niskin_groups_fixed.csv: Depth group categories for each cruise/cast/niskin record to make merging with other data sources possible.

### 5. Other data sources needed for supplemental figures

5.1: npp_discrete_version1.csv and npp_discrete_version2.csv: These come directly from EDI and are for the dark-corrected NPP rate versus Non-corrected NPP rate plot. 

5.2 pp_discrete_averaged_notemp_filter.csv: Discrete NPP rate dataset that includes samples incubated at temperatures outside of the deltaT range.

5.3 historical_pp.csv: Previsouly collected NPP rate data obtained from http://orca.science.oregonstate.edu/field.data.c14.online.php

5.4marmap_data.txt: All MARMAP data *** the historical file and marmap file were cross-checked to make sure there were not duplicate data entries ***

