[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R version](https://img.shields.io/badge/R-4.2.1-blue.svg)](https://cran.r-project.org/)

## Wytham Woods NDVI Analysis
This project analyzes NDVI (Normalized Difference Vegetation Index) data collected from drone flights over Wytham Woods.

### Installation

Initialize and restore the renv environment:
```r
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()
```


This will automatically install all required packages with the correct versions as specified in the renv.lock file.


### Features
- Processes NDVI raster files from multiple drone flights
- Creates a hexagonal grid sampling system (50x50m) across the study site
- Extracts and analyzes NDVI mean and standard deviation values
- Generates time series visualizations of NDVI changes
- Produces spatial maps showing NDVI patterns across different flight dates
- Identifies areas with highest/lowest NDVI values and variability

### Notes
- The script automatically filters out data from May 13th, 2023 due to flight issues
- Analysis focuses on cells with the highest/lowest 5% NDVI values
- Standard deviation filtering (15%) is used to identify stable high-NDVI areas
