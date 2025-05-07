# Displacement Risk Analysis

**Overview**
This R script performs a multi-step analysis to quantify and visualize housing displacement risk across Indiana counties using ACS data and external eviction records. It:

1. Installs and loads required packages (`tidycensus`, `tidyverse`, `qs`, `tigris`, `sf`, `tmap`, `ggplot2`).
2. Sets up the Census API key for ACS data access.
3. Retrieves state- and county-level ACS variables (income, population, housing, race, age).
4. Merges ACS tables into a master dataset, computing proportions of children and seniors.
5. Processes an external eviction dataset (`.qs`), constructs county GEOIDs, and aggregates eviction counts and rates.
6. Merges eviction rates with ACS data, flags counties with high eviction risk (>5% renters).
7. Calculates "at-risk" populations by demographic groups and summarizes risk proportions.
8. Visualizes results via lollipop chart, histogram, and interactive county map.

**Dependencies**

* R packages: tidycensus, tidyverse, qs, tigris, sf, tmap, ggplot2
* A valid Census API key
* Eviction data file (`d5_case_aggregated.qs`) with `state_code` & `county_code` fields

**Usage**

```r
# 1. Install packages and load libraries
install.packages(c("tidycensus","tidyverse","qs","tigris","sf","tmap","ggplot2"))
library(tidycensus); library(tidyverse); library(qs); library(tigris); library(sf); library(tmap); library(ggplot2)

# 2. Set Census API key
census_api_key("YOUR_KEY", install = TRUE)

# 3. Run script: data retrieval, merging, and visualization
source("displacement_risk_analysis.R")
```

---

# NY County ACS Analysis

**Overview**
This R script retrieves and visualizes 2023 ACS data for New York counties, focusing on:

* Median household income
* Unemployment rate
* Rent burden categories (30–35%, 35–40%, 40–50%, 50%+)
* Poverty rate

It produces bar charts, scatter plots, and correlation statistics to explore relationships between income, unemployment, rent burden, and poverty.

**Dependencies**

* R packages: tidycensus, tidyverse, ggplot2, scales, gridExtra
* Census API key for ACS access

**Usage**

```r
# 1. Install packages and load libraries
install.packages(c("tidycensus","tidyverse","ggplot2","scales","gridExtra"))
library(tidycensus); library(tidyverse); library(ggplot2); library(scales); library(gridExtra)

# 2. Set Census API key
census_api_key("YOUR_KEY", install = TRUE)

# 3. Run steps:
#    - Retrieve ACS variables for NY counties
#    - Compute and visualize median income & unemployment
#    - Compute rent burden percentages and plot
#    - Retrieve poverty data and analyze correlations

source("ny_county_acs_analysis.R")
```


