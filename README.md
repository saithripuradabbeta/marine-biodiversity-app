# Interactive Marine Biodiversity Visualisation

A Shiny dashboard built for the Data Visualization Laboratory course project. It explores marine observation records by group, region, year, depth and location.

## Features

- Filters for group, region, year range and depth range
- Summary boxes: records, species, groups, mean depth
- Charts: records by group, yearly trends by region, group-by-region heat map, depth histogram, depth box plots, longitude/latitude map
- Searchable data table with CSV download of the filtered data
- Optional CSV upload (demo data is used when nothing is uploaded)

## Requirements

R 4.1 or later and these packages:

```r
install.packages(c("shiny", "shinydashboard", "dplyr", "tidyr",
                   "ggplot2", "DT", "scales"))
```

## Run

```r
shiny::runApp()
```

or open `app.R` in RStudio and click **Run App**.

## Uploading your own data

The CSV must contain these columns:

`scientificName, group, region, year, latitude, longitude, depth_m`

An optional `observation_count` column is also supported. Invalid years, coordinates and negative depths are set to missing (NA) during cleaning. When the year or depth slider is active, rows with a missing value in that column are excluded.

## Project structure

```
marine-biodiversity-app/
├── app.R
├── README.md
└── .gitignore
```
