# iNat-Viz: Invasive Species Visualizer

## Table of Contents
1. [Overview](#overview)
2. [Dependencies](#dependencies)
3. [Setup](#setup)
4. [Features](#features)
5. [Usage](#usage)
6. [Contact](#contact)

## Overview
The `iNat-Viz` is a Shiny application designed to visualize invasive species data from the iNaturalist platform. The application incorporates geographic, taxonomic, and management information to provide a comprehensive view of invasive species in a specified location. 

## intent
this app was created as part of a Interfaces of Global Change capstone project by graduate students Bailey Howell, Jordan Coscia, Nic Bone and Forde Upshur, the Invasive Species Working group at Virginia Tech, and the Town of Blacksburg.

## Dependencies
- shiny
- shinybusy
- rinat
- dplyr
- leaflet
- leaflegend
- leaflet.extras
- raster
- readxl
- rgdal

To install all dependencies, run the following:
```R
install.packages(c("shiny", "shinybusy", "rinat", "dplyr", "leaflet", "leaflegend", "leaflet.extras", "raster", "readxl", "rgdal"))
```

## Setup
1. Clone this repository.
2. Open the app.R file in RStudio.
3. Run the Shiny app.

## Features
- **Interactive Map**: Visualizes the locations of invasive species with markers colored by degree of invasiveness.
- **Popup Information**: Clicking on a marker reveals additional information such as species name, invasiveness level, and management strategies.
- **Layer Control**: Allows the user to toggle the visibility of different species on the map.
- **Dynamic Query**: Ability to input iNaturalist Place ID to visualize different locations.

## Usage
- Input the iNaturalist Place ID for the location you want to explore.
- Click the "Submit" button to render the map.
- Use the layer control to toggle the visibility of different species on the map.
- Click on markers to view more details about each species.

## Contact
For more information or troubleshooting, feel free to contact Bailey Howell (bkhowell@vt.edu) or Nic Bone (nicbone@vt.edu).
