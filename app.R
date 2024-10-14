library(shiny)
library(shinybusy)
library(rinat)
library(dplyr)
library(leaflet)
library(leaflegend)
library(leaflet.extras)
library(raster)
library(readxl)
library(rgdal)
library(doParallel)
library(foreach)
library(httr)

# Define global variables and functions
max_obs <- 10000

# just data processing 
process_data <- function(place_input) {
  place_id <- convert_to_numeric_id(place_input)
  place_obs <- get_inat_obs(place_id = place_id, maxresults = max_obs)
  place_obs$introduced <- NA ## creating a new column for introduced species
  uni_species <- as.data.frame(unique(place_obs$scientific_name))
  n.cores <- parallel::detectCores() - 1 ## gotta be careful with this 
  my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
  doParallel::registerDoParallel(cl = my.cluster)
  
  uni_species$introduced <- foreach(i = 1:length(uni_species$`unique(place_obs$scientific_name)`), .combine = 'c') %dopar% { 
    url <- "https://inaturalist.org"
    places <- "/places.json?"
    taxon <- gsub(" ", "", paste("taxon=", gsub(" ", "+", uni_species$`unique(place_obs$scientific_name)`[i])))
    place_type <- "place_type=state"
    state <- "q=" 
    em <- "establishment_means=introduced"
    stat <- httr::GET(url, path = paste(places, paste(taxon, place_type, state, em, sep = "&"), sep = ""))
    stat <- httr::content(stat)
    
    if (length(stat) == 0) 0 else 1
  }
  
  parallel::stopCluster(my.cluster)
  
  colnames(uni_species) <- c("scientific name", "introduced")
  uni_species_in <- filter(uni_species, introduced == 1)
  
  invasives <- subset(place_obs, place_obs$scientific_name %in% uni_species_in$`scientific name`)
  invasives$year <- sapply(strsplit(invasives$datetime, "-"), `[`, 1)
  
  # Get coordinates from iNat API
  url <- paste("https://api.inaturalist.org/v1/places", place_input, sep = "/") ## getting the coordinates of the place from iNat
  gson <- httr::GET(url)
  geoson <- httr::content(gson)$results[[1]]$geometry_geojson$coordinates
  geoson <- unlist(geoson)
  geoson <- as.data.frame(geoson)
  row_dat <- seq_len(nrow(geoson)) %% 2
  coords <- data.frame(
    y = geoson$geoson[row_dat == 0],
    x = geoson$geoson[row_dat == 1]
  )
  
  # Load management data
  Invasives_Management <- read_xlsx("Data/species_individual_management_dataset.xlsx")
  
  # Prepare Map Data
  Map_Data <- invasives %>%
    filter(scientific_name %in% unique(Invasives_Management$species)) %>% ## filtering for only the species that are in the invasives management dataset
    merge(Invasives_Management, by.x = "scientific_name", by.y = "species") %>%
    rename(Species = scientific_name, Lat = latitude, Lon = longitude, Name = common, Year = year)
  
  Map_Data$"burn (y/n)"[Map_Data$"burn (y/n)" == "y"] <- "Yes"
  Map_Data$"burn (y/n)"[Map_Data$"burn (y/n)" == "n"] <- "No"
  
  Map_Split <- split(Map_Data, Map_Data$Name)
  Map_Year <- split(Map_Data, Map_Data$Year)
  
  return(list(Map_Data = Map_Data, Map_Split = Map_Split, Map_Year = Map_Year, coords = coords))
}

create_popups <- function(Map_dat) {
  paste(
    "<b>Common Name: </b>", Map_dat$Name, "<br>",
    "<b>Species: </b><i>", Map_dat$Species, "</i><br>",
    "<b>Location: </b>", Map_dat$Lat, ", ", Map_dat$Lon, "<br>",
    "<b>Year observed: </b>", Map_dat$Year, "<br>",
    "<b>Invasiveness: </b>", Map_dat$invasiveness, "<br>",
    "<b>Manual Removal: </b>", Map_dat$"how to remove", "in ", Map_dat$'when to remove', "<br>",
    "<b>Foliar Herbicide: </b>", Map_dat$"foliar herbicide", "in ", Map_dat$'when foliar herbicide', "<br>",
    "<b>Manageable by Perscribed Fire: </b>", Map_dat$burn, "<br>",
    "<b>Other Options: </b>", Map_dat$other
  )
}

create_map <- function(Map_Data, Map_Split, Map_Year, coords) {
  Map_Icons <- leaflet::awesomeIconList(
    "low" = makeAwesomeIcon(library = "ion", icon = "leaf", iconColor = "white", markerColor = "green"),
    "medium" = makeAwesomeIcon(library = "ion", icon = "leaf", iconColor = "white", markerColor = "orange"),
    "high" = makeAwesomeIcon(library = "ion", icon = "leaf", iconColor = "white", markerColor = "red")
  )
  
  Map <- leaflet(Map_Data) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    addPolygons(data = as.matrix(coords), color = "green", weight = 3, opacity = 1, fillColor = "green", fillOpacity = 0.2) %>%
    addMiniMap(position = "bottomleft") %>%
    addLegendAwesomeIcon(Map_Icons, title = "Invasiveness", position = "topright") %>%
    addScaleBar()
  
  # Add markers for species
  for (i in seq_along(Map_Split)) {
    Map_dat <- Map_Split[[i]]
    Popups <- create_popups(Map_dat)
    Map <- addAwesomeMarkers(Map, data = Map_dat, lng = ~Lon, lat = ~Lat, label = ~Name, popup = Popups, icon = ~Map_Icons[invasiveness], group = ~Name) 
  }
  
  # Add markers for years
  for (y in seq_along(Map_Year)) {
    Map_dat <- Map_Year[[y]]
    Popups <- create_popups(Map_dat)
    Map <- addAwesomeMarkers(Map, data = Map_dat, lng = ~Lon, lat = ~Lat, label = ~Name, popup = Popups, icon = ~Map_Icons[invasiveness], group = ~Year)
  }
  
  addLayersControl(Map, overlayGroups = c(unique(Map_Data$Name), unique(Map_Data$Year)))
}


convert_to_numeric_id <- function(input_string) {
  # Remove any non-digit characters and convert to numeric
  numeric_id <- as.numeric(gsub("\\D", "", input_string))
  if (is.na(numeric_id)) {
    stop("Invalid input. Please enter a valid place ID number.")
  }
  return(numeric_id)
}


# Define UI for application
ui <- fluidPage(
  titlePanel("Invasive Species Visualizer"),
  add_busy_spinner(spin = "fading-circle"),
  textInput("place_input", "Place ID (from iNat)?"),
  actionButton("submit", label = "Submit"),
  mainPanel(
    leafletOutput("Map")
  )
)


server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    # Process data
    withProgress(message = 'Processing data...', value = 0, {
      tryCatch({
        data(process_data(input$place_input))
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Create map
    output$Map <- renderLeaflet({
      req(data())
      create_map(data()$Map_Data, data()$Map_Split, data()$Map_Year, data()$coords)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)