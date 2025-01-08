library(shiny)
library(shinybusy)
library(rinat)
library(dplyr)
library(leaflet)
library(leaflegend)
library(leaflet.extras)
library(raster)
library(readxl)
library(doParallel)
library(foreach)
library(httr)
library(jsonlite)

# Define global variables and functions
max_obs <- 10000

#  function to search places
find_place_id <- function(place_name) {
  # Encode the place name for URL
  encoded_name <- URLencode(place_name)
  
  # Query the iNaturalist API
  url <- paste0("https://api.inaturalist.org/v1/places/autocomplete?q=", encoded_name)
  response <- GET(url)
  
  if (status_code(response) != 200) {
    stop("Error connecting to iNaturalist API")
  }
  
  results <- fromJSON(rawToChar(response$content))
  
  if (length(results$results) == 0) {
    return(NULL)
  }
  
  # Return the first matching place ID
  return(as.character(results$results$id[1]))
}

# Data processing function
process_data <- function(place_input) {
  place_id <- as.numeric(place_input)  # Modified to use ID directly
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
    "<b>Manageable by Perscribed Fire: </b>", Map_dat$"burn (y/n)", "<br>",
    "<b>Other Options: </b>", Map_dat$other
  )
}

# map function
create_map <- function(Map_Data, Map_Split, Map_Year, coords) {
  
  # Calculate center point from coords
  center_lat <- mean(coords$y)
  center_lon <- mean(coords$x)
  
  Map_Icons <- leaflet::awesomeIconList(
    "low" = makeAwesomeIcon(library = "ion", icon = "leaf", iconColor = "white", markerColor = "green"),
    "medium" = makeAwesomeIcon(library = "ion", icon = "leaf", iconColor = "white", markerColor = "orange"),
    "high" = makeAwesomeIcon(library = "ion", icon = "leaf", iconColor = "white", markerColor = "red")
  )
  
  Map <- leaflet(Map_Data) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    # Set view to center on the area with appropriate zoom
    setView(lng = center_lon, lat = center_lat, zoom = 13) %>%
    addPolygons(data = as.matrix(coords), 
                color = "green", 
                weight = 3, 
                opacity = 1, 
                fillColor = "green", 
                fillOpacity = 0.2) %>%
    addMiniMap(position = "bottomleft",
               toggleDisplay = TRUE) %>%
    addLegendAwesomeIcon(Map_Icons, 
                         title = "Invasiveness", 
                         position = "topright") %>%
    addScaleBar(position = "bottomright")
  
  # Add markers for species
  for (i in seq_along(Map_Split)) {
    Map_dat <- Map_Split[[i]]
    Popups <- create_popups(Map_dat)
    Map <- addAwesomeMarkers(Map, 
                             data = Map_dat, 
                             lng = ~Lon, 
                             lat = ~Lat, 
                             label = ~Name, 
                             popup = Popups, 
                             icon = ~Map_Icons[invasiveness], 
                             group = ~Name) 
  }
  
  # Add markers for years
  #for (y in seq_along(Map_Year)) {
  #  Map_dat <- Map_Year[[y]]
  #  Popups <- create_popups(Map_dat)
   # Map <- addAwesomeMarkers(Map, 
  #                           data = Map_dat, 
  #                           lng = ~Lon, 
  #                           lat = ~Lat, 
   #                          label = ~Name, 
  #                           popup = Popups, 
   #                          icon = ~Map_Icons[invasiveness], 
   #                          group = ~Year)
  #}
  
  addLayersControl(Map, 
                   overlayGroups = c(unique(Map_Data$Name), unique(Map_Data$Year)),
                   options = layersControlOptions(collapsed = FALSE))
}



# Modified server
server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)
  place_id <- reactiveVal(NULL)
  
  # Handle place name input
  observe({
    req(input$place_name)
    if (nchar(input$place_name) >= 3) { # Only search if at least 3 characters
      tryCatch({
        id <- find_place_id(input$place_name)
        place_id(id)
      }, error = function(e) {
        showNotification("Error searching for place", type = "error")
      })
    }
  })
  
  # Display found ID
  output$place_id <- renderText({
    if (!is.null(place_id())) {
      paste("Found place ID:", place_id())
    } else if (nchar(input$place_name) >= 3) {
      "No matching place found"
    }
  })
  
  # Handle submit button
  observeEvent(input$submit, {
    req(place_id())
    
    # Process data
    withProgress(message = 'Processing data...', value = 0, {
      tryCatch({
        data(process_data(place_id()))
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

#  UI with instructions and sources
ui <- fluidPage(
  titlePanel("iNaturalist Invasive Species Visualizer"),
  add_busy_spinner(spin = "fading-circle"),
  
  # Create a sidebar layout
  sidebarLayout(
    # Put inputs in the sidebar
    sidebarPanel(
      width = 3,
      textInput("place_name", "Enter place name (e.g., 'Brush Mountain 3', 'Blacksburg')"),
      verbatimTextOutput("place_id"),
      actionButton("submit", label = "Submit", class = "btn-primary btn-block"),
      
      # Add instructions
      tags$hr(),
      tags$div(
        class = "instructions",
        tags$h4("How to Use This Map"),
        tags$ul(
          tags$li("Hover over any marker to see species information"),
          tags$li("Click markers for detailed management suggestions from literature"),
          tags$li("Use the checkboxes on the right to show/hide specific species"),
          tags$li("Colors indicate invasiveness level (see legend)"),
          tags$li("All recommendations are based on Virginia ecosystems")        )
      ),
      
      # Add sources section
      tags$hr(),
      tags$div(
        class = "sources",
        tags$h4("Data Sources"),
        tags$small(
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Source"), 
                tags$th("Information Used")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Flora of Virginia (Weakley et al. 2012)"), 
                tags$td("Species Latin and common names")
              ),
              tags$tr(
                tags$td("Virginia Invasive Plant Species List (Heffernan et al. 2014)"), 
                tags$td("Virginia invasiveness rankings")
              ),
              tags$tr(
                tags$td("Management Guide for Invasive Plants in Southern Forests (Miller et al. 2013)"), 
                tags$td("Management strategies for most species")
              ),
              tags$tr(
                tags$td("Invasive Plants in Virginia Weed Report (DiTomaso et al. 2013)"), 
                tags$td("Smartweed management")
              ),
              tags$tr(
                tags$td("Weed of the Week: Wineberry (Forest Health Staff 2005)"), 
                tags$td("Wineberry management")
              ),
              tags$tr(
                tags$td("Plant Invaders of Mid-Atlantic Natural Areas (Swearingen et al. 2010)"), 
                tags$td("Ground Ivy management")
              )
            )
          ),
          tags$p(class = "mt-2", 
                 "Management suggestions are derived from published literature and should be verified with local experts before implementation.")
        )
      )
    ),
    
    # Main panel with larger map
    mainPanel(
      width = 9,
      # Set map height with CSS
      tags$style(type = "text/css", 
                 "#Map {height: calc(100vh - 100px) !important;}
                .instructions { font-size: 0.9em; }
                .sources { font-size: 0.85em; }
                .table-sm td, .table-sm th { padding: 0.3rem; }
                "),
      leafletOutput("Map")
    )
  )
)
# Run the application 
shinyApp(ui = ui, server = server)