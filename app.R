#

library(shiny)
library(shinybusy)
library("rinat")
library(dplyr)
library(leaflet)
library(leaflegend)
library(leaflet.extras)
library(raster)
library(readxl)
library(rgdal)

project_input <- "invasive-species-survey-for-brush-mountain"
place_input <- 184078


## the actual app

# Define UI for application that 
ui <- fluidPage(
    # Application title
    titlePanel("invasive species visualizer"),
    #textInput("project_input", "Name of project?"),
    add_busy_spinner(spin = "fading-circle"),
    textInput("place_input", "Place ID (from iNat)?"),
    # submit button
    actionButton("submit", label = "Submit"),
    
    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
       # sidebarPanel(
            #sliderInput("resources",
             #           "Estimated amount of resources (in USD? effort?):",
           #             min = 0,
       #                 max = 100,
       #                 value = 10)
     #   ),
        
      
        # Show a map of place w species and "invasivness"
        mainPanel(
        leafletOutput("Map")
          )
    )

# Define server to use project input and output resource f(x)
server <- function(input, output) {
  
  observeEvent(input$submit, {
    
  ## fixed 
    
  max_obs <- 10000
  place_obs <- get_inat_obs(place_id = place_input, maxresults = max_obs) # place id for brush mountain 3
  place_obs$introduced<-NA
  # just unique introduced sp. names
  uni_species <- as.data.frame(unique(place_obs$scientific_name))
  uni_species$introduced<-NA
  # we gotta find a way to speed this loop up
  for (t in 1:length(uni_species$`unique(place_obs$scientific_name)`)) {
    url <- "https://inaturalist.org"
    places <- "/places.json?"
    taxon <- gsub(" ", "", paste("taxon=",gsub(" ", "+", uni_species$`unique(place_obs$scientific_name)`[t])))
    place_type <- "place_type=state"
    state <- "q=" # specifiying location 
    em <- "establishment_means=introduced"
    stat<-httr::GET(url, path=paste(places, paste(taxon, place_type, state, em, sep = "&"), sep = ""))
    stat<-httr::content(stat)
    
    if (length(stat)==0) {
      uni_species$introduced[t]<-0
    } else {
      uni_species$introduced[t]<-1
    }
  }
  colnames(uni_species) <- c("scientific name", "introduced")
  uni_species_in <- filter(uni_species, introduced == 1)
  
  # filtering only for the introduced observations
  invasives <- subset(place_obs,place_obs$scientific_name %in% uni_species_in$`scientific name`)
  # just unique invasives
  uni_invasives <- unique(invasives$scientific_name[invasives$scientific_name != ""])
  # counting how many of each invasive sp
  how_many_sp <- list()
  for(i in 1:length(uni_invasives)){
    temp <- invasives[invasives$scientific_name == uni_invasives[[i]],]
    # prolly a better way to do this 
    how_many_sp[[i]] <- length(unique(temp$datetime))
  }
  how_many <- unlist(how_many_sp)
  # dataframe of invasive count
  in_final <- data.frame(uni_invasives, how_many)

# species geolocation (jordan)
  
  ## need to make this shapefile from the API and transformation
  
  Property_3<-raster::shapefile("Shapefiles/BrushMtn3PolyBuffered/BrushMtn3PolyBuffered.shp")
  Invasives_Management<-read_xlsx("Data/species_individual_management_dataset.xlsx")
  iNat_Locations <- data.frame(invasives$scientific_name, invasives$latitude, invasives$longitude)
  
  #Make Map Data
  Map_Data<-iNat_Locations %>% 
    filter(invasives.scientific_name %in% unique(Invasives_Management$species)) %>% 
    merge(Invasives_Management, by.x="invasives.scientific_name", by.y="species") %>% 
    rename(Species=invasives.scientific_name, Lat=invasives.latitude, Lon=invasives.longitude, Name=common)
  
  #Alter Burn Column Text
  Map_Data$"burn (y/n)"[Map_Data$"burn (y/n)"=="y"]<-"Yes"
  Map_Data$"burn (y/n)"[Map_Data$"burn (y/n)"=="n"]<-"No"
  
  #Make Map Icons
  Map_Icons<-awesomeIconList(
    "low"=makeAwesomeIcon(library="ion", icon="leaf", iconColor="white", markerColor="green"),
    "medium"=makeAwesomeIcon(library="ion", icon="leaf", iconColor="white", markerColor="orange"),
    "high"=makeAwesomeIcon(library="ion", icon="leaf", iconColor="white", markerColor="red"))
  
  #Make Map Popup Text
  Popups<-paste("<b>Common Name: </b>", Map_Data$Name, "<br>", "<b>Species: </b><i>", Map_Data$Species, "</i><br>", "<b>Location: </b>", Map_Data$Lat, ", ", Map_Data$Lon, "<br>", "<b>Invasiveness: </b>", Map_Data$invasiveness,
                "<br>", "<b>Manual Removal: </b>", Map_Data$"how to remove", "in ", Map_Data$'when to remove', "<br>", "<b>Foliar Herbicide: </b>", Map_Data$"foliar herbicide", "in ", Map_Data$'when foliar herbicide', "<br>", "<b>Manageable by Perscribed Fire: </b>", Map_Data$burn, "<br>", "<b>Other Options: </b>", Map_Data$other)
  #Make Map Label Text
  Labels<-paste(Map_Data$Name)
  #Split Map Data by Species
  Map_Split<-split(Map_Data, Map_Data$Name)
  #Make Base Map
  Map <-leaflet(Map_Data) %>% 
    addProviderTiles(providers$Esri.WorldTopoMap) %>% 
    addPolygons(data=Property_3, color="green", weight=3, opacity=1, fillColor="green", fillOpacity=0.2) %>% 
    addMiniMap(position="bottomleft") %>% 
    addLegendAwesomeIcon(Map_Icons, title="Invasiveness", position="topright") %>% 
    addScaleBar()
  
  #Add Layers of Markers to Map by Species
  for (i in 1:length(Map_Split)){
    Map<-Map %>% addAwesomeMarkers(data=Map_Split[i], lng=Map_Split[[i]][["Lon"]], lat=Map_Split[[i]][["Lat"]], label=Labels, popup=Popups, icon=Map_Icons[Map_Split[[i]][["invasiveness"]]], group=Map_Split[[i]][["Name"]][[1]])
  }
  Map <- Map %>%
    addLayersControl(overlayGroups=unique(Map_Data$Name))
  
  ## rendering the Map so it can be used by the ui
  output$Map <- renderLeaflet(Map)
  
  })}

# Run the application 
shinyApp(ui = ui, server = server)
