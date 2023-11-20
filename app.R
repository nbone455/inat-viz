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
library(doParallel)
library(foreach)

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
  n.cores <- parallel::detectCores() - 1
  
  my.cluster <- parallel::makeCluster(
    n.cores, 
    type = "PSOCK"
  )
  
  doParallel::registerDoParallel(cl = my.cluster)
  foreach::getDoParRegistered()
  foreach::getDoParWorkers()
  
  # we gotta find a way to speed this loop up
  uni_species$introduced<-foreach (i=1:length(uni_species$`unique(place_obs$scientific_name)`), .combine='c') %dopar% {
    url <- "https://inaturalist.org"
    places <- "/places.json?"
    taxon <- gsub(" ", "", paste("taxon=",gsub(" ", "+", uni_species$`unique(place_obs$scientific_name)`[i])))
    place_type <- "place_type=state"
    state <- "q=" # specifiying location 
    em <- "establishment_means=introduced"
    stat<-httr::GET(url, path=paste(places, paste(taxon, place_type, state, em, sep = "&"), sep = ""))
    stat<-httr::content(stat)
    
    if (length(stat)==0) {
      print(0)
    } else {
      print(1)
    }
  }
  stopCluster(my.cluster)
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

  
  invasives$year<-sapply(strsplit(invasives$datetime,"-"), `[`, 1)
# species geolocation (jordan)
  
  ## need to make this shapefile from the API and transformation
  
  Property_3<-raster::shapefile("Shapefiles/BrushMtn3PolyBuffered/BrushMtn3PolyBuffered.shp")
  Invasives_Management<-read_xlsx("Data/species_individual_management_dataset.xlsx")
  iNat_Locations <- data.frame(invasives$scientific_name, invasives$latitude, invasives$longitude, invasives$year)
  
  ## getting coordinates to make polygon from iNat api (can use for any place on iNaturalist when appropriate)
  ## should decide if we want to take out using the shapefile or if we want to implement an if else for place input?
  
  url<-paste("https://api.inaturalist.org/v1/places", place_input, sep = "/")
  gson<-httr::GET(url)
  geoson<-httr::content(gson)$results[[1]]$geometry_geojson$coordinates
  geoson<-unlist(geoson)
  geoson<-as.data.frame(geoson)
  row_dat<-seq_len(nrow(geoson))%%2
  coords<-as.data.frame(rep(NA, length(geoson$geoson)/2))
  colnames(coords)<-"y"
  coords$y<-geoson$geoson[row_dat==0]
  coords$x<-geoson$geoson[row_dat==1]
  coords<-coords[,2:1]
  
  #Make Map Data
  Map_Data<-iNat_Locations %>% 
    filter(invasives.scientific_name %in% unique(Invasives_Management$species)) %>% 
    merge(Invasives_Management, by.x="invasives.scientific_name", by.y="species") %>% 
    rename(Species=invasives.scientific_name, Lat=invasives.latitude, Lon=invasives.longitude, Name=common, Year=invasives.year)
  
  #Alter Burn Column Text
  Map_Data$"burn (y/n)"[Map_Data$"burn (y/n)"=="y"]<-"Yes"
  Map_Data$"burn (y/n)"[Map_Data$"burn (y/n)"=="n"]<-"No"
  
  #Make Map Icons
  Map_Icons<-leaflet::awesomeIconList(
    "low"=makeAwesomeIcon(library="ion", icon="leaf", iconColor="white", markerColor="green"),
    "medium"=makeAwesomeIcon(library="ion", icon="leaf", iconColor="white", markerColor="orange"),
    "high"=makeAwesomeIcon(library="ion", icon="leaf", iconColor="white", markerColor="red"))
  
  
  #Split Map Data by Species
  Map_Split<-split(Map_Data, Map_Data$Name)
  Map_Year<-split(Map_Data, Map_Data$Year)
  #Make Base Map
  #Map <-leaflet(Map_Data) %>% 
    #addProviderTiles(providers$Esri.WorldTopoMap) %>% 
    #addPolygons(data=Property_3, color="green", weight=3, opacity=1, fillColor="green", fillOpacity=0.2) %>% 
    #addMiniMap(position="bottomleft") %>% 
    #addLegendAwesomeIcon(Map_Icons, title="Invasiveness", position="topright") %>% 
    #addScaleBar()
  
   
  ## Base map using coordinates from iNat api (this is the only code that uses the shapefile and only code that needed to edited)
  Map <-leaflet(Map_Data) %>% 
    addProviderTiles(providers$Esri.WorldTopoMap) %>% 
    addPolygons(data=as.matrix(coords), color="green", weight=3, opacity=1, fillColor="green", fillOpacity=0.2) %>% 
    addMiniMap(position="bottomleft") %>% 
    addLegendAwesomeIcon(Map_Icons, title="Invasiveness", position="topright") %>% 
    addScaleBar()
  
  
  
  nameMarkers<-Map
  #Add Layers of Markers to Map by Species
  for (i in 1:length(Map_Split)){
    Map_dat<-Map_Split[[i]]
    Popups<-paste("<b>Common Name: </b>", Map_dat$Name, "<br>", "<b>Species: </b><i>", Map_dat$Species, "</i><br>", "<b>Location: </b>", Map_dat$Lat, ", ", Map_dat$Lon, "<br>", "<b>Year observed: </b>", Map_dat$Year, "<br>", "<b>Invasiveness: </b>", Map_dat$invasiveness,
                  "<br>", "<b>Manual Removal: </b>", Map_dat$"how to remove", "in ", Map_dat$'when to remove', "<br>", "<b>Foliar Herbicide: </b>", Map_dat$"foliar herbicide", "in ", Map_dat$'when foliar herbicide', "<br>", "<b>Manageable by Perscribed Fire: </b>", Map_dat$burn, "<br>", "<b>Other Options: </b>", Map_dat$other)
    
    nameMarkers<-leaflet::addAwesomeMarkers(nameMarkers, data=Map_dat, lng=Map_dat[["Lon"]], lat=Map_dat[["Lat"]], label=Map_dat[["Name"]], popup=Popups, icon=Map_Icons[Map_dat[["invasiveness"]]], group=Map_dat[["Name"]])
  }
  
  
  yearMarkers<-Map
  for (y in 1:length(Map_Year)){
    Map_dat<-Map_Year[[y]]
    Popups<-paste("<b>Common Name: </b>", Map_dat$Name, "<br>", "<b>Species: </b><i>", Map_dat$Species, "</i><br>", "<b>Location: </b>", Map_dat$Lat, ", ", Map_dat$Lon, "<br>", "<b>Year observed: </b>", Map_dat$Year, "<br>", "<b>Invasiveness: </b>", Map_dat$invasiveness,
                  "<br>", "<b>Manual Removal: </b>", Map_dat$"how to remove", "in ", Map_dat$'when to remove', "<br>", "<b>Foliar Herbicide: </b>", Map_dat$"foliar herbicide", "in ", Map_dat$'when foliar herbicide', "<br>", "<b>Manageable by Perscribed Fire: </b>", Map_dat$burn, "<br>", "<b>Other Options: </b>", Map_dat$other)
    
    yearMarkers<-leaflet::addAwesomeMarkers(yearMarkers, data=Map_dat, lng=Map_dat[["Lon"]], lat=Map_dat[["Lat"]], label=Map_dat[["Name"]], popup=Popups, icon=Map_Icons[Map_dat[["invasiveness"]]], group=Map_dat[["Year"]])
  }
  
  NameControl <- addLayersControl(nameMarkers, overlayGroups=unique(Map_Data$Name))
  YearControl <- addLayersControl(yearMarkers, overlayGroups=unique(Map_Data$Year))
  
  ## rendering the Map so it can be used by the ui
  output$Map <- renderLeaflet(NameControl)
  
  })}

# Run the application 
shinyApp(ui = ui, server = server)
