parking_model_mapping<- function(instance_name, project_list_instance, project_list) {
  
  # Tutorial --------------------------------------------------------------------------------------------------------------------
  #http://spatioanalytics.com/2013/07/12/throw-some-throw-some-stats-on-that-map-part-1/
  #http://www.r-bloggers.com/shapefile-polygons-plotted-on-google-maps-using-ggmap-in-r-throw-some-throw-some-stats-on-that-mappart-2/
  
  # Clear workspace -------------------------------------------------------------------------------------------------------------
  # rm(list = ls()) 
  
  print(paste("----------Mapping for instance:", instance_name))
  
  # Prepare functions  ----------------------------------------------------------------------------------------------------------
  clean_text <- function(df,col_name) {
    trim <- function (x) gsub(" ", "", x)
    df[,c(col_name)] <- as.character(df[,c(col_name)]) # change factor to character
    for (i in 1:nrow(df)) {df[,c(col_name)][i] <- str_replace_all(df[,c(col_name)][i], "[[:punct:]]", " ")} # remove non-alphanums
    df[,c(col_name)] <- trim(df[,c(col_name)]) # remove all spaces
    return(df)
  }
  
  # Load packages ---------------------------------------------------------------------------------------------------------------
  require(openxlsx)
  require(ggmap)
  require(rgdal)
  require(ggplot2)
  require(RgoogleMaps)
  require(dtplyr)
  require(dplyr)
  require(stringr)
  require(Hmisc)
  require(latticeExtra)
  require(sp)
  source("map_parking_conditions.R")
  
  # Setup basemap ---------------------------------------------------------------------------------------------------------------
  CenterOfMap <- geocode("37.427688, -122.170020")
  Stanford <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 14, maptype = "roadmap", source = "google", color="bw")
  basemap <- ggmap(Stanford)
  
  # Set parameters  -------------------------------------------------------------------------------------------------------------
  path2shape_bldg <- c("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\R","Shape_Buildings_2016_07_29")
  path2shape_pkng <- c("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\R","Shape_Parking_2016_08_05_ovalcut")
  path2demand_bldg <- "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg4mapping_all.csv"
  path2demand_pkng <- "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng4mapping_all.csv"
  bldgXpkng4mapping <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="bldgXpkng4mapping")
  mapcolors <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="mapcolors")
  shape2bldg <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="shape2bldg")
  
  pkng_breaks <- c(0,0.0001,0.40,0.55,0.70,0.85,1)
  pkng_labels <- c("  Non-Applicable Lot","  00%- 40%","  41%- 55%","  56%- 70%","  71%- 85%","  85%-100%")
  pkng_color_palette <- mapcolors$color[mapcolors$group=="pkng"]
  path2demand_pkng <- path2demand_pkng
  pkng_demand_info <- bldgXpkng4mapping[grep("pkng_all",bldgXpkng4mapping$group),c("field_name","text_title")]
  
  bldg_breaks <- c(0,1,25,50,75,100,10000)
  bldg_labels <- c(" Non-Applicable Bldg"," 00 - 25"," 26 - 50"," 51 - 75"," 75 -100"," Exceeds 100")
  bldg_color_palette <- mapcolors$color[mapcolors$group=="bldg"]
  path2demand_bldg <-path2demand_bldg
  bldg_demand_info <- bldgXpkng4mapping[grep("bldg_all",bldgXpkng4mapping$group),c("field_name","text_title")]
  
  both_demand_info <- bldgXpkng4mapping[grep("both_all",bldgXpkng4mapping$group),c("field_name","text_title")]
  both_breaks <- union(pkng_breaks,bldg_breaks)
  both_color_palette <- c("  Non-Applicable Lot" = pkng_color_palette[1],
                          "  00%- 40%" = pkng_color_palette[2],
                          "  41%- 55%" = pkng_color_palette[3],
                          "  56%- 70%" = pkng_color_palette[4],
                          "  71%- 85%" = pkng_color_palette[5],
                          "  85%-100%" = pkng_color_palette[6],
                          " Non-Applicable Bldg" = bldg_color_palette[1],
                          " 00 - 25" = bldg_color_palette[2],
                          " 26 - 50" = bldg_color_palette[3],
                          " 51 - 75" = bldg_color_palette[4],
                          " 75 -100" = bldg_color_palette[5],
                          " Exceeds 100" = bldg_color_palette[6])
  plot_pkng <- FALSE
  plot_bldg <- FALSE
  plot_both <- TRUE
  
  # Load shape and other data ---------------------------------------------------------------------------------------------------
  shape_bldg <- readOGR(path2shape_bldg[1],path2shape_bldg[2])
  shape_pkng <- readOGR(path2shape_pkng[1],path2shape_pkng[2])
  
  shape_bldg <- spTransform(shape_bldg, CRS("+proj=longlat +datum=WGS84"))
  shape_pkng <- spTransform(shape_pkng, CRS("+proj=longlat +datum=WGS84"))
  
  shape_bldg@data$id <- rownames(shape_bldg@data)
  shape_pkng@data$id <- rownames(shape_pkng@data)
  
  # fortify converts spatial object to data frame -------------------------------------------------------------------------------
  bldg_shape_data <- fortify(shape_bldg)
  pkng_shape_data <- fortify(shape_pkng)
  
  bldg_shape_data <- left_join(bldg_shape_data, shape_bldg@data, by = "id")
  bldg_shape_data = clean_text(bldg_shape_data, col_name="NAME")
  
  pkng_shape_data <- left_join(pkng_shape_data, shape_pkng@data, by = "id")
  pkng_shape_data = clean_text(pkng_shape_data, col_name="LOT")
  
  # ADD GENERIC SHAPE DATA FOR NEW LOTS AND BUILDINGS -------------------------------------------------------------------------
  demand_bldg <- read.csv(path2demand_bldg,stringsAsFactors=FALSE)
  demand_bldg = clean_text(demand_bldg, col_name = "BLDG_NAME")
  temp_add_bldg <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$BLDG_NAME),]
  temp_add_bldg = clean_text(temp_add_bldg, col_name = "BLDG_NAME")
  
  debug_newshapes <- FALSE
  
  for (i in 1:nrow(temp_add_bldg)) {
    # if name is used, add 'new' until it is unique
    while (temp_add_bldg$BLDG_NAME[i] %in% bldg_shape_data$NAME | temp_add_bldg$BLDG_NAME[i] %in% shape2bldg$BLDG_NAME) {
      if (debug_newshapes) {print("adjusting name")}
      temp_add_bldg$BLDG_NAME[i] <- paste("New",temp_add_bldg$BLDG_NAME[i],sep="")
    }
    
    temp <- bldg_shape_data[bldg_shape_data$NAME == "SKILLINGHUGHHILDRETHBUILDING" & !is.na(bldg_shape_data$NAME),]
    temp$group <- as.character(temp$group)
    checkdf <- temp
    
    if (mean(temp$long) != temp_add_bldg$Longitude[i]) {
      if (debug_newshapes) {print("adjusting longitudes")}
      temp$long <- temp$long + (temp_add_bldg$Longitude[i] - mean(temp$long))
      temp$Lon <- temp$Lon + (temp_add_bldg$Lon[i] - mean(temp$Lon))
      temp$Longitude <- temp$Longitude + (temp_add_bldg$Longitude[i] - mean(temp$Longitude))
    }
    
    if (mean(temp$lat) != temp_add_bldg$Latitude[i]) {
      if (debug_newshapes) {print("adjusting latitudes")}
      temp$lat <- temp$lat + (temp_add_bldg$Latitude[i] - mean(temp$lat))
      temp$Lat <- temp$Lat + (temp_add_bldg$Latitude[i] - mean(temp$Lat))
      temp$Latitude <- temp$Latitude + (temp_add_bldg$Latitude[i] - mean(temp$Latitude))
    }
    
    if (checkdf$NAME[1] == temp$NAME[1]) {
      if (debug_newshapes) {print("adjusting name")}
      temp$NAME <- temp_add_bldg$BLDG_NAME[i]
    }
    
    new_id <- as.numeric(max(bldg_shape_data$OBJECTID)) + 1
    
    if (checkdf$id[1] == temp$id[1]) {
      if (debug_newshapes) {print("adjusting id")}
      temp$id <- as.character(new_id - 1)
    }
    
    if (checkdf$group[1] == temp$group[1]) {
      if (debug_newshapes) {print("adjusting group")}
      temp$group <- as.character(new_id - 0.9)
    }
    
    if (checkdf$OBJECTID[1] == temp$OBJECTID[1]) {
      if (debug_newshapes) {print("adjusting OBJECTID")}
      temp$OBJECTID <- as.character(new_id)
    }
    
    if (checkdf$OBJECTID_1[1] == temp$OBJECTID_1[1]) {
      if (debug_newshapes) {print("adjusting OBJECTID_1")}
      temp$OBJECTID_1 <- as.character(new_id)
    }
    
    if (!temp$NAME[1] %in% bldg_shape_data$NAME) {
      if (debug_newshapes) {print(paste("inserting shape for",temp$NAME[1]))}
      bldg_shape_data <- rbind(bldg_shape_data, temp)
    }
  }
  
  demand_pkng <- read.csv(path2demand_pkng,stringsAsFactors=FALSE)
  demand_pkng = clean_text(demand_pkng, col_name = "LOT")
  temp_add_pkng <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$`P&TS.Lot.Name`),]
  temp_add_pkng = clean_text(temp_add_pkng, col_name = 'P&TS.Lot.Name')
  
  for (i in 1:nrow(temp_add_pkng)) {
    # if name is used, add 'new' until it is unique
    while (temp_add_pkng$'P&TS.Lot.Name'[i] %in% pkng_shape_data$Lot) {
      if (debug_newshapes) {print("adjusting name")}
      temp_add_pkng$'P&TS.Lot.Name'[i] <- paste("New",temp_add_pkng$'P&TS.Lot.Name'[i],sep="")
    }
    
    temp <- pkng_shape_data[pkng_shape_data$LOT == "GilbertDock" & !is.na(pkng_shape_data$LOT),]
    temp$group <- as.character(temp$group)
    checkdf <- temp
    
    if (mean(temp$long) != temp_add_pkng$Longitude[i]) {
      if (debug_newshapes) {print("adjusting longitudes")}
      temp$long <- temp$long + (temp_add_pkng$Longitude[i] - mean(temp$long))
      temp$Lon <- temp$Lon + (temp_add_pkng$Lon[i] - mean(temp$Lon))
      temp$Longitude <- temp$Longitude + (temp_add_pkng$Longitude[i] - mean(temp$Longitude))
    }
    
    if (mean(temp$lat) != temp_add_pkng$Latitude[i]) {
      if (debug_newshapes) {print("adjusting latitudes")}
      temp$lat <- temp$lat + (temp_add_pkng$Latitude[i] - mean(temp$lat))
      temp$Lat <- temp$Lat + (temp_add_pkng$Latitude[i] - mean(temp$Lat))
      temp$Latitude <- temp$Latitude + (temp_add_pkng$Latitude[i] - mean(temp$Latitude))
    }
    
    if (checkdf$LOT[1] == temp$LOT[1]) {
      if (debug_newshapes) {print("adjusting name")}
      temp$LOT <- temp_add_pkng$'P&TS.Lot.Name'[i]
    }
    
    new_id <- as.numeric(max(pkng_shape_data$OBJECTID)) + 1
    
    if (checkdf$id[1] == temp$id[1]) {
      if (debug_newshapes) {print("adjusting id")}
      temp$id <- as.character(new_id - 1)
    }
    
    if (checkdf$group[1] == temp$group[1]) {
      if (debug_newshapes) {print("adjusting group")}
      temp$group <- as.character(new_id - 0.9)
    }
    
    if (checkdf$OBJECTID[1] == temp$OBJECTID[1]) {
      if (debug_newshapes) {print("adjusting OBJECTID")}
      temp$OBJECTID <- as.character(new_id)
    }
    
    if (checkdf$OBJECTID_1[1] == temp$OBJECTID_1[1]) {
      if (debug_newshapes) {print("adjusting OBJECTID_1")}
      temp$OBJECTID_1 <- as.character(new_id)
    }
    
    if (!temp$LOT[1] %in% pkng_shape_data$Lot) {
      if (debug_newshapes) {print(paste("inserting shape for",temp$LOT[1]))}
      pkng_shape_data <- rbind(pkng_shape_data, temp)
    }
  }
  
  
  temp_bldg_coords <- project_list_instance[!is.na(project_list_instance$BLDG_NAME),]
  temp_bldg_coords = clean_text(temp_bldg_coords, col_name = "BLDG_NAME")
  temp_pkng_coords <- project_list_instance[!is.na(project_list_instance$`P&TS.Lot.Name`),]
  temp_pkng_coords = clean_text(temp_pkng_coords, col_name = 'P&TS.Lot.Name')
  
  temp_logical <- project_list$`End.Date.(Optional)` == instance_name
  temp_logical[is.na(temp_logical)] <- FALSE
  project_list_uptonow <- project_list[project_list$Start.Date == instance_name | temp_logical,]
  
  color_active <- "Blue"
  color_inactive <- "Black"
  bldg_to_circle = data.frame(NAME = unique(bldg_shape_data$NAME)[unique(bldg_shape_data$NAME) %in% na.omit(temp_bldg_coords$BLDG_NAME)], lat = numeric(length(unique(temp_bldg_coords$BLDG_NAME))), lon = numeric(length(unique(temp_bldg_coords$BLDG_NAME))), radius = numeric(length(unique(temp_bldg_coords$BLDG_NAME))), color = character(length(unique(temp_bldg_coords$BLDG_NAME))))

  i=3
  for (i in 1:nrow(bldg_to_circle)) {
    temp <- na.omit(bldg_shape_data[bldg_shape_data$NAME == bldg_to_circle$NAME[i],])
    bldg_to_circle$lat[i] <- mean(temp$lat)
    bldg_to_circle$lon[i] <- mean(temp$lon)
    bldg_to_circle$radius[i] <- max(max(temp$lat) - min(temp$lat), max(temp$lon) - min(temp$lon)) / 2 * 5
    if (temp_bldg_coords$No.[i] %in% project_list_uptonow$No.) {
      bldg_to_circle$color[i] <- color_active
    } else {
      bldg_to_circle$color[i] <- color_inactive
    }
  }

  pkng_to_circle = data.frame(LOT = unique(pkng_shape_data$LOT)[unique(pkng_shape_data$LOT) %in% na.omit(temp_pkng_coords$`P&TS.Lot.Name`)], lat = numeric(length(unique(temp_pkng_coords$`P&TS.Lot.Name`))), lon = numeric(length(unique(temp_pkng_coords$`P&TS.Lot.Name`))), radius = numeric(length(unique(temp_pkng_coords$`P&TS.Lot.Name`))), color = character(length(unique(temp_pkng_coords$`P&TS.Lot.Name`))))
  
  i=2
  for (i in 1:nrow(pkng_to_circle)) {
    temp <- pkng_shape_data[pkng_shape_data$LOT == pkng_to_circle$LOT[i],]
    pkng_to_circle$lat[i] <- mean(temp$lat)
    pkng_to_circle$lon[i] <- mean(temp$lon)
    pkng_to_circle$radius[i] <- max(max(temp$lat) - min(temp$lat), max(temp$lon) - min(temp$lon)) / 2 * 5
    if (temp_pkng_coords$No.[i] %in% project_list_uptonow$No.) {
      pkng_to_circle$color[i] <- color_active
    } else {
      pkng_to_circle$color[i] <- color_inactive
    }
  }
  
  
  # run map functions -----------------------------------------------------------------------------------------------------------
  map_parking_conditions(path2shape_bldg = path2shape_bldg, path2shape_pkng = path2shape_pkng, path2demand_bldg = path2demand_bldg,
                         path2demand_pkng = path2demand_pkng, pkng_demand_info = pkng_demand_info, bldg_demand_info = bldg_demand_info, 
                         both_demand_info = both_demand_info, pkng_breaks = pkng_breaks, pkng_labels = pkng_labels, shape2bldg = shape2bldg,
                         pkng_color_palette = pkng_color_palette, bldg_breaks = bldg_breaks, bldg_labels = bldg_labels,
                         bldg_color_palette = bldg_color_palette, plot_pkng = plot_pkng, plot_bldg = plot_bldg, plot_both = plot_both, 
                         both_color_palette = both_color_palette, instance_name = instance_name, basemap = basemap,
                         shape_bldg = shape_bldg, shape_pkng = shape_pkng, bldg_shape_data = bldg_shape_data, pkng_shape_data = pkng_shape_data,
                         bldg_to_circle = bldg_to_circle, pkng_to_circle = pkng_to_circle)
  
  path2demand_bldg <- "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg4mapping_filtered.csv"
  path2demand_pkng <- "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng4mapping_filtered.csv"
  pkng_demand_info <- bldgXpkng4mapping[grep("pkng_filtered",bldgXpkng4mapping$group),c("field_name","text_title")]
  bldg_demand_info <- bldgXpkng4mapping[grep("bldg_filtered",bldgXpkng4mapping$group),c("field_name","text_title")]
  both_demand_info <- bldgXpkng4mapping[grep("both_filtered",bldgXpkng4mapping$group),c("field_name","text_title")]
  plot_pkng <- FALSE
  plot_bldg <- FALSE
  plot_both <- FALSE
  
  map_parking_conditions(path2shape_bldg = path2shape_bldg, path2shape_pkng = path2shape_pkng, path2demand_bldg = path2demand_bldg,
                         path2demand_pkng = path2demand_pkng, pkng_demand_info = pkng_demand_info, bldg_demand_info = bldg_demand_info, 
                         both_demand_info = both_demand_info, pkng_breaks = pkng_breaks, pkng_labels = pkng_labels, shape2bldg = shape2bldg,
                         pkng_color_palette = pkng_color_palette, bldg_breaks = bldg_breaks, bldg_labels = bldg_labels,
                         bldg_color_palette = bldg_color_palette, plot_pkng = plot_pkng, plot_bldg = plot_bldg, plot_both = plot_both, 
                         both_color_palette = both_color_palette, instance_name = instance_name, basemap = basemap,
                         shape_bldg = shape_bldg, shape_pkng = shape_pkng, bldg_shape_data = bldg_shape_data, pkng_shape_data = pkng_shape_data,
                         bldg_to_circle = bldg_to_circle, pkng_to_circle = pkng_to_circle)
  
  # Clear workspace -------------------------------------------------------------------------------------------------------------
  rm(list = ls()) 
}
