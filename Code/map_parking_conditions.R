map_parking_conditions <- function(path2shape_bldg, path2shape_pkng, path2demand_bldg, path2demand_pkng, pkng_demand_info, bldg_demand_info, both_demand_info,
                                   pkng_breaks, pkng_labels, pkng_color_palette, bldg_breaks, bldg_labels, bldg_color_palette, plot_pkng = TRUE, plot_bldg = TRUE,
                                   plot_both = TRUE, shape2bldg, both_color_palette, instance_name, basemap,
                                   shape_bldg, shape_pkng, bldg_shape_data, pkng_shape_data, bldg_to_circle, pkng_to_circle) {

  clean_text <- function(df,col_name) {
    trim <- function (x) gsub(" ", "", x)
    df[,c(col_name)] <- as.character(df[,c(col_name)]) # change factor to character
    for (i in 1:nrow(df)) {df[,c(col_name)][i] <- str_replace_all(df[,c(col_name)][i], "[[:punct:]]", " ")} # remove non-alphanums
    df[,c(col_name)] <- trim(df[,c(col_name)]) # remove all spaces
    return(df)
  }
  
  # make_circle <- function(center = c(0,0),diameter = 1, npoints = 100){
  #   r <- diameter / 2
  #   tt <- seq(0,2*pi,length.out = npoints)
  #   xx <- center[1] + r * cos(tt)
  #   yy <- center[2] + r * sin(tt)
  #   return(data.frame(x = xx, y = yy))
  # }
  
  # Load parking and building data ----------------------------------------------------------------------------------------------
  demand_bldg <- read.csv(path2demand_bldg,stringsAsFactors=FALSE)
  demand_pkng <- read.csv(path2demand_pkng,stringsAsFactors=FALSE)
  
  # Join shape data with utilization data ---------------------------------------------------------------------------------------
  demand_bldg = clean_text(demand_bldg, col_name = "BLDG_NAME")
  bldg_shape_data <- left_join(bldg_shape_data, shape2bldg, by = c("NAME" = "NAME")) # add key field to connect shape and building data
  bldg_shape_data <- left_join(bldg_shape_data, demand_bldg, by = c("BLDG_NAME" = "BLDG_NAME"))
  
  demand_pkng = clean_text(demand_pkng, col_name = "LOT")
  pkng_shape_data <- left_join(pkng_shape_data, demand_pkng, by = c("LOT" = "LOT"))
  
  
  unique(pkng_shape_data[pkng_shape_data$LOT == "L03QuarrySouth",-(1:3)])
  
  # temp <- subset(pkng_shape_data, !duplicated(LOT))
  # temp[,c("LOT","utilization_total_day_all")]
  # demand_pkng[,c("LOT","utilization_total_day_all")]
  
  
  # 
  # pkng_shape_data = cbind(pkng_shape_data, shape_pkng@data[match(pkng_shape_data$group,(shape_pkng@data$OBJECTID_1-0.9)),c("FACIL_ID","LOT")])
  # pkng_shape_data = clean_text(pkng_shape_data, col_name="LOT")
  # demand_pkng = clean_text(demand_pkng, col_name = "LOT")
  # pkng_shape_data <- left_join(pkng_shape_data, demand_pkng, by = c("LOT" = "LOT"))
  # 
  # bldg_shape_data = cbind(bldg_shape_data, shape_bldg@data[match(bldg_shape_data$group,(shape_bldg@data$OBJECTID_1-0.9)),c("NAME","BLDG_ID")])
  # bldg_shape_data = clean_text(bldg_shape_data, col_name = "NAME")
  # demand_bldg = clean_text(demand_bldg, col_name = "BLDG_NAME")
  # bldg_shape_data <- left_join(bldg_shape_data, demand_bldg, by = c("NAME" = "BLDG_NAME"))
  
  # Clean data ------------------------------------------------------------------------------------------------------------------
  # set NAs to zero for pertinant columns
  fields = c("total", "demand", "utilization")
  for (word in fields) {
    pkng_shape_data[,grepl(word, names(pkng_shape_data))][!is.finite(as.matrix(pkng_shape_data[,grepl(word, names(pkng_shape_data))]))] <- 0}
  for (word in fields) {
    bldg_shape_data[,grepl(word, tolower(names(bldg_shape_data)))][!is.finite(as.matrix(bldg_shape_data[,grepl(word, tolower(names(bldg_shape_data)))]))] <- 0}
  
  # type <- 1
  # both_demand_info = both_demand_info[type,]
  # pkng_demand_info = pkng_demand_info[type,]
  # bldg_demand_info = bldg_demand_info[type,]
  
  
  # create map plotting and printing function -----------------------------------------------------------------------------------
  print_map <- function(basemap, both_demand_info,
                        plot_pkng = FALSE, pkng_shape_data, pkng_demand_info, pkng_breaks, pkng_labels, pkng_color_palette,
                        plot_bldg = FALSE, bldg_shape_data, bldg_demand_info, bldg_breaks, bldg_labels, bldg_color_palette) {
    
    # If plotting bldg, generate map --------------------------------------------------------------------------------
    if (plot_bldg) {
      bldg_map_title <- paste(instance_name, bldg_demand_info[1,2], sep = " ")
      bldg_shape_data$datadiscrete <- cut(bldg_shape_data[,c(bldg_demand_info[1,1])],breaks = bldg_breaks, labels = bldg_labels, include.lowest = TRUE) # buckets continuous var
      bldg_color_palette = bldg_color_palette[levels(bldg_shape_data$datadiscrete) %in% unique(bldg_shape_data$datadiscrete)]
      bldgmap <- basemap + 
        geom_polygon(aes(x=long, y=lat, group=group, fill=datadiscrete), data=bldg_shape_data, alpha=1) +
        scale_fill_manual(values=bldg_color_palette, guide_legend(title = "Demand (#Spaces)")) +
        ggtitle(bldg_map_title)
      
      # if (nrow(bldg_to_circle) > 0) {
      #   for (i in 1:nrow(bldg_to_circle)) {
      #     circle4plot <- make_circle(c(bldg_to_circle$lat[i],bldg_to_circle$lon[i]),bldg_to_circle$radius[i],npoints = 100)
      #     bothmap <- bothmap +
      #       geom_path(aes(x = x, y = y), data = circle4plot, size = 5, color = bldg_to_circle$color[i])
      #   }
      # }
        
      # image_name <- paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Output\\", bldg_map_title," ",gsub(":","-",Sys.time()),".png",sep="")
      image_name <- paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Output\\", bldg_map_title," ",".png",sep="")
      ggsave(plot = bldgmap, image_name, width = 11, height = 8.5, units = "in", dpi = 200)
      print(paste("Map Saved: ", bldg_map_title))
    }
    
    # If plotting pkng, generate map --------------------------------------------------------------------------------
    if (plot_pkng) {
      pkng_map_title <- paste(instance_name, pkng_demand_info[1,2], sep = " ")
      pkng_shape_data$datadiscrete <- cut(pkng_shape_data[,c(pkng_demand_info[1,1])],breaks = pkng_breaks, labels = pkng_labels, include.lowest = TRUE) # buckets continuous var
      pkng_color_palette = pkng_color_palette[levels(pkng_shape_data$datadiscrete) %in% unique(pkng_shape_data$datadiscrete)]
      pkngmap <- basemap + 
        geom_polygon(aes(x=long, y=lat, group=group, fill=datadiscrete), data=pkng_shape_data, alpha=1) +
        scale_fill_manual(values=pkng_color_palette, guide_legend(title = "Utilization (%Spaces)")) +
        ggtitle(pkng_map_title)
      
      # if (nrow(pkng_to_circle) > 0) {
      #   for (i in 1:nrow(pkng_to_circle)) {
      #     circle4plot <- make_circle(c(pkng_to_circle$lat[i],pkng_to_circle$lon[i]),pkng_to_circle$radius[i],npoints = 100)
      #     bothmap <- bothmap +
      #       geom_path(aes(x = x, y = y), data = circle4plot, size = 5, color = pkng_to_circle$color[i])
      #   }
      # }
      
      # image_name <- paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Output\\", pkng_map_title," ",gsub(":","-",Sys.time()),".png",sep="")
      image_name <- paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Output\\", pkng_map_title," ",".png",sep="")
      ggsave(plot = pkngmap, image_name, width = 11, height = 8.5, units = "in", dpi = 200)
      print(paste("Map Saved: ", pkng_map_title))
    }
    # pkngmap
    # unique(pkng_shape_data$LOT)
    # pkng_shape_data$datadiscrete[which(pkng_shape_data$LOT == "L 02  Quarry Psychiatry")]
    
    if (plot_both) {
      both_map_title <- paste(instance_name, both_demand_info[1,2], sep = " ")
      bldg_shape_data$datadiscrete <- cut(bldg_shape_data[,c(bldg_demand_info[1,1])],breaks = bldg_breaks, labels = bldg_labels, include.lowest = TRUE) # buckets continuous var
      bldg_color_palette = bldg_color_palette[levels(bldg_shape_data$datadiscrete) %in% unique(bldg_shape_data$datadiscrete)]
      pkng_shape_data$datadiscrete <- cut(pkng_shape_data[,c(pkng_demand_info[1,1])],breaks = pkng_breaks, labels = pkng_labels, include.lowest = TRUE) # buckets continuous var
      pkng_color_palette = pkng_color_palette[levels(pkng_shape_data$datadiscrete) %in% unique(pkng_shape_data$datadiscrete)]
      
      bldgmap <- basemap + 
        # for each building of interest (not just added bldgs)
        # make filtered df of shape data of just that bldg and extend all the lat lon fields to be bigger slightly
        # plot after the geo_polygon same way as below.
        # geom_polygon(aes(x=long, y=lat, group=group, border = "black"), data=bldg_shape_data, alpha=1) +
        geom_polygon(aes(x=long, y=lat, group=group, fill=datadiscrete), data=bldg_shape_data, alpha=1) +
        scale_fill_manual(values=both_color_palette, guide_legend(title = "Utilization                  Demand"))
      bldgmap
      
      
      bothmap <- bldgmap +
        geom_polygon(aes(x=long, y=lat, group=group, fill=datadiscrete), data=pkng_shape_data, alpha=1) +
        ggtitle(both_map_title) +
        guides(fill=guide_legend(ncol=2))

       bothmap
      
      # if (nrow(bldg_to_circle) > 0) {
      #   for (i in 1:nrow(bldg_to_circle)) {
      #     circle4plot <- make_circle(c(bldg_to_circle$lat[i],bldg_to_circle$lon[i]),bldg_to_circle$radius[i],npoints = 100)
      #     bothmap <- bothmap +
      #       geom_path(aes(x = x, y = y), data = circle4plot, size = 10, colour = bldg_to_circle$color[i])
      #   }
      # }
      
      
      # i=1
      # bldg_to_circle$NAME
      # circle4plot$x
      # circle4plot$y
      # if (nrow(bldg_to_circle) > 0) {
      #   for (i in 1:nrow(bldg_to_circle)) {
      #     circle4plot <- make_circle(c(bldg_to_circle$lat[i],bldg_to_circle$lon[i]),bldg_to_circle$radius[i],npoints = 100)
      #     bothmap <- bothmap +
      #       geom_path(aes(x = x, y = y), data = circle4plot, size = 10, colour = bldg_to_circle$color[i])
      #   }
      # }
      
      # if (nrow(pkng_to_circle) > 0) {
      #   for (i in 1:nrow(pkng_to_circle)) {
      #     circle4plot <- make_circle(c(pkng_to_circle$lat[i],pkng_to_circle$lon[i]),pkng_to_circle$radius[i],npoints = 100)
      #     bothmap <- bothmap +
      #       geom_path(aes(x = x, y = y), data = circle4plot, size = 5, color = pkng_to_circle$color[i])
      #   }
      # }
      
      # image_name <- paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Output\\", both_map_title," ",gsub(":","-",Sys.time()),".png",sep="")
      image_name <- paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Output\\", both_map_title," ",".png",sep="")
      
      ggsave(plot = bothmap, image_name, width = 11, height = 8.5, units = "in", dpi = 200)
      print(paste("Map Saved: ", both_map_title))
    }
  }
  
  # print overlayed building and parking maps
  for (type in 1:nrow(pkng_demand_info)) {
    print_map(basemap = basemap, both_demand_info = both_demand_info[type,],
              plot_pkng = plot_pkng, pkng_shape_data = pkng_shape_data, pkng_demand_info = pkng_demand_info[type,], 
              pkng_breaks = pkng_breaks, pkng_labels = pkng_labels, pkng_color_palette = pkng_color_palette,
              plot_bldg = plot_bldg, bldg_shape_data = bldg_shape_data, bldg_demand_info = bldg_demand_info[type,], 
              bldg_breaks = bldg_breaks, bldg_labels = bldg_labels, bldg_color_palette = bldg_color_palette)
  }
  
}
  
