parking_model_demand_allocation <- function(instance_name, project_list_instance) {
  # Clear workspace --------------------------------------------------------------------------------------------------------
  rm(list = ls()) 
  
  print(paste("----------Allocating demand for instance:", instance_name))
  
  # load functions ---------------------------------------------------------------------------------------------------------
  source("AllocationMatrixBySpace.R")
  
  # setup packages ---------------------------------------------------------------------------------------------------------
  require(openxlsx)
  require(raster)
  require(stringr)
  require(dtplyr)
  
  # load datasets ----------------------------------------------------------------------------------------------------------
  bldg_u <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_BldgData_U.csv",1, stringsAsFactors = FALSE)
  pkng_u <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_PkngData_U.csv",1, stringsAsFactors = FALSE)
  bldg_h <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_BldgData_H.csv",1, stringsAsFactors = FALSE)
  pkng_h <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_PkngData_H.csv",1, stringsAsFactors = FALSE)
  pknglatlonu <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx", sheet = "pknglatlonu")
  pknglatlonh <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx", sheet = "pknglatlonh")

  # calculate variables ----------------------------------------------------------------------------------------------------
  # adds missing coordinates
  pkng_u[pkng_u$P.TS.Lot.Name %in% pknglatlonu$`P&TS.Lot.Name`,c("Lat")] <- pknglatlonu[order(pkng_u$P.TS.Lot.Name[pkng_u$P.TS.Lot.Name %in% pknglatlonu$`P&TS.Lot.Name`]),c("Lat")]
  pkng_u[pkng_u$P.TS.Lot.Name %in% pknglatlonu$`P&TS.Lot.Name`,c("Lon")] <- pknglatlonu[order(pkng_u$P.TS.Lot.Name[pkng_u$P.TS.Lot.Name %in% pknglatlonu$`P&TS.Lot.Name`]),c("Lon")]
  pkng_h[pkng_h$P.TS.Lot.Name %in% pknglatlonh$`P&TS.Lot.Name`,c("Lat")] <- pknglatlonh[order(pkng_h$P.TS.Lot.Name[pkng_h$P.TS.Lot.Name %in% pknglatlonh$`P&TS.Lot.Name`]),c("Lat")]
  pkng_h[pkng_h$P.TS.Lot.Name %in% pknglatlonh$`P&TS.Lot.Name`,c("Lon")] <- pknglatlonh[order(pkng_h$P.TS.Lot.Name[pkng_h$P.TS.Lot.Name %in% pknglatlonh$`P&TS.Lot.Name`]),c("Lon")]
  
  # pulls out coordinates for both datasets
  pts_bldg_u <- matrix(c(bldg_u$Lon,bldg_u$Lat),ncol=2)
  pts_pkng_u <- matrix(c(pkng_u$Lon,pkng_u$Lat),ncol=2)
  pts_bldg_h <- matrix(c(bldg_h$Lon,bldg_h$Lat),ncol=2)
  pts_pkng_h <- matrix(c(pkng_h$Lon,pkng_h$Lat),ncol=2)
  
  # calculates the distance matrix in units of miles. Each row is a building and each column is a parking lot
  distance_matrix_u <- pointDistance(pts_bldg_u, pts_pkng_u, lonlat=TRUE, allpairs=TRUE) * 0.000621371; rm(pts_bldg_u, pts_pkng_u)
  distance_matrix_h <- pointDistance(pts_bldg_h, pts_pkng_h, lonlat=TRUE, allpairs=TRUE) * 0.000621371; rm(pts_bldg_h, pts_pkng_h)
  
  # for each building and for each permit type, allocate it's demand based on the probabilty matrix
  bldg_id_u <- bldg_u$BLDG_ID
  pkng_id_u <- pkng_u$P.TS.Lot.Name
  bldg_id_h <- bldg_h$BLDG_ID
  pkng_id_h <- pkng_h$P.TS.Lot.Name
  bldg_key_u <- bldg_u[,c("BLDG_ID","BLDG_NAME")]
  bldg_key_h <- bldg_h[,c("BLDG_ID","BLDG_NAME")]
  
  soft_cap <- TRUE
  set_demand <- 0
  allocation_block <- 5
  debug <- FALSE
  debug_space <- FALSE
  
  # distance_matrix <- distance_matrix_u
  # bldg_demand <- bldg_u$DEMAND_UNI_DAY_UGRESIDENT + bldg_u$DEMAND_UNI_DAY_GRRESIDENT
  # pkng_capacity <- pkng_u$day_resident_spaces_total
  # pkng_current <- pkng_u$day_resident_spaces_used
  # bldg_id <- bldg_id_u
  # pkng_id <- pkng_id_u
  # key <- bldg_key_u
  # debug_space=TRUE
  
  # assume soft maximums for new lot usage
  add_lot_u <- any(!is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == FALSE,"P&TS.Lot.Name"]))
  add_lot_h <- any(!is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == TRUE,"P&TS.Lot.Name"]))
  
  assumed_utilization <- 0.85
  temp_pkng_u <- pkng_u
  temp_pkng_h <- pkng_h
  
  if (add_lot_u) {
    added_lots_u <- read.csv(paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_added_lots_u",".csv",sep=""), stringsAsFactors = FALSE)
    temp_pkng_u$visitor_spaces_used[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_u$visitor_spaces_total[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_u$day_commuter_spaces_used[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_u$day_commuter_spaces_total[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_u$night_commuter_spaces_used[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_u$night_commuter_spaces_total[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_u$day_resident_spaces_used[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_u$day_resident_spaces_total[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_u$night_resident_spaces_used[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_u$night_resident_spaces_total[temp_pkng_u$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
  }
  
  if (add_lot_h) {
    added_lots_h <- read.csv(paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_added_lots_h",".csv",sep=""), stringsAsFactors = FALSE)
    temp_pkng_h$visitor_spaces_used[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_h$visitor_spaces_total[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_h$day_commuter_spaces_used[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_h$day_commuter_spaces_total[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_h$night_commuter_spaces_used[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_h$night_commuter_spaces_total[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_h$day_resident_spaces_used[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_h$day_resident_spaces_total[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
    temp_pkng_h$night_resident_spaces_used[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name] <- assumed_utilization*temp_pkng_h$night_resident_spaces_total[temp_pkng_h$P.TS.Lot.Name %in% added_lots_u$P.TS.Lot.Name]
  }
  
  write.csv(bldg_u, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_bldg_u",".csv",sep=""), na="",row.names=FALSE)
  write.csv(bldg_h, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_bldg_h",".csv",sep=""), na="",row.names=FALSE)
  write.csv(pkng_u, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_pkng_u",".csv",sep=""), na="",row.names=FALSE)
  write.csv(pkng_h, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_pkng_h",".csv",sep=""), na="",row.names=FALSE)
  
  # run functions for each demand type --------------------------------------------------------------------------------------
  print("allocating for University Daytime Commuters")
  out_DEMAND_UNI_DAY_COMMUTER <- AllocationMatrixBySpace(debug = debug, soft_cap = soft_cap, allocation_block = allocation_block,
                                                        distance_matrix = distance_matrix_u,
                                                        bldg_demand = bldg_u$DEMAND_UNI_DAY_COMMUTER, 
                                                        pkng_capacity = temp_pkng_u$day_commuter_spaces_total,
                                                        pkng_current = temp_pkng_u$day_commuter_spaces_used,
                                                        bldg_id = bldg_id_u, 
                                                        pkng_id = pkng_id_u,
                                                        key = bldg_key_u, 
                                                        set_demand = 0, debug_space = debug_space)
  
  print("allocating for University Daytime Residents")
  out_DEMAND_UNI_DAY_RESIDENT <- AllocationMatrixBySpace(debug = debug, soft_cap = soft_cap, allocation_block = allocation_block,
                                                        distance_matrix = distance_matrix_u,
                                                        bldg_demand = bldg_u$DEMAND_UNI_DAY_UGRESIDENT + bldg_u$DEMAND_UNI_DAY_GRRESIDENT, 
                                                        pkng_capacity = temp_pkng_u$day_resident_spaces_total,
                                                        pkng_current = temp_pkng_u$day_resident_spaces_used,
                                                        bldg_id = bldg_id_u, 
                                                        pkng_id = pkng_id_u,
                                                        key = bldg_key_u, 
                                                        set_demand = 0, debug_space = debug_space)
  
  print("allocating for University Nighttime Commuters")
  out_DEMAND_UNI_NIGHT_COMMUTER <- AllocationMatrixBySpace(debug = debug, soft_cap = soft_cap, allocation_block = allocation_block,
                                                          distance_matrix = distance_matrix_u,
                                                          bldg_demand = bldg_u$DEMAND_UNI_NIGHT_COMMUTER, 
                                                          pkng_capacity = temp_pkng_u$night_commuter_spaces_total,
                                                          pkng_current = temp_pkng_u$night_commuter_spaces_used,
                                                          bldg_id = bldg_id_u, 
                                                          pkng_id = pkng_id_u,
                                                          key = bldg_key_u, 
                                                          set_demand = 0, debug_space = debug_space)
  
  print("allocating for University Nighttime Residents")
  out_DEMAND_UNI_NIGHT_RESIDENT <- AllocationMatrixBySpace(debug = debug, soft_cap = soft_cap, allocation_block = allocation_block,
                                                          distance_matrix = distance_matrix_u,
                                                          bldg_demand = bldg_u$DEMAND_UNI_NIGHT_UGRESIDENT + bldg_u$DEMAND_UNI_NIGHT_GRRESIDENT, 
                                                          pkng_capacity = temp_pkng_u$night_resident_spaces_total,
                                                          pkng_current = temp_pkng_u$night_resident_spaces_used,
                                                          bldg_id = bldg_id_u, 
                                                          pkng_id = pkng_id_u,
                                                          key = bldg_key_u, 
                                                          set_demand = 0, debug_space = debug_space)
  
  print("allocating for University Visitors")
  out_DEMAND_UNI_VISITOR <- AllocationMatrixBySpace(debug = debug, soft_cap = soft_cap, allocation_block = allocation_block,
                                                   distance_matrix = distance_matrix_u,
                                                   bldg_demand = bldg_u$DEMAND_UNI_VISITOR, 
                                                   pkng_capacity = temp_pkng_u$visitor_spaces_total,
                                                   pkng_current = temp_pkng_u$visitor_spaces_used,
                                                   bldg_id = bldg_id_u, 
                                                   pkng_id = pkng_id_u,
                                                   key = bldg_key_u, 
                                                   set_demand = 0, debug_space = debug_space)
  
  print("allocating for Hospital Commuters")
  out_DEMAND_HOSP_COMMUTER <- AllocationMatrixBySpace(debug = debug, soft_cap = soft_cap, allocation_block = allocation_block,
                                                     distance_matrix = distance_matrix_h,
                                                     bldg_demand = bldg_h$DEMAND_HOSP_COMMUTER, 
                                                     pkng_capacity = temp_pkng_h$commuter_spaces_total,
                                                     pkng_current = temp_pkng_h$commuter_spaces_used,   
                                                     bldg_id = bldg_id_h, 
                                                     pkng_id = pkng_id_h,
                                                     key = bldg_key_h, 
                                                     set_demand = 0, debug_space = debug_space)
  
  # distance_matrix <- distance_matrix_h
  # bldg_demand <- bldg_h$DEMAND_HOSP_VISITOR
  # pkng_capacity <- temp_pkng_h$visitor_spaces_total
  # pkng_current <- temp_pkng_h$visitor_spaces_used
  # bldg_id <- bldg_id_h
  # pkng_id <- pkng_id_h
  # key <- bldg_key_h
  
  print("allocating for Hospital Visitors")
  out_DEMAND_HOSP_VISITOR <- AllocationMatrixBySpace(debug = debug, soft_cap = soft_cap, allocation_block = allocation_block,
                                                    distance_matrix = distance_matrix_h,
                                                    bldg_demand = bldg_h$DEMAND_HOSP_VISITOR, 
                                                    pkng_capacity = temp_pkng_h$visitor_spaces_total,
                                                    pkng_current = temp_pkng_h$visitor_spaces_used,     
                                                    bldg_id = bldg_id_h, 
                                                    pkng_id = pkng_id_h,
                                                    key = bldg_key_h, 
                                                    set_demand = 0, debug_space = debug_space)
  
  write.csv(out_DEMAND_UNI_DAY_COMMUTER, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_DAY_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(out_DEMAND_UNI_NIGHT_COMMUTER, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_NIGHT_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(out_DEMAND_UNI_DAY_RESIDENT, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_DAY_RESIDENT",".csv",sep=""), na="",row.names=FALSE)
  write.csv(out_DEMAND_UNI_NIGHT_RESIDENT, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_NIGHT_RESIDENT",".csv",sep=""), na="",row.names=FALSE)
  write.csv(out_DEMAND_UNI_VISITOR, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_VISITOR",".csv",sep=""), na="",row.names=FALSE)
  write.csv(out_DEMAND_HOSP_COMMUTER, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_HOSP_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(out_DEMAND_HOSP_VISITOR, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_HOSP_VISITOR",".csv",sep=""), na="",row.names=FALSE)
  
  write.csv(bldg_u, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg_u",".csv",sep=""), na="",row.names=FALSE)
  write.csv(pkng_u, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng_u",".csv",sep=""), na="",row.names=FALSE)
  write.csv(bldg_h, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg_h",".csv",sep=""), na="",row.names=FALSE)
  write.csv(pkng_h, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng_h",".csv",sep=""), na="",row.names=FALSE)
  
  # Clear workspace -------------------------------------------------------------------------------------------------------------
  rm(list = ls()) 
}