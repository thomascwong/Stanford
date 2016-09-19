# space allocation function using adjusted huff's model http://www.ctre.iastate.edu/Research/traffic/huffprobability.htm -------------
AllocationMatrixBySpace <- function(distance_matrix, bldg_demand, pkng_capacity, pkng_current, bldg_id, pkng_id, set_demand = 0,
                                    key, soft_cap = TRUE, allocation_block = 1, debug = FALSE, debug_space = FALSE) {
  require(openxlsx)
  require(raster)
  require(data.table)
  require(dplyr)

  # apply initial adjustments and tests ----------------------------------------------------------------------------------------------
  
  # adjustment: if specified, adjust demand  -----------------------------------------------------------------------------------------
  if (set_demand != 0) {bldg_demand = bldg_demand*set_demand/sum(bldg_demand)}
  
  # adjustment: if soft_cap is TRUE set to current utilization counts first before allocating to rest of spaces  ---------------------
  if (soft_cap) {pkng_supply <- pkng_current; pkng_extra <- pkng_capacity - pkng_supply
  } else {pkng_supply <- pkng_capacity} 

  # set initial parameters -----------------------------------------------------------------------------------------------------------
  huff_matrix <- distance_matrix # powers the distance matrix by lambda
  allocation_matrix <- data.frame(distance_matrix)*0
  colnames(allocation_matrix) <- pkng_id; rownames(allocation_matrix) <- bldg_id
  bldg_demand_true_indices <- which(bldg_demand > 0)
  index_counter <- 1
  bldg_index <- bldg_demand_true_indices[index_counter]
  total <- sum(bldg_demand)
  switch_rm_emptylot <- FALSE
  if (debug_space) {
    k <- 1
    to_filter <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Input\\Parking Model Input.xlsx", sheet = "Filter")
    temp_index <- which(bldg_id == "07-340")
    }
 
  # allocation while loop ------------------------------------------------------------------------------------------------------------
  while (sum(bldg_demand) > 0) {
    if (debug) {print(paste(sum(bldg_demand),"demand left to allocate"))}
    if (findInterval(sum(bldg_demand) %% 50, c(0,1)) == 1 | (sum(bldg_demand) < 15)) {pb <- txtProgressBar(min = 0, max = total, style = 3); setTxtProgressBar(pb, sum(allocation_matrix))} # progress bar
    if (debug_space & findInterval(sum(bldg_demand) %% round(total/10,0), c(0,1)) == 1) {
      print(paste("Printing #",k, "at demand:",round(sum(allocation_matrix))))
      temp <- data.frame(allocation_matrix)
      temp <- temp[,order(distance_matrix[temp_index,])]
      setDT(temp, keep.rownames = TRUE)[]
      temp <- suppressWarnings(left_join(temp, key, by = c("rn" = "BLDG_ID")))
      temp <- temp[,c(ncol(temp),1:(ncol(temp)-1))]
      temp$BLDG_NAME %in% to_filter$BLDG_NAME
      
      temp <- temp[c(which(temp$BLDG_NAME %in% to_filter$BLDG_NAME),which(!temp$BLDG_NAME %in% to_filter$BLDG_NAME)),]
      
      sum_rows <- rowSums(temp[,-(1:2)])
      temp <- cbind(sum_rows, temp)
      sum_cols <- colSums(temp[,-(2:3)])
      sum_cols <- c(sum_cols[1],NA,NA,sum_cols[-1])
      temp <- rbind(sum_cols,temp)

      write.csv(temp,paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Allocation_Matrix",k,".csv"))
      k <- k + 1
    }
    
    # if all lots are full, stop
    if (all(pkng_supply == 0 | pkng_supply == Inf) & (!soft_cap)) {
      print("Allocation stopped. No more supply.")
      break
      
      # if building doesn't have any demand to distribute, skip
    } else if (bldg_demand[bldg_index] == 0) {switch_rm_emptylot <- TRUE; if (debug) {print("empty lot detected.")}}
    
      # otherwise start allocation loop
      else {
        if (debug & ceiling(sum(allocation_matrix)) %% 100 == 0) {print(paste("demand allocated:", ceiling(sum(allocation_matrix,na.rm=TRUE)), "of", ceiling(total)))}
        demand_block <- min(bldg_demand[bldg_index], allocation_block) # set demand block to min of remaining demand or allocation unit
        while (demand_block > 0) {
          
          # if soft_cap is TRUE and demand reaches supply, open up remaining spaces
          if (all(pkng_supply == 0 | pkng_supply == Inf) & (soft_cap)) {
            print(paste("soft_cap reached. Opening up remaining spaces. Old supply:", sum(pkng_supply)))
            pkng_supply <- pkng_supply + pkng_extra
            huff_matrix <- distance_matrix # reset distance matrix
            soft_cap <- FALSE # turn off soft_cap
            print(paste("soft_cap reached. Opening up remaining spaces. New supply:", sum(pkng_supply)))
            Sys.sleep(2)
          }
          
          huff_matrix[,pkng_supply == 0] <- Inf # sets the distance of lots with 0 spaces to Infinity
          
          # finds nearest lot with spaces
          nearestlot_index <- which(huff_matrix[bldg_index,] == min(huff_matrix[bldg_index,])); if (debug) {print(paste("lot index:",nearestlot_index))} 
          
          # allocate the min of demand, lot supply, and 1
          allocated_amount <- min(1,pkng_supply[nearestlot_index],bldg_demand[bldg_index]); if (debug) {print(paste("alloc. unit:",allocated_amount))} 
          
          # add allocated space to output matrix. Remove space from parking supply, building demand, and demand block
          if (debug) {print(paste("old added to output matrix:", allocation_matrix[bldg_index,nearestlot_index]))} 
          allocation_matrix[bldg_index,nearestlot_index] <- allocation_matrix[bldg_index,nearestlot_index] + allocated_amount
          
          if (debug) {print(paste("new added to output matrix:", allocation_matrix[bldg_index,nearestlot_index],"old supply:",
                                  pkng_supply[nearestlot_index],"alloc. unit:",allocated_amount))} 
          if(pkng_supply[nearestlot_index] - allocated_amount <0) {
            stop(paste("Error. Allocated more than available:",pkng_supply[nearestlot_index],"supply versus",allocated_amount, "demand"))}
  
          pkng_supply[nearestlot_index] <- pkng_supply[nearestlot_index] - allocated_amount
          if (debug) {print(paste("new supply:",pkng_supply[nearestlot_index],"old bldg i demand:", bldg_demand[bldg_index]))}
          
          bldg_demand[bldg_index] <- bldg_demand[bldg_index] - allocated_amount
          if (debug) {print(paste("new bldg i demand:", bldg_demand[bldg_index], "old demand block:",demand_block))}
          
          demand_block <- demand_block - allocated_amount
          if (debug) {print(paste("new demand block:",demand_block))}
          
          if (debug) {Sys.sleep(0.001)}
        }
      }
    
    if (switch_rm_emptylot) {
      bldg_demand_true_indices <- bldg_demand_true_indices[-index_counter]
      if (debug) {print(paste("removing lot at index",index_counter,"with supply",bldg_demand[bldg_index]))}
      length(bldg_demand_true_indices[-(1:length(bldg_demand_true_indices))])
      index_counter <- index_counter; if(debug) {print(paste("index counter:", index_counter))}
      
      if (index_counter == length(bldg_demand_true_indices) | length(bldg_demand_true_indices) == 1) {
        index_counter <- 1; if(debug) {print(paste("counter reset. New counter index:",index_counter))}}  # reset counter
      
      bldg_index <- bldg_demand_true_indices[index_counter]; if(debug) {
        print(paste("bldg index:", bldg_index, "bldg_true_index_length:",length(bldg_demand_true_indices)))}
      
      switch_rm_emptylot <- FALSE
    } else {
      index_counter <- index_counter + 1; if(debug) {print(paste("index counter:", index_counter))}
      
      if (index_counter == length(bldg_demand_true_indices) | length(bldg_demand_true_indices) == 1) {
        index_counter <- 1; if(debug) {print(paste("counter reset. New counter index:",index_counter))}}  # reset counter
      
      bldg_index <- bldg_demand_true_indices[index_counter]; if(debug) {
        print(paste("bldg index:", bldg_index, "bldg_true_index_length:",length(bldg_demand_true_indices)))}
    }
    
  }
  close(pb)
  
  
  # print metrics ---------------------------------------------------------------------------------------------------------------------
  # print(paste("Starting Demand: ",sum(total)))
  # print(paste("Allocated Demand:",sum(allocation_matrix)))
  # print(paste("Difference: ", sum(allocation_matrix)-sum(total)))
  # print(paste("Negative Sum: ", sum(which(allocation_matrix<0))))
  # 
  # metric = data.frame(abs(pkng_current-colSums(allocation_matrix)))
  # rownames(metric) = colnames(allocation_matrix)
  # print(paste("Sum of Lot Differences (Metric): ",round(sum(metric),0)))
  
  # setup output to return ------------------------------------------------------------------------------------------------------------
  setDT(allocation_matrix, keep.rownames = TRUE)[]
  allocation_matrix <- as.data.frame(allocation_matrix)
  allocation_matrix = suppressWarnings(left_join(allocation_matrix, key, by = c("rn" = "BLDG_ID")))
  allocation_matrix = allocation_matrix[,c(ncol(allocation_matrix),1:(ncol(allocation_matrix)-1))]
  return(allocation_matrix)
}