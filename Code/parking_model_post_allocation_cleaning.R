parking_model_post_allocation_cleaning <- function(instance_name, project_list_instance) {
  
  # Clear workspace --------------------------------------------------------------------------------------------------------
  rm(list = ls()) 
  
  print(paste("----------Post-allocation cleaning for instance:", instance_name))
  
  # setup packages ---------------------------------------------------------------------------------------------------------
  require(openxlsx)
  require(raster)
  require(dtplyr)
  require(stringr)
  
  
  out_DEMAND_UNI_DAY_COMMUTER <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_DAY_COMMUTER",".csv",sep=""), na="")
  out_DEMAND_UNI_NIGHT_COMMUTER <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_NIGHT_COMMUTER",".csv",sep=""), na="")
  out_DEMAND_UNI_DAY_RESIDENT <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_DAY_RESIDENT",".csv",sep=""), na="")
  out_DEMAND_UNI_NIGHT_RESIDENT <- read.csv( file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_NIGHT_RESIDENT",".csv",sep=""), na="")
  out_DEMAND_UNI_VISITOR <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_UNI_VISITOR",".csv",sep=""), na="")
  out_DEMAND_HOSP_COMMUTER <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_HOSP_COMMUTER",".csv",sep=""), na="")
  out_DEMAND_HOSP_VISITOR <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\unsorted_allocation_out_DEMAND_HOSP_VISITOR",".csv",sep=""), na="")
  
  bldg_u <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg_u",".csv",sep=""), na="",stringsAsFactors=FALSE)
  pkng_u <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng_u",".csv",sep=""), na="",stringsAsFactors=FALSE)
  bldg_h <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg_h",".csv",sep=""), na="",stringsAsFactors=FALSE)
  pkng_h <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng_h",".csv",sep=""), na="",stringsAsFactors=FALSE)
  
  # merge into one parking demand dataframe
  names_pkng <- unique(union(colnames(out_DEMAND_UNI_DAY_COMMUTER),colnames(out_DEMAND_HOSP_COMMUTER)))[-c(2)]
  names_bldg <- unique(union(out_DEMAND_UNI_DAY_COMMUTER$BLDG_NAME,out_DEMAND_HOSP_COMMUTER$BLDG_NAME))

  temp <- data.frame(matrix(0, ncol = length(names_pkng), nrow = length(names_bldg)))
  
  colnames(temp) <- names_pkng
  temp$BLDG_NAME <- names_bldg
    
  bldg2pkng_template <- temp
  
  dfoverlay <- function(big_df, small_df, fix=TRUE) {
    a <- merge(big_df,small_df, all.y=TRUE)
    a <- subset(a,select = -c(rn))
    b <- big_df[!big_df$BLDG_NAME %in% a$BLDG_NAME,]
    d <- rbind(a,b)
    d$BLDG_NAME <- as.character(d$BLDG_NAME)
    d <- d[order(d$BLDG_NAME),]
    if (fix) {
      d[d$BLDG_NAME == "BIOLOGY GREENHOUSES",][1,-1] <- colSums(d[d$BLDG_NAME == "BIOLOGY GREENHOUSES",-1][2:4,],na.rm=TRUE)
      d <- d[-c(which(d$BLDG_NAME == "BIOLOGY GREENHOUSES")[2:4]),]
    }
    
    #order columns and rows
    d <- d[,order(colnames(d))];
    col_idx <- grep("BLDG_NAME", names(d))
    d <- d[, c(col_idx, (1:ncol(d))[-col_idx])]
    d <- d[order(d[,"BLDG_NAME"]),]
    return(d)
  }
  
  
  demand_allBP_uni_day_commuter <- dfoverlay(bldg2pkng_template,out_DEMAND_UNI_DAY_COMMUTER)
  demand_allBP_uni_night_commuter <- dfoverlay(bldg2pkng_template,out_DEMAND_UNI_NIGHT_COMMUTER)
  demand_allBP_uni_day_resident <- dfoverlay(bldg2pkng_template,out_DEMAND_UNI_DAY_RESIDENT)
  demand_allBP_uni_night_resident <- dfoverlay(bldg2pkng_template,out_DEMAND_UNI_NIGHT_RESIDENT)
  demand_allBP_uni_visitor <- dfoverlay(bldg2pkng_template,out_DEMAND_UNI_VISITOR)
  demand_allBP_hosp_commuter <- dfoverlay(bldg2pkng_template,out_DEMAND_HOSP_COMMUTER, fix = FALSE)
  demand_allBP_hosp_visitor <- dfoverlay(bldg2pkng_template,out_DEMAND_HOSP_VISITOR, fix = FALSE)
  
  # creates bldg2pkng dfs by demand type
  write.csv(demand_allBP_uni_day_commuter, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_DAY_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_night_commuter, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_NIGHT_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_day_resident, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_DAY_RESIDENT",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_night_resident, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_NIGHT_RESIDENT",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_visitor, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_VISITOR",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_hosp_commuter, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_HOSP_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_hosp_visitor, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_HOSP_VISITOR",".csv",sep=""), na="",row.names=FALSE)
  
  write.csv(demand_allBP_uni_day_commuter, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_allocation_out_DEMAND_UNI_DAY_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_night_commuter, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_allocation_out_DEMAND_UNI_NIGHT_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_day_resident, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_allocation_out_DEMAND_UNI_DAY_RESIDENT",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_night_resident, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_allocation_out_DEMAND_UNI_NIGHT_RESIDENT",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_uni_visitor, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_allocation_out_DEMAND_UNI_VISITOR",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_hosp_commuter, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_allocation_out_DEMAND_HOSP_COMMUTER",".csv",sep=""), na="",row.names=FALSE)
  write.csv(demand_allBP_hosp_visitor, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_allocation_out_DEMAND_HOSP_VISITOR",".csv",sep=""), na="",row.names=FALSE)
  
  
  demand_allBP_uni_day_commuter <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_DAY_COMMUTER",".csv",sep=""), na="")
  demand_allBP_uni_night_commuter <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_NIGHT_COMMUTER",".csv",sep=""), na="")
  demand_allBP_uni_day_resident <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_DAY_RESIDENT",".csv",sep=""), na="")
  demand_allBP_uni_night_resident <- read.csv( file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_NIGHT_RESIDENT",".csv",sep=""), na="")
  demand_allBP_uni_visitor <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_UNI_VISITOR",".csv",sep=""), na="")
  demand_allBP_hosp_commuter <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_HOSP_COMMUTER",".csv",sep=""), na="")
  demand_allBP_hosp_visitor <- read.csv(file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\allocation_out_DEMAND_HOSP_VISITOR",".csv",sep=""), na="")
  
  # create flattened df by bldg
  to_filter <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Input\\Parking Model Input.xlsx", sheet = "Filter")
  filter_exists <- (nrow(to_filter) != 0)
  
  if (filter_exists) {
    temp1 <- demand_allBP_uni_day_commuter[demand_allBP_uni_day_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,]
    temp2 <- demand_allBP_uni_night_commuter[demand_allBP_uni_night_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,]
    temp3 <- demand_allBP_uni_day_resident[demand_allBP_uni_day_resident$BLDG_NAME %in% to_filter$BLDG_NAME,]
    temp4 <- demand_allBP_uni_night_resident[demand_allBP_uni_night_resident$BLDG_NAME %in% to_filter$BLDG_NAME,]
    temp5 <- demand_allBP_uni_visitor[demand_allBP_uni_visitor$BLDG_NAME %in% to_filter$BLDG_NAME,]
    temp6 <- demand_allBP_hosp_commuter[demand_allBP_hosp_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,]
    temp7 <- demand_allBP_hosp_visitor[demand_allBP_hosp_visitor$BLDG_NAME %in% to_filter$BLDG_NAME,]
    
    temp1[,-1] <- rowSums(demand_allBP_uni_day_commuter[demand_allBP_uni_day_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp2[,-1] <- rowSums(demand_allBP_uni_night_commuter[demand_allBP_uni_night_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp3[,-1] <- rowSums(demand_allBP_uni_day_resident[demand_allBP_uni_day_resident$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp4[,-1] <- rowSums(demand_allBP_uni_night_resident[demand_allBP_uni_night_resident$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp5[,-1] <- rowSums(demand_allBP_uni_visitor[demand_allBP_uni_visitor$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp6[,-1] <- rowSums(demand_allBP_hosp_commuter[demand_allBP_hosp_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp7[,-1] <- rowSums(demand_allBP_hosp_visitor[demand_allBP_hosp_visitor$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    
    temp1 <- temp1[,1:2]
    temp2 <- temp2[,1:2]
    temp3 <- temp3[,1:2]
    temp4 <- temp4[,1:2]
    temp5 <- temp5[,1:2]
    temp6 <- temp6[,1:2]
    temp7 <- temp7[,1:2]
    
    flatdf_bldg_filtered <- temp1
    flatdf_bldg_filtered[,1] <- as.character(flatdf_bldg_filtered[,1])
    colnames(flatdf_bldg_filtered) <- c("BLDG_NAME","demand_uni_day_commuter")
    flatdf_bldg_filtered$demand_uni_night_commuter <- temp2[,2]
    flatdf_bldg_filtered$demand_uni_day_resident <- temp3[,2]
    flatdf_bldg_filtered$demand_uni_night_resident <- temp4[,2]
    flatdf_bldg_filtered$demand_uni_visitor <- temp5[,2]
    flatdf_bldg_filtered$demand_hosp_commuter <- temp6[,2]
    flatdf_bldg_filtered$demand_hosp_visitor <- temp7[,2]
    flatdf_bldg_filtered$demand_total_commuter <- flatdf_bldg_filtered$demand_uni_day_commuter + flatdf_bldg_filtered$demand_hosp_commuter
    flatdf_bldg_filtered$demand_total_visitor <- flatdf_bldg_filtered$demand_uni_visitor + flatdf_bldg_filtered$demand_hosp_visitor
    flatdf_bldg_filtered$demand_total_hospital <- rowSums(flatdf_bldg_filtered[,c("demand_hosp_commuter","demand_hosp_visitor")])
    flatdf_bldg_filtered$demand_total_university <- rowSums(flatdf_bldg_filtered[,c("demand_uni_day_commuter","demand_uni_day_resident","demand_uni_visitor")])
    flatdf_bldg_filtered$demand_total_day_all <- rowSums(flatdf_bldg_filtered[,c("demand_uni_day_commuter","demand_uni_day_resident","demand_uni_visitor","demand_hosp_commuter","demand_hosp_visitor")])
    
    flatdf_bldg_filtered[is.na(flatdf_bldg_filtered)] <- 0
    
    write.csv(flatdf_bldg_filtered, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg4mapping_filtered",".csv",sep=""), na="",row.names=FALSE)
    write.csv(flatdf_bldg_filtered, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\flatdf_bldg_filtered_withDemand",".csv",sep=""), na="",row.names=FALSE)
    write.csv(flatdf_bldg_filtered, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_bldg4mapping_filtered",".csv",sep=""), na="",row.names=FALSE)
    write.csv(flatdf_bldg_filtered, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_flatdf_bldg_filtered_withDemand",".csv",sep=""), na="",row.names=FALSE)
    
  }
  
  temp1 <- demand_allBP_uni_day_commuter
  temp2 <- demand_allBP_uni_night_commuter
  temp3 <- demand_allBP_uni_day_resident
  temp4 <- demand_allBP_uni_night_resident
  temp5 <- demand_allBP_uni_visitor
  temp6 <- demand_allBP_hosp_commuter
  temp7 <- demand_allBP_hosp_visitor
  
  temp1[,-1] <- rowSums(demand_allBP_uni_day_commuter[,-1],na.rm=TRUE)
  temp2[,-1] <- rowSums(demand_allBP_uni_night_commuter[,-1],na.rm=TRUE)
  temp3[,-1] <- rowSums(demand_allBP_uni_day_resident[,-1],na.rm=TRUE)
  temp4[,-1] <- rowSums(demand_allBP_uni_night_resident[,-1],na.rm=TRUE)
  temp5[,-1] <- rowSums(demand_allBP_uni_visitor[,-1],na.rm=TRUE)
  temp6[,-1] <- rowSums(demand_allBP_hosp_commuter[,-1],na.rm=TRUE)
  temp7[,-1] <- rowSums(demand_allBP_hosp_visitor[,-1],na.rm=TRUE)
  
  temp1 <- temp1[,1:2]
  temp2 <- temp2[,1:2]
  temp3 <- temp3[,1:2]
  temp4 <- temp4[,1:2]
  temp5 <- temp5[,1:2]
  temp6 <- temp6[,1:2]
  temp7 <- temp7[,1:2]
  
  flatdf_bldg_all <- temp1
  flatdf_bldg_all[,1] <- as.character(flatdf_bldg_all[,1])
  colnames(flatdf_bldg_all) <- c("BLDG_NAME","demand_uni_day_commuter")
  flatdf_bldg_all$demand_uni_night_commuter <- temp2[,2]
  flatdf_bldg_all$demand_uni_day_resident <- temp3[,2]
  flatdf_bldg_all$demand_uni_night_resident <- temp4[,2]
  flatdf_bldg_all$demand_uni_visitor <- temp5[,2]
  flatdf_bldg_all$demand_hosp_commuter <- temp6[,2]
  flatdf_bldg_all$demand_hosp_visitor <- temp7[,2]
  flatdf_bldg_all$demand_total_commuter <- flatdf_bldg_all$demand_uni_day_commuter + flatdf_bldg_all$demand_hosp_commuter
  flatdf_bldg_all$demand_total_visitor <- flatdf_bldg_all$demand_uni_visitor + flatdf_bldg_all$demand_hosp_visitor
  flatdf_bldg_all$demand_total_hospital <- rowSums(flatdf_bldg_all[,c("demand_hosp_commuter","demand_hosp_visitor")])
  flatdf_bldg_all$demand_total_university <- rowSums(flatdf_bldg_all[,c("demand_uni_day_commuter","demand_uni_day_resident","demand_uni_visitor")])
  flatdf_bldg_all$demand_total_day_all <- rowSums(flatdf_bldg_all[,c("demand_uni_day_commuter","demand_uni_day_resident","demand_uni_visitor","demand_hosp_commuter","demand_hosp_visitor")])
  
  flatdf_bldg_all[is.na(flatdf_bldg_all)] <- 0
  
  write.csv(flatdf_bldg_all, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\bldg4mapping_all",".csv",sep=""), na="",row.names=FALSE)
  write.csv(flatdf_bldg_all, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\flatdf_bldg_all_withDemand",".csv",sep=""), na="",row.names=FALSE)
  write.csv(flatdf_bldg_all, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_bldg4mapping_all",".csv",sep=""), na="",row.names=FALSE)
  write.csv(flatdf_bldg_all, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_flatdf_bldg_all_withDemand",".csv",sep=""), na="",row.names=FALSE)
  
  # create flattened df by pkng
  if (filter_exists) {
    temp1 <- demand_allBP_uni_day_commuter
    temp2 <- demand_allBP_uni_night_commuter
    temp3 <- demand_allBP_uni_day_resident
    temp4 <- demand_allBP_uni_night_resident
    temp5 <- demand_allBP_uni_visitor
    temp6 <- demand_allBP_hosp_commuter
    temp7 <- demand_allBP_hosp_visitor
    
    temp1[!temp1$BLDG_NAME %in% to_filter$BLDG_NAME,-1] <- 0
    temp2[!temp2$BLDG_NAME %in% to_filter$BLDG_NAME,-1] <- 0
    temp3[!temp3$BLDG_NAME %in% to_filter$BLDG_NAME,-1] <- 0
    temp4[!temp4$BLDG_NAME %in% to_filter$BLDG_NAME,-1] <- 0
    temp5[!temp5$BLDG_NAME %in% to_filter$BLDG_NAME,-1] <- 0
    temp6[!temp6$BLDG_NAME %in% to_filter$BLDG_NAME,-1] <- 0
    temp7[!temp7$BLDG_NAME %in% to_filter$BLDG_NAME,-1] <- 0
    
    temp1[1,-1] <- colSums(demand_allBP_uni_day_commuter[demand_allBP_uni_day_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp2[1,-1] <- colSums(demand_allBP_uni_night_commuter[demand_allBP_uni_night_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp3[1,-1] <- colSums(demand_allBP_uni_day_resident[demand_allBP_uni_day_resident$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp4[1,-1] <- colSums(demand_allBP_uni_night_resident[demand_allBP_uni_night_resident$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp5[1,-1] <- colSums(demand_allBP_uni_visitor[demand_allBP_uni_visitor$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp6[1,-1] <- colSums(demand_allBP_hosp_commuter[demand_allBP_hosp_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    temp7[1,-1] <- colSums(demand_allBP_hosp_visitor[demand_allBP_hosp_visitor$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
    
    temp1 <- temp1[1,] #udc
    temp2 <- temp2[1,] #unc
    temp3 <- temp3[1,] #udr
    temp4 <- temp4[1,] #unr
    temp5 <- temp5[1,] #uv
    temp6 <- temp6[1,] #hc
    temp7 <- temp7[1,] #hv
    
    flatdf_pkng_filtered <- temp1
    flatdf_pkng_filtered[,1] <- as.character(flatdf_pkng_filtered[,1])
    colnames(flatdf_pkng_filtered)[1] <- "type"
    flatdf_pkng_filtered$type[1] <- "demand_uni_day_commuter"
    flatdf_pkng_filtered[2,] <- temp2; flatdf_pkng_filtered$type[2] <- "demand_uni_night_commuter"
    flatdf_pkng_filtered[3,] <- temp3; flatdf_pkng_filtered$type[3] <- "demand_uni_day_resident"
    flatdf_pkng_filtered[4,] <- temp4; flatdf_pkng_filtered$type[4] <- "demand_uni_night_resident"
    flatdf_pkng_filtered[5,] <- temp5; flatdf_pkng_filtered$type[5] <- "demand_uni_visitor"
    flatdf_pkng_filtered[6,] <- temp6; flatdf_pkng_filtered$type[6] <- "demand_hosp_commuter"
    flatdf_pkng_filtered[7,] <- temp7; flatdf_pkng_filtered$type[7] <- "demand_hosp_visitor"
    
    temp <- temp1[,2:ncol(temp1)] + temp6[,2:ncol(temp6)]
    temp <- cbind(temp1[,1], temp)
    flatdf_pkng_filtered[8,] <- temp; flatdf_pkng_filtered$type[8] <- "demand_total_commuter"
    
    temp <- temp5[,2:ncol(temp5)] + temp7[,2:ncol(temp7)]
    temp <- cbind(temp1[,1], temp)
    flatdf_pkng_filtered[9,] <- temp; flatdf_pkng_filtered$type[9] <- "demand_total_visitor"
    
    temp <- temp6[,2:ncol(temp6)] + temp7[,2:ncol(temp7)]
    temp <- cbind(temp1[,1], temp)
    flatdf_pkng_filtered[10,] <- temp; flatdf_pkng_filtered$type[10] <- "demand_total_hospital" #hosp commuter, hosp visitor
    
    temp <- temp1[,2:ncol(temp1)] + temp5[,2:ncol(temp5)] + temp3[,2:ncol(temp3)]
    temp <- cbind(temp1[,1], temp)
    flatdf_pkng_filtered[11,] <- temp; flatdf_pkng_filtered$type[11] <- "demand_total_university" #uni visitor, day uni commuter, day resident
    
    temp <- temp1[,2:ncol(temp1)] + temp6[,2:ncol(temp6)] + temp5[,2:ncol(temp5)] + temp7[,2:ncol(temp7)] + temp3[,2:ncol(temp3)]
    temp <- cbind(temp1[,1], temp)
    flatdf_pkng_filtered[12,] <- temp; flatdf_pkng_filtered$type[12] <- "demand_total_day_all" #uni commuter, hosp commuter, uni visitor, hosp visitor, resident ALL day
    
    flatdf_pkng_filtered <- data.frame(t(flatdf_pkng_filtered))
    flatdf_pkng_filtered <- setDT(flatdf_pkng_filtered, keep.rownames = TRUE)[]
    
    colnames(flatdf_pkng_filtered) <- colnames(flatdf_bldg_all)
    colnames(flatdf_pkng_filtered)[1] <- "LOT"
    flatdf_pkng_filtered <- flatdf_pkng_filtered[2:nrow(flatdf_pkng_filtered),]
    
    flatdf_pkng_filtered[] <- lapply(flatdf_pkng_filtered, as.character)
    flatdf_pkng_filtered <- transform(flatdf_pkng_filtered, demand_uni_day_commuter = as.numeric(demand_uni_day_commuter), demand_uni_night_commuter = as.numeric(demand_uni_night_commuter),
                                      demand_uni_day_resident = as.numeric(demand_uni_day_resident),demand_uni_night_resident = as.numeric(demand_uni_night_resident),
                                      demand_uni_visitor = as.numeric(demand_uni_visitor),demand_hosp_commuter = as.numeric(demand_hosp_commuter),
                                      demand_hosp_visitor = as.numeric(demand_hosp_visitor), demand_total_commuter = as.numeric(demand_total_commuter),
                                      demand_total_visitor = as.numeric(demand_total_visitor),demand_total_hospital = as.numeric(demand_total_hospital),
                                      demand_total_university = as.numeric(demand_total_university),demand_total_day_all = as.numeric(demand_total_day_all))
    
    flatdf_pkng_filtered <- data.frame(flatdf_pkng_filtered)
    flatdf_pkng_filtered[is.na(flatdf_pkng_filtered)] <- 0
    flatdf_pkng_filtered$LOT <- gsub("L[.]","L-",flatdf_pkng_filtered$LOT)
    flatdf_pkng_filtered$LOT <- gsub("S[.]","S-",flatdf_pkng_filtered$LOT)
    flatdf_pkng_filtered$LOT <- gsub("[.][.][.]"," @ ",flatdf_pkng_filtered$LOT)
    flatdf_pkng_filtered$LOT <- gsub("[.][.][.]"," @ ",flatdf_pkng_filtered$LOT)
    
    # remove all non-alphanumerics to better match/join later
    for (i in 1:nrow(flatdf_pkng_filtered)) {flatdf_pkng_filtered$LOT[i] <- str_replace_all(flatdf_pkng_filtered$LOT[i], "[[:punct:]]", " ")}
    flatdf_pkng_filtered$LOT <- gsub("X717", "717", flatdf_pkng_filtered$LOT)
    
    write.csv(flatdf_pkng_filtered, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\flatdf_pkng_filtered",".csv",sep=""), na="",row.names=FALSE)
    write.csv(flatdf_pkng_filtered, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_flatdf_pkng_filtered",".csv",sep=""), na="",row.names=FALSE)
    
  }
  temp1 <- demand_allBP_uni_day_commuter
  temp2 <- demand_allBP_uni_night_commuter
  temp3 <- demand_allBP_uni_day_resident
  temp4 <- demand_allBP_uni_night_resident
  temp5 <- demand_allBP_uni_visitor
  temp6 <- demand_allBP_hosp_commuter
  temp7 <- demand_allBP_hosp_visitor
  
  sum(demand_allBP_uni_day_commuter[demand_allBP_uni_day_commuter$BLDG_NAME %in% to_filter$BLDG_NAME,-1],na.rm=TRUE)
  
  temp1[1,-1] <- colSums(demand_allBP_uni_day_commuter[,-1],na.rm=TRUE)
  temp2[1,-1] <- colSums(demand_allBP_uni_night_commuter[,-1],na.rm=TRUE)
  temp3[1,-1] <- colSums(demand_allBP_uni_day_resident[,-1],na.rm=TRUE)
  temp4[1,-1] <- colSums(demand_allBP_uni_night_resident[,-1],na.rm=TRUE)
  temp5[1,-1] <- colSums(demand_allBP_uni_visitor[,-1],na.rm=TRUE)
  temp6[1,-1] <- colSums(demand_allBP_hosp_commuter[,-1],na.rm=TRUE)
  temp7[1,-1] <- colSums(demand_allBP_hosp_visitor[,-1],na.rm=TRUE)
  
  temp1 <- temp1[1,]
  temp2 <- temp2[1,]
  temp3 <- temp3[1,]
  temp4 <- temp4[1,]
  temp5 <- temp5[1,]
  temp6 <- temp6[1,]
  temp7 <- temp7[1,]
  
  flatdf_pkng_all <- temp1
  colnames(flatdf_pkng_all)[1] <- "type"
  flatdf_pkng_all[,1] <- as.character(flatdf_pkng_all[,1])
  flatdf_pkng_all$type[1] <- "demand_uni_day_commuter"
  flatdf_pkng_all[2,] <- temp2; flatdf_pkng_all$type[2] <- "demand_uni_night_commuter"
  flatdf_pkng_all[3,] <- temp3; flatdf_pkng_all$type[3] <- "demand_uni_day_resident"
  flatdf_pkng_all[4,] <- temp4; flatdf_pkng_all$type[4] <- "demand_uni_night_resident"
  flatdf_pkng_all[5,] <- temp5; flatdf_pkng_all$type[5] <- "demand_uni_visitor"
  flatdf_pkng_all[6,] <- temp6; flatdf_pkng_all$type[6] <- "demand_hosp_commuter"
  flatdf_pkng_all[7,] <- temp7; flatdf_pkng_all$type[7] <- "demand_hosp_visitor"
  
  temp <- temp1[,2:ncol(temp1)] + temp6[,2:ncol(temp6)]
  temp <- cbind(temp1[,1], temp)
  flatdf_pkng_all[8,] <- temp; flatdf_pkng_all$type[8] <- "demand_total_commuter"
  
  temp <- temp5[,2:ncol(temp5)] + temp7[,2:ncol(temp7)]
  temp <- cbind(temp1[,1], temp)
  flatdf_pkng_all[9,] <- temp; flatdf_pkng_all$type[9] <- "demand_total_visitor"
  
  temp <- temp6[,2:ncol(temp6)] + temp7[,2:ncol(temp7)]
  temp <- cbind(temp1[,1], temp)
  flatdf_pkng_all[10,] <- temp; flatdf_pkng_all$type[10] <- "demand_total_hospital" #hosp commuter, hosp visitor
  
  temp <- temp1[,2:ncol(temp1)] + temp5[,2:ncol(temp5)] + temp3[,2:ncol(temp3)]
  temp <- cbind(temp1[,1], temp)
  flatdf_pkng_all[11,] <- temp; flatdf_pkng_all$type[11] <- "demand_total_university" #uni visitor, day uni commuter, day resident
  
  temp <- temp1[,2:ncol(temp1)] + temp6[,2:ncol(temp6)] + temp5[,2:ncol(temp5)] + temp7[,2:ncol(temp7)] + temp3[,2:ncol(temp3)]
  temp <- cbind(temp1[,1], temp)
  flatdf_pkng_all[12,] <- temp; flatdf_pkng_all$type[12] <- "demand_total_day_all" #uni commuter, hosp commuter, uni visitor, hosp visitor, resident ALL day
  
  flatdf_pkng_all <- data.frame(t(flatdf_pkng_all))
  flatdf_pkng_all <- setDT(flatdf_pkng_all, keep.rownames = TRUE)[]
  
  colnames(flatdf_pkng_all) <- colnames(flatdf_bldg_all)
  colnames(flatdf_pkng_all)[1] <- "LOT"
  flatdf_pkng_all <- flatdf_pkng_all[2:nrow(flatdf_pkng_all),]
  
  flatdf_pkng_all[] <- lapply(flatdf_pkng_all, as.character)
  flatdf_pkng_all <- transform(flatdf_pkng_all, demand_uni_day_commuter = as.numeric(demand_uni_day_commuter), demand_uni_night_commuter = as.numeric(demand_uni_night_commuter),
                               demand_uni_day_resident = as.numeric(demand_uni_day_resident),demand_uni_night_resident = as.numeric(demand_uni_night_resident),
                               demand_uni_visitor = as.numeric(demand_uni_visitor),demand_hosp_commuter = as.numeric(demand_hosp_commuter),
                               demand_hosp_visitor = as.numeric(demand_hosp_visitor), demand_total_commuter = as.numeric(demand_total_commuter),
                               demand_total_visitor = as.numeric(demand_total_visitor),demand_total_hospital = as.numeric(demand_total_hospital),
                               demand_total_university = as.numeric(demand_total_university),demand_total_day_all = as.numeric(demand_total_day_all))
  
  flatdf_pkng_all <- data.frame(flatdf_pkng_all)
  flatdf_pkng_all[is.na(flatdf_pkng_all)] <- 0
  
  # remove all non-alphanumerics to better match/join later
  for (i in 1:nrow(flatdf_pkng_all)) {flatdf_pkng_all$LOT[i] <- str_replace_all(flatdf_pkng_all$LOT[i], "[[:punct:]]", " ")}
  flatdf_pkng_all$LOT <- gsub("X717", "717", flatdf_pkng_all$LOT)
  
  write.csv(flatdf_pkng_all, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\flatdf_pkng_all",".csv",sep=""), na="",row.names=FALSE)
  write.csv(flatdf_pkng_all, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_flatdf_pkng_all",".csv",sep=""), na="",row.names=FALSE)
  
  sum(flatdf_pkng_all[,-1])
  sum(flatdf_pkng_filtered[,-1])
  
  sum(flatdf_bldg_all[,-1])
  sum(flatdf_bldg_filtered[,-1])
  
  
  # calculate utilization rates for maps ----------------------------------------------------------------------------------------------------
  ###################################
  
  p4shapeu <- pkng_u[,c("Facilities.Quad.Bldg..","Facilities.Lot.Name","P.TS.Lot..","P.TS.Lot.Name",
                        "visitor_spaces_total","day_commuter_spaces_total" ,"day_resident_spaces_total","night_commuter_spaces_total","night_resident_spaces_total")]
  p4shapeh <- pkng_h[,c("Facilities.Quad.Bldg..","Facilities.Lot.Name","P.TS.Lot..","P.TS.Lot.Name",
                        "visitor_spaces_total","commuter_spaces_total")]
  
  colnames(p4shapeu)[5:9] <- paste("uni_",colnames(p4shapeu)[5:9],sep="")
  colnames(p4shapeh)[5:6] <- paste("hosp_",colnames(p4shapeh)[5:6],sep="")
  
  ptotals <- merge(p4shapeu,p4shapeh, all.x=TRUE, all.y=TRUE)
  
  ptotals[is.na(ptotals)] <- 0
  
  ptotals[,4] <- as.character(ptotals[,4])
  
  ptotals[,c("total_commuter_spaces_total")] <- ptotals$uni_day_commuter_spaces_total + ptotals$hosp_commuter_spaces_total
  ptotals[,c("total_visitor_spaces_total")] <- ptotals$uni_visitor_spaces_total + ptotals$hosp_visitor_spaces_total
  ptotals[,c("total_hospital_spaces_total")] <- ptotals$hosp_commuter_spaces_total + ptotals$hosp_visitor_spaces_total
  ptotals[,c("total_university_spaces_total")] <- ptotals$uni_day_commuter_spaces_total + ptotals$uni_day_resident_spaces_total + ptotals$uni_visitor_spaces_total
  ptotals[,c("total_day_all_spaces_total")] <- ptotals$uni_day_commuter_spaces_total + ptotals$uni_day_resident_spaces_total + ptotals$uni_visitor_spaces_total + ptotals$hosp_commuter_spaces_total + ptotals$hosp_visitor_spaces_total
  
  
  for (i in 1:nrow(ptotals)) {ptotals$P.TS.Lot.Name[i] <- str_replace_all(ptotals$P.TS.Lot.Name[i], "[[:punct:]]", " ")}
  
  
  if (filter_exists) {
    flatdf_pkng_filtered_withUtil <- left_join(flatdf_pkng_filtered,ptotals[,4:16], by = c("LOT" = 'P.TS.Lot.Name'))
    types <- gsub("demand_","",names(flatdf_pkng_filtered_withUtil)[str_detect(names(flatdf_pkng_filtered_withUtil), "demand")])
    for (i in 1:length(types)) {
      flatdf_pkng_filtered_withUtil[,c(paste("utilization_",types[i],sep=""))] <- flatdf_pkng_filtered_withUtil[,c(paste("demand_",types[i],sep=""))]/flatdf_pkng_filtered_withUtil[,c(paste(types[i],"_spaces_total",sep=""))]
    }
  }
  
  flatdf_pkng_all_withUtil <- left_join(flatdf_pkng_all,ptotals[,4:16], by = c("LOT" = 'P.TS.Lot.Name'))
  types <- gsub("demand_","",names(flatdf_pkng_all_withUtil)[str_detect(names(flatdf_pkng_all_withUtil), "demand")])
  for (i in 1:length(types)) {
    flatdf_pkng_all_withUtil[,c(paste("utilization_",types[i],sep=""))] <- flatdf_pkng_all_withUtil[,c(paste("demand_",types[i],sep=""))]/flatdf_pkng_all_withUtil[,c(paste(types[i],"_spaces_total",sep=""))]
  }
  
  write.csv(flatdf_pkng_filtered_withUtil, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng4mapping_filtered.csv",sep=""), na="",row.names=FALSE)
  write.csv(flatdf_pkng_all_withUtil, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\pkng4mapping_all.csv",sep=""), na="",row.names=FALSE)
  write.csv(flatdf_pkng_filtered_withUtil, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_pkng4mapping_filtered.csv",sep=""), na="",row.names=FALSE)
  write.csv(flatdf_pkng_all_withUtil, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_pkng4mapping_all.csv",sep=""), na="",row.names=FALSE)
  
  
  # Clear workspace -------------------------------------------------------------------------------------------------------------
  rm(list = ls()) 
}
