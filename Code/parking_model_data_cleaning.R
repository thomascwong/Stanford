parking_model_data_cleaning <- function(instance_name, project_list_instance) {
  # project instance variables
  add_bldg <- any(!is.na(project_list_instance[project_list_instance$Action == "Add","BLDG_NAME"]),
                  !is.na(project_list_instance[project_list_instance$Action == "Add","BLDG_ID"]))
  add_bldg_u <- any(!is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == FALSE,"BLDG_NAME"]),
                  !is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == FALSE,"BLDG_ID"]))
  add_bldg_h <- any(!is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == TRUE,"BLDG_NAME"]),
                  !is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == TRUE,"BLDG_ID"]))
  add_lot_u <- any(!is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == FALSE,"P&TS.Lot.Name"]))
  add_lot_h <- any(!is.na(project_list_instance[project_list_instance$Action == "Add" & project_list_instance$HOSPITAL == TRUE,"P&TS.Lot.Name"]))
  rm_bldg <- any(!is.na(project_list_instance[project_list_instance$Action == "Remove","BLDG_NAME"]),
                  !is.na(project_list_instance[project_list_instance$Action == "Remove","BLDG_ID"]))
  rm_lot_u <- any(!is.na(project_list_instance[project_list_instance$Action == "Remove" & project_list_instance$HOSPITAL == FALSE,"P&TS.Lot.Name"]))
  rm_lot_h <- any(!is.na(project_list_instance[project_list_instance$Action == "Remove" & project_list_instance$HOSPITAL == TRUE,"P&TS.Lot.Name"]))
  change_bldg <- any(!is.na(project_list_instance[project_list_instance$Action == "Change","BLDG_NAME"]),
                  !is.na(project_list_instance[project_list_instance$Action == "Change","BLDG_ID"]))
  change_lot_u <- any(!is.na(project_list_instance[project_list_instance$Action == "Change" & project_list_instance$HOSPITAL == FALSE,"P&TS.Lot.Name"]))
  change_lot_h <- any(!is.na(project_list_instance[project_list_instance$Action == "Change" & project_list_instance$HOSPITAL == TRUE,"P&TS.Lot.Name"]))
  
  # LOAD PACKAGES
  require(openxlsx)
  require(dplyr)
  require(data.table)
  require(gdata)
  require(reshape)
  
  print(paste("----------Cleaning data for instance:", instance_name))
  
  # LOAD DATA
  # read in parking datasets
  pknginv <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\parking_inventory_05-30-16_for_TW-RM_Searsville Lot Update.xlsx",1)
  pkngutil <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\data set\\utilization\\win16_plus_report_unsorted_03-28-16_330PM.xlsx",1)
  
  # read in building datasets
  broomsum <- read.xls("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Bldg-Dept-RmType-Summary.xls",2)
  
  # read in 4D datasets
  rdefeed <- read.csv("S:\\Groups\\Transportation\\4d\\4d imports\\15-16\\20160517\\rde_harp_parking_data_v1.txt", sep="\t",header=FALSE)
  table_customer <- read.csv("S:\\Groups\\Transportation\\4D\\4D data\\CClub Audit files\\FY15-16\\CC Audit data_2016-05-02.txt", sep="\t")
  table_ID <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Table_IDs.txt", sep="\t")
  
  # read in shape datasets
  sbldg <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Buildings_withPoints.txt")
  spkng <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking_Lots_withBeckman.txt")
  
  # read in manual datasets
  table_class2type <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="table_class2type")
  bldg2updateSQFT <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="bldg2updateSQFT")
  table_pkng2inventory <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="table_pkng2inventory")
  table_permits <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="table_permits")
  table_hospbldg <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="table_hospbldg")
  table_hosppkng <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="table_hosppkng")
  table_pkng2pshape <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="table_pkng2pshape")
  btypeassignments <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="btypeassignments")
  rdecodes <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="rdecodes")
  table_rde2bldgname <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="table_rde2bldgname")
  names4bldgs <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="names4bldgs")
  input2df <- read.xlsx("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Tables_ParkingModel.xlsx",sheet="input2df")
  
  uem_drive_commute_freq <- 0.917 # from commute survey
  exist_genrates <- file.exists("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\generation_rates.csv")
  exist_utilrates <- file.exists("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\utilization_rates.csv")
  
  # CLEAN, TRANSFORM BUILDING DATA ----------------------------------------------------------------------------------------------------------
  
  # transform input data fields

  for (i in 1:nrow(input2df)) {
    if (!is.na(input2df$name_df[i])) {
      colnames(project_list_instance)[colnames(project_list_instance) == input2df$name_excel[i]] <- input2df$name_df[i]
    }
  }
  
  # update names for buildings in shape data
  sbldg$Key <- paste(sbldg$Longitude,"-",sbldg$Latitude,sep="")
  sbldg <- left_join(sbldg, names4bldgs, by = c("Key" = "Key"))
  sbldg$NAME <- sbldg$ADJ_BLDG_NAME
  sbldg <- sbldg[,-which(names(sbldg) %in% c("Key","ADJ_BLDG_NAME"))]
  
  # merge some datasets to assign stanford IDs to the rde data
  customer <- suppressWarnings(left_join(table_customer, table_ID, by = c("CUSTOMER_ID" = "ï..Customer_ID")))
  customer <- suppressWarnings(left_join(customer, table_class2type, by = c("CLASS_LEVEL" = "CLASS_LEVEL")))
  
  rde <- suppressWarnings(left_join(rdefeed, rdecodes, by = c("V3" = "CURRENT_RESIDENCE_CODE_ABBRV")))
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  customer[] <- lapply(customer, as.character)
  customer$Stanford_ID <- substrRight(customer$Stanford_ID , 7)
  rde[] <- lapply(rde, as.character)
  
  rde <- left_join(rde, customer, by = c("V1" = "Stanford_ID"))
  
  broomsum[] <- lapply(broomsum, as.character)
  df_bldg <- broomsum
  df_bldg[] <- lapply(df_bldg, as.character)
  df_bldg$RM_TYPE_TOTAL_SQFT <- gsub(",","",df_bldg$RM_TYPE_TOTAL_SQFT)
  df_bldg$RM_TYPE_TOTAL_SQFT <- as.numeric(df_bldg$RM_TYPE_TOTAL_SQFT)
  
  # assign status to rooms
  # unusable list removes stadium, library, museum, structural/hvac, bed and sleep spaces
  # sum(df_bldg$RM_TYPE_TOTAL_SQFT)
  df_bldg <- left_join(df_bldg, btypeassignments, all.y = TRUE, by = c("ROOM_TYPE_DESCRIPTION" = "ROOM_TYPE_DESCRIPTION"))
  
  # sum(df_bldg$RM_TYPE_TOTAL_SQFT[df_bldg$REMOVE.OLD == FALSE])
  
  # xtabs(RM_TYPE_TOTAL_SQFT~PARKING.DEMAND.TYPE,df_bldg)
  # xtabs(RM_TYPE_TOTAL_SQFT~PARKING.DEMAND.TYPE,df_bldg[df_bldg$REMOVE.OLD == FALSE,])
  # 
  
  # xtabs(~STUDENT_TYPE,rde)
  
  # reshape df from unique building-room to unique building 
  df4cast <- df_bldg[df_bldg$REMOVE.OLD == FALSE,]
  df4cast <- df4cast[,c(1,2,3,4,6,9)]
  
  mdf <- melt(df4cast, id=c("BLDG_ID","BLDG_NAME","BLDG_ADDRESS_1","BLDG_ADDRESS_2","PARKING.DEMAND.TYPE","RM_TYPE_TOTAL_SQFT"))
  mdf$RM_TYPE_TOTAL_SQFT <- as.numeric(mdf$RM_TYPE_TOTAL_SQFT)
  castdf <- suppressMessages(cast(mdf,BLDG_ID+BLDG_NAME+BLDG_ADDRESS_1+BLDG_ADDRESS_2~PARKING.DEMAND.TYPE, sum))
  
  # colSums(castdf)
  
  # only keep buildings that match the shape data
  castbdfXshapes <- castdf[castdf$BLDG_ID %in% intersect(castdf$BLDG_ID, sbldg$BLDG_ID),]
  # colSums(castbdfXshapes[,5:7])
  
  # update building sqft (changes are set to zero now until after generation rates are calculated)
  matches <- castbdfXshapes$BLDG_NAME[castbdfXshapes$BLDG_NAME %in% bldg2updateSQFT$BLDG_NAME_KEY]
  
  for (i in matches) {
    suppressWarnings(new_com <- bldg2updateSQFT$UPDATE_COM[bldg2updateSQFT$BLDG_NAME_KEY == i])
    suppressWarnings(new_res <- bldg2updateSQFT$UPDATE_RES[bldg2updateSQFT$BLDG_NAME_KEY == i])
    suppressWarnings(new_vis <- bldg2updateSQFT$UPDATE_VIS[bldg2updateSQFT$BLDG_NAME_KEY == i])
    suppressWarnings(if (!is.na(new_com)) {castbdfXshapes$COM[castbdfXshapes$BLDG_NAME == i] <- 0})
    suppressWarnings(if (!is.na(new_res)) {castbdfXshapes$RES[castbdfXshapes$BLDG_NAME == i] <- 0})
    suppressWarnings(if (!is.na(new_vis)) {castbdfXshapes$VIS[castbdfXshapes$BLDG_NAME == i] <- 0})
  }
  
  # add beds to building data
  table_beds <- cast(data.frame(xtabs(~CURRENT_RESIDENCE_CODE_NAME+STUDENT_TYPE,rde)),CURRENT_RESIDENCE_CODE_NAME~STUDENT_TYPE,sum)
  colSums(table_beds[,-1])
  
  table_beds[,1] <- as.character(table_beds[,1])
  table_beds$CURRENT_RESIDENCE_CODE_NAME <- trim(table_beds$CURRENT_RESIDENCE_CODE_NAME)
  castbdfXshapes2 <- left_join(castbdfXshapes,table_rde2bldgname, by = c("BLDG_NAME" = "BLDG_NAME"))
  castbdfXshapes3 <- suppressWarnings(group_by(castbdfXshapes2, CURRENT_RESIDENCE_CODE_NAME) %>% mutate(RES_percent = RES/sum(RES)))
  castbdfXshapes4 <- left_join(castbdfXshapes3,table_beds, by = c("CURRENT_RESIDENCE_CODE_NAME" = "CURRENT_RESIDENCE_CODE_NAME"))
  castbdfXshapes4$BEDS_GRAD <- castbdfXshapes4$GR * castbdfXshapes4$RES_percent
  castbdfXshapes4$BEDS_POSTDOC <- castbdfXshapes4$PD * castbdfXshapes4$RES_percent
  castbdfXshapes4$BEDS_NONFRESHMAN_UNDERGRAD <- castbdfXshapes4$UG * castbdfXshapes4$RES_percent
  castbdfXshapes4$BEDS_FRESHMAN <- castbdfXshapes4$`UG-FR` * castbdfXshapes4$RES_percent
  # colSums(castbdfXshapes4[,c(10:13)], na.rm=TRUE)
  # castbdfXshapes4[,c(10:13)] <- round(castbdfXshapes4[,c(10:13)],0)
  # colSums(castbdfXshapes4[,c(10:13)], na.rm=TRUE)
  # write.csv(castbdfXshapes3,"castbdfXshapes3.csv")
  # write.csv(castbdfXshapes4,"castbdfXshapes4.csv")
  # write.csv(table_beds,"table_beds123.csv")
  
  # sum(castbdfXshapes4$BEDS_GRAD,na.rm=TRUE)
  # sum(table_beds$GR,na.rm=1)
  # castbdfXshapes4[castbdfXshapes4$BLDG_NAME == "KAIROS",]
  
  df_bldg_sqftbeds <- castbdfXshapes4[,-9]
  


  # assign buildings to hospital
  df_bldg_sqftbeds[,"HOSPITAL"] <- FALSE
  df_bldg_sqftbeds$HOSPITAL[df_bldg_sqftbeds$BLDG_NAME %in% table_hospbldg$BLDG_NAME] <- TRUE
  # update building names
  
  # Parking Model Input Edits for Buildings
  if (add_bldg) {
    temp_p <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$BLDG_NAME),]
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      temp_add <- df_bldg_sqftbeds[0,]
      for (j in intersect(names(temp_add), colnames(temp_p_r)[!is.na(temp_p_r)])) {
        temp_add[1,j] <- temp_p_r[1,j]
      }
      temp_add$BLDG_ID <- as.character(as.numeric(gsub("-","",max(df_bldg_sqftbeds$BLDG_ID, na.rm=TRUE))) + 1)
      df_bldg_sqftbeds <- rbind(df_bldg_sqftbeds, temp_add)
    }
  }
  
  if (change_bldg) {
    temp_p <- project_list_instance[project_list_instance$Action == "Change" & !is.na(project_list_instance$BLDG_NAME),]
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      temp_change <- df_bldg_sqftbeds[df_bldg_sqftbeds$BLDG_NAME == temp_p_r$BLDG_NAME,]
      temp_change$COM
      for (j in intersect(names(temp_change), colnames(temp_p_r)[!is.na(temp_p_r)])) {
        temp_change[1,j] <- temp_p_r[1,j]
      }
      temp_change$COM
      df_bldg_sqftbeds[df_bldg_sqftbeds$BLDG_NAME == temp_p_r$BLDG_NAME,] <- temp_change
    }
  }
  
  if (rm_bldg) {
    temp_p <- project_list_instance[project_list_instance$Action == "Remove" & !is.na(project_list_instance$BLDG_NAME),]
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      if (!is.na(temp_p_r$BLDG_ID)) {
        df_bldg_sqftbeds <- df_bldg_sqftbeds[df_bldg_sqftbeds$BLDG_ID != temp_p_r$BLDG_ID,]
      } else if (!is.na(temp_p_r$BLDG_NAME)) {
        df_bldg_sqftbeds <- df_bldg_sqftbeds[df_bldg_sqftbeds$BLDG_ID != temp_p_r$BLDG_NAME,]
      }
    }
  }
  
  df_bldg_sqftbeds$BEDS_TOTAL <- rowSums(df_bldg_sqftbeds[,c("BEDS_GRAD","BEDS_NONFRESHMAN_UNDERGRAD")])
  # sum(df_bldg_sqftbeds$BEDS_TOTAL,na.rm=TRUE)
  # colSums(df_bldg_sqftbeds[,c(9:13)],na.rm=TRUE)
  
  # df_bldg_sqftbeds[grep("ESCONDIDO", df_bldg_sqftbeds$BLDG_NAME),c(1,2,6,9,10:18)]
  
  # CLEAN, TRANSFORM PARKING UTIL DATA ------------------------------------------------------------------------------------------------------
  pu <- pkngutil[,-c(1:4,6,21:23)]
  pu[2,c(is.na(pu[2,]))] <- pu[1,c(is.na(pu[2,]))] # combine first two rows
  
  puavg <- pu[grep("AVERAGE",pu$X5),]
  colnames(puavg) <- pu[2,]
  puavg <- cbind("Parking_Name" = gsub(" - AVERAGE","",puavg$'Parking Area'), puavg)
  puavg <- puavg[,-2]
  puavg[,-1] <- lapply(puavg[,-1], as.numeric)
  i <- sapply(puavg, is.factor); puavg[i] <- lapply(puavg[i], as.character); rm(i) #change factors to characters
  
  # combine Bowdoin lots in util
  temp1 <- puavg[puavg$Parking_Name == "Bowdoin East",]
  temp2 <- puavg[puavg$Parking_Name == "Bowdoin West",]
  temp <- temp1[,-1] + temp2[,-1]
  temp <- cbind(Parking_Name = "Bowdoin",temp)
  colnames(temp) <- colnames(puavg)
  puavg[puavg$Parking_Name == "Bowdoin East",] <- temp; rm(temp1, temp2, temp)
  puavg[puavg$Parking_Name == "1",1] <- "Bowdoin"
  puavg <- puavg[!puavg$Parking_Name == "Bowdoin West",]
  
  # add key field to utilization to match inventory
  putil <- cbind(Inventory_Name="",puavg)
  putil$Inventory_Name <- lapply(putil$Inventory_Name, as.character)
  
  for (i in 1:nrow(table_pkng2inventory)) {
    putil$Inventory_Name[putil$Parking_Name==table_pkng2inventory$Utilization_Name[i]] <- table_pkng2inventory$Inventory_Name[i]
  }
  
  # CLEAN, TRANSFORM PARKING INV DATA
  # split inventory by university and hospital
  which(pknginv$`P&TS.Lot.Name` == "UNIVERSITY SUBTOTAL")
  pknginvhosp <- pknginv[-c(1:(which(pknginv$`P&TS.Lot.Name` == "UNIVERSITY SUBTOTAL")+1)),]
  pknginvhosp <- head(pknginvhosp,-3)
  pknginvuni <- pknginv[c(2:(which(pknginv$`P&TS.Lot.Name` == "UNIVERSITY SUBTOTAL")-1)),]
  
  temphosp <- pknginv[pknginv$`P&TS.Lot.Name` %in% table_hosppkng$Inventory_Name,] # split shared lots by percent based on dwight's spot check
  
  # temphosp[,c(11:23)]
  
  temphosp <- temphosp[order(table_hosppkng$Inventory_Name),]
  # table_hosppkng$Inventory_Name
  # temphosp$`P&TS.Lot.Name`
  
  for (i in 1:nrow(temphosp)) {
    temphosp[i,c(11:23)] <- round(temphosp[i,c(11:23)] * table_hosppkng$Percent_Hospital[i],0)
  }
  
  # temphosp[,c(11:23)]
  # sum(temphosp[,c(11:23)])
  
  pknginvhosp <- rbind(pknginvhosp,temphosp)
  
  pknginv[pknginv$`P&TS.Lot.Name` %in% table_hosppkng$Inventory_Name,][,c(11:23)]
  temphosp[,c(11:23)]
  tempuni <- pknginv[pknginv$`P&TS.Lot.Name` %in% table_hosppkng$Inventory_Name,]
  tempuni[,c(11:23)] <- pknginv[pknginv$`P&TS.Lot.Name` %in% table_hosppkng$Inventory_Name,][,c(11:23)] - temphosp[order(temphosp[,c(6)]),c(11:23)]
  tempuni[,c(11:23)]
  
  pknginvuni[pknginvuni$`P&TS.Lot.Name` %in% table_hosppkng$Inventory_Name,][,c(11:23)] <- tempuni[,c(11:23)]
  
  # Parking Model Input Edits for Parking Lots - University
  # print(dim(pknginvuni))
  if (add_lot_u) {
    temp_p <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == FALSE,]
    write.csv(temp_p, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_added_lots_u",".csv",sep=""), na="",row.names=FALSE)
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      temp_add <- pknginvuni[0,]
      temp_p_r[,-c(1:12)][is.na(temp_p_r[,-c(1:12)])] <- 0
      for (j in intersect(names(temp_add), colnames(temp_p_r))) {
        temp_add[1,j] <- temp_p_r[1,j]
      }
      pknginvuni <- rbind(pknginvuni, temp_add)
    }
  }
  # print(dim(pknginvuni))
  
  if (change_lot_u) {
    temp_p <- project_list_instance[project_list_instance$Action == "Change" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == FALSE,]
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      temp_change <- pknginvuni[pknginvuni$`P&TS.Lot.Name` == temp_p_r$`P&TS.Lot.Name`,]
      for (j in intersect(names(temp_change), colnames(temp_p_r)[!is.na(temp_p_r)])) {
        temp_change[1,j] <- temp_p_r[1,j]
      }
      pknginvuni[pknginvuni$`P&TS.Lot.Name` == temp_p_r$`P&TS.Lot.Name`,] <- temp_change
    }
  }
  
  if (rm_lot_u) {
    temp_p <- project_list_instance[project_list_instance$Action == "Remove" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == FALSE,]
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      if (!is.na(temp_p_r$`P&TS.Lot.Name`)) {
        pknginvuni <- pknginvuni[pknginvuni$`P&TS.Lot.Name` != temp_p_r$`P&TS.Lot.Name`,]
      }
    }
  }
  # print(dim(pknginvuni))
  
  # Parking Model Input Edits for Parking Lots - Hospital
  if (add_lot_h) {
    temp_p <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == TRUE,]
    write.csv(temp_p, file = paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"_added_lots_h",".csv",sep=""), na="",row.names=FALSE)
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      temp_add <- pknginvhosp[0,]
      temp_p_r[,-c(1:12)][is.na(temp_p_r[,-c(1:12)])] <- 0
      for (j in intersect(names(temp_add), colnames(temp_p_r))) {
        temp_add[1,j] <- temp_p_r[1,j]
      }
      pknginvhosp <- rbind(pknginvhosp, temp_add)
    }
  }
  
  if (change_lot_h) {
    temp_p <- project_list_instance[project_list_instance$Action == "Change" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == TRUE,]
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      temp_change <- pknginvhosp[pknginvhosp$`P&TS.Lot.Name` == temp_p_r$`P&TS.Lot.Name`,]
      for (j in intersect(names(temp_change), colnames(temp_p_r)[!is.na(temp_p_r)])) {
        temp_change[1,j] <- temp_p_r[1,j]
      }
      pknginvhosp[pknginvhosp$`P&TS.Lot.Name` == temp_p_r$`P&TS.Lot.Name`,] <- temp_change
    }
  }
  
  if (rm_lot_h) {
    temp_p <- project_list_instance[project_list_instance$Action == "Remove" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == TRUE,]
    for (i in 1:nrow(temp_p)) {
      temp_p_r <- temp_p[i,]
      if (!is.na(temp_p_r$`P&TS.Lot.Name`)) {
        pknginvhosp <- pknginvhosp[pknginvhosp$`P&TS.Lot.Name` != temp_p_r$`P&TS.Lot.Name`,]
      }
    }
  }
  
  # aggregate university inventory df by demand types (commuter, resident, visitor)
  
  # calculate day peak (higher commute demand) and night peak (higher resident demand) ratios
  
  # calculate grad to undergrad permit ratio. Use this to split the resident parking demand
  temp3 <- left_join(table_permits, table_class2type, by = c("Class_Level" = "CLASS_LEVEL"))
  proportion_grad_permit <- table(temp3[temp3$Group=="R",5])["GR"]/(sum(table(temp3[temp3$Group=="R",5])["GR"],table(temp3[temp3$Group=="R",5])["UG"]))
  
  # calculate day and night demands
  tbpmt <- data.frame(xtabs(~Group+Customer_Affil,table_permits))
  peak_commuter_demand <- round(sum(tbpmt[tbpmt$Group %in% c("A","C")& !tbpmt$Customer_Affil %in% c("HEM","RES","EME"),]$Freq) * uem_drive_commute_freq,0)
  # peak_commuter_demand
  peak_resident_demand <- (sum(table(temp3[temp3$Group=="R",5])["GR"],table(temp3[temp3$Group=="R",5])["UG"]))
  # peak_resident_demand
  
  # estimate peak demands. Change adjust demand later with function
  util_rate_commuter <- (sum(pknginvuni[,c("A","A-Carpool","C","C-Carpool","Resident/A","Resident/C")])-sum(putil[,c("A","A-Carpool","C","C-Carpool","Resident/A","Resident/C")]))/sum(pknginvuni[,c("A","A-Carpool","C","C-Carpool","Resident/A","Resident/C")])
  util_rate_resident <- (sum(pknginvuni[,c("Resident")])-sum(putil[,c("Resident")]))/sum(pknginvuni[,c("Resident")])
  spillover_peak_commuter_demand <- peak_commuter_demand/util_rate_commuter - sum(pknginvuni[,c("A","A-Carpool","C","C-Carpool")])
  spillover_peak_resident_demand <- peak_resident_demand/util_rate_resident - sum(pknginvuni[,c("Resident")])
  commuter_day_sharedspace_percent <- min(1,spillover_peak_commuter_demand/sum(pknginvuni[,c("Resident/A","Resident/C")]))
  commuter_night_sharedspace_percent <- min(1,1-spillover_peak_resident_demand/sum(pknginvuni[,c("Resident/A","Resident/C")]))
  
  df_pkng_u <- pknginvuni
  
  df_pkng_u$visitor_spaces_total <- rowSums(pknginvuni[,c("Meter","Machine","Time.Limit","Other","Service.Vehicle")])
  df_pkng_u$day_commuter_spaces_total <- rowSums(round(pknginvuni[,c("Resident/A","Resident/C")]*commuter_day_sharedspace_percent,0))+rowSums(pknginvuni[,c("A","A-Carpool","C","C-Carpool","Disabled")])
  df_pkng_u$day_resident_spaces_total <- rowSums(round(pknginvuni[,c("Resident/A","Resident/C")]*(1-commuter_day_sharedspace_percent),0))+pknginvuni[,c("Resident")]
  df_pkng_u$night_commuter_spaces_total <- rowSums(round(pknginvuni[,c("Resident/A","Resident/C")]*commuter_night_sharedspace_percent,0))+rowSums(pknginvuni[,c("A","A-Carpool","C","C-Carpool","Disabled")])
  df_pkng_u$night_resident_spaces_total <- rowSums(round(pknginvuni[,c("Resident/A","Resident/C")]*(1-commuter_night_sharedspace_percent),0))+pknginvuni[,c("Resident")]
  
  # aggregate hospital inventory df by demand types (commuter, visitor)
  df_pkng_h <- pknginvhosp
  temp <- c("commuter_spaces_total","visitor_spaces_total")
  df_pkng_h[,temp] <- NA
  
  df_pkng_h$commuter_spaces_total <- rowSums(pknginvhosp[,c("A","A-Carpool","C","C-Carpool","Service.Vehicle","Disabled","Resident/A","Resident/C","Resident")])
  df_pkng_h$visitor_spaces_total <- rowSums(pknginvhosp[,c("Meter","Machine","Time.Limit","Other")])
  
  # sum(df_pkng_h[,c(11:23)])
  # sum(df_pkng_h[,c(38:39)])
  
  # split utilization by university and hospital
  
  # set up hospital util df first
  temphosp <- putil[putil$`Inventory_Name` %in% pknginvhosp$`P&TS.Lot.Name`,] # pulls out shared lots
  colSums(temphosp[,c(3:15)])
  sum(temphosp[,c(3:15)])
  
  for (i in 1:nrow(table_hosppkng)) {
    tempi <- which(temphosp$Inventory_Name==table_hosppkng$Inventory_Name[i])
    temphosp[tempi,-c(1:2)] <- temphosp[tempi,-c(1:2)]* table_hosppkng$Percent_Hospital[i]
  }
  
  # colSums(temphosp[,c(3:15)])
  # sum(temphosp[,c(3:15)])
  
  # split hospital utilization of PS-4 spaces like inventory (Disabled and Other are hospital, rest are university)
  temphosp[temphosp$Inventory_Name=="S-9 (Parking Structure 9)",-c(1:2,14:16)] <- 0
  pkngutilhosp <- temphosp[,c(1:15)]
  
  # Parking Model Input Edits for Parking Lot Utilization - Hospital
  # dim(pkngutilhosp)
  if (add_lot_h) {
    added_lots <- project_list_instance$`P&TS.Lot.Name`[project_list_instance$Action == "Add" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == TRUE]
    for (i in added_lots) {
      temp <- pkngutilhosp[0,]
      temp[1,c("Inventory_Name","Parking_Name")] <- i
      temp[is.na(temp)] <- 0
      pkngutilhosp <- rbind(pkngutilhosp, temp)
    }
  }
  
  # dim(pkngutilhosp)

  
  # set up university util df second
  tempuni <- putil[putil$`Inventory_Name` %in% pknginvuni$`P&TS.Lot.Name`,]
  for (i in 1:nrow(table_hosppkng)) {
    tempi <- which(tempuni$Inventory_Name==table_hosppkng$Inventory_Name[i])
    tempuni[tempi,-c(1:2)] <- tempuni[tempi,-c(1:2)]* (1-table_hosppkng$Percent_Hospital[i])
  }
  
  # split university utilization of PS-4 spaces like inventory (Disabled and Other are hospital, rest are university)
  tempuni[tempuni$Inventory_Name=="S-9 (Parking Structure 9)",-c(1:13)] <- 0
  
  pkngutiluni <- tempuni[,c(1:15)]
  
  # Parking Model Input Edits for Parking Lot Utilization - University
  # dim(pkngutiluni)
  if (add_lot_u) {
    added_lots <- project_list_instance$`P&TS.Lot.Name`[project_list_instance$Action == "Add" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == FALSE]
    for (i in added_lots) {
      temp <- pkngutiluni[0,]
      temp[1,c("Inventory_Name","Parking_Name")] <- i
      temp[is.na(temp)] <- 0
      pkngutiluni <- rbind(pkngutiluni, temp)
    }
  }
  
  # dim(pkngutiluni)
  
  # aggregate university utilization df by demand types (commuter, resident, visitor)
  pkngutiluni <- pkngutiluni[match(df_pkng_u$`P&TS.Lot.Name`, pkngutiluni$Inventory_Name),]
  pkngutiluni[,-c(1:2)][is.na(pkngutiluni[,-c(1:2)])] <- 0
  
  df_pkng_u$visitor_spaces_empty <- rowSums(pkngutiluni[,c("Pay Meter","Pay Machine","Time Limit","Other")])
  df_pkng_u$day_commuter_spaces_empty <- rowSums(round(pkngutiluni[,c("Resident/A","Resident/C")]*commuter_day_sharedspace_percent,0))+rowSums(pkngutiluni[,c("A","A-Carpool","C","C-Carpool","Service Vehicle","Disabled")])
  df_pkng_u$day_resident_spaces_empty <- rowSums(round(pkngutiluni[,c("Resident/A","Resident/C")]*(1-commuter_day_sharedspace_percent),0))+pkngutiluni[,c("Resident")]
  df_pkng_u$night_commuter_spaces_empty <- rowSums(round(pkngutiluni[,c("Resident/A","Resident/C")]*commuter_night_sharedspace_percent,0))+rowSums(pkngutiluni[,c("A","A-Carpool","C","C-Carpool","Service Vehicle","Disabled")])
  df_pkng_u$night_resident_spaces_empty <- rowSums(round(pkngutiluni[,c("Resident/A","Resident/C")]*(1-commuter_night_sharedspace_percent),0))+pkngutiluni[,c("Resident")]
  
  # colSums(df_pkng_u[,c(43:45)])
  # colSums(df_pkng_u[,c(43,46:47)])
  # colSums(pkngutiluni[,-c(1:2)])
  # sum(df_pkng_u[,c(43:45)])
  
  # aggregate hospital inventory df by demand types (commuter, visitor)
  pkngutilhosp <- pkngutilhosp[match(df_pkng_h$`P&TS.Lot.Name`, pkngutilhosp$Inventory_Name),]
  pkngutilhosp[,-c(1:2)][is.na(pkngutilhosp[,-c(1:2)])] <- 0
  
  df_pkng_h$commuter_spaces_empty <- rowSums(pkngutilhosp[,c("A","A-Carpool","C","C-Carpool","Service Vehicle","Disabled","Resident/A","Resident/C","Resident")])
  df_pkng_h$visitor_spaces_empty <- rowSums(pkngutilhosp[,c("Pay Meter","Pay Machine","Time Limit","Other")])
  
  # colSums(df_pkng_h[,c(40:41)])
  # colSums(pkngutilhosp[,-c(1:2)])
  
  # create the used space columns
  df_pkng_u$visitor_spaces_used <- df_pkng_u$visitor_spaces_total - df_pkng_u$visitor_spaces_empty
  df_pkng_u$day_commuter_spaces_used <- df_pkng_u$day_commuter_spaces_total - df_pkng_u$day_commuter_spaces_empty
  df_pkng_u$day_resident_spaces_used <- df_pkng_u$day_resident_spaces_total - df_pkng_u$day_resident_spaces_empty
  df_pkng_u$night_commuter_spaces_used <- df_pkng_u$night_commuter_spaces_total - df_pkng_u$night_commuter_spaces_empty
  df_pkng_u$night_resident_spaces_used <- df_pkng_u$night_resident_spaces_total - df_pkng_u$night_resident_spaces_empty
  df_pkng_h$visitor_spaces_used <- df_pkng_h$visitor_spaces_total - df_pkng_h$visitor_spaces_empty
  df_pkng_h$commuter_spaces_used <- df_pkng_h$commuter_spaces_total - df_pkng_h$commuter_spaces_empty
  
  # some "other" utilizations are really commuter utilization counts. Bring all the overflow from visitor spaces to commuter ones
  df_pkng_u[df_pkng_u$visitor_spaces_used<0,c("day_commuter_spaces_used")] <- df_pkng_u[df_pkng_u$visitor_spaces_used<0,c("day_commuter_spaces_used")] + df_pkng_u[df_pkng_u$visitor_spaces_used<0,c("visitor_spaces_used")]
  df_pkng_u[df_pkng_u$visitor_spaces_used<0,c("night_commuter_spaces_used")] <- df_pkng_u[df_pkng_u$visitor_spaces_used<0,c("night_commuter_spaces_used")] + df_pkng_u[df_pkng_u$visitor_spaces_used<0,c("visitor_spaces_used")]
  df_pkng_u$visitor_spaces_used[df_pkng_u$visitor_spaces_used<0] <- df_pkng_u$visitor_spaces_total[df_pkng_u$visitor_spaces_used<0]
  
  # Bring all the overflow from negative commuter spaces to resident ones
  df_pkng_u[df_pkng_u$day_commuter_spaces_used<0,c("day_resident_spaces_used")] <- df_pkng_u[df_pkng_u$day_commuter_spaces_used<0,c("day_resident_spaces_used")] + df_pkng_u[df_pkng_u$day_commuter_spaces_used<0,c("day_commuter_spaces_used")]
  df_pkng_u[df_pkng_u$night_commuter_spaces_used<0,c("night_resident_spaces_used")] <- df_pkng_u[df_pkng_u$night_commuter_spaces_used<0,c("night_resident_spaces_used")] + df_pkng_u[df_pkng_u$night_commuter_spaces_used<0,c("night_commuter_spaces_used")]
  df_pkng_u$day_commuter_spaces_used[df_pkng_u$day_commuter_spaces_used<0] <- df_pkng_u$day_commuter_spaces_total[df_pkng_u$day_commuter_spaces_used<0]
  df_pkng_u$night_commuter_spaces_used[df_pkng_u$night_commuter_spaces_used<0] <- df_pkng_u$night_commuter_spaces_total[df_pkng_u$night_commuter_spaces_used<0]
  
  # Bring all the overflow from hospital visitor spaces to hospital commuter ones
  df_pkng_h[df_pkng_h$visitor_spaces_used<0,c("commuter_spaces_used")] <- df_pkng_h[df_pkng_h$visitor_spaces_used<0,c("commuter_spaces_used")] + df_pkng_h[df_pkng_h$visitor_spaces_used<0,c("visitor_spaces_used")]
  df_pkng_h$visitor_spaces_used[df_pkng_h$visitor_spaces_used<0] <- df_pkng_h$visitor_spaces_total[df_pkng_h$visitor_spaces_used<0]
  
  # sum(df_pkng_u$visitor_spaces_used[df_pkng_u$visitor_spaces_used<0])
  # sum(df_pkng_u$day_commuter_spaces_used[df_pkng_u$day_commuter_spaces_used<0])
  # sum(df_pkng_u$day_resident_spaces_used[df_pkng_u$day_resident_spaces_used<0])
  # sum(df_pkng_u$night_commuter_spaces_used[df_pkng_u$night_commuter_spaces_used<0])
  # sum(df_pkng_u$night_resident_spaces_used[df_pkng_u$night_resident_spaces_used<0])
  # sum(df_pkng_h$visitor_spaces_used[df_pkng_h$visitor_spaces_used<0])
  # sum(df_pkng_h$commuter_spaces_used[df_pkng_h$commuter_spaces_used<0])
  # 
  # sum(df_pkng_u$visitor_spaces_used[df_pkng_u$visitor_spaces_used>0])
  # sum(df_pkng_u$day_commuter_spaces_used[df_pkng_u$day_commuter_spaces_used>0])
  # sum(df_pkng_u$day_resident_spaces_used[df_pkng_u$day_resident_spaces_used>0])
  # sum(df_pkng_u$night_commuter_spaces_used[df_pkng_u$night_commuter_spaces_used>0])
  # sum(df_pkng_u$night_resident_spaces_used[df_pkng_u$night_resident_spaces_used>0])
  # sum(df_pkng_h$visitor_spaces_used[df_pkng_h$visitor_spaces_used>0])
  # sum(df_pkng_h$commuter_spaces_used[df_pkng_h$commuter_spaces_used>0])
  
  # if remaining spaces vacant is greater than total spaces, set vacant spaces to 0 (and used spaces to the total)
  df_pkng_u$day_resident_spaces_used[df_pkng_u$day_resident_spaces_used<0] <- df_pkng_u$day_resident_spaces_total[df_pkng_u$day_resident_spaces_used<0]
  df_pkng_u$night_resident_spaces_used[df_pkng_u$night_resident_spaces_used<0] <- df_pkng_u$night_resident_spaces_total[df_pkng_u$night_resident_spaces_used<0]
  df_pkng_h$commuter_spaces_used[df_pkng_h$commuter_spaces_used<0] <- df_pkng_h$commuter_spaces_total[df_pkng_h$commuter_spaces_used<0]
  
  # sum(df_pkng_u$visitor_spaces_used[df_pkng_u$visitor_spaces_used<0])
  # sum(df_pkng_u$day_commuter_spaces_used[df_pkng_u$day_commuter_spaces_used<0])
  # sum(df_pkng_u$day_resident_spaces_used[df_pkng_u$day_resident_spaces_used<0])
  # sum(df_pkng_u$night_commuter_spaces_used[df_pkng_u$night_commuter_spaces_used<0])
  # sum(df_pkng_u$night_resident_spaces_used[df_pkng_u$night_resident_spaces_used<0])
  # sum(df_pkng_h$visitor_spaces_used[df_pkng_h$visitor_spaces_used<0])
  # sum(df_pkng_h$commuter_spaces_used[df_pkng_h$commuter_spaces_used<0])
  
  
  # CREATE RATES USING BUILDING, PARKING, AND PERMIT DATA
  total_sqft_hosp_commuter <- sum(df_bldg_sqftbeds$COM[df_bldg_sqftbeds$HOSPITAL==TRUE])
  total_sqft_hosp_visitor <- sum(df_bldg_sqftbeds$VIS[df_bldg_sqftbeds$HOSPITAL==TRUE])
  total_sqft_uni_commuter <- sum(df_bldg_sqftbeds$COM[df_bldg_sqftbeds$HOSPITAL==FALSE])
  total_bed_uni_ugresident <- sum(df_bldg_sqftbeds$BEDS_NONFRESHMAN_UNDERGRAD[df_bldg_sqftbeds$HOSPITAL==FALSE],na.rm=TRUE)
  total_bed_uni_grresident <- sum(df_bldg_sqftbeds$BEDS_GRAD[df_bldg_sqftbeds$HOSPITAL==FALSE],na.rm=TRUE)
  total_sqft_uni_visitor <- sum(df_bldg_sqftbeds$VIS[df_bldg_sqftbeds$HOSPITAL==FALSE])
  
  total_used_spaces_hosp_visitor <- sum(df_pkng_h$visitor_spaces_used)
  total_used_spaces_hosp_commuter <- sum(df_pkng_h$commuter_spaces_used)
  total_used_spaces_uni_visitor <- sum(df_pkng_u$visitor_spaces_used)
  total_used_spaces_uni_day_commuter <- sum(df_pkng_u$day_commuter_spaces_used)
  total_used_spaces_uni_day_resident <- sum(df_pkng_u$day_resident_spaces_used)
  total_used_spaces_uni_night_commuter <- sum(df_pkng_u$night_commuter_spaces_used)
  total_used_spaces_uni_night_resident <- sum(df_pkng_u$night_resident_spaces_used)
  
  bldg2updateSQFT$HOSPITAL <- FALSE
  bldg2updateSQFT$HOSPITAL[bldg2updateSQFT$BLDG_NAME_KEY %in% table_hospbldg] <- TRUE
  adj_uv <- sum(bldg2updateSQFT$UPDATE_VIS[bldg2updateSQFT$HOSPITAL == FALSE],na.rm=TRUE)
  adj_hc <- sum(bldg2updateSQFT$UPDATE_COM[bldg2updateSQFT$HOSPITAL == TRUE],na.rm=TRUE)
  adj_hv <- sum(bldg2updateSQFT$UPDATE_VIS[bldg2updateSQFT$HOSPITAL == TRUE],na.rm=TRUE)
  adj_uc <- sum(bldg2updateSQFT$UPDATE_COM[bldg2updateSQFT$HOSPITAL == FALSE],na.rm=TRUE)
  adj_ur <- sum(bldg2updateSQFT$UPDATE_RES[bldg2updateSQFT$HOSPITAL == FALSE],na.rm=TRUE)
  
  rate_calculator <- function(total_bed_sqft, parking_spaces, proportion = 1) {
    return(proportion*parking_spaces/total_bed_sqft)
  }
  
  # generation_rates
  if(exist_genrates) {
    generation_rates <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\generation_rates.csv")
  } else {
    generation_rates <- data.frame(UNI_DAY_COMMUTER = numeric(1),UNI_DAY_UGRESIDENT = numeric(1),UNI_DAY_GRRESIDENT = numeric(1),
                                  UNI_VISITOR = numeric(1), HOSP_COMMUTER = numeric(1),HOSP_VISITOR = numeric(1),
                                  UNI_NIGHT_COMMUTER = numeric(1),UNI_NIGHT_UGRESIDENT = numeric(1),UNI_NIGHT_GRRESIDENT = numeric(1))
    
    generation_rates$UNI_VISITOR <- rate_calculator(total_sqft_uni_visitor, total_used_spaces_uni_visitor - adj_uv, 1000) # proportion convert sqft to ksf
    generation_rates$HOSP_COMMUTER <- rate_calculator(total_sqft_hosp_commuter, total_used_spaces_hosp_commuter - adj_hc, 1000)
    generation_rates$HOSP_VISITOR <- rate_calculator(total_sqft_hosp_visitor, total_used_spaces_hosp_visitor - adj_hv, 1000)
    
    generation_rates$UNI_DAY_COMMUTER <- rate_calculator(total_sqft_uni_commuter, total_used_spaces_uni_day_commuter - adj_uc, 1000)
    generation_rates$UNI_DAY_UGRESIDENT <- rate_calculator(total_bed_uni_ugresident, total_used_spaces_uni_day_resident - adj_ur, 1-proportion_grad_permit)
    generation_rates$UNI_DAY_GRRESIDENT <- rate_calculator(total_bed_uni_grresident, total_used_spaces_uni_day_resident - adj_ur, proportion_grad_permit)
    
    generation_rates$UNI_NIGHT_COMMUTER <- rate_calculator(total_sqft_uni_commuter, total_used_spaces_uni_night_commuter - adj_uc, 1000)
    generation_rates$UNI_NIGHT_UGRESIDENT <- rate_calculator(total_bed_uni_ugresident, total_used_spaces_uni_night_resident - adj_ur, 1-proportion_grad_permit)
    generation_rates$UNI_NIGHT_GRRESIDENT <- rate_calculator(total_bed_uni_grresident, total_used_spaces_uni_night_resident - adj_ur, proportion_grad_permit)
    write.csv(generation_rates, file = "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\generation_rates.csv", na="",row.names=FALSE)
    
    }
  
  temp_sp <- unique(left_join(spkng, table_pkng2pshape)) #link lot coordinates to lots
  temp_sp[duplicated(temp_sp$`P&TS.Lot.Name`), ]
  temp_sp$`P&TS.Lot.Name`[order(temp_sp$`P&TS.Lot.Name`)]
  
  df_pkng_u_s <- unique(left_join(df_pkng_u, temp_sp))
  df_pkng_h_s <- unique(left_join(df_pkng_h, unique(temp_sp)))
  df_pkng_u_s <- df_pkng_u_s[!duplicated(df_pkng_u_s$`P&TS.Lot.Name`),] # some buildings have multiple shapes with different IDs. Remove duplicates here.
  
  temp_sb <- unique(left_join(df_bldg_sqftbeds, sbldg)) #link building coordinates to buildings
  temp_sb2 <- temp_sb[!duplicated(temp_sb$BLDG_ID), ] # only keeps one of each bldg_ID. Cant join back later with no problems.
  
  temp_sb2$DEMAND_UNI_DAY_COMMUTER <- temp_sb2$COM * generation_rates$UNI_DAY_COMMUTER / 1000 # calculate demand in new columns, converting sf to ksf
  temp_sb2$DEMAND_UNI_DAY_UGRESIDENT <- temp_sb2$BEDS_NONFRESHMAN_UNDERGRAD * generation_rates$UNI_DAY_UGRESIDENT
  temp_sb2$DEMAND_UNI_DAY_GRRESIDENT <- temp_sb2$BEDS_GRAD * generation_rates$UNI_DAY_GRRESIDENT
  temp_sb2$DEMAND_UNI_VISITOR <- temp_sb2$VIS * generation_rates$UNI_VISITOR / 1000
  temp_sb2$DEMAND_UNI_NIGHT_COMMUTER <- temp_sb2$COM * generation_rates$UNI_NIGHT_COMMUTER / 1000
  temp_sb2$DEMAND_UNI_NIGHT_UGRESIDENT <- temp_sb2$BEDS_NONFRESHMAN_UNDERGRAD * generation_rates$UNI_NIGHT_UGRESIDENT
  temp_sb2$DEMAND_UNI_NIGHT_GRRESIDENT <- temp_sb2$BEDS_GRAD * generation_rates$UNI_NIGHT_GRRESIDENT
  temp_sb2$DEMAND_HOSP_VISITOR <- temp_sb2$VIS * generation_rates$HOSP_VISITOR / 1000
  temp_sb2$DEMAND_HOSP_COMMUTER <- temp_sb2$COM * generation_rates$HOSP_COMMUTER / 1000
  
  # Baseline adjustments. Different than project list adjustments
  # update building sqft (changes are set to the manual value since generation rates are already calculated)
  adj_uv <- sum(bldg2updateSQFT$UPDATE_VIS[bldg2updateSQFT$HOSPITAL == FALSE],na.rm=TRUE)
  adj_hc <- sum(bldg2updateSQFT$UPDATE_COM[bldg2updateSQFT$HOSPITAL == TRUE],na.rm=TRUE)
  adj_hv <- sum(bldg2updateSQFT$UPDATE_VIS[bldg2updateSQFT$HOSPITAL == TRUE],na.rm=TRUE)
  adj_uc <- sum(bldg2updateSQFT$UPDATE_COM[bldg2updateSQFT$HOSPITAL == FALSE],na.rm=TRUE)
  adj_ur <- sum(bldg2updateSQFT$UPDATE_RES[bldg2updateSQFT$HOSPITAL == FALSE],na.rm=TRUE)
  
  for (i in matches) {
    if (bldg2updateSQFT$HOSPITAL[bldg2updateSQFT$BLDG_NAME_KEY == i] == FALSE) {
      new_ucom <- bldg2updateSQFT$UPDATE_COM[bldg2updateSQFT$BLDG_NAME_KEY == i] 
      new_ures <- bldg2updateSQFT$UPDATE_RES[bldg2updateSQFT$BLDG_NAME_KEY == i]
      new_uvis <- bldg2updateSQFT$UPDATE_VIS[bldg2updateSQFT$BLDG_NAME_KEY == i]
      if (!is.na(new_ucom)) {
        temp_sb2$DEMAND_UNI_DAY_COMMUTER[temp_sb2$BLDG_NAME == i] <- new_ucom
        temp_sb2$DEMAND_UNI_NIGHT_COMMUTER[temp_sb2$BLDG_NAME == i] <- new_ucom
      }
      if (!is.na(new_ures)) {
        temp_sb2$DEMAND_UNI_DAY_UGRESIDENT[temp_sb2$BLDG_NAME == i] <- new_ures * (1 - proportion_grad_permit)
        temp_sb2$DEMAND_UNI_NIGHT_UGRESIDENT[temp_sb2$BLDG_NAME == i] <- new_ures * (1 - proportion_grad_permit)
        temp_sb2$DEMAND_UNI_DAY_GRRESIDENT[temp_sb2$BLDG_NAME == i] <- new_ures * (proportion_grad_permit)
        temp_sb2$DEMAND_UNI_NIGHT_GRRESIDENT[temp_sb2$BLDG_NAME == i] <- new_ures * (proportion_grad_permit)
      }
      if (!is.na(new_uvis)) {temp_sb2$DEMAND_UNI_VISITOR[temp_sb2$BLDG_NAME == i] <- new_uvis}
      
    } else {
      new_hcom <- bldg2updateSQFT$UPDATE_COM[bldg2updateSQFT$BLDG_NAME_KEY == i] 
      new_hvis <- bldg2updateSQFT$UPDATE_VIS[bldg2updateSQFT$BLDG_NAME_KEY == i]
      if (!is.na(new_hcom)) {temp_sb2$DEMAND_HOSP_COMMUTER[temp_sb2$BLDG_NAME == i] <- new_hcom}
      if (!is.na(new_hvis)) {temp_sb2$DEMAND_HOSP_VISITOR[temp_sb2$BLDG_NAME == i] <- new_hvis}
    }
  }
  
  # for hospital buildings, remove the university demand
  temp_sb2[temp_sb2$HOSPITAL==TRUE,c("DEMAND_UNI_DAY_COMMUTER","DEMAND_UNI_DAY_UGRESIDENT","DEMAND_UNI_DAY_GRRESIDENT","DEMAND_UNI_VISITOR",
                                     "DEMAND_UNI_NIGHT_COMMUTER","DEMAND_UNI_NIGHT_UGRESIDENT","DEMAND_UNI_NIGHT_GRRESIDENT")] <- 0
  
  # for university buildings, remove the hospital demand
  temp_sb2[temp_sb2$HOSPITAL==FALSE,c("DEMAND_HOSP_VISITOR","DEMAND_HOSP_COMMUTER")] <- 0
  
  # turn demand that is NA to 0
  temp_sb2[,c((ncol(temp_sb2)-8):ncol(temp_sb2))][is.na(temp_sb2[,c((ncol(temp_sb2)-8):ncol(temp_sb2))])] <- 0
  
  colSums(temp_sb2[temp_sb2$HOSPITAL==FALSE,27:33],na.rm=TRUE)
  colSums(temp_sb2[temp_sb2$HOSPITAL==TRUE,34:35],na.rm=TRUE)
  
  x <- temp_sb2$COM; sum(x, na.rm=TRUE)
  x2 <- temp_sb2$BEDS_NONFRESHMAN_UNDERGRAD; sum(x2, na.rm=TRUE)
  x3 <- temp_sb2$BEDS_GRAD; sum(x3, na.rm=TRUE)
  x4 <- temp_sb2$VIS; sum(x4, na.rm=TRUE)
  
  temp_sb2$COM[temp_sb2$HOSPITAL==FALSE] <- temp_sb2$DEMAND_UNI_DAY_COMMUTER[temp_sb2$HOSPITAL==FALSE] / generation_rates$UNI_DAY_COMMUTER * 1000 # calculate demand in new columns, converting sf to ksf
  temp_sb2$BEDS_NONFRESHMAN_UNDERGRAD[temp_sb2$HOSPITAL==FALSE] <- temp_sb2$DEMAND_UNI_DAY_UGRESIDENT[temp_sb2$HOSPITAL==FALSE] / generation_rates$UNI_DAY_UGRESIDENT
  temp_sb2$BEDS_GRAD[temp_sb2$HOSPITAL==FALSE] <- temp_sb2$DEMAND_UNI_DAY_GRRESIDENT[temp_sb2$HOSPITAL==FALSE] / generation_rates$UNI_DAY_GRRESIDENT
  temp_sb2$VIS[temp_sb2$HOSPITAL==FALSE] <- temp_sb2$DEMAND_UNI_VISITOR[temp_sb2$HOSPITAL==FALSE] / generation_rates$UNI_VISITOR * 1000
  temp_sb2$COM[temp_sb2$HOSPITAL==TRUE] <- temp_sb2$DEMAND_HOSP_COMMUTER[temp_sb2$HOSPITAL==TRUE] / generation_rates$HOSP_COMMUTE * 1000
  temp_sb2$VIS[temp_sb2$HOSPITAL==TRUE]  <- temp_sb2$DEMAND_HOSP_VISITOR[temp_sb2$HOSPITAL==TRUE] / generation_rates$HOSP_VISITOR * 1000
  
  y <- round(temp_sb2$COM,0); sum(y, na.rm=TRUE)
  y2 <- round(temp_sb2$BEDS_NONFRESHMAN_UNDERGRAD,0); sum(y2, na.rm=TRUE)
  y3 <- round(temp_sb2$BEDS_GRAD,0); sum(y3, na.rm=TRUE)
  y4 <- round(temp_sb2$VIS,0); sum(y4, na.rm=TRUE)
  
  # x[x != y]
  # y[x != y]
  
  # calculate utilization rates for each demand type
  if (exist_utilrates) {
    utilization_rates <- data.frame(UNI_DAY_COMMUTER = numeric(1),UNI_DAY_RESIDENT = numeric(1),
                                   UNI_VISITOR = numeric(1), HOSP_COMMUTER = numeric(1),HOSP_VISITOR = numeric(1),
                                   UNI_NIGHT_COMMUTER = numeric(1),UNI_NIGHT_RESIDENT = numeric(1))
    
    utilization_rates$UNI_VISITOR <- sum(df_pkng_u_s$visitor_spaces_used)/sum(df_pkng_u_s$visitor_spaces_total)
    utilization_rates$HOSP_COMMUTER <- sum(df_pkng_h_s$commuter_spaces_used)/sum(df_pkng_h_s$commuter_spaces_total)
    utilization_rates$HOSP_VISITOR <- sum(df_pkng_h_s$visitor_spaces_used)/sum(df_pkng_h_s$visitor_spaces_total)
    
    utilization_rates$UNI_DAY_COMMUTER <- sum(df_pkng_u_s$day_commuter_spaces_used)/sum(df_pkng_u_s$day_commuter_spaces_total)
    utilization_rates$UNI_DAY_RESIDENT <- sum(df_pkng_u_s$day_resident_spaces_used)/sum(df_pkng_u_s$day_resident_spaces_total)
    
    utilization_rates$UNI_NIGHT_COMMUTER <- sum(df_pkng_u_s$night_commuter_spaces_used)/sum(df_pkng_u_s$night_commuter_spaces_total)
    utilization_rates$UNI_NIGHT_RESIDENT <- sum(df_pkng_u_s$night_resident_spaces_used)/sum(df_pkng_u_s$night_resident_spaces_total)
    assign(paste(instance_name,"utilization_rates",sep="_"),utilization_rates)
    utilization_rates <- read.csv("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\utilization_rates.csv")
    write.csv(paste(instance_name,"utilization_rates",sep="_"), paste("S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\",instance_name,"utilization_rates.csv",sep="_"))
    
  } else {
    utilization_rates <- data.frame(UNI_DAY_COMMUTER = numeric(1),UNI_DAY_RESIDENT = numeric(1),
                                   UNI_VISITOR = numeric(1), HOSP_COMMUTER = numeric(1),HOSP_VISITOR = numeric(1),
                                   UNI_NIGHT_COMMUTER = numeric(1),UNI_NIGHT_RESIDENT = numeric(1))
    
    utilization_rates$UNI_VISITOR <- sum(df_pkng_u_s$visitor_spaces_used)/sum(df_pkng_u_s$visitor_spaces_total)
    utilization_rates$HOSP_COMMUTER <- sum(df_pkng_h_s$commuter_spaces_used)/sum(df_pkng_h_s$commuter_spaces_total)
    utilization_rates$HOSP_VISITOR <- sum(df_pkng_h_s$visitor_spaces_used)/sum(df_pkng_h_s$visitor_spaces_total)
    
    utilization_rates$UNI_DAY_COMMUTER <- sum(df_pkng_u_s$day_commuter_spaces_used)/sum(df_pkng_u_s$day_commuter_spaces_total)
    utilization_rates$UNI_DAY_RESIDENT <- sum(df_pkng_u_s$day_resident_spaces_used)/sum(df_pkng_u_s$day_resident_spaces_total)
    
    utilization_rates$UNI_NIGHT_COMMUTER <- sum(df_pkng_u_s$night_commuter_spaces_used)/sum(df_pkng_u_s$night_commuter_spaces_total)
    utilization_rates$UNI_NIGHT_RESIDENT <- sum(df_pkng_u_s$night_resident_spaces_used)/sum(df_pkng_u_s$night_resident_spaces_total)
    
    write.csv(utilization_rates, file = "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\utilization_rates.csv", na="",row.names=FALSE)
  }
  
  # utilization_rates
  
  
  # colSums(temp_sb2[,c((ncol(temp_sb2)-8):ncol(temp_sb2))])
  # sum(temp_sb2[,c("DEMAND_UNI_DAY_COMMUTER","DEMAND_UNI_DAY_UGRESIDENT","DEMAND_UNI_DAY_GRRESIDENT","DEMAND_UNI_VISITOR","DEMAND_HOSP_COMMUTER")])
  # sum(temp_sb2[,c("DEMAND_UNI_VISITOR","DEMAND_UNI_NIGHT_COMMUTER","DEMAND_UNI_NIGHT_UGRESIDENT","DEMAND_UNI_NIGHT_GRRESIDENT","DEMAND_HOSP_COMMUTER")])
  
  # colSums(df_pkng_u_s[,c(11:23)])
  
  df_bldg_u_s <- temp_sb2[temp_sb2$HOSPITAL == FALSE,]
  df_bldg_h_s <- temp_sb2[temp_sb2$HOSPITAL == TRUE,]
  
  # Parking Model Input Edits for Coordinates - Hospital
  if (add_lot_h) {
    added_lots <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == TRUE,]
    for (i in 1:nrow(added_lots)) {
      df_pkng_h_s[df_pkng_h_s$`P&TS.Lot.Name` == added_lots$`P&TS.Lot.Name`[i],][,c("Lat","Lon")] <- added_lots[i,c("Latitude","Longitude")]
    }
  }
  
  if (add_bldg_h) {
    added_bldgs <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$BLDG_NAME) & project_list_instance$HOSPITAL == TRUE,]
    for (i in 1:nrow(added_bldgs)) {
      df_bldg_h_s[df_bldg_h_s$BLDG_NAME == added_bldgs$BLDG_NAME[i],][,c("Lat","Lon")] <- added_bldgs[i,c("Latitude","Longitude")]
    }
  }
  
  # Parking Model Input Edits for Parking Lot Coordinates - University
  if (add_lot_u) {
    added_lots <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$`P&TS.Lot.Name`) & project_list_instance$HOSPITAL == FALSE,]
    for (i in 1:nrow(added_lots)) {
      df_pkng_u_s[df_pkng_u_s$`P&TS.Lot.Name` == added_lots$`P&TS.Lot.Name`[i],][,c("Lat","Lon")] <- added_lots[i,c("Latitude","Longitude")]
    }
  }
  
  if (add_bldg_u) {
    added_bldgs <- project_list_instance[project_list_instance$Action == "Add" & !is.na(project_list_instance$BLDG_NAME) & project_list_instance$HOSPITAL == FALSE,]
    for (i in 1:nrow(added_bldgs)) {
      df_bldg_u_s[df_bldg_u_s$BLDG_NAME == added_bldgs$BLDG_NAME[i],][,c("Lat","Lon")] <- added_bldgs[i,c("Latitude","Longitude")]
    }
  }
  
  write.csv(df_pkng_u_s, file = "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_PkngData_U.csv", na="",row.names=FALSE)
  write.csv(df_bldg_u_s, file = "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_BldgData_U.csv", na="",row.names=FALSE)
  write.csv(df_pkng_h_s, file = "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_PkngData_H.csv", na="",row.names=FALSE)
  write.csv(df_bldg_h_s, file = "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\Adj_BldgData_H.csv", na="",row.names=FALSE)
  
  rm(list=setdiff(ls(), c("utilization_rates","df_pkng_u_s","df_bldg_u_s","df_pkng_h_s","df_bldg_h_s")))
  # Clear workspace -------------------------------------------------------------------------------------------------------------
  rm(list = ls()) 
}