# CLEAR WORKSPACE
rm(list = ls())

options(stringsAsFactors=FALSE)
require(zoo)
require(openxlsx)
source("parking_model_data_cleaning.R")
source("parking_model_demand_allocation.R")
source("parking_model_post_allocation_cleaning.R")
source("parking_model_mapping.R")

path2input = "S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Parking Model Input\\Parking Model Input.xlsx"
input <- read.xlsx(path2input, sheet="Project List", detectDates = TRUE)


unique_dates <- unique(union(input$Start.Date, input$`End.Date.(Optional)`))
unique_dates <- unique_dates[!is.na(unique_dates)]
unique_dates <- c(unique_dates, max(unique_dates)+1)
unique_dates <- c(unique_dates, min(unique_dates)-1)
unique_dates <- unique_dates[order(unique_dates)]

times_of_interest <- data.frame(day = character(0))

for (i in 1:nrow(input)) {
  times_of_interest[1, paste(input$No.[i],"_", input$Name[i], sep = "")] <- FALSE
}

for (i in 1:length(unique_dates)) {
  date_i <- unique_dates[i]
  times_of_interest[i,1] <- as.character(as.Date(date_i))
  for (j in 1:nrow(input)) {
    if (input$Start.Date[j] <= date_i & any(input$`End.Date.(Optional)`[j] >= date_i, is.na(input$`End.Date.(Optional)`[j]))) {
      times_of_interest[i,j+1] <- TRUE
    } else {times_of_interest[i,j+1] <- FALSE}
  }
}

write.csv(times_of_interest,"S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\times_of_interest.csv")
track_instance <- data.frame(day = times_of_interest$day)
track_instance$processing_now <- FALSE
track_instance$processing_now[1] <- TRUE
write.csv(track_instance,"S:\\Groups\\Transportation\\Planning\\Parking estimates\\PArking model\\Data Generated\\track_instance.csv", row.names = FALSE)

i=3
for (i in 1:nrow(times_of_interest)) {
  index_i <- times_of_interest[i,-1] == TRUE
  project_list_instance <- input[index_i,]
  instance_name <- times_of_interest[i,1]
  project_list <- input
  print(paste("Running parking model for instance:", instance_name))
  parking_model_data_cleaning(instance_name, project_list_instance)
  parking_model_demand_allocation(instance_name, project_list_instance)
  parking_model_post_allocation_cleaning(instance_name, project_list_instance)
  parking_model_mapping(instance_name, project_list_instance, project_list)
}







