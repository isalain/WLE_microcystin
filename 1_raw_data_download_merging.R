######################################################
### DATA DOWNLOAD AND PREPROCESSING ##################
######################################################
library(dplyr)
library(lubridate)
# data links
urls <- c("https://www.nodc.noaa.gov/archive/arc0204/0254720/1.1/data/0-data/noaa-glerl-erie-habs-field-sampling-results-2020-2021.csv",
          "https://www.nodc.noaa.gov/archive/arc0152/0209116/1.1/data/0-data/lake_erie_habs_field_sampling_results_2019.csv",
          "https://www.nodc.noaa.gov/archive/arc0135/0187718/2.2/data/0-data/lake_erie_habs_field_sampling_results_2012_2018_v2.csv",
          "https://www.ncei.noaa.gov/data/oceans/archive/arc0225/0292222/1.1/data/0-data/noaa-glerl-erie-habs-field-sampling-results-2022.csv")
folder_path <- "C:/Users/aisabwe/Desktop/pMicr"
for (i in 1:length(urls)) {
  url_filename <- basename(urls[i])
  filename <- paste0("file_", i, ".csv")
  destination <- file.path(folder_path, filename)
  download.file(urls[i], destination, method = "auto")
}

# Read raw data files
RawData1 <- read.csv("C:/Users/aisabwe/Desktop/pMicr/file_1.csv", check.names = FALSE)
RawData2 <- read.csv("C:/Users/aisabwe/Desktop/pMicr/file_2.csv", check.names = FALSE)
RawData3 <- read.csv("C:/Users/aisabwe/Desktop/pMicr/file_3.csv", check.names = FALSE)
RawData4 <- read.csv("C:/Users/aisabwe/Desktop/pMicr/file_4.csv", check.names = FALSE)
RawData5 <- read.csv("C:/Users/aisabwe/Desktop/pMicr/Lake_Erie_Merged_Data_2023.csv", check.names = FALSE)

# Define column selection and naming for primary datasets
SelectCol <- c(1,2,5,7,8,13,15,19,20,21,22,23,24,25,26,27,28,32)
SimpNames <- c("Date","Site","Category","Lat","Lon","Temp", "Beam_att", 
               "Turb", "pMicr", "dMicr", "Phyco", "Chla", "TP", "TDP", 
               "SRP", "NH3", "NOx","TSS")

# Process RawData1
for (i in 1:length(SelectCol)) {
  colnames(RawData1)[SelectCol[i]] <- SimpNames[i]
}
RawData1 <- subset(RawData1, select = SelectCol)

# Process RawData2
for (i in 1:length(SelectCol)) {
  colnames(RawData2)[SelectCol[i]] <- SimpNames[i]
}
RawData2 <- subset(RawData2, select = SelectCol)

# Process RawData3 (different structure)
SelectCol3 <- c(1, 2, 5, 7, 8,13, 14, 16, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,35)
SimpNames3 <- c("Date", "Site","Category", "Lat", "Lon", "TempSamp", "TempCTD", "Beam_att", 
                "Turb", "pMicr", "dMicr", "Phyco", "Chla", "TP", "TDP", "SRP", "NH3", "NOx", "TSS")
colnames(RawData3)[SelectCol3] <- SimpNames3
RawData3$TempSamp <- RawData3[, 13]
RawData3 <- RawData3[, SelectCol3]
RawData3  <- RawData3  %>%
  mutate(Temp = coalesce(TempSamp, TempCTD)) %>%
  dplyr::select(-TempSamp, -TempCTD)

# Process RawData4 
SelectCol4 <- c(1, 2, 5, 7, 8, 12, 14, 18,19,20,21,22,25,26,27,28,29,33)
SimpNames4 <- c("Date", "Site","Category", "Lat", "Lon", "Temp", "Beam_att", 
                "Turb", "pMicr", "dMicr", "Phyco", "Chla", "TP", "TDP", "SRP", "NH3", "NOx","TSS")

for (i in 1:length(SelectCol4)) {
  colnames(RawData4)[SelectCol4[i]] <- SimpNames4[i]
}
RawData4 <- subset(RawData4, select = SelectCol4)

# Process RawData5 
SelectCol5 <- c(1, 2, 3, 7, 8, 13, 15, 19, 20, 21, 22, 23, 26, 27, 28, 29, 30, 33)  
SimpNames5 <- c("Date", "Site", "Category", "Lat", "Lon", "Temp", "Beam_att", 
                "Turb", "pMicr", "dMicr", "Phyco", "Chla", "TP", "TDP", "SRP", "NH3", "NOx","TSS")

# Rename columns
for (i in 1:length(SelectCol5)) {
  colnames(RawData5)[SelectCol5[i]] <- SimpNames5[i]
}

# Subset to keep only selected columns
RawData5 <- RawData5[, SelectCol5]

# Combine all datasets and filter for surface samples and main stations
RawData <- rbind(RawData1, RawData2, RawData3, RawData4, RawData5)
RawData <- RawData[RawData$Category == "Surface", ]
known_sites <- c("WE2", "WE4","WE6","WE8","WE9","WE12","WE13","WE14","WE15","WE16")
RawData <- RawData[RawData$Site %in% known_sites, ]



# Process dates
RawData$Date <- as.Date(RawData$Date, format = "%m/%d/%Y")
RawData$day_of_year <- yday(RawData$Date)
RawData$week_of_year <- week(RawData$Date)
RawData$Year <- paste0("Year_", year(RawData$Date))
RawData$pMicr <- ifelse(RawData$pMicr == "bdl", "<0.1", RawData$pMicr)
# dealing with chr into numeric

numeric_cols <- c("Temp", "Beam_att", "Turb", "Phyco", "TP", "TDP", "SRP", "NH3", "NOx", "TSS")

# Function to handle "<" values by replacing with NA
convert_to_numeric <- function(x) {
  # Replace any values with "<" with NA
  x[grep("<", x)] <- NA
  # Convert to numeric
  as.numeric(x)
}

# Apply the conversion to each column
for (col in numeric_cols) {
  if (col %in% names(RawData)) {
    RawData[[col]] <- convert_to_numeric(RawData[[col]])
  }
}
# Adding row identifier
RawData$original_id <- 1:nrow(RawData)
str(RawData)







