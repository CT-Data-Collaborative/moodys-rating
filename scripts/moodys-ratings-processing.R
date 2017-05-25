library(dplyr)
library(datapkg)
library(stringi)

##################################################################
#
# Processing Script for Moodys Ratings
# Created by Jenna Daly
# On 05/25/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
#grabs all csvs (even not FISCIN data)
all_csvs <- dir(path, recursive=T, pattern = ".csv") 

#create empty data frame with set columns
all_data <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), 
                     c("Town", 
                       "Year", 
                       "Value"))

#read in each raw file and get ready for master combine
for (i in 1:length(all_csvs)) {
  current_file <- read.csv(paste0(path, "/", all_csvs[i]), stringsAsFactors=F, header=T)
  remove_folder <- sub(".*/", "", all_csvs[i]) #filename
  get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(remove_folder)), "")))
  get_year <- get_year + 2000
  SFY <- paste(get_year - 1, get_year, sep = "-")
  SFY2 <- paste("SFY", SFY)
  current_file$Year <- SFY2
  col_names <- colnames(current_file)
  moody_col <- grep("moody", col_names, ignore.case=T, value=T)
  final_columns <- current_file[, c("Municipality", 
                                    "Year", 
                                    moody_col[1])]  
  names(final_columns) <- c("Town", 
                            "Year", 
                            "Value")
  
  # take out "Groton (City of)" because it is a political subdivision of groton and not the town.
  final_columns <- final_columns[final_columns$Town != "GROTON (City of)",]
  
  # Add this iteration's data to main container, first removing duplicated year data
  all_data <- all_data[all_data$Year!=SFY2,]
  all_data <- rbind(all_data, final_columns)
}

# Town names to title case
all_data$Town <- stri_trans_totitle(all_data$Town)
all_data$Value <- trimws(all_data$Value)

#Add Measure Type, Variable, and FIPS
all_data$"Measure Type" <- "Moody's Rating"
all_data$"Variable" <- "Moody's Rating"

town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

all_data_fips <- merge(all_data, fips, all=T)

#remove "Connecticut"
moodys_data <- all_data_fips[!all_data_fips$Town == "Connecticut",]

#Reorder and sort columns
moodys_data <- moodys_data %>% 
  select(Town, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(Year, Variable, `Measure Type`, Town)

# Write to File
write.table(
  moodys_data,
  file.path(getwd(), "data", "moodys_ratings_2015.csv"),
  sep = ",",
  row.names = F
)

