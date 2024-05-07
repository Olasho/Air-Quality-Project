# Loading the packages

library(tidyverse)
library(readxl)

# loading the data
getwd()
street_apperley <- readxl::read_xlsx("./input_Data/Bradford Apperley Bridge Marina Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx",
                                     skip = 3, col_names = TRUE)


street_cavendish <- readxl::read_xlsx("./input_Data/Bradford Cavendish Prim Sch Hall Rd Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx",
                                      skip = 3, col_names = TRUE)


street_EastBowling <- readxl::read_xlsx("./input_Data/Bradford East Bowling Flockton Grove Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx",
                                      skip = 3, col_names = TRUE)


street_Fairfield <- readxl::read_xlsx("./input_Data/Bradford Farfield Prim Sch Reevy Cres Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                      skip = 3, col_names = TRUE)


street_Girlington <- readxl::read_xlsx("./input_Data/Bradford Girlington St Leonards Road Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                       skip = 3, col_names = TRUE)


street_GT_Horton <- readxl::read_xlsx("./input_Data/Bradford Gt Horton All Saints Prim Sch Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                      skip = 3, col_names = TRUE)


street_Horton <- readxl::read_xlsx("./input_Data/Bradford Horton Aberdeen Place Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                   skip = 3, col_names = TRUE)


street_Ilkley <- readxl::read_xlsx("./input_Data/Bradford Ilkley Grove Road Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                   skip = 3, col_names = TRUE)


street_Keighley <- readxl::read_xlsx("./input_Data/Bradford Keighley Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                     skip = 3, col_names = TRUE)


street_low_moor <- readxl::read_xlsx("./input_Data/Bradford Low Moor First Street Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                     skip = 3, col_names = TRUE)


street_saltaire <- readxl::read_xlsx("./input_Data/Bradford Saltaire Titus Street Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                     skip = 3, col_names = TRUE)


street_silsden <- readxl::read_xlsx("./input_Data/Bradford Silsden Hothfield Street Zephyr 15-Minute Means Mass units 8 June to 20 September 2023.xlsx", 
                                    skip = 3, col_names = TRUE)


# Ensuring the dates are in UTC

head(street_apperley$Date) 

# Checking for number of empty rows

sum(!complete.cases(street_apperley$`Nitric Oxide µg m-3 (20'C 1013mb)`))

# Creating a function to get rid of empty cells by row

remove_missing_roW <-  function(df, lower_empty_value, higher_empty_value) {
  
  missing_cells <- vector("numeric", length = nrow(df)) # create empty vector
  
  for (i in 1:nrow(df)) {
    missing_cells[i] <- sum(is.na(df[i, ]))                # fill empty vector with number of missing cells per row
  }
  
  df_1 <- df %>%
    mutate(number_of_empty_cells = missing_cells)         # create a new column for the number of empty cells
  
  df_2 <- df_1 %>%
    filter(!(number_of_empty_cells >= lower_empty_value & number_of_empty_cells <= higher_empty_value))          # this filters out the rows with the specified number of empty cells
  
  df_3 <- subset(df_2, select = -number_of_empty_cells)   # removing the column for number of emtpy cells
  
  return(df_3)                                            # returning the dataframe 
}

# Applying the funtion to all the datasets to get rid of rows with more than 8 empty cells

street_apperley <- remove_missing_roW(street_apperley, 8, 9)

street_cavendish <- remove_missing_roW(street_cavendish, 8, 9)

street_EastBowling <- remove_missing_roW(street_EastBowling, 8, 9)

street_Fairfield <- remove_missing_roW(street_Fairfield, 8, 9)

street_Girlington <- remove_missing_roW(street_Girlington, 8, 9)

street_GT_Horton <- remove_missing_roW(street_Horton, 8, 9)

street_Horton <- remove_missing_roW(street_Horton, 8, 9)

street_Ilkley <- remove_missing_roW(street_Ilkley, 8, 9)

street_Keighley <- remove_missing_roW(street_Keighley, 8, 9)

street_low_moor <- remove_missing_roW(street_low_moor, 8, 9)

street_saltaire <- remove_missing_roW(street_saltaire, 8, 9)

street_silsden <- remove_missing_roW(street_silsden, 8, 9)

# Adding the school street name to each dataset in separate columns

street_apperley$School_street <- "Apperley Bridge marina"

street_cavendish$School_street <- "Cavendish Prim Sch Hall Rd"

street_EastBowling$School_street <- "East Bowling Flockton Grove"

street_Fairfield$School_street <- "Farfield PRim Sch Reevy"

street_Girlington$School_street <- "Girlington St Leonards Road"

street_GT_Horton$School_street <- "Gt Horton All Saints Primary School"

street_Horton$School_street <- "Horton Aberdeen Place"

street_Ilkley$School_street <- "Ilkley Grove Road"

street_Keighley$School_street <- "Keighley"

street_low_moor$School_street <- "Low Moor First"

street_saltaire$School_street <- "Saltaire Titus"

street_silsden$School_street <- "Silsden Hothfield"

# Adding the school street ward to each dataset in separate columns

street_apperley$Ward <- "Idle and Thackley"

street_cavendish$Ward <- "Eccleshill"

street_EastBowling$Ward <- "Bowling and Barkerend"

street_Fairfield$Ward <- "Wibsey"

street_Girlington$Ward <- "Toller"

street_GT_Horton$Ward <- "Great Horton"

street_Horton$Ward <- "Great Horton"

street_Ilkley$Ward <- "Ilkley"

street_Keighley$Ward <- "Keighley West"

street_low_moor$Ward <- "Wyke"

street_saltaire$Ward <- "Shipley"

street_silsden$Ward <- "Craven"

# Adding the 2022 ward code for each ward

street_apperley$Ward_code <- "E05001353"

street_cavendish$Ward_code <- "E05001350"

street_EastBowling$Ward_code <- "E05001345"

street_Fairfield$Ward_code <- "E05001367"

street_Girlington$Ward_code <- "E05001364"

street_GT_Horton$Ward_code <- "E05001351"

street_Horton$Ward_code <- "E05001351"

street_Ilkley$Ward_code <- "E05001354"

street_Keighley$Ward_code <- "E05001357"

street_low_moor$Ward_code <- "E05001370"

street_saltaire$Ward_code <- "E05001362"

street_silsden$Ward_code <- "E05001349"

# Joining all the school streets together

street_all_schools <- rbind(street_apperley, street_cavendish,
                            street_EastBowling, street_Fairfield, 
                            street_Girlington, street_GT_Horton,
                            street_Horton, street_Ilkley,
                            street_Keighley, street_low_moor,
                            street_saltaire, street_silsden)

# Reordering the variables so the ward code, ward, and school street come first

street_all_schools <- street_all_schools %>%
  select(Ward_code, Ward, School_street, everything())

# Filling the month column with the appropriate month
# converting date from POSIXct to character

street_all_schools_2 <- street_all_schools

street_all_schools_2$Date <- as.character(street_all_schools_2$Date) 

street_all_schools_2$Month <- ifelse(str_detect(
  street_all_schools_2$Date, "06"), "June", 
  ifelse(str_detect(street_all_schools_2$Date, "07"), "July", 
  ifelse(str_detect(street_all_schools_2$Date, "08"), "August", 
  ifelse(str_detect(street_all_schools_2$Date, "09"), "September", "others"))))

# converting the date back to GMT

street_all_schools_2$Date <- ymd(street_all_schools_2$Date, tz = "GMT")

# Separating the date from the time

street_all_schools_2 <- separate(street_all_schools_2, Hour, into = c("ate", "Hour"), sep = " ") 

# Removing the ate column created

street_all_schools_2 <- subset(street_all_schools_2, select = -ate)

# Creating the excel format of the aggregated data of all the locations 

writexl::write_xlsx(street_all_schools_2, "./output_data/All_school_street.xlsx")

# Renaming the columns

street_new_column_names <- c("Ward-Code", "Ward", "School street", "Date", "Month",
                             "Hour", "Temperature \u00B0C", "Atmospheric Pressure",
                             "NO", "NO2", "O3", "PM1", "PM10", "PM25")

names(street_all_schools_2) <- street_new_column_names

# Here, I am going to create a new dataframe to combine all the pollutants into a single column

street_all_schools_3 <- pivot_longer(street_all_schools_2, cols = c("NO", "NO2", "O3", "PM25", "PM1", "PM10"),  names_to = "Pollutants", values_to = "Concentration")


View(street_all_schools_2)
