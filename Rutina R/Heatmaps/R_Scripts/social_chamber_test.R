rm(list = ls())

library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(fs) 

folder_path <- "D:/Violeta_Medan/Social_Test/social_activity"
position <- read_excel("D:/Violeta_Medan//Social_Test/position_social_arena.xlsx")

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
print(csv_files)

# Create the "social_activity" subfolder if it doesn't exist
# social_activity_path <- file.path(directory_path, "social_activity")
# if (!file.exists(social_activity_path)) {
#   dir.create(social_activity_path, recursive = TRUE)
# }

# Loop through each file in the csv_files list
# for (file in csv_files) {
#   
#   # Read the original CSV file
#   data <- read.table(file = file, header = FALSE, sep = ";")
#   replace_comma_with_period <-
#     function(x) {
#       return(gsub(",", ".", x))
#     }
#   
#   data <- data.frame(lapply(data, function(x) replace_comma_with_period(as.character(x))))
#   
#   # Extract the last 17900 rows
#   data_tail <- tail(data, 17900)
#   
#   # Get the filename without the path
#   file_name <- basename(file)
#   
#   # Define the new file path in the "social_activity" subfolder
#   new_file_path <- file.path(social_activity_path, file_name)
#   
#   # Write the extracted data to a new CSV file
#   write.csv(data_tail, new_file_path, row.names = FALSE)
#   
#   cat("New file created:", new_file_path, "\n")
# }

# Define the folder path and create the "data_head" subfolder if it doesn't exist
data_head_folder <- file.path(folder_path, "data_head")
if (!file.exists(data_head_folder)) {
  dir.create(data_head_folder)
}

# Loop through each CSV file in "data_head" subfolder and produce new files with only x nd y coordinates of the head

for (file in csv_files) {
  print(file)
  file_name <- basename(file)
  
  title <- str_extract(file_name, "^[^_]+_[^_]+_[^_]+_[^_]+")  # Extracts "C2_21_A_S" from the example
    segments <- strsplit(title, "_")[[1]]
  
  cruza <- as.numeric(substr(segments[1], 2, nchar(segments[1])))
  individuo <- as.numeric(segments[2])
  cond_social <- segments[3]
  
  # Read the CSV file into a data frame
  data <- read.table(file = file, header = FALSE, sep = ",")

    
  # replace_comma_with_period <-
  #   function(x) {
  #     return(gsub(",", ".", x))
  #   }
  # 
  # data <- data.frame(lapply(data, function(x) replace_comma_with_period(as.character(x))))

  data_selected <- data[2:nrow(data), c(2, 3, 5, 6)]
  data_selected[] <- lapply(data_selected, as.numeric)
  
  mean_x <- rowMeans(data_selected[, c(1, 3)])
  mean_y <- rowMeans(data_selected[, c(2, 4)])
  
  data_head <- data.frame(x = mean_x, y = mean_y)
  
  # Define the file path for the data_head CSV file
  data_head_file <- file.path(data_head_folder, paste0(title, "_data_head.csv"))
  
  # Write the data_head data frame to a CSV file
  write.csv(data_head, data_head_file, row.names = FALSE)
}

#=========================================================================


# Define the folder paths
data_head_folder <- "D:/Violeta_Medan/Social_Test/social_activity/data_head"
isolated_folder <- file.path(data_head_folder, "Isolated")
social_folder <- file.path(data_head_folder, "Social")

# Create the "Isolated" and "Social" folders if they don't exist
dir.create(isolated_folder, showWarnings = FALSE)
dir.create(social_folder, showWarnings = FALSE)

# Get a list of all CSV files in the data_head folder
csv_files <- dir(data_head_folder, pattern = "\\.csv$", full.names = TRUE)

##########DOES NOT WORK.....

# Loop through each file
for (file in csv_files) {
  # Extract filename without extension
  filename <- fs::file_remove_extension(basename(file))
  
  # Split the filename by "_"
  filename_parts <- strsplit(filename, "_")[[1]]
  
  # Check if "A" or "S" is present in the filename parts
  if ("A" %in% filename_parts) {
    destination_folder <- isolated_folder
  } else if ("S" %in% filename_parts) {
    destination_folder <- social_folder
  } else {
    next  # Skip files with unexpected filenames
  }
  
  # Move the file to the destination folder
  file_move(file, file.path(destination_folder, basename(file)))
}
  