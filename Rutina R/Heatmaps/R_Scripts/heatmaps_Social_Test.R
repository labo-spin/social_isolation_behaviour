rm(list = ls())

#Valentín Azar registró la conducta social en la arena Dreosti (2023/2024) para animales desarrollados en aislamiento o en contexto social.
#Los videos fueron luego trackeados con DLC en base a esqueleto que incluye ambos ojos, vejiga natoria anterior y posterior, 
# y aprox. 5 puntos de la cola.
#Para este análisis, el animal se simplifica a un único punto calculado como el promedio entre las posiciones X e Y de ambos ojos.

#Packages

install.packages("ggplot2")
install.packages("readr")

library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(scales)


# Directory containing the CSV files
folder_path <- ("D:/Violeta_Medan/Social_Test/social_activity/data_head/Isolated")

position <- read_excel("D:/Violeta_Medan/Social_Test/position_social_arena.xlsx")

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

#####################################################################################
######## WHOLE FILE #################################################################
#####################################################################################

# Loop through each CSV file
for (file in csv_files) {
  print(file)
  file_name <- basename(file)
  title <- str_extract(file_name, "^[^_]+_[^_]+_[^_]+_[^_]+")  # Extracts "C2_21_A_S" from the example
  
  #para definir posicion de Social Chamber
  
  segments <- strsplit(title, "_")[[1]]
  
  cruza <- as.numeric(substr(segments[1], 2, nchar(segments[1])))
  individuo <- as.numeric(segments[2])
  cond_social <- segments[3]
  
  print(paste("Cruza:", cruza))
  print(paste("Individuo:", individuo))
  print(paste("Condición social:", cond_social))
  
  # Filtrar el dataframe "position"
  
  filtro <- position[position$Cruza == cruza & position$Individuo == individuo & position$CondSocial == cond_social, ]
  posicion <- filtro$Posicion
  print(paste("Posición:", posicion))
  
  # Agregar el valor de posición al título
  title_with_position <- paste(title, "Posicion:", posicion)
  print(title_with_position)
  
  # Read the CSV file into a data frame
  data <- read.table(file = file, header = FALSE, sep = ";")
  
  replace_comma_with_period <-
    function(x) {
      return(gsub(",", ".", x))
    }
  
  #data <- data.frame(lapply(data, function(x) replace_comma_with_period(as.character(x))))
  #data_selected <- data[4:nrow(data), c(2, 3, 5, 6)]
  #data_selected[] <- lapply(data_selected, as.numeric)
  
  #mean_x <- rowMeans(data_selected[, c(1, 3)])
  mean_y <- rowMeans(data_selected[, c(2, 4)])
  
  data_head <- data.frame(x = mean_x, y = mean_y)
  
  data_head$y <- -data_head$y + 1080
  
  plot <- ggplot(data_head, aes(x=x, y=y) ) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      scale_fill_distiller(palette="Spectral", direction=-1) +
      scale_x_continuous(limits = c(0, 368), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) +
      theme(legend.position='none') +
      coord_fixed(ratio = 0.5) +
      labs(title = title_with_position)
  print(plot)
}

#####################################################################################
######## 10 MIN OF SOCIAL INTERACTION OF WHOLE FILE ###############################################
#####################################################################################

# Loop through each CSV file
for (file in csv_files) {
  print(file)
  
  # pARA DEFINIR TÍTULO Y POSICIÓN DE SC EN EL VIDEO
  
  file_name <- basename(file)
  title <- str_extract(file_name, "^[^_]+_[^_]+_[^_]+_[^_]+")
  segments <- strsplit(title, "_")[[1]]
  
  cruza <- as.numeric(substr(segments[1], 2, nchar(segments[1])))
  individuo <- as.numeric(segments[2])
  cond_social <- segments[3]
  
  # Filtrar el dataframe "position"
  
  filtro <- position[position$Cruza == cruza & position$Individuo == individuo & position$CondSocial == cond_social, ]
  posicion <- filtro$Posicion
  print(paste("Posición:", posicion))
  title_with_position <- paste(title, "Posicion:", posicion)
  
  # Read the CSV file into a data frame
  data <- read.table(file = file, header = FALSE, sep = ";")
  
  replace_comma_with_period <-
    function(x) {
      return(gsub(",", ".", x)) }
  
  data <- data.frame(lapply(data, function(x) replace_comma_with_period(as.character(x))))
  data_selected <- data[4:nrow(data), c(2, 3, 5, 6)]
  data_selected[] <- lapply(data_selected, as.numeric)
  
  mean_x <- rowMeans(data_selected[, c(1, 3)])
  mean_y <- rowMeans(data_selected[, c(2, 4)])
  
  data_head <- data.frame(x = mean_x, y = mean_y)
  data_head$y <- -data_head$y + 1080
  
  data_social <- tail(data_head, 17980) 
  
  plot <- ggplot(data_social, aes(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_fill_distiller(palette="Spectral", direction=-1) +
    scale_x_continuous(limits = c(0, 368), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) +
    theme(legend.position='none') +
    coord_fixed(ratio = 0.5) +
    labs(title = title_with_position)
  print(plot)
}

#####################################################################################
######## 5 MIN OF PRE SOCIAL INTERACTION OF WHOLE FILE ############################################
######## tener en cuenta que animales de la camada 2 a veces no tienen pre-social ###
#####################################################################################

# Loop through each CSV file
for (file in csv_files) {
  print(file)
  
  # PARA DEFINIR TÍTULO Y POSICIÓN DE SC EN EL VIDEO
  
  file_name <- basename(file)
  title <- str_extract(file_name, "^[^_]+_[^_]+_[^_]+_[^_]+")
  segments <- strsplit(title, "_")[[1]]
  
  cruza <- as.numeric(substr(segments[1], 2, nchar(segments[1])))
  individuo <- as.numeric(segments[2])
  cond_social <- segments[3]
  
  # Filtrar el dataframe "position"
  
  filtro <- position[position$Cruza == cruza & position$Individuo == individuo & position$CondSocial == cond_social, ]
  posicion <- filtro$Posicion
  print(paste("Posición:", posicion))
  title_with_position <- paste(title, "Posicion:", posicion)
  
  # Read the CSV file into a data frame
  data <- read.table(file = file, header = FALSE, sep = ";")
  
  replace_comma_with_period <-
    function(x) {
      return(gsub(",", ".", x)) }
  
  data <- data.frame(lapply(data, function(x) replace_comma_with_period(as.character(x))))
  data_selected <- data[4:nrow(data), c(2, 3, 5, 6)]
  data_selected[] <- lapply(data_selected, as.numeric)
  
  mean_x <- rowMeans(data_selected[, c(1, 3)])
  mean_y <- rowMeans(data_selected[, c(2, 4)])
  
  data_head <- data.frame(x = mean_x, y = mean_y)
  data_head$y <- -data_head$y + 1080
  
  data_social <- head (data_head, 7192) 
  
  plot <- ggplot(data_social, aes(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_fill_distiller(palette="Spectral", direction=-1) +
    scale_x_continuous(limits = c(0, 368), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) +
    theme(legend.position='none') +
    coord_fixed(ratio = 0.5) +
    labs(title = title_with_position)
  print(plot)
}

#####################################################################################
######## AISLADOS: 10 MIN OF SOCIAL INTERACTION ###############################################
#####################################################################################


for (file in csv_files) {
  print(file)
  
  # Para definir título y posición de SC en el video
  file_name <- basename(file)
  title <- str_extract(file_name, "^[^_]+_[^_]+_[^_]+_[^_]+")
  segments <- strsplit(title, "_")[[1]]
  
  cruza <- as.numeric(substr(segments[1], 2, nchar(segments[1])))
  individuo <- as.numeric(segments[2])
  cond_social <- segments[3]
  
  # Solo procesar archivos con cond_social igual a "A"
  if (cond_social == "A") {
    # Filtrar el dataframe "position"
    filtro <- position[position$Cruza == cruza & position$Individuo == individuo & position$CondSocial == cond_social, ]
    posicion <- filtro$Posicion
    print(paste("Posición:", posicion))
    title_with_position <- paste(title, "Posicion:", posicion)
    
    # Leer el archivo CSV solo si cumple con la condición
    data <- read.table(file = file, header = FALSE, sep = ";")
    
    replace_comma_with_period <- function(x) {
      return(gsub(",", ".", x)) }
    
    data <- data.frame(lapply(data, function(x) replace_comma_with_period(as.character(x))))
    data_selected <- data[4:nrow(data), c(2, 3, 5, 6)]
    data_selected[] <- lapply(data_selected, as.numeric)
    
    mean_x <- rowMeans(data_selected[, c(1, 3)])
    mean_y <- rowMeans(data_selected[, c(2, 4)])
    
    data_head <- data.frame(x = mean_x, y = mean_y)
    data_head$y <- -data_head$y + 1080
    
    data_social <- tail(data_head, 17980) 
    
    plot <- ggplot(data_social, aes(x = x, y = y) ) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      scale_x_continuous(limits = c(0, 368), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) +
      theme(legend.position = 'none') +
      coord_fixed(ratio = 0.5) +
      labs(title = title_with_position)
    print(plot)
  }
}


#####################################################################################
######## SOCIALES: 10 MIN OF SOCIAL INTERACTION ###############################################
#####################################################################################


for (file in csv_files) {
  print(file)
  
  # Para definir título y posición de SC en el video
  file_name <- basename(file)
  title <- str_extract(file_name, "^[^_]+_[^_]+_[^_]+_[^_]+")
  segments <- strsplit(title, "_")[[1]]
  
  cruza <- as.numeric(substr(segments[1], 2, nchar(segments[1])))
  individuo <- as.numeric(segments[2])
  cond_social <- segments[3]
  
  # Solo procesar archivos con cond_social igual a "S"
  if (cond_social == "S") {
    # Filtrar el dataframe "position"
    filtro <- position[position$Cruza == cruza & position$Individuo == individuo & position$CondSocial == cond_social, ]
    posicion <- filtro$Posicion
    print(paste("Posición:", posicion))
    title_with_position <- paste(title, "Posicion:", posicion)
    
    # Leer el archivo CSV solo si cumple con la condición
    data <- read.table(file = file, header = FALSE, sep = ";")
    
    replace_comma_with_period <- function(x) {
      return(gsub(",", ".", x)) }
    
    data <- data.frame(lapply(data, function(x) replace_comma_with_period(as.character(x))))
    data_selected <- data[4:nrow(data), c(2, 3, 5, 6)]
    data_selected[] <- lapply(data_selected, as.numeric)
    
    mean_x <- rowMeans(data_selected[, c(1, 3)])
    mean_y <- rowMeans(data_selected[, c(2, 4)])
    
    data_head <- data.frame(x = mean_x, y = mean_y)
    data_head$y <- -data_head$y + 1080
    
    data_social <- tail(data_head, 17980) 
    
    plot <- ggplot(data_social, aes(x = x, y = y) ) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      scale_x_continuous(limits = c(0, 368), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) +
      theme(legend.position = 'none') +
      coord_fixed(ratio = 0.5) +
      labs(title = title_with_position)
    print(plot)
  }
}
