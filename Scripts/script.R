## PROBLEM SET 2 ##
# Intro to Big Data & Machine Learning
# R Version: 4.3.1

rm(list = ls())

install.packages("pacman")
library(pacman)

p_load(tidyverse,  # Manipulacio de dataframes
       sf,         # Leer, escribir y manipular datos espaciales
       tmaptools,  # Conexcion de API de OSM
       osmdata,    # Obtener datos de Open Street Map (OSM)
       leaflet,    # Mapas interactivos
       tidymodels, # Para modelos de ML
       rio,        # Importacion
       stargazer,
       skimr)


getwd()
directorio <- "/Users/ricardoandressilvatorres/Documents/GitHub/BDML_PS2"
setwd(directorio)

install_formats() # Cuestiones de importacion de archivos del paquete rio
list.files()
list.files("Stores/")

# El template tiene un property_id y price como variables
template <- import("Stores/submission_template.csv")
rm(template)


# Importacion de base train y test
data <- import("Stores/train.csv")
data2 <- import("Stores/test.csv")

skim(data)
glimpse(data)
str(data)
names(data)
