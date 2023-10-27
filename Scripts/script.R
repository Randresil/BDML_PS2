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
       skimr,
       glmnet)


getwd()
directorio <- "/Users/ricardoandressilvatorres/Documents/GitHub/BDML_PS2"
setwd(directorio)

install_formats() # Cuestiones de importacion de archivos del paquete rio
list.files()
list.files("Stores/")

# El template tiene un property_id y price como variables
template <- import("Stores/submission_template.csv")
rm(template)


## IMPORTACION DE DATOS -----------
# Importacion de base train y test
data <- import("Stores/train.csv")
data2 <- import("Stores/test.csv")

skim(data)
glimpse(data)
str(data)
names(data)


# Merge con variables de train y test
data <- data %>% mutate(div = "train")
data2 <- data2 %>% mutate(div = "test")

data_tot <- rbind(data, data2)




## CHEQUEO ESPACIAL LEAFLET ----------------
latitud_central <- mean(data_tot$lat)
longitud_central <- mean(data_tot$lon)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 10) %>% 
  addCircles(lng = data_tot$lon, 
             lat = data_tot$lat,
             color = data_tot$div)



## MANEJO DE TEXTOS ---------------------
# Elementos de interes:
# - garaje
# - habitaciones
# - bano
# - terraza
# - piso
# - parqueadero

# Suma de NAs en las columnas
apply(is.na(data_tot), MARGIN = 2, FUN = sum)
skim(data_tot)
glimpse(data_tot)


data_tot$description[1]
data_tot$description[2]
data_tot$description[3]
data_tot$description[4]

# Todo en minuscula
data_tot <- data_tot %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
data_tot <- data_tot %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
data_tot <- data_tot %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
data_tot <- data_tot %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))



## MANEJO ESPACIAL ------------
limites <- getbb("Bogota Colombia") # nos brinda la longitud y latitud en cuadrado de ciudad
bogota <- opq(bbox = getbb("Bogotá Colombia")) # 

available_tags("leisure") %>% print(n = Inf) # stadium, park
available_tags("amenity") %>% print(n = Inf) 
# bank, bus_station, college, hospital, police, university, pub
available_features() %>% head(Inf) 

