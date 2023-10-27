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
       stargazer,  # Tablas bonitas de regs y estad desc
       rgeos, # Calcular centroides de un poligono
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


# Extraemos la info de todos los parques
park <- bogota %>%
  add_osm_feature(key = "leisure" , value = "park") 

stadium <- bogota %>% 
  add_osm_feature(key = "leisure", value = "stadium")

# Cambiamos el formato para que sea un objeto sf (simple features)
park_sf <- osmdata_sf(park)
stadium_sf <- osmdata_sf(stadium)


# Seleccion de poligonos de las variables de leisure
# y sus respectivos centroides 
park_geometria <- park_sf$osm_polygons %>% 
  select(osm_id, name)
centroides_park <- gCentroid(as(park_geometria$geometry, "Spatial"), byid = T)

stadium_geometria <- stadium_sf$osm_polygons %>% 
  select(osm_id, name)
centroides_stadium <- gCentroid(as(stadium_geometria$geometry, "Spatial"), byid = T)


# Transformacion de nuestros datos al tipo de simple features sf
data_tot_sf <- st_as_sf(data_tot, coords = c("lon", "lat"), crs=4326)

centroides_park_sf <- st_as_sf(centroides_park, coords = c("lon", "lat"), crs=4326)
centroides_stadium_sf <- st_as_sf(centroides_stadium, coords = c("lon", "lat"), crs=4326)

nearest_park <- st_nearest_feature(data_tot_sf,centroides_park_sf)
nearest_stadium <- st_nearest_feature(data_tot_sf,centroides_stadium_sf)

data_tot <- data_tot %>% mutate(distancia_park = st_distance(x = data_tot_sf, y = centroides_park_sf[nearest_park,], by_element=TRUE),
                                distancia_stadium = st_distance(x = data_tot_sf, y = centroides_stadium_sf[nearest_stadium,], by_element=TRUE))



for (i in c("bank", "bus_station", "college", "hospital", "police", "university", "pub")) {
  print(i)
  
  # Extraccion de informacion
  A <- paste0("var", i)
  A <- bogota %>%
    add_osm_feature(key = "amenity" , value = i) 
  
  # Formato sf
  A_sf <- osmdata_sf(A)
  
  # Seleccion de poligonos y centroides
  B <- A_sf$osm_polygons %>% 
    select(osm_id, name)
  
  centroides_B <- gCentroid(as(B$geometry, "Spatial"), byid = T)
  
  # Centroides y mas cercano
  centroides_B_sf <- st_as_sf(centroides_B, coords = c("lon", "lat"), crs=4326)
  nearest_B <- st_nearest_feature(data_tot_sf,centroides_B_sf)
  
  # Añadir variable a base de datos
  data_tot <- data_tot %>% mutate(!!i := st_distance(x = data_tot_sf, y = centroides_B_sf[nearest_B,], by_element=TRUE))
}

