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


# Parqueaderos
data_tot <- data_tot %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero|garaje|estacionamiento|parqueo", data_tot$description)))
data_tot %>% count(parqueadero)

# Terraza
data_tot <- data_tot %>%
  mutate(terraza = as.numeric(grepl("jardin|terraza|azotea|balcon", data_tot$description)))
data_tot %>% count(terraza)

# Piscina
data_tot <- data_tot %>% 
  mutate(piscina = as.numeric(grepl("piscina|jacuzzi|sauna|turco", data_tot$description)))
data_tot %>% count(piscina)

# Conjunto
data_tot <- data_tot %>% 
  mutate(conjunto = as.numeric(grepl("conjunto", data_tot$description)))
data_tot %>% count(conjunto)

# Apartaestudio
data_tot <- data_tot %>% 
  mutate(apartaestudio = as.numeric(grepl("apartaestudio", data_tot$description)))
data_tot %>% count(apartaestudio)


# Pisos
data_tot <- data_tot %>% 
  mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", 
                      "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei", "once|onceavo")
numeros_numericos <- as.character(1:11)

data_tot <- data_tot %>% 
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))

data_tot <- data_tot %>% 
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

data_tot <- data_tot %>% 
  mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico))

data_tot <- data_tot %>% 
  mutate(piso_numerico = replace_na(piso_numerico, 1))
skim(data_tot$piso_numerico)


# Baños
data_tot <- data_tot %>% 
  mutate(bano_number = str_count(description, "bano"))
skim(data_tot$bano_number)

data_tot <- data_tot %>% 
  mutate(bano_info= str_extract(description, "(\\w+|\\d+) bano (\\w+|\\d+)"))

data_tot <- data_tot %>% 
  mutate(bano_numerico = as.integer(str_extract(bano_info, "\\d+")))

data_tot <- data_tot %>% 
  mutate(bano_numerico = ifelse(bano_numerico > 10, NA, bano_numerico))

media <- round(mean(data_tot$bano_numerico, na.rm = TRUE))
data_tot <- data_tot %>% 
  mutate(bano_numerico = replace_na(bano_numerico, media))
skim(data_tot$bano_numerico)

# Habitaciones predeterminadas por la base
media <- round(mean(data_tot$bathrooms, na.rm = TRUE))
data_tot <- data_tot %>% 
  mutate(bathrooms = replace_na(bathrooms, media))
skim(data_tot$bathrooms)


# Habitaciones
data_tot <- data_tot %>% 
  mutate(habitaciones_info= str_extract(description, "(\\w+|\\d+) (habitaciones|cuartos) (\\w+|\\d+)"))

numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", 
                      "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei", "once|onceavo")
numeros_numericos <- as.character(1:11)

data_tot <- data_tot %>% 
  mutate(habitaciones_numerico = as.integer(str_extract(habitaciones_info, "\\d+")))

data_tot <- data_tot %>% 
  mutate(habitaciones_numerico = ifelse(habitaciones_numerico > 11, NA, habitaciones_numerico))

media <- round(mean(data_tot$habitaciones_numerico, na.rm = TRUE))
data_tot <- data_tot %>% 
  mutate(habitaciones_numerico = replace_na(habitaciones_numerico, media))
skim(data_tot$habitaciones_numerico)

# Habitaciones predeterminadas por la base
media <- round(mean(data_tot$rooms, na.rm = TRUE))
data_tot <- data_tot %>% 
  mutate(rooms = replace_na(rooms, media))
skim(data_tot$rooms)



# Metros cuadrados (m2, mt2, mts2)
for (i in c(1:10)) {
  print(data_tot$description[i])
}

sum(grepl(c("m2 | mt2 | mts2 | metros2 | metros cuadrados"), data_tot$description))

data_tot <- data_tot %>% 
  mutate(metros = case_when(
    grepl("m2", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) m2"),
    grepl("mt2", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) mt2"),
    grepl("metros2", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) metros2"),
    grepl("metros cuadrados", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) metros cuadrados"),
    .default = NA ))

data_tot <- data_tot %>% 
  mutate(metros_num = as.integer(str_extract(metros, "\\d+")))
skim(data_tot$metros_num)



## VARIABLES EXTRA DE TITLE Y DESCRIPTION
# duplex, penthouse, vista
for (i in c("vista", "duplex", "penthouse")){
  a <- sum(grepl(i, data_tot$title))
  b <- sum(grepl(i, data_tot$description))
  cat(paste("Para", i, "En el titulo se tiene:", a, "Y en descripción:", b, "\n"))
  cat(paste("Total de:", a+b),"\n")
}

# Duplex, penthouse, vista
data_tot <- data_tot %>% 
  mutate(duplex = as.numeric(grepl("duplex", data_tot$description)) + as.numeric(grepl("duplex", data_tot$title)))
data_tot %>% count(duplex)
data_tot$duplex[data_tot$duplex == 2] <- 1

data_tot <- data_tot %>% 
  mutate(vista = as.numeric(grepl("vista", data_tot$description)) + as.numeric(grepl("vista", data_tot$title)))
data_tot %>% count(vista)
data_tot$vista[data_tot$vista == 2] <- 1

data_tot <- data_tot %>% 
  mutate(penthouse = as.numeric(grepl("penthouse", data_tot$description)) + as.numeric(grepl("penthouse", data_tot$title)))
data_tot %>% count(penthouse)
data_tot$penthouse[data_tot$penthouse == 2] <- 1

# Dummy de casa
data_tot <- data_tot %>% 
  mutate(casa = ifelse(data_tot$property_type == "Casa", 1, 0),
         casa_title = as.numeric(grepl("casa", data_tot$title)))
data_tot %>% count(casa)
data_tot %>% count(casa_title)
data_tot <- data_tot %>% select(-casa_title)
data_tot <- data_tot %>%
  mutate(casa = as.factor(casa))


## MANEJO ESPACIAL ------------
limites <- getbb("Bogota Colombia") # nos brinda la longitud y latitud en cuadrado de ciudad
bogota <- opq(bbox = getbb("Bogotá Colombia")) # 

available_tags("leisure") %>% print(n = Inf) # stadium, park, nature_reserve
available_tags("amenity") %>% print(n = Inf) # bank, bus_station, college, hospital, police, university, pub, veterinary
available_tags("tourism") %>% print(n = Inf) # museum
available_tags("shop") %>% print(n = Inf) # mall

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


# FOR LOOP PARA AMENITY
for (i in c("bank", "bus_station", "college", "hospital", "police", "university", "pub", "veterinary")) {
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


# FOR LOOP PARA LEISURE
for (i in c("mall")) {
  print(i)
  # Extraccion de informacion
  A <- paste0("var", i)
  A <- bogota %>%
    add_osm_feature(key = "shop" , value = i) 
  
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

# FOR LOOP PARA SHOP
for (i in c("mall")) {
  print(i)
  # Extraccion de informacion
  A <- paste0("var", i)
  A <- bogota %>%
    add_osm_feature(key = "shop" , value = i) 
  
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


# FOR LOOP PARA LEISURE
for (i in c("nature_reserve")) {
  print(i)
  # Extraccion de informacion
  A <- paste0("var", i)
  A <- bogota %>%
    add_osm_feature(key = "leisure" , value = i) 
  
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




## DATAFRAMES OF TRAIN AND TEST AFTER MANIPULATION ------------------
# Variables de interes para nuestro modelaje
# Y = price

# X basicos = rooms, bedrooms, bathrooms, property_type 
# X construidos espacial = bank, bus_station, college, hospital, police, university
#                          pub, veterinary, mall, nature_reserve, 

# X DUMMIES:               parqueadero, terraza, piscina, conjunto, apartaestudio, duplex, vista, penthouse
# X construidos description = piso_numerico, bano_numerico, habitaciones_numerico, bano_number, metros_num

dummies <- c("parqueadero", "terraza", "piscina", "conjunto", "apartaestudio",
             "duplex", "vista", "penthouse")
data_tot <- data_tot %>%  mutate_at(dummies, as.factor)


data <- data_tot %>% filter(div == "train")
data2 <- data_tot %>% filter(div == "test")


fitControl <- trainControl(method ="cv", number=5)
tree_lenght_geo <- train(
  price ~ bedrooms + bathrooms + terraza + bus_station + police + casa ,
  data=data,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=100
)
tree_lenght_geo
prp(tree_lenght_geo$finalModel, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,tweak=1.2,clip.facs= TRUE,box.palette = "Blues")
cp_geo <- predict(tree_lenght_geo, data2)
cp_geo
