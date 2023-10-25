
## Ahora importo test con puntos, le puse formato puntos, no se si funcione

train <- read.csv("~/Octavo semestre/Big data and machine learning/Taller2/train_latitud.csv")
test <- read.csv("~/Octavo semestre/Big data and machine learning/Taller2/Test_latitud.csv")


# List of variable names (replace with your actual variable names)
variable_names <- c( "lat", "lon", "price" )
# Use lapply to apply class and typeof to each variable
class_types <- lapply(variable_names, function(var_name) {
  var <- train[[var_name]]
  list(variable = var_name, class = class(var), type = typeof(var))
})

# Print the summary of data types
print(class_types)

require("pacman")
p_load("tidyverse","stargazer")

p_load("glmnet")

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels) #para modelos de ML






leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)



# Ahora solo nos quedaremos con las observaciones que efectivamente están dentro de Bogota y no están mal georeferenciadas
limites <- getbb("Bogota Colombia")
train <- train %>%
  filter(
    between(X.lon., limites[1, "min"], limites[1, "max"]) & 
      between(X.lat., limites[2, "min"], limites[2, "max"]))



# Calcular estadísticas descriptivas para la variable X.price.
summary(train$X.price.)


# Crear un histograma con límites en el eje x
hist(train$price, 
     main = "Histograma de Precios",
     xlab = "Precio",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "black",
     breaks = 50,
     xlim = c(200000000, 1000000000)  # Establece los límites del eje x
)



parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar s ubciacion como un solo punto 
centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)


# Creamos el mapa de Bogota
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada parque
# Priemro tomamos niestros datos geoespaciales y los convertimos al formato sf (simple features)
# Esto para que esten en el mismo formato de los parques y poder sacar distancia. 
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(train_sf) <- 4326
# convertimos los scontroides a formato sf(simple features)
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
# Esto va a ser demorado!
# Calculamos las diatnacias para cada combinacion immueble - parque
dist_matrix <- st_distance(x = train_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_parque = dist_min)


available_tags("leisure")
available_tags("amenity")
available_tags("public services")


# Run the available_tags function to get a list of tags for "amenity"
tags <- available_tags("amenity")

# Use print with n parameter to display more rows
print(tags, n = 90)

##_______________ AHORA CON TEST________________


# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, 
             lat = test$lat)


latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)

# Ahora solo nos quedaremos con las observaciones que efectivamente están dentro de Bogota y no están mal georeferenciadas
limites <- getbb("Bogota Colombia")
test <- test %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"]))

# Calcular estadísticas descriptivas para la variable X.price.
summary(test$X.price.)

# Crear un histograma con límites en el eje x
hist(test$price, 
     main = "Histograma de Precios",
     xlab = "Precio",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "black",
     breaks = 50,
     xlim = c(200000000, 1000000000)  # Establece los límites del eje x
)

parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = TRUE)

# Creamos el mapa de Bogota
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red", weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada parque
# Priemro tomamos nuestros datos geoespaciales y los convertimos al formato sf (simple features)
# Esto para que estén en el mismo formato de los parques y poder sacar distancia. 
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(test_sf) <- 4326
# Convertimos los centroides a formato sf (simple features)
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
# Esto va a ser demorado!
# Calculamos las distancias para cada combinación inmueble - parque
dist_matrix <- st_distance(x = test_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variable a nuestra base de datos original 
test <- test %>% mutate(distancia_parque = dist_min)


## _________________DISTANCIA A BAR TRAIN__________________________

# Extraemos la info de todos los parques de Cali
bar <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bar") 
# Cambiamos el formato para que sea un objeto sf (simple features)
bar_sf <- osmdata_sf(bar)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
bar_geometria <- bar_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada bar para aproximar s ubciacion como un solo punto 
centroides_bar <- gCentroid(as(bar_geometria$geometry, "Spatial"), byid = T)

# Creamos el mapa de Bogota
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = bar_geometria, col = "brown",weight = 10,
              opacity = 0.8, popup = bar_geometria$name) %>%
  addCircles(lng = centroides_bar$x, 
             lat = centroides_bar$y, 
             col = "yellow", opacity = 0.5, radius = 1)

# convertimos los scontroides a formato sf(simple features)
centroides_bar_sf <- st_as_sf(centroides_bar, coords = c("x", "y"))

# Calculamos las diatnacias para cada combinacion immueble - bar
dist_matrix2 <- st_distance(x = train_sf, y = centroides_bar_sf)

## TRAIN_SF es el que ya construmos que tiene en formato sf los inmuebles

# Encontramos la distancia mínima a un parque
dist_min2 <- apply(dist_matrix2, 1, min)
# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_bar = dist_min2)

##--------------------------- Lo mismo pero ahora para test______________
# Extraemos la info de todos los bares en Bogotá
bar <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bar") 
# Cambiamos el formato para que sea un objeto sf (simple features)
bar_sf <- osmdata_sf(bar)

# De las features de los bares nos interesa su geometría y donde están ubicados 
bar_geometria <- bar_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada bar para aproximar su ubicación como un solo punto 
centroides_bar <- gCentroid(as(bar_geometria$geometry, "Spatial"), byid = TRUE)

# Creamos el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central_test, lat = latitud_central_test, zoom = 12) %>%
  addPolygons(data = bar_geometria, col = "brown", weight = 10,
              opacity = 0.8, popup = bar_geometria$name) %>%
  addCircles(lng = centroides_bar$x, 
             lat = centroides_bar$y, 
             col = "yellow", opacity = 0.5, radius = 1)

# Convertimos los centroides a formato sf (simple features)
centroides_bar_sf <- st_as_sf(centroides_bar, coords = c("x", "y"))

# Calculamos las distancias para cada combinación inmueble - bar
dist_matrix2 <- st_distance(x = test_sf, y = centroides_bar_sf)

## TEST_SF es el que ya construimos que tiene en formato sf los inmuebles

# Encontramos la distancia mínima a un bar
dist_min2 <- apply(dist_matrix2, 1, min)
# La agregamos como variable a nuestra base de datos original 
test <- test %>% mutate(distancia_bar = dist_min2)


## _________________DISTANCIA A ESTACION POLICIA TRAIN__________________________

# Extraemos la info de todos las estaciones policia de Bogota
policia <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police") 
# Cambiamos el formato para que sea un objeto sf (simple features)
policia_sf <- osmdata_sf(policia)

# De las features de las estaciones de policia nos interesa su geomoetría y donde estan ubicados 
policia_geometria <- policia_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada estacion de policia para aproximar s ubciacion como un solo punto 
centroides_policia <- gCentroid(as(policia_geometria$geometry, "Spatial"), byid = T)

# Creamos el mapa de Bogota
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = policia_geometria, col = "green",weight = 10,
              opacity = 0.8, popup = policia_geometria$name) %>%
  addCircles(lng = centroides_policia$x, 
             lat = centroides_policia$y, 
             col = "purple", opacity = 0.5, radius = 1)

# convertimos los scontroides a formato sf(simple features)
centroides_policia_sf <- st_as_sf(centroides_policia, coords = c("x", "y"))

# Calculamos las diatnacias para cada combinacion immueble - parque
dist_matrix3 <- st_distance(x = train_sf, y = centroides_policia_sf)

## TRAIN_SF es el que ya construmos que tiene en formato sf los inmuebles

# Encontramos la distancia mínima a un parque
dist_min3 <- apply(dist_matrix3, 1, min)

# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_policia = dist_min3)

##__________ AHORA LO MISMO PERO PARA TEST _____-

latitud_central_test <- mean(test$lat)
longitud_central_test <- mean(test$lon)

# Extraemos la info de todas las estaciones de policía en Bogotá
policia <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity", value = "police") 
# Cambiamos el formato para que sea un objeto sf (simple features)
policia_sf <- osmdata_sf(policia)

# De las features de las estaciones de policía nos interesa su geometría y donde están ubicadas 
policia_geometria <- policia_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada estación de policía para aproximar su ubicación como un solo punto 
centroides_policia <- gCentroid(as(policia_geometria$geometry, "Spatial"), byid = TRUE)

# Creamos el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central_test, lat = latitud_central_test, zoom = 12) %>%
  addPolygons(data = policia_geometria, col = "green", weight = 10,
              opacity = 0.8, popup = policia_geometria$name) %>%
  addCircles(lng = centroides_policia$x, 
             lat = centroides_policia$y, 
             col = "purple", opacity = 0.5, radius = 1)

# Convertimos los centroides a formato sf (simple features)
centroides_policia_sf <- st_as_sf(centroides_policia, coords = c("x", "y"))

# Calculamos las distancias para cada combinación inmueble - estación de policía
dist_matrix3 <- st_distance(x = test_sf, y = centroides_policia_sf)

## TEST_SF es el que ya construimos que tiene en formato sf los inmuebles

# Encontramos la distancia mínima a una estación de policía
dist_min3 <- apply(dist_matrix3, 1, min)

# La agregamos como variable a nuestra base de datos original 
test <- test %>% mutate(distancia_policia = dist_min3)


##___________ AHORA DISTANCIA ESTACION BUS______-


# Extraemos la info de todos las estaciones policia de Bogota
bus <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bus_station") 
# Cambiamos el formato para que sea un objeto sf (simple features)
bus_sf <- osmdata_sf(bus)

# De las features de las estaciones de policia nos interesa su geomoetría y donde estan ubicados 
bus_geometria <- bus_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada estacion de policia para aproximar s ubciacion como un solo punto 
centroides_bus <- gCentroid(as(bus_geometria$geometry, "Spatial"), byid = T)

# Creamos el mapa de Bogota
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = bus_geometria, col = "green",weight = 10,
              opacity = 0.8, popup = policia_geometria$name) %>%
  addCircles(lng = centroides_bus$x, 
             lat = centroides_bus$y, 
             col = "purple", opacity = 0.5, radius = 1)

# convertimos los scontroides a formato sf(simple features)
centroides_bus_sf <- st_as_sf(centroides_bus, coords = c("x", "y"))

# Calculamos las diatnacias para cada combinacion immueble - parque
dist_matrix4 <- st_distance(x = train_sf, y = centroides_bus_sf)

## TRAIN_SF es el que ya construmos que tiene en formato sf los inmuebles

# Encontramos la distancia mínima a un parque
dist_min4 <- apply(dist_matrix4, 1, min)

# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_bus = dist_min4)


##_______________ LO MISMO PERO PARA TEST (ESTACION BUS)

# Extraemos la info de todos las estaciones policia de Bogota
bus <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bus_station") 
# Cambiamos el formato para que sea un objeto sf (simple features)
bus_sf <- osmdata_sf(bus)

# De las features de las estaciones de policia nos interesa su geomoetría y donde estan ubicados 
bus_geometria <- bus_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada estacion de policia para aproximar s ubciacion como un solo punto 
centroides_bus <- gCentroid(as(bus_geometria$geometry, "Spatial"), byid = T)

# Creamos el mapa de Bogota
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central_test, lat = latitud_central_test, zoom = 12) %>%
  addPolygons(data = bus_geometria, col = "green",weight = 10,
              opacity = 0.8, popup = policia_geometria$name) %>%
  addCircles(lng = centroides_bus$x, 
             lat = centroides_bus$y, 
             col = "purple", opacity = 0.5, radius = 1)

# convertimos los scontroides a formato sf(simple features)
centroides_bus_sf <- st_as_sf(centroides_bus, coords = c("x", "y"))

# Calculamos las diatnacias para cada combinacion immueble - parque
dist_matrix4 <- st_distance(x = test_sf, y = centroides_bus_sf)

## TRAIN_SF es el que ya construmos que tiene en formato sf los inmuebles

# Encontramos la distancia mínima a un parque
dist_min4 <- apply(dist_matrix4, 1, min)

# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_bus = dist_min4)



##_________________ VER GRAFICOS DE CORRELACION LOGARITMICOS Y NORMALES
## DE LAS VARIABLES DE DISTANCIA QUE ACABAMOS DE CREAR________

#Vamos a tomar una muestra dada la cantidad de los datos 
## se ven mejores los normales que los logarirtmicos

p1 <- ggplot(train %>% sample_n(1000), aes(x = distancia_bar, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un bar en metros (metros-scale)", 
       y = "Valor de inmueble",
       title = "Relación entre la proximidad a un bar y el precio del immueble") 
ggplotly(p1)


p1 <- ggplot(train %>% sample_n(1000), aes(x = distancia_bar, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un bar en metros (log-scale)", 
       y = "Valor de inmueble(log)",
       title = "Relación entre la proximidad a un bar y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p1)

p2 <- ggplot(train %>% sample_n(1000), aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor de inmueble(log)",
       title = "Relación entre la proximidad a un parque y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p2)

p2 <- ggplot(train %>% sample_n(1000), aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (metros-scale)", 
       y = "Valor de inmueble",
       title = "Relación entre la proximidad a un parque y el precio del immueble") 
ggplotly(p2)


## AQUI O SE PUEDE REPRESENTAR EN FORMA LOGARITMICIA O EN FORMA NORMAL LOS EJES

p3 <- ggplot(train %>% sample_n(1000), aes(x = distancia_policia, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un CAI en metros (log-scale)", 
       y = "Valor de inmueble(log)",
       title = "Relación entre la proximidad a un CAI y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p3)

p3 <- ggplot(train %>% sample_n(1000), aes(x = distancia_policia, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un CAI en metros (metros-scale)", 
       y = "Valor de inmueble",
       title = "Relación entre la proximidad a un CAI y el precio del immueble")
ggplotly(p3)


p4 <- ggplot(train %>% sample_n(1000), aes(x = distancia_policia, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estacion bus en metros (metros-scale)", 
       y = "Valor de inmueble",
       title = "Relación entre la proximidad a una estacion bus y el precio del immueble")
ggplotly(p4)


##_______________________________AHORA CON TEXTO_____________________


##________ TRABAJAMOS CON TEXTO___________ 
# Primero normalizaremos todo el texto
# Todo en minuscula
train <- train %>%
  mutate(description2 = str_to_lower(description))
# Eliminamos tildes
train <- train %>%
  mutate(description2 = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
train <- train %>%
  mutate(descriptio2 = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
train <- train %>%
  mutate(description2 = str_trim(gsub("\\s+", " ", description)))


train <- train %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero|garaje", train$description2)))


train %>%
  count(parqueadero)

train <- train %>%
  mutate(terraza = as.numeric(grepl("jardin|terraza|azotea", train$description2)))

train %>%
  count(terraza)

train <- train %>%
  mutate(piscina = as.numeric(grepl("piscina|jacuzzi|sauna|turco", train$description2)))

train %>%
  count(piscina)


## lo mismo pero para test

##________ TRABAJAMOS CON TEXTO___________ 
# Primero normalizaremos todo el texto
# Todo en minúscula
test <- test %>%
  mutate(description2 = str_to_lower(description))
# Eliminamos tildes
test <- test %>%
  mutate(description2 = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
test <- test %>%
  mutate(descriptio2 = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
test <- test %>%
  mutate(description2 = str_trim(gsub("\\s+", " ", description)))

test <- test %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero|garaje", test$description2)))

test %>%
  count(parqueadero)

test <- test %>%
  mutate(terraza = as.numeric(grepl("jardin|terraza|azotea", test$description2)))

test %>%
  count(terraza)

test <- test %>%
  mutate(piscina = as.numeric(grepl("piscina|jacuzzi|sauna|turco", test$description2)))

test %>%
  count(piscina)



##________ TRABAJAMOS CON TEXTO___________ 
# Primero normalizaremos todo el texto
# Todo en minuscula
train <- train %>%
  mutate(title2 = str_to_lower(title))
# Eliminamos tildes
train <- train %>%
  mutate(title2 = iconv(title, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
train <- train %>%
  mutate(title2 = str_replace_all(title, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
train <- train %>%
  mutate(title2 = str_trim(gsub("\\s+", " ", title)))


train <- train %>%
  mutate(casa = as.numeric(grepl("casa", train$title2)))


train %>%
  count(casa)

##________ TRABAJAMOS CON TEXTO AHORA TEST___________ 
# Primero normalizaremos todo el texto
# Todo en minúscula
test <- test %>%
  mutate(title2 = str_to_lower(title))
# Eliminamos tildes
test <- test %>%
  mutate(title2 = iconv(title, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
test <- test %>%
  mutate(title2 = str_replace_all(title, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
test <- test %>%
  mutate(title2 = str_trim(gsub("\\s+", " ", title)))

test <- test %>%
  mutate(casa = as.numeric(grepl("casa", test$title2)))

test %>%
  count(casa)

## PREDICCIONES______________




param_grid_boost_expand <- expand.grid(
  trees = c(400, 450, 500, 550, 600),
  min_n = c(2, 4, 6),
  learn_rate = c(0.001, 0.005, 0.01)
)

# Specification of the boost_tree model in tidymodels
boost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  # Change to regression mode

# Assuming you are performing linear regression
model_spec <- linear_reg()

rec_geo <- recipe( price ~ bedrooms  + parqueadero + terraza + distancia_bar + distancia_bus + distancia_parque,data= train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

model_spec <- model_spec %>%
  set_mode("regression")  # Set the mode to regression   

workflow_boost <- workflow() %>%
  add_recipe(rec_geo) %>%
  add_model(boost_spec)

tune_boost_expand <- tune_grid (
  workflow_boost,
  resamples = df_fold, 
  grid = param_grid_boost_expand,
  metrics = metric_set(mae)
)


best_parms_boost_expand <- select_best(tune_boost_expand, metric = "mae")
best_parms_boost_expand

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
boost_final_expand <- finalize_workflow(workflow_boost, best_parms_boost_expand)

# Ajustar el modelo  utilizando los datos de entrenamiento
boost_final_fit_expand <- fit(boost_final_expand, data = train)

prediccion_boost_expand <- predict(boost_final_fit_expand, new_data=test)
prediccion_boost_expand 
write.csv(test %>% select(property_id) %>% bind_cols(prediccion_boost_expand),file = 'prediccion_boost_expand.csv')

##_____________________ OTROOOOO________