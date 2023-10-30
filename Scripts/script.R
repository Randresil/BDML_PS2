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
       glmnet,
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample) # Muestreo espacial para modelos de aprendizaje automático)


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

sum(grepl(c("m2 | mt2 | mts2 | metros2 | metros cuadrados | mts"), data_tot$description))

data_tot <- data_tot %>% 
  mutate(metros = case_when(
    grepl("m2", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) m2"),
    grepl("mt2", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) mt2"),
    grepl("mts2", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) mts2"),
    grepl("metros2", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) metros2"),
    grepl("metros cuadrados", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) metros cuadrados"),
    grepl("mts", data_tot$description) == TRUE ~ str_extract(description, "(\\w+|\\d+) mts"),
    .default = NA ))

data_tot <- data_tot %>% 
  mutate(metros_num = as.integer(str_extract(metros, "\\d+")))
skim(data_tot$metros_num)

quantile(data_tot$metros_num, probs = c(0, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99, 1), na.rm = TRUE)
data_tot$metros_num[data_tot$metros_num > 300] <- NA
quantile(data_tot$metros_num, probs = c(0, 0.1, 0.15, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99, 1), na.rm = TRUE)
skim(data_tot$metros_num)
mediana_metros <- median(data_tot$metros_num, na.rm = TRUE)
data_tot$metros_num[is.na(data_tot$metros_num)] <- mediana_metros


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





# Guardar la base de datos para no repetir todo el proceso anterior
write.csv(data_tot, file = "Stores/Data_total.csv")
data_tot <- import("Stores/Data_total.csv")



## Trabajo de la base para valores en NA
skim(data_tot)
data_met <- data_tot %>% filter(is.na(metros_num))

for (i in c(1:30)) {
  print(data_met$description[i])
}





## MODELAJE ----------------------
train <- data_tot %>% filter(div == "train")
test <- data_tot %>% filter(div == "test")

str(train$distancia_park)

df_fold <- vfold_cv(train, v = 5)

ridge_recipe <- 
  recipe(formula = price ~ distancia_park + distancia_stadium + bank + bus_station + college + hospital +
          police + university + pub + veterinary + mall + nature_reserve + parqueadero + terraza + piscina +
           conjunto + apartaestudio + duplex + vista + penthouse + casa + habitaciones_numerico + bano_numerico, data = train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")
  
ridge_workflow <- workflow() %>%
  add_recipe(ridge_recipe) %>%
  add_model(ridge_spec)

penalty_grid <- grid_regular(penalty(range = c(0, 200)), levels = 5000)
penalty_grid

tune_res <- tune_grid(
  ridge_workflow,             # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = df_fold,        # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metric_set(mae))
tune_res

autoplot(tune_res)
collect_metrics(tune_res)

best_penalty <- select_best(tune_res, metric = "mae")
best_penalty

ridge_final <- finalize_workflow(ridge_workflow, best_penalty)
ridge_final_fit <- fit(ridge_final, data = train)

prediccion_ridge <- predict(ridge_final_fit, new_data = test)
prediccion_ridge

prediccion_ridge <- test %>% select(property_id) %>% bind_cols(prediccion_ridge) %>% rename(price = .pred, property_id = property_id)
write.csv(prediccion_ridge, file = 'Stores/prediccion_ridge.csv', row.names = FALSE)



lasso_recipe <- 
  recipe(formula = price ~ distancia_park + distancia_stadium + bank + bus_station + college + hospital +
           police + university + pub + veterinary + mall + nature_reserve + parqueadero + terraza + piscina +
           conjunto + apartaestudio + duplex + vista + penthouse + casa + habitaciones_numerico + bano_numerico, data = train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

lasso_workflow <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_spec)

penalty_grid <- grid_regular(penalty(range = c(-4, 200)), levels = 300)
penalty_grid

tune_res <- tune_grid(
  lasso_workflow,             # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = df_fold,        # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metric_set(mae))
tune_res

autoplot(tune_res)
collect_metrics(tune_res)

best_penalty <- select_best(tune_res, metric = "mae")
best_penalty

lasso_final <- finalize_workflow(lasso_workflow, best_penalty)
lasso_final_fit <- fit(lasso_final, data = train)

prediccion_lasso <- predict(lasso_final_fit, new_data = test)
prediccion_lasso

prediccion_lasso <- test %>% select(property_id) %>% bind_cols(prediccion_lasso) %>% rename(price = .pred, property_id = property_id)
write.csv(prediccion_lasso, file = 'Stores/prediccion_lasso.csv', row.names = FALSE)





elastic_recipe <- 
  recipe(formula = price ~ distancia_park + distancia_stadium + bank + bus_station + college + hospital +
           police + university + pub + veterinary + mall + nature_reserve + parqueadero + terraza + piscina +
           conjunto + apartaestudio + duplex + vista + penthouse + casa + habitaciones_numerico + bano_numerico +
           metros_num, data = train) %>% 
  step_normalize(all_predictors()) %>% 
  step_poly(metros_num, mall, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

elastic_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

elastic_workflow <- workflow() %>%
  add_recipe(elastic_recipe) %>%
  add_model(elastic_spec)

penalty_grid <- grid_regular(penalty(range = c(-4, 200)), levels = 300)
penalty_grid

tune_res <- tune_grid(
  elastic_workflow,             # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = df_fold,        # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metric_set(mae))
tune_res

autoplot(tune_res)
collect_metrics(tune_res)

best_penalty <- select_best(tune_res, metric = "mae")
best_penalty

elastic_final <- finalize_workflow(elastic_workflow, best_penalty)
elastic_final_fit <- fit(elastic_final, data = train)

prediccion_elastic <- predict(elastic_final_fit, new_data = test)
prediccion_elastic

prediccion_elastic <- test %>% select(property_id) %>% bind_cols(prediccion_elastic) %>% rename(price = .pred, property_id = property_id)
write.csv(prediccion_elastic, file = 'Stores/prediccion_elastic.csv', row.names = FALSE)



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


p_load("caret")

fitControl <- trainControl(method ="cv", number=5)
fitControl

tree_ranger_grid <- train(price ~ bedrooms  +bathrooms + terraza + conjunto + apartaestudio + piso_numerico +
                            bus_station + police + college + mall + bank + casa,
                          data=data,
                          method = "ranger",
                          trControl = fitControl,
                          tuneGrid=expand.grid(
                            mtry = c(1,2,3),
                            splitrule = "variance",
                            min.node.size = c(5,10,15))
)

tree_ranger_grid
p_load(rpart.plot)
prp(tree_ranger_grid$finalModel, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,tweak=1.2,clip.facs= TRUE,box.palette = "Blues")
cp_geo <- predict(tree_ranger_grid , data2)
cp_geo

write.csv(data2 %>% select(property_id) %>% bind_cols(cp_geo),file = 'rm_ranger_all.csv')

tree_ranger_grid


##________________


p_load("bst")

tree_boosted <- train(price ~bathrooms  +habitaciones_numerico + terraza + conjunto + apartaestudio + piso_numerico +
                        bus_station + police + college + mall + bank + casa + hospital,
                      data=data,
                      method = "bstTree",
                      trControl = fitControl,
                      tuneGrid=expand.grid(
                        mstop = c(300,400,500), #Boosting Iterations (M)
                        maxdepth = c(1,2,3), # Max Tree Depth (d)
                        nu = c(0.01,0.001)) # Shrinkage (lambda)
)

tree_boosted
boost <- predict(tree_boosted,data2)
boost
write.csv(data2 %>% select(property_id) %>% bind_cols(boost),file = 'tree_boosted.csv')



# RANDOM FOREST
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)

recipe <- recipe(formula = price ~ distancia_park + distancia_stadium + bank + bus_station + college + hospital +
                   police + university + pub + veterinary + mall + nature_reserve + parqueadero + terraza + piscina +
                   conjunto + apartaestudio + duplex + vista + penthouse + casa + habitaciones_numerico + bano_numerico +
                   metros_num, data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

# Tune grid aleatorio para el modelo de rf
rf_grid_random <- grid_random(  mtry(range = c(2, 10)),
                                min_n(range = c(2, 12)),
                                trees(range = c(100, 150)), size = 4)

## Modelo de rf
rf_spec<- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")       # Cambiar a modo de regresión

workflow_rf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)

tune_rf <- tune_grid(
  workflow_rf,
  resamples = block_folds, 
  grid = rf_grid_random,
  metrics = metric_set(mae))

best_parms_rf<- select_best(tune_rf, metric = "mae")
best_parms_rf

rf_final <- finalize_workflow(workflow_rf, best_parms_rf)

rf_final_fit <- fit(rf_final, data = train)

prediccion_rf <- predict(rf_final_fit, new_data = test)
prediccion_rf

prediccion_rf <- test %>% select(property_id) %>% bind_cols(prediccion_rf) %>% rename(price = .pred, property_id = property_id)
write.csv(prediccion_rf, file = 'Stores/prediccion_rf.csv', row.names = FALSE)



# Modelo Arbol Boosting
# Tune grid aleatorio para el modelo de boost
tune_grid_boost <- grid_random(
  trees(range = c(400, 600)),
  min_n(range = c(1, 12)),
  learn_rate(range = c(0.001, 0.01)), size = 4
)

# Especificación del modelo boost_tree en tidymodels
boost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")

workflow_boost <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(boost_spec)

tune_boost <- tune_grid(
  workflow_boost,
  resamples = block_folds, 
  grid = tune_grid_boost,
  metrics = metric_set(mae)
)

best_parms_boost <- select_best(tune_boost, metric = "mae")
best_parms_boost

boost_final <- finalize_workflow(workflow_boost, best_parms_boost)

boost_final_fit <- fit(boost_final, data = train)

prediccion_boost <- predict(boost_final_fit, new_data = test)
prediccion_boost

prediccion_boost <- test %>% select(property_id) %>% bind_cols(prediccion_boost) %>% rename(price = .pred, property_id = property_id)
write.csv(prediccion_boost, file = 'Stores/prediccion_boost.csv', row.names = FALSE)


# RANDOM FOREST 2
recipe <- recipe(formula = price ~ distancia_park + distancia_stadium + bank + bus_station + college + hospital +
                   police + university + pub + veterinary + mall + nature_reserve + parqueadero + terraza + piscina +
                   conjunto + apartaestudio + duplex + vista + penthouse + casa + habitaciones_numerico + bano_numerico +
                   metros_num, data = train) %>%
  # step_poly(metros_num = 2)
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

# Tune grid aleatorio para el modelo de rf
rf_grid_random <- grid_random(  mtry(range = c(6, 14)),
                                min_n(range = c(4, 14)),
                                trees(range = c(100, 120)), size = 4)

## Modelo de rf
rf_spec<- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")       # Cambiar a modo de regresión

workflow_rf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)

tune_rf <- tune_grid(
  workflow_rf,
  resamples = block_folds, 
  grid = rf_grid_random,
  metrics = metric_set(mae))

best_parms_rf<- select_best(tune_rf, metric = "mae")
best_parms_rf

rf_final <- finalize_workflow(workflow_rf, best_parms_rf)

rf_final_fit <- fit(rf_final, data = train)

prediccion_rf <- predict(rf_final_fit, new_data = test)
prediccion_rf

prediccion_rf <- test %>% select(property_id) %>% bind_cols(prediccion_rf) %>% rename(price = .pred, property_id = property_id)
write.csv(prediccion_rf, file = 'Stores/prediccion_rf2.csv', row.names = FALSE)





## ESTADISTICAS DESCRIPTIVAS y GRAFICOS -------------------
glimpse(data_tot)
data_tot2 <- data_tot %>% select(-c(V1, piso_info, bano_info, habitaciones_info, metros, bano_number))
names(data_tot2)

stargazer(data_tot2, type = "text")

stargazer(data_tot2, 
          title = "Estadísticas Descriptivas", 
          type = "text",
          digits = 1,
          covariate.labels = c("Price", "Month", "Year", "Surface Total", "Surface Covered",
                               "Rooms", "Bedrooms", "Bathrooms", "Latitude", "Longitude",
                               "Parqueadero", "Terraza", "Piscina", "Conjunto", "Apartaestudio",
                               "Número Piso", "Cantidad Baños", "Cantidad Cuartos", "Metros", 
                               "Duplex", "Vista", "Penthouse", "Casa", "Distancia Parque", 
                               "Distancia Estadio", "Distancia Banco", "Distancia Bus", "Distancia College",
                               "Distancia Hospital", "Distancia Policía", "Distancia Universidad",
                               "Distancia Pub", "Distancia Veterinaria", "Distancia Mall", "Distancia Reserva"),
          out = "Stores/Estad_desc.txt")

stargazer(data_tot2, 
          title = "Estadísticas Descriptivas", 
          type = "html",
          digits = 1,
          covariate.labels = c("Price", "Month", "Year", "Surface Total", "Surface Covered",
                               "Rooms", "Bedrooms", "Bathrooms", "Latitude", "Longitude",
                               "Parqueadero", "Terraza", "Piscina", "Conjunto", "Apartaestudio",
                               "Número Piso", "Cantidad Baños", "Cantidad Cuartos", "Metros", 
                               "Duplex", "Vista", "Penthouse", "Casa", "Distancia Parque", 
                               "Distancia Estadio", "Distancia Banco", "Distancia Bus", "Distancia College",
                               "Distancia Hospital", "Distancia Policía", "Distancia Universidad",
                               "Distancia Pub", "Distancia Veterinaria", "Distancia Mall", "Distancia Reserva"),
          out = "Stores/Estad_desc.htm")


data$log_price <- log(data$price)

# Crea un diagrama de caja y bigotes diferenciado por la variable dummy 'parqueadero'
boxplot(log_price ~ parqueadero, data = data, 
        xlab = "Parqueadero (1 = Sí, 0 = No)", 
        ylab = "Precio de Vivienda (LOG)",
        main = " Caja y Bigotes de Precio  por Parqueadero")

# Crea un diagrama de caja y bigotes diferenciado por la variable dummy 'parqueadero'
boxplot(log_price ~ terraza, data = data, 
        xlab = "terraza (1 = Sí, 0 = No)", 
        ylab = "Precio de Vivienda (Logaritmo)",
        main = " Caja y Bigotes de Precio  por terraza")

# Crea un diagrama de caja y bigotes diferenciado por la variable dummy 'parqueadero'
boxplot(log_price ~ casa, data = data, 
        xlab = "casa (1 = Sí, 0 = No)", 
        ylab = "Precio de Vivienda (Logaritmo)",
        main = " Caja y Bigotes de Precio  por casa")

# Crea un diagrama de caja y bigotes diferenciado por la variable dummy 'parqueadero'
boxplot(log_price ~ piscina, data = data, 
        xlab = "casa (1 = Sí, 0 = No)", 
        ylab = "Precio de Vivienda (Logaritmo)",
        main = " Caja y Bigotes de Precio  por pisicna")

install.packages("plotly")
library(plotly)
library(units)

p1 <- ggplot(data%>%sample_n(1000), aes(x = pub, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  geom_smooth() +
  labs(x = "Distancia mínima a un bar en metros (metros)", 
       y = "Valor de inmueble ",
       title = "Relación entre la proximidad a un bar y el precio") +
  theme_bw()
ggplotly(p1)

p2 <- ggplot(data%>%sample_n(1000), aes(x = distancia_park, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  geom_smooth() +
  labs(x = "Distancia mínima a un parque en metros (metros)", 
       y = "Valor de inmueble ",
       title = "Relación entre la proximidad a un parque y el precio") +
  theme_bw()
ggplotly(p2)

