library(here)
library(png)

# Funcion de extraccion de caracteristicas
extraer_caracteristicas <- function(ruta_imagen) {
  
  img <- readPNG(ruta_imagen)
  
  # Las imagenes se encuentran en escalas de grises
  R <- img
  G <- img
  B <- img
  
  int_gris <- 0.2989 * R + 0.5870 * G + 0.1140 * B
  
  seg_bin <- ifelse(int_gris < 0.95, 1, 0)
  
  # Seguridad por si la foto sale toda blanca
  if (sum(seg_bin) == 0) {
    return(c(brillo_medio = NA, contraste_global = NA, area_objeto = NA, 
             rugosidad_superficial = NA, variacion_color_verde = NA))
  }
  
  brillo_medio <- mean(int_gris)
  contraste_global <- sd(int_gris)
  area_objeto <- sum(seg_bin)
  
  pixeles_espuma_grises <- int_gris[seg_bin == 1]
  rugosidad_superficial <- sd(pixeles_espuma_grises)
  
  pixeles_espuma_verdes <- G[seg_bin == 1]
  variacion_color_verde <- sd(pixeles_espuma_verdes)
  
  # almacenamos en un vector
  caracteristicas <- c(brillo_medio = brillo_medio, 
                       contraste_global = contraste_global,
                       area_objeto = area_objeto, 
                       rugosidad_superficial = rugosidad_superficial,
                       variacion_color_verde = variacion_color_verde)
  
  return(caracteristicas)
}

# Cargamos el archivo de las etiquetas
ruta_labels <- here("data", "raw", "Caso_2", "labels_imagenes.csv")
df_img_labels <- read.csv(ruta_labels) # montamos un dataframe

# creamos vectores vacios para despues enlazarlos con los defectos de las metricas
vec_id <- c()
vec_respuesta <- c()

# Como el enfoque será centrarnos en los defectos de cada imagen
# definimos los vectores donde almacenaremos cada defecto en las metricas
vec_razon_area <- c()
vec_peor_rugosidad <- c()
vec_peor_contraste <- c()
vec_peor_color <- c()
vec_rango_brillo <- c()

# definimos la ruta de la carpeta
ruta_carpeta <- here("data", "raw", "Caso_2", "imagenes_espuma")

# Extraccion

# iteramos sobre la primera columna del df con las labels
for (i in 1:nrow(df_img_labels)) {
  
  # capturamos el id y la respuesta de la fila actual
  id_actual <- df_img_labels$imagen_id[i]
  respuesta_actual <- df_img_labels$respuesta[i]
  
  # sabiendo que tenemos una estructura de nombres similares para cada 
  # imagen definimos la ruta de dicha imagen concatenando la ruta de
  # la carpeta con el id de la columna actual (en la que estamos iterando)
  # y le pegamos el numero de imagen (de -0 a -2).png
  
  ruta_0 <- file.path(ruta_carpeta, paste0(id_actual, "-0.png"))
  ruta_1 <- file.path(ruta_carpeta, paste0(id_actual, "-1.png"))
  ruta_2 <- file.path(ruta_carpeta, paste0(id_actual, "-2.png"))
  
  # asumiendo que las caracteristicas estan completas
  cara_0 <- extraer_caracteristicas(ruta_0)
  cara_1 <- extraer_caracteristicas(ruta_1)
  cara_2 <- extraer_caracteristicas(ruta_2)
  
  # calculamos los defectos
  
  # razon de Area (Maximo dividido Minimo)
  
  # sacamos un vector con los defectos del area
  areas <- c(cara_0["area_objeto"], cara_1["area_objeto"], cara_2["area_objeto"])
  razon_area <- max(areas) / min(areas)
  
  # peor rugosidad (el valor maximo de las 3 caras)
  rugosidades <- c(cara_0["rugosidad_superficial"], cara_1["rugosidad_superficial"], cara_2["rugosidad_superficial"])
  peor_rug <- max(rugosidades)
  
  # peor contraste (el valor maximo)
  contrastes <- c(cara_0["contraste_global"], cara_1["contraste_global"], cara_2["contraste_global"])
  peor_cont <- max(contrastes)
  
  # peor variacion de color (el valor maximo)
  colores <- c(cara_0["variacion_color_verde"], cara_1["variacion_color_verde"], cara_2["variacion_color_verde"])
  peor_col <- max(colores)
  
  # rango de brillo (maximo menos minimo)
  brillos <- c(cara_0["brillo_medio"], cara_1["brillo_medio"], cara_2["brillo_medio"])
  rango_bri <- max(brillos) - min(brillos)
  
  # guardamos lo calculado en los vectores vacios que habiamos definido
  vec_id <- c(vec_id, id_actual)
  vec_respuesta <- c(vec_respuesta, respuesta_actual)
  
  vec_razon_area <- c(vec_razon_area, razon_area)
  vec_peor_rugosidad <- c(vec_peor_rugosidad, peor_rug)
  vec_peor_contraste <- c(vec_peor_contraste, peor_cont)
  vec_peor_color <- c(vec_peor_color, peor_col)
  vec_rango_brillo <- c(vec_rango_brillo, rango_bri)
  
  # esto imprime un mensaje cada 100 imagenes porque el proceso tarda un poco
  if (i %% 100 == 0) {
    print(paste("Procesadas", i, "de", nrow(df_img_labels)))
  }
}

# Creamos el dataframe final
# union de los vectores
df_final <- data.frame(imagen_id = vec_id, respuesta = vec_respuesta, 
                       razon_area = vec_razon_area, peor_rugosidad = vec_peor_rugosidad,
                       peor_contraste = vec_peor_contraste, peor_variacion_color = vec_peor_color,
                       rango_brillo = vec_rango_brillo
)

# Guardamos el resultado
write.csv(df_final, here("data", "processed", "dataset_espumas.csv"), row.names = FALSE)

