library(here)
library(png)

# Funcion de extraccion de caracteristicas 
# Basados en la extraccion de prueba

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

# data set largo para exploracion inicial
ruta_images <- here("data", "raw", "Caso_2", "imagenes_espuma")
df_labels <- read.csv(here("data", "raw", "Caso_2", "labels_imagenes.csv"))

vec_largo_nombres <- c()
vec_largo_respuestas <- c()
vec_largo_brillo <- c()
vec_largo_contraste <- c()
vec_largo_area <- c()
vec_largo_rugosidad <- c()
vec_largo_color <- c()

for (i in 1:nrow(df_labels)) {
  id_actual <- df_labels$imagen_id[i]
  respuesta_actual <- df_labels$respuesta[i]
  
  for (cara in 0:2) {
    ruta_completa <- file.path(ruta_images, paste0(id_actual, "-", cara, ".png"))
    metricas <- extraer_caracteristicas(ruta_completa)
    
    vec_largo_nombres <- c(vec_largo_nombres, paste0(id_actual, "-", cara))
    vec_largo_respuestas <- c(vec_largo_respuestas, respuesta_actual)
    vec_largo_brillo <- c(vec_largo_brillo, metricas["brillo_medio"])
    vec_largo_contraste <- c(vec_largo_contraste, metricas["contraste_global"])
    vec_largo_area <- c(vec_largo_area, metricas["area_objeto"])
    vec_largo_rugosidad <- c(vec_largo_rugosidad, metricas["rugosidad_superficial"])
    vec_largo_color <- c(vec_largo_color, metricas["variacion_color_verde"])
    
  }
}

df_largo_auxiliar <- data.frame(imagen_id_foto = vec_largo_nombres, 
                                respuesta_global = vec_largo_respuestas,
                                brillo_medio = vec_largo_brillo,
                                contraste_global = vec_largo_contraste,
                                area_objeto= vec_largo_area, 
                                rugosidad_superficial = vec_largo_rugosidad,
                                variacion_color_verde = vec_largo_color)

write.csv(df_largo_auxiliar, here("data", "processed", "dataset_exploratorio.csv"), row.names = FALSE)
