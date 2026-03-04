library(png)
library(here)
#cargamos el df con la variable de respuesta
df_img_labels = read.csv(here("data" , "raw", "Caso_2", "labels_imagenes.csv"))

summary(df_img_labels)
str(df_img_labels)

extraer_caracteristicas <- function(ruta_imagen) {
  img <- readPNG(ruta_imagen)
  
  R <- img
  G <- img
  B <- img
  
  int_gris <- 0.2989 * R + 0.5870 * G + 0.1140 * B
  
  seg_bin <- ifelse(test = int_gris < 0.95, 1, 0)
  
  if (sum(seg_bin) == 0) return(NULL)
  
  brillo_medio <- mean(int_gris)
  contraste_global <- sd(int_gris)
  area_objeto <- sum(seg_bin)
  pixeles_espuma_grises <- int_gris[seg_bin == 1]
  rugosidad_superficial <- sd(pixeles_espuma_grises)
  pixeles_espuma_verdes <- G[seg_bin == 1]
  variacion_color_verde <- sd(pixeles_espuma_verdes)
  
  # Devolvemos un Data Frame con 1 fila
  return(data.frame(brillo_medio = brillo_medio, contraste_global = contraste_global,
                    area_objeto = area_objeto, rugosidad_superficial = rugosidad_superficial,
                    variacion_color_verde = variacion_color_verde
  ))
}
# dicotomizamos el df
#df_img_labels$respuesta[df_img_labels$respuesta == "No conforme"] <- 0
# df_img_labels$respuesta[df_img_labels$respuesta == "Conforme"] <- 1

ruta_carpeta_imagenes <- here("data", "raw", "Caso_2", "imagenes_espuma")

# Extraccion de metricas
# Lista donde guardaremos el mejor resultado de cada ID

lista_resultados <- list()

# Usamos el id del df con las variables de respuesta para iterar
for (i in 1:nrow(df_img_labels)) {
  
  id_actual <- df_img_labels$imagen_id[i] # almacena el id
  respuesta_actual <- df_img_labels$respuesta[i] # almacena la respuesta
  
  # lista auxiliar para tener las 3 imagenes
  resultados_caras <- list()
  
  # las imagenes estan numeradas de 0 a 2 al final, construimos la ruta de la
  # imagen basados en el id en el que estamos iterando, un guion, la "cara", que
  # en este caso lo usaremos para identificar el numero de imagen que estamos
  # analizando; y finalizamos con la extension .png
  for (cara in 0:2) {
    nombre_archivo <- paste0(id_actual, "-", cara, ".png")
    ruta_completa <- file.path(ruta_carpeta_imagenes, nombre_archivo)
    
    # asegurarse de que la ruta este bien y que exista
    if (file.exists(ruta_completa)) {
      metricas <- extraer_caracteristicas(ruta_completa) # le pasamos la ruta de las imagenes
      
      if (!is.null(metricas)) {
        # guardamos la cara (el numero de la imagen), esto nos servira para
        # saber que imagen fue seleccionada de las 3
        metricas$cara_peor <- cara
        resultados_caras[[cara + 1]] <- metricas
      }
    }
  }
  
  # Si logramos extraer al menos una cara con éxito
  if (length(resultados_caras) > 0) {
    
    # juntamos los vectores de la lista en un dataframe
    df_caras <- do.call(rbind, resultados_caras)
    
    # extramos el indice de donde se encuentre la mayot rugosidad
    indice_peor_cara <- which.max(df_caras$rugosidad_superficial)
    
    # Se extrae solo esa fila
    mejor_resultado <- df_caras[indice_peor_cara, ]
    
    # le juntamos el id y la respuesta
    mejor_resultado$imagen_id <- id_actual
    mejor_resultado$respuesta <- respuesta_actual
    
    # Lo guardamos en la lista final
    lista_resultados[[i]] <- mejor_resultado
  }
}

df_espumas <- do.call(rbind, lista_resultados)

# Reordenamos las columnas para que quede bonito (ID y Respuesta de primeras)
columnas_ordenadas <- c("imagen_id", "respuesta", "cara_peor", "brillo_medio", "contraste_global",
                        "area_objeto", "rugosidad_superficial", "variacion_color_verde")
df_espumas <- df_espumas[, columnas_ordenadas]

# exportamos el dataset como .csv
write.csv(df_espumas, here("data", "processed", "dataset_espumas_procesado.csv"), row.names = FALSE)

dim(df_espumas)
head(df_espumas)