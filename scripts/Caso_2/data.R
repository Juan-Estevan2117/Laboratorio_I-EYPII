library(here)

df_img_labels = read.csv(here("data" , "raw", "Caso_2", "labels_imagenes.csv"))
summary(df_img_labels) # Tenemos 1340 registros en el archivo csv que contiene 
                       # La variable de respuesta

# Esto retorna la cantidad de imagenes que tenemos
length(list.files(here("data" , "raw", "Caso_2", "imagenes_espuma")))

