library(png)
library(here)

#cargar imagen de prueba
img <- readPNG(here("data", "raw", "Caso_2", "imagenes_espuma", "img_0001-0.png"))

# Con el view(image) solo se veian puros 1's entonces se dió una ojeada con el
# summary. No son todos 1. Los valores van de 0 - 1 donde 0 son los negros y los 1
# son los blancos. Los intermedios son distintos tonos d grises

summary(img)
class(img) # Estamos trabajando con matrices
dim(img) # matriz de 256 x 256

#las imagenes ya se encuentran en escalas de grises, en las esclalas de grises
# los canales RGB son identicos
R <- img
G <- img # 
B <- img

# >>>Intensidad en la escala de grises<<<
#aplicacion de la formula sugerida
int_gris <- 0.2989 * R + 0.5870 * G + 0.1140 * B

# >>>Segmentacion binaria<<<

seg_bin <- ifelse(test = int_gris < 0.95, 1, 0)

# extraccion de metricas

# Brillo medio: la formula sugerida equivale al promedio de los elementos del 
# objeto int_gris
brillo_medio <- mean(int_gris) 
brillo_medio

# Contraste global: en equivalencia, es la desviacion estandar de los elementos
# del objeto int_gris
contraste_global <- sd(int_gris)
contraste_global

# Area del objeto: es la suma de los elementos de la matriz de segmentacion
# binaria
area_objeto <- sum(seg_bin)
area_objeto

# Rugosidad superficial (desviación estándar de los píxeles grises SOLO del objeto)
# Filtramos la matriz int_gris usando la segmentacion binaria (seg_bin)

pixeles_espuma_grises <- int_gris[seg_bin == 1]
rugosidad_superficial <- sd(pixeles_espuma_grises)
rugosidad_superficial

pixeles_espuma_verdes <- G[seg_bin == 1]
variacion_color_verde <- sd(pixeles_espuma_verdes)
variacion_color_verde

# Una vez obtenidas las metricas para una sola imagen, podriamos hacer una idea
# del flujo que necesitamos para la organizacion de los datos.

# Adaptar una funcion que haga esto para todas las imagenes