library(here)
library(ggplot2)
library(corrplot)
library(dplyr)

df_espumas <- read.csv(here("data", "processed", "dataset_espumas.csv"))
str(df_espumas)

# dicotomizamos la variable de respuesta
df_espumas$respuesta[df_espumas$respuesta == "No conforme"] <- 0
df_espumas$respuesta[df_espumas$respuesta == "Conforme"] <- 1

str(df_espumas)
summary(df_espumas)

# verificacion del balanceo de clases: para este caso, no hay
ggplot(df_espumas, aes(x = respuesta, fill = respuesta)) + 
  geom_bar() + scale_x_discrete(labels = c("0" = "No conformes", "1" = "Conformes")) + 
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#2ecc71"), labels = c("No conformes", "Conformes")) +
  labs(x = "Categoria", y = "Frecuencia") + theme_minimal()

# armando la matriz de correlacion
num_df_espumas <- df_espumas%>%select(razon_area, peor_rugosidad, peor_contraste, peor_variacion_color, rango_brillo)

correlaciones_1 <- cor(num_df_espumas)

# se nota que entre peor_variacion_color y razon_area hay una correlacion
# perfecta. Esto es porque las imagenes estan en escalas de grises
# por tanto son virtualmente identicas. Podemos quitarla
corrplot(correlaciones_1, method="color", type="lower", addCoef.col="black", tl.cex=0.8, number.cex=0.6)

df_espumas$peor_variacion_color <- NULL

# Recalculamos la matriz
num_df_espumas_2 <- df_espumas%>%select(razon_area, peor_rugosidad, peor_contraste, rango_brillo)

correlaciones_2 <- cor(num_df_espumas_2)

corrplot(correlaciones_2, method="color", type="lower", addCoef.col="black", tl.cex=0.8, number.cex=0.6)

# boxplot

# razon areas
ggplot(df_espumas, aes(x = respuesta, y = razon_area, fill = respuesta)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  theme(legend.position = "none") + scale_x_discrete(labels = c("No conformes", "Conformes")) + 
  labs(x = "Categoria de Respuesta", y = "Asimetria del area (max/min)") + theme_minimal() + scale_fill_discrete(guide = "none")

# peor rugosidad
ggplot(df_espumas, aes(x = respuesta, y = peor_rugosidad, fill = respuesta)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") + scale_x_discrete(labels = c("No conformes", "Conformes")) +
  theme_minimal() + labs(x = "Categoria de Respuesta", y = "Peor Rugosidad") + scale_y_log10() + 
  theme(legend.position = "none")

# rango de brillo
ggplot(df_espumas, aes(x = respuesta, y = rango_brillo, fill = respuesta)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") + scale_x_discrete(labels = c("No conformes", "Conformes")) +
  theme_minimal() + labs(x = "Categoria de Respuesta", y = "Rango de brillo") +
  theme(legend.position = "none")

# peor contraste
ggplot(df_espumas, aes(x = respuesta, y = peor_contraste, fill = respuesta)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") + scale_x_discrete(labels = c("No conformes", "Conformes")) +
  theme_minimal() + labs(x = "Categoria de Respuesta", y = "Peor contraste") +
  theme(legend.position = "none")

# este scatterplot nos permite poner en las espumas conformes y no conformes
# en relacion con la peor rugosidad superficial y la asimetria de area
# de aqui sacamos que hay espumas No conformes que son en terminos de area simetricas
# pero se desplazan a la derecha por asimetria en la rugosidad, fallan por textura.
# Y hay espumas No conformes que aunque no terminan de ser lisas por completo, fallan 
# por rumputaras o variaciones fuertes en la razon de sus areas.
ggplot(df_espumas, aes(x = peor_rugosidad, y = razon_area, color = as.factor(respuesta))) + 
  geom_point(alpha = 0.7, size = 2) + scale_y_log10() +  theme_minimal() +
  labs(x = "Peor Rugosidad Superficial (Defecto de Textura)", y = "Asimetría de Área (Defecto Estructural) - Escala Log") +
  scale_color_manual(name = "Categoría", values = c("0" = "#e74c3c", "1" = "#2ecc71"), 
                     labels = c("0" = "No Conforme", "1" = "Conforme")) +
  theme(legend.position = "bottom")