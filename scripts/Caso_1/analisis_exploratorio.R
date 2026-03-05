library(here) # Configuracion de rutas
library(naniar) # Validacion de nulos
library(ggplot2) 
library(dplyr)

df_rendimiento <- read.csv(here("data", "processed", "rendimiento_2021-2025_sem1&sem2.csv"))

# -------------------------------------------------------------------------
# EDA - Analisis exploratorio de datos
# -------------------------------------------------------------------------

summary(df_rendimiento) # -> Se nota que hay valores fuera de rangos fisicamente posibles
str(df_rendimiento) # -> Se requiere normalizacion de los datos. 

head(df_rendimiento)
tail(df_rendimiento)

# -------------------------------------------------------------------------
# >>>> Columna anio: <<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$anio) # Sin cambios

# -------------------------------------------------------------------------
# >>>> Columna semestre <<<<
# -------------------------------------------------------------------------

# Semestre
unique(df_rendimiento$semestre) # Solo cambio a lowercase


# -------------------------------------------------------------------------
# >>>> Columna puntaje_final <<<<
# -------------------------------------------------------------------------

# Los puntajes, aunque tengan outliers, teniendo en cuenta que se tratan de puntajes
# como una prueba icfes, estan dentro de lo que se considera fisicamente
# posible (suponiendo que la escala va de 0 a 250)

df_rendimiento %>% summarise(min = min(puntaje_final, na.rm = TRUE), max = max(puntaje_final, na.rm = TRUE))

ggplot(df_rendimiento, aes(y = puntaje_final)) + geom_boxplot(fill = "pink", color = "red", width = 0.05) + 
  theme(axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(y = "Puntaje Final") + scale_y_continuous(limits = c(0, 250))

# -------------------------------------------------------------------------
# >>>> Columna horas_estudio<<<<<
# -------------------------------------------------------------------------

# Revisamos los maximos y minimos de la columna
df_rendimiento %>% summarise(min = min(horas_estudio, na.rm = TRUE), max = max(horas_estudio, na.rm = TRUE))

# -------------------------------------------------------------------------
# >>>> Columna asistencia<<<<<
# -------------------------------------------------------------------------

# Revisamos los maximos y minimos de la columna

df_rendimiento %>% summarise(min = min(asistencia, na.rm = TRUE), max = max(asistencia, na.rm = TRUE))

# Revisamos que cantidad de registros tenemos fuera del rango
sum((df_rendimiento$asistencia < 0 | df_rendimiento$asistencia > 100) &
      !is.na(df_rendimiento$asistencia))

# Cantidad de registros nulos
sum(is.na(df_rendimiento$asistencia))

# -------------------------------------------------------------------------
# >>>> Columna promedio_previo<<<<<
# -------------------------------------------------------------------------

# Como no queda clara la escala
df_rendimiento %>% summarise(min = min(promedio_previo, na.rm = TRUE), max = max(promedio_previo, na.rm = TRUE))

# quisimos ver la distribucion de los datos con un histograma
ggplot(df_rendimiento, aes(x = promedio_previo)) + geom_histogram(bins = 60, fill = "coral", color = "black") + 
  theme_minimal() + labs(x = "Promedio Previo", y = "Frecuencia") + scale_x_continuous(limits = c(0, 15))

# Concluimos que todos los valores se pueden considerar dentro de lo "normal"
# Por tanto el rango de la variable "promedio_previo" [0, 15]

# Si definimos una escala de [0, 10] la cantidad de datos nos llevaria a tener
# 192 nulos adicionales a los nulos que ya teniamos

# Cantidad de nulos
sum(is.na(df_rendimiento$promedio_previo)) #74 nulos

sum((df_rendimiento$promedio_previo < 0 | df_rendimiento$promedio_previo > 10) &
      !is.na(df_rendimiento$promedio_previo)) #192 nulos

# -------------------------------------------------------------------------
# >>>> Columna horas_sueno<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(horas_sueno, na.rm = TRUE), max = max(horas_sueno, na.rm = TRUE))

# Viendo que contextualmente no es posible dormir EN PROMEDIO 22 horas.
# Basados en la regla del 1.5IQR para saber si n valores son outliers o errores
# dentro de los datos. 

# Calculo IQR
iqr_hs = IQR(df_rendimiento$horas_sueno, na.rm = TRUE)

# Calculo del Q3
q3_hs <- quantile(df_rendimiento$horas_sueno, 0.75, na.rm = TRUE)

# Regla del 1.5IQR
limite_superior_hs <- q3_hs + 1.5*iqr_hs
limite_superior_hs# 8.43285 -> Todo valor por encima es considerado un error de los datos

# Calculo del Q1
q1_hs <- quantile(df_rendimiento$horas_sueno, 0.25, na.rm = TRUE)

limite_inferior_hs <- q1_hs - 1.5*iqr_hs
limite_inferior_hs

# Con esta regla definimos que los valores de la columna "hora_sueno" estan
# en un rango logico de [3, 9]

# -------------------------------------------------------------------------
# Toma de decision con respecto a la IMPUTACION

# Media 
media_hs <- mean(df_rendimiento$horas_sueno, na.rm = TRUE)

# Mediana
mediana_hs <- median(df_rendimiento$horas_sueno, na.rm = TRUE)

ggplot(df_rendimiento, aes(x = horas_sueno)) + geom_histogram(bins = 60, fill = "coral", color = "black") + 
  theme_minimal() + labs(x = "Horas de sueño - h/dias", y = "Frecuencia") + 
  scale_x_continuous(breaks = seq(0, 20, by = 1)) + geom_vline(aes(xintercept = media_hs, color = "Media"), linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = mediana_hs, color = "Mediana"), linetype = "dotted", size = 1) + 
  scale_color_manual(name = "Estadístico", values = c("Media" = "blue", "Mediana" = "red"))

ggplot(df_rendimiento, aes(y = horas_sueno)) + geom_boxplot(fill = "pink", color = "red", width = 0.05) + 
  theme(axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(y = "Horas de sueño - h/dia")

# -------------------------------------------------------------------------
# >>>> Columna edad<<<<<
# -------------------------------------------------------------------------

# Aunque sea raro, es un rango teoricamente posible 
df_rendimiento %>% summarise(min = min(edad, na.rm = TRUE), max = max(edad, na.rm = TRUE))


# -------------------------------------------------------------------------
# >>>> Columna estres<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(estres, na.rm = TRUE), max = max(estres, na.rm = TRUE))

# Como no es claro el rango, podemos visualizar un histograma para deducirlo

ggplot(df_rendimiento, aes(x = estres)) + geom_histogram(bins = 60, fill = "coral", color = "black") + 
  theme_minimal() + labs(x = "Nivel de Estrés", y = "Frecuencia")

# -------------------------------------------------------------------------
# >>>> Columna uso_redes<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(uso_redes, na.rm = TRUE), max = max(uso_redes, na.rm = TRUE))

# Histograma

# Toma de decision con respecto a la IMPUTACION

# Media 
media_ur <- mean(df_rendimiento$uso_redes, na.rm = TRUE)

# Mediana
mediana_ur <- median(df_rendimiento$uso_redes, na.rm = TRUE)

ggplot(df_rendimiento, aes(x = uso_redes)) + geom_histogram(bins = 60, fill = "coral", color = "black") + 
  theme_minimal() + labs(x = "Uso de redes - h/dias", y = "Frecuencia") + 
  scale_x_continuous(breaks = seq(0,24 , by = 4)) + geom_vline(aes(xintercept = media_ur, color = "Media"), linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = mediana_ur, color = "Mediana"), linetype = "dotted", size = 1) + 
  scale_color_manual(name = "Estadístico", values = c("Media" = "blue", "Mediana" = "red"))


# -------------------------------------------------------------------------
# >>>> Columna ingresos_familiares<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(ingresos_familiares, na.rm = TRUE), max = max(ingresos_familiares, na.rm = TRUE))

# Toma de decision con respecto a la IMPUTACION

ggplot(df_rendimiento, aes(x = estres)) + geom_histogram(bins = 60, fill = "coral", color = "black") + 
  theme_minimal() + labs(x = "$ Ingresos familiares", y = "Frecuencia")

ggplot(df_rendimiento, aes(y = ingresos_familiares)) + geom_boxplot(fill = "pink", color = "red", width = 0.05) + 
  theme(axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(y = "$USD/mes Ingresos Familiares")

# Media 
mean(df_rendimiento$ingresos_familiares, na.rm = TRUE)

# Mediana
median(df_rendimiento$ingresos_familiares, na.rm = TRUE)

# -------------------------------------------------------------------------
# >>>> Columna genero<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$genero) # Teniamos Male, Female, FEMALE y male

# -------------------------------------------------------------------------
# >>>> Columna carrera<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$carrera) # Teniamos Business y Busines

# -------------------------------------------------------------------------
# >>>> Columna acceso_internet<<<<<
# -------------------------------------------------------------------------
unique(df_rendimiento$acceso_internet) # tenemos NO, Yes, No, yes

# -------------------------------------------------------------------------
# >>>> Columna trabaja<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$trabaja) # Teniamos No, Si, sí, NO

# -------------------------------------------------------------------------
# >>>> Columna modalidad<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$modalidad)

# -------------------------------------------------------------------------
# Revision de datos faltantes
# -------------------------------------------------------------------------

# Calculo de porcentaje de nulos, lo almacenamos en un vector "col_nul" para
# depues consultar cada columna individualmente

col_nul <- colMeans(is.na(df_rendimiento)) * 100
col_nul

# Esta grafica nos da un primer vistazo
vis_miss(df_rendimiento) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Variable", y = "Observación") + scale_x_discrete(position = "bottom")

# Y esta nos da una visualizacion más clara
gg_miss_var(df_rendimiento, show_pct = TRUE) + theme_bw() + labs(y = "% Porcentaje por variable")

# Test de correlaciones pre-limpieza
df_num <- df_rendimiento%>% select(puntaje_final, horas_estudio, asistencia, promedio_previo,
                                   horas_sueno, edad, estres, uso_redes, ingresos_familiares) 

cor.test(df_num$horas_estudio, df_num$puntaje_final) #XHoras estudio vs puntaje final
cor.test(df_num$asistencia, df_num$puntaje_final) #XAsistencia vs puntaje final
cor.test(df_num$promedio_previo, df_num$puntaje_final) #Promedio previo vs puntaje final
cor.test(df_num$horas_sueno, df_num$puntaje_final) #Horas de sueño vs puntaje final
cor.test(df_num$edad, df_num$puntaje_final) #Edad vs puntaje final
cor.test(df_num$estres, df_num$puntaje_final) #Estrés  vs puntaje final
cor.test(df_num$uso_redes, df_num$puntaje_final) #XUso de redes  vs puntaje final

# verificacion con diagramas de dispercion

qplot(horas_estudio, puntaje_final, data = df_rendimiento) #Relación observable 
qplot(asistencia, puntaje_final, data = df_rendimiento) # No
qplot(promedio_previo, puntaje_final, data = df_rendimiento) #Relación observable
qplot(horas_sueno, puntaje_final, data = df_rendimiento) #No
qplot(edad, puntaje_final, data = df_rendimiento) #No
qplot(estres, puntaje_final, data = df_rendimiento) #No
qplot(uso_redes, puntaje_final, data = df_rendimiento) #No 

#Nos quedamos con las variables más significativas: horas_estudio, promedio_previo 
#y hacemos prueba de correlación entre ellas 2

cor.test(df_num$horas_estudio, df_num$promedio_previo) #XHoras estudio vs promedio_previo

corrplot(cor(df_num, use="complete.obs"), method="color", type="lower", addCoef.col="black", 
         tl.cex=0.8, number.cex=0.6)