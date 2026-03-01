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

# Como no queda clara la escala (Ya que pusiste ejemplo de 0 a 10 de forma arbitraria)
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

# -------------------------------------------------------------------------
# >>>> Columna ingresos_familiares<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(ingresos_familiares, na.rm = TRUE), max = max(ingresos_familiares, na.rm = TRUE))

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
