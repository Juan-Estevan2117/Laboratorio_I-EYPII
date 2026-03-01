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
# Limpieza de datos por columna
# -------------------------------------------------------------------------

# Se verificara la unicidad de cada registro para variables cualitativas, los
# rangos logicos para las cuantitativas, posteriormente, normalizaremos valores
# dejando strings en minusculas, cambio de idioma y de haber categorias mal 
# escritas (ejemplo: en lugar de Data, Datta o por el estilo) cambiarlas a los
# valores correspondientes

# -------------------------------------------------------------------------
# >>>> Columna anio: <<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$anio) # Sin cambios

# -------------------------------------------------------------------------
# >>>> Columna semestre <<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$semestre)

# cambio a lowecase
df_rendimiento$semestre <- tolower(df_rendimiento$semestre)

# -------------------------------------------------------------------------
# >>>> Columna puntaje_final <<<<
# -------------------------------------------------------------------------

# Los puntajes, aunque tengan outliers, teniendo en cuenta que se tratan de puntajes
# como una prueba icfes, estan dentro de lo que se considera fisicamente
# posible (suponiendo que la escala va de 0 a 250)

ggplot(df_rendimiento, aes(y = puntaje_final)) + geom_boxplot(fill = "pink", color = "red", width = 0.05) + 
  theme(axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(y = "Puntaje Final") + scale_y_continuous(limits = c(0, 250))

# -------------------------------------------------------------------------
# >>>> Columna horas_estudio<<<<<
# -------------------------------------------------------------------------

# Revisamos los maximos y minimos de la columna
df_rendimiento %>% summarise(min = min(horas_estudio, na.rm = TRUE), max = max(horas_estudio, na.rm = TRUE))

# Como son horas de estudio semanales el rango fisicamente posible es [0, 168]
# Marcamos como NAN los valores fuera del rango

df_rendimiento$horas_estudio[df_rendimiento$horas_estudio < 0 | df_rendimiento$horas_estudio > 128] <- NA
# -------------------------------------------------------------------------
# >>>> Columna asistencia<<<<<
# -------------------------------------------------------------------------

# Revisamos los maximos y minimos de la columna

df_rendimiento %>% summarise(min = min(asistencia, na.rm = TRUE), max = max(asistencia, na.rm = TRUE))

# Como es un porcentaje, el rango estricto es [0, 100]
# Marcamos como NAN los valores fuera de rango

df_rendimiento$asistencia[df_rendimiento$asistencia < 0 | df_rendimiento$asistencia > 100] <- NA

# -------------------------------------------------------------------------
# >>>> Columna promedio_previo<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(promedio_previo, na.rm = TRUE), max = max(promedio_previo, na.rm = TRUE))

# Rango segun diccionario de variables [0, 10]

df_rendimiento$promedio_previo[df_rendimiento$promedio_previo < 0 | df_rendimiento$promedio_previo > 10] <- NA

# -------------------------------------------------------------------------
# >>>> Columna horas_sueno<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(horas_sueno, na.rm = TRUE), max = max(horas_sueno, na.rm = TRUE))

# aunque es seria tremendo outlier, debemos considerar un rango de [0, 24]

df_rendimiento$horas_sueno[df_rendimiento$horas_sueno < 0 | df_rendimiento$horas_sueno > 24] <- NA

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

# El rango puede plantearse en una escala de [0, 10]

df_rendimiento$estres[df_rendimiento$estres < 0 | df_rendimiento$estres > 10] <- NA

# -------------------------------------------------------------------------
# >>>> Columna uso_redes<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(uso_redes, na.rm = TRUE), max = max(uso_redes, na.rm = TRUE))

# Rango fisicamente posible [0, 24] - horas por dia

df_rendimiento$uso_redes[df_rendimiento$uso_redes < 0 | df_rendimiento$uso_redes > 24] <- NA

# -------------------------------------------------------------------------
# >>>> Columna ingresos_familiares<<<<<
# -------------------------------------------------------------------------

df_rendimiento %>% summarise(min = min(ingresos_familiares, na.rm = TRUE), max = max(ingresos_familiares, na.rm = TRUE))

# No es posible tener ingresos negativos por tanto [>0] USD/mes

df_rendimiento$ingresos_familiares[df_rendimiento$ingresos_familiares < 0] <- NA

# -------------------------------------------------------------------------
# >>>> Columna genero<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$genero) # Teniamos Male, Female, FEMALE y male

# normalizamos
df_rendimiento$genero <- tolower(df_rendimiento$genero)

# cambiamos el idioma

df_rendimiento$genero[df_rendimiento$genero == "female"] <- "mujer"
df_rendimiento$genero[df_rendimiento$genero == "male"] <- "hombre"

# -------------------------------------------------------------------------
# >>>> Columna carrera<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$carrera) # Teniamos Business y Busines

# normalizamos
df_rendimiento$carrera <- tolower(df_rendimiento$carrera)

# Corregimos el error de tipado
df_rendimiento$carrera[df_rendimiento$carrera == "busines"] <- "business"

# Cambiamos el idioma
df_rendimiento$carrera[df_rendimiento$carrera == "business"] <- "negocios" 
df_rendimiento$carrera[df_rendimiento$carrera == "cs"] <- "ciencias de la computacion"
df_rendimiento$carrera[df_rendimiento$carrera == "data"] <- "datos"

# -------------------------------------------------------------------------
# >>>> Columna acceso_internet<<<<<
# -------------------------------------------------------------------------
unique(df_rendimiento$acceso_internet) # tenemos NO, Yes, No, yes

#normalizamos
df_rendimiento$acceso_internet <- tolower(df_rendimiento$acceso_internet)

# Dicotomizamos para futuro analisis [0 = no, 1 = si]
df_rendimiento$acceso_internet[df_rendimiento$acceso_internet == "yes"] <- 1
df_rendimiento$acceso_internet[df_rendimiento$acceso_internet == "no"] <- 0
# -------------------------------------------------------------------------
# >>>> Columna trabaja<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$trabaja) # Teniamos No, Si, sí, NO

# normalizamos
df_rendimiento$trabaja <- tolower(df_rendimiento$trabaja)
df_rendimiento$trabaja[df_rendimiento$trabaja == "sí"] <- "si"

# Dicotomizamos para posterior analisis
df_rendimiento$trabaja[df_rendimiento$trabaja == "si"] <- 1
df_rendimiento$trabaja[df_rendimiento$trabaja == "no"] <- 0


# -------------------------------------------------------------------------
# >>>> Columna modalidad<<<<<
# -------------------------------------------------------------------------

unique(df_rendimiento$modalidad)

# Normalizamos
df_rendimiento$modalidad <- tolower(df_rendimiento$modalidad)

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
