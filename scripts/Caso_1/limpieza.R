
df_rendimiento <- read.csv(here("data", "processed", "rendimiento_2021-2025_sem1&sem2.csv"))

# -------------------------------------------------------------------------
# Limpieza de datos por columna
# -------------------------------------------------------------------------

# Se verificara la unicidad de cada registro para variables cualitativas, los
# rangos logicos para las cuantitativas, posteriormente, normalizaremos valores
# dejando strings en minusculas, cambio de idioma y de haber categorias mal 
# escritas (ejemplo: en lugar de Data, Datta o por el estilo) cambiarlas a los
# valores correspondientes

# Semestre
df_rendimiento$semestre <- tolower(df_rendimiento$semestre)

# Como son horas de estudio semanales el rango fisicamente posible es [0, 168]
# Marcamos como NAN los valores fuera del rango

# Horas de estudio
df_rendimiento$horas_estudio[df_rendimiento$horas_estudio < 0 | df_rendimiento$horas_estudio > 128] <- NA

# asistencia

# Como es un porcentaje, el rango estricto es [0, 100]
# Marcamos como NAN los valores fuera de rango

df_rendimiento$asistencia[df_rendimiento$asistencia < 0 | df_rendimiento$asistencia > 100] <- NA

# Promedio previo
# Rango segun diccionario de variables [0, 10]

df_rendimiento$promedio_previo[df_rendimiento$promedio_previo < 0 | df_rendimiento$promedio_previo > 15] <- NA

# Horas de sueño
# aunque es seria tremendo outlier, debemos considerar un rango de [0, 24]

df_rendimiento$horas_sueno[df_rendimiento$horas_sueno < 0 | df_rendimiento$horas_sueno > 24] <- NA

# Estres
# El rango puede plantearse en una escala de [0, 10]

df_rendimiento$estres[df_rendimiento$estres < 0 | df_rendimiento$estres > 10] <- NA

# Uso de redes
# Rango fisicamente posible [0, 24] - horas por dia

df_rendimiento$uso_redes[df_rendimiento$uso_redes < 0 | df_rendimiento$uso_redes > 24] <- NA

# Ingresos familiares
# No es posible tener ingresos negativos por tanto [>0] USD/mes

df_rendimiento$ingresos_familiares[df_rendimiento$ingresos_familiares < 0] <- NA

# GENERO
# normalizamos
df_rendimiento$genero <- tolower(df_rendimiento$genero)

# Dicotomizamos para posterior analisis

df_rendimiento$genero[df_rendimiento$genero == "female"] <- 1
df_rendimiento$genero[df_rendimiento$genero == "male"] <- 0

# Carrera
# normalizamos
df_rendimiento$carrera <- tolower(df_rendimiento$carrera)

# Corregimos el error de tipado
df_rendimiento$carrera[df_rendimiento$carrera == "busines"] <- "business"

# Cambiamos el idioma
df_rendimiento$carrera[df_rendimiento$carrera == "business"] <- "negocios" 
df_rendimiento$carrera[df_rendimiento$carrera == "cs"] <- "ciencias de la computacion"
df_rendimiento$carrera[df_rendimiento$carrera == "data"] <- "datos"

# ACCESO A INTERNET
#normalizamos
df_rendimiento$acceso_internet <- tolower(df_rendimiento$acceso_internet)

# Dicotomizamos para futuro analisis [0 = no, 1 = si]
df_rendimiento$acceso_internet[df_rendimiento$acceso_internet == "yes"] <- 1
df_rendimiento$acceso_internet[df_rendimiento$acceso_internet == "no"] <- 0

# Trabaja
# normalizamos
df_rendimiento$trabaja <- tolower(df_rendimiento$trabaja)
df_rendimiento$trabaja[df_rendimiento$trabaja == "sí"] <- "si"

# Dicotomizamos para posterior analisis
df_rendimiento$trabaja[df_rendimiento$trabaja == "si"] <- 1
df_rendimiento$trabaja[df_rendimiento$trabaja == "no"] <- 0

# Modalidad
# Normalizamos
df_rendimiento$modalidad <- tolower(df_rendimiento$modalidad)