library(mice)

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

# dicotomizar
df_rendimiento$semestre[df_rendimiento$semestre == "sem1"] <- 0
df_rendimiento$semestre[df_rendimiento$semestre == "sem2"] <- 1

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
# Basado en el EDA, concluimos que un rango logico 

df_rendimiento$horas_sueno[df_rendimiento$horas_sueno < 3 | df_rendimiento$horas_sueno > 9] <- NA

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

# Dicotomizamos
df_rendimiento$modalidad[df_rendimiento$modalidad == "presencial"] <- 1
df_rendimiento$modalidad[df_rendimiento$modalidad == "virtual"] <- 0

# -------------------------------------------------------------------------
# Imputacion por columnas
# -------------------------------------------------------------------------
col_nul <- colMeans(is.na(df_rendimiento)) * 100
col_nul

# Creando una copia del df

imp_df_rendimiento <- df_rendimiento

# Verificar porcentajes del df imputado
imp_col_nul <- colMeans(is.na(imp_df_rendimiento)) * 100
imp_col_nul

# -------------------------------------------------------------------------
# Ingresos familiares: Imputacion por mediana
# -------------------------------------------------------------------------

filter(df_rendimiento, is.na(ingresos_familiares))

imp_df_rendimiento$ingresos_familiares[is.na(imp_df_rendimiento$ingresos_familiares)] <- 
  median(imp_df_rendimiento$ingresos_familiares, na.rm = TRUE)

# -------------------------------------------------------------------------
# Horas de sueño: Imputacion por mediana
# -------------------------------------------------------------------------

filter(df_rendimiento, is.na(horas_sueno))

imp_df_rendimiento$horas_sueno[is.na(imp_df_rendimiento$horas_sueno)] <- 
  median(imp_df_rendimiento$horas_sueno, na.rm = TRUE)

# -------------------------------------------------------------------------
# Uso de redes: Imputacion por mediana
# -------------------------------------------------------------------------

filter(df_rendimiento, is.na(uso_redes))

imp_df_rendimiento$uso_redes[is.na(imp_df_rendimiento$uso_redes)] <- 
  median(imp_df_rendimiento$uso_redes, na.rm = TRUE)


# -------------------------------------------------------------------------
# Imputacion por MICE (Imputacion Multiple por ecuaciones concatenadas)
# -------------------------------------------------------------------------

# Definimos como factor TODAS las variables categoricas
# Para que no se traten como numericas o caracteres y se aplique regresion logistica

imp_df_rendimiento$anio <- as.factor(imp_df_rendimiento$anio)
imp_df_rendimiento$semestre <- as.factor(imp_df_rendimiento$semestre)
imp_df_rendimiento$genero <- as.factor(imp_df_rendimiento$genero)
imp_df_rendimiento$carrera <- as.factor(imp_df_rendimiento$carrera)
imp_df_rendimiento$acceso_internet <- as.factor(imp_df_rendimiento$acceso_internet)
imp_df_rendimiento$trabaja <- as.factor(imp_df_rendimiento$trabaja)
imp_df_rendimiento$modalidad <- as.factor(imp_df_rendimiento$modalidad)

# creamos el objeto donde se haran las imputaciones
imputacion <- mice(imp_df_rendimiento, m = 5, maxit = 10, method = NULL, seed = 13102005)

# Verificamos que metodos de imputacion se usaron para cada columnas
imputacion$method

# Extraemos el primer dataframe creado para poder hacer analisis de varianza
eda_df_rendimiento <- complete(imputacion, action = 1)

# Exportamos el dataframe para el EDA - ANOVA
write.csv(eda_df_rendimiento, file = here("data", "processed", "rendimiento_imputado.csv"), row.names = FALSE)

# Revision de los graficos para validar que los datos imputados si se hayan
# adaptado a los datos originales opservados

stripplot(imputacion, pch = 20, cex = 1.2)

densityplot(imputacion)
