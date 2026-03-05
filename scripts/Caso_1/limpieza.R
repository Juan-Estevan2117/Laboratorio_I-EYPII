library(here)
library(dplyr)
library(ggplot2)

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

df_rendimiento$asistencia <- NULL

# Promedio previo
# Rango segun diccionario de variables [0, 10]

df_rendimiento$promedio_previo[df_rendimiento$promedio_previo < 0 | df_rendimiento$promedio_previo > 15] <- NA

# Horas de sueño
df_rendimiento$horas_sueno <- NULL

# Estres
df_rendimiento$estres<- NULL

# Uso de redes
# Rango fisicamente posible [0, 24] - horas por dia

df_rendimiento$uso_redes <- NULL

# Ingresos familiares
df_rendimiento$ingresos_familiares <- NULL

# edad
df_rendimiento$edad <- NULL

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


# Revision de los graficos para validar que los datos imputados si se hayan
# adaptado a los datos originales opservados

# imputamos promedio previo con la mediana
imp_df_rendimiento$promedio_previo[is.na(imp_df_rendimiento$promedio_previo)] <- 
  median(imp_df_rendimiento$promedio_previo, na.rm = TRUE)

# impoutamos horas de estudio con la mediana
imp_df_rendimiento$horas_estudio[is.na(imp_df_rendimiento$horas_estudio)] <- 
  median(imp_df_rendimiento$horas_estudio, na.rm = TRUE)

# imputamos carrera con la moda estadística ("negocios" era la más frecuente)

imp_df_rendimiento$carrera[is.na(imp_df_rendimiento$carrera)] <- "negocios"

write.csv(imp_df_rendimiento, file = here("data", "processed", "rendimiento_imputado.csv"), row.names = FALSE)


# Crear dataframes auxiliares para graficar con ggplot de forma muy sencilla
# Tomamos la columna original (con NAs)
df_original_horas <- data.frame(valor = df_rendimiento$horas_estudio, 
                                estado = "1. Original (Con NAs)")

# Tomamos la columna imputada (sin NAs, rellenada con mediana)
df_imputado_horas <- data.frame(valor = imp_df_rendimiento$horas_estudio, 
                                estado = "2. Imputado (Mediana)")

# Unimos ambos dataframes uno debajo del otro usando rbind
df_grafico_horas <- rbind(df_original_horas, df_imputado_horas)


# Usamos un grafico de densidad que es muy visual para comparar distribuciones
ggplot(df_grafico_horas, aes(x = valor, fill = estado)) +
  geom_density(alpha = 0.5) +  # Alpha hace que los colores sean semi-transparentes
  theme_minimal() + labs(x = "Horas de Estudio", y = "Densidad") + 
  scale_fill_manual(values = c("1. Original (Con NAs)" = "#e74c3c", "2. Imputado (Mediana)" = "#2ecc71"))

# Mismo proceso para promedio previo
df_original_promedio <- data.frame(valor = df_rendimiento$promedio_previo, 
                                   estado = "1. Original (Con NAs)")

df_imputado_promedio <- data.frame(valor = imp_df_rendimiento$promedio_previo, 
                                   estado = "2. Imputado (Mediana)")

df_grafico_promedio <- rbind(df_original_promedio, df_imputado_promedio)

# grafico de densidad para promedio previo
ggplot(df_grafico_promedio, aes(x = valor, fill = estado)) + 
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(x = "Promedio Previo", y = "Densidad") + scale_fill_manual(values = c("1. Original (Con NAs)" = "#e74c3c", "2. Imputado (Mediana)" = "#3498db"))
