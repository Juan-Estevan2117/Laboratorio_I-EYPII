library(corrplot)
library(gtsummary)
library(here)
library(lmtest)
library(car)

# Cargamos los datos imputados
df_anova_rendimiento <- read.csv(here("data", "processed", "rendimiento_imputado.csv"))

summary(df_anova_rendimiento)

str(df_anova_rendimiento)

# Dicotomizando la variable carrera
# Como 'carrera' tiene 4 categorías, crearemos 4 columnas nuevas llenas de ceros.
# Luego, si la fila pertenece a esa carrera, le ponemos un 1.

# inicializamos las columnas en 0
df_anova_rendimiento$carrera_negocios <- 0
df_anova_rendimiento$carrera_datos <- 0
df_anova_rendimiento$carrera_ingenieria <- 0
df_anova_rendimiento$carrera_cs <- 0  # Ciencias de la Computación

# Paso 2: Asignar un 1 donde corresponda usando ifelse
df_anova_rendimiento$carrera_negocios <- ifelse(df_anova_rendimiento$carrera == "negocios", 1, 0)
df_anova_rendimiento$carrera_datos <- ifelse(df_anova_rendimiento$carrera == "datos", 1, 0)
df_anova_rendimiento$carrera_ingenieria <- ifelse(df_anova_rendimiento$carrera == "ingenieria", 1, 0)
df_anova_rendimiento$carrera_cs <- ifelse(df_anova_rendimiento$carrera == "ciencias de la computacion", 1, 0)

# Verificamos que haya quedado bien
head(df_anova_rendimiento[, c("carrera", "carrera_negocios", "carrera_datos", "carrera_ingenieria", "carrera_cs")])

# Creación de Interacciones Estadísticas

# Interacciones con Horas de Estudio
df_anova_rendimiento$inter_horas_datos <- df_anova_rendimiento$horas_estudio * df_anova_rendimiento$carrera_datos
df_anova_rendimiento$inter_horas_ing <- df_anova_rendimiento$horas_estudio * df_anova_rendimiento$carrera_ingenieria
df_anova_rendimiento$inter_horas_cs <- df_anova_rendimiento$horas_estudio * df_anova_rendimiento$carrera_cs

# Interacciones con Promedio Previo
df_anova_rendimiento$inter_prom_datos <- df_anova_rendimiento$promedio_previo * df_anova_rendimiento$carrera_datos
df_anova_rendimiento$inter_prom_ing <- df_anova_rendimiento$promedio_previo * df_anova_rendimiento$carrera_ingenieria
df_anova_rendimiento$inter_prom_cs <- df_anova_rendimiento$promedio_previo * df_anova_rendimiento$carrera_cs


# -------------------------------------------------------------------------
# Interaccion con las variables dicotomicas
# -------------------------------------------------------------------------

# Interacciones con trabaja (1 = si, 0 = no)
df_anova_rendimiento$inter_horas_trabaja <- df_anova_rendimiento$horas_estudio * df_anova_rendimiento$trabaja
df_anova_rendimiento$inter_prom_trabaja <- df_anova_rendimiento$promedio_previo * df_anova_rendimiento$trabaja

# Interacciones con modalidad (1 = presencial, 0 = virtual)
df_anova_rendimiento$inter_horas_modalidad <- df_anova_rendimiento$horas_estudio * df_anova_rendimiento$modalidad
df_anova_rendimiento$inter_prom_modalidad <- df_anova_rendimiento$promedio_previo * df_anova_rendimiento$modalidad

# Interacciones con acceso_internet (1 = si, 0 = no)
df_anova_rendimiento$inter_horas_internet <- df_anova_rendimiento$horas_estudio * df_anova_rendimiento$acceso_internet

# Interacciones con genero (1 = female, 0 = male)
df_anova_rendimiento$inter_horas_genero <- df_anova_rendimiento$horas_estudio * df_anova_rendimiento$genero


# Solo modalidad sale como significativa teniendo en cuenta un al alpha de 0.05
modelo <- lm(puntaje_final ~ horas_estudio + promedio_previo + 
    carrera_datos + carrera_ingenieria + carrera_cs + trabaja + modalidad + 
    acceso_internet + genero + semestre + inter_horas_datos + inter_horas_ing + 
    inter_horas_cs + inter_prom_datos + inter_prom_ing + inter_prom_cs + inter_horas_trabaja + 
    inter_prom_trabaja + inter_horas_modalidad + inter_prom_modalidad + inter_horas_internet + 
    inter_horas_genero, data = df_anova_rendimiento)

summary(modelo)

# montamos el modelo solo con las significativas
modelo_simplificado <- lm(puntaje_final ~ horas_estudio + promedio_previo + 
                            modalidad, data = df_anova_rendimiento)

summary(modelo_simplificado)

# Validacion de supuestos

# Extraer los residuales

error = modelo_simplificado$residuals

# Linealidad:  mediante el grafico de residuos vs valores ajustados

plot(modelo_simplificado$fitted.values, error, pch = 19)
abline(h = 0, col = "red", lwd = 2, lty = 2)

# verificamos con prueba t-student
t.test(error) # como p-value es = 1 se cumple el supuesto

# Normalidad: Test de Shapiro-Wilk
shapiro.test(error) 

# Homocedasticidad - varianza constante: No se cumple el supuesto
s
bptest(modelo_simplificado)

# Multicolinealidad: Se evalúa mediante el Factor de Inflación de Varianza (VIF):
vif(modelo_simplificado) # Como VIF < 10 no tenemos problemas de multicolinealidad

# Metricas de rendimiento

# -------------------------------------------------------------------------

ECM = function(y_observado, y_ajustado){
  
  r = sum((y_observado - y_ajustado)^2)/length((y_observado))
  return(r)
}

ECM(df_anova_rendimiento$puntaje_final, modelo$fitted.values)

# -------------------------------------------------------------------------

EAM = function(y_observado, y_ajustado){
  
  r = sum(abs(y_observado - y_ajustado))/length((y_observado))
  return(r)
}

EAM(df_anova_rendimiento$puntaje_final, modelo$fitted.values)

# -------------------------------------------------------------------------

MAPE = function(y_observado, y_ajustado, porcentaje){
  
  if (porcentaje < 0 | porcentaje > 1 ) {
    return("Error! el valor del porcentaje debe estar entre 0 y 1")
  }
  r = (porcentaje/length(y_observado))*sum(abs((y_observado - y_ajustado)/y_observado))
  return(r)
}

MAPE(df_anova_rendimiento$puntaje_final, modelo$fitted.values, 1)

