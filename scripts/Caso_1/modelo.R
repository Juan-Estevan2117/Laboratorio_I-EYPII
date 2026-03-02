library(corrplot)
library(gtsummary)
# -------------------------------------------------------------------------
# ANOVA
# -------------------------------------------------------------------------

# Cargamos los datos imputados
df_anova_rendimiento <- read.csv(here("data", "processed", "rendimiento_imputado.csv"))

# -------------------------------------------------------------------------
# CAMBIANDO LAS CATEGORICAS A FACTORES
# -------------------------------------------------------------------------
df_anova_rendimiento$anio <- as.factor(df_anova_rendimiento$anio)
df_anova_rendimiento$semestre <- as.factor(df_anova_rendimiento$semestre)
df_anova_rendimiento$genero <- as.factor(df_anova_rendimiento$genero)
df_anova_rendimiento$carrera <- as.factor(df_anova_rendimiento$carrera)
df_anova_rendimiento$acceso_internet <- as.factor(df_anova_rendimiento$acceso_internet)
df_anova_rendimiento$trabaja <- as.factor(df_anova_rendimiento$trabaja)
df_anova_rendimiento$modalidad <- as.factor(df_anova_rendimiento$modalidad)

# -------------------------------------------------------------------------

# matriz de correlacion 

# diagrama de dispercion con las variables con mayor correlacion en la matriz

# Seleccionamos las variables cuantitativas
num_df_AR <- df_anova_rendimiento %>% select(puntaje_final, horas_estudio, 
                                             asistencia, horas_sueno, 
                                             promedio_previo, edad, estres, 
                                             uso_redes, ingresos_familiares)

cor_df <- cor(num_df_AR)
corrplot(cor_df, method = "number", type = "lower")

# Interaccion entre variables categoricas

# Modelo de prueba solo con variables numericas
modelo_1 <- lm(puntaje_final ~ horas_estudio + asistencia +promedio_previo + 
               horas_sueno + edad + estres + uso_redes + ingresos_familiares,
               data = num_df_AR)

options(scipen = 999) # Elimina la notacion cientifica
summary(modelo_1)

# -------------------------------------------------------------------------
# Ya que vimos las variables que si aportan, ahora realizaremos la interaccion
# Con las variables categoricas. 
# Verificamos con scatterplot donde vemos la dispercion entre puntaje final
# y una variable significativa (o una variable de nuestro interes)
# graficamos la recta de regresion con una de las variables categoricas
# y si se cruzan tenemos una interaccion entre las variables
# -------------------------------------------------------------------------

# Scatterplot: ASISTENCIA VS MODALIDAD: Si hay
# Sospechamos que las personas en alguna de las 2 modalidades asisten mas a 
# clases que otras. Ademas Asistencia fue significativa en "modelo_1"

ggplot(df_anova_rendimiento, aes(x = asistencia, y = puntaje_final, color = as.factor(modalidad))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: ASISTENCIA VS TRABAJA: Si hay
# La asistencia de los estudiantes podria verse afectada si trabajan o no

ggplot(df_anova_rendimiento, aes(x = asistencia, y = puntaje_final, color = as.factor(trabaja))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: PROMEDIO PREVIO VS MODALIDAD : No hay
# Sospechamos que la modalidad podria influir en el promedio previo de los
# estudiantes. Ademas promedio previo fue significativo en el modelo_1

ggplot(df_anova_rendimiento, aes(x = promedio_previo, y = puntaje_final, color = as.factor(modalidad))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: PROMEDIO PREVIO VS ACCESO A INTERNET: Si hay
# Pensamos que si un estudiante no tenia acceso a internet, su promedio se podria
# ver perjudicado

ggplot(df_anova_rendimiento, aes(x = promedio_previo, y = puntaje_final, color = as.factor(acceso_internet))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: ESTRES VS GENERO: Si hay
# Sospechamos que el estres podria ser percibido de distinta forma por el genero.
# Ademas estres fue significativa en el modelo_1

ggplot(df_anova_rendimiento, aes(x = estres, y = puntaje_final, color = as.factor(genero))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: ESTRES VS TRABAJA: Si hay
# Sospechabamos que el estres podria verse afectado en si trabaja o no

ggplot(df_anova_rendimiento, aes(x = estres, y = puntaje_final, color = as.factor(trabaja))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: ESTRES VS MODALIDAD: Si hay
# Sospechamos que una modalidad podia afectar los niveles de estres mas que otra

ggplot(df_anova_rendimiento, aes(x = estres, y = puntaje_final, color = as.factor(modalidad))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: HORAS DE ESTUDIO VS GENERO: Si hay
# Sospechamos que uno de los generos estudiaba mas que otro. Ademas de que 
# Horas de estudio fue significativa

ggplot(df_anova_rendimiento, aes(x = horas_estudio, y = puntaje_final, color = as.factor(genero))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: HORAS DE ESTUDIO VS MODALIDAD: No hay
# Sospechamos que los estudiantes en alguna modalidad podian estudiar mas que 
# en la otra

ggplot(df_anova_rendimiento, aes(x = horas_estudio, y = puntaje_final, color = as.factor(modalidad))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: HORAS DE ESTUDIO VS TRABAJA: Si hay
# Pensamos que si algun estudiante trabaja sus horas de estudio se ven afectadas

ggplot(df_anova_rendimiento, aes(x = horas_estudio, y = puntaje_final, color = as.factor(trabaja))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: ESTRES VS SEMESTRE: Si hay
# Sospechamos que los estudiantes de la primera mitad del año pueden tener menos
# estres que los de segunda mitad

ggplot(df_anova_rendimiento, aes(x = estres, y = puntaje_final, color = as.factor(semestre))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# Scatterplot: ASISTENCIA VS SEMESTRE: Si hay
# Pensamos que la asistencia podia fluctuar dependiendo del semestre

ggplot(df_anova_rendimiento, aes(x = asistencia, y = puntaje_final, color = as.factor(semestre))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# -------------------------------------------------------------------------
# MODELO CON LAS INTERACCIONES
# -------------------------------------------------------------------------

# Definimos las interacciones



