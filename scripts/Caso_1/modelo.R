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
# MODELO CON LAS INTERACCIONES DICOTOMICAS
# -------------------------------------------------------------------------

# Definimos las interacciones
interaccion_asistenciavsmodalidad = df_anova_rendimiento$asistencia * df_anova_rendimiento$modalidad
interaccion_asistenciavstrabaja = df_anova_rendimiento$asistencia * df_anova_rendimiento$trabaja
interaccion_promediopreviovsmodalidad = df_anova_rendimiento$promedio_previo * df_anova_rendimiento$modalidad
interaccion_promediopreviovsaccesointernet = df_anova_rendimiento$promedio_previo * df_anova_rendimiento$acceso_internet
interaccion_estresvsgenero = df_anova_rendimiento$estres * df_anova_rendimiento$genero
interaccion_estresvstrabajo = df_anova_rendimiento$estres * df_anova_rendimiento$trabaja
interaccion_estresvsmodalidad = df_anova_rendimiento$estres * df_anova_rendimiento$modalidad
interaccion_horasestudiovstrabaja = df_anova_rendimiento$horas_estudio * df_anova_rendimiento$trabaja
interaccion_estresvssemestre = df_anova_rendimiento$estres * df_anova_rendimiento$semestre
interaccion_asistenciavssemestre = df_anova_rendimiento$asistencia * df_anova_rendimiento$semestre

# La añadimos al dataframe
df_anova_rendimiento$interaccion_asistenciavsmodalidad = interaccion_asistenciavsmodalidad
df_anova_rendimiento$interaccion_asistenciavstrabaja = interaccion_asistenciavstrabaja
df_anova_rendimiento$interaccion_promediopreviovsmodalidad = interaccion_promediopreviovsmodalidad
df_anova_rendimiento$interaccion_promediopreviovsaccesointernet = interaccion_promediopreviovsaccesointernet
df_anova_rendimiento$interaccion_estresvsgenero = interaccion_estresvsgenero
df_anova_rendimiento$interaccion_estresvstrabajo = interaccion_estresvstrabajo
df_anova_rendimiento$interaccion_estresvsmodalidad = interaccion_estresvsmodalidad
df_anova_rendimiento$interaccion_horasestudiovstrabaja = interaccion_horasestudiovstrabaja
df_anova_rendimiento$interaccion_estresvssemestre = interaccion_estresvssemestre
df_anova_rendimiento$interaccion_asistenciavssemestre = interaccion_asistenciavssemestre

# Modelo 2: con las variables dicotomicas

modelo_2 = lm(puntaje_final ~ horas_estudio + asistencia + promedio_previo + 
                estres + interaccion_asistenciavsmodalidad + 
                interaccion_asistenciavstrabaja + 
                interaccion_promediopreviovsmodalidad + 
                interaccion_promediopreviovsaccesointernet + 
                interaccion_estresvsgenero + interaccion_estresvstrabajo + 
                interaccion_estresvsmodalidad + interaccion_horasestudiovstrabaja + 
                interaccion_estresvssemestre + interaccion_asistenciavssemestre, 
              data = df_anova_rendimiento)

modelo_2_test = lm(puntaje_final ~ horas_estudio + asistencia + promedio_previo + 
                estres + (asistencia * modalidad) + (asistencia * trabaja) + (promedio_previo * modalidad) +
                (promedio_previo * acceso_internet) + (estres * genero) + 
                (estres*trabaja) + (estres*modalidad) + (horas_estudio * trabaja) + 
                (estres * semestre) + (asistencia*semestre), data = df_anova_rendimiento)

summary(modelo_2)
summary(modelo_2_test)

# -------------------------------------------------------------------------
# Variables politomicas
# -------------------------------------------------------------------------

# Implementaremos la misma logica que usamos para las dicotomicas, usando las
# variables que sospechemos puedan tener inflluencia por la categoria

# HORAS DE ESTUDIO VS CARRERA: Si hay
ggplot(df_anova_rendimiento, aes(x = horas_estudio, y = puntaje_final, color = as.factor(carrera))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# ESTRES VS CARRERA: Si hay
ggplot(df_anova_rendimiento, aes(x = estres, y = puntaje_final, color = as.factor(carrera))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# PROMEDIO PREVIO VS CARRERA: No hay
ggplot(df_anova_rendimiento, aes(x = promedio_previo, y = puntaje_final, color = as.factor(carrera))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# USO REDES VS CARRERA: Si hay
ggplot(df_anova_rendimiento, aes(x = uso_redes, y = puntaje_final, color = as.factor(carrera))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# ASISTENCIA VS CARRERA: Si hay
ggplot(df_anova_rendimiento, aes(x = asistencia, y = puntaje_final, color = as.factor(carrera))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# PROMEDIO PREVIO VS AÑO: Si hay
ggplot(df_anova_rendimiento, aes(x = promedio_previo, y = puntaje_final, color = as.factor(anio))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# HORAS_ESTUDIO VS AÑO: Si hay
ggplot(df_anova_rendimiento, aes(x = horas_estudio, y = puntaje_final, color = as.factor(anio))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# ESTRES VS AÑO: Si hay
ggplot(df_anova_rendimiento, aes(x = estres, y = puntaje_final, color = as.factor(anio))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# ASISTENCIA VS AÑO: Si hay
ggplot(df_anova_rendimiento, aes(x = asistencia, y = puntaje_final, color = as.factor(anio))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Traza las líneas de regresión
  theme_minimal()

# -------------------------------------------------------------------------
# MODELO CON LAS INTERACCIONES POLITOMICAS
# -------------------------------------------------------------------------

modelo_3 = lm(puntaje_final ~ horas_estudio + asistencia + promedio_previo + 
                estres + (horas_estudio * carrera) + (estres * carrera)+ 
                (uso_redes * carrera) + (asistencia * carrera) + 
                (promedio_previo * anio) + (horas_estudio * anio) + 
                (estres * anio) + (asistencia * anio), data = df_anova_rendimiento)

summary(modelo_3)

# -------------------------------------------------------------------------
# Modelo con todas las interacciones
# -------------------------------------------------------------------------

# Traemos el objeto de imputacion del otro archivo
imputacion <- readRDS("imputacion.rds")

modelo_final <- with(imputacion, lm(puntaje_final ~ horas_estudio + asistencia + 
                                    promedio_previo + estres + (asistencia * modalidad) +
                                    (horas_estudio * trabaja) + (horas_estudio * anio) +
                                    (horas_estudio * carrera)))

# Agrupamos los resultados
resultado <- pool(modelo_final)
options(scipen = 0)
summary(resultado)


