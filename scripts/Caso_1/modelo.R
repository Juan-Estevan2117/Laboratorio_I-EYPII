library(corrplot)
library(gtsummary)
# -------------------------------------------------------------------------
# ANOVA
# -------------------------------------------------------------------------

# Cargamos los datos imputados
df_anova_rendimiento <- read.csv(here("data", "processed", "rendimiento_imputado.csv"))

# matriz de correlacion 

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

options(scipen = 999)
summary(modelo_1)
