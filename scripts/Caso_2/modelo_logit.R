library(ISLR)
library(ggplot2)
library(gtsummary)
library(logistf)
library(car)

# cargamos el dataset
df_espumas <- read.csv(here("data", "processed", "dataset_espumas.csv"))

# eliminamos la columna de variacion de color verde que ya vimos en el EDA
# que no aporta nada

df_espumas$peor_variacion_color <- NULL


# queremos mostrar que pasa cuando usamos alguna de las 2 variables
# con problemas de separacion completa. Entendemos que el nombre no es el mas
# apropiado, sin embargo representa bien lo que queremos decir,
# nos referimos a q predice la etiqueta de conforme o no conforme
# el 100% de las veces
modelo_deterministico <- glm(as.factor(respuesta) ~ razon_area + 
                               peor_contraste + rango_brillo,
                             data = df_espumas,
                             family = binomial())

summary(modelo_deterministico)

exp(coef(modelo_deterministico))

# modelo sin las variables que generan problemas de separacion completa
modelo_logit <- glm(as.factor(respuesta) ~ peor_contraste + rango_brillo,
                    data = df_espumas,
                    family = binomial())

summary(modelo_logit)

exp(coef(modelo_logit))

# existe alta correlacion entre peor_contraste y rango_brillo
# entonces, queremos comparar que pasa cuando quitamos una de ellas
modelo_reducido <- glm(as.factor(respuesta) ~ rango_brillo,
                    data = df_espumas,
                    family = binomial())

summary(modelo_reducido)

exp(coef(modelo_reducido))

# pruebas de comparacion

# sacamos el VIF del modelo logit

vif(modelo_logit)

# comparacion de modelos
# Si hay diferencias entre los modelos
anova(modelo_reducido, modelo_logit, test = "Chisq")

# Menor AIC -> Mejor modelo (penaliza modelos demasiado complejos)
AIC(modelo_reducido, modelo_logit)

# probabilidades

probabilidades <- predict(modelo_logit, type = "response")
head(probabilidades)

# Clasificación (umbral 0.5)
# OJO: R ordena alfabéticamente los niveles. Nivel 1 = "Conforme" (Probabilidad cerca a 0), Nivel 2 = "No conforme" (Probabilidad cerca a 1).
# Entonces, predict > 0.5 significa "No conforme".
pred_clase <- ifelse(probabilidades > 0.5, "No conforme", "Conforme")

# Matriz de confusión
mc <- table(Predicho = pred_clase,
            Real = df_espumas$respuesta)

print("Matriz de Confusión:")
print(mc)

# Extraer valores correctamente asumiendo que "No conforme" es la clase Positiva (el defecto a detectar)
VP <- mc["No conforme", "No conforme"]   # Verdaderos Positivos (Predijimos defecto y tenía defecto)
VN <- mc["Conforme", "Conforme"]         # Verdaderos Negativos (Predijimos sana y estaba sana)
FP <- mc["No conforme", "Conforme"]      # Falsos Positivos (Predijimos defecto pero estaba sana)
FN <- mc["Conforme", "No conforme"]      # Falsos Negativos (Predijimos sana pero tenía defecto)

# Indicadores

# Accuracy (Exactitud global)
accuracy <- (VP + VN) / sum(mc)

# Sensibilidad (Recall / Tasa de Verdaderos Positivos)
sensibilidad <- VP / (VP + FN)

# Especificidad (Tasa de Verdaderos Negativos)
especificidad <- VN / (VN + FP)

# Precisión (Precision / Valor predictivo positivo)
precision <- VP / (VP + FP)

# Tasa de Falsos Positivos (FPR)
fpr <- FP / (FP + VN)

# Tasa de Falsos Negativos (FNR)
fnr <- FN / (FN + VP)

# Mostrar resultados
accuracy
sensibilidad
especificidad
precision
fpr
fnr
