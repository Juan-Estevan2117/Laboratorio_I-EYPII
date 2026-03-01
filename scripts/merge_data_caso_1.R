library(here)

# Cargamos los datos crudos

df_2021_sem1 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2021_sem1.csv"))
df_2021_sem2 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2021_sem2.csv"))
df_2022_sem1 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2022_sem1.csv"))
df_2022_sem2 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2022_sem2.csv"))
df_2023_sem1 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2023_sem1.csv"))
df_2023_sem2 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2023_sem2.csv"))
df_2024_sem1 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2024_sem1.csv"))
df_2024_sem2 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2024_sem2.csv"))
df_2025_sem1 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2025_sem1.csv"))
df_2025_sem2 = read.csv(here("data", "raw", "Caso_1", "rendimiento_2025_sem2.csv"))

# Revisamos que tengan las mismas columnas

# Guardamos los df's en una lista
archivos <- list(df_2021_sem1, df_2021_sem2, df_2022_sem1, df_2022_sem2, df_2023_sem1,
                 df_2023_sem2, df_2024_sem1, df_2024_sem2, df_2025_sem1, df_2025_sem2)

# con lapply(<lista>, <funcion>) se le aplica la misma funcion a cada elemento
# de una lista, almacenamos el resultado en "columnas"

columnas <- lapply(archivos, colnames)

# devuelve true si todos son iguales y false si alguno es distinto
all(sapply(columnas, function(x) identical(x, columnas[[1]])))

# Como son iguales, los juntamos todos

df_data_historica = rbind(df_2021_sem1, df_2021_sem2, df_2022_sem1, df_2022_sem2, 
                          df_2023_sem1, df_2023_sem2, df_2024_sem1, df_2024_sem2, 
                          df_2025_sem1, df_2025_sem2)

# exportamos como csv 

write.csv(df_data_historica, file = here("data", "processed", "rendimiento_2021-2025_sem1&sem2.csv"), row.names = FALSE)
